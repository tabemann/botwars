-- Copyright (c) 2017, Travis Bemann
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
-- o Redistributions of source code must retain the above copyright notice, this
--   list of conditions and the following disclaimer.
-- 
-- o Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
-- 
-- o Neither the name of the copyright holder nor the names of its
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}

module Robots.Genetic.HunterKiller.Save

  (save)

where

import Robots.Genetic.HunterKiller.Types
import Data.Sequence as Seq
import Data.Text as Text
import Text.Printf (printf)
import Data.Foldable (foldl')

-- | Save a robot program as text.
save :: Seq.Seq RobotConstEntry -> RobotExpr -> Text.Text
save consts expr = outputExpr consts 0 0 expr

-- | Actually output a robot program as text.
outputExpr :: Seq.Seq RobotConstEntry -> Int -> Int -> RobotExpr -> Text.Text
outputExpr _ firstIndent _ (RobotLoad index) =
  indentText firstIndent . Text.pack $ printf "load %i" index
outputExpr _ firstIndent nextIndent (RobotConst value) =
  let valueText = outputValue 0 (nextIndent + Text.length "const ") value
  in indentText firstIndent . Text.pack $
       printf "const %s" valueText
outputExpr consts firstIndent _ (RobotSpecialConst index) =
  case Seq.lookup index consts of
    Just (RobotConstEntry _ name) ->
      indentText firstIndent $ Text.append "special " name
    Nothing -> error "this should be impossible"
outputExpr consts firstIndex nextIndex (RobotBind boundExprs expr) =
  Text.intercalate "\n"
    [indentText firstIndex "bind",
     outputExprs consts (nextIndex + 1) (nextIndex + 1) boundExprs,
     indentText nextIndex "in",
     outputParenExpr consts (nextIndex + 1) (nextIndex + 1) expr]
outputExpr consts firstIndex nextIndex (RobotFunc argCount expr) =
  Text.intercalate "\n"
    [indentText firstIndex . Text.pack $ printf "func %i as" argCount,
     outputParenExpr consts (nextIndex + 1) (nextIndex + 1) expr]
outputExpr consts firstIndex nextIndex (RobotApply argExprs funcExpr) =
  Text.intercalate "\n"
    [indentText firstIndex "apply with",
     outputExprs consts (nextIndex + 1) (nextIndex + 1) argExprs,
     indentText nextIndex "for",
     outputParenExpr consts (nextIndex + 1) (nextIndex + 1) funcExpr]
outputExpr consts firstIndex nextIndex (RobotCond condExpr trueExpr falseExpr) =
  Text.intercalate "\n"
    [indentText firstIndex "if",
     outputParenExpr consts (nextIndex + 1) (nextIndex + 1) condExpr,
     indentText nextIndex "then",
     outputParenExpr consts (nextIndex + 1) (nextIndex + 1) trueExpr,
     indentText nextIndex "else"
     outputParenExpr consts (nextIndex + 1) (nextIndex + 1) falseExpr]

-- | Output a list of expressions.
outputExprs :: Seq.Seq RobotConstEntry -> Int -> Int -> Seq.Seq RobotExpr ->
               Text.Text
outputExprs consts firstIndex nextIndex exprs =
  case Seq.length exprs of
    0 -> indentText firstIndent "{}"
    1 ->
      case Seq.lookup 0 exprs of
        Just expr -> let exprText = outputExpr consts 0 (nextIndent + 1) expr
                     in indentText firstIndent . Text.pack $
                          printf "{%s}" exprText
        Nothing -> error "this should be impossible"
    _ -> let exprText = outputExpr consts 0 (nextIndent + 1) expr
             firstLine = indentText firstIndent . Text.pack $
                           printf "{%s," exprText
             (_, text) = foldl' (\(count, text) expr ->
                                   let exprText = outputExpr consts
                                                    (nextIndent + 1)
                                                    (nextIndent + 1) expr
                                       nextLine =
                                         if count > 0
                                         then Text.append exprText ","
                                         else Text.append exprText "]"
                                   in (count - 1,
                                       Text.append (Text.append text "\n")
                                         nextLine))
                           (Seq.length exprs - 1, firstLine) exprs
         in text

-- | Output an expression in parentheses.
outputParenExpr :: Seq.Seq RobotConstEntry -> Int -> Int -> RobotExpr ->
                   Text.Text
outputParenExpr consts firstIndex nextIndex expr =
  indentText firstIndex . Text.pack $ printf "(%s)"
    (outputExpr consts 0 (nextIndex + 1) expr)

-- | Output a value as text.
outputValue :: Int -> Int -> RobotValue -> Text.Text
outputValue firstIndent _ RobotNull = indentText firstIndent "null"
outputValue firstIndent _ (RobotBool True) = indentText firstIndent "true"
outputValue firstIndent _ (RobotBool False) = indentText firstIndent "false"
outputValue firstIndent _ (RobotInt value) =
  indentText firstIndent . Text.pack $ printf "int %i" value
outputValue firstIndent _ (RobotFloat value) =
  indentText firstIndent . Text.pack $ printf "float %g" value
outputValue firstIndent nextIndent (RobotVector values) =
  case Seq.length values of
    0 -> indentText firstIndent "[]"
    1 ->
      case Seq.lookup 0 values of
        Just value -> let valueText = outputValue 0 (nextIndent + 1) value
                      in indentText firstIndent . Text.pack $
                           printf "[%s]" valueText
        Nothing -> error "this should be impossible"
    _ -> let valueText = outputValue 0 (nextIndent + 1) value
             firstLine = indentText firstIndent . Text.pack $
                           printf "[%s," valueText
             (_, text) = foldl' (\(count, text) value ->
                                   let valueText = outputValue (nextIndent + 1)
                                                     (nextIndent + 1) value
                                       nextLine =
                                         if count > 0
                                         then Text.append valueText ","
                                         else Text.append valueText "]"
                                   in (count - 1,
                                       Text.append (Text.append text "\n")
                                         nextLine))
                           (Seq.length values - 1, firstLine) values
         in text

-- | Generate an indent string.
indentText :: Int -> Text.Text -> Text.Text
indentText n = Text.append (Text.replicate n " ")
