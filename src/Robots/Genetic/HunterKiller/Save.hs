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

{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Robots.Genetic.HunterKiller.Save

  (saveWorld,
   save)

where

import Robots.Genetic.HunterKiller.Types
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Text.Printf (printf)
import Data.Foldable (foldl',
                      toList)
import Data.Sequence ((><),
                      (<|),
                      (|>))

-- | Save a robot world as text.
saveWorld :: Seq.Seq RobotConstEntry -> Seq.Seq RobotExpr -> Text.Text
saveWorld consts exprs =
  Text.concat . toList $ "world\n" <| outputExprs consts 1 1 exprs

-- | Save a robot program as text.
save :: Seq.Seq RobotConstEntry -> RobotExpr -> Text.Text
save consts expr = Text.concat . toList $ outputExpr consts 0 0 expr

-- | Actually output a robot program as text.
outputExpr :: Seq.Seq RobotConstEntry -> Int -> Int -> RobotExpr ->
  Seq.Seq Text.Text
outputExpr _ firstIndent _ (RobotLoad index) =
  indentText firstIndent . Seq.singleton . Text.pack $ printf "load %i" index
outputExpr _ firstIndent nextIndex (RobotConst value) =
  let valueText = outputValue 0 (nextIndex + Text.length "const ") value
  in indentText firstIndent $ "const " <| valueText
outputExpr consts firstIndent _ (RobotSpecialConst index) =
  case Seq.lookup index consts of
    Just (RobotConstEntry _ name) -> indentText firstIndent ["special ", name]
    Nothing -> error "this should be impossible"
outputExpr consts firstIndex nextIndex (RobotBind boundExprs expr) =
  indentText firstIndex (Seq.singleton "bind\n") ><
  outputExprs consts (nextIndex + 1) (nextIndex + 1) boundExprs ><
  Seq.singleton "\n" >< indentText nextIndex (Seq.singleton "in\n") ><
  outputParenExpr consts (nextIndex + 1) (nextIndex + 1) expr
outputExpr consts firstIndex nextIndex (RobotFunc argCount expr) =
  (indentText firstIndex . Seq.singleton . Text.pack $
    printf "func %i as\n" argCount) ><
  outputParenExpr consts (nextIndex + 1) (nextIndex + 1) expr
outputExpr consts firstIndex nextIndex (RobotApply argExprs funcExpr) =
  indentText firstIndex (Seq.singleton "apply with\n") ><
  outputExprs consts (nextIndex + 1) (nextIndex + 1) argExprs ><
  Seq.singleton "\n" >< indentText nextIndex (Seq.singleton "for\n") ><
  outputParenExpr consts (nextIndex + 1) (nextIndex + 1) funcExpr
outputExpr consts firstIndex nextIndex (RobotCond condExpr trueExpr falseExpr) =
  indentText firstIndex (Seq.singleton "if\n") ><
  outputParenExpr consts (nextIndex + 1) (nextIndex + 1) condExpr ><
  Seq.singleton "\n" >< indentText nextIndex (Seq.singleton "then\n") ><
  outputParenExpr consts (nextIndex + 1) (nextIndex + 1) trueExpr ><
  Seq.singleton "\n" >< indentText nextIndex (Seq.singleton "else\n") ><
  outputParenExpr consts (nextIndex + 1) (nextIndex + 1) falseExpr

-- | Output a list of expressions.
outputExprs :: Seq.Seq RobotConstEntry -> Int -> Int -> Seq.Seq RobotExpr ->
               Seq.Seq Text.Text
outputExprs consts firstIndex nextIndex exprs =
  case Seq.length exprs of
    0 -> indentText firstIndex $ Seq.singleton "{}"
    1 ->
      case Seq.lookup 0 exprs of
        Just expr -> let exprText = outputExpr consts 0 (nextIndex + 1) expr
                     in indentText firstIndex $
                        Seq.singleton "{" >< exprText >< Seq.singleton "}"
        Nothing -> error "this should be impossible"
    _ -> case Seq.lookup 0 exprs of
           Just expr ->
             let exprText = outputExpr consts 0 (nextIndex + 1) expr
                 firstLine = indentText firstIndex $
                             Seq.singleton "{" >< exprText >< Seq.singleton ","
                 (_, text) = foldl' (\(count, text) expr ->
                                       let exprText = outputExpr consts
                                                      (nextIndex + 1)
                                                      (nextIndex + 1) expr
                                           nextLine =
                                             if count > 0
                                             then exprText |> ","
                                             else exprText |> "}"
                                       in (count - 1,
                                           text >< Seq.singleton "\n" ><
                                           nextLine))
                             (Seq.length exprs - 2, firstLine)
                             (Seq.drop 1 exprs)
             in text
           Nothing -> error "this should be impossible"

-- | Output an expression in parentheses.
outputParenExpr :: Seq.Seq RobotConstEntry -> Int -> Int -> RobotExpr ->
                   Seq.Seq Text.Text
outputParenExpr consts firstIndex nextIndex expr =
  indentText firstIndex $ Seq.singleton "(" ><
    outputExpr consts 0 (nextIndex + 1) expr >< Seq.singleton ")"

-- | Output a value as text.
outputValue :: Int -> Int -> RobotValue -> Seq.Seq Text.Text
outputValue firstIndex _ RobotNull =
  indentText firstIndex $ Seq.singleton "null"
outputValue firstIndex _ (RobotBool True) =
  indentText firstIndex $ Seq.singleton "true"
outputValue firstIndex _ (RobotBool False) =
  indentText firstIndex $ Seq.singleton "false"
outputValue firstIndex _ (RobotInt value) =
  indentText firstIndex . Seq.singleton . Text.pack $ printf "int %i" value
outputValue firstIndex _ (RobotFloat value) =
  indentText firstIndex . Seq.singleton . Text.pack $ printf "float %g" value
outputValue firstIndex nextIndex (RobotVector values) =
  case Seq.length values of
    0 -> indentText firstIndex $ Seq.singleton "[]"
    1 ->
      case Seq.lookup 0 values of
        Just value -> let valueText = outputValue 0 (nextIndex + 1) value
                      in indentText firstIndex $ Seq.singleton "[" ><
                         valueText >< Seq.singleton "]"
        Nothing -> error "this should be impossible"
    _ ->
      case Seq.lookup 0 values of
        Just value ->
          let valueText = outputValue 0 (nextIndex + 1) value
              firstLine = indentText firstIndex $ Seq.singleton "[" ><
                          valueText >< Seq.singleton ","
              (_, text) = foldl' (\(count, text) value ->
                                    let valueText = outputValue (nextIndex + 1)
                                                    (nextIndex + 1) value
                                        nextLine =
                                          if count > 0
                                          then valueText |> ","
                                          else valueText |> "]"
                                    in (count - 1,
                                        text >< Seq.singleton "\n" ><
                                         nextLine))
                          (Seq.length values - 2, firstLine)
                          (Seq.drop 1 values)
          in text
        Nothing -> error "this should be impossible"

-- | Generate an indent string.
indentText :: Int -> Seq.Seq Text.Text -> Seq.Seq Text.Text
indentText n s = (Text.replicate n " ") <| s
