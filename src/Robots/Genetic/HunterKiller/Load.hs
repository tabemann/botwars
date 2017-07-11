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

module Robots.Genetic.HunterKiller.Load

  (load)

where

import Robots.Genetic.HunterKiller.Types
import Robots.Genetic.HunterKiller.World
import Data.Text as Text
import Data.Attoparsec.Text as Atto
import Data.Sequence as Seq
import Data.Sequence ((<|))
import Data.Either (isLeft)
import Data.Functor ((<$>))
import Control.Applicative ((<*>),
                            (*>),
                            (<*),
                            (<|>))
import Data.Char (isSpace)

-- | Load a robot program from text.
load :: Seq.Seq RobotConstEntry -> Text.Text -> Either Text.Text RobotExpr
load consts text =
  case Atto.parseOnly (parseExpr consts) text of
    Right expr -> verifyExpr startingContextDepth expr
    Left errorMessage -> Left . Text.pack $ errorMessage

-- | Parse an expression.
parseExpr :: Seq.Seq RobotConstEntry -> Atto.Parser (Either Text.Text RobotExpr)
parseExpr consts =
  Atto.choice [parseLoad,
               parseConst,
               parseSpecialConst consts,
               parseBind consts,
               parseFunc consts,
               parseApply consts,
               parseCond consts]

-- | Parse a load expression.
parseLoad :: Atto.Parser RobotExpr
parseLoad =
  RobotLoad <$> (Atto.skipSpace *>
                 "load" *>
                 Atto.takeWhile1 isSpace *>
                 Atto.decimal <*
                 Atto.skipSpace)

-- | Parse a const expression.
parseConst :: Atto.Parser RobotExpr
parseConst =
  RobotConst <$> (Atto.skipSpace *>
                  "const" *>
                  Atto.takeWhile1 isSpace *>
                  parseValue <*
                  Atto.skipSpace)

-- | Parse a special const expression.
parseSpecialConst :: Seq.Seq RobotConstEntry -> Atto.Parser RobotExpr
parseSpecialConst consts =
  tryVerifySpecialConst consts <$> (Atto.skipSpace *>
                                    "special" *>
                                    Atto.takeWhile1 isSpecialConstChar <*
                                    Atto.skipSpace)

-- | Actually try to parse a special const expression.
tryVerifySpecialConst :: Seq.Seq RobotConstEntry -> Text.Text -> RobotExpr
tryVerifySpecialConst consts name =
  case Seq.elemIndexL name (fmap (\(RobotConstEntry _ name) -> name) const) of
    Just index -> RobotSpecialConst index
    Nothing -> RobotSpecialConst (-1)

-- | Check whether a character is a valid special constant name character
isSpecialConstChar :: Char -> Bool
isSpecialConstChar char = (not . isSpace $ char) &&
                          (char != '[') &&
                          (char != ']') &&
                          (char != '(') &&
                          (char != ')') &&
                          (char != ',')

-- | Parse a bind expression.
parseBind :: Seq.Seq RobotConstEntry -> Atto.Parser RobotExpr
parseBind consts =
  RobotBind <$> (Atto.skipSpace *>
                 "bind" *>
                 Atto.skipSpace *>
                 parseExprs consts)
            <*> (Atto.skipSpace *>
                 "in" *>
                 Atto.skipSpace *>
                 "(" *>
                 Atto.skipSpace *>
                 parseExpr consts <*
                 Atto.skipSpace <*
                 ")" <*
                 Atto.skipSpace)
 
-- | Parse expression list.
parseExprs :: Seq.Seq RobotConstEntry -> Atto.Parser (Seq.Seq RobotExpr)
parseExprs consts =
  "{" *>
  Atto.skipSpace *>
  Atto.option Seq.empty
   ((<|) <$> (parseExpr consts <*>
              (Seq.fromList <$> (Atto.many1
                                 (Atto.skipSpace *>
                                  "," *>
                                  Atto.skipSpace *>
                                  parseExpr consts))))) <*
  Atto.skipSpace <*
  "}")

-- | Parse a func expression.
parseFunc :: Seq.Seq RobotConstEntry -> Atto.Parser RobotExpr
parseFunc consts =
  RobotFunc <$> (Atto.skipSpace *>
                 "func" *>
                 Atto.skipSpace *>
                 decimal)
            <*> (Atto.skipSpace *>
                 "as" *>
                 Atto.skipSpace *>
                 Atto.skipSpace *>
                 "(" *>
                 Atto.skipSpace *>
                 parseExpr consts <*
                 Atto.skipSpace <*
                 ")" <*
                 Atto.skipSpace)

-- | Parse an apply expression.
parseApply :: Seq.Seq RobotConstEntry -> Atto.Parser RobotExpr
parseApply consts =
  RobotApply <$> (Atto.skipSpace *>
                  "apply" *>
                  Atto.skipSpace *>
                  "with" *>
                  Atto.skipSpace *>
                  parseExprs consts)
             <*> (Atto.skipSpace *>
                  "for" *>
                  Atto.skipSpace *>
                  "(" *>
                  Atto.skipSpace *>
                  parseExpr consts <*
                  Atto.skipSpace <*
                  ")" <*
                  Atto.skipSpace)

-- | Parse a cond expression.
parseCond :: Seq.Seq RobotConstEntry -> Atto.Parser RobotExpr
parseCond consts =
  RobotCond <$> (Atto.skipSpace *>
                 "if" *>
                 Atto.skipSpace *>
                 "(" *>
                 Atto.skipSpace *>
                 parseExpr consts <*
                 Atto.skipSpace <*
                 ")" <*
                 Atto.skipSpace)
            <*> (Atto.skipSpace *>
                 "then" *>
                 Atto.skipSpace *>
                 "(" *>
                 Atto.skipSpace *>
                 parseExpr consts <*
                 Atto.skipSpace <*
                 ")" <*
                 Atto.skipSpace)
            <*> (Atto.skipSpace *>
                 "else" *>
                 Atto.skipSpace *>
                 "(" *>
                 Atto.skipSpace *>
                 parseExpr consts <*
                 Atto.skipSpace <*
                 ")" <*
                 Atto.skipSpace)

-- | Parse a value.
parseValue :: Atto.Parser RobotValue
parseValue =
  ((const RobotNull) <$> (Atto.skipSpace *>
                          "null" <*
                          Atto.skipSpace)) <|>
  ((const (RobotBool True)) <$> (Atto.skipSpace *>
                                 "true" <*
                                 Atto.skipSpace)) <|>
  ((const (RobotBool False)) <$> (Atto.skipSpace *>
                                  "false" <*
                                  Atto.skipSpace)) <|>
  (RobotInt <$> (Atto.skipSpace *>
                 "int" *>
                 Atto.takeWhile1 isSpace *>
                 Atto.signed Atto.decimal <*
                 Atto.skipSpace)) <|>
  (RobotFloat <$> (Atto.skipSpace *>
                   "float" *>
                   Atto.takeWhile1 isSpace *>
                   Atto.double <*
                   Atto.skipSpace)) <|>
  ((RobotVector . Seq.fromList) <$>
   (Atto.skipSpace *>
    "[" *>
    Atto.skipSpace *>
    Atto.option Seq.empty
     ((<|) <$> (parseValue <*>
                (Seq.fromList <$> (Atto.many1
                                   (Atto.skipSpace *>
                                    "," *>
                                    Atto.skipSpace *>
                                    parseValue))))) <*
     
    Atto.skipSpace <*
    "]"))

-- | Verify an expression.
verifyExpr :: Int -> RobotExpr -> Either Text.Text RobotExpr
verifyExpr contextDepth (RobotLoad index)@expr =
  if (index >= 0) && (index < contextDepth)
  then Right expr
  else Left "invalid load index"
verifyExpr _ (RobotConst _)@expr = Right expr
verifyExpr _ (RobotSpecialConst index)@expr =
  if index >= 0
  then Right expr
  else Left "unknown special constant"
verifyExpr contextDepth (RobotBind boundExprs expr')@expr =
  let boundExprs' =
        fmap (verifyExpr $ contextDepth + Seq.length boundExprs) boundExprs
  in case Seq.elemIndexL isLeft boundExprs' of
       Just index ->
         case Seq.lookup index boundExprs' of
           Just exprError -> exprError
           Nothing -> error "could not find index"
       Nothing ->
         case verifyExpr (contextDepth + Seq.length boundExprs) expr' of
           Right _ -> Right expr
           exprError -> exprError
verifyExpr contextDepth (RobotFunc argCount expr')@expr =
  case verifyExpr (contextDepth + argCount) expr' of
    Right _ -> Right expr
    exprError -> exprError
verifyExpr contextDepth (RobotApply argExprs funcExpr)@expr =
  let argExprs' = fmap (verifyExpr contextDepth) argExprs
  in case Seq.elemIndexL isLeft argExprs' of
       Just index ->
         case Seq.lookup index argExprs' of
           Just exprError -> exprError
           Nothing -> error "could not find index"
       Nothing ->
         case verifyExpr contextDepth funcExpr of
           Right _ -> Right expr
           exprError -> exprError
verifyExpr contextDepth (RobotCond condExpr trueExpr falseExpr)@expr =
  case verifyExpr contextDepth condExpr of
    Right _ ->
      case verifyExpr contextDepth trueExpr of
        Right _ ->
          case verifyExpr contextDepth falseExpr of
            Right _ -> Right expr
            exprError -> exprError
        exprError -> exprError
    exprError -> exprError
