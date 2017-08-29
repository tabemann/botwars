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

  (loadWorld,
   load)

where

import Robots.Genetic.HunterKiller.Types
import Robots.Genetic.HunterKiller.Utility
import Robots.Genetic.HunterKiller.World
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Sequence as Seq
import Data.Sequence ((<|))
import Data.Either (isLeft)
import Data.Functor ((<$>))
import Control.Applicative ((<*>),
                            (*>),
                            (<*),
                            (<|>))
import Data.Char (isSpace)

-- | Load a robot world from text.
loadWorld :: Seq.Seq RobotConstEntry -> Text.Text ->
             Either Text.Text (Seq.Seq RobotExpr)
loadWorld consts =
  condenseLefts . Atto.parseOnly (parseWorld consts <* Atto.endOfInput) .
  prepText

-- | Load a robot from text.
load :: Seq.Seq RobotConstEntry -> Text.Text -> Either Text.Text RobotExpr
load consts =
  condenseLefts . Atto.parseOnly (parse consts <* Atto.endOfInput) . prepText

-- | Condense lefts
condenseLefts :: Either String (Either Text.Text b) -> Either Text.Text b
condenseLefts (Right (Right x)) = Right x
condenseLefts (Right (Left y)) = Left y
condenseLefts (Left z) = Left $ Text.pack z

-- | Parse a robot world.
parseWorld :: Seq.Seq RobotConstEntry ->
             Atto.Parser (Either Text.Text (Seq.Seq RobotExpr))
parseWorld consts =
  Atto.skipSpace *>
  "world" *>
  Atto.skipSpace *>
  "{" *>
  Atto.skipSpace *>
  (condenseErrors <$> (Atto.option Seq.empty
                       ((<|) <$> parse consts
                             <*> (Seq.fromList <$> (Atto.many'
                                                    (Atto.skipSpace *>
                                                     "," *>
                                                     Atto.skipSpace *>
                                                     parse consts)))))) <*
  Atto.skipSpace <*
  "}" <*
  Atto.skipSpace

-- | Condense errors from a list of potential errors.
condenseErrors :: Seq.Seq (Either Text.Text RobotExpr) ->
                  Either Text.Text (Seq.Seq RobotExpr)
condenseErrors values =
  case Seq.findIndexL isLeft values of
    Just index ->
      case Seq.lookup index values of
        Just (Left errorMessage) -> Left errorMessage
        _ -> error "this should be impossible"
    Nothing -> Right $ fmap (\value -> case value of
                                Right value' -> value'
                                Left _ -> error "this should be impossible")
                         values

-- | Parse a robot program.
parse :: Seq.Seq RobotConstEntry -> Atto.Parser (Either Text.Text RobotExpr)
parse consts = verifyExpr startingContextDepth <$> parseExpr consts

-- | Parse an expression.
parseExpr :: Seq.Seq RobotConstEntry -> Atto.Parser RobotExpr
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
                                    Atto.skipSpace *>
                                    Atto.takeWhile1 isSpecialConstChar <*
                                    Atto.skipSpace)

-- | Actually try to parse a special const expression.
tryVerifySpecialConst :: Seq.Seq RobotConstEntry -> Text.Text -> RobotExpr
tryVerifySpecialConst consts name =
  case Seq.elemIndexL name (fmap (\(RobotConstEntry _ name) -> name) consts) of
    Just index -> RobotSpecialConst index
    Nothing -> RobotSpecialConst (-1)

-- | Check whether a character is a valid special constant name character
isSpecialConstChar :: Char -> Bool
isSpecialConstChar char = (not $ isSpace char) &&
                          (char /= '[') &&
                          (char /= ']') &&
                          (char /= '(') &&
                          (char /= ')') &&
                          (char /= '{') &&
                          (char /= '}') &&
                          (char /= ',')

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
  Atto.skipSpace *>
  "{" *>
  Atto.skipSpace *>
  (Atto.option Seq.empty
    ((<|) <$> parseExpr consts
          <*> (Seq.fromList <$> (Atto.many'
                                  (Atto.skipSpace *>
                                   "," *>
                                   Atto.skipSpace *>
                                   parseExpr consts))))) <*
  Atto.skipSpace <*
  "}" <*
  Atto.skipSpace

-- | Parse a func expression.
parseFunc :: Seq.Seq RobotConstEntry -> Atto.Parser RobotExpr
parseFunc consts =
  RobotFunc <$> (Atto.skipSpace *>
                 "func" *>
                 Atto.skipSpace *>
                 Atto.decimal)
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
  (RobotVector <$>
   (Atto.skipSpace *>
    "[" *>
    Atto.skipSpace *>
    (Atto.option Seq.empty
     ((<|) <$> parseValue
           <*> (Seq.fromList <$> (Atto.many'
                                  (Atto.skipSpace *>
                                   "," *>
                                   Atto.skipSpace *>
                                   parseValue))))) <*
     
    Atto.skipSpace <*
    "]"))

-- | Verify an expression.
verifyExpr :: Int -> RobotExpr -> Either Text.Text RobotExpr
verifyExpr contextDepth expr@(RobotLoad index) =
  if (index >= 0) && (index < contextDepth)
  then Right expr
  else Left "invalid load index"
verifyExpr _ expr@(RobotConst _) = Right expr
verifyExpr _ expr@(RobotSpecialConst index) =
  if index >= 0
  then Right expr
  else Left "unknown special constant"
verifyExpr contextDepth expr@(RobotBind boundExprs expr') =
  let boundExprs' =
        fmap (verifyExpr (contextDepth + Seq.length boundExprs)) boundExprs
  in case Seq.findIndexL isLeft boundExprs' of
       Just index ->
         case Seq.lookup index boundExprs' of
           Just exprError -> exprError
           Nothing -> error "could not find index"
       Nothing ->
         case verifyExpr (contextDepth + Seq.length boundExprs) expr' of
           Right _ -> Right expr
           exprError -> exprError
verifyExpr contextDepth expr@(RobotFunc argCount expr') =
  case verifyExpr (contextDepth + argCount) expr' of
    Right _ -> Right expr
    exprError -> exprError
verifyExpr contextDepth expr@(RobotApply argExprs funcExpr) =
  let argExprs' = fmap (verifyExpr contextDepth) argExprs
  in case Seq.findIndexL isLeft argExprs' of
       Just index ->
         case Seq.lookup index argExprs' of
           Just exprError -> exprError
           Nothing -> error "could not find index"
       Nothing ->
         case verifyExpr contextDepth funcExpr of
           Right _ -> Right expr
           exprError -> exprError
verifyExpr contextDepth expr@(RobotCond condExpr trueExpr falseExpr) =
  case verifyExpr contextDepth condExpr of
    Right _ ->
      case verifyExpr contextDepth trueExpr of
        Right _ ->
          case verifyExpr contextDepth falseExpr of
            Right _ -> Right expr
            exprError -> exprError
        exprError -> exprError
    exprError -> exprError
