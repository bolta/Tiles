{-# LANGUAGE DeriveDataTypeable #-}

-- | 文字列で与えた式を eval して Divider を動的に得るモジュール
module DividerEval (evalToDivider, DividerWrapper (WrapDivider)) where

import Data.List (intercalate)
import Data.Typeable
import Language.Haskell.Interpreter

import Divider

{- |
	式を eval した結果を Divider として得る。
	正しく評価できれば結果が Right で、失敗したらエラーが Left で返る
-}
evalToDivider :: String -> IO (Either String Divider)
evalToDivider code = do
	result <- evalToDivider' code
	return $ case result of
		Left error -> Left $ errorToString error
		Right wrapped -> Right $ unwrapDivider wrapped

-- see http://stackoverflow.com/questions/15995116/how-to-add-instance-declaration-for-typeable
newtype DividerWrapper =
	WrapDivider { unwrapDivider :: Divider }
	deriving Typeable

evalToDivider' :: String -> IO (Either InterpreterError DividerWrapper)
evalToDivider' code = runInterpreter $ do
	loadModules ["Divider", "DividerEval"]
	setImports ["Prelude", "Divider", "DividerEval"]
	interpret ("WrapDivider $ " ++ code) (as :: DividerWrapper)

errorToString :: InterpreterError -> String
errorToString error = case error of
	UnknownError e ->
		"Unknown error: " ++ e
	WontCompile es ->
		"Won't compile: " ++ (intercalate "\n" $ map errMsg es)
	NotAllowed e ->
		"Not allowed: " ++ e
	GhcException e ->
		"GHC exception: " ++ e


