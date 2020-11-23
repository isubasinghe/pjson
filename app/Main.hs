module Main where

import qualified Control.Monad as M
import qualified Data.Either as E
import qualified Data.Map.Lazy as L
import qualified Parser as P
import System.Environment

repl :: L.Map String P.JsonObject -> IO ()
repl ctx = do
  s <- getLine
  M.unless (s == ":quit") (f s >> repl ctx)
  where
    f s = do
      let value = L.lookup s ctx
      case value of
        (Just v) -> print v
        _ -> putStrLn $ "Cannot find " ++ s

main :: IO ()
main = do
  args <- getArgs
  files <- traverse readFile args
  let (ast, _) = head $ E.rights $ map (P.runParser P.all') files
  case ast of
    (P.JsonObject obj) -> repl obj
    _ -> putStrLn "Root level must be a JSON object"
