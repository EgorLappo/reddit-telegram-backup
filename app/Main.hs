module Main (main) where

import App (app, runApp) 
import Config (defaultConfig)

import qualified Data.Text.IO as T

main :: IO ()
main = do 
    result <- runApp defaultConfig app
    case result of 
        (Left e) -> T.putStrLn $ "Error encountered:\n" <> e 
        (Right res) -> T.putStrLn $ res <> "\nDone!" 
