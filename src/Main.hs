module Main where

import App
import Control.Lens

main :: IO ()
main = do
    app  <- initializeApp $ Game "blah"
    startApp app

newtype Game = Game String

instance UserData Game where
    start _ = return $ Game "initialGame"
    input i g = Game $ "game " ++ show (i ^. events)
    step _ = id
    render (Game s) = do {putStrLn s; return $ Game s}
    end _ = putStrLn "That's all folks."

