module Main where

import Interpreter (interpret)
import Language
import Parser

import Graphics.Rendering.Cairo
import System.Environment
import System.Exit
import Text.Megaparsec

main :: IO ()
main = do
  args <- getArgs
  if (length args) < 2
    then die "Usage: cabal run postscript -- PSFILE OUTFILE"
    else return ()
  parsingRes <- parseFromFile (args !! 0)
  case parsingRes of
    Left e -> putStr (errorBundlePretty e)
    Right ps -> runInterpreter (args !! 1) interpret ps

runInterpreter :: Show s => String -> (PSExpr -> Render (Result s)) -> PSExpr -> IO ()
runInterpreter outf intp ps = do
    surface <- createImageSurface FormatRGB24 512 512
    out <- renderWith surface (do
      translate 0 512
      scale 1 (-1)
      setSourceRGB 1.0 1.0 1.0
      paint
      setSourceRGB 0 0 0
      intp ps)
    surfaceWriteToPNG surface outf
    case out of
      Left (err, state) -> do
        putStrLn $ "Error!\nError message: " ++ err
        putStrLn "Final state:"
        print state
      Right state -> do
        putStrLn $ "Success! Wrote " ++ outf ++ "\nFinal state:"
        print state

