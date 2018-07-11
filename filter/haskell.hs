{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Pandoc
import Text.Pandoc.Walk (walk)

import Text.Pandoc.JSON

import Data.Text (Text)

import System.Process.Typed
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Concurrent.STM (atomically)
import Control.Exception (throwIO)

import Data.Map

import System.IO
import System.IO.Unsafe

import Debug.Trace

import Data.String.Utils

transformHaskell :: Block -> Block
transformHaskell x@(Text.Pandoc.CodeBlock attr code) =
    if isHaskell attr
    then (Text.Pandoc.RawBlock (Format "tex") $ prettyHaskellCodeBlock attr code)
    else if isC attr
         then (Text.Pandoc.RawBlock (Format "tex") $ prettyCFig attr code)
         else x
transformHaskell x = walk transformInline x

transformInline :: Inline -> Inline
transformInline (Text.Pandoc.Code attr code) = Text.Pandoc.RawInline (Format "tex") $ prettyHaskellInline code
transformInline x = x

{-
 HACKY AF, but seems to work

 TODO: we require a variant without a figure environment!
-}
prettyHaskellFig :: Attr -> String -> String
prettyHaskellFig (ident, classes, kvs) input =
    "\\begin{figure}[" ++ options kvs ++ "]\n" ++
    "\\centering" ++
    (prettyHaskell $ "\n\\begin{code}\n" ++ input ++ "\n\\end{code}\n") ++
    caption kvs ++
    label ident ++
    "\\end{figure}"

prettyCFig :: Attr -> String -> String
prettyCFig (ident, classes, kvs) input =
    "\\begin{figure}[" ++ options kvs ++ "]\n" ++
    "\\lstset{language=C}\n" ++
    "\\centering\n" ++
    "\\begin{lstlisting}" ++
    "\n" ++ input ++ "\n" ++
    "\\end{lstlisting}\n" ++
    caption kvs ++
    label ident ++
    "\\end{figure}"

prettyHaskellCodeBlock :: Attr -> String -> String
prettyHaskellCodeBlock attr@(ident, classes, kvs) input =
    if isFigure attr
    then prettyHaskellFig attr input
    else (prettyHaskell $ "\n\\begin{code}\n" ++ input ++ "\n\\end{code}\n")

prettyHaskellInline :: String -> String
prettyHaskellInline input = prettyHaskell $ "|" ++ input ++ "|"

isHaskell :: Attr -> Bool
isHaskell (ident, classes, kvs) = "haskell" `elem` classes

isC :: Attr -> Bool
isC (ident, classes, kvs) = "c" `elem` classes

isFigure :: Attr -> Bool
isFigure (ident, classes, kvs) = "figure" `elem` classes

options :: [(String, String)] -> String
options kvs = (prettyHaskell $ findWithDefault "" "options" (fromList kvs))

caption :: [(String, String)] -> String
caption kvs = "\\caption{" ++ (prettyHaskell $ findWithDefault "." "caption" (fromList kvs)) ++ "}"

label :: String -> String
label ident =
    if ((length ident) > 0)
    then "\\label{" ++ ident ++ "}"
    else ""

{-# NOINLINE prettyHaskell #-}
{-
prettyHaskell :: String -> String
prettyHaskell input = unsafePerformIO $ do
    writeFile "escapeInput.tmp" (input)
    let config = proc "bash" ["filter/escape.sh"]
    (out, err) <- readProcess_ config
    return $ L8.unpack out
-}
prettyHaskell = id


main :: IO ()
main = do
   toJSONFilter $ transformHaskell -- . traceShowId