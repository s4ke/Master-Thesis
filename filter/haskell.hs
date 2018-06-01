{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Pandoc
import Text.Pandoc.Walk (walk)

import Text.Pandoc.JSON

import Data.Text (Text)

import System.IO (hPutStr, hClose, hFlush)
import System.Process.Typed
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Concurrent.STM (atomically)
import Control.Exception (throwIO)

import Control.DeepSeq

import System.IO.Unsafe
import System.IO

import Debug.Trace

import Data.String.Utils

import System.Posix.Escape

transformHaskell :: Block -> Block
transformHaskell (Text.Pandoc.CodeBlock attr code) = Text.Pandoc.RawBlock (Format "tex") $ (prettyHaskell $ code)
transformHaskell x = walk transformInline x

transformInline :: Inline -> Inline
transformInline (Text.Pandoc.Code attr code) = Text.Pandoc.RawInline (Format "tex") $ prettyHaskell code
transformInline x = x

{-
 HACKY AF, but seems to work
-}

{-# NOINLINE prettyHaskell #-}
prettyHaskell :: String -> String
prettyHaskell input = unsafePerformIO $ do
    writeFile "escapeInput.tmp" input
    let config = proc "bash" ["filter/escape.sh"]
    (out, err) <- readProcess_ config
    return $ L8.unpack out

main :: IO ()
main = do
   toJSONFilter transformHaskell