module Main where

import System.Environment
import System.Exit
import Data.Word
import qualified Data.ByteString as B
import Data.ByteString.Base64
import System.FilePath
import System.IO
import Data.Char

main = getArgs >>= parse

parse (package:files) = 
	putStrLn ( "module " ++ package ++ " where" )
	>> mapM_ genFile (numerateFiles 0 files) 
	>> exitSuccess
parse [] = hPutStrLn stderr "Need at lest 2 arguments: package name and file(s) to encode" 
	>> exitFailure

numerateFiles _ [] = []
numerateFiles num (x:xs) = (num, x):(numerateFiles (num+1) xs)

genFile (num, file) =
	putStrLn ("-- encoded " ++ file ++ " file")
	>> B.readFile file 
	>>= (\byteString -> B.putStrLn $ genHsFile (encode byteString) num)

genHsFile content num = B.concat [(B.pack $ toWord8 $ "d" ++ show num ++ " = \"" ), content, (B.pack $ toWord8 "\"")] 
	where toWord8 = map (fromIntegral . ord)

