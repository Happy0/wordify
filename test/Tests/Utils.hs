module Tests.Utils (withTempFile) where

	import Control.Exception (bracket)
	import System.IO (Handle, hClose, hFlush, hIsOpen, hIsWritable, openTempFile)
	import Control.Monad (when)
	import System.Directory (removeFile, getTemporaryDirectory)

	withTempFile :: (FilePath -> Handle -> IO a) -> IO a
	withTempFile = bracket (getTemporaryDirectory >>= \tempDir -> openTempFile tempDir "scrabbleTest.txt") cleanupTemp . uncurry
	  where
	    cleanupTemp (path,h) = do
	      open <- hIsOpen h
	      when open (hClose h)
	      removeFile path