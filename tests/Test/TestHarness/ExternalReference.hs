module Test.TestHarness.ExternalReference
  ( callPythonReference,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Prelude

-- | Call external Python reference implementation with binary I/O
--
-- This function calls an external Python script to compute the expected result.
-- The script receives raw bytes on stdin and outputs raw bytes on stdout.
-- No encoding/decoding overhead - pure binary communication.
--
-- Example:
-- >>> callPythonReference "reference/kyber/prf.py" inputBytes
callPythonReference :: FilePath -> ByteString -> ByteString
callPythonReference scriptPath input = unsafePerformIO $ do
  -- Create process with pipes for stdin/stdout
  (Just hIn, Just hOut, _, ph) <-
    createProcess
      (proc "python3" [scriptPath])
        { std_in = CreatePipe,
          std_out = CreatePipe
        }

  -- Write input bytes to Python's stdin
  BS.hPut hIn input
  hClose hIn

  -- Read output bytes from Python's stdout
  output <- BS.hGetContents hOut

  -- Wait for process to complete
  _ <- waitForProcess ph

  return output
{-# NOINLINE callPythonReference #-}
