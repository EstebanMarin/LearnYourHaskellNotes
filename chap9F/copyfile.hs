import Control.Exception
import Data.ByteString.Lazy qualified as B
import System.Directory
import System.Environment
import System.IO

main :: IO ()
main = do
  (file1 : file2 : _) <- getArgs
  copy file1 file2

copy :: FilePath -> FilePath -> IO ()
copy source dest = do
  contents <- B.readFile source
  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        B.hPutStr tempHandle contents
        hClose tempHandle
        removeFile dest
        renameFile tempName dest
    )
