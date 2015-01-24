

module Maven.Analysis where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Filesystem.Path
import Filesystem.Path.CurrentOS
import System.FilePath.Find
import Text.XML
import Text.XML.Cursor

import Maven.Parser.Pom
import Maven.Types.Pom as P

import Prelude hiding (readFile, FilePath)


-- | Get the list of pom files living under the given project's directory.
findPomsIn :: FilePath -> IO [FilePath]
findPomsIn d = liftM (map decodeString) $ find always fp (encodeString d)
    where
        fp = fileName ==? "pom.xml"

parsePoms :: [FilePath] -> [IO P.Pom]
parsePoms fs = map readAndParse fs

-- Temporary helper function
readAndParse :: FilePath -> IO P.Pom
readAndParse f = do
    doc <- readFile def f
    let cursor = fromDocument doc
    print "Parsing..."
    return $ parsePom cursor



