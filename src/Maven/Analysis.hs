

module Maven.Analysis where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
--import Data.HashMap.Lazy hiding (map)
import Data.Tree
import Data.Tree.Lens
import Filesystem.Path
import Filesystem.Path.CurrentOS
import System.FilePath.Find
import Text.XML
import Text.XML.Cursor

import Maven.Parser.Pom
import Maven.Types.Pom

import Prelude hiding (readFile, FilePath)


-- | Get the list of pom files living under the given project's directory.
findPomsIn :: FilePath -> IO [FilePath]
findPomsIn d = liftM (map decodeString) $ find always fp (encodeString d)
    where
        fp = fileName ==? "pom.xml"

parsePoms :: [FilePath] -> IO [Pom]
parsePoms fs = sequence $ map readAndParse fs

readAndParse :: FilePath -> IO Pom
readAndParse f = do
    doc <- readFile def f
    let cursor = fromDocument doc
    print "Parsing..."
    return $ parsePom cursor


