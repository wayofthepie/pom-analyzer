{-# LANGUAGE
    OverloadedStrings
    #-}

module Pom where

import Control.Monad (liftM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat, (<>))
import qualified Data.Text as T
import Filesystem.Path
import Filesystem.Path.CurrentOS
import System.FilePath.Find
import Text.XML
import Text.XML.Cursor

import qualified Maven.Types.Dependency as D
import qualified Maven.Types.Pom as P

import Prelude hiding (readFile, FilePath)


-- | Get the list of pom files living under the given project's directory.
findPomsIn :: FilePath -> IO [FilePath]
findPomsIn d = liftM (map decodeString) $ find always fp (encodeString d)
    where
        fp = fileName ==? "pom.xml"


-- | Parse a pom file.
parsePom :: Cursor -> P.Pom
parsePom c = do
    let groupId     = getContent c groupIdTag
        artifactId  = getContent c artifactIdTag
        version     = getContent c versionTag
        dependencyMan   = Just $ D.DepMan $ parseDepMan c
        dependencies    = Just $ parseDeps c
    P.Pom groupId artifactId version dependencyMan dependencies


-- | Parse dependencyManagement.
parseDepMan :: Cursor -> [D.Dependency]
parseDepMan c = c $/ element dependencyManagementTag >=> parseDeps


-- | Parse dependencies.
parseDeps :: Cursor -> [D.Dependency]
parseDeps c = c $/
    element dependenciesTag &// element dependencyTag >=> parseDep


-- | Parse dependency.
parseDep :: Cursor -> [D.Dependency]
parseDep c = do
    let groupId     = getContent c groupIdTag
        artifactId  = getContent c artifactIdTag
        version     = Just $ getContent c versionTag
    [D.Dependency groupId artifactId version]


-- Tags (hardcode the namespace for now ...)

modelv4ns   = "http://maven.apache.org/POM/4.0.0"
buildName e = Name e (Just modelv4ns) Nothing
groupIdTag      = buildName "groupId"
artifactIdTag   = buildName "artifactId"
versionTag      = buildName "version"
dependencyManagementTag = buildName "dependencyManagement"
dependenciesTag = buildName "dependencies"
dependencyTag   = buildName "dependency"


-- Helper functions
getContent :: Cursor -> Name -> T.Text
getContent c t = mconcat $ c $/ element t &/ content

-- Temporary helper function
test :: FilePath -> IO P.Pom
test f = do
    doc <- readFile def f
    let cursor = fromDocument doc
    return $ parsePom cursor


{-
buildPomRelationship :: [FilePath] -> Tree a
buildPomRelationship (f:fs) =
-}



