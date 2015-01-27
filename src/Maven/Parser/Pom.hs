{-# LANGUAGE
    OverloadedStrings
    #-}

module Maven.Parser.Pom where

import Control.Lens.At
import Control.Lens.Fold
import Control.Monad (liftM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Monoid (mconcat, (<>))
import qualified Data.Text as T
import Filesystem.Path hiding (null)
import Filesystem.Path.CurrentOS hiding (null)
import Text.XML
import Text.XML.Cursor

import qualified Maven.Types.Pom as P

import Prelude hiding (readFile, FilePath)



-------------------------------------------------------------------------------
-- | Parse a pom file.
parsePom :: Cursor -> P.Pom
parsePom c = do
    let groupId     = text2maybe $ getContent c groupIdTag
        artifactId  = getContent c artifactIdTag
        version     = text2maybe $ getContent c versionTag
        parent      = liftM P.Parent $ ( parseParent c ) ^? ix 0
        dependencyMan   = liftM P.DepMan $ list2maybe $ parseDepMan c
        dependencies    = list2maybe $ parseDeps  c
        modules     = list2maybe $ parseModules c
    P.Pom groupId artifactId version
        parent dependencyMan dependencies modules


-------------------------------------------------------------------------------
-- | Parse dependencyManagement.
parseDepMan :: Cursor -> [P.Dependency]
parseDepMan c = c $/ element dependencyManagementTag >=> parseDeps


-------------------------------------------------------------------------------
-- | Parse dependencies.
parseDeps :: Cursor -> [P.Dependency]
parseDeps c = c $/ element dependenciesTag &// element dependencyTag >=> parseDep


-------------------------------------------------------------------------------
-- | Parse dependency.
parseDep :: Cursor -> [P.Dependency]
parseDep c = do
    let groupId     = text2maybe $ getContent c groupIdTag
        artifactId  = getContent c artifactIdTag
        version     = text2maybe $ getContent c versionTag
    [P.Dependency groupId artifactId version]


-------------------------------------------------------------------------------
-- | Parse parent.
parseParent :: Cursor -> [P.Dependency]
parseParent c = c $/ element parentTag >=> parseDep


-------------------------------------------------------------------------------
-- | Parse modules.
parseModules :: Cursor -> [T.Text]
parseModules c = c $/ element modulesTag >=> parseModule


-------------------------------------------------------------------------------
-- | Parse module.
parseModule :: Cursor -> [T.Text]
parseModule c = c $/ element moduleTag &/ content


-------------------------------------------------------------------------------
-- Tags (hardcode the namespace for now ...)
modelv4ns   = "http://maven.apache.org/POM/4.0.0"
buildName e = Name e (Just modelv4ns) Nothing
groupIdTag      = buildName "groupId"
artifactIdTag   = buildName "artifactId"
versionTag      = buildName "version"
parentTag       = buildName "parent"
dependencyManagementTag = buildName "dependencyManagement"
dependenciesTag = buildName "dependencies"
dependencyTag   = buildName "dependency"
modulesTag      = buildName "modules"
moduleTag       = buildName "module"


-------------------------------------------------------------------------------
-- Helper functions
getContent :: Cursor -> Name -> T.Text
getContent c t = mconcat $ c $/ element t &/ content

text2maybe :: T.Text -> Maybe T.Text
text2maybe t | t == T.empty = Nothing
             | otherwise = Just t

list2maybe :: [a] -> Maybe [a]
list2maybe l = if null l then Nothing else Just l

