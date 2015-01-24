{-# LANGUAGE
    TemplateHaskell
    #-}

module Maven.Types.Pom where

import Control.Lens.TH
import Data.Text as T

import Maven.Types.Dependency

data Pom = Pom
    { _groupId       :: T.Text
    , _artifactId    :: T.Text
    , _version       :: T.Text
    , _dependencyManagement :: Maybe DependencyManagement
    , _dependencies  :: Maybe [Dependency]
    } deriving (Eq, Show)

$(makeLenses ''Pom)
