
module Maven.Types.Pom (
    Pom (Pom)
    , groupId
    , artifactId
    , version
    , dependencyManagement
    , dependencies
    ) where

import Control.Lens.TH
import Data.Text as T

import Maven.Types.Dependency hiding
    ( groupId
    , artifactId
    , version
    )

data Pom = Pom
    { _groupId       :: T.Text
    , _artifactId    :: T.Text
    , _version       :: T.Text
    , _dependencyManagement :: Maybe DependencyManagement
    , _dependencies  :: Maybe [Dependency]
    } deriving (Eq, Show)

groupId     = _groupId
artifactId  = _artifactId
version     = _version
dependencyManagement= _dependencyManagement
dependencies        = _dependencies


