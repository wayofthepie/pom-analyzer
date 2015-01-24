{-# LANGUAGE
    TemplateHaskell
    #-}

module Maven.Types.Dependency (
    DependencyManagement (DepMan)
    , Dependency (Dependency)
    , groupId
    , artifactId
    , version
    ) where

import Data.Text as T

newtype DependencyManagement = DepMan [Dependency] deriving (Eq, Show)

data Dependency = Dependency
    { _groupId      :: T.Text
    , _artifactId   :: T.Text
    , _version      :: Maybe T.Text
    } deriving (Eq, Show)

groupId     = _groupId
artifactId  = _artifactId
version     = _version
