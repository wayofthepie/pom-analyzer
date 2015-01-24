{-# LANGUAGE
    TemplateHaskell
    #-}

module Maven.Types.Dependency where

import Control.Lens.TH
import Data.Text as T

newtype DependencyManagement = DepMan [Dependency] deriving (Eq, Show)

data Dependency = Dependency
    { _groupId      :: T.Text
    , _artifactId   :: T.Text
    , _version      :: Maybe T.Text
    } deriving (Eq, Show)

$(makeLenses ''DependencyManagement)
$(makeLenses ''Dependency)

