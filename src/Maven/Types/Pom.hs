
module Maven.Types.Pom (
    Pom (Pom)
    , DependencyManagement(DepMan)
    , Dependency(..)
    , groupId
    , artifactId
    , version
    , dependencies
    , dependencyManagement
    ) where


import Data.Text as T


data Pom = Pom
    { _groupId       :: T.Text
    , _artifactId    :: T.Text
    , _version       :: T.Text
    , _dependencyManagement :: Maybe DependencyManagement
    , _dependencies  :: Maybe [Dependency]
    } deriving (Eq, Show)

newtype DependencyManagement = DepMan [Dependency] deriving (Eq, Show)

-- It would be better to express version as a Maybe as it can be missing,
-- currently it will just appear as an empty String


data Dependency = Dependency
    -- | groupId
    T.Text
    -- | artifactId
    T.Text
    -- | version
    T.Text deriving (Eq, Show)


class HasPackageInfo a where
    groupId     :: a -> T.Text
    artifactId  :: a -> T.Text
    version     :: a -> T.Text


instance HasPackageInfo Dependency where
    groupId (Dependency gid _ _)    = gid
    artifactId (Dependency _ aid _) = aid
    version (Dependency _ _ v)      = v


instance HasPackageInfo Pom where
    groupId    = _groupId
    artifactId = _artifactId
    version    = _version


dependencyManagement= _dependencyManagement
dependencies        = _dependencies
