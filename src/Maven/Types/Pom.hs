
module Maven.Types.Pom (
    Pom (Pom)
    , DependencyManagement(DepMan)
    , Dependency(..)
    , Parent(..)
    , groupId
    , artifactId
    , version
    , dependencies
    , dependencyManagement
    ) where


import Data.Text as T


data Pom = Pom
    { _groupId      :: Maybe T.Text -- Can be taken from the parent.
    , _artifactId   :: T.Text
    , _version      :: Maybe T.Text
    , _parent       :: Maybe Parent
    , _dependencyManagement :: Maybe DependencyManagement
    , _dependencies         :: Maybe [Dependency]
    } deriving (Eq, Show)

newtype Parent = Parent Dependency deriving (Eq, Show)

newtype DependencyManagement = DepMan [Dependency] deriving (Eq, Show)

-- It would be better to express version as a Maybe as it can be missing,
-- currently it will just appear as an empty String
data Dependency = Dependency
    -- | groupId
    ( Maybe T.Text )
    -- | artifactId
    T.Text
    -- | version
    ( Maybe T.Text ) deriving (Eq, Show)



class HasPackageInfo a where
    groupId     :: a -> Maybe T.Text
    artifactId  :: a -> T.Text
    version     :: a -> Maybe T.Text


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
