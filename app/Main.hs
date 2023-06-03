module Main where

import Data.HashMap.Strict qualified as HMS
import Data.Map.Strict qualified as MS
import Data.Maybe
import GHC.Exts (IsList(..))
import System.Random
import Control.Monad
import Criterion
import Criterion.Main
import Data.Typeable
import Control.DeepSeq

data User = User { age :: Int, name :: String }

instance NFData User where
  rnf (User x y) = rnf x `seq` rnf y

-- | Unique user id that is auto-generated using a uniform distribution
type UserId = String

type UserStorage f = f UserId User

class IsMap f where
  getUserById :: UserStorage f -> UserId -> User
  addUser :: UserStorage f -> (UserId, User) -> UserStorage f
  getAllUsers :: UserStorage f -> [User]

instance IsMap HMS.HashMap where
  getUserById m k = fromJust $ HMS.lookup k m
  addUser m k = uncurry HMS.insert k m
  getAllUsers = HMS.elems

instance IsMap MS.Map where
  getUserById m k = fromJust $ MS.lookup k m
  addUser m k = uncurry MS.insert k m
  getAllUsers = MS.elems

genUserId :: IO UserId
genUserId = replicateM 16 randomIO

genUser :: IO User
genUser = User <$> randomIO <*> replicateM 5 randomIO

setupEnv
  :: (IsList (UserStorage f), Item (UserStorage f) ~ (UserId, User))
  => Int -> IO (UserId, UserStorage f, (UserId, User))
setupEnv n = do
  existingKeys@(key:_) <- replicateM n genUserId
  premadeItems <- zip existingKeys <$> replicateM n genUser
  newItem <- (,) <$> genUserId <*> genUser
  pure (key, fromList premadeItems, newItem)

benches
  :: forall f. (IsMap f, Item (UserStorage f) ~ (UserId, User), NFData (UserStorage f), IsList (UserStorage f))
  => Int -> Benchmark
benches n = env (setupEnv @f n) $ \ ~(ks, m, is) -> bgroup (show n)
  [ bench "getAllUsers" $ nf (getAllUsers) m
  , bench "getUserById" $ nf (getUserById m) ks
  , bench "addUser" $ nf (addUser m) is
  ]

seriesBenches
  :: forall f. (IsMap f, Item (UserStorage f) ~ (UserId, User), Typeable f, NFData (UserStorage f), IsList (UserStorage f))
  => Benchmark
seriesBenches = bgroup (show (typeRep $ Proxy @f)) $
  benches @f <$>
  [ 10000
  , 50000
  , 100000
  , 500000
  , 1000000
  , 5000000
  , 10000000
  ]

main :: IO ()
main = defaultMain
  [ seriesBenches @HMS.HashMap
  , seriesBenches @MS.Map
  ]
