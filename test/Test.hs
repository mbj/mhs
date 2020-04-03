import MPrelude
import System.IO (IO)

import qualified DBT
import qualified DBT.Postgresql as DBT
import qualified Data.Text.IO   as Text
import qualified Devtools
import qualified PGT
import qualified PGT.Selector   as PGT
import qualified System.Path    as Path
import qualified Test.Tasty     as Tasty

main :: IO ()
main = do
  Text.putStrLn ""
  success <- PGT.expand ([PGT.Selector $ Path.rel "examples/success.sql"] :: [PGT.Selector])

  DBT.withDatabaseContainer $ \pgConfig -> do
    config <- PGT.configure pgConfig { DBT.databaseName = DBT.DatabaseName "template0" } empty
    Tasty.defaultMain $
      Tasty.testGroup ""
        [ devtools
        , PGT.testTree config success
        ]

devtools :: Tasty.TestTree
devtools = Devtools.testTree Devtools.defaultConfig
