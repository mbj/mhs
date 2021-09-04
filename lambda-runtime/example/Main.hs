-- | Lambda CLI example command:
--
-- cli-example -e '{"name": "allan"}' -f output.json
--
module Main where

import AWS.Prelude

import qualified AWS.Lambda.CLI as CLI
import qualified Data.Aeson     as JSON

main :: IO ()
main = CLI.run () lambdaFn
  where
    lambdaFn :: JSON.Value -> RIO () JSON.Value
    lambdaFn = pure
