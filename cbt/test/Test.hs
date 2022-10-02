{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import CBT.Prelude
import Test.Tasty.HUnit

import qualified CBT
import qualified CBT.Container
import qualified CBT.Image
import qualified CBT.Image.BuildDefinition as CBT.Image
import qualified CBT.Image.Name            as CBT.Image
import qualified Devtools
import qualified MRIO.Log                  as Log
import qualified System.Path               as Path
import qualified Test.Tasty                as Tasty

main :: IO ()
main =
  Tasty.defaultMain
    $ Tasty.testGroup "cbt"
    [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "cbt"])
    , container
    , imageDirectory
    , imageStatic
    ]

imageDirectory :: Tasty.TestTree
imageDirectory
  = testCase "image from directory definition"
  . void
  . CBT.runDefaultEnvironment
  $ CBT.Image.buildIfAbsent =<< CBT.Image.fromDirectory imageName (Path.dir "test/app")

imageStatic :: Tasty.TestTree
imageStatic
  = testCase "image from static definition"
  . void
  . CBT.runDefaultEnvironment
  $ CBT.Image.buildIfAbsent buildDefinitionStatic

container :: Tasty.TestTree
container
  = testCase "container"
  . void
  . CBT.runDefaultEnvironment
  $ do
    containerName <- CBT.Container.nextName (CBT.Container.Prefix "cbt-text")

    CBT.Container.withBuildRun
      buildDefinitionStatic
      (CBT.Container.minimalDefinition (getField @"imageName" buildDefinitionStatic) containerName)
        { CBT.Container.command    = pure $ CBT.Container.mkEntrypoint "true"
        , CBT.Container.stopRemove = CBT.Container.StopNoRemove
        , CBT.Container.detach     = CBT.Container.Detach
        }
      (pure ())

buildDefinitionStatic :: CBT.Image.BuildDefinition
buildDefinitionStatic =
  CBT.Image.fromDockerfileContent
    imageName
    (CBT.Image.DockerfileContent "FROM registry.hub.docker.com/library/alpine")

noopLogAction :: Log.Action
noopLogAction = Log.Action . const $ pure ()

imageName :: CBT.Image.Name
imageName = CBT.Image.mkLocalName "cbt-test"
