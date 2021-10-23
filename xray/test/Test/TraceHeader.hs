{-# LANGUAGE CPP #-}
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
{-# OPTIONS -Wno-ambiguous-fields #-}
#endif

module Test.TraceHeader (testTree) where

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import XRay.Prelude
import XRay.Segment
import XRay.TraceHeader
import XRay.TraceId

testTree :: TestTree
testTree = testGroup "TraceHeader" $
  pure $ testGroup "parsing"
  [ testCase "parses from expected strings" $
      forM_ examples $ \(expected, input) ->
        assertEqual
          ("On input: " <> convert input)
          (Right expected)
          (parse input)
  , testCase "fails parsing on invalid inputs" $
      forM_ invalid $ \(expected, input) ->
        assertEqual
          ("On input: " <> convert input)
          (Left $ TraceHeaderFormatError expected)
          (parse input)
  ]

  where
    parse :: Text -> Either TraceHeaderFormatError TraceHeader
    parse = parseTraceHeader

    traceHeader = TraceHeader
      { traceId  = TraceId 1 (2, 3, 4)
      , parentId = empty
      , sampled  = empty
      }

    invalid :: [(Text, Text)]
    invalid =
      [ ("Failed reading: Missing root field", "")
      , ("Failed reading: Missing root field", "Parent=0000000000000001")
      , ("Failed reading: Missing root field", "Parent=0000000000000001;Sampled=1")
      , ("Failed reading: Missing root field", "Sampled=1")
      , ("Failed reading: Missing root field", "Root=")
      , ("endOfInput", "Root=1-00000001-000000020000000300000004;Parent=")
      , ("endOfInput", "Root=1-00000001-000000020000000300000004;Parent=0000000000000001;")
      , ("endOfInput", "Root=1-00000001-000000020000000300000004;Parent=0000000000000001;Sampled=")
      , ("Failed reading: parent: Failed reading: fixedHex", "Root=1-00000001-000000020000000300000004;Parent=xxxxxxxxxxxxxxxx")
      , ("Failed reading: root: Failed reading: fixedHex", "Root=1-xxxxxxxx-000000020000000300000004")
      , ("Failed reading: sampled: Failed reading: Unexpected sampled value", "Root=1-00000001-000000020000000300000004;Sampled=2")
      ]

    examples :: [(TraceHeader, Text)]
    examples =
      [ ( traceHeader
        , "Root=1-00000001-000000020000000300000004"
        )
      , ( traceHeader { sampled = pure True } :: TraceHeader
        , "Root=1-00000001-000000020000000300000004;Sampled=1"
        )
      , ( traceHeader { parentId = pure $ SegmentId 1 } :: TraceHeader
        , "Root=1-00000001-000000020000000300000004;Parent=0000000000000001"
        )
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure True }
        , "Root=1-00000001-000000020000000300000004;Parent=0000000000000001;Sampled=1"
        )
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure False }
        , "Root=1-00000001-000000020000000300000004;Parent=0000000000000001;Sampled=0"
        )
        -- Different order
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure False }
        , "Root=1-00000001-000000020000000300000004;Sampled=0;Parent=0000000000000001"
        )
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure False }
        , "Sampled=0;Root=1-00000001-000000020000000300000004;Parent=0000000000000001"
        )
        -- Extra field
      , ( traceHeader { parentId = pure $ SegmentId 1 } :: TraceHeader
        , "Root=1-00000001-000000020000000300000004;Parent=0000000000000001;App=Specific"
        )
        -- Duplicate valid field Sampled
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure False }
        , "Root=1-00000001-000000020000000300000004;Parent=0000000000000001;Sampled=0;Sampled=1"
        )
        -- Duplicate invalid field Sampled
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure False }
        , "Root=1-00000001-000000020000000300000004;Parent=0000000000000001;Sampled=0;Sampled=x"
        )
        -- Duplicate valid field Parent
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure False }
        , "Root=1-00000001-000000020000000300000004;Parent=0000000000000001;Parent=0000000000000002;Sampled=0"
        )
        -- Duplicate invalid field Parent
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure False }
        , "Root=1-00000001-000000020000000300000004;Parent=0000000000000001;Parent=xxxxxxxxxxxxxxxx;Sampled=0"
        )
        -- Duplicate valid field Root
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure False }
        ,"Root=1-00000001-000000020000000300000004;Root=1-11111111-222222223333333344444444;Parent=0000000000000001;Parent=0000000000000002;Sampled=0"
        )
        -- Duplicate invalid field Root
      , ( traceHeader { parentId = pure $ SegmentId 1, sampled = pure False }
        , "Root=1-00000001-000000020000000300000004;Root=x-xxxxxxxx-222222223333333344444444;Parent=0000000000000001;Parent=0000000000000002;Sampled=0"
        )
      ]
