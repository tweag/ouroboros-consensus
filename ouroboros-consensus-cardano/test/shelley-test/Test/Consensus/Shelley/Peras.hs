{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.Peras (tests) where

import Cardano.Ledger.Core (mkBasicBlockBody)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Ouroboros.Consensus.Block (BlockProtocol)
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Node.Peras
  ( setShelleyBlockBodyPerasCert
  , shelleyBlockBodyPerasCert
  )
import Test.QuickCheck
  ( Property
  , forAll
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Peras (genPerasCert)

tests :: TestTree
tests =
  testGroup
    "ShelleyBlockPerasCert"
    [ testProperty
        "Set then read the Peras certificate of a Dijkstra block body"
        prop_DijkstraPerasCertRoundtrip
    ]

prop_DijkstraPerasCertRoundtrip :: Property
prop_DijkstraPerasCertRoundtrip =
  forAll (genPerasCert @StandardDijkstraBlock True) $ \cert ->
    shelleyBlockBodyPerasCert @(BlockProtocol StandardDijkstraBlock)
      ( setShelleyBlockBodyPerasCert @(BlockProtocol StandardDijkstraBlock)
          (Just cert)
          (mkBasicBlockBody @DijkstraEra)
      )
      === Just cert
