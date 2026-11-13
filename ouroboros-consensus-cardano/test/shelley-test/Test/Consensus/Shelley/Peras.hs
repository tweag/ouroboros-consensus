{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.Peras (tests) where

import Cardano.Ledger.Core (mkBasicBlockBody)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Ouroboros.Consensus.Shelley.HFEras (StandardDijkstraBlock)
import Ouroboros.Consensus.Shelley.Ledger.Block
  ( ShelleyPerasCertCompatibleWithLedger (..)
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
        "Inject then extract through the Dijkstra block body"
        prop_DijkstraPerasCertRoundtrip
    ]

prop_DijkstraPerasCertRoundtrip :: Property
prop_DijkstraPerasCertRoundtrip =
  forAll (genPerasCert @StandardDijkstraBlock True) $ \cert ->
    extractPerasCertFromShelleyBlockBody
      (injectPerasCertIntoShelleyBlockBody cert (mkBasicBlockBody @DijkstraEra))
      === Just cert
