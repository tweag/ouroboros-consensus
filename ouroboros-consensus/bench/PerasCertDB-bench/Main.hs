{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

import Criterion.Main
import Criterion.Types qualified as Criterion
import Data.List (iterate')
import Data.Map.Strict qualified as Map
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Block (PerasWeight (PerasWeight))
import Ouroboros.Consensus.Storage.PerasCertDB.API
  ( PerasWeightSnapshot (..)
  , boostedWeightForFragment
  )
import Ouroboros.Network.AnchoredFragment qualified as AF
import Test.Util.TestBlock (TestBlock)
import Test.Util.TestBlock qualified as TestBlock

-- we aim to benchmark PerasCertDB.API.boosterWeightForFragment.
-- For that, we need to generate the arguments of this function:
-- - PerasWeightSnapshot blk
-- - AnchoredFragment blk
--
-- or perhaps we could get away with a fixed number of input sequences.
-- It would be enough to generate a fixed progressions of anchored fragments
-- with length from 0 to 7k, as we would not never have longer ones in a caught-up node.
--
-- How to generate a reasonable weight snapshot? Do we need a separate snapshot or every
-- chain fragment, or is one over-approximated one enough? Probably we need separate ones,
-- as the traversal of the underlying map in the snapshot is something that we actually
-- are trying to benchmark.
--
-- the blk parameter should probably instantiated to TB.TestBlock.
-- Or do we need some sort of a payload for Peras?
--
--

main :: IO ()
main =
  defaultMainWith criterionConfig $ map benchBoostedWeightForFragment generateInputs
 where
  generateInputs :: [(Natural, (PerasWeightSnapshot TestBlock, AF.AnchoredFragment TestBlock))]
  generateInputs =
    getEveryN samplingRate $
      take k $
        zip [0 ..] $
          zip (map uniformWeightSnapshot fragments) fragments

  k :: Int
  k = 2160

  samplingRate :: Natural
  samplingRate = 500

  criterionConfig :: Criterion.Config
  criterionConfig = defaultConfig

benchBoostedWeightForFragment ::
  (Natural, (PerasWeightSnapshot TestBlock, AF.AnchoredFragment TestBlock)) -> Benchmark
benchBoostedWeightForFragment (i, (weightSnapshot, fragment)) =
  bench ("boostedWeightForFragment of length " <> show i) $
    whnf (boostedWeightForFragment weightSnapshot) fragment

-- | An infinite list of chain fragments
fragments :: [AF.AnchoredFragment TestBlock]
fragments = iterate' addSuccessorBlock genesisFragment
 where
  genesisFragment :: AF.AnchoredFragment TestBlock
  genesisFragment = AF.Empty AF.AnchorGenesis

  addSuccessorBlock :: AF.AnchoredFragment TestBlock -> AF.AnchoredFragment TestBlock
  addSuccessorBlock = \case
    AF.Empty _ -> (AF.Empty AF.AnchorGenesis) AF.:> (TestBlock.firstBlock 0)
    (xs AF.:> x) -> (xs AF.:> x) AF.:> TestBlock.successorBlock x

-- | Given a chain fragment, construct a weight snapshot where there's a boosted block every 300 slots
uniformWeightSnapshot :: AF.AnchoredFragment TestBlock -> PerasWeightSnapshot TestBlock
uniformWeightSnapshot fragment =
  let pointsToBoost = map snd . getEveryN 300 . zip [0 ..] . map AF.blockPoint . AF.toOldestFirst $ fragment
      weights = map PerasWeight $ repeat 15
   in PerasWeightSnapshot{getPerasWeightSnapshot = Map.fromList $ zip pointsToBoost weights}

getEveryN :: Natural -> [(Natural, a)] -> [(Natural, a)]
getEveryN n = filter (\(i, _) -> (i `mod` n) == 0)
