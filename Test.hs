{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck
import Debug.Trace
import Test.QuickCheck.All
import Data.AEq ((~==))
import Graphics.SVG.ArcConvert

instance Arbitrary EndpointArc where
    arbitrary = do
        ((x1,y1),(x2,y2)) <- arbitrary `suchThat` (\(u,v) -> u /= v)
        rx                <- arbitrary `suchThat` (>0)
        ry                <- arbitrary `suchThat` (>0)
        phi               <- choose (0,2*pi)
        (fA,fS)           <- arbitrary
        return $ correctRadiiSize (x1, y1, x2, y2, fA, fS, rx, ry, phi)

approxEq (x1a, y1a, x2a, y2a, fAa, fSa, rxa, rya, phia) (x1b, y1b, x2b, y2b, fAb, fSb, rxb, ryb, phib) =
       abs (x1a - x1b  ) < 1
    && abs (y1a - y1b  ) < 1
    && abs (x2a - x2b  ) < 1
    && abs (y2a - y2b  ) < 1
    && abs (y2a - y2b  ) < 1
    && abs (rxa - rxb  ) < 1
    && abs (rya - ryb  ) < 1
    && abs (phia - phib) < 1
    -- && fAa == fAb
    && fSa == fSb

prop_correctInOneIteration :: EndpointArc -> Bool
prop_correctInOneIteration earc =
    let result = correctRadiiSize earc
    in earc ~== trace ("RESULT:" ++ show result) result

prop_conversionRetains :: EndpointArc -> Bool
prop_conversionRetains earc =
    let result = centerToEndpoint (trace ("FIRST:" ++ show (endpointToCenter earc)) (endpointToCenter earc))
    in earc `approxEq` trace ("SECOND:" ++ show result) result

return []

main = $verboseCheckAll
