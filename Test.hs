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

prop_correctInOneIteration :: EndpointArc -> Bool
prop_correctInOneIteration earc = earc ~== correctRadiiSize earc

prop_conversionRetains :: EndpointArc -> Bool
prop_conversionRetains earc =
    earc ~== trace "RESULT:" (traceShowId (centerToEndpoint (endpointToCenter earc)))

return []

runTests = $verboseCheckAll

main = runTests
