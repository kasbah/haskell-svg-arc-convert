{- see http://www.w3.org/TR/SVG11/implnote.html#ArcImplementationNotes -}
module Graphics.SVG.ArcConvert where
import Numeric.Matrix
import Data.Fixed (mod')

type EndpointArc = ( Double, Double, Double, Double
                   , Bool, Bool, Double, Double, Double )

type CenterArc = ( Double, Double, Double, Double
                 , Double, Double, Double )

{- see F.6.5.1 -}
primed :: Double -> Double -> Double -> Double -> Double
       -> (Double, Double)
primed x1 y1 x2 y2 phi = toTuple $
    m [[ cos phi, sin phi]
      ,[-sin phi, cos phi]
      ]
    * m [[(x1 - x2)/2]
        ,[(y1 - y2)/2]
        ]

{- see F.6.5 -}
endpointToCenter :: EndpointArc -> CenterArc
endpointToCenter (x1, y1, x2, y2, fA, fS, rx, ry, phi) =
    let (x1',y1') = primed x1 y1 x2 y2 phi
        (cx',cy') = (sq * rx * y1' / ry, sq * (-ry) * x1' / rx)
                where sq = negateIf (fA == fS) $ sqrt $ abs
                         $ ( rx^2 * ry^2 - rx^2 * y1'^2 - ry^2 * x1'^2 )
                         / ( rx^2 * y1'^2 + ry^2 * x1'^2 )

        (cx,cy)   = toTuple $
                    m [[cos phi, -sin phi]
                      ,[sin phi,  cos phi]
                      ]
                    * m [[cx']
                        ,[cy']
                        ]
                    + m [[(x1 + x2)/2]
                        ,[(y1 + y2)/2]
                        ]

        theta1 = angle (1,0) ( (x1' - cx')/rx, (y1' - cy')/ry )

        dtheta | fS        = if dtheta' < 0
                             then dtheta' + (2*pi)
                             else dtheta'
               | otherwise = if dtheta' > 0
                             then dtheta' - (2*pi)
                             else dtheta'
        dtheta'    = dtheta'' `mod'` (2*pi)
        dtheta''   = angle ((x1' - cx')/rx, (y1' - cy')/ry)
                        (((-x1') - cx')/rx,((-y1') - cy')/ry)

        angle u@(ux,uy) v@(vx,vy) = negateIf ((ux*vy - uy*vx) < 0)
                                  $ acos ((u `dot` v)/(mag u * mag v))

        negateIf b x = if b then -x else x
        dot (ux,uy) (vx,vy) = ux*vx + uy*vy
        mag (x,y)           = sqrt (x^2 + y^2)
    in (cx, cy, rx, ry, phi, theta1, dtheta)

{- see F.6.4 -}
centerToEndpoint :: CenterArc -> EndpointArc
centerToEndpoint (cx, cy, rx, ry, phi, theta1, dtheta) =
    let (x1,y1) = toTuple $
                  m [[cos phi, -sin phi]
                    ,[sin phi,  cos phi]
                    ]
                  * m [[rx * cos theta1]
                      ,[ry * sin theta1]
                      ]
                  + m [[cx]
                      ,[cy]
                      ]
        (x2,y2) = toTuple $
                  m [[cos phi, -sin phi]
                    ,[sin phi,  cos phi]
                    ]
                  * m [[rx * cos (theta1 + dtheta)]
                      ,[ry * sin (theta1 + dtheta)]]
                  + m [[cx]
                      ,[cy]
                      ]
        fA = abs dtheta > pi
        fS = dtheta >= 0
    in (x1, y1, x2, y2, fA, fS, rx, ry, phi)

{- see F.6.6.2 -}
correctRadiiSize :: EndpointArc -> EndpointArc
correctRadiiSize (x1, y1, x2, y2, fA, fS, rx, ry, phi) =
    let (x1',y1') = primed x1 y1 x2 y2 phi
        lambda    = (x1'^2/rx^2) + (y1'^2/ry^2)
        (rx',ry') | lambda <= 1 = (rx, ry)
                  | otherwise   = ((sqrt lambda) * rx, (sqrt lambda) * ry)
    in (x1, y1, x2, y2, fA, fS, rx', ry', phi)

m :: [[Double]] -> Matrix Double
m = fromList

toTuple :: Matrix Double -> (Double, Double)
toTuple = (\[[x],[y]] -> (x,y)) . toList
