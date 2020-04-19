-- file: ch03/AlgebraicVector.hs
module AlgebraicVector where

-- x and y coordinates or lengths.
data Cartesian2D = Cartesian2D {
        xPos :: Double
    ,   yPos :: Double
    }
    deriving (Eq, Show)

-- Angle and distance (magnitude).
data Polar2D = Polar2D {
        polarRadius :: Double
    ,   polarAngle :: Double
    }
    deriving (Eq, Show)
