module Geometry
    ( triangleArea
    , trianglePerimeter
    , squareArea
    , squarePerimeter
    , regPyramidVolume
    , regPyramidSurfaceArea
    , cylinderVolume
    , cylinderSurfaceArea
    ) where

triangleArea :: Float -> Float -> Float
triangleArea a h = (1/2) * (a * h)

trianglePerimeter :: Num a => a -> a -> a -> a
trianglePerimeter a b c = a + b + c

squareArea :: Num a => a -> a
squareArea a = a^2

squarePerimeter :: Num a => a -> a
squarePerimeter a = a * 4

regPyramidVolume :: Float -> Float -> Float -> Float -> Float
regPyramidVolume a r n h = (1/3) * ((n * r * a)/2) * h

regPyramidSurfaceArea :: Float -> Float -> Float -> Float
regPyramidSurfaceArea ap a n = (1/2) * a * n * ap

cylinderVolume :: Float -> Float -> Float
cylinderVolume h r = pi * r^2 * h

cylinderSurfaceArea :: Float -> Float -> Float
cylinderSurfaceArea r h = 2 * pi * r * (h + r)
