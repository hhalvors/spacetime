import Graphics.Gloss

-- Hyperbola function y^2 = x^2 - 1
hyperbola :: Float -> Float
hyperbola x = sqrt(x^2 - 1)

-- Function to draw the orthogonal line
drawLine :: Float -> Picture
drawLine x = line [(x - lineLength, y - lineLength * slope), (x + lineLength, y + lineLength * slope)]
    where
        y = hyperbola x
        slope = -(y / x)  -- Slope of the line orthogonal to the tangent
        lineLength = 50   -- Length of the line

-- Function to move the dot and the line
moveDotAndLine :: Float -> Picture
moveDotAndLine time = pictures [translate x y $ color red $ circleSolid 5, drawLine x]
    where
        x = (time * 10)   -- x increases with time
        y = hyperbola x  -- calculate y based on x

-- Main function to create the window and animate the dot and line
main :: IO ()
main = animate (InWindow "Hyperbola Animation with Orthogonal Line" (600, 600) (10, 10)) white moveDotAndLine

-- Function to create the line with a given slope
lineWithSlope :: Float -> Picture
lineWithSlope t = Line [(0, 0), (300, 300 * t)]

-- Function to update the line's slope over time
animateLine :: Float -> Picture
animateLine t = lineWithSlope t

-- Main function to run the animation
mainNew :: IO ()
mainNew = animate (InWindow "Line Animation" (600, 600) (10, 10)) white animateLine
