import Data.List
data Color = Color Int Int Int
    deriving Show

data Shape
    = Circle (Double, Double) Double Color
    | Rectangle (Double, Double) (Double, Double) Color
    deriving Show

data Frankenshape = Frankenshape [Shape] deriving Show

class Svg a where
    toSvg :: a -> String

instance Svg Color where
    toSvg (Color r g b) = "rgb(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"

instance Svg Shape where
    toSvg (Circle (x, y) r c) = "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"" ++ show r ++ "\" fill=\"" ++ toSvg c ++ "\" />\n"
    toSvg (Rectangle (x, y) (w, h) c) = "<rect x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" width =\"" ++ show w ++ "\" height =\"" ++ show h ++ "\" fill=\"" ++ toSvg c ++ "\" />\n"

instance Svg Frankenshape where
    toSvg (Frankenshape shapes) = "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" ++ (intercalate "/n" $ map toSvg shapes) ++ "</svg>"

main = do
    let circle = Circle (200, 200) 99 $ Color 0 128 0
    let rectangle = Rectangle (200, 480) (14, 200) $ Color 0 128 0
    let circle2 = Circle (100, 300) 99 $ Color 0 128 0
    --let rectangle2 = Rectangle (40, 20) (62, 14) $ Color 0 128 255
    let circle3 = Circle (300, 300) 99 $ Color 0 128 0
    let circle4 = Circle (200, 400) 99 $ Color 0 128 0
    let shape = Frankenshape [circle, circle2, circle3, circle4, rectangle]
    writeFile "frankenshape.svg" $ toSvg shape
