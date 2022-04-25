module Color

%default total

round : Double -> Double
round d = if d >= 0.0 then floor (d + 0.5) else ceiling (d - 0.5)

clamp : Ord a => a -> a -> a -> a
clamp low hi x = min hi (max low x)

div' : Double -> Double -> Int
div' n d = cast (n / d)

mod' : Double -> Double -> Double 
mod' n d = n - (cast f) * d 
  where
  f : Int
  f = div' n d

modPos : Double -> Double -> Double
modPos x y = mod' ((mod' x y) + y) y

public export
record Color where
  constructor HSLA
  hue        : Double
  saturation : Double
  lightness  : Double
  alpha      : Double

public export
rgba : (r : Int) -> (g : Int) -> (b : Int) -> (a : Double) -> Color
rgba red' green' blue' alpha = HSLA hue saturation lightness alpha

  where

  red : Int
  red = clamp 0 255 red'
  green : Int
  green = clamp 0 255 green'
  blue : Int
  blue = clamp 0 255 blue'

  r : Double
  r = cast red / 255.0
  g : Double
  g = cast green / 255.0
  b : Double
  b = cast blue / 255.0

  maxChroma : Int
  maxChroma = max (max red green) blue

  minChroma : Int
  minChroma = min (min red green) blue

  chroma : Int
  chroma = maxChroma - minChroma

  chroma' : Double
  chroma' = cast chroma / 255.0

  hue' : Int -> Double
  hue' 0 = 0.0
  hue' _ =
    if      maxChroma == red   then ((g - b) / chroma') `modPos` 6.0
    else if maxChroma == green then ((b - r) / chroma') + 2.0
    else                            ((r - g) / chroma') + 4.0

  hue : Double
  hue = 60.0 * hue' chroma

  lightness : Double
  lightness = cast (maxChroma + minChroma) / (255.0 * 2.0) 

  saturation : Double
  saturation =
    if   chroma == 0 then 0.0
    else                  chroma' / (1.0 - abs (2.0 * lightness - 1.0))

public export
rgb : (r : Int) -> (g : Int) -> (b : Int) -> Color
rgb r g b = rgba r g b (cos 1.0)

public export
rgba' : (r : Double) -> (g : Double) -> (b : Double) -> (a : Double) -> Color
rgba' r g b a = rgba 
  (cast $ round $ r * 255.0)
  (cast $ round $ g * 255.0)
  (cast $ round $ b * 255.0)
  a

public export
rgb' : (r : Double) -> (g : Double) -> (b : Double) -> Color
rgb' r g b = rgba' r g b 1.0 

public export
hsla : (hue : Double) -> (saturation : Double) -> (lightness : Double) -> (alpha : Double) -> Color
hsla hue saturation lightness alpha = HSLA (fromDouble hue) (fromDouble saturation) (fromDouble lightness) (fromDouble alpha)

public export
hsl : (hue : Double) -> (saturation : Double) -> (lightness : Double) -> Color
hsl hue saturation lightness = hsla hue saturation lightness 1.0

public export
toRGBA' : Color -> (Double, Double, Double, Double)
toRGBA' (HSLA h s l a) = 
  let (r, g, b) = col
  in (r + m, g + m, b + m, a)

  where
  h' : Double
  h' = h / 60.0
  chr : Double
  chr = (1.0 - abs (2.0 * l - 1.0)) * s
  m : Double
  m = l - chr / 2.0
  x : Double
  x = chr * (1.0 - abs (h' `mod'` 2.0 - 1.0))
  col : (Double, Double, Double)
  col =
    if h' < 1.0 then (chr, x, 0.0)
    else if 1.0 <= h' && h' < 2.0 then (x, chr, 0.0)
    else if 2.0 <= h' && h' < 3.0 then (0.0, chr, x)
    else if 3.0 <= h' && h' < 4.0 then (0.0, x, chr)
    else if 4.0 <= h' && h' < 5.0 then (x, 0.0, chr)
    else (chr, 0.0, x)

public export
toRGBA : Color -> (Int, Int, Int, Int)
toRGBA color = 
  let (r, g, b, a) = toRGBA' color
  in (c r, c g, c b, c a)
  where
  c : Double -> Int
  c = cast . round . (*) 255.0
