module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture
type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)


-- Interpretaciones de los constructores de Dibujo

--interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar f = \x w h -> f (x V.+ w)(h)(V.negate w)

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar f = \x w h -> f (x V.+ w) (V.negate w) (h)

--interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 f = \x w h ->  f (x V.+ (mitad (w V.+ h))) (mitad (w V.+ h)) (mitad (h V.- w))

--interpreta el operador de apilar
interp_apilar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar n m f g = img 
    where 
        img x w h = pictures [(f(x V.+ h') (w) (r V.* h)), (g(x) (w) (h'))]
            where   r' = n/(m+n)
                    r = m/(m+n)
                    h'= r' V.* h

--interpreta el operador de juntar
interp_juntar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar n m f g = img
    where 
        img x w h = pictures[(f(x) (w') (h)), (g(x V.+ w') (r' V.* w) (h))]
            where   r' = n/(m+n)
                    r = m/(m+n)
                    w'= r V.* w

--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar f g = \x w h -> pictures[(f(x)(w)(h)), (g(x)(w)(h))]

--interpreta cualquier expresion del tipo Dibujo a  
--utilizar foldDib 
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp f d = foldDib (\x -> f x) (\x -> interp_rotar x)(\x -> interp_espejar x)(\x -> interp_rotar45 x)(\i j x y -> interp_apilar i j x y)(\i j x y -> interp_juntar i j x y) (\x y -> interp_encimar x y) d