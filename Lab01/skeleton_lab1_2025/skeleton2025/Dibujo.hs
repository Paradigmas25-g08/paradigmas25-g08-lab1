module Dibujo where

-- Definir el lenguaje via constructores de tipo
data Dibujo a =  Basica a 
              | Rotar (Dibujo a)
              | Rotar45 (Dibujo a)  
              | Espejar (Dibujo a) 
              | Apilar Float Float (Dibujo a) (Dibujo a) 
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)


-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 1 a = f $ a
comp f n a = comp f (n-1) (f $ a)


-- Rotaciones de múltiplos de 90.
r90 :: Dibujo a -> Dibujo a
r90 dib = Rotar (dib)

r180 :: Dibujo a -> Dibujo a
r180 dib = comp r90 2 dib

r270 :: Dibujo a -> Dibujo a
r270 dib = comp r90 3 dib



-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) dib1 dib2 = Apilar 1.0 1.0 dib1 dib2


-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) dib1 dib2 = Juntar 1.0 1.0 dib1 dib2


-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) dib1 dib2 = Encimar dib1 dib2

-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto dib1 dib2 dib3 dib4 = (.-.) ((///) dib2 dib1) ((///) dib3 dib4)

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 dib = (^^^) (  (^^^) (  (^^^) (r180 dib) (r270 dib) ) (r90 dib)  ) (dib)


-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar dib = (.-.) ((///) (r90 dib) dib) ((///) (r180 dib) (r270 dib))

-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib a = Basica a


-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica a) = Basica (f a)
mapDib f (Rotar dib) = Rotar (mapDib f dib)
mapDib f (Rotar45 dib) = Rotar45 (mapDib f dib) 
mapDib f (Espejar dib) = Espejar (mapDib f dib)
mapDib f (Apilar x y dib1 dib2) = Apilar x y (mapDib f dib1) (mapDib f dib2) 
mapDib f (Juntar x y dib1 dib2) = Juntar x y (mapDib f dib1) (mapDib f dib2)
mapDib f (Encimar dib1 dib2) = Encimar (mapDib f dib1) (mapDib f dib2)


-- Funcion de fold para Dibujos a
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
      (b -> b -> b) ->
       Dibujo a -> b

foldDib fBas fRot fEs fRot45 fApi fJun fEn d =
       let foldDibRecur = foldDib fBas fRot fEs fRot45 fApi fJun fEn
       in case d of
              Basica x -> fBas x
              Rotar d -> fRot $ foldDibRecur d
              Espejar d -> fEs $ foldDibRecur d
              Rotar45 d -> fRot45 $ foldDibRecur d
              Apilar x y dib1 dib2 -> fApi x y (foldDibRecur dib1) (foldDibRecur dib2)
              Juntar x y dib1 dib2 -> fJun x y (foldDibRecur dib1) (foldDibRecur dib2)
              Encimar dib1 dib2 -> fEn (foldDibRecur dib1) (foldDibRecur dib2)






