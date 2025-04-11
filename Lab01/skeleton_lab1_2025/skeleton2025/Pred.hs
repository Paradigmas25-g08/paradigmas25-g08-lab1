module Pred where

import Dibujo

type Pred a = a -> Bool

--Para la definiciones de la funciones de este modulo, no pueden utilizar
--pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib 

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.

cambiar :: Pred a -> a -> Dibujo a -> Dibujo a
cambiar p f dib = mapDib (\x -> if p x then f else x) dib

-- Alguna básica satisface el predicado.    
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p dib = foldDib (\x -> p x) (\x -> x) (\x -> x) (\x -> x) (\i j x y -> x || y) (\i j x y -> x || y) (\x y -> x || y) dib

-- Todas las básicas satisfacen el predicado
allDib :: Pred a -> Dibujo a -> Bool
allDib p d = foldDib (\x -> p x) (\x -> x) (\x -> x) (\x -> x) (\i j x y -> x && y) (\i j x y -> x && y) (\x y -> x && y) d

-- Hay 4 rotaciones seguidas.
type Acum = (Int, Bool)

esRot360 :: Pred (Dibujo a)
esRot360 dibujo =
  snd $ foldDib
    (const (0, False))  -- fBas: ignora el contenido y devuelve (0, False)
    (\acum -> (fst acum + 1, (snd acum) || (fst acum + 1 >= 4)))  -- fRot: incrementa el contador y evalúa el flag
    (\acum -> (0, snd acum))  -- fEs: reinicia el contador, dejando la flag intacta
    (\acum -> (0, snd acum))  -- fRot45: igual que fEs
    (\x y acum1 acum2 -> (0, (snd acum1) || (snd acum2)))  -- fApi: combina ambos acumuladores, reiniciando el contador
    (\x y acum1 acum2 -> (0, (snd acum1) || (snd acum2)))  -- fJun: idem para juntar
    (\acum1 acum2 -> (0, (snd acum1) || (snd acum2)))        -- fEn: idem para encimar
    dibujo

-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 dibujo =
  snd $ foldDib
    (const (0, False))  -- fBas: ignora el contenido y devuelve (0, False)
    (\acum -> (0, snd acum))  -- fRot: reinicia el contador, dejando la flag intacta
    (\acum -> (fst acum + 1, (snd acum) || (fst acum + 1 >= 2)))  -- fEs: incrementa el contador y evalúa el flag
    (\acum -> (0, snd acum))  -- fRot45: igual que fRot
    (\x y acum1 acum2 -> (0, (snd acum1) || (snd acum2)))  -- fApi: combina ambos acumuladores, reiniciando el contador
    (\x y acum1 acum2 -> (0, (snd acum1) || (snd acum2)))  -- fJun: idem para juntar
    (\acum1 acum2 -> (0, (snd acum1) || (snd acum2)))        -- fEn: idem para encimar
    dibujo

data Superfluo = RotacionSuperflua | FlipSuperfluo 
  deriving (Show)

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]
errorRotacion dib  = if esRot360 dib then [RotacionSuperflua] else []

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
errorFlip dib  = if esFlip2 dib then [FlipSuperfluo] else []

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)
checkSuperfluo dib = if (esRot360 dib || esFlip2 dib) then Left (errorRotacion dib ++ errorFlip dib) else Right dib