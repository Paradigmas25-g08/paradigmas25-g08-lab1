module Basica.Escher where
import Graphics.Gloss
import Dibujo
import Interp
import Basica.Comun
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- supongamos que eligen
type Escher = Triangulo
            | Vacio

ejemplo :: Dibujo Escher
ejemplo = Espejar(Triangulo)

-- el dibujo u
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = Encimar(Encimar(p2,Rotar(p2)),Encimar(Rotar(Rotar(p2)),Rotar(Rotar(Rotar(p2)))))
    where
        p2 = Espejar(Rotar45(p))
-- el dibujo t
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT d = Encimar(d,Encimar(d2,d3))
    where
        d2 = Espejar(Rotar45(d))
        d3 = Rotar(Rotar(Rotar(d2))) 


-- lado con nivel de detalle
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado n d
    | n < 0 = Vacio  -- no deberia pasar
    | n == 1 = cuarteto (Vacio) (Vacio) (Rotar d) d
    | n > 1 =
        let l = lado (n-1) d
        in cuarteto l l (Rotar d) d

-- esquina con nivel de detalle en base a la figura p
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina n d
    | n < 0 = Vacio  -- no deberia pasar
    | n == 1 = cuarteto (Vacio) (Vacio) (Vacio) (dibujoU d)
    | n > 1 =
        let l = lado (n-1) d
        in cuarteto (esquina (n-1) d) l (Rotar l) (dibujoU d)

-- por suerte no tenemos que poner el tipo!
noneto p q r s t u v w x =
    let pq = Juntar 2 1 p q
        pqr = Juntar 1 3 pq r
        st = Juntar 2 1 s t
        stu = Juntar 1 3 st u
        vw = Juntar 2 1 v w
        vwx = Juntar 1 3 vw x
    in Encimar 1 3 (Encimar 2 1 pqr stu) vwx

-- el dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher n e =
    let p = esquina n $ pureDib e
        q = lado n $ pureDib e
        r = r270 p
        s = Rotar q
        t = pureDib e
        u = r270 q
        v = Rotar p
        w = r180 q
        x = r180 p
    in noneto p q r s t u v w x

interpBas :: Escher -> ImagenFlotante
interpBas Triangulo x y z = triangulo x y z
interpBas Vacio x y z = blank