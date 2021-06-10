module Library where
import PdePreludat

-- Parcial Functional Master Series

-- Nombre: Apellido, Nombre (reemplazar por el tuyo)
-- Legajo: 999999-9 (reemplazar por el tuyo)

type Palabra = String
type Verso = String
type Estrofa = [Verso]
 
esVocal :: Char -> Bool
esVocal = flip elem "aeiou"
 
tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"
 
cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f op v1 v2 = f v1 `op` f v2
 
-- 1) Saber si dos palabras riman. Existen las rimas asonantes y consonantes...
riman :: Palabra -> Palabra -> Bool
riman palabra1 palabra2 = palabra1 /= palabra2 &&
 (rimaAsonante palabra1 palabra2 || rimaConsonante palabra1 palabra2)

rimaAsonante :: Palabra -> Palabra -> Bool
rimaAsonante = cumplen (ultimasVocales 2) (==)
 
rimaConsonante :: Palabra -> Palabra -> Bool
rimaConsonante = cumplen (ultimasLetras 3) (==)
 
ultimasLetras :: Number -> Palabra -> String
ultimasLetras n = reverse . take n . reverse
 
ultimasLetrasV2 n palabra = drop (length palabra - n) palabra
 
ultimasVocales :: Number -> Palabra -> String
ultimasVocales n = ultimasLetras n . filter vocal

vocal :: Char -> Bool
vocal letra = esVocal letra || tieneTilde letra

{-
Las clases de equivalencia son
  - Dos palabras riman por rima asonante
  - Dos palabras riman por rima consonante
  - Dos palabras iguales no riman
  - Dos palabras sin conexion no riman
-}


-- 2) Conjugaciones: Dos versos están conjugados cuando
type Conjugacion = Verso -> Verso -> Bool
 
-- 2a) Por medio de rimas.
rimaCon :: Conjugacion
rimaCon = cumplen ultimaPalabra riman
 
ultimaPalabra :: Verso -> Palabra
ultimaPalabra = last . words
 
-- 2b) Haciendo anadiplosis
anadiplosis :: Conjugacion
anadiplosis verso1 verso2 = ultimaPalabra verso1 == primerPalabra verso2
 
primerPalabra :: Verso -> Palabra
primerPalabra = head . words
 
-- 3) Patrones: son estructuras que se van armando dentro de una estrofa
type Patron = Estrofa -> Bool
 
type Par = (Number, Number)
 
-- simple: rima un verso con otro, cualesquiera
simple :: Par -> Patron
simple (n1, n2) estrofa = versoAt n1 estrofa `rimaCon` versoAt n2 estrofa
 
versoAt :: Number -> Estrofa -> Verso
versoAt n estrofa = estrofa !! (n - 1)
 
-- esdrujulas: todos los versos terminan con una palabra esdrújula, diremos que son esdrújulas las palabras que tienen la anteúltima vocal con tilde.
esdrujulas :: Patron
esdrujulas = all (esEsdrujula . ultimaPalabra)
 
esEsdrujula :: Palabra -> Bool
esEsdrujula = tieneTilde . head . ultimasVocales 3
 
-- anafora: todos los versos empiezan con la misma palabra
anafora :: Patron
anafora = iguales . map primerPalabra
 
anaforaV2 versos = all ((primerPalabra (head versos) ==).primerPalabra) versos
 
iguales :: [Palabra] -> Bool
iguales []                 = False -- (?)
iguales (palabra:palabras) = all (== palabra) palabras
 
-- 3b) Patrones complejos
 
-- cadena: crea un patrón donde cada verso se conjuga con el siguiente. Debe servir para cualquier conjugación de versos
cadena :: Conjugacion -> Patron
cadena _ [] = False -- (?)
cadena _ [_] = True
cadena conjugacion (verso1:verso2:versos) =
 conjugacion verso1 verso2 && cadena conjugacion (verso2 : versos)
 
 
-- sumar patrones: se deben cumplir ambos
y :: Patron -> Patron -> Patron
y patron1 patron2 estrofa = patron1 estrofa && patron2 estrofa
 
-- aabb: riman el primer con el segundo verso, y el tercero con el cuarto.
aabb :: Patron
aabb = simple (1, 2) `y` simple (3, 4)
 
abab :: Patron
abab = simple (1, 3) `y` simple (2, 4)
 
abba :: Patron
abba = simple (1, 4) `y` simple (2, 3)
 
hardore :: Patron
hardore = cadena rimaCon `y` esdrujulas
 
 
-- 4) Puestas en escena.
type Artista = String
 
data PuestaEnEscena = UnaPuestaEnEscena
 { artista         :: Artista
 , freestyle       :: Estrofa
 , potencia        :: Number
 , publicoExaltado :: Bool
 }
 
puestaBase :: Artista -> Estrofa -> PuestaEnEscena
puestaBase mc estrofa =
 UnaPuestaEnEscena
   {artista = mc, freestyle = estrofa, potencia = 1, publicoExaltado = False}
 
aumentarPotencia :: Number -> PuestaEnEscena -> PuestaEnEscena
aumentarPotencia factor puesta =
 puesta {potencia = potencia puesta * (1 + factor)}
 
exaltarPublico :: Bool -> PuestaEnEscena -> PuestaEnEscena
exaltarPublico exaltado puesta = puesta {publicoExaltado = exaltado}
 
exaltarPublicoSiCumple :: Patron -> PuestaEnEscena -> PuestaEnEscena
exaltarPublicoSiCumple patron puesta =
 exaltarPublico (cumplePatron puesta patron) puesta
 
cumplePatron :: PuestaEnEscena -> Patron -> Bool
cumplePatron puesta patron = patron (freestyle puesta)
 
type Estilo = PuestaEnEscena -> PuestaEnEscena
 
gritar :: Estilo
gritar = aumentarPotencia 0.5
 
respuesta :: Bool -> Estilo
respuesta efectiva = exaltarPublico efectiva . aumentarPotencia 0.2
 
tirarSkills :: Patron -> Estilo
tirarSkills patron = exaltarPublicoSiCumple patron . aumentarPotencia 0.1
 
--
tirarFreestyle :: Artista -> Estrofa -> Estilo -> PuestaEnEscena
tirarFreestyle mc estrofa estilo = estilo (puestaBase mc estrofa)
 
-- 5) Jurado.
type Jurado = [Criterio]
 
type Criterio = (PuestaEnEscena -> Bool, Number)
 
puntaje :: PuestaEnEscena -> Jurado -> Number
puntaje puesta = puntajeFinal . valoraciones . cirteriosNumbereresantes puesta
 
puntajeFinal :: [Number] -> Number
puntajeFinal = max 3 . sum
 
valoraciones :: [Criterio] -> [Number]
valoraciones = map snd
 
cirteriosNumbereresantes :: PuestaEnEscena -> Jurado -> [Criterio]
cirteriosNumbereresantes puesta = filter (($ puesta) . fst)
 
-- 6) BONUS: 3, 2, 1... tiempo!
type Performance = [PuestaEnEscena] -- Todas del mismo artista
 
type Batalla = Performance -- Tiene el mismo tipo pero están todos los artistas mezclados, según el orden cronológico
 
seLlevaElCNumbero :: [Jurado] -> Batalla -> Artista
seLlevaElCNumbero jurados =
 artistaPerformance . performanceGanadora jurados . separarPorArtistas
 
artistaPerformance :: Performance -> Artista
artistaPerformance = artista . head
 
performanceGanadora :: [Jurado] -> (Performance, Performance) -> Performance
performanceGanadora jurados (performance1, performance2)
 | puntajeTotal jurados performance1 > puntajeTotal jurados performance2 =
   performance1
 | puntajeTotal jurados performance1 < puntajeTotal jurados performance2 =
   performance2
 | otherwise = error "REPLICA"
 
separarPorArtistas :: Batalla -> (Performance, Performance)
separarPorArtistas batalla =
 splitBy ((artistaPerformance batalla ==) . artista) batalla
 
puntajeTotal :: [Jurado] -> Performance -> Number 
puntajeTotal jurados = sum . map (flip puntajeTotalDePuesta jurados)
 
puntajeTotalDePuesta :: PuestaEnEscena -> [Jurado] -> Number
puntajeTotalDePuesta puesta = sum . map (puntaje puesta)
 
splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy cond lista = (filter cond lista, filter (not . cond) lista)
 

