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

