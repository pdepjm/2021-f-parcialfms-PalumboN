module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Rimas" $ do

    it "Rima asonante" $ do
      ("parcial", "estirar") `shouldSatisfy2` rimaAsonante

    it "Rima consonante" $ do
      ("función", "canción") `shouldSatisfy2` rimaConsonante

    it "Palabras iguales no riman" $ do
      ("haskell", "haskell") `shouldNotSatisfy2` riman

    it "Palabras sin conexion no riman" $ do
      ("funcional", "efecto") `shouldNotSatisfy2` riman

  describe "Conjugaciones" $ do

    it "Por medio de rimas" $ do
      ("no hace falta un programa que genere una canción", 
       "para saber que esto se resuelve con una función") `shouldSatisfy2` rimaCon

    it "Haciendo anadiplosis" $ do
      ("este examen no se aprueba sin aplicación parcial", 
       "parcial lindo y divertido si rendiste todas las katas") `shouldSatisfy2` anadiplosis

  describe "Patrones" $ do

    it "Simple piola" $ do
      ["esta rima es fácil como patear un penal",
       "solamente tiene como objetivo servir de ejemplo",
       "los versos del medio son medio fríos",
       "porque el remate se retoma al final"] `shouldSatisfy` simple (1,4)

    it "Simple no piola" $ do
      ["esta rima es fácil como patear un penal",
       "solamente tiene como objetivo servir de ejemplo",
       "los versos del medio son medio fríos",
       "porque el remate se retoma al final"] `shouldNotSatisfy` simple (1,3)

    it "Esdrujulas" $ do
      ["a ponerse los guantes y subir al cuadrilátero",
       "que después de este parcial acerca el paradigma lógico",
       "no entiendo por qué está fallando mi código",
       "si todas estas frases terminan en esdrújulas"] `shouldSatisfy` esdrujulas

    it "Anafora" $ do
      ["paradigmas hay varios, recién vamos por funcional",
       "paradigmas de programación es lo que analizamos acá",
       "paradigmas que te invitan a otras formas de razonar",
       "paradigmas es la materia que más me gusta cursar"] `shouldSatisfy` anafora

    it "Cadena" $ do
      ["este es un ejemplo de un parcial compuesto",
       "compuesto de funciones que se operan entre ellas",
       "ellas también pueden pasarse por parámetro",
       "parámetro que recibe otra función de alto orden"] `shouldSatisfy` cadena anadiplosis

    it "CombinaDos" $ do
      ["estrofa que sirve como caso ejémplico",
       "estrofa dedicada a la gente fanática",
       "estrofa comenzada toda con anáfora",
       "estrofa que termina siempre con esdrújulas"] `shouldSatisfy` esdrujulas `y` anafora
  

  


shouldSatisfy2 (v1, v2) f = f v1 v2 `shouldBe` True
shouldNotSatisfy2 (v1, v2) f = f v1 v2 `shouldBe` False