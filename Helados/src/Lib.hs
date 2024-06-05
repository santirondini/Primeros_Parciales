import Data.Char (digitToInt)
type Sabor = String 
type Temperatura = Int 
type Proporcion = Float 

type Helado = (Sabor, Temperatura, Proporcion)

nombre (n,_,_) = n
temperatura (_,t,_) = t
proporcion (_,_,p) = p

mandarina :: Helado
mandarina = ("mandarina",0,0.4)

frutilla :: Helado
frutilla = ("frutilla",5,0.4)

durazno :: Helado
durazno = ("durazno",15,0.5)

noLlegaAcongelarse :: Helado -> Bool
noLlegaAcongelarse helado = temperatura helado > 0

esVocal :: Char -> Bool
esVocal caracter = caracter `elem` "AEIOUaeiou"

soloLasVocales :: String -> String
soloLasVocales = filter esVocal

cantidadDeVocales :: String -> Int
cantidadDeVocales palabra = length (soloLasVocales palabra)

condicionParaLasDemas :: Helado -> Bool
condicionParaLasDemas (nombre,_,proporcion)
                                           | length nombre < 8 = fromIntegral (length nombre) / 10.0 /= proporcion
                                           | otherwise = proporcion /= 0.4

proporcionIncorrecta :: Helado -> Bool
proporcionIncorrecta ("frutilla",_,proporcion) = proporcion /= 0.4
proporcionIncorrecta ("durazno",_,proporcion) = proporcion < 0.2 && proporcion > 0.6
proporcionIncorrecta helado = condicionParaLasDemas helado

salioMal :: Helado -> Bool
salioMal helado = noLlegaAcongelarse helado || proporcionIncorrecta helado

--

type Maquina = Helado -> Helado
type Cajon = String 
type Agua = String 
type Fruta  = String 
type Kilos = Int
type Volumen = Float

enfria :: Int -> Helado -> Helado
enfria numero (nombre , temperatura, proporcion) = (nombre , temperatura - numero, proporcion)

heladera :: Int -> Maquina
heladera = enfria

frutaDelCajon :: Cajon -> Fruta 
frutaDelCajon cajon = last (words cajon)

kilosDelCajon :: Cajon -> Kilos 
kilosDelCajon cajon = digitToInt (head (head (words cajon)))

proporcion' :: Cajon -> Agua -> Proporcion
proporcion' cajon litros = fromIntegral (kilosDelCajon cajon) / sacarVolumen litros

sacarGrados :: Agua -> Temperatura
sacarGrados oracion =  read (words oracion !! 3)

sacarVolumen :: Agua -> Volumen
sacarVolumen oracion = read (head (words oracion)) 

-- Formato = batidora "5kg de durzano" "3 litros a 45 grados"

batidora :: Cajon -> Agua -> Helado
batidora cajon agua = (frutaDelCajon cajon, sacarGrados agua , proporcion' cajon agua )


