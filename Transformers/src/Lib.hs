
type Fuerza = Int
type Velocidad = Int
type Resistencia = Int


data Autobot =
  Robot String (Fuerza,Velocidad,Resistencia) ((Fuerza,Velocidad,Resistencia) -> (Velocidad,Resistencia)) | 
  Vehiculo String (Velocidad,Resistencia)

optimus = Robot "Optimus Prime" (20,20,10) optimusTransformacion
optimusTransformacion (_,v,r) = (v * 5, r * 2)

jazz = Robot "Jazz" (8,35,3) jazzTransformacion
jazzTransformacion (_,v,r) = (v * 6, r * 3)

wheeljack = Robot "Wheeljack" (11,30,4) wheeljackTransformacion
wheeljackTransformacion (_,v,r) = (v * 4, r * 3)

bumblebee = Robot "Bumblebee" (10,33,5) bumblebeeTransformacion
bumblebeeTransformacion (_,v,r) = (v * 4, r * 2)

autobots = [ optimus, jazz, wheeljack, bumblebee ]

--1) 

maximoSegun :: (Int -> Int -> Int) -> Int -> Int -> Int
maximoSegun funcion a b 
                        | funcion a b > funcion b a = a
                        | otherwise = b