
type Nombre = String
type Tiempo = Int

-- Planetas:

data Planeta = UnPlaneta Nombre Posicion (Int -> Int)

posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

-- Astronautas: 

data Astronauta = UnAstronauta String Int Planeta

nombre (UnAstronauta n _ _) = n
edad (UnAstronauta _ e _) = e
planeta (UnAstronauta _ _ p) = p


-- 1)

sumaDeCoordenadas :: Planeta -> Planeta -> Float
sumaDeCoordenadas p1 p2 = (coordX (posicion p1) + coordX (posicion p2))^2 + (coordY (posicion p1) + coordY (posicion p2))^2 + (coordZ (posicion p1) + coordZ (posicion p2))^2 

distanciaEntreDosPlanetas :: Planeta -> Planeta -> Float
distanciaEntreDosPlanetas planeta1 plantea2 = sqrt (sumaDeCoordenadas planeta1 plantea2)

type Velocidad = Float

cuantoSeTarda :: Planeta -> Planeta -> Velocidad -> Float
cuantoSeTarda planeta1 plantea2 velocidad = distanciaEntreDosPlanetas planeta1 plantea2 / velocidad

--2)

type Anios = Int

pasarTiempo :: Astronauta -> Anios -> Int
pasarTiempo astronauta anios = edad astronauta * anios 

--3)

data Nave = UnaNave Planeta Planeta Int [String] Tiempo



