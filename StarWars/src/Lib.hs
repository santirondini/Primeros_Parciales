data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: Poder
}

type Poder = Nave -> Nave

turbo :: Poder
turbo nave = nave {ataque = ataque nave + 25}

reparacion :: Poder
reparacion nave = nave {durabilidad = durabilidad nave + 50, ataque = ataque nave - 30}

reducirDurabilidad :: Poder
reducirDurabilidad nave = nave {durabilidad = durabilidad nave - 45}

superTurbo :: Poder
superTurbo = turbo.turbo.turbo.reducirDurabilidad

incrementarEscudos :: Poder
incrementarEscudos nave = nave {escudo = escudo nave + 100}

poderDeFalcon :: Poder
poderDeFalcon = reparacion.incrementarEscudos

tie = UnaNave "TIE Fighter" 200 100 50 turbo
xwing = UnaNave "X Wing" 300 150 100 reparacion
darth = UnaNave "Nave de Darth Vader" 500 300 200 superTurbo
falcon = UnaNave "Milennium Falcon" 1000 500 50 poderDeFalcon

-- Mi nave:

elPoderDelSony :: Poder
elPoderDelSony = superTurbo.superTurbo.poderDeFalcon

elsony = UnaNave "El Sony" 1000 5000 8000 elPoderDelSony

--

durabilidades :: [Nave] -> [Int]
durabilidades = map durabilidad

durabilidadTotal :: [Nave] -> Int
durabilidadTotal naves = sum (durabilidades naves)

--

type Atacante = Nave
type Atacada = Nave

elEscudoEsSuperiorAlAtaque :: Atacante -> Atacada -> Bool
elEscudoEsSuperiorAlAtaque naveatacante naveatacada = escudo naveatacada > ataque naveatacante

danioRecibido :: Atacante -> Atacada -> Int
danioRecibido naveatacante naveatacada 
                                     | escudo naveatacada - ataque naveatacante <= 0 = 0
                                     | otherwise =  escudo naveatacada - ataque naveatacante

naveAtacada :: Atacante -> Atacada -> Nave
naveAtacada naveatacante naveatacada = naveatacada {durabilidad = durabilidad naveatacada - danioRecibido naveatacante naveatacada}

postAtaque :: Atacante -> Atacada -> Nave
postAtaque naveatacante naveatacada 
                                   | elEscudoEsSuperiorAlAtaque naveatacante naveatacada = naveatacada 
                                   | otherwise = naveAtacada naveatacante naveatacada
                                
batalla :: Atacante -> Atacada -> Nave
batalla naveatacante naveatacada = postAtaque naveatacante (poder naveatacada naveatacada) 

--

estaFueraDeCombate :: Estrategia
estaFueraDeCombate nave = durabilidad nave == 0

--
type FlotaEnemiga = [Nave]
type Estrategia = Nave -> Bool

debiles :: Estrategia
debiles nave = escudo nave < 200

peligrosidad :: Int -> Estrategia
peligrosidad x nave = ataque nave > x 

laEstrategiaDelSony :: Char -> Estrategia
laEstrategiaDelSony caracter nave  = head (nombre nave) == caracter 

siCumpleLaEstrategiaAtacar :: Estrategia -> Atacante -> Atacada -> Nave
siCumpleLaEstrategiaAtacar estrategia atacante atacada 
                                                        | estrategia atacada = batalla atacante atacada 
                                                        | otherwise = atacada 

comoQuedaLaFlota :: Estrategia -> Atacante -> FlotaEnemiga -> FlotaEnemiga
comoQuedaLaFlota estrategia atacante flota = map (siCumpleLaEstrategiaAtacar estrategia atacante) flota












