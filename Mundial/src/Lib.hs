
data Jugador = CJugador {
    nombre :: String,
    edad :: Int,
    promedioDeGol :: Float,
    habilidad :: Int,
    cansacion :: Float
} deriving (Show,Eq)

type Equipo = (String, Grupo, [Jugador])

nombreE (nombre, grupo , jugadores) = nombre  
grupoE (nombre, grupo , jugadores)  = grupo 
jugadoresE (nombre, grupo , jugadores) = jugadores


-- JUGADORES:
martin = CJugador "Martin" 26 0.0 50 35.0
juan = CJugador "Juancho" 30 0.2 50 40.0
maxi = CJugador "Maxi Lopez" 27 0.4 68 30.0
jonathan = CJugador "Chueco" 20 1.5 80 99.0
lean = CJugador "Hacha" 23 0.01 50 35.0
brian = CJugador "Panadero" 21 5 80 15.0
garcia = CJugador "Sargento" 30 1 80 13.0
messi = CJugador "Pulga" 26 10 99 43.0
aguero = CJugador "Aguero" 24 5 90 5.0

-- EQUIPOS:

equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero])

quickSort _ [] = []
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs

-- 1) 

habilidadSuficiente:: Jugador -> Bool
habilidadSuficiente = (>75).habilidad

promedioMayorAcero :: Jugador -> Bool
promedioMayorAcero = (>0).promedioDeGol

esFigura :: Jugador -> Bool
esFigura jugador = habilidadSuficiente jugador && promedioMayorAcero jugador

--2)

jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

esFarandulero :: String -> Bool
esFarandulero jugador = jugador `elem` jugadoresFaranduleros

algunoEsFarandulero :: [String] -> Bool
algunoEsFarandulero = any esFarandulero

nombresDe = map nombre 

tieneFarandulero :: Equipo -> Bool 
tieneFarandulero (nombre, grupo, jugadores) = algunoEsFarandulero (nombresDe jugadores) 

-- 3) 

type Grupo = Char

esJoven :: Jugador -> Bool
esJoven = (<27).edad 

esDificil :: Jugador -> Bool
esDificil jugador = esJoven jugador && esFigura jugador && not (esFarandulero (nombre jugador))  

losDificiles :: Equipo -> [Jugador]
losDificiles (nombre, grupo , jugadores) = filter esDificil jugadores 

sonDelGrupo :: Char -> Equipo -> Bool
sonDelGrupo letra (nombre, grupo , jugadores)  = grupo == letra   

filtrarXgrupo :: [Equipo] -> Grupo -> [Equipo]
filtrarXgrupo equipos grupo = filter (sonDelGrupo grupo) equipos

figuritasDificiles :: [Equipo] -> Grupo -> [Jugador]
figuritasDificiles lista grupo = map losDificiles (filtrarXgrupo (map jugadoresE lista) grupo) 