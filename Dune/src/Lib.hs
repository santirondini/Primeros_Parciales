

data Fremen = UnFremen {
    nombre :: String,
    tolerancia :: Float,
    titulos :: [String],
    reconocimientos :: Int
} deriving (Eq,Show)

--1)

nuevoReconocimiento :: Fremen -> Fremen
nuevoReconocimiento fremen = fremen { reconocimientos = reconocimientos fremen + 1 }

paraserCandidato :: Fremen -> Bool
paraserCandidato fremen = elem "Domador" (titulos fremen) && tolerancia fremen > 100

candidatos :: [Fremen] -> Bool
candidatos = any paraserCandidato

tieneMasReconocimientos :: Fremen -> Fremen -> Fremen
tieneMasReconocimientos f1 f2
                             | reconocimientos f1 > reconocimientos f2 = f1
                             | otherwise = f2

elegido :: [Fremen] -> Fremen
elegido fremenes = foldl1 tieneMasReconocimientos (filter paraserCandidato fremenes)

--2)
type Cria = Gusano
data Gusano = UnGusano {
    longitud :: Float,
    hidratacion :: Int,
    descripcion :: String 
} deriving(Show,Eq)
cria = UnGusano 0 0 " "
bicho = UnGusano 10 5 "rojo con lunares"
gusano = UnGusano 8 1 "dientes puntiagudos"
gusano1 = UnGusano 7 1 "dientes amarillos"
gusano2 = UnGusano 5 1 "pija larga"
gusano3 = UnGusano 90 5 "nada"

lista1 = [bicho, gusano, gusano1]
lista2 =[gusano2,gusano3]

reproducirGusanos :: Gusano -> Gusano -> Cria
reproducirGusanos gusano1 gusano2 = cria {longitud = max (longitud gusano1) (longitud gusano2) * 0.1, descripcion = descripcion gusano1 ++ " - " ++ descripcion gusano2}

aparearGusanoConLista :: Gusano -> [Gusano] -> [Cria]
aparearGusanoConLista gusano gusanos = map (\gusanox -> reproducirGusanos gusanox gusano) gusanos 

aparear :: [Gusano] -> [Gusano] -> [Cria]
aparear [] [] = []
aparear [] ys = ys
aparear xs [] = xs 
aparear (x:xs) (y:ys) = reproducirGusanos x y : aparear xs ys

aparearListasDeGusanos :: [Gusano] -> [Gusano] -> [Cria]
aparearListasDeGusanos lista1 lista2 = aparear lista1 lista2

-- 3) 

type Mision = Fremen -> Gusano -> Fremen

puedeDomarlo :: Fremen -> Gusano -> Bool
puedeDomarlo fremen gusano = tolerancia fremen >= longitud gusano 

domarGusanoDeArena :: Mision
domarGusanoDeArena fremen gusano 
                               | puedeDomarlo fremen gusano = fremen {titulos = "Domador" :  titulos fremen,  tolerancia = tolerancia fremen + 100}
                               | otherwise = fremen {tolerancia = tolerancia fremen * 0.9 }

puedeDestruirlo :: Fremen -> Gusano -> Bool
puedeDestruirlo fremen gusano = elem "Domador" (titulos fremen) && tolerancia fremen < longitud gusano * 0.5 

destruirGusanoDeArena :: Mision
destruirGusanoDeArena fremen gusano 
                                   | puedeDestruirlo fremen gusano = fremen {reconocimientos = reconocimientos fremen + 1, tolerancia = tolerancia fremen + 100}
                                   | otherwise = fremen {tolerancia = tolerancia fremen*0.8}

-- inventada: 

condicion :: Fremen -> Gusano -> Bool
condicion fremen gusano = elem "Guitarrista" (titulos fremen) && (reconocimientos fremen > 15 || descripcion gusano == "sabe cantar")   

tocarLaGuitarra :: Mision
tocarLaGuitarra fremen gusano 
                            | condicion fremen gusano = fremen {reconocimientos = reconocimientos fremen + 5, titulos = titulos fremen ++ ["Guitar Hero"], nombre = "El guitarrista" ++ nombre fremen}
                            | otherwise = fremen {titulos = titulos fremen ++ ["El fracasado"], tolerancia = 0}

type Tribu = [Fremen]

simulacion :: Tribu -> Gusano -> Mision -> [Fremen]
simulacion tribu gusano mision  = map (\fremen -> mision fremen gusano) tribu 

sigueSiendoElElegido :: Gusano -> Tribu -> Mision -> Bool
sigueSiendoElElegido gusano tribu mision = elegido tribu == elegido (simulacion tribu gusano mision)


