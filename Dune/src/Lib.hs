

data Fremen = UnFremen {
    nombre :: String,
    tolerancia :: Float,
    titulos :: [String],
    reconocimientos :: Int
} deriving (Eq,Show)

--1)

nuevoReconocimiento :: Fremen -> Fremen
nuevoReconocimiento fremen = fremen { reconocimientos = reconocimientos fremen + 1 }

tieneTolerianciaMayorA100 :: Fremen -> Bool
tieneTolerianciaMayorA100 fremen = tolerancia fremen > 100

es :: String -> Fremen -> Bool
es titulo fremen =  titulo `elem` titulos fremen

paraserCandidato :: Fremen -> Bool
paraserCandidato fremen = es "Domador" fremen && tieneTolerianciaMayorA100 fremen

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

seConvierteEnDomador :: Fremen -> Fremen
seConvierteEnDomador fremen = fremen {titulos = "Domador" :  titulos fremen}

sumaTolerancia :: Float -> Fremen -> Fremen 
sumaTolerancia numero fremen = fremen {tolerancia = tolerancia fremen + numero}

toleranciaBajaPorcentualmente :: Float -> Fremen -> Fremen 
toleranciaBajaPorcentualmente porcentaje fremen = fremen {tolerancia = tolerancia fremen - tolerancia fremen * (porcentaje/100) }

domarGusanoDeArena :: Mision
domarGusanoDeArena fremen gusano
                               | puedeDomarlo fremen gusano = seConvierteEnDomador (sumaTolerancia 10 fremen) 
                               | otherwise = toleranciaBajaPorcentualmente 10 fremen 

toleranciaMenor :: Fremen -> Gusano -> Bool
toleranciaMenor fremen gusano = tolerancia fremen < longitud gusano * 0.5

puedeDestruirlo :: Fremen -> Gusano -> Bool
puedeDestruirlo fremen gusano = es "Domador" fremen && toleranciaMenor fremen gusano 

destruirGusanoDeArena :: Mision
destruirGusanoDeArena fremen gusano
                                   | puedeDestruirlo fremen gusano = nuevoReconocimiento (sumaTolerancia 100 fremen)   
                                   | otherwise = toleranciaBajaPorcentualmente 20 fremen 

-- inventada: 

reconocimientosMenoresQue15 :: Fremen -> Bool
reconocimientosMenoresQue15 fremen = reconocimientos fremen > 15

elGusanoSabeCantar :: Gusano -> Bool 
elGusanoSabeCantar gusano = descripcion gusano == "sabe cantar"

condicion :: Fremen -> Gusano -> Bool
condicion fremen gusano = es "Guitarrista" fremen && (reconocimientosMenoresQue15 fremen || elGusanoSabeCantar gusano)

tocarLaGuitarra :: Mision
tocarLaGuitarra fremen gusano
                            | condicion fremen gusano = fremen {reconocimientos = reconocimientos fremen + 5, titulos = titulos fremen ++ ["Guitar Hero"], nombre = "El guitarrista" ++ nombre fremen}
                            | otherwise = fremen {titulos = titulos fremen ++ ["El fracasado"], tolerancia = 0}

type Tribu = [Fremen]

simulacion :: Tribu -> Gusano -> Mision -> [Fremen]
simulacion tribu gusano mision  = map (\fremen -> mision fremen gusano) tribu

sigueSiendoElElegido :: Gusano -> Tribu -> Mision -> Bool
sigueSiendoElElegido gusano tribu mision = elegido tribu == elegido (simulacion tribu gusano mision)


