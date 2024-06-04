data Postre = UnPostre {
    sabores :: [String],
    peso :: Float,
    temperatura :: Float
} deriving (Show,Eq)

type Hechizo = Postre -> Postre

-- Funciones:

pierdeUnGrado :: Hechizo
pierdeUnGrado postre = postre {temperatura = temperatura postre - 1}

pierdeNporcientoDeSuPeso :: Float -> Hechizo
pierdeNporcientoDeSuPeso porciento postre = postre {peso = peso postre - peso postre*(porciento/100)}

congelarPostre :: Hechizo
congelarPostre postre = postre {temperatura = 0}

agregarSabor :: String -> Hechizo
agregarSabor sabor postre = postre {sabores = sabor : sabores postre}

pierdeSabores :: Hechizo
pierdeSabores postre = postre {sabores = []}

-- Hechizos

incendio :: Hechizo
incendio = pierdeUnGrado.pierdeNporcientoDeSuPeso 5

immobulus :: Hechizo
immobulus = congelarPostre

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarSabor "concentrado". pierdeNporcientoDeSuPeso 10

diffindo :: Float -> Hechizo
diffindo = pierdeNporcientoDeSuPeso

riddikulus :: String -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

avadaKedavra :: Hechizo
avadaKedavra = pierdeSabores.immobulus

--

noEstaCongelado :: Postre -> Bool
noEstaCongelado postre = temperatura postre > 0

pesaAlgoMasQue :: Float -> Postre -> Bool
pesaAlgoMasQue cantidad postre = peso postre > cantidad

contieneElsabor :: String -> Postre -> Bool
contieneElsabor sabor postre = sabor `elem` sabores postre

postreListo :: Float -> String -> Postre -> Bool
postreListo cantidad sabor postre = noEstaCongelado postre && pesaAlgoMasQue cantidad postre && contieneElsabor sabor postre

conElHechizoAplicado :: Hechizo -> [Postre] -> [Postre]
conElHechizoAplicado = map

estanListos :: Float -> String -> Hechizo -> [Postre] -> Bool
estanListos cantidad sabor hechizo postres= all (postreListo cantidad sabor) (conElHechizoAplicado hechizo postres)

postresListos :: Float -> String -> [Postre] -> [Postre]
postresListos cantidad sabor  = filter (postreListo cantidad sabor)

sacarPromedio :: [Float] -> Float 
sacarPromedio lista = sum lista / fromIntegral (length lista) 

listaDePesos :: [Postre] -> [Float]
listaDePesos lista = map peso lista 

promedioDePostresListos :: Float -> String -> [Postre] -> Float 
promedioDePostresListos cantidad sabor = sacarPromedio . listaDePesos . postresListos cantidad sabor