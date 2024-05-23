

data Pais = UnPais{
    ingresopercapita :: Float,
    poblacionActivaPublica :: Float,
    poblacionActivaPrivado :: Float,
    recursosNaturales :: [String],
    deuda :: Float
} deriving (Show,Eq)

argentina = UnPais 10 25 25 ["Petroleo"] 0

type Estrategia = Pais->Pais
type Receta = [Estrategia]

prestarNmillones :: Float -> Estrategia
prestarNmillones n pais = pais {deuda = deuda pais + n * 1500000 }

redicirXpuestosDeTrabajo:: Float -> Estrategia
redicirXpuestosDeTrabajo x pais
               | poblacionActivaPublica pais > 100 = pais {poblacionActivaPublica = poblacionActivaPublica pais - x, ingresopercapita = ingresopercapita pais - (ingresopercapita pais)*0.2}
               | otherwise = pais {poblacionActivaPublica = poblacionActivaPublica pais - x, ingresopercapita = ingresopercapita pais - (ingresopercapita pais)*0.15}

sacar :: [String]->String->[String]
sacar [] _ = []
sacar (x:xs) asacar
                    | x == asacar = sacar xs asacar
                    | otherwise = x : sacar xs asacar

darEmpresa :: String -> Estrategia
darEmpresa recurso pais = pais { recursosNaturales = sacar (recursosNaturales pais) recurso , deuda = deuda pais - 2000000}

pbi pais = ingresopercapita pais * (poblacionActivaPrivado pais + poblacionActivaPublica pais)

blindaje :: Estrategia
blindaje pais = pais {deuda = deuda pais + pbi pais, poblacionActivaPublica = poblacionActivaPublica pais - 500}

--3) 
receta = [prestarNmillones 200, darEmpresa "Minería"]

-- 4)

zafa :: Pais->Bool
zafa pais = "Petroleo" `elem` recursosNaturales pais

cualeszafan :: [Pais]->[Pais]
cualeszafan = filter zafa

listaDeDeudas = map deuda

deudaConElFMI :: [Pais]->Float
deudaConElFMI paises = foldl1 (+) (listaDeDeudas paises)

--5) 

-- Sin recursivadad:

aplicarRecetas :: Pais -> [Receta] -> [Pais]
aplicarRecetas pais recetas = map (\receta -> aplicarEstrategias pais receta) recetas

listaDePBIconRecetasAplicadas :: [Pais]->[Float]
listaDePBIconRecetasAplicadas paises = map pbi paises

aplicarEstrategias :: Pais->[Estrategia]->Pais
aplicarEstrategias = foldl (\pais estrategia -> estrategia pais)

recetasOrdenadas :: Pais->[Receta]->Bool
recetasOrdenadas _ [_] = True 
recetasOrdenadas _ [] = True 
recetasOrdenadas pais (x:y:xs) = pbi (aplicarEstrategias pais x) < pbi (aplicarEstrategias pais y) && recetasOrdenadas pais xs







