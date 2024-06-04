data Raton = UnRaton {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    enfermedades :: [String]
} deriving (Show,Eq)

cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis", "sarampion", "tuberculosis"]
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []
huesudo = UnRaton "Huesudo" 4 20 ["alta obesidad", "sinusitis"]

-- HIERBAS: 

type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena raton = raton { edad = sqrt (edad raton) }

coladepalabra :: Int -> String -> String
coladepalabra cantidad palabra = drop (length palabra - cantidad) palabra

noTerminaEn :: String -> String -> Bool
noTerminaEn fin palabra = coladepalabra (length fin) palabra /= fin

terminaEn :: String -> String -> Bool
terminaEn fin palabra = coladepalabra (length fin) palabra == fin

filtrarSiNoTerminanEn :: String -> [String] -> [String]
filtrarSiNoTerminanEn forma enfermedades = filter (noTerminaEn forma) enfermedades

hierbaVerde :: String -> Hierba
hierbaVerde forma raton = raton {enfermedades = filtrarSiNoTerminanEn forma (enfermedades raton)}

siPesaMas2kilos :: Raton -> Bool
siPesaMas2kilos raton = peso raton > 2

pierdeNporcientoDePeso :: Float -> Raton -> Raton
pierdeNporcientoDePeso n raton = raton {peso = peso raton - peso raton * (n/100)}

alcachofa :: Hierba
alcachofa raton
                | siPesaMas2kilos raton = pierdeNporcientoDePeso 10 raton
                | otherwise = pierdeNporcientoDePeso 5 raton

pierdeEnfermedades :: Raton -> Raton
pierdeEnfermedades raton = raton {enfermedades = []}

quedanEnCeroAnios :: Raton -> Raton
quedanEnCeroAnios raton = raton {edad = 0}

hierbaZort :: Hierba
hierbaZort = pierdeEnfermedades.quedanEnCeroAnios

borraConMenosDeDiezLetras :: [String] -> [String]
borraConMenosDeDiezLetras = filter (\palabra -> length palabra < 10)

eliminaEnfermedades :: Raton -> Raton
eliminaEnfermedades raton = raton {enfermedades = borraConMenosDeDiezLetras (enfermedades raton)}

restaHastaCero :: Float -> Float -> Float
restaHastaCero numero1 numero2
                              | numero1 - numero2 <= 0 = 0
                              | otherwise = numero1 - numero2

pierdePeso :: Raton -> Raton
pierdePeso raton = raton {edad = restaHastaCero (edad raton) 0.1}

hierbaDeLDiablo :: Hierba
hierbaDeLDiablo = pierdePeso.eliminaEnfermedades

-- MEDICAMENTOS 

type Medicamento = [Hierba]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]

reduceFatFast :: Int -> Medicamento
reduceFatFast potencia = hierbaVerde "obesidad" : replicate potencia alcachofa

esInfecciosa :: String -> Bool
esInfecciosa enfermedad = any (\sufijo -> not (noTerminaEn sufijo enfermedad)) sufijosInfecciosas

--pdepCilina :: Medicamento
--pdepCilina = replicate 

-- EXPERIMENTO 

type Condicion = Int -> Bool

naturales :: [Int]
naturales = iterate (+1) 0

esflaco :: Raton -> Bool
esflaco raton = peso raton < 1

aplicarMedicamento :: Medicamento -> Raton -> Raton
aplicarMedicamento medicamento raton = foldl (\raton hierba -> hierba raton) raton medicamento

ratonesConMedicamento :: Medicamento -> [Raton] -> [Raton]
ratonesConMedicamento medicamento = map (aplicarMedicamento medicamento)

tieneMenosDeTresEnfemedades :: Raton -> Bool
tieneMenosDeTresEnfemedades raton = length (enfermedades raton) < 3

ratonesSanos :: [Raton] -> Bool
ratonesSanos = all tieneMenosDeTresEnfemedades

ratonesflacos :: [Raton] -> Bool
ratonesflacos = all esflaco

sonFlacosYSanos :: [Raton] -> Bool
sonFlacosYSanos ratones = ratonesSanos ratones && ratonesflacos ratones

lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento ratones = sonFlacosYSanos (ratonesConMedicamento medicamento ratones)

