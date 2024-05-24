
data Personaje = UnPersonaje {
  nombre :: String,
  salud :: Float,
  elementos :: [Elemento],
  anioPresente :: Int
} 

data Elemento = UnElemento {
    tipo :: String,
    ataque :: Personaje-> Personaje,
    defensa :: Personaje-> Personaje
} 

-- 1) 

mandarAlAnio :: Int->Personaje->Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

meditar :: Personaje->Personaje
meditar personaje = personaje {salud = 1.5* salud personaje}

menorquecero a b
               | a < b = 0
               | otherwise = a - b

causarDanio :: Float->Personaje->Personaje
causarDanio danio personaje = personaje { salud = menorquecero (salud personaje) danio}

-- 2) 

listaDeTipos elementos = map tipo elementos

esMalvado :: Personaje->Bool
esMalvado personaje = "Maldad" `elem` listaDeTipos (elementos personaje)

danioQueProduce :: Personaje->Elemento->Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

type Enemigo = Personaje

mataDeUnSoloElemento :: Personaje->Elemento->Bool
mataDeUnSoloElemento personaje elemento = danioQueProduce personaje elemento == salud personaje 

enemigoMataDeUnSoloElemento :: Personaje->Enemigo->Bool
enemigoMataDeUnSoloElemento personaje enemigo = all (mataDeUnSoloElemento personaje) (elementos enemigo) 

enemigosMortales :: Personaje -> [Enemigo] -> [Enemigo]
enemigosMortales personaje enemigos = filter (enemigoMataDeUnSoloElemento personaje) enemigos 


-- 3) 
type Nivel = Float 

nada :: Personaje->Personaje
nada p = p

con = UnElemento "Magia" nada meditar

meditarN :: Float->Personaje->Personaje
meditarN numero personaje = personaje {salud = numero * 1.5* salud personaje}

concentracion :: Nivel -> Elemento
concentracion nivel = con { defensa = meditarN nivel}

esbirro = UnElemento "Maldad" nada (causarDanio 1)

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados 0 = []
esbirrosMalvados cantidad = esbirro : esbirrosMalvados (cantidad - 1) 
                         
katana = UnElemento "Magia" (causarDanio 1000) nada
jack = UnPersonaje "Jack" 300 [concentracion 3,katana] 200

portalafuturo = UnElemento "Magia"  enviarAlFuturo nada

enviarAlFuturo :: Personaje->Personaje
enviarAlFuturo personaje = personaje {anioPresente = anioPresente personaje + 2800}
 
person = UnPersonaje "Aku" 0 [concentracion 4] 0
 
aku :: Int->Float->Personaje
aku anio salud = person {elementos = elementos person ++ esbirrosMalvados (100 * anio) ++ [portalafuturo] }

nuevoAku :: Int->Float->Personaje->Personaje
nuevoAku anio salud aku = aku {anioPresente = anio, salud = salud }   



