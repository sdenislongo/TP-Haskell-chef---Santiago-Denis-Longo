module Library where
import PdePreludat

data Participante = Participante {
    nombre :: String,
    trucosCocina :: [Plato -> Plato],
    platoEspecialidad :: Plato
} deriving (Show)


data Plato = Plato {
    dificultad :: Number,
    componentes :: [Componente]
} deriving (Show, Eq)


type Componente = (Ingrediente, Peso)

type Ingrediente = String
type Peso = Number


platoPrueba1 :: Plato
platoPrueba1 = Plato {
    dificultad = 9,
    componentes = [("sal", 2), ("harina", 100), ("huevos", 50)]
}

-- Para probar endulzar cuando NO hay azúcar (debe agregar)
platoPrueba2 :: Plato
platoPrueba2 = Plato {
    dificultad = 5,
    componentes = [("azucar", 10), ("sal", 5)]
}

-- Para probar endulzar cuando SÍ hay azúcar (debe modificar)
platoPepe :: Plato
platoPepe = Plato {
    dificultad = 8,
    componentes = [("carne", 200), ("sal", 3), ("azucar", 20), ("lacteos", 50), ("harina", 80), ("aceite", 30), ("longaniza", 9)]
}
-- Este tiene >5 componentes y dificultad >7, útil para probar simplificar


hayIngrediente :: String -> [Componente] -> Bool
hayIngrediente unIngrediente componentes = any (==unIngrediente).map fst $ componentes
--

agregarOModificar :: Number -> String -> Plato -> Plato
agregarOModificar cantidad unIngrediente unPlato
 | hayIngrediente unIngrediente (componentes unPlato) = modificarComponente unPlato unIngrediente (+cantidad)
 | otherwise = agregarComponente unPlato unIngrediente cantidad

modificarComponente :: Plato -> String -> (Number -> Number) -> Plato
modificarComponente unPlato unIngrediente unaOperacion = unPlato{ componentes = map (modificarSiEs unIngrediente unaOperacion) (componentes unPlato) }

modificarSiEs :: String -> (Number -> Number) -> Componente -> Componente
modificarSiEs unIngrediente unaOperacion (nombre, peso)
 | nombre == unIngrediente = (nombre, unaOperacion peso)
 | otherwise = (nombre, peso)

agregarComponente :: Plato -> String -> Number -> Plato
agregarComponente unPlato unIngrediente cantidad = unPlato{componentes = (unIngrediente, cantidad) : componentes unPlato}

endulzar :: Number -> Plato -> Plato
endulzar cantidad unPlato = agregarOModificar cantidad "azucar" unPlato

salar :: Number -> Plato -> Plato
salar cantidad unPlato = agregarOModificar cantidad "sal" unPlato

darSabor :: Number -> Number -> Plato -> Plato
darSabor cantAzucar cantSal unPlato = endulzar cantAzucar . salar cantSal $ unPlato

modificarPesos ::(Number -> Number) -> Componente -> Componente --Para duplicarPorcion
modificarPesos unaOperacion (nombre, peso) = (nombre, unaOperacion peso)

duplicarPorcion :: Plato -> Plato
duplicarPorcion unPlato = unPlato{componentes = map (modificarPesos (*2)) $ componentes unPlato}


-- duplicarPorcion se puede hacer tmb (mas facil, con lambda):

duplicarPorcion2 :: Plato -> Plato
duplicarPorcion2 unPlato = unPlato { componentes = map (\(nombre, peso) -> (nombre, peso * 2)) $ componentes unPlato }

--esUnBardo == esComplejo, tienen la misma función
esUnBardo :: Plato -> Bool
esUnBardo unPlato = ((>5).length . componentes $ unPlato) && dificultad unPlato > 7

simplificar :: Plato -> Plato
simplificar unPlato
 | esUnBardo unPlato = unPlato{ dificultad = 5, componentes = filter ((>=10).snd) (componentes unPlato)}
 | otherwise = unPlato


--Simplificar se puede hacer tambien con lambda (IMPORTANTE APRENDER ESA MIERDA)

simplificar2 :: Plato -> Plato
simplificar2 unPlato
 | esUnBardo unPlato = unPlato{ dificultad = 5, componentes = filter (\(_, peso) -> peso >= 10) (componentes unPlato)}
 | otherwise = unPlato



esSin:: Plato -> [String] -> Bool
esSin unPlato ciertosAlimentos = not $ any (`elem` ciertosAlimentos).map fst.componentes $ unPlato

--Explicacion para que se entienda
--Any va tomando de a un elemento de la lista de componentes y lo mete en el elem. Por ejemplo:
-- "carne" elem ["carne, "huevo", "leche"], en este caso devolvería true el elem, por lo que any tmb
-- Osea, primero hace fst -> fst ("carne", 200) = "carne" y despues "carne" elem ciertos alimentos



esVegano :: Plato -> Bool
esVegano unPlato = esSin unPlato ["carne", "huevos", "lacteos",  "queso", "leche"]


--Este es mejor y mas entendible funcionan ambos
esVegano2 :: Plato -> Bool
esVegano2 unPlato = not $ any (\(nombre, _) -> nombre `elem` ["carne", "huevos", "lacteos",  "queso", "leche"]) (componentes unPlato)


esSinTacc :: Plato -> Bool
esSinTacc unPlato = esSin unPlato ["harina"]


--IMPORTANTE SABER USAR LAMBDA
noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = any (\(ingrediente, peso) -> ingrediente == "sal" && peso > 2).componentes $ unPlato

pepeRonccino :: Participante
pepeRonccino = Participante{
    nombre = "Pepe Ronccino",
    trucosCocina = [simplificar, duplicarPorcion, darSabor 5 2],
    platoEspecialidad = platoPepe
}


--Con recursividad
cocinar :: Participante -> Plato
cocinar unParticipante = aplicarTrucos (trucosCocina unParticipante) (platoEspecialidad unParticipante)

aplicarTrucos :: [Plato -> Plato] -> Plato -> Plato
aplicarTrucos [] unPlato = unPlato
aplicarTrucos (x : xs) unPlato = aplicarTrucos xs (x unPlato)

--usando foldl (uso foldl porque aplica primero el primer truco de la lista, usando foldr aplicaría primero el ultimo y terminaria por el primero)
--Aca se ve como uso foldl operacion semilla lista
cocinar2 :: Participante -> Plato
cocinar2 unParticipante = foldl (\plato truco -> truco plato) (platoEspecialidad unParticipante) (trucosCocina unParticipante)

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = (dificultad unPlato > dificultad otroPlato) && (sumatoriaPesos unPlato < sumatoriaPesos otroPlato)

sumatoriaPesos :: Plato -> Number
sumatoriaPesos unPlato = sum.map snd.componentes $ unPlato


participanteEstrella :: [Participante] -> Participante
participanteEstrella [unParticipante] = unParticipante
participanteEstrella (primerParticipante:restoDeParticipantes) = elMejorEntre2 primerParticipante (participanteEstrella restoDeParticipantes)


elMejorEntre2 :: Participante -> Participante -> Participante
elMejorEntre2 unParticipante otroParticipante
 | esMejorQue (cocinar2 unParticipante) (cocinar2 otroParticipante) = unParticipante
 | otherwise = otroParticipante




--Para probar particianteEstrella
platoMaria :: Plato
platoMaria = Plato {
    dificultad = 9,
    componentes = [("sal", 1), ("azucar", 2), ("aceite", 5)]
}

mariaCocina :: Participante
mariaCocina = Participante {
    nombre = "Maria Cocina",
    trucosCocina = [duplicarPorcion],
    platoEspecialidad = platoMaria
}

platoJuan :: Plato
platoJuan = Plato {
    dificultad = 6,
    componentes = [("harina", 100), ("agua", 50)]
}

juanCocina :: Participante
juanCocina = Participante {
    nombre = "Juan Cocina",
    trucosCocina = [salar 3, endulzar 10],
    platoEspecialidad = platoJuan
}

participantes :: [Participante]
participantes = [pepeRonccino, mariaCocina, juanCocina, mario, jorge]


mario :: Participante 
mario = Participante{
    nombre = "mario",
    trucosCocina = [endulzar 8, simplificar],
    platoEspecialidad = platoMario
}

platoMario :: Plato
platoMario = Plato{
    dificultad = 10,
    componentes = [("sal", 1), ("aceite", 2)]
}

jorge :: Participante 
jorge = Participante{
    nombre = "jorgito",
    trucosCocina = [darSabor 30 30, duplicarPorcion],
    platoEspecialidad = platoPrueba2
}

