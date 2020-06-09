module Lib where
import Text.Show.Functions

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
--La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
--Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.
--Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.

type Palo= Habilidad->Tiro


putter :: Palo

putter habilidad = UnTiro {
    velocidad=10,
    precision = 2*precisionJugador habilidad,
    altura=0

    }

madera :: Palo
madera habilidad = UnTiro {
    velocidad=100,
    precision = precisionJugador habilidad `div` 2,
    altura=5
    }

hierro :: Int -> Palo
hierro n habilidad = UnTiro {
    velocidad=precisionJugador habilidad `div` n,
    precision = n * fuerzaJugador habilidad,
    altura= n-3 `max` 0
}
palos :: [Palo]
palos = [madera,putter] ++ map hierro [0..10]

--Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
--Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.


golpe :: Jugador -> Palo ->Tiro

golpe jugador palo = palo (habilidad jugador)

type Obstaculo = Tiro -> Tiro
--Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
--independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.
--Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo de la laguna.
--Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
--Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.
tiroDetenido = UnTiro 0 0 0

obstaculoSuperableSi
  :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo
obstaculoSuperableSi condicion efecto tiroOriginal
  | condicion tiroOriginal = efecto tiroOriginal
  | otherwise = tiroDetenido


tunelConRampita :: Obstaculo
tunelConRampita = obstaculoSuperableSi superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}

vaAlRasDelSuelo = (==0).altura

laguna :: Int -> Obstaculo
laguna largo = obstaculoSuperableSi superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5.altura) tiro
efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo}


{-
Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
-}

hoyo :: Obstaculo
hoyo = obstaculoSuperableSi superaHoyo efectoHoyo
superaHoyo tiro = (between 5 20.velocidad) tiro && vaAlRasDelSuelo tiro
efectoHoyo _ = tiroDetenido

--palosUtiles :: Jugador -> Obstaculo -> [Palo]

palosUtiles jugador obstaculo = filter (leSirve jugador obstaculo) palos

leSirve jugador obstaculo = supero.obstaculo.golpe jugador

supero = (/=tiroDetenido)

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera tiro [] = 0
cuantosObstaculosConsecutivosSupera tiro (obstaculo : obstaculos)
  | supero (obstaculo tiro)
      = 1 + cuantosObstaculosConsecutivosSupera (obstaculo tiro) obstaculos
  | otherwise = 0

--paloMasUtil :: Jugador -> [Obstaculo] -> Palo

paloMasUtil jugador obstaculos = maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos.golpe jugador) palos

type Lista = [(Jugador, Puntos)]

jugadorDeTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo
  = (map (padre.jugadorDeTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador
  = (all ((< puntosGanados puntosDeUnJugador).puntosGanados)
      . filter (/= puntosDeUnJugador)) puntosDeTorneo