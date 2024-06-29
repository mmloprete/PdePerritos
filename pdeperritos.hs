import Text.Show.Functions()

{-ACLARACION: Cuando utilizo > o menor, se que puedo usar aplicacion parcial con composición (>) o (<), pero no la utilizo
debido a que en mi opinión mi código queda mucho menos expresivo-}
data Perrito = UnPerrito {
    raza                    :: String,
    juguetesFavoritos       :: [String],
    tiempoEnGuarderia       :: Int, {-El tiempo es en minutos-}
    energia                 :: Int
} deriving Show

chuck :: Perrito
chuck = UnPerrito {
    raza                = "dalmata",
    juguetesFavoritos   = ["pelota","patito","juguete1","juguete2","juguete3"],
    tiempoEnGuarderia   = 900,
    energia             = 90
}

{-mappers-}
mapRaza :: (String -> String) -> Perrito -> Perrito
mapRaza fn perrito = perrito {raza = fn.raza $ perrito}

mapJuguetesFavoritos :: ([String] -> [String]) -> Perrito -> Perrito
mapJuguetesFavoritos fn perrito = perrito {juguetesFavoritos = fn.juguetesFavoritos $ perrito}

mapTiempoEnGuarderia :: (Int -> Int) -> Perrito -> Perrito
mapTiempoEnGuarderia fn perrito = perrito {tiempoEnGuarderia = fn.tiempoEnGuarderia $ perrito}

mapEnergia :: (Int -> Int) -> Perrito -> Perrito
mapEnergia fn perrito = perrito {energia = fn.energia $ perrito}
{-mappers-}

{-Parte A-}
type Rutina = [Actividad]
type Actividad = (Ejercicio, Int)
type Ejercicio = Perrito -> Perrito

{-Ejercicios:-}

{-jugar: disminuye en 10 unidades la energía del perrito 🪫. ¡No puede quedar un valor negativo!-}
jugar :: Ejercicio
jugar perrito = mapEnergia (max 0.subtract 10) perrito

{-ladrar: aumenta la energía la mitad de los ladridos que se establezcan. 🗣️-}
ladrar :: Int -> Ejercicio
ladrar cantidadDeLadridos perrito = mapEnergia (+ dividirLadridosEnDos cantidadDeLadridos) perrito

dividirLadridosEnDos :: Int -> Int
dividirLadridosEnDos ladridos = div ladridos 2

{-regalar: ¡cómo no le vamos a dar un juguetito! Añade el juguete que se especifique a los favoritos. 🎁-}
regalar :: String -> Ejercicio
regalar regalo perrito = mapJuguetesFavoritos (++[regalo]) perrito

{-diaDeSpa: si el perro va a permanecer 50 minutos como mínimo en la guardería o es de raza extravagante, su energía pasa a ser 100 🔋 
y se le regala el juguete "peine de goma” 🪮. Si no, no pasa nada. Las razas extravagantes son dálmata y pomerania. -}

esDeRazaExtravagante :: Perrito -> Bool
esDeRazaExtravagante perrito = raza perrito == "dalmata" || raza perrito == "pomeraria" 
{-dálmata lo pongo sin tilde ya que no se muestra bien en consola-}

permaneceMinCincuentaMinutos :: Perrito -> Bool
permaneceMinCincuentaMinutos perrito = tiempoEnGuarderia perrito >= 50

energiaEnCien :: Int -> Int
energiaEnCien energia = 100

diaDeSpa :: Ejercicio
diaDeSpa perrito 
    | esDeRazaExtravagante perrito || permaneceMinCincuentaMinutos perrito = (regalar "peine de goma").(mapEnergia energiaEnCien) $ perrito
    | otherwise                                                            = perrito

{-diaDeCampo: ¡nada más lindo que ver perritos jugar! Lástima que así siempre pierden el primer juguete.😅-}
diaDeCampo :: Ejercicio
diaDeCampo = perderPrimerJuguete.jugar

perderPrimerJuguete :: Ejercicio
perderPrimerJuguete perrito = mapJuguetesFavoritos (drop 1) perrito

{-Modelar: A Zara, una perra dálmata que tiene como juguetes favoritos una pelota y una mantita, permanece 1 hora y 
media en la guardería y su energía es de 80. -}
zara :: Perrito
zara = UnPerrito {
    raza                = "dalmata",
    juguetesFavoritos   = ["pelota","mantita"],
    tiempoEnGuarderia   = 90,
    energia             = 80
}

{-La Guardería PdePerritos que tiene como rutina ciertas actividades (ver enunciado) -}
guarderiaPdePerritos :: Rutina
guarderiaPdePerritos = [(jugar, 30),(ladrar 18, 20),(regalar "pelota", 0),(diaDeSpa, 120),(diaDeCampo, 720)]
{-Parte A-}

{-Parte B-}
{-Saber si un perro puede estar en una guardería. Para eso, el tiempo de permanencia tiene que ser mayor que el de la rutina. 🕰️-}
obtenerEjercicio :: Actividad -> Ejercicio
obtenerEjercicio (ejercicio, _) = ejercicio

obtenerTiempo :: Actividad -> Int
obtenerTiempo (_,tiempo) = tiempo

obtenerTiemposActividadesRutina :: Rutina -> [Int]
obtenerTiemposActividadesRutina guarderia = map obtenerTiempo guarderia

obtenerTiempoRutina :: [Int] -> Int
obtenerTiempoRutina listaTiempos = sum listaTiempos

puedeEstarEnGuarderia :: Perrito -> Rutina -> Bool
puedeEstarEnGuarderia perrito guarderia = tiempoEnGuarderia perrito > (obtenerTiempoRutina.obtenerTiemposActividadesRutina $ guarderia)

{-Reconocer a perros responsables. Estos serían los que aún después de pasar un día de campo siguen teniendo más de 3 juguetes. 🧸-}
tieneMasDeTresJuguetesTrasDiaDeCampo :: Perrito -> Bool
tieneMasDeTresJuguetesTrasDiaDeCampo perrito = length (juguetesFavoritos.diaDeCampo $ perrito) > 3

perrosResponsables :: [Perrito] -> [Perrito]
perrosResponsables perritos = filter tieneMasDeTresJuguetesTrasDiaDeCampo perritos

{-Que un perro realice una rutina de la guardería (que realice todos sus ejercicios). 
Para eso, el tiempo de la rutina no puede ser mayor al tiempo de permanencia. 
En caso de que esta condición no se cumpla, el perro no hace nada. 🐕-}

realizarEjercicios :: Perrito -> Rutina -> Perrito
realizarEjercicios perrito rutina = foldr ($) perrito (map obtenerEjercicio rutina)

realizarRutina ::  Rutina -> Perrito -> Perrito
realizarRutina rutina perrito
    | (obtenerTiempoRutina.obtenerTiemposActividadesRutina $ rutina) <= tiempoEnGuarderia perrito = realizarEjercicios perrito rutina
    | otherwise                                                                                   = perrito

{-Dados unos perros, reportar todos los que quedan cansados después de realizar la rutina de una guardería. 
Es decir, que su energía sea menor a 5 luego de realizar todos los ejercicios. 💤-}
estaCansado :: Perrito -> Bool
estaCansado perrito = energia perrito < 5

perrosRealizanRutina :: Rutina -> [Perrito] -> [Perrito]
perrosRealizanRutina rutina perritos = map (realizarRutina rutina) perritos 

perrosCansados :: Rutina -> [Perrito] -> [Perrito]
perrosCansados rutina perritos = filter estaCansado (perrosRealizanRutina rutina perritos) 
{-Parte B-}

{-Parte C-}
perritoPi :: Perrito
perritoPi = UnPerrito {
    raza                = "labrador",
    juguetesFavoritos   = juguetesDePi,
    tiempoEnGuarderia   = 314,
    energia             = 159
}

juguetesDePi :: [String]
juguetesDePi = map (\unNumero -> "soguita " ++ show unNumero) [1..]

{-Luego de modelar a Pi, respondé las siguientes preguntas justificando y escribiendo la consulta que harías en la consola:-}
{-¿Sería posible saber si Pi es de una raza extravagante?-}
{-Es posible, ya que haskell usa lazy evaluation llamaría a la funcion esDeRazaExtravagante perritoPi y debería solamente analizar 
su raza y devolver false (debido a que es labrador). Al usar lazy evaluation no va a analizar sus juguetes en este caso que son infinitos
-}

{-Que pasa ¿Qué pasa si queremos saber si Pi tiene…
… algún huesito como juguete favorito? 
… alguna pelota luego de pasar por la Guardería de Perritos?
… la soguita 31112?
-}

{-Desarrollaría las siguientes funciones para mas facilidad y llamaría a tieneXJuguete "huesito" perritoPi-}
tieneXJuguete :: String -> Perrito -> Bool
tieneXJuguete juguete perrito = juguete `elem` juguetesFavoritos perrito
{-Nunca tendremos una respuesta clara, ya que haskell se va a quedar infinitamente buscando el juguete-}

tieneXJugueteTrasGuarderiaDePerritos :: String -> Perrito -> Bool
tieneXJugueteTrasGuarderiaDePerritos juguete perrito = tieneXJuguete juguete (realizarRutina guarderiaPdePerritos perrito)
{-Es el mismo caso que el anterior subpunto, al realizar rutina se le añade una pelota
pero nunca llega a analizarse por la lista infinita de soguitas, por lo que no puede devolver una respuesta .
Ejecutaría tieneXJugueteTrasGuarderiaDePerritos "pelota" perritoPi-}

{-En el caso de la soguita, tardaría un poco mas en devolver la respuesta (aunque no lo notaríamos) ya que ni bien encuentre
la soguita 31112 nos devolvería true.
Para obtener esta respuesta ejecutaría tieneXJugueteTrasGuarderiaDePerritos "soguita 31112" perritoPi -}

{-¿Es posible que Pi realice una rutina?-}
{-Es posible, aunque al mostrar a pi luego de realizar la rutina tendría infinitas sogas y nos molestaría para ver como la rutina
afecta a perritoPi.
Ejecutaría realizarRutina guarderiaPdePerritos perritoPi-}

{-Si le regalamos un hueso a pi, el punto 2.a de la parte C (la de los huesitos) sería exactamente igual, es decir evaluaría 
infinitamente (debido a que al crear la función regalar añade el huesito al final de la lista), si hubiera usado la función
preceder en lugar de concatenacion para añadir elementos a la lista, la función devolvería true debido a que el huesito se encontraría 
al inicio . Ejecutaría tieneXJugueteTrasGuarderiaDePerritos "huesito" (regalar "huesito" perritoPi) para analizar este caso-}
{-Parte C-}