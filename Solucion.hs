module Solucion where

-- Nombre de Grupo: Haskelleros
-- Integrante 1: Ignacio Fullin, nacho.m.fullin@gmail.com, 1718/21
-- Integrante 2: Joel Mastroiaco, joelmastroiaco@gmail.com, 1075/22
-- Integrante 3: Paulo Accardo, paulo.matias.accardo@gmail.com, 836/22
-- Integrante 4: Decidio no continuar con el tp

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])


-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- Ejercicio 1.
-- Recibe una red social y extrae el primer elemento de la tupla, que es una lista de usuarios y llama a la funcion auxiliar nombresDe.
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (us,rs,ps) = nombresDe us

-- Recibe una lista (de usuarios) y extrae el nombre de cada usuario de esa secuencia con la funcion nombreDeUsuario para armar una lista con dichos nombres.
nombresDe :: [Usuario] -> [String]
nombresDe [] = []
nombresDe (x:xs) = nombreDeUsuario x : nombresDe xs

-- Ejercicio 2.
-- Toma una RedSocial y un Usuario, y devuelve la lista de usuarios que se relacionan (son amigos) con el primero.
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us, rs, ps) n = amigos (relaciones(us, rs, ps)) n

-- Toma las relaciones de la RedSocial y chequea quienes son los amigos del usuario dado.
amigos :: [Relacion] -> Usuario -> [Usuario]
amigos [] nm = []
amigos (x:xs) n | n == fst x = snd x : amigos xs n
                | n == snd x = fst x : amigos xs n
                | otherwise = amigos xs n

-- Ejercicio 3.
-- Toma una red social y un usuario, extrae los amigos del usuario con amigosDe, llama a la auxiliar.
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us, rs, ps) n = contadorDeAmigos (amigosDe (us, rs, ps) n)

-- Recibe una lista de usuarios y cuenta la cantidad de relaciones/amigos que tiene dicho usuario.
contadorDeAmigos :: [Usuario] -> Int
contadorDeAmigos [] = 0
contadorDeAmigos (x:xs) | xs == [] = 1
                        | otherwise = contadorDeAmigos xs + 1

-- Ejercicio 4.
-- Toma una red social y compara la cantidad de relaciones/amigos de cada usuario para devolver el usuario con mayor cantidad.
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ((x:xs), rs, ps) | xs == [] = x
                                     | cantidadDeAmigos ((x:xs), rs, ps) x >= cantidadDeAmigos ((x:xs), rs, ps) (head(xs)) = usuarioConMasAmigos ((x : tail(xs)), rs, ps)
                                     | otherwise = usuarioConMasAmigos (xs, rs, ps)

-- Ejercicio 5.
-- Toma una red social y revisa si algun usuario tiene mas de 1000000 de amigos (en este caso lo hicimos con 10 amigos).
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos ((x:xs), rs, ps) | (cantidadDeAmigos ((x:xs), rs, ps) x) > 10 = True
                                   | otherwise = estaRobertoCarlos (xs, rs, ps)

-- Ejercicio 6.
-- Dada una red social y un usuario, extrae las publicaciones de la red social y se lo manda a la auxiliar junto al usuario
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe rd n = publicacionesDelUsuario (publicaciones(rd)) n

-- Recibe una lista de publicaciones de la red, junto un usuario y devuelve solo las publicaciones de ese usuario.
publicacionesDelUsuario :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDelUsuario [] _ = []
publicacionesDelUsuario (x:xs) n | usuarioDePublicacion x == n = x : publicacionesDelUsuario xs n
                                 | otherwise = publicacionesDelUsuario xs n

-- Ejercicio 7.
-- Recibe una red social y un usuario para armar una lista con las publicaciones que le gustan a dicho usuario. Para ello utiliza la auxiliar leGustaLaPublicacionA.
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) n = []
publicacionesQueLeGustanA (us, rs, (x:xs)) n | leGustaLaPublicacionA x n == True = x : publicacionesQueLeGustanA (us, rs, xs) n
                                             | otherwise = publicacionesQueLeGustanA (us, rs, xs) n

-- Dada una publicacion y un usuario, verifica si dicha publicacion le gusta al usuario.
leGustaLaPublicacionA :: Publicacion -> Usuario -> Bool
leGustaLaPublicacionA (us, txt, []) n = False
leGustaLaPublicacionA (us, txt, (x:xs)) n | x == n = True
                                          | otherwise = leGustaLaPublicacionA (us, txt, xs) n

-- Ejercicio 8.
-- Recibe una red social y dos usuarios, devuelve True solo si a ambos usuarios le gustan exactamente las mismas publicaciones.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs us1 us2 = publicacionesQueLeGustanA rs us1 == publicacionesQueLeGustanA rs us2

-- Ejercicio 9.
-- Recibe una red social y un usuario, devuelve True si existe otro usuario que le haya dado like a todas las publicacion del primero.
-- Para ello extremos las publicaciones del usuario y los likes de la primera publicacion y los pasamos a usuariosFieles.
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (_,_,[]) _ = False
tieneUnSeguidorFiel rs a = usuariosFieles (publicacionesDe rs a) (likesDePublicacion (head(publicacionesDe rs a)))

-- Dados una lista de publicaciones y una lista de usuarios(los likes), chequea si alguno de esos usuarios cumple la funcion esFiel.
usuariosFieles :: [Publicacion] -> [Usuario] -> Bool
usuariosFieles pbs [] = False
usuariosFieles pbs likes | esFiel pbs (head(likes)) == True = True
                         | otherwise = usuariosFieles pbs (tail(likes))

-- Dada una lista de publicaciones y un usuario, devuelve True si ese usuario le dio like a todas las publicaciones de la lista.
esFiel :: [Publicacion] -> Usuario -> Bool
esFiel [] n = True
esFiel (x:xs) n | leGustaLaPublicacionA x n == True = esFiel xs n
                | leGustaLaPublicacionA x n == False = False

-- Ejercicio 10.
-- Recibe una red social y dos usuario y verifica si existe una cadena de amigos entre estos.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs us1 us2 = verificador rs us2 (amigosDe rs us1) [us1]

-- Dado un usario y una lista de usuarios, primero chequea si el usuario pertenece a la lista o si la lista de amigos es vacia (implicando que ya fue chequeada), sino llama a las auxiliares para probbar con los amigos de amigos.
verificador :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
verificador rs us2 as lista | pertenece us2 as = True
                            | listaVerificada as lista == [] = False
                            | otherwise = verificador rs us2 (amigosDeAmigos rs as) (lista ++ listaVerificada as lista)

-- Dada una red social y una lista de usuarios, devuelve una lista con todos los amigos de cada uno de esos usuario.
amigosDeAmigos :: RedSocial -> [Usuario] -> [Usuario]
amigosDeAmigos _ [] = []
amigosDeAmigos rs (x:xs) | xs == [] = amigosDe rs x
                         | otherwise = (amigosDe rs x) ++ (amigosDeAmigos rs xs)

-- Toma un usuario y una lsita de usuarios para chequear si el usuario pertenece a la lista.
pertenece :: Usuario -> [Usuario] -> Bool
pertenece us [] = False
pertenece us (x:xs) | x == us = True
                    | otherwise = pertenece us xs

-- Recibe dos listas de usuarios, chequea si los elementos de la primera estan en la segunda, y devuelve la primera menos los elementos de la segunda.
listaVerificada :: [Usuario] -> [Usuario] -> [Usuario]
listaVerificada [] ys = []
listaVerificada (x:xs) ys | pertenece x ys = listaVerificada xs ys
                          | otherwise = x : listaVerificada xs ys