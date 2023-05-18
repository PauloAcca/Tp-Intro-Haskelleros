module TestSolucion where
import Test.HUnit
import Solucion

main = runTestTT todosLosTest

todosLosTest = test [testsuite1, testsuite2, testsuite3, testsuite4, testsuite5, testsuite6, testsuite7, testsuite8, testsuite9, testsuite10]

-------------- test nombres de usuarios ------------------------
testsuite1 = test [
    "testnombresUsuarios 1" ~: (nombresDeUsuarios redSocialVacia) ~?= [], --red social vacía
    "testnombresUsuarios 2" ~: (nombresDeUsuarios redSocialUnUsuario) ~?= ["Juan"], --red social un usuario
    "testnombresUsuarios 3" ~: (nombresDeUsuarios redA) ~?= ["Juan", "Natalia", "Pedro", "Mariela"] --red social varios usuarios
 ]

-------------- test amigosDe ------------------------
testsuite2 = test [
    "testAmigosDe 1" ~: (amigosDe redSocialVacia usuario1) ~?= [], --red social vacía
    "testAmigosDe 2" ~: (amigosDe redB usuario5 ) ~?= [], --red social sin amistades del usuario
    "testAmigosDe 3" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4] --red social con amistades del usuario
 ]

-------------- test CantidadDeAmigos ------------------------
testsuite3 = test [
    "testCantidadDeAmigos 1" ~: (cantidadDeAmigos redSocialVacia usuario1) ~?= 0, --red social sin usuarios
    "testCantidadDeAmigos 2" ~: (cantidadDeAmigos redB usuario5) ~?= 0, --red social sin amigos de
    "testCantidadDeAmigos 3" ~: (cantidadDeAmigos redA usuario1) ~?= 2 --red social con amigos de
 ]

 -------------- test usuarioConMasAmigos ------------------------
testsuite4 = test [
    "testUsuarioConMasAmigos 1" ~: (usuarioConMasAmigos redSocialUnUsuario ) ~?= usuario1, --red social con un solo usuario
    "testUsuarioConMasAmigos 2" ~: (usuarioConMasAmigos redSinRelaciones ) ~?= usuario1, --red social sin relaciones
    "testUsuarioConMasAmigos 3" ~: (usuarioConMasAmigos redConMismaCantAmigos ) ~?= usuario1, --red social con misma cantidad de amigos en todos los usuarios
    "testUsuarioConMasAmigos 4" ~: (usuarioConMasAmigos redB ) ~?= usuario2 --red social con alguien con mas amigos
 ]

-------------- test estaRobertoCarlos ------------------------
testsuite5 = test [
    "testEstaRobertoCarlos 1" ~: (estaRobertoCarlos redSocialVacia ) ~?= False, --red social sin usuarios
    "testEstaRobertoCarlos 2" ~: (estaRobertoCarlos redB ) ~?= False, --red social con usuarios que no cumplan
    "testEstaRobertoCarlos 3" ~: (estaRobertoCarlos red10Amigos ) ~?= True --red social con usuarios que cumplan
 ]

 -------------- test PublicacionesDe ------------------------
testsuite6 = test [
    "testPublicacionesDe 1" ~: (publicacionesDe redSinPublicaciones  usuario2) ~?= [], --red social sin publicaciones
    "testPublicacionesDe 2" ~: (publicacionesDe redUnaPublicacionDeUsuario  usuario3) ~?= [publicacion3_1], --red social con una sola publicacion del usuario
    "testPublicacionesDe 3" ~: (publicacionesDe redA usuario1) ~?= [publicacion1_1, publicacion1_2] --red social con muchas publicaciones del usuario
 ]

 -------------- test PublicacionesQueLeGustanA ------------------------
testsuite7 = test [
    "testPublicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redSinPublicaciones  usuario2) ~?= [], --red social sin publicaciones
    "testPublicacionesQueLeGustanA 2" ~: (publicacionesQueLeGustanA redB  usuario1) ~?= [], --red social sin publicaciones que le gusten al usuario
    "testPublicacionesQueLeGustanA 3" ~: (publicacionesQueLeGustanA redUnaPublicacionDeUsuario usuario2) ~?= [publicacion1_3], --red social con una sola publicacion que le guste al usuario
    "testPublicacionesQueLeGustanA 4" ~: (publicacionesQueLeGustanA redA usuario4) ~?= [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2] --red social con varias publicaciones que le gusten al usuario
 ]
 -------------- test lesGustanLasMismasPublicaciones ------------------------
testsuite8 = test [
    "testLesGustanLasMismasPublicaciones 1" ~: (lesGustanLasMismasPublicaciones redSinPublicaciones  usuario2 usuario1) ~?= True, --red social sin publicaciones
    "testLesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB  usuario1 usuario3) ~?= True, --red social sin publicaciones que le gusten a los usuarios
    "testLesGustanLasMismasPublicaciones 3" ~: (lesGustanLasMismasPublicaciones redMismaCantPublicacionesLikeadasUnaSola usuario2 usuario4) ~?= True, --red social con una sola publicacion que le guste a ambos usuarios
    "testLesGustanLasMismasPublicaciones 4" ~: (lesGustanLasMismasPublicaciones redMismaCantPublicacionesLikeadas usuario2 usuario5) ~?= True, --red social con varias publicaciones que le gusten a ambos usuarios
    "testLesGustanLasMismasPublicaciones 5" ~: (lesGustanLasMismasPublicaciones redA usuario4 usuario2) ~?= False, --red social que le gusten mas publicacion al primer usuario
    "testLesGustanLasMismasPublicaciones 6" ~: (lesGustanLasMismasPublicaciones redA usuario2 usuario4) ~?= False --red social que le gusten mas publicacion al segundo usuario
 ]
 -------------- test tieneUnSeguidorFiel ------------------------
testsuite9 = test [
    "testTieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redSinPublicaciones usuario1) ~?= False, --red social sin publicaciones
    "testTieneUnSeguidorFiel 2" ~: (tieneUnSeguidorFiel redSinLikes usuario1 ) ~?= False, --red social sin publicaciones que le gusten a los usuarios
    "testTieneUnSeguidorFiel 3" ~: (tieneUnSeguidorFiel redUnaPublicacionUnLike usuario3) ~?= True, --red social con una publicacion del usuario y un seguidor fiel
    "testTieneUnSeguidorFiel 4" ~: (tieneUnSeguidorFiel redMismaCantPublicacionesLikeadasUnaSola usuario1) ~?= True, --red social con una publicacion del usuario y mas de un seguidor fiel
    "testTieneUnSeguidorFiel 5" ~: (tieneUnSeguidorFiel redA usuario2) ~?= True, --red social con varias publicaciones del usuario y un seguidor fiel
    "testTieneUnSeguidorFiel 6" ~: (tieneUnSeguidorFiel redSeguidoresFieles usuario1) ~?= True, --red social con varias publicaciones del usuario y mas de un seguidor fiel
    "testTieneUnSeguidorFiel 7" ~: (tieneUnSeguidorFiel redA usuario3) ~?= False --red social sin seguidor fiel
 ]
 -------------- test existeSecuenciaDeAmigos ------------------------

testsuite10 = test [
    "testExisteSecuenciaDeAmigos 1"  ~: (existeSecuenciaDeAmigos ([usuario1,usuario2],[],[]) usuario1 usuario3) ~?= False, --red social sin relaciones
    "testExisteSecuenciaDeAmigos 2"  ~: (existeSecuenciaDeAmigos redB usuario1 usuario3) ~?= True, -- red social con cadena 1 amigo
    "testExisteSecuenciaDeAmigos 3"  ~: (existeSecuenciaDeAmigos redA usuario2 usuario3) ~?= True, -- usuario2 y usuario3 son amigos directos
    "testExisteSecuenciaDeAmigos 4"  ~: (existeSecuenciaDeAmigos redSinCadenaDeAmigos usuario1 usuario4) ~?= False, -- red social sin cadena de amigos
    "testExisteSecuenciaDeAmigos 5"  ~: (existeSecuenciaDeAmigos redCadenaCompleja usuario1 usuario5) ~?= True -- red social con cadena de mas de un amigo
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Nacho")
usuario7 = (7, "Paulo")
usuario8 = (8, "Lioenel")
usuario9 = (9, "Leonel")
usuario10 = (10, "Carmelo")
usuario11 = (11, "Taichu")
usuario12 = (11, "Joel")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

redSocialVacia = ([],[],[])
redSocialUnUsuario = ([usuario1],[],[])
redSinRelaciones = (usuariosB, [], publicacionesB)
redConMismaCantAmigos = ([usuario1, usuario2, usuario3], [relacion1_2, relacion2_3, relacion1_3], publicacionesB)
red10Amigos = (usuariosA, [relacion1_2, relacion1_3, relacion1_4, (usuario1, usuario5), (usuario1, usuario6), (usuario1, usuario7), (usuario1, usuario8), (usuario1, usuario9), (usuario1, usuario10), (usuario1, usuario11), (usuario1, usuario12)], [])
redSinPublicaciones = (usuariosB,relacionesB,[])
redUnaPublicacionDeUsuario = (usuariosB, relacionesB, [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1])
redMismaCantPublicacionesLikeadasUnaSola = (usuariosA, relacionesA, [publicacion1_1, publicacion3_1, publicacion4_2,publicacion4_3])
redMismaCantPublicacionesLikeadas = (usuariosB, relacionesB, [publicacion1_3, publicacion1_4, publicacion3_1, publicacion3_3])
redSinLikes = (usuariosA, [], [publicacion1_4, publicacion3_1, publicacion4_2])
redUnaPublicacionUnLike = (usuariosB, relacionesB, [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_2])
redSeguidoresFieles = (usuariosA, relacionesA, [publicacion1_1, (usuario1, "Este es mi tercer post", [usuario2, usuario4]), (usuario1, "Este es mi segundo post", [usuario4,usuario2])])
redSinCadenaDeAmigos = (usuariosA, [relacion1_2, relacion3_4], [])
redCadenaCompleja = ([usuario1, usuario2, usuario3, usuario4, usuario5, usuario6], [relacion1_2, relacion2_3, relacion2_4, relacion3_4, (usuario4, usuario5), (usuario1, usuario6)], [])