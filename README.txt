# algo_steganographie

Commandes make:

clean: supprime les fichiers cmi, cmo et l'image de sortie du programme
setup: initialise opam et installe ocaml 4.14.0
install-libs: installe toutes les bibliothèques nécessaires au bon fonctionnement du programme
all: compile le programme et donne un exécutable RSA

Pour lancer le programme:
$ make all
$ ./RSA <image bmp> <texte a encrypter>

Le texte doit être d'une taille maximale de 15 caractères (dépendant de n donc peut varier.)
Le programme va afficher les clés RSA utilisées ainsi que le chiffrement / déchiffrement du message dans l'image.
L'image contenant le message crypté est "mario_res.bmp"