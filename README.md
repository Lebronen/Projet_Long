# cotrez-shay-plong-2024

Au préalable il faut avoir les librairies raylib et yojson installé :

```
opam depext raylib
opam install raylib
```

```
opam install yojson
```

Le programme se compile en executant
```
cd jeu
dune build
```

Une fois compilé il suffit d'executer 
```
dune exec jeu
```
 pour lancer le jeu.

Pour se déplacer avec le personnages il faut appuyer sur les touches directonnelles gauche et droite.
La touche directionnelle du haut permet au personnages de sauter.
Et maintenant la touche espace déclenche le grappin du personnage le faisant se balancer de gauche à droite à la manière d'un pendule

1)
Ce projet répond au problème de créer un jeu vidéo graphique complexe en langage fonctionnel (ocaml en l'ocurrence)

2)
L'objcetif principal est de créer un jeu de type métroid-vania en 2D dans l'univers du manga l'attaque des titans avec des plateformes et des pnj (IA) au long du jeu.
Nous pensons réaliser les designs et animations à l'aide d'une intelligence artificielle.
Si nous avons le temps nous aimerions également ajouter des fonctionnalités en réseau (multijoueur,...).

3)
Le projet étant graphique, les tests seront pour la plupart visuels et une fois le jeu lancé, mais nous essaierons d'implémenter tout de même des tests unitaires (surtout pour la potentielle partie réseau en testant les différentes étapes de création de socket et de connexion)

4)
Le projet est original déjà dans son sa conception (à ma connaissance il n'existe pas de jeu metroid-vania dans l'univers de l'attaque des titans), mais également dans son implémentation (en ocaml)

5)
Comme dit précédemment aucun jeu de ce type n'existe pour le moment, nous devrons donc tout créer à l'exception des librairies graphiques utilisées.

6)
Le but premier sera d'avoir l'interface graphique fonctionnelle avec les plateformes, ensuite d'y rajouter les différents personnages (joueur comme non-joueur), puis implémenter les animations, et enfin (si le temps nous le premet), un mode multijoueur en réseau.

