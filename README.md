# Attack on Titouans
## Project
The goal of the project is to create 2D plateform game with Ocaml langage, in a functionnal and monadic style using the less possible side effects.

## Requirements
Raylib and YoJson libraries are needed : 

### Install raylib
```
opam depext raylib
opam install raylib
```

### install yojson

```
opam install yojson
```

## Building and running

``
cd jeu
``

then build using

``
dune build
``

and run with

``
dune exec jeu
``

## How to play

- use arrows to move your character (up arrow to jump).
- use "space" key to launch your hook. It will cling on the closest reachable object. But be carefull, you only have limited usage. The limit is represented by the blue bar in the superior left corner.
- use "e" to attack your ennemies.
- If you touch an ennemi without killing him you are going to lose health points. Your health points are represented by a green bar in the superior left corner.

## Contributing

To contribute to the project, just create a new branch to work on and make a merge request (chose Lebronen (me) to review your MR) 
There are two ways you can contribute to the project

### 1) Creating new levels
If you are not a big fan of the Ocaml langage, you can contribute anyway to the project by creating new levels. Level building is quite esay using Json. 

```
"platforms": [
      {
        "x": 500,
        "y": 400,
        "width": 600,
        "height": 30
      },
...
```
in the "plateform section you add as much plateform as you want and indicate their length and where you want them to be placed on the map.

```
"ennemis": [
      {
        "sprite": "../resources/knight.png",
        "xmin": 1100.0,
        "xmax": 1300.0,
        "y": 150.0
      },
...
```
for the ennemies you have to precise the path to the png file you want to use, the points between wich the ennmi will "patrol" (move from one point to the other until his death or until the end of the universe, that's up to you), and finally his position on y axis.

Once your are done creating unbelievably difficult levels you can add them to the level list in jeu/jeu.ml file (I trust you to find it in the code independently of your Ocaml skills). Then just build again as written in the corresponding dection of this Readme and have fun !

## 2) Contributing to the code
If you happen to be an Ocaml fan you can contribute to the source code of the project. As you can see the game is minimalist and still in developpement. Here are some features you could work on : 

- create new weapons and objects for the character
- improve ennemies (maybe using AI)
- add new level design elements
- make the game multiplayer (local and online)
- Add tests (also maybe a CI pipeline)

of course feel free to add more features, it's only a little list of things we thought could be usefull
