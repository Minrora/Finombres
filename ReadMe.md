# Les Finombres 

étude d’une dynamique arithmétique par Romain Bietrix dans le cadre de l'épreuve du TIPE de l'année 2025 - "transformation".

## Description

Dans le cadre de ce travail, nous nous intéressons à une suite définie de manière originale par
une relation de récurrence faisant intervenir le renversement des chiffres d’un entier naturel. Plus
précisément, l’opération consiste à soustraire un entier par sa version renversée. Cette transfor-
mation simple en apparence donne naissance à des suites d’entiers présentant des comportements
parfois inattendus, et constitue le point de départ de notre étude.

Le thème du TIPE de cette année, « transformation », m’a conduit à concevoir cette opération inspirée de l’arithmétique des chiffres. En observant les évolutions de ces suites, il est apparu
pertinent de distinguer deux types d’entiers : les Finombres, qui atteignent un état stable après
un nombre fini d’itérations, et les Infinombres, qui ne semblent jamais se stabiliser.

L’objectif de ce travail est de modéliser, analyser et classifier ces comportements à l’aide
d’une approche à la fois algorithmique et mathématique. L’étude de cette dynamique, que nous
avons nommée suite des Finombres, vise à mettre en lumière ses propriétés structurelles, ses
régularités ainsi que d’éventuelles anomalies.

Ce dépot GitHub sert de support pour le TIPE.

## Getting Started

### Dependances

* Se projet compile en OCaml et utilise la librairie Graphics.

Installer OCaml : https://ocaml.org/install

Ajouter la librairie Graphics : 
```
opam install graphics
```

### Installation

Pour installer le projet :
* cloner d'abord le projet
```
git clone [URL fournit par github]
```
* lancer le makefile avec les différent argument possibles. (tapoter tab pour voir les suggestions)
```
make arg
```

### Structure

Le dépot est organisé en une variété de fichiers.

* main.ml - Fichier appelant les autres méthodes dans les autres fichiers.
* Perso.ml - Contient les fonctions de bases utiles au TIPE sur les Finombres.
* plan.ml - Contient les fonctions pour représenter les ensembles E_4, E_n (voir rapport).

Création de graphiques : 
* younggraph.ml - Créer un graphe de représentation des palintuples d'une base a l'autre.
* graphgenerator.ml - Construit le graphe des successeur dans un format .dot.
* graphicgenerator.ml - Utilitaire pour manipuler le module graphics plus simplement.
* pixelure.ml - Construit l'image représentant les Finombres et les Infinombres.

Utilitaire : 
* writer.ml - Fichier de fonctione intermediaire permettant d'écrire dans un fichier.
* scale.ml - Permet de manipuler les sequence de nombre plus facilement.

## Execution
Voici les différentes commandes faisable avec `make`.
* `find_nature` - trouve la nature de tout les nombres de 0 a 10·000·000
* `find_vol` - trouve le temps de vol des nombres de 10·000 a 1·000·000 (je crois ??)
* `graphic` - Affiche la fonction de répartition des Finombres et Infinombres jusqu'à 50000.
* `grid_vol` - Affiche un graphique des temps de vol des entier.
* `kpalintiple` - Calcule les k-palintuples et affiche 10 images de kpalintuples. (cursed un peu je comprend pas ce qui s'affiche x))) 
* `nature_finder_from` - Trouve la nature des entier depuis un certain nombre de départ.
* `pixel` - Génère une sequence d'infinombre et finombre et affiche leur nature.
* `plan` - TODO
* `show_them` - Affiche la répartition des premières centaines d'infinombres.

## Auteurs

Contributors names and contact info

* [@DomPizzie](https://twitter.com/dompizzie)
* Minrora13@gmail.com
## License

This project is licensed under the Free License - see the LICENSE.md file for details 

## Bibliographie

* L. H. Kendrick — Young Graphs : 1089 et al. , Journal of Integer Sequences, Vol. 18
(2015), Article 15.9.7
* OEIS, suite A031877 (Nontrivial reversal numbers).
