# Hamaze

A simple command line maze generator written in Haskell!

## Motivation

This project implements a
[randomized depth first
search](https://en.wikipedia.org/wiki/Maze_generation_algorithm#Randomized_depth-first_search)
algorithm to generate a maze; the algorithm itself is fairly simple,
but is ususally described in a general manner and around iterative
language concepts. In developing this project, I wanted to discover
the differences between reading an algorithm described iteratively and
then implementing it in a pure functional language.

Specific pain points I discovered was in translating sequences of
steps into compositions of functions: particularly interesting was
that functional languages seem to require a "reverse nesting" of
logic: the steps that must take place first must be placed *into*
functions that perform later steps: for example, the function which
removes a wall between two cells must require the results of choosing
an unvisited neighbour: syntactically, in Haskell, this often means
that the logic choosing a cell comes *after* the logic for removing
the wall.

Technically, this is obvious: the only way to use the result of a
function in another function is to calculate it first. But, since most
programmers are used to reading control flow from top-to-bottom and
left-to-right, this kind of syntax can be a little difficult to
reconcile.

## Hamaze, the project

### Building
This project uses cabal, and can be built with the command `cabal
build`.  It only has one dependency, `random` (which itself depends on
the `splitmix` library), which it uses to generate random numbers (to
facilitate maze randomness). Cabal will download and link this
dependency automatically.

### Exploring the code

Individual functions can be experimented with cabal's repl functions,
but in order to access the full spectrum of functions, you'll want to
target the exe: `cabal repl exe:hamaze` (other wise, the repl will
only load the library, which only contains the non-user-interface
logic).

### Executable

The final executable will be called `hamaze.exe` (on Windows).

The command line interface includes input validation.

- Just the exe:
``` shell
> hamaze
Usage: hamaze [OPTION...]
  -d Integer x Integer  --dimensions=Integer x Integer  The dimensions of the maze to generate, in nxn format.
  -s Single Integer     --seed=Single Integer           The seed to use to generate the maze
```
- Using all options:
``` shell
> hamaze -d 5x5 -s 1234
#===#===#===#===#===#
|           |       |
#===#===#   #   #   #
|           |   |   |
#   #===#===#   #   #
|   |           |   |
#   #   #===#===#   #
|   |   |       |   |
#   #===#   #   #   #
|           |       |
#===#===#===#===#===#
```
- Invalid option syntax:
``` shell
> hamaze -d 5xc -s 1234
The dimensions must be given in an nxn format (e.g. for a 5 by 3 maze, '-d 5x3')
```

