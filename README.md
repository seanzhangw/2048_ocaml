# 2048 Game in OCaml

## Introduction
This project is an implementation of the popular 2048 game, written in OCaml and utilizing the Cornell CS 3110 class OPAM libraries. The game features a graphical user interface powered by Raylib and is built using the Dune project manager.

## Installation

### Prerequisites
- Ensure you have [OPAM](https://opam.ocaml.org/) installed on your system.
- [Dune](https://dune.build/) is used for building and managing the project.

### Installing Dependencies
1. Install the base libraries required for the project:
>opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc
2. Install the Raylib library for the graphical user interface:
>opam depext raylib

>opam install raylib
3
## Building and Running

### Building the Game
Run the following command to build the game:
>make build

### Running the Game
To start the game, use:
>make game


### Other Commands

- **Running Tests**: To run tests, execute `make test`.
- **Interactive Console**: For an interactive console, use `make utop`.
- **Cleaning Build Files**: To clean up build files, run `make clean`.
- **Documentation**: Generate documentation using `make doc` and open it with `make opendoc`.
- **Zipping Project**: To zip the project, use `make zip`.
- **Code Count**: To count the lines of code, execute `make count`.

## Acknowledgments
- Contributors: Dora Weng (cw749), Vicky Wang (vw92), Linda Sun (lhs86), Sean Zhang (swz9)
- Special thanks to the Cornell CS 3110 course team.

