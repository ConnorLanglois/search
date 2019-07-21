# Search

An implementation of a variety of AI classical and local search algorithms to solve Robot World by moving a robot in a square grid to a specific goal coordinate and to solve the N-Queens problem by placing n unattacking queens on a chess board.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

```
Lisp
Quicklisp
```

### Installing

Clone the repo.

### Running

Robot World:
1. Point current working directory to `src/robot/src`.

2. Run `robot.lisp`.

N-Queens:
1. Point current working directory to `src/queens/src`.

2. Run `queens.lisp`.

	NOTE: For some reason, the output of the black queen UTF-8 character
		in the example runs pushes the line one character too far to
		the right.
		This causes the grid of the world to not line up correctly.
		This issue does not persist when run inside the editor (Visual Studio Code).

NOTE: The output consists of UTF-8 characters.
	There is a line of code in each lisp file (`robot.lisp` and `queens.lisp`)
	that will set the default encoding of SBCL to UTF-8.
	If you are not compiling with SBCL, you must make sure the default encoding
	of your compiler is UTF-8.

## Authors

* **Connor Langlois** - [ConnorLanglois](https://github.com/ConnorLanglois)

## License

This project is not licensed.
