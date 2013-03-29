Sudoku Solver
=============

This is a sudoku solver application created as a training exercise while
learning Haskell.

Usage
-----

Start by downloading the repo to your local machine. It is not uploaded to
Haskells package index Hackage and I doubt it will, so you'll need to use git.

.. code-block:: bash

    $ git clone https://github.com/Damgaard/sudoku.git
    $ cd Sudoku

After downloading, install it to your system using cabal.

.. code-block:: bash

    $ cabal configure
    $ cabal build
    $ cabal install

You can now run the program using the installed executable ``Sudoku``.

The program takes a 81 long string composed of ints in the range of 0 to 9,
each int represents a pos on the sudoku board. So the first int in the argument
represent the first pos i.e top left, the second int represent the second pos
i.e. top second-most left etc.

The value represent the value the pos has at initialization with 0 representing
an unknown value.

As and example try:

.. code-block:: bash

    $ Sudoku 248395716071628349906741582682539174009174628714862953863417295195286437427953861
