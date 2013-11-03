# cl-checkers

cl-checkers is a simple checkers game and AI written in Common Lisp
and a Common Lisp learning project.

## Details

Searching is done using minimax with alpha-beta pruning. This code is
*lightly* optimized. On average, 8-11 plys are searched in 1 second,
depending on whether there are kings on the board.

Fair warning: the AI sucks at endgames. Especially two kings vs one
kings endgames. It is smart enough to not lose, but not smart enough
to win. However, it is sometimes kind enough to realize that it cannot
win a two king vs one king endgame and may put itself out of its
misery.

The initial structure of the game was inspired by the Othello project
in chapter 18 of Peter Norvig's Paradigms of Artificial Intelligence
Programming: Case Studies in Common LISP.

An endgame database from http://www.fierz.ch/cake.php was implemented,
but results were not satisfactory with the used evaluation
functions. This would be a good area to re-visit in future.

### Transposition Tables

 **With the default settings, checkers will crash due to heap
exhaustion if more than one second is allowed for each computer player
move.**

Transposition tables are implemented and enabled by default, but
should be disabled when giving the computer players more than 1 second
per move due to their memory requirements. I do not know how much
memory is required for using transposition tables with greater than 1
second moves. If sbcl is allowed insufficient memory for the hash
tables, it will crash due to heap exhaustion. More memory can be
allowed by increasing the numeric argument to the `--dynamic-size`
flag for sbcl.

To disable transposition tables, set `*enable-transposition-tables*`
to `nil` in run.lisp. Transposition tables definitely improve play, so
the user in encouraged to try enabling them when allowing the computer
one second per move. When transposition tables are disabled, less
`*time-headroom*` can be reduced to something like 0.02.


## Usage

### Installation

1. Install quicklisp. $ represents a bash shell and * an Lisp REPL.
```$ wget http://beta.quicklisp.org/quicklisp.lisp && sbcl --load quicklisp.lisp
   * (quicklisp-quickstart:install)
   * (ql:add-to-init-file)
   * (sb-ext:exit)
```


2. Configure ASDF2:
```$ mkdir -p ~/.config/common-lisp/source-registry.conf.d/```

~/.config/common-lisp/source-registry.conf.d/projects.conf should
contain the following contents:
```(:tree (:home "Documents/lisp/"))```

"Documents/lisp/" is a path relative to your home directory that ASDF
will search for lisp projects in.

3. Run checkers:
```$ sbcl --dynamic-size 2048 --load run.lisp```

For the sake of your sanity, suggest running sbcl through rlwrap:
```$ rlwrap sbcl```

### Configuration

The board scale, textual representation of the squares/pieces, and the
color of the squares/pieces can be modified by editing run.lisp. The
commands necessary to manually load and interact with the program from
a REPL can be seen in run.lisp.


### Runtime

Upon startup, you will be prompted to specify which, if any, of the
two players will be computers, whether to load a game from a file or
start a new one, and how much time to give the computer player(s) for
each move.

Every square on the board is assigned a number and moves are specified
as cons cell of the source square and a *movement vector*. For
example, to move a piece from 38 to 47, one would enter (38
. 9). Therefore, the following mapping exists between movement
directions and vectors: ((NW . -11) (NE . -9) (SW . 9) (SE . 9)).
Jumps are handled in a similar fashion, with the vector pointing to
the final destination: ((NW-J . -22) (NE-J . -18) (SW-J . 18) (SE-J . 11)).
No vectors other than these eight are valid movement vectors. n-jumps are
treated as n successive moves by the same player, so each step of the jump
must be entered individually.

During gameplay, a primitive command shell is available to the
player. A list of commands can be viewed by entering (help) when
prompted for a move. Currently, only the (help), (print-board), and
(dump-board) commands are implemented, but this could be extended to
act as a full debugging and game control console. The input (including
move input) is robust against erroneous input and will re-prompt, with
the exception mismatched closing parenthesis, which will trigger an
error.

