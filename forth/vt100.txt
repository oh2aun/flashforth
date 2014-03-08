\ *********************************************************************
\ 
\    Filename:       vt100.txt
\    Date:           02.03.2014
\    FF Version:     5.0 
\    Author:         Attila Herman
\ *********************************************************************
\    FlashForth is licensed acording to the GNU General Public License*
\ *********************************************************************
\ Frequently used commands for VT100 compatible terminals
\ Unfortunately there is some differencies between terminal emulators!
\ Maybe some words doesn't work, or does another function depending on
\ terminal.

-vt100
marker -vt100

\ Auxiliary words
: esc[ #27 emit #91 emit ; \ 'esc' and '[' for starting escape sequence
: .n ( n -- )              \ Print n without separator space character
  0 <# #s #>  for dup c@ emit 1+ next drop ;
: \; [char] ; emit ;       \ Emit semicolon character

\ Erasing and cursor positioning words
: \h    esc[ [char] H emit ;               \ Cursor to home position
: \cls  esc[ [char] 2 emit [char] J emit ; \ Clear the screen
: \clsh \cls \h ;                          \ cls + home
: \el   esc[ [char] 2 emit [char] K emit ; \ Erase line
: \esl  esc[ [char] 1 emit [char] K emit ; \ Erase from start of line
: \eel  esc[ [char] 0 emit [char] K emit ; \ Erase to end of line
: \nl   esc[ [char] E emit ;    \ Next line
: \cu   esc[ [char] A emit ;    \ Cursor up
: \cun  esc[ .n [char] A emit ; \ Cursor up with n line
: \cd   esc[ [char] B emit ;    \ Cursor down
: \cdn  esc[ .n [char] B emit ; \ Cursor down with n line
: \cf   esc[ [char] C emit ;    \ Cursor foreward
: \cfn  esc[ .n [char] C emit ; \ Cursor foreward with n position
: \cb   esc[ [char] D emit ;    \ Cursor backward
: \cbn  esc[ .n [char] D emit ; \ Cursor backward with n position
: \cp   esc[ swap .n \; .n [char] f emit ; \ Cursor position to line, row
: \t    9 emit ;                \ Cursor to next tab position

\ Attributes
: \attr esc[ .n [char] m emit ; \ Set the current attribute
: \res  0 \attr ;       \ Reset attributes to default
: \bri  1 \attr ;       \ Bright
: \unl  4 \attr ;       \ Underline
: \bli  5 \attr ;       \ Blinked
: \rev  7 \attr ;       \ Reverse
: \hid  8 \attr ;       \ Hidden

\ Cursor on/off
: \c+   esc[ [char] ? emit [char] 2 emit [char] 5 emit [char] h emit ;
: \c-   esc[ [char] ? emit [char] 2 emit [char] 5 emit [char] l emit ;

