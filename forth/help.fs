\ *******************************************************************
\                                                                   *
\    Filename:      help.txt                                        *
\    Date:          03.03.2014                                      *
\    FF Version:    5.0                                             *
\    Copyright:     Mikael Nordman                                  *
\    Author:        Mikael Nordman                                  *
\ *******************************************************************
\ FlashForth is licensed according to the GNU General Public License*
\ *******************************************************************
-help
marker -help
ram hex

$1b constant esc
$09 constant tab
$0d constant ret
$0a constant nl

flash hi $32ff - constant ahelp \ Start of help text area
ram

: h= ( caddr caddr1 u -- flag )
  swap !p>r
  for
    c@+ pc@ p+ -
    if drop false rdrop r>p exit then
  next
  r>p drop true
;


: .help ( addr -- )
  cr
  begin
    c@+ dup emit ret =
  until
  cr drop
;
: help ( "name" -- )
  bl word         \ addr
  dup c@ 0= if words abort then
  ahelp !p>r
  begin
    busy pause idle
    @p over c@+ h= if @p .help r>p drop exit then
    begin 
      pc@ ret = pc@ nl = or 
      p+
      pc@ ret <> pc@ nl <> and 
      and
    until
    pc@ [char] | =
  until
  r>p drop
;

: loadhelp ( --     store help info )
  ahelp !p>r        \ Help info stored here
  begin
    key
    dup emit
    dup pc! p+ 
    [char] | =
  until
  r>p
;

