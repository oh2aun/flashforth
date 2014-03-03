loadhelp
!	x addr --	Store x to addr
!p	addr --		Store addr to p(ointer) register
!p>r	addr --		Compile Only. Push contents of p to return stack and stor addr to p
#	u1 -- u2	Compile Only. Convert 1 digit to formatted numeric string
#>	u1 -- c-addr u	Compile Only. Leave address and count of formatted numeric string
#s	u1 -- u2	Compile Only Convert remaining digits to formatted numeric output
'	-- xt		Parse word and find it in dictionary
'source	-- a-addr	User Variable Current input source
(	--		Skip input on the same line until ) is encountered
*	u1/n1 u2/n2 -- u3/n3	Signed and unsigned 16*16->16 bit multiplikation
+	n1 n2 -- n3	Add n1 to n2
+!	n addr -- 	Add n to cell at addr
, 	x -- 		Append x to the current data section
-	n1 n2 -- n3	Subtract n2 from n1
.	n --		Display n signed according to base
.s	--		Display the stack contents
.st	--		Emit status string for base, current data section, and display the stack contents
/	n1 n2 -- n3	16/16->16 bit division
/string	addr u n -- addr+n u-n	Trim string
0<	n -- flag	Leave true flag if n is less than zero
0=	x -- flag	Leave true flag if x is zero
1	-- 1
1+	n -- n1		Add one to n
1-      n -- n1		Subtract 1 from n
2*	u1 -- u2	Shift u1 left one bit
2+      n -- n1		Add two to n
2-	n -- n1		Subtract 2 from n
2/	n1 -- n2	Shift n1 right one bit.
2@	a-addr -- x1 x2	Fetch two cells
2!	x1 x2 a-addr --	Store two cells
2drop	x1 x2 --	Drop two cells
2dup	x1 x2  -- x1 x2 x1 x2	Duplicate two top cells
:	"name" --	Begin a colon definition
:noname	-- addr		Define headerless forth code
;	--		Compile Only. End a colon definition
;i	--		Compile Only. End a interrupt word
<	n1 n2 -- flag	Leave true flag if n1 is less than n2
<#	--		Compile Only. Begin numeric conversion
<>      x1 x2 -- flag	Leave true flag if x1 and x2 are not equal
=	x1 x2 -- flag	Leave true flag if x1 and x2 are equal
>	n1 n2 -- flag	Leave true flag if n1 is grater than n2
>body	xt -- a-addr	Leave the parameter field address of a created word
>digit	n -- c		Convert n to ascii character value
>in	-- a-addr	User Variable. Holds offset into tib
>number	n1 addr1 u1 -- n2 addr2 u2	Convert string to number
>r	x -- R: -- x	Compile Only. Push x from the parameter stack to the return stack
?abort	flag c-addr u --	Print message and abort if flag is true
?abort?	flag --		If flag is true, emit ? and abort
?negate	n1 n2 -- n3	Negate n1 if n2 is negative
@	addr -- x	Fetch x from addr
@+	addr1 -- addr2 x	Fetch cell from addr1 and increment addr1 by a cell
@p	-- addr		Fetch the p register to the stack
@ex	addr --		Fetch vector from addr and execute.
[	--		Enter interpreter state
[']	"name" --	Compile Only. Compile xt of name as a literal
[char]	"char" --	Compile Only. Compile inline ascii character
[i	--		Compile Only. Enter Fort interrupt context
\	--		Skip rest of line
]	--		Enter compilation state
abort	--		Reset stack pointer and execute quit
abort"	"string" --	Compile Only. Compile inline string and postpone abort?
abs	n -- n1		Leave absolute value of n
accept  c-addr +n -- +n'	Get line from terminal
again	addr --		Compile Only. begin ... again
align	--		Align the current data section dictionary pointer to cell boundary
aligned	addr -- a-addr	Align addr to a cell boundary.
allot	n --		Adjust the current data section dictionary pointer
and	x1 x2 -- x3	Bitwise and of x1 and x2
base	a-addr		User Variable. Numeric conversion base
begin	-- a-addr	Compile Only. Begin loop definition
bin	--		Set base to binary
bl	-- c		Ascii space
c!	c addr --	Store c to addr
c@	addr -- c	Fetch c from addr
c@+	addr1 -- addr2 c	Fetch char from addr1 and increment addr1
c,	c --		Append c to the current data section
cell	-- n		Leave the size of one cell in characters.
cell+	addr1 -- addr2	Add cell size to addr1
cells	x1 -- x2	Convert cells to address units.
char	"char" -- n	Parse a char and leave ascii value on stack
char+	c-addr1 -- c-addr2	Add one to c.addr1
chars	x1 -- x2	Convert characters to address units
cf,	xt --           Compile xt into the flash dictionary.
cfa>nfa	addr1 -- addr2	Convert cfa to nfa
cmove	addr1 addr2 u --	Move u chars from addr1 to addr2
cold	--		Make a cold start. Reset all dictionary pointers.
con	x "name" --	Create a constant in rom as inline code
constant	x "name" --	Create an constant in rom with docreate as runtime
cr	--		Emit CR LF
create	"name" --	Create a word definition and store the current data section pointer.
cse	-- addr		Ram variable holding the current data section value
cwd	--		Clear the WatchDog counter.
decimal	--		Set numeric base to decimal 10.
defer	"name --	Define a deferred execution vector
di	--		Disable interrupts
digit?	c -- n flag	Convert char to a digit according to base
does>	--		Compile Only. Define the runtime action of a created word.
dp	-- addr		Eeprom variable mirrored in ram. Dictionary pointer
drop	x1 --		Drop top of stack
dump	addr u --	Display a memory dump 
dup	x -- x x	Duplicate top of stack
ei	--		Enable interrupts
end	task-addr --	Remove a task from the task list.
eeprom	--		Set data section context to eeprom
else	addr1 -- addr2	Compile Only. if ... else ... then
emit	c --		Emit c to the serial port FIFO. FIFO is 46 chars. Executes pause.
evaluate	c-addr n --	Evaluate ram buffer
execute	addr --		Execute word at addr
exit	--		Exit from a word.
false	-- 0
flash	--		Set data section context to flash
fill	c-addr u c --	Fill u bytes with c staring at c-addr
find	c-addr -- c-addr 0/1/-1		Find a word in dictionary. Leave 1 if immediate, -1 if normal, 0 if not found
for	u --		Compile Only. Loop u times. for ... next
forget	"name --	Forget name
here	-- addr		Leave the current data section dictionary pointer
hex	--		Set numeric base to hexadecimal
hold	c --		Compile Only. Append char to formatted numeric string
hp	-- a-addr	User Variable. Hold pointer for formatted numeric output
i]	--		Compile Only. Exit Fort interrupt context
i,	x --		Append x to the flash data section.
ic,	c --		Append c to the flash data section.
if	-- a-addr	Compile Only. if ... else ... then
iflush  --		Flush the flash write buffer
immed?  addr -- n	Leave a nonzero value if addr contains a immediate flag
immediate	--	Mark latest definition as immediate
in?	nfa -- flag	Leave true flag if nfa has inline bit set
inline	"name" --	Inline the following word.
inlined	--		Mark the latest compiled word as inlined.
interpret	c-addr u --	Interpret the ram buffer
invert	x1 -- x2 )	Ones complement of x1
irq	-- a-addr	Ram value. Interrupt vector. Cleared at warm start
is	x "name" --	Set the value a deferred word
key	-- c		Get a character from the serial port FIFO. Execute pause until a character is available
key?	-- flag		Leave true if character is waiting in the serial port FIFO
khz	-- u		Leave the cpu clock in KHz
latest  -- a-addr	Variable holding the address of the latest defined word
leave	--		Compile only. Leave a for/next loop when next is encountered. Sets top of return stack to zero
literal	x --		Compile a literal into the dictionary
lshift	x1 u -- x2	Shift x1 u bits to the left
m+	d1 n -- d2	Add double number d1 to n
marker	"name" --	Mark a dictionary state
max	n1 n2 -- n3	Leave max of n1 and n2
mclr	mask caddr --	AND the contents of ram-caddr with the complement of mask
min	n1 n2 -- n3	Leave min of n1 and n2
ms	+n --		Pause for +n milliseconds
mset	mask caddr --	OR the contents of ram-caddr with mask.
mtst	mask caddr -- x	AND the contents of ram-caddr with mask
n=	caddr nfa u -- flag	Compare strings in ram(c-addr) and flash(nfa) flag is true if strings match. u<32.
negate	n -- -n		negate n
next	bra-addr bc-addr --	Compile Only. for ... next
nfa>lfa	addr1 -- addr2	Convert nfa to lfa
nip	x1 x2 -- x2	Remove x1 from the stack
number?	caddr -- n/caddr flag	Convert string to number, # is decimal prefix, $ is hexadecimal prefix, % is binary prefix
operator	-- addr Leave the address of the operator task
or	x1 x2 -- x3	Or bitwise x1 with x2
over	x1 x2 -- x1 x2 x1	Copy x1 to top of stack
p+	--		Increment P by one
p2+     --              Add 2 to P
p++	n --		Add n to P
p!	x --		Store x to the location pointed by P
pc!	c --		Store c to the location pointed by P
p@	-- x		Fetch the cell pointed by P
pc@	-- c		Fetch the char pointed by P
pad	-- a-addr	: pad ram here $20 + ;
pause	--		Switch task
place	addr1 u addr2 --	Place string from addr1 to addr2 as a counted string
postpone	"name" --	Compile Only. Postpone action of immediate word
prompt	-- a-addr	Deferred execution vector for the info displayed by quit. ' .st is defer
quit	--		Interpret from keyboard
r>      -- x R: x --	Compile Only. Pop x from the return stack to the parameter stack
r>p	--   R: x --	Compile Only. Pop from return stack to p register
r@	-- x R: x -- x	Compile Only. Copy x from the return stack to the parameter stack
ram	--		Set data section context to ram
rcnt	-- a-addr	User Variable. Number of saved return stack cells
rdrop	--   R: x --	Compile Only. Remove top element from return stack
repeat	addr2 addr1 --	Compile Only. begin ... while ... repeat
rhere	-- addr		Start of free ram
rot	x1 x2 x3 -- x2 x3 x1	Rotate three top stack items
rsave	-- a-addr	User variable. Return stack save area
rshift	x1 u -- x2	Shift x1 u bits to the right
run	task-addr --	Link the task to the task list. The task starts running immediately.
s0	-- a-addr	Variable for start of parameter stack
scan	c-addr u c -- c-addr' u'	Scan string until c is found. c-addr must point to ram. u<255
sign	n --		Append minus sign to formatted numeric output
sign?	addr1 n1 -- addr2 n2 flag	Get optional minus sign
single	--		End all tasks except the operator task.
skip	c-addr u c -- c-addr' u' Skip string until c not encountered. c-addr must point to ram. u<255
sp@	-- addr		Leave parameter stack pointer
sp!	addr --		Set the parameter stack pointer to addr
s"	"text" --	Compile Only. Compile string into flash
."	"text" --	Compile Only. Compile string to print into flash
source	-- c-addr n	Current input buffer
space	--		Emit one space character
spaces	n --		Emit n space characters
ssave	-- a-addr	User Variable. Saved return stack pointer
state	-- a-addr	User Variable. Compilation state
swap	x1 x2 -- x2 x1	Swap two top stack items
task:	tibsize stacksize rstacksize addsize --		Define a task           
tinit	taskloop-addr task-addr --	Initialise the user area and link it to a task loop
then	addr --		Compile Only. if ... else ... then
tib	-- addr		User variable. Terminal input buffer
ti#	-- n		Size of terminal input buffer. Task constant
ticks	-- u		System ticks. One ms resolution
to	x "name" --	Store x into value "name".
true	-- -1
tuck	x1 x2 -- x2 x1 x2	Insert x2 below x1 in the stack
turnkey	-- a-addr	Eeprom value mirrored in ram. Vector for user startup word
type	c-addr u --	Type line to terminal. u < $100
u*/mod	u1 u2 u3 -- u4(remainder) u5(quotient)	Unsigned u1*u2/u3 with 32 bit intermediate result
u.	u --		Display u unsigned according to numeric base
u.r	u +n --		Display u in field of width n. 0<n<256
u/	u1 u2 -- u3	Unsigned 16/16->16 bit division
u/mod	u1 u2 -- u3(remainder) u4(quotient)	Unsigned 16/16->16 bit division
u<	u1 u2 -- flag	Leave true flag if u1 is less than u2
u>	u1 u2 -- flag	Leave true flag if u1 is greater than u2
ulink	-- a-addr	USER. Link to next task
um*	u1 u2 -- ud	Unsigned 16x16 -> 32 bit multiply
um/mod	ud u1 -- u2(remainder) u3(quotient)	unsigned 32/16 -> 16 bit division
umax	u1 u2 -- u	Leave the unsigned larger of u1 and u2.
umin	u1 u2 -- u	Leave the unsigned smaller of u1 and u2.
until	flag --		Compile only. begin..until
up	-- a-addr	Variable holding the user pointer
user	n "name" --	Define a user variable at offset n
value	x "name" --	Define a value
variable	"name" --	Create a variable in the current data section
warm	--		Make a warm start
while	addr1 -- addr2 addr1	Compile Only. begin ... while ... repeat
within	x xl xh -- flag	Leave true if  xl <= x < xh
word	c -- c-addr	Copy a word delimited by c to c-addr
words	--		List words
xor	x1 x2 -- x3	Xor bitwise x1 with x2.
btfsc,	f b a --
btfss,	f b a --
bcf,	f b a --
bsf,	f b a --
andlw,	k --
movf,	f d a --
a,	-- 0
w,	-- 0
call,	addr --
goto,	addr --
rcall,	rel-addr --
bra,	rel-addr --
z,	-- cc
nz,	-- cc
not,	cc -- not-cc
if,	cc -- here
else,	back-addr -- here
then,	back-addr --
begin,	-- here
again,	back-addr --
until,	back-addr cc --
|
