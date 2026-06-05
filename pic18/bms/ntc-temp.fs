\ Generate table with gforth
\ e^((3950/(273.15+00))-(3950/(273.15+25)))*10000
\ gentemp ( temp -- resistance ) parameters are integer
\ : gen-r s>f 273.15e0 f+ 3950e0 fswap f/ 13.248365e0 f- fexp 1e4 f* f>s ;
\ : gen-128 ( r -- adval ) gen-r dup >r 128 * r> 10000 + / ;
\ : values ( stop start -- ) do cr i 2 .r ."  c,  \ " i gen-128 . -1 +loop ;
fl+ single
-ntc-table
marker -ntc-table
decimal
flash create ntc-table-7bit
\ temperature index == 10-bit-ad-value >> 3
60 c,  \ 25 start index value
59 c,  \ 26
57 c,  \ 27 
56 c,  \ 28 
55 c,  \ 29 
54 c,  \ 30 
53 c,  \ 31 
51 c,  \ 32 
50 c,  \ 33 
49 c,  \ 34 
48 c,  \ 35 
47 c,  \ 36 
46 c,  \ 37 
45 c,  \ 38 
44 c,  \ 39 
43 c,  \ 40 
42 c,  \ 41 
42 c,  \ 42 
41 c,  \ 43 
40 c,  \ 44 
39 c,  \ 45 
38 c,  \ 46 
37 c,  \ 47 
37 c,  \ 48 
36 c,  \ 49 
35 c,  \ 50 
34 c,  \ 51 
33 c,  \ 52 
33 c,  \ 53 
32 c,  \ 54 
31 c,  \ 55 
30 c,  \ 56 
30 c,  \ 57 
29 c,  \ 58 
28 c,  \ 59 
27 c,  \ 60 
27 c,  \ 61 
26 c,  \ 62 
25 c,  \ 63 
25 c,  \ 64
24 c,  \ 65 
23 c,  \ 66 
22 c,  \ 67 
22 c,  \ 68 
21 c,  \ 69 
20 c,  \ 70 
20 c,  \ 71 
19 c,  \ 72 
18 c,  \ 73 
18 c,  \ 74 
17 c,  \ 75 
16 c,  \ 76 
16 c,  \ 77 
15 c,  \ 78 
14 c,  \ 79 
13 c,  \ 80 
13 c,  \ 81 
12 c,  \ 82 
11 c,  \ 83 
11 c,  \ 84 
10 c,  \ 85 
 9 c,  \ 86 
 8 c,  \ 87 
 8 c,  \ 88 
 7 c,  \ 89 
 6 c,  \ 90 
 6 c,  \ 91 
 5 c,  \ 92 
 4 c,  \ 93 
 3 c,  \ 94 
 2 c,  \ 95
 2 c,  \ 96 
 1 c,  \ 97 
 0 c,  \ 98 
-1 c,  \ 99 
-2 c,  \ 100 
-3 c,  \ 101 
-3 c,  \ 102 
-4 c,  \ 103 
-5 c,  \ 104 
-6 c,  \ 105 
-7 c,  \ 106 
-8 c,  \ 107 
-9 c,  \ 108 
-10 c,  \ 109 
-11 c,  \ 110 
-13 c,  \ 111 
-14 c,  \ 112 
-15 c,  \ 113 
-17 c,  \ 114 
-18 c,  \ 115
-20 c,  \ 116 
ram

: >n ( c -- n ) dup 128 and if $ff00 + then ;
: ad>temp ( ad-value -- temperature )
  3 rshift 25 - dup 0 92 within 
  if   ntc-table-7bit + c@ >n
  else drop 99 
  then ;



