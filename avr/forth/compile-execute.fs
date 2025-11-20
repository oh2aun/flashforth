\ Compile the code between [[ ]] and execute it
\ without extending the dictionary
\ [[ 10 for r@ . next ]]

: [[ flash here ram ] ;
: ]] $9508 i, iflush dup flash dp ! ram postpone [ execute ; immediate 
