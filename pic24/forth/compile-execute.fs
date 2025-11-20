\ Compile the code between [[ ]] and execute it
\ without extending the dictionary
\ [[ 10 for r@ . next ]]

: [[ flash here ram ] ;
: ]] return, iflush dup flash dp ! ram postpone [ execute ; immediate 
