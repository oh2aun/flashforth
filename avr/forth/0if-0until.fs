\ 0if and 0until for Atmega

: 0if ( -- addr) $f009 i, ['] if #8 + execute ; immediate
: 0until ( addr -- ) $f009 i, postpone again ; immediate
: 1until ( addr -- ) $f409 i, postpone again ; immediate




