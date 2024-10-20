: synonym ( "new-name" "old-name" -- )
  flash create immediate ' , ram
  does> @ state 0= over c>n immed? or 
        if execute else cf, then
;

