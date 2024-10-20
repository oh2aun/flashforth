: 2value  ( d "name" -- ) create swap , , does> 2@ ;
: 2to     ( d "name" -- ) ' >body state if postpone 2! else 2! then ;


