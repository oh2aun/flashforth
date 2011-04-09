-stest
marker -stest
ram
: stest
  begin
    -#499 !p>r
    #499 for
      1 ms
      0 @p spos
      1 @p spos
      2 @p spos
      3 @p spos
      2 p++
    next
    #499 !p
    #499 for
      1 ms
      0 @p spos
      1 @p spos
      2 @p spos
      3 @p spos
      -2 p++ 
    next
    r>p
  key?
  until
  key drop
;

sinit
stest
