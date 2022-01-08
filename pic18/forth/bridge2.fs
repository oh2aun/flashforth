-bridge
marker -bridge
: bridge ( -- )
  u1-
  begin
    pause
    rx1? if rx1 txu then
  again
;

