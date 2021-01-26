-bridge
marker -bridge
: bridge ( -- )
  u1-
  begin
    pause
    rxu? if rxu tx1 then
    rx1? if rx1 txu then
  again
;

