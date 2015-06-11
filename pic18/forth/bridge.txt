: bridge ( -- )
  u1-
  begin
    rxu? if rxu tx1 then
    rx1? if rx1 txu then
  again
;

