CREATE OR REPLACE
TRIGGER t98tcap_protestos BEFORE INSERT
   ON tcap_protestos FOR EACH ROW
BEGIN
:new.CHP_MULTA:= nvl(:new.CHP_MULTA,0);
:new.CHP_VALCOB:=nvl(:new.CHP_VALCOB,0);
END;

/