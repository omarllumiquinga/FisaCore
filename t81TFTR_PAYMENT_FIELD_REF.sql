CREATE OR REPLACE TRIGGER T81PAYMENT_FIELD_REF
BEFORE INSERT ON TFTR_PAYMENT_FIELD_REF    
 REFERENCING NEW AS NEW OLD AS OLD  
  FOR EACH ROW
DECLARE
horai  number(5);
horaf  number(5);
horaid number(5);
horafd number(5);
BEGIN
  if inserting then
  if :new.field_id = 'name' then
     :new.value := substr(:new.value,1,50);
  end if;
  end if;
END ;
