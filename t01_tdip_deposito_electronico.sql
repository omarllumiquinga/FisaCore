CREATE OR REPLACE
TRIGGER T99_tdip_deposito_electronico
AFTER INSERT ON tdip_deposito_electronico
REFERENCING NEW AS NEW OLD AS OLD FOR EACH ROW
DECLARE
  de varchar2(50) := 'BDIOperaciones@BDI.COM.DO';
  para varchar2(50) := 'fcollado@blh.com.do';
  mensaje varchar2(4000); --:= 'Cuenta Nostro esta por Sobregirarse';
  asunto varchar2(1000):= 'INGRESO UN DEPOSITO DE CHEQUE ELECTRONICO A LA CTA:'||:NEW.RPC_CUENTA_CREDITO||' POR UN MONTO DE:'||:NEW.RPC_VALORCHEQUE||' TIPO:'||:NEW.RPC_TIPOCHEQUE;
  CURSOR P IS
  SELECT emp_emaili mail
   FROM  tdom_alertusertra a, tgen_usuario b,tgen_empleado c
   WHERE a.ale_codusr = b.usr_codigo
     AND b.usr_codemp = c.emp_codigo
     AND a.ale_tabtipoaut = 513
    AND a.ale_tipoaut = 20--ALERTA DE INGRESO DE DEPOSITO CHEQUE ELECTRONICO
     AND a.ale_hasta IS  NULL;
BEGIN
IF INSERTING THEN
       FOR X IN P LOOP
       para:= X.MAIL;
       mensaje:= 'INGRESO UN DEPOSITO DE CHEQUE ELECTRONICO A LA CTA:'||:NEW.RPC_CUENTA_CREDITO||' '||:NEW.RPC_CLIENTE||' POR UN MONTO DE:'||:NEW.RPC_VALORCHEQUE||' NROCHQ:'||:NEW.RPC_NROCHEQUE ||' TIPO:'||:NEW.RPC_TIPOCHEQUE;
       PKG_GENMAIL.ENVIA_MAIL(de,
                           para,
                           mensaje    ,
                           asunto);
       END LOOP;     
END IF;
EXCEPTION
  WHEN OTHERS THEN
   NULL;
END;

/