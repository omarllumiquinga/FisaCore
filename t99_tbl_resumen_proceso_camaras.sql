CREATE OR REPLACE
TRIGGER t99_tbl_res_proc_camaras
AFTER INSERT OR DELETE OR UPDATE OF
  RPC_MONTO,
  RPC_NUMERO_CUENTA_DESTINO,
  RPC_NUMERO_CHEQUE
  ON tbl_resumen_proceso_camaras
 REFERENCING NEW AS NEW OLD AS OLD
 FOR EACH ROW


DECLARE
BEGIN
 if updating then 
	 IF NVL(TO_CHAR(:NEW.RPC_MONTO),'@@') <> NVL(TO_CHAR(:OLD.RPC_MONTO),'@@') then
	     update tdip_deposito_electronico
	       set  RPC_VALORCHEQUE = :NEW.RPC_MONTO
	      where RPC_FECHAPROCESO  = :old.rpc_fecha_proceso
	        and RPC_ID = :old.rpc_id                
	        and RPC_BATCH_ID =  :old.rpc_batch_id;           
	  end if;
	 IF NVL(TO_CHAR(:NEW.RPC_NUMERO_CUENTA_DESTINO),'@@') <> NVL(TO_CHAR(:OLD.RPC_NUMERO_CUENTA_DESTINO),'@@') then
     update tdip_deposito_electronico
       set  RPC_CUENTA_CREDITO = :NEW.RPC_NUMERO_CUENTA_DESTINO
      where RPC_FECHAPROCESO  = :old.rpc_fecha_proceso
        and RPC_ID = :old.rpc_id                
        and RPC_BATCH_ID =  :old.rpc_batch_id;           
	  end if;
	 IF NVL(TO_CHAR(:NEW.RPC_NUMERO_CHEQUE),'@@') <> NVL(TO_CHAR(:OLD.RPC_NUMERO_CHEQUE),'@@') then
     update tdip_deposito_electronico
       set  RPC_NROCHEQUE  = :NEW.RPC_NUMERO_CHEQUE
      where RPC_FECHAPROCESO  = :old.rpc_fecha_proceso
        and RPC_ID = :old.rpc_id                
        and RPC_BATCH_ID =  :old.rpc_batch_id;           
	  end if;	  
 end if;
END;

/