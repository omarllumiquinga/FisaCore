CREATE OR REPLACE
PACKAGE BODY pkg_bdi_pago_tc_electronico IS
PROCEDURE PAGO_TARJETA_TC (P_CUENTA   IN NUMBER,   --ahorros
                           P_TARJETA  IN VARCHAR2, --NUMBER,   --tarjeta
						   P_MONEDAPAGAR IN NUMBER,--moneda de TC que voy a pagar                           
                           P_TIPOPAGO IN VARCHAR2, --N
                           P_VALOR    IN  NUMBER,--valor a debitar - pagar
                           P_CANAL IN NUMBER,
                           P_MENSAJE   OUT VARCHAR2, --mensaje a transmitir
                           P_CODUSRP OUT NUMBER,--codigo que ejecuto la trans
                           P_NUMTRAP OUT NUMBER,--numero de transaccion unica
                           P_ServRegistroOK  OUT VARCHAR2,--o no exiete error 1 existe un error
                           P_ServMsgError OUT VARCHAR2) IS --si es 1 se despliega el mensaje de error
  P_sucursal   number;
  P_oficina    number;
  P_depto      number;
  P_operador   number;
  P_terminal    varchar2(20);
  fecha DATE;
  p_horarionormal VARCHAR2(8):= NULL;
  p_secuencia number;
  p_diferido varchar2(1);
  p_moneda number;
  p_rubro  number;
  P_REF varchar2(20):='44699999';
  P_REFD NUMBER;
  P_TIPOTRA VARCHAR2(1);
  P_tiporub VARCHAR2(1);
  p_autorizador number;
  P_MODORG NUMBER;
  P_PROORG NUMBER;
  P_TIPORG NUMBER;
  P_MONORG NUMBER;
  P_MODDES NUMBER;
  P_PRODES NUMBER:= NULL;
  P_TIPDES NUMBER:= NULL;
  P_MONDES NUMBER;
  P_NUMPAGO NUMBER;
  p_parametro3 VARCHAR2(30);
  p_parametro4 VARCHAR2(30);
  p_clientetar NUMBER;
  P_CLIENTE NUMBER;
  p_codigoserv NUMBER;
  P_PRODUCTO NUMBER:=1;
  P_TIPO NUMBER:=1;
  P_TRANSACCION NUMBER:=4;
  P_USRSECUENCIA NUMBER;
  P_SEC NUMBER;
  v_tc varchar2(1);
  modifica	varchar2(2);
  saldo     number(18,3);
  p_valoraux number;
--MULTIMONEDA
   vln_totalxmon      tpre_prestamos.pre_monto%TYPE;
   vln_totmoncta      tpre_prestamos.pre_monto%TYPE;
   vln_monto      tpre_prestamos.pre_monto%TYPE;
   vln_montotra      tpre_prestamos.pre_monto%TYPE;   
   vln_cotizcompra    tgen_monfecha.mof_promedio%TYPE;
   vln_cotizventa     tgen_monfecha.mof_promedio%TYPE;
   vlc_tipotra        VARCHAR2(1);
  vlc_DISPONIBLE  NUMBER;
     W_RELACION VARCHAR2(1);
BEGIN
--PARAMETROS GENERALES
  -- Insertar sesion
P_ServRegistroOK :='0';  
   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_LANGUAGE=''ENGLISH''';
   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_DATE_FORMAT=''YYYY/MM/DD''';

  BEGIN
  SELECT IVR_VALOR
    INTO p_operador
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'OPERADOR';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'OPERADOR  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_sucursal
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'SUCURSAL';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'SUCURSAL  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_Oficina
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'OFICINA';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'OFICINA  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_Depto
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'DEPARTAMENTO';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'DEPARTAMENTO  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO P_terminal
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'TERMINAL';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'DEPARTAMENTO  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_horarionormal
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'HORARIO_NORMAL';
  IF p_horarionormal IS NULL THEN
     raise_application_error(-20006,'HORARIO  IVR  DEFINIDO INCORRECTAMENTE');
  END IF;
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20006,'DEPARTAMENTO  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  begin
    insert into tgen_sesion
      (ses_sesion, ses_sucursal, ses_oficina, ses_operador)
    values
      (userenv('sessionid'), p_sucursal, p_oficina, p_operador);
  exception when dup_val_on_index then null;
            when others then
        raise_application_error(-20898,'SESION NO GENERADA '||sqlerrm);
  end;
        --<<hd10435
         --llamada a la iniciacion de variable
  pkg_gen_bancaelectronica.fun_gen_InicializaParWEB('S',null,p_Canal, P_CUENTA, P_CUENTA);--p_tarjeta);
         -->>hd10435

--FIN PARAMETROS GENERALES
  IF TO_NUMBER(TO_CHAR(fecha,'HH24MMSS')) > TO_NUMBER(p_horarionormal) THEN
     p_diferido:='S';
  ELSE
     p_diferido:='N';
  END IF; -- Fin de fechas
  val_cuenta(p_cuenta,4,'D',99);
  --v_tc:=PKG_CMS_INTERFAZCREDITCARD.VALIDA_TARJETA(p_tarjeta);
   --SAVE_LOG(' P_FECHA:'|| P_FECHA);
   W_RELACION :=NULL;   
  BEGIN
  SELECT VIS_CODCLI,VIS_MONEDA,VIS_MOD,VIS_PRO,VIS_TIP,CLC_TITULAR
    INTO p_cliente,p_moneda,P_MODORG,P_PROORG,P_TIPORG,W_RELACION
    FROM TCAP_VISTA,TCLI_CLICTA
   WHERE VIS_NUMCUE = P_CUENTA
     AND VIS_NUMCUE = CLC_CUE
     AND VIS_CODCLI = CLC_CODCLI;
  EXCEPTION
   WHEN OTHERS THEN
   P_ServRegistroOK :='1';--o no exiete error 1 existe un error
   P_ServMsgError := 'ERROR EN CUENTA  INGRESADA:';
   --raise_application_error(-20002,'ERROR EN CUENTA  INGRESADA:'||SQLERRM);
  END;

 BEGIN
 SELECT CLIENT
   INTO p_clientetar
  FROM tcreditextract
 WHERE CREDIT=  p_tarjeta;
  EXCEPTION
   WHEN OTHERS THEN
     NULL;
       --raise_application_error(-20002,'ERROR EN TARJETA  INGRESADA:'||SQLERRM);
  END;

--IF P_cliente <> P_clienteTAR THEN
--raise_application_error(-20005,'10-Error de Datos');
--END IF;
--PARA QUE PUEDAN PAGAR LOS COTITULARES CON RELACION O
  IF P_cliente <> P_clienteTAR THEN
     FOR X IN (SELECT CLC_CODCLI
                 FROM TCLI_CLICTA
                 WHERE CLC_CUE = P_CUENTA
                 ORDER BY CLC_ORDEN) LOOP
        dbms_output.put_line('Pas1:'||W_RELACION);
      IF P_clienteTAR = X.CLC_CODCLI THEN
        dbms_output.put_line(W_RELACION);
         IF W_RELACION = 'O' THEN
            P_cliente:=P_clienteTAR;
            EXIT;
         END IF;
      END IF;
     END LOOP;
  END IF;

  IF P_cliente <> P_clienteTAR THEN
   P_ServRegistroOK :='1';--o no exiete error 1 existe un error
   P_ServMsgError := 'Error de Datos. Cliente No Autorizado para realizar pagos con esta cuenta.'||P_cliente||' - '||P_clienteTAR||' - '||W_RELACION;
   --raise_application_error(-20005,'Error de Datos. Cliente no permitido para realizar pagos con esta cuenta.');
  END IF;
--Para multimoneda  
IF P_ServRegistroOK ='0' THEN
IF p_moneda <> P_MONEDAPAGAR THEN
        vln_totalxmon   := NULL;
        vln_totmoncta   := NULL;
        vln_cotizcompra := NULL;
        vln_cotizventa  := NULL;
        vlc_tipotra     := NULL;
   		vln_monto       := NULL;                      
		vln_montotra       := NULL;                         		
           
        IF  P_MONEDAPAGAR = 0 THEN
          vln_totmoncta := P_VALOR;--PNI_MONTO;
          vlc_tipotra   := 'C'; -- Compra

        ELSE
          vln_totalxmon := P_VALOR;--PNI_MONTO;
          vlc_tipotra   := 'D'; -- Venta

        END IF;
  
        IF vlc_tipotra = 'C' THEN
          pkg_cambios.pcotdivisas(p_sucursal,
                                  vlc_tipotra,
                                  0,
                                  P_MONEDAPAGAR,
                                  p_moneda,
                                  p_cliente,
                                  vln_cotizcompra,
                                  vln_cotizventa,
                                  vln_totalxmon,
                                  vln_totmoncta);
                   vln_monto:= vln_totalxmon;
                   vln_montotra:= vln_totmoncta;                                                                    
        ELSIF vlc_tipotra = 'D' THEN
          pkg_cambios.pcotdivisas(p_sucursal,
                                  vlc_tipotra,
                                  0,
                                  p_moneda,
                                  P_MONEDAPAGAR,                                
                                  p_cliente,
                                  vln_cotizcompra,
                                  vln_cotizventa,
                                  vln_totalxmon,
                                  vln_totmoncta);                              
                   vln_monto:= vln_totmoncta;                                                                    
                   vln_montotra:= vln_totalxmon;                                                                                       
        END IF;                                                                                                     
        
	  P_MONORG:= p_moneda;
	  P_MONDES:= P_MONEDAPAGAR;
ELSE
  P_MONORG:= p_moneda;
  P_MONDES:= P_MONEDAPAGAR;
  vln_monto:= p_valor;                                                                    
  vln_montotra:= p_valor;                                                                                       
  vln_cotizcompra:=1;
  vln_cotizventa:=1;
END IF;
DBMS_OUTPUT.PUT_LINE('vln_totalxmon:'||vln_totalxmon);
DBMS_OUTPUT.PUT_LINE('vln_totmoncta:'||vln_totmoncta);
DBMS_OUTPUT.PUT_LINE('vln_cotizcompra:'||vln_cotizcompra);
DBMS_OUTPUT.PUT_LINE('vln_cotizventa:'||vln_cotizventa);

--fin de multimoneda

  P_secuencia := UPD_SECTRAN(P_operador);
  P_MODDES:= 3;--CAJA
  P_TRANSACCION:=4;
  P_PRODUCTO :=1;
  P_TIPO:=1;
  P_ServRegistroOK :='0';
  P_ServMsgError:=NULL;

  IF P_MONEDAPAGAR = 0 THEN
     p_codigoserv := 1;--DE ACUERDO A LO DEFINIDO EN BDI
     p_rubro := 6;
  ELSE
    p_codigoserv:= 4;--DE ACUERDO A LO DEFINIDO EN BDI
    p_rubro := 46;
  END IF;
  dbms_output.put_line('paso 1');
  --tipos
  SELECT COD_TIPOTRA,RUB_EFECTIVO
    INTO P_TIPOTRA,P_TIPORUB
    FROM TGEN_CODTRANS,TGEN_TRANRUBRO
    WHERE COD_MOD = RUB_MOD
      AND COD_TRA =RUB_TRA
      AND COD_MOD = P_MODDES
      AND COD_TRA = P_TRANSACCION
      AND RUB_RUBRO = P_RUBRO;
  dbms_output.put_line('paso 1.2 P_VALOR:'||P_VALOR);

  numero_ref(p_sucursal,p_oficina,
           p_operador,null,10,p_refd,modifica);

    saldo:=sal_cuenta(p_cuenta,1,1);
    vlc_disponible:=saldo;
     if saldo < nvl(vln_monto,0) then
            raise_application_error(-20007, 'FONDOS INSUFICIENTES');
     end if;

  --
  p_valoraux := p_valor ;      
  --aqui llena la 3-4
  INSERTA_TCAP_TRAMONAUX(p_operador,p_secuencia,p_sucursal,p_oficina ,p_diferido,p_modDES ,p_producto ,p_tipo,P_MONEDAPAGAR ,
  p_transaccion,
                         p_rubro,p_depto,P_REF,P_TIPOTRA,vln_montotra,P_tiporub,P_CUENTA,p_autorizador,p_terminal);
  INS_TCAJ_FORPAGO_MULTI(P_CUENTA,P_MODORG,P_PROORG,P_TIPORG,P_MONORG,
                   P_DEPTO, P_SUCURSAL,P_OFICINA,P_CLIENTE,vln_monto,
                   P_MODDES,P_PRODES,P_TIPDES,P_MONDES,P_OPERADOR,vln_cotizcompra,vln_cotizventa,vlc_disponible,P_NUMPAGO);--INSERTA EN TCAJ_FORPAGO
  p_usrsecuencia := P_secuencia;
  P_sec   := ver_sec(f_fechatrabajo,p_operador,p_usrsecuencia)-1;
  realiza_debitos(P_CUENTA,P_SUCURSAL,P_OFICINA,P_DEPTO,P_REFD,P_DIFERIDO,P_OPERADOR,p_usrsecuencia,P_NUMPAGO,P_SEC); --REALIZA EL DEBITO DE LA CUENTA
  inserta_tcap_tramon(P_CUENTA,P_OPERADOR,P_SECUENCIA,P_TRANSACCION,P_DIFERIDO,P_VALOR,P_CLIENTE);--INSERTA EN TCAP_TRAMON
  INSERTA_TCAP_PAGOSERVICIO(p_tarjeta,p_tipopago,p_valor,p_codigoserv,
                            p_sucursal,p_oficina,p_operador,p_secuencia,
                            p_diferido,P_DEPTO,P_TERMINAL,P_SEC,p_parametro3,p_parametro4);--INSERT EN TCAP_PAGOSERVICIOS
  VALIDA_TRANSACCION(P_OPERADOR,p_secuencia,p_mensaje,P_ServRegistroOK,P_ServMsgError,p_canal,P_MONEDAPAGAR);
  P_CODUSRP := P_OPERADOR;
  P_NUMTRAP := p_secuencia;
END IF;
  IF nvl(P_ServRegistroOk,'X') != '0' then
    ROLLBACK;
   	 raise_application_error(-20901,'Error Interface Externa.... : '||P_ServMsgError);
  ELSE
     --COMMIT;
     NULL;
  END IF;


EXCEPTION
WHEN OTHERS THEN
   ROLLBACK;
   P_ServRegistroOK :='1';--o no exiete error 1 existe un error
   P_ServMsgError := 'Err:'||sqlerrm;   
 --raise_application_error(-20901,'ERROR EN PAGO DE TARJETA '||sqlerrm);
END;
/*----------------------------PAGOS MASIVOS DEB CTA---------------------------------*/
PROCEDURE PAGO_TARJETA_TC_MAS (P_CUENTA   IN NUMBER,   --ahorros
                           P_TARJETA  IN VARCHAR2, --NUMBER,   --tarjeta
						   P_MONEDAPAGAR IN NUMBER,--moneda de TC que voy a pagar                           
                           P_TIPOPAGO IN VARCHAR2, --N
                           P_VALOR    IN  NUMBER,--valor a debitar - pagar
                           P_CANAL IN NUMBER,  
                           PI_OPERADOR IN NUMBER,--operador que envia los pagos
                           PI_SUCURSAL IN NUMBER,
                           PI_OFICINA IN NUMBER,
                           PI_DEPTO IN NUMBER,
                           PI_TERMINAL IN VARCHAR2,                           
                           P_MENSAJE   OUT VARCHAR2, --mensaje a transmitir
                           P_CODUSRP OUT NUMBER,--codigo que ejecuto la trans
                           P_NUMTRAP OUT NUMBER,--numero de transaccion unica
                           P_ServRegistroOK  OUT VARCHAR2,--o no exiete error 1 existe un error
                           P_ServMsgError OUT VARCHAR2) IS --si es 1 se despliega el mensaje de error
  P_sucursal   number;
  P_oficina    number;
  P_depto      number;
  P_operador   number;
  P_terminal    varchar2(20);
  fecha DATE;
  p_horarionormal VARCHAR2(8):= NULL;
  p_secuencia number;
  p_diferido varchar2(1);
  p_moneda number;
  p_rubro  number;
  P_REF varchar2(20):='44699999';
  P_REFD NUMBER;
  P_TIPOTRA VARCHAR2(1);
  P_tiporub VARCHAR2(1);
  p_autorizador number;
  P_MODORG NUMBER;
  P_PROORG NUMBER;
  P_TIPORG NUMBER;
  P_MONORG NUMBER;
  P_MODDES NUMBER;
  P_PRODES NUMBER:= NULL;
  P_TIPDES NUMBER:= NULL;
  P_MONDES NUMBER;
  P_NUMPAGO NUMBER;
  p_parametro3 VARCHAR2(30);
  p_parametro4 VARCHAR2(30);
  p_clientetar NUMBER;
  P_CLIENTE NUMBER;
  p_codigoserv NUMBER;
  P_PRODUCTO NUMBER:=1;
  P_TIPO NUMBER:=1;
  P_TRANSACCION NUMBER:=4;
  P_USRSECUENCIA NUMBER;
  P_SEC NUMBER;
  v_tc varchar2(1);
  modifica	varchar2(2);
  saldo     number(18,3);
  p_valoraux number;
--MULTIMONEDA
   vln_totalxmon      tpre_prestamos.pre_monto%TYPE;
   vln_totmoncta      tpre_prestamos.pre_monto%TYPE;
   vln_monto      tpre_prestamos.pre_monto%TYPE;
   vln_montotra      tpre_prestamos.pre_monto%TYPE;   
   vln_cotizcompra    tgen_monfecha.mof_promedio%TYPE;
   vln_cotizventa     tgen_monfecha.mof_promedio%TYPE;
   vlc_tipotra        VARCHAR2(1);
  vlc_DISPONIBLE  NUMBER;
     W_RELACION VARCHAR2(1);
BEGIN
--PARAMETROS GENERALES
  -- Insertar sesion
P_ServRegistroOK :='0';  
   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_LANGUAGE=''ENGLISH''';
   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_DATE_FORMAT=''YYYY/MM/DD''';

  BEGIN
  SELECT IVR_VALOR
    INTO p_operador
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'OPERADOR';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'OPERADOR  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_sucursal
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'SUCURSAL';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'SUCURSAL  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_Oficina
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'OFICINA';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'OFICINA  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_Depto
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'DEPARTAMENTO';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'DEPARTAMENTO  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO P_terminal
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'TERMINAL';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'DEPARTAMENTO  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_horarionormal
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'HORARIO_NORMAL';
  IF p_horarionormal IS NULL THEN
     raise_application_error(-20006,'HORARIO  IVR  DEFINIDO INCORRECTAMENTE');
  END IF;
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20006,'DEPARTAMENTO  IVR  DEFINIDO INCORRECTAMENTE');
  END;                       
  P_OPERADOR := PI_OPERADOR;
  P_SUCURSAL := PI_SUCURSAL;
  P_OFICINA := PI_OFICINA;
  P_DEPTO := PI_DEPTO;  
  P_TERMINAL := PI_TERMINAL;
    
  begin
    insert into tgen_sesion
      (ses_sesion, ses_sucursal, ses_oficina, ses_operador)
    values
      (userenv('sessionid'), p_sucursal, p_oficina, p_operador);
  exception when dup_val_on_index then null;
            when others then
        raise_application_error(-20898,'SESION NO GENERADA '||sqlerrm);
  end;
        --<<hd10435
         --llamada a la iniciacion de variable
  pkg_gen_bancaelectronica.fun_gen_InicializaParWEB('S',null,p_Canal, P_CUENTA, P_CUENTA);--p_tarjeta);
         -->>hd10435

--FIN PARAMETROS GENERALES
  IF TO_NUMBER(TO_CHAR(fecha,'HH24MMSS')) > TO_NUMBER(p_horarionormal) THEN
     p_diferido:='S';
  ELSE
     p_diferido:='N';
  END IF; -- Fin de fechas
  val_cuenta(p_cuenta,4,'D',99);
  --v_tc:=PKG_CMS_INTERFAZCREDITCARD.VALIDA_TARJETA(p_tarjeta);
   --SAVE_LOG(' P_FECHA:'|| P_FECHA);
   W_RELACION :=NULL;   
  BEGIN
  SELECT VIS_CODCLI,VIS_MONEDA,VIS_MOD,VIS_PRO,VIS_TIP,CLC_TITULAR
    INTO p_cliente,p_moneda,P_MODORG,P_PROORG,P_TIPORG,W_RELACION
    FROM TCAP_VISTA,TCLI_CLICTA
   WHERE VIS_NUMCUE = P_CUENTA
     AND VIS_NUMCUE = CLC_CUE
     AND VIS_CODCLI = CLC_CODCLI;
  EXCEPTION
   WHEN OTHERS THEN
   P_ServRegistroOK :='1';--o no exiete error 1 existe un error
   P_ServMsgError := 'ERROR EN CUENTA  INGRESADA:';
   --raise_application_error(-20002,'ERROR EN CUENTA  INGRESADA:'||SQLERRM);
  END;

 BEGIN
 SELECT CLIENT
   INTO p_clientetar
  FROM tcreditextract
 WHERE CREDIT=  p_tarjeta;
  EXCEPTION
   WHEN OTHERS THEN
     NULL;
       --raise_application_error(-20002,'ERROR EN TARJETA  INGRESADA:'||SQLERRM);
  END;

--IF P_cliente <> P_clienteTAR THEN
--raise_application_error(-20005,'10-Error de Datos');
--END IF;
--PARA QUE PUEDAN PAGAR LOS COTITULARES CON RELACION O
  IF P_cliente <> P_clienteTAR THEN
     FOR X IN (SELECT CLC_CODCLI
                 FROM TCLI_CLICTA
                 WHERE CLC_CUE = P_CUENTA
                 ORDER BY CLC_ORDEN) LOOP
        dbms_output.put_line('Pas1:'||W_RELACION);
      IF P_clienteTAR = X.CLC_CODCLI THEN
        dbms_output.put_line(W_RELACION);
         IF W_RELACION = 'O' THEN
            P_cliente:=P_clienteTAR;
            EXIT;
         END IF;
      END IF;
     END LOOP;
  END IF;

  IF P_cliente <> P_clienteTAR THEN
   P_ServRegistroOK :='1';--o no exiete error 1 existe un error
   P_ServMsgError := 'Error de Datos. Cliente No Autorizado para realizar pagos con esta cuenta.'||P_cliente||' - '||P_clienteTAR||' - '||W_RELACION;
   --raise_application_error(-20005,'Error de Datos. Cliente no permitido para realizar pagos con esta cuenta.');
  END IF;
--Para multimoneda  
IF P_ServRegistroOK ='0' THEN
IF p_moneda <> P_MONEDAPAGAR THEN
        vln_totalxmon   := NULL;
        vln_totmoncta   := NULL;
        vln_cotizcompra := NULL;
        vln_cotizventa  := NULL;
        vlc_tipotra     := NULL;
   		vln_monto       := NULL;                      
		vln_montotra       := NULL;                         		
           
        IF  P_MONEDAPAGAR = 0 THEN
          vln_totmoncta := P_VALOR;--PNI_MONTO;
          vlc_tipotra   := 'C'; -- Compra

        ELSE
          vln_totalxmon := P_VALOR;--PNI_MONTO;
          vlc_tipotra   := 'D'; -- Venta

        END IF;
  
        IF vlc_tipotra = 'C' THEN
          pkg_cambios.pcotdivisas(p_sucursal,
                                  vlc_tipotra,
                                  0,
                                  P_MONEDAPAGAR,
                                  p_moneda,
                                  p_cliente,
                                  vln_cotizcompra,
                                  vln_cotizventa,
                                  vln_totalxmon,
                                  vln_totmoncta);
                   vln_monto:= vln_totalxmon;
                   vln_montotra:= vln_totmoncta;                                                                    
        ELSIF vlc_tipotra = 'D' THEN
          pkg_cambios.pcotdivisas(p_sucursal,
                                  vlc_tipotra,
                                  0,
                                  p_moneda,
                                  P_MONEDAPAGAR,                                
                                  p_cliente,
                                  vln_cotizcompra,
                                  vln_cotizventa,
                                  vln_totalxmon,
                                  vln_totmoncta);                              
                   vln_monto:= vln_totmoncta;                                                                    
                   vln_montotra:= vln_totalxmon;                                                                                       
        END IF;                                                                                                     
        
	  P_MONORG:= p_moneda;
	  P_MONDES:= P_MONEDAPAGAR;
ELSE
  P_MONORG:= p_moneda;
  P_MONDES:= P_MONEDAPAGAR;
  vln_monto:= p_valor;                                                                    
  vln_montotra:= p_valor;                                                                                       
  vln_cotizcompra:=1;
  vln_cotizventa:=1;
END IF;
DBMS_OUTPUT.PUT_LINE('vln_totalxmon:'||vln_totalxmon);
DBMS_OUTPUT.PUT_LINE('vln_totmoncta:'||vln_totmoncta);
DBMS_OUTPUT.PUT_LINE('vln_cotizcompra:'||vln_cotizcompra);
DBMS_OUTPUT.PUT_LINE('vln_cotizventa:'||vln_cotizventa);

--fin de multimoneda

  P_secuencia := UPD_SECTRAN(P_operador);
  P_MODDES:= 3;--CAJA
  P_TRANSACCION:=4;
  P_PRODUCTO :=1;
  P_TIPO:=1;
  P_ServRegistroOK :='0';
  P_ServMsgError:=NULL;

  IF P_MONEDAPAGAR = 0 THEN
     p_codigoserv := 1;--DE ACUERDO A LO DEFINIDO EN BDI
     p_rubro := 6;
  ELSE
    p_codigoserv:= 4;--DE ACUERDO A LO DEFINIDO EN BDI
    p_rubro := 46;
  END IF;
  dbms_output.put_line('paso 1');
  --tipos
  SELECT COD_TIPOTRA,RUB_EFECTIVO
    INTO P_TIPOTRA,P_TIPORUB
    FROM TGEN_CODTRANS,TGEN_TRANRUBRO
    WHERE COD_MOD = RUB_MOD
      AND COD_TRA =RUB_TRA
      AND COD_MOD = P_MODDES
      AND COD_TRA = P_TRANSACCION
      AND RUB_RUBRO = P_RUBRO;
  dbms_output.put_line('paso 1.2 P_VALOR:'||P_VALOR);

  numero_ref(p_sucursal,p_oficina,
           p_operador,null,10,p_refd,modifica);

    saldo:=sal_cuenta(p_cuenta,1,1);
    vlc_disponible:=saldo;
     if saldo < nvl(vln_monto,0) then
            raise_application_error(-20007, 'FONDOS INSUFICIENTES');
     end if;

  --
  p_valoraux := p_valor ;      
  --aqui llena la 3-4
  INSERTA_TCAP_TRAMONAUX(p_operador,p_secuencia,p_sucursal,p_oficina ,p_diferido,p_modDES ,p_producto ,p_tipo,P_MONEDAPAGAR ,
  p_transaccion,
                         p_rubro,p_depto,P_REF,P_TIPOTRA,vln_montotra,P_tiporub,P_CUENTA,p_autorizador,p_terminal);
  INS_TCAJ_FORPAGO_MULTI(P_CUENTA,P_MODORG,P_PROORG,P_TIPORG,P_MONORG,
                   P_DEPTO, P_SUCURSAL,P_OFICINA,P_CLIENTE,vln_monto,
                   P_MODDES,P_PRODES,P_TIPDES,P_MONDES,P_OPERADOR,vln_cotizcompra,vln_cotizventa,vlc_disponible,P_NUMPAGO);--INSERTA EN TCAJ_FORPAGO
  p_usrsecuencia := P_secuencia;
  P_sec   := ver_sec(f_fechatrabajo,p_operador,p_usrsecuencia)-1;
  realiza_debitos(P_CUENTA,P_SUCURSAL,P_OFICINA,P_DEPTO,P_REFD,P_DIFERIDO,P_OPERADOR,p_usrsecuencia,P_NUMPAGO,P_SEC); --REALIZA EL DEBITO DE LA CUENTA
  inserta_tcap_tramon(P_CUENTA,P_OPERADOR,P_SECUENCIA,P_TRANSACCION,P_DIFERIDO,P_VALOR,P_CLIENTE);--INSERTA EN TCAP_TRAMON
  INSERTA_TCAP_PAGOSERVICIO(p_tarjeta,p_tipopago,p_valor,p_codigoserv,
                            p_sucursal,p_oficina,p_operador,p_secuencia,
                            p_diferido,P_DEPTO,P_TERMINAL,P_SEC,p_parametro3,p_parametro4);--INSERT EN TCAP_PAGOSERVICIOS
  VALIDA_TRANSACCION_MAS(P_OPERADOR,p_secuencia,p_mensaje,P_ServRegistroOK,P_ServMsgError,p_canal,P_MONEDAPAGAR);
  P_CODUSRP := P_OPERADOR;
  P_NUMTRAP := p_secuencia;
END IF;
  IF nvl(P_ServRegistroOk,'X') != '0' then
    ROLLBACK;
   	 raise_application_error(-20901,'Error Interface Externa.... : '||P_ServMsgError);
  ELSE
     --COMMIT;
     NULL;
  END IF;


EXCEPTION
WHEN OTHERS THEN
   ROLLBACK;
   P_ServRegistroOK :='1';--o no exiete error 1 existe un error
   P_ServMsgError := 'Err:'||sqlerrm;   
 --raise_application_error(-20901,'ERROR EN PAGO DE TARJETA '||sqlerrm);
END;
---MASIVO CON DEBITO CUENTA CONTROL DE CLIENTE DE LA 1/9107
/*----------------------------PAGOS MASIVOS DEB CTA---------------------------------*/
PROCEDURE PAGO_TARJETA_TC_MASDEBCTA (P_CUENTA   IN NUMBER,   --ahorros
                           P_TARJETA  IN VARCHAR2, --NUMBER,   --tarjeta
						   P_MONEDAPAGAR IN NUMBER,--moneda de TC que voy a pagar                           
                           P_TIPOPAGO IN VARCHAR2, --N
                           P_VALOR    IN  NUMBER,--valor a debitar - pagar
                           P_CANAL IN NUMBER,  
                           PI_OPERADOR IN NUMBER,--operador que envia los pagos
                           PI_SUCURSAL IN NUMBER,
                           PI_OFICINA IN NUMBER,
                           PI_DEPTO IN NUMBER,
                           PI_TERMINAL IN VARCHAR2,                           
                           P_MENSAJE   OUT VARCHAR2, --mensaje a transmitir
                           P_CODUSRP OUT NUMBER,--codigo que ejecuto la trans
                           P_NUMTRAP OUT NUMBER,--numero de transaccion unica
                           P_ServRegistroOK  OUT VARCHAR2,--o no exiete error 1 existe un error
                           P_ServMsgError OUT VARCHAR2) IS --si es 1 se despliega el mensaje de error
  P_sucursal   number;
  P_oficina    number;
  P_depto      number;
  P_operador   number;
  P_terminal    varchar2(20);
  fecha DATE;
  p_horarionormal VARCHAR2(8):= NULL;
  p_secuencia number;
  p_diferido varchar2(1);
  p_moneda number;
  p_rubro  number;
  P_REF varchar2(20):='44699999';
  P_REFD NUMBER;
  P_TIPOTRA VARCHAR2(1);
  P_tiporub VARCHAR2(1);
  p_autorizador number;
  P_MODORG NUMBER;
  P_PROORG NUMBER;
  P_TIPORG NUMBER;
  P_MONORG NUMBER;
  P_MODDES NUMBER;
  P_PRODES NUMBER:= NULL;
  P_TIPDES NUMBER:= NULL;
  P_MONDES NUMBER;
  P_NUMPAGO NUMBER;
  p_parametro3 VARCHAR2(30);
  p_parametro4 VARCHAR2(30);
  p_clientetar NUMBER;
  P_CLIENTE NUMBER;
  p_codigoserv NUMBER;
  P_PRODUCTO NUMBER:=1;
  P_TIPO NUMBER:=1;
  P_TRANSACCION NUMBER:=4;
  P_USRSECUENCIA NUMBER;
  P_SEC NUMBER;
  v_tc varchar2(1);
  modifica	varchar2(2);
  saldo     number(18,3);
  p_valoraux number;
--MULTIMONEDA
   vln_totalxmon      tpre_prestamos.pre_monto%TYPE;
   vln_totmoncta      tpre_prestamos.pre_monto%TYPE;
   vln_monto      tpre_prestamos.pre_monto%TYPE;
   vln_montotra      tpre_prestamos.pre_monto%TYPE;   
   vln_cotizcompra    tgen_monfecha.mof_promedio%TYPE;
   vln_cotizventa     tgen_monfecha.mof_promedio%TYPE;
   vlc_tipotra        VARCHAR2(1);
  vlc_DISPONIBLE  NUMBER;
     W_RELACION VARCHAR2(1);
BEGIN
--PARAMETROS GENERALES
  -- Insertar sesion
P_ServRegistroOK :='0';  
   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_LANGUAGE=''ENGLISH''';
   EXECUTE IMMEDIATE 'ALTER SESSION SET NLS_DATE_FORMAT=''YYYY/MM/DD''';

  BEGIN
  SELECT IVR_VALOR
    INTO p_operador
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'OPERADOR';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'OPERADOR  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_sucursal
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'SUCURSAL';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'SUCURSAL  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_Oficina
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'OFICINA';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'OFICINA  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_Depto
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'DEPARTAMENTO';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'DEPARTAMENTO  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO P_terminal
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'TERMINAL';
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20192,'DEPARTAMENTO  IVR  DEFINIDO INCORRECTAMENTE');
  END;
  BEGIN
  SELECT IVR_VALOR
    INTO p_horarionormal
    FROM TGEN_DEF_IVR
   WHERE IVR_COD = 'HORARIO_NORMAL';
  IF p_horarionormal IS NULL THEN
     raise_application_error(-20006,'HORARIO  IVR  DEFINIDO INCORRECTAMENTE');
  END IF;
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20006,'DEPARTAMENTO  IVR  DEFINIDO INCORRECTAMENTE');
  END;                       
  P_OPERADOR := PI_OPERADOR;
  P_SUCURSAL := PI_SUCURSAL;
  P_OFICINA := PI_OFICINA;
  P_DEPTO := PI_DEPTO;  
  P_TERMINAL := PI_TERMINAL;
    
  begin
    insert into tgen_sesion
      (ses_sesion, ses_sucursal, ses_oficina, ses_operador)
    values
      (userenv('sessionid'), p_sucursal, p_oficina, p_operador);
  exception when dup_val_on_index then null;
            when others then
        raise_application_error(-20898,'SESION NO GENERADA '||sqlerrm);
  end;
        --<<hd10435
         --llamada a la iniciacion de variable
  pkg_gen_bancaelectronica.fun_gen_InicializaParWEB('S',null,p_Canal, P_CUENTA, P_CUENTA);--p_tarjeta);
         -->>hd10435

--FIN PARAMETROS GENERALES
  IF TO_NUMBER(TO_CHAR(fecha,'HH24MMSS')) > TO_NUMBER(p_horarionormal) THEN
     p_diferido:='S';
  ELSE
     p_diferido:='N';
  END IF; -- Fin de fechas
  val_cuenta(p_cuenta,4,'D',99);
  --v_tc:=PKG_CMS_INTERFAZCREDITCARD.VALIDA_TARJETA(p_tarjeta);
   --SAVE_LOG(' P_FECHA:'|| P_FECHA);
   W_RELACION :=NULL;   
  BEGIN
  SELECT VIS_CODCLI,VIS_MONEDA,VIS_MOD,VIS_PRO,VIS_TIP,CLC_TITULAR
    INTO p_cliente,p_moneda,P_MODORG,P_PROORG,P_TIPORG,W_RELACION
    FROM TCAP_VISTA,TCLI_CLICTA
   WHERE VIS_NUMCUE = P_CUENTA
     AND VIS_NUMCUE = CLC_CUE
     AND VIS_CODCLI = CLC_CODCLI;
  EXCEPTION
   WHEN OTHERS THEN
   P_ServRegistroOK :='1';--o no exiete error 1 existe un error
   P_ServMsgError := 'ERROR EN CUENTA  INGRESADA:';
   --raise_application_error(-20002,'ERROR EN CUENTA  INGRESADA:'||SQLERRM);
  END;

 BEGIN
 SELECT CLIENT
   INTO p_clientetar
  FROM tcreditextract
 WHERE CREDIT=  p_tarjeta;
  EXCEPTION
   WHEN OTHERS THEN
     NULL;
       --raise_application_error(-20002,'ERROR EN TARJETA  INGRESADA:'||SQLERRM);
  END;
 --para ver el cliente que paga la tc
   BEGIN
   SELECT prc_codcli
    INTO p_clientetar
    FROM tcap_paramservclient_tc_dom
    WHERE prc_tc = p_tarjeta
      AND prc_codserv = DECODE(P_MONEDAPAGAR,0,1,4);--pesos;
   EXCEPTION
     WHEN OTHERS THEN 
		 BEGIN
		 SELECT CLIENT
		   INTO p_clientetar
		  FROM tcreditextract
		 WHERE CREDIT=  p_tarjeta;
		  EXCEPTION
		   WHEN OTHERS THEN
		     NULL;
		       --raise_application_error(-20002,'ERROR EN TARJETA  INGRESADA:'||SQLERRM);
		  END;
   END;                                                        
  --fin de cliente que paga la tc
--IF P_cliente <> P_clienteTAR THEN
--raise_application_error(-20005,'10-Error de Datos');
--END IF;
--PARA QUE PUEDAN PAGAR LOS COTITULARES CON RELACION O
  IF P_cliente <> P_clienteTAR THEN
     FOR X IN (SELECT CLC_CODCLI
                 FROM TCLI_CLICTA
                 WHERE CLC_CUE = P_CUENTA
                 ORDER BY CLC_ORDEN) LOOP
        dbms_output.put_line('Pas1:'||W_RELACION);
      IF P_clienteTAR = X.CLC_CODCLI THEN
        dbms_output.put_line(W_RELACION);
         IF W_RELACION = 'O' THEN
            P_cliente:=P_clienteTAR;
            EXIT;
         END IF;
      END IF;
     END LOOP;
  END IF;

  IF P_cliente <> P_clienteTAR THEN
   P_ServRegistroOK :='1';--o no exiete error 1 existe un error
   P_ServMsgError := 'Error de Datos. Cliente No Autorizado para realizar pagos con esta cuenta.'||P_cliente||' - '||P_clienteTAR||' - '||W_RELACION;
   --raise_application_error(-20005,'Error de Datos. Cliente no permitido para realizar pagos con esta cuenta.');
  END IF;
--Para multimoneda  
IF P_ServRegistroOK ='0' THEN
IF p_moneda <> P_MONEDAPAGAR THEN
        vln_totalxmon   := NULL;
        vln_totmoncta   := NULL;
        vln_cotizcompra := NULL;
        vln_cotizventa  := NULL;
        vlc_tipotra     := NULL;
   		vln_monto       := NULL;                      
		vln_montotra       := NULL;                         		
           
        IF  P_MONEDAPAGAR = 0 THEN
          vln_totmoncta := P_VALOR;--PNI_MONTO;
          vlc_tipotra   := 'C'; -- Compra

        ELSE
          vln_totalxmon := P_VALOR;--PNI_MONTO;
          vlc_tipotra   := 'D'; -- Venta

        END IF;
  
        IF vlc_tipotra = 'C' THEN
          pkg_cambios.pcotdivisas(p_sucursal,
                                  vlc_tipotra,
                                  0,
                                  P_MONEDAPAGAR,
                                  p_moneda,
                                  p_cliente,
                                  vln_cotizcompra,
                                  vln_cotizventa,
                                  vln_totalxmon,
                                  vln_totmoncta);
                   vln_monto:= vln_totalxmon;
                   vln_montotra:= vln_totmoncta;                                                                    
        ELSIF vlc_tipotra = 'D' THEN
          pkg_cambios.pcotdivisas(p_sucursal,
                                  vlc_tipotra,
                                  0,
                                  p_moneda,
                                  P_MONEDAPAGAR,                                
                                  p_cliente,
                                  vln_cotizcompra,
                                  vln_cotizventa,
                                  vln_totalxmon,
                                  vln_totmoncta);                              
                   vln_monto:= vln_totmoncta;                                                                    
                   vln_montotra:= vln_totalxmon;                                                                                       
        END IF;                                                                                                     
        
	  P_MONORG:= p_moneda;
	  P_MONDES:= P_MONEDAPAGAR;
ELSE
  P_MONORG:= p_moneda;
  P_MONDES:= P_MONEDAPAGAR;
  vln_monto:= p_valor;                                                                    
  vln_montotra:= p_valor;                                                                                       
  vln_cotizcompra:=1;
  vln_cotizventa:=1;
END IF;
DBMS_OUTPUT.PUT_LINE('vln_totalxmon:'||vln_totalxmon);
DBMS_OUTPUT.PUT_LINE('vln_totmoncta:'||vln_totmoncta);
DBMS_OUTPUT.PUT_LINE('vln_cotizcompra:'||vln_cotizcompra);
DBMS_OUTPUT.PUT_LINE('vln_cotizventa:'||vln_cotizventa);

--fin de multimoneda

  P_secuencia := UPD_SECTRAN(P_operador);
  P_MODDES:= 3;--CAJA
  P_TRANSACCION:=4;
  P_PRODUCTO :=1;
  P_TIPO:=1;
  P_ServRegistroOK :='0';
  P_ServMsgError:=NULL;

  IF P_MONEDAPAGAR = 0 THEN
     p_codigoserv := 1;--DE ACUERDO A LO DEFINIDO EN BDI
     p_rubro := 6;
  ELSE
    p_codigoserv:= 4;--DE ACUERDO A LO DEFINIDO EN BDI
    p_rubro := 46;
  END IF;
  dbms_output.put_line('paso 1');
  --tipos
  SELECT COD_TIPOTRA,RUB_EFECTIVO
    INTO P_TIPOTRA,P_TIPORUB
    FROM TGEN_CODTRANS,TGEN_TRANRUBRO
    WHERE COD_MOD = RUB_MOD
      AND COD_TRA =RUB_TRA
      AND COD_MOD = P_MODDES
      AND COD_TRA = P_TRANSACCION
      AND RUB_RUBRO = P_RUBRO;
  dbms_output.put_line('paso 1.2 P_VALOR:'||P_VALOR);

  numero_ref(p_sucursal,p_oficina,
           p_operador,null,10,p_refd,modifica);

    saldo:=sal_cuenta(p_cuenta,1,1);
    vlc_disponible:=saldo;
     if saldo < nvl(vln_monto,0) then
            raise_application_error(-20007, 'FONDOS INSUFICIENTES');
     end if;

  --
  p_valoraux := p_valor ;      
  --aqui llena la 3-4
  INSERTA_TCAP_TRAMONAUX(p_operador,p_secuencia,p_sucursal,p_oficina ,p_diferido,p_modDES ,p_producto ,p_tipo,P_MONEDAPAGAR ,
  p_transaccion,
                         p_rubro,p_depto,P_REF,P_TIPOTRA,vln_montotra,P_tiporub,P_CUENTA,p_autorizador,p_terminal);
  INS_TCAJ_FORPAGO_MULTI(P_CUENTA,P_MODORG,P_PROORG,P_TIPORG,P_MONORG,
                   P_DEPTO, P_SUCURSAL,P_OFICINA,P_CLIENTE,vln_monto,
                   P_MODDES,P_PRODES,P_TIPDES,P_MONDES,P_OPERADOR,vln_cotizcompra,vln_cotizventa,vlc_disponible,P_NUMPAGO);--INSERTA EN TCAJ_FORPAGO
  p_usrsecuencia := P_secuencia;
  P_sec   := ver_sec(f_fechatrabajo,p_operador,p_usrsecuencia)-1;
  realiza_debitos(P_CUENTA,P_SUCURSAL,P_OFICINA,P_DEPTO,P_REFD,P_DIFERIDO,P_OPERADOR,p_usrsecuencia,P_NUMPAGO,P_SEC); --REALIZA EL DEBITO DE LA CUENTA
  inserta_tcap_tramon(P_CUENTA,P_OPERADOR,P_SECUENCIA,P_TRANSACCION,P_DIFERIDO,P_VALOR,P_CLIENTE);--INSERTA EN TCAP_TRAMON
  INSERTA_TCAP_PAGOSERVICIO(p_tarjeta,p_tipopago,p_valor,p_codigoserv,
                            p_sucursal,p_oficina,p_operador,p_secuencia,
                            p_diferido,P_DEPTO,P_TERMINAL,P_SEC,p_parametro3,p_parametro4);--INSERT EN TCAP_PAGOSERVICIOS
  VALIDA_TRANSACCION_MAS(P_OPERADOR,p_secuencia,p_mensaje,P_ServRegistroOK,P_ServMsgError,p_canal,P_MONEDAPAGAR);
  P_CODUSRP := P_OPERADOR;
  P_NUMTRAP := p_secuencia;
END IF;
  IF nvl(P_ServRegistroOk,'X') != '0' then
    ROLLBACK;
   	 raise_application_error(-20901,'Error Interface Externa.... : '||P_ServMsgError);
  ELSE
     --COMMIT;
     NULL;
  END IF;


EXCEPTION
WHEN OTHERS THEN
   ROLLBACK;
   P_ServRegistroOK :='1';--o no exiete error 1 existe un error
   P_ServMsgError := 'Err:'||sqlerrm;   
 --raise_application_error(-20901,'ERROR EN PAGO DE TARJETA '||sqlerrm);
END;

--FIN DE NUEVO PROCESO DE DEBITO MASIVO

/*------------------------FIN DE PAGOS MASIVOS-------------------------------------*/
---PROCEDO DE ANTONHY
PROCEDURE PAG_TC_ANTONHY (P_FECHA IN DATE, --FECHA QUE PAGO EN LA TIENDA
                          P_CUENTA   IN NUMBER,--Cuenta de ahorros o corriente
                          P_TARJETA  IN VARCHAR2, --IN NUMBER
                          P_TIPOPAGO IN VARCHAR2,--N normal
                          P_VALOR    IN  NUMBER,--valor del pago
                          P_CANAL IN NUMBER,--2 caja bdi
                          P_OPERADOR IN NUMBER,--operador que envia los pagos
                          P_SUCURSAL IN NUMBER,
                          P_OFICINA IN NUMBER,
                          P_DEPTO IN NUMBER,
                          P_TERMINAL IN VARCHAR2,
                          P_FORMAPAGO IN NUMBER, --FORMA DE PAGO EFECTIVO O CHEQUE                          
                          P_MENSAJE   OUT VARCHAR2,--mensaje de salida
                          P_CODUSRP OUT NUMBER,--usuario que realizo el pago
                          P_NUMTRAP OUT NUMBER,--numtra
                          P_ServRegistroOK  OUT VARCHAR2,
                          P_ServMsgError OUT VARCHAR2) IS --si es 1 se despliega el mensaje de error
  fecha DATE;
  p_horarionormal VARCHAR2(8):= NULL;
  p_secuencia number;
  p_diferido varchar2(1);
  p_moneda number;
  p_rubro  number;
  P_REF varchar2(20):='44699999';
  P_REFD NUMBER;
  P_TIPOTRA VARCHAR2(1);
  P_tiporub VARCHAR2(1);
  p_autorizador number;
  P_MODORG NUMBER;
  P_PROORG NUMBER;
  P_TIPORG NUMBER;
  P_MONORG NUMBER;
  P_MODDES NUMBER;
  P_PRODES NUMBER:= NULL;
  P_TIPDES NUMBER:= NULL;
  P_MONDES NUMBER;
  P_NUMPAGO NUMBER;
  p_parametro3 VARCHAR2(30);
  p_parametro4 VARCHAR2(30);
  p_clientetar NUMBER;
  P_CLIENTE NUMBER;
  p_codigoserv NUMBER;
  P_PRODUCTO NUMBER:=1;
  P_TIPO NUMBER:=1;
  P_TRANSACCION NUMBER:=4;
  P_USRSECUENCIA NUMBER;
  P_SEC NUMBER;
  v_tc varchar2(1);
  modifica	varchar2(2);
  saldo     number(18,3);
  p_valoraux number;
  p_sqlerrm VARCHAR2(100);
  vlc_DISPONIBLE  NUMBER;
BEGIN
--PARAMETROS GENERALES
  -- Insertar sesion

--FIN PARAMETROS GENERALES
--QUE FECHA ENVIO CUANDO SE PASA DE LA HORA DE TRANSMISION DE PAGOS , ES DECIR QUE FECHA ENVIO
--UNA VEZ QUE YA EJECUTARON EL FIN DE DIA DE TARJETA

/*  IF TO_NUMBER(TO_CHAR(fecha,'HH24MMSS')) > TO_NUMBER(p_horarionormal) THEN
     p_diferido:='S';
  ELSE
     p_diferido:='N';
  END IF; -- Fin de fechas*/
  DELETE LOG_BATCH;
  val_cuenta(p_cuenta,4,'D',99);
  --v_tc:=PKG_CMS_INTERFAZCREDITCARD.VALIDA_TARJETA(p_tarjeta);
   --SAVE_LOG(' P_FECHA:'|| P_FECHA);
  BEGIN
  SELECT VIS_CODCLI,VIS_MONEDA,VIS_MOD,VIS_PRO,VIS_TIP
    INTO p_cliente,p_moneda,P_MODORG,P_PROORG,P_TIPORG
    FROM TCAP_VISTA
   WHERE VIS_NUMCUE = P_CUENTA;
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20002,'ERROR EN CUENTA  INGRESADA:'||SQLERRM);
  END;

 BEGIN
 SELECT CLIENT
   INTO p_clientetar
  FROM tcreditextract
 WHERE CREDIT=  p_tarjeta;
  EXCEPTION
   WHEN OTHERS THEN
     NULL;
       --raise_application_error(-20002,'ERROR EN TARJETA  INGRESADA:'||SQLERRM);
  END;

--Para este caso se va a debitar una sola cuenta y ed ahi se va a distribuir los valores a pagar
--de las tarjetas.

--  IF P_cliente <> P_clienteTAR THEN
  --   raise_application_error(-20005,'10-Error de Datos');
  --END IF;


  P_secuencia := UPD_SECTRAN(P_operador);
  P_MONORG:= p_moneda;
  P_MONDES:= p_moneda;
  P_MODDES:= 3;--CAJA
  P_TRANSACCION:=4;
  P_PRODUCTO :=1;
  P_TIPO:=1;
  P_ServRegistroOK :='0';
  P_ServMsgError:=NULL;

  IF P_MONORG = 0 THEN
     p_codigoserv := 1;--DE ACUERDO A LO DEFINIDO EN BDI
     p_rubro := 6;
  ELSE
    p_codigoserv:= 4;--DE ACUERDO A LO DEFINIDO EN BDI
    p_rubro := 46;
  END IF;
  dbms_output.put_line('paso 1');
  --tipos
	  SELECT COD_TIPOTRA,RUB_EFECTIVO
	    INTO P_TIPOTRA,P_TIPORUB
	    FROM TGEN_CODTRANS,TGEN_TRANRUBRO
	    WHERE COD_MOD = RUB_MOD
	      AND COD_TRA =RUB_TRA
	      AND COD_MOD = P_MODDES
	      AND COD_TRA = P_TRANSACCION
	      AND RUB_RUBRO = P_RUBRO;
	  dbms_output.put_line('paso 1.2 P_VALOR:'||P_VALOR);
	  numero_ref(p_sucursal,p_oficina,
           p_operador,null,10,p_refd,modifica);

    saldo:=sal_cuenta(p_cuenta,1,1); --para este caso la cuenta se va asobregirar
    vlc_DISPONIBLE :=saldo;
    -- if saldo < nvl(p_valor,0) then
      --      raise_application_error(-20007, 'FONDOS INSUFICIENTES');
     ---end if;

  --
	  p_valoraux := p_valor ;
	  INSERTA_TCAP_TRAMONAUX(p_operador,p_secuencia,p_sucursal,p_oficina ,p_diferido,p_modDES ,p_producto ,p_tipo,p_monORG ,p_transaccion,
	                         p_rubro,p_depto,P_REF,P_TIPOTRA,P_VALOR,P_tiporub,P_CUENTA,p_autorizador,p_terminal);
	  INS_TCAJ_FORPAGO(P_CUENTA,P_MODORG,P_PROORG,P_TIPORG,P_MONORG,
	                   P_DEPTO, P_SUCURSAL,P_OFICINA,P_CLIENTE,P_VALORaux,
	                   P_MODDES,P_PRODES,P_TIPDES,P_MONDES,P_OPERADOR,vlc_disponible,P_NUMPAGO);--INSERTA EN TCAJ_FORPAGO
	  p_usrsecuencia := P_secuencia;
	  P_sec   := ver_sec(f_fechatrabajo,p_operador,p_usrsecuencia)-1;
	  realiza_debitos(P_CUENTA,P_SUCURSAL,P_OFICINA,P_DEPTO,P_REFD,P_DIFERIDO,P_OPERADOR,p_usrsecuencia,P_NUMPAGO,P_SEC); --REALIZA EL DEBITO DE LA CUENTA
	  inserta_tcap_tramon(P_CUENTA,P_OPERADOR,P_SECUENCIA,P_TRANSACCION,P_DIFERIDO,P_VALOR,P_CLIENTE);--INSERTA EN TCAP_TRAMON
	  INSERTA_TCAP_PAGOSERVICIO(p_tarjeta,p_tipopago,p_valor,p_codigoserv,
	                            p_sucursal,p_oficina,p_operador,p_secuencia,
	                            p_diferido,P_DEPTO,P_TERMINAL,P_SEC,p_parametro3,p_parametro4);--INSERT EN TCAP_PAGOSERVICIOS
	  VALIDA_TRANSACCION_ANT(P_OPERADOR,p_secuencia,P_FORMAPAGO,p_mensaje,P_ServRegistroOK,P_ServMsgError,p_canal,p_moneda);
	  P_CODUSRP := P_OPERADOR;
	  P_NUMTRAP := p_secuencia;

	  IF nvl(P_ServRegistroOk,'X') != '0' then
	    ROLLBACK;
	   	INSERT INTO LOG_BATCH VALUES('Error en pago Tc.... : '||P_ServRegistroOk||'^'||p_cuenta||'-'||p_tarjeta||'-'||substr(P_ServMsgError,1,50));
	   	COMMIT;
	   	 --raise_application_error(-20901,'Error Interface Externa.... : '||P_ServMsgError);
	  ELSE
	     COMMIT;
	  END IF;

EXCEPTION
WHEN OTHERS THEN
   ROLLBACK;
   p_sqlerrm:=SUBSTR(SQLERRM,1,100);
   P_ServRegistroOk:='1';
-- raise_application_error(-20901,'ERROR EN PAGO DE TARJETA '||sqlerrm);
	   	INSERT INTO LOG_BATCH VALUES('Error en pago Tc.... :'||P_ServRegistroOk||'^'||p_cuenta||'-'||p_tarjeta||'-'||substr(P_ServMsgError,1,50)||p_sqlerrm);
        COMMIT;
END;
--FIN DE PAGO ANTONHY
--PAGO ACH
PROCEDURE PAG_TC_ACH (P_FECHA IN DATE, --FECHA QUE PAGO EN LA TIENDA
                          P_CUENTA   IN NUMBER,--Cuenta de ahorros o corriente
                          P_TARJETA  IN VARCHAR2, --IN NUMBER
                          P_TIPOPAGO IN VARCHAR2,--N normal
                          P_VALOR    IN  NUMBER,--valor del pago
                          P_CANAL IN NUMBER,--2 caja bdi
                          P_OPERADOR IN NUMBER,--operador que envia los pagos
                          P_SUCURSAL IN NUMBER,
                          P_OFICINA IN NUMBER,
                          P_DEPTO IN NUMBER,
                          P_TERMINAL IN VARCHAR2,
                          P_MENSAJE   OUT VARCHAR2,--mensaje de salida
                          P_CODUSRP OUT NUMBER,--usuario que realizo el pago
                          P_NUMTRAP OUT NUMBER,--numtra
                          P_ServRegistroOK  OUT VARCHAR2,
                          P_ServMsgError OUT VARCHAR2) IS --si es 1 se despliega el mensaje de error
  fecha DATE;
  p_horarionormal VARCHAR2(8):= NULL;
  p_secuencia number;
  p_diferido varchar2(1);
  p_moneda number;
  p_rubro  number;
  P_REF varchar2(20):='44699999';
  P_REFD NUMBER;
  P_TIPOTRA VARCHAR2(1);
  P_tiporub VARCHAR2(1);
  p_autorizador number;
  P_MODORG NUMBER;
  P_PROORG NUMBER;
  P_TIPORG NUMBER;
  P_MONORG NUMBER;
  P_MODDES NUMBER;
  P_PRODES NUMBER:= NULL;
  P_TIPDES NUMBER:= NULL;
  P_MONDES NUMBER;
  P_NUMPAGO NUMBER;
  p_parametro3 VARCHAR2(30);
  p_parametro4 VARCHAR2(30);
  p_clientetar NUMBER;
  P_CLIENTE NUMBER;
  p_codigoserv NUMBER;
  P_PRODUCTO NUMBER:=1;
  P_TIPO NUMBER:=1;
  P_TRANSACCION NUMBER:=4;
  P_USRSECUENCIA NUMBER;
  P_SEC NUMBER;
  v_tc varchar2(1);
  modifica	varchar2(2);
  saldo     number(18,3);
  p_valoraux number;
  p_sqlerrm VARCHAR2(100);
  W_RUBROCRE VARCHAR2(40):=NULL;
  W_TRACRE VARCHAR2(40):=NULL;
    vlc_DISPONIBLE  NUMBER;
BEGIN
  dbms_output.put_line('paso 0');
--PARAMETROS GENERALES
  -- Insertar sesion

--FIN PARAMETROS GENERALES
--QUE FECHA ENVIO CUANDO SE PASA DE LA HORA DE TRANSMISION DE PAGOS , ES DECIR QUE FECHA ENVIO
--UNA VEZ QUE YA EJECUTARON EL FIN DE DIA DE TARJETA

/*  IF TO_NUMBER(TO_CHAR(fecha,'HH24MMSS')) > TO_NUMBER(p_horarionormal) THEN
     p_diferido:='S';
  ELSE
     p_diferido:='N';
  END IF; -- Fin de fechas*/
  DELETE LOG_BATCH;          
  COMMIT;
  val_cuenta(p_cuenta,4,'D',99);
  --v_tc:=PKG_CMS_INTERFAZCREDITCARD.VALIDA_TARJETA(p_tarjeta);
   --SAVE_LOG(' P_FECHA:'|| P_FECHA);
  dbms_output.put_line('paso 0.1');   
  BEGIN
  SELECT VIS_CODCLI,VIS_MONEDA,VIS_MOD,VIS_PRO,VIS_TIP
    INTO p_cliente,p_moneda,P_MODORG,P_PROORG,P_TIPORG
    FROM TCAP_VISTA
   WHERE VIS_NUMCUE = P_CUENTA;
  EXCEPTION
   WHEN OTHERS THEN
       raise_application_error(-20002,'ERROR EN CUENTA  INGRESADA:'||SQLERRM);
  END;
  dbms_output.put_line('paso 0.2');   
 BEGIN
 SELECT CLIENT
   INTO p_clientetar
  FROM tcreditextract
 WHERE CREDIT=  p_tarjeta;
  EXCEPTION
   WHEN OTHERS THEN
     NULL;
       --raise_application_error(-20002,'ERROR EN TARJETA  INGRESADA:'||SQLERRM);
  END;
  dbms_output.put_line('paso 0.3');   
 BEGIN
 SELECT APA_VALOR
   INTO W_TRACRE
  FROM TATM_PARAMETRO
 WHERE APA_CONCEPTO= 'TRANSACCION_ACH'
   AND APA_ITEM='CREDITO_X_ACH';
  EXCEPTION
   WHEN OTHERS THEN
    W_TRACRE:=32;
       --raise_application_error(-20002,'ERROR EN TARJETA  INGRESADA:'||SQLERRM);
  END;
  dbms_output.put_line('paso 0.4');   
 BEGIN
 SELECT APA_VALOR
   INTO W_RUBROCRE
  FROM TATM_PARAMETRO
 WHERE APA_CONCEPTO= 'RUBRO_ACH'
   AND APA_ITEM='RUBRO_CRED_ACH';
  EXCEPTION
   WHEN OTHERS THEN
    W_RUBROCRE:=1;
       --raise_application_error(-20002,'ERROR EN TARJETA  INGRESADA:'||SQLERRM);
  END;

  dbms_output.put_line('paso 0.6');   
--Para este caso se va a debitar una sola cuenta y ed ahi se va a distribuir los valores a pagar
--de las tarjetas.

--  IF P_cliente <> P_clienteTAR THEN
  --   raise_application_error(-20005,'10-Error de Datos');
  --END IF;


  P_secuencia := UPD_SECTRAN(P_operador);
  dbms_output.put_line('paso 0.7');     
  P_MONORG:= p_moneda;
  P_MONDES:= p_moneda;
  P_MODDES:= 3;--CAJA
  P_TRANSACCION:=4;
  P_PRODUCTO :=1;
  P_TIPO:=1;
  P_ServRegistroOK :='0';
  P_ServMsgError:=NULL;

  IF P_MONORG = 0 THEN
     p_codigoserv := 1;--DE ACUERDO A LO DEFINIDO EN BDI
     p_rubro := 6;
  ELSE
    p_codigoserv:= 4;--DE ACUERDO A LO DEFINIDO EN BDI
    p_rubro := 46;
  END IF;
  dbms_output.put_line('paso 1');
  --tipos
	  SELECT COD_TIPOTRA,RUB_EFECTIVO
	    INTO P_TIPOTRA,P_TIPORUB
	    FROM TGEN_CODTRANS,TGEN_TRANRUBRO
	    WHERE COD_MOD = RUB_MOD
	      AND COD_TRA =RUB_TRA
	      AND COD_MOD = P_MODDES
	      AND COD_TRA = P_TRANSACCION
	      AND RUB_RUBRO = P_RUBRO;
	  dbms_output.put_line('paso 1.2 P_VALOR:'||P_VALOR);
	  numero_ref(p_sucursal,p_oficina,
           p_operador,null,10,p_refd,modifica);

    saldo:=sal_cuenta(p_cuenta,1,1); --para este caso la cuenta se va asobregirar
      vlc_DISPONIBLE:=saldo;
    -- if saldo < nvl(p_valor,0) then
    --      raise_application_error(-20007, 'FONDOS INSUFICIENTES');
   ---end if;

  --
	  p_valoraux := p_valor ;
	  INSERTA_TCAP_TRAMONAUX(p_operador,p_secuencia,p_sucursal,p_oficina ,p_diferido,p_modDES ,p_producto ,p_tipo,p_monORG ,p_transaccion,
	                         p_rubro,p_depto,P_REF,P_TIPOTRA,P_VALOR,P_tiporub,P_CUENTA,p_autorizador,p_terminal);
	  INS_TCAJ_FORPAGO(P_CUENTA,P_MODORG,P_PROORG,P_TIPORG,P_MONORG,
	                   P_DEPTO, P_SUCURSAL,P_OFICINA,P_CLIENTE,P_VALORaux,
	                   P_MODDES,P_PRODES,P_TIPDES,P_MONDES,P_OPERADOR,vlc_disponible,P_NUMPAGO);--INSERTA EN TCAJ_FORPAGO
	  p_usrsecuencia := P_secuencia;
	  P_sec   := ver_sec(f_fechatrabajo,p_operador,p_usrsecuencia);
      realiza_credito(P_CUENTA,P_SUCURSAL ,P_OFICINA,P_DEPTO,P_REF,P_DIFERIDO,P_OPERADOR,p_usrsecuencia,P_NUMPAGO,
                      P_SEC,P_MODORG,P_PROORG,P_TIPORG,P_MONEDA,TO_NUMBER(W_TRACRE),TO_NUMBER(W_RUBROCRE),P_VALOR);
	  P_sec   := ver_sec(f_fechatrabajo,p_operador,p_usrsecuencia)-1;
	  realiza_debitos(P_CUENTA,P_SUCURSAL,P_OFICINA,P_DEPTO,P_REFD,P_DIFERIDO,P_OPERADOR,p_usrsecuencia,P_NUMPAGO,P_SEC); --REALIZA EL DEBITO DE LA CUENTA
	  inserta_tcap_tramon(P_CUENTA,P_OPERADOR,P_SECUENCIA,P_TRANSACCION,P_DIFERIDO,P_VALOR,P_CLIENTE);--INSERTA EN TCAP_TRAMON
	  INSERTA_TCAP_PAGOSERVICIO(p_tarjeta,p_tipopago,p_valor,p_codigoserv,
	                            p_sucursal,p_oficina,p_operador,p_secuencia,
	                            p_diferido,P_DEPTO,P_TERMINAL,P_SEC,p_parametro3,p_parametro4);--INSERT EN TCAP_PAGOSERVICIOS
	  VALIDA_TRANSACCION_ACH(P_OPERADOR,p_secuencia,p_mensaje,P_ServRegistroOK,P_ServMsgError,p_canal);
	  P_CODUSRP := P_OPERADOR;
	  P_NUMTRAP := p_secuencia;

	  IF nvl(P_ServRegistroOk,'X') != '0' then
	    ROLLBACK;
	   	INSERT INTO LOG_BATCH VALUES('Error en pago Tc.... : '||P_ServRegistroOk||'^'||p_cuenta||'-'||p_tarjeta||'-'||substr(P_ServMsgError,1,50));
	   	COMMIT;
	   	 --raise_application_error(-20901,'Error Interface Externa.... : '||P_ServMsgError);
	  ELSE
	     COMMIT;
	  END IF;

EXCEPTION
WHEN OTHERS THEN
   ROLLBACK;
   p_sqlerrm:=SUBSTR(SQLERRM,1,100);
   P_ServRegistroOk:='1';
   P_ServMsgError:=p_sqlerrm;
-- raise_application_error(-20901,'ERROR EN PAGO DE TARJETA '||sqlerrm);
	   	INSERT INTO LOG_BATCH VALUES('Error en pago Tc.... :'||P_ServRegistroOk||'^'||p_cuenta||'-'||p_tarjeta||'-'||substr(P_ServMsgError,1,50)||p_sqlerrm);
        COMMIT;
END;
--FIN PAGO ACH
PROCEDURE INS_TCAJ_FORPAGO(P_CUENTA IN NUMBER,
                           P_MODORG IN NUMBER,
                           P_PROORG IN NUMBER,
                           P_TIPORG IN NUMBER,
                           P_MONORG IN NUMBER,
                           P_DEPTO  IN NUMBER,
						   P_SUC IN NUMBER,
						   P_OFI IN NUMBER,
						   P_CLIENTE NUMBER,
						   P_VALOR IN OUT NUMBER,
                           P_MODDES IN NUMBER,
                           P_PRODES IN NUMBER,
                           P_TIPDES IN NUMBER,
                           P_MONDES IN NUMBER,
                           P_OPERADOR IN  NUMBER,
                           P_DISPONIBLE IN NUMBER,
                           P_NUMPAGO OUT NUMBER) IS
 p_sec number := 0;
 P_FECHA DATE;
 P_NUMSOL NUMBER;
 P_CUENTADES NUMBER;
 P_TRANSA NUMBER;
 p_deptoorg NUMBER;
 p_valorpenalizado NUMBER;
 p_cotcompra NUMBER:=1;
 p_cotventa NUMBER:=1;
 totalxmon NUMBER:=0;
 totmoncta NUMBER:=0;
 p_codcargo NUMBER := NULL;
 p_cargo NUMBER :=NULL;
 p_traorg NUMBER:=16;
 sobregiro VARCHAR2(1);
 abierto VARCHAR2(1);
BEGIN
  dbms_output.put_line('paso 2.1');
 P_numpago:=upd_secforpago(p_operador);
 IF p_valor IS NOT NULL THEN
    p_sec:=p_sec+1;
  END IF;
  p_fecha     := f_fechareal;
  p_numsol    := null;
  dbms_output.put_line('paso 2.2:'||p_valor);
  --p_cuentaorg := p_cuenta;
  --p_prodes := null;
  --p_tipdes := null;
  p_cuentades :=p_operador; --null;
  --p_clides := p_cliente;
  --p_mondes:= p_monorg;--Para este caso son de la misma moneda
  p_transa := null;
  p_deptoorg := 42;
  p_valorpenalizado:=NULL;
  --p_cotcompra := 1;
  --p_cotventa :=  1;
   pkg_cambios.pcotdivisas_ven(p_suc,'C',0,
               p_monorg,p_monorg,P_cliente,
               p_cotcompra,p_cotventa,totalxmon,totmoncta,'S');
   --p_valor := totalxmon;
  dbms_output.put_line('paso 2.3:'||p_cotcompra||' p_cotventa:'||p_cotventa||' p_valor:'||p_valor);
    if p_modorg = 4 then -- para el cargo por transferencia
        val_cargo(p_cuenta,p_modorg,p_proorg,p_tiporg,
                  p_monorg,12,p_valor,p_fecha,p_cargo,
                  sobregiro,abierto);     
      dbms_output.put_line('P_DISPONIBLE:'||P_DISPONIBLE||' p_valor:'||p_valor||' p_cargo:'||p_cargo);
                        
      /*if  nvl(p_valor,0)+nvl(p_cargo,0) >
        nvl(P_DISPONIBLE,0)   then
        raise_application_error(-20901,'Valida: Valor más Cargo Sobrepaso el saldo máximo disponible.'||sqlerrm);        
      end if;*/
    end if; 
  
   insert into tcaj_forpago
      (fpa_codeje, fpa_numpago, fpa_sec, fpa_fecha, fpa_sucorg,
       fpa_ofiorg, fpa_modorg, fpa_valor, fpa_monorg, fpa_moddes,
       fpa_cotcompra, fpa_cotventa, fpa_proorg, fpa_tiporg,
       fpa_cuentaorg, fpa_prodes, fpa_tipdes, fpa_cuentades, fpa_clides,
       fpa_mondes, fpa_codcargo, fpa_cargo,fpa_numsol,fpa_transa,fpa_traorg,
       fpa_deptoORG, fpa_valorpenalizado,FPA_SUCDES, FPA_OFIDES)
   values
      (p_operador,p_numpago,p_sec,p_fecha,p_suc,
       p_ofi,p_modorg,p_valor,p_monorg,p_moddes,
       p_cotcompra,p_cotventa,p_proorg,p_tiporg,
       p_cuenta,p_prodes,p_tipdes,p_cuentades,p_cliente,
       p_mondes,p_codcargo,p_cargo,p_numsol,p_transa,p_traorg,
       p_depto, p_valorpenalizado,p_suc,p_ofi);
  dbms_output.put_line('paso 2.4');
EXCEPTION
WHEN OTHERS THEN
 raise_application_error(-20901,'ERROR EN TCAJ_FORPAGO '||sqlerrm);
END;
---
PROCEDURE INS_TCAJ_FORPAGO_MULTI(P_CUENTA IN NUMBER,
                           P_MODORG IN NUMBER,
                           P_PROORG IN NUMBER,
                           P_TIPORG IN NUMBER,
                           P_MONORG IN NUMBER,
                           P_DEPTO  IN NUMBER,
						   P_SUC IN NUMBER,
						   P_OFI IN NUMBER,
						   P_CLIENTE NUMBER,
						   P_VALOR IN OUT NUMBER,
                           P_MODDES IN NUMBER,
                           P_PRODES IN NUMBER,
                           P_TIPDES IN NUMBER,
                           P_MONDES IN NUMBER,
                           P_OPERADOR IN  NUMBER,
                           P_TASACOMPRA IN NUMBER,
                           P_TASAVENTA IN NUMBER, 
                           P_DISPONIBLE IN NUMBER,                                                     
                           P_NUMPAGO OUT NUMBER) IS
 p_sec number := 0;
 P_FECHA DATE;
 P_NUMSOL NUMBER;
 P_CUENTADES NUMBER;
 P_TRANSA NUMBER;
 p_deptoorg NUMBER;
 p_valorpenalizado NUMBER;
 p_cotcompra NUMBER:=1;
 p_cotventa NUMBER:=1;
 totalxmon NUMBER:=0;
 totmoncta NUMBER:=0;
 p_codcargo NUMBER := NULL;
 p_cargo NUMBER :=NULL;
 p_traorg NUMBER:=16;     
 sobregiro VARCHAR2(1);
 abierto VARCHAR2(1); 
BEGIN
  dbms_output.put_line('paso 2.1');
 P_numpago:=upd_secforpago(p_operador);
 IF p_valor IS NOT NULL THEN
    p_sec:=p_sec+1;
  END IF;
  p_fecha     := f_fechareal;
  p_numsol    := null;
  dbms_output.put_line('paso 2.2:'||p_valor);
  --p_cuentaorg := p_cuenta;
  --p_prodes := null;
  --p_tipdes := null;
  p_cuentades :=p_operador; --null;
  --p_clides := p_cliente;
  --p_mondes:= p_monorg;--Para este caso son de la misma moneda
  p_transa := null;
  p_deptoorg := 42;
  p_valorpenalizado:=NULL;
  --p_cotcompra := 1;
  --p_cotventa :=  1;
	 p_cotcompra :=p_tasacompra;
	 p_cotventa :=p_tasaventa;
   --p_valor := totalxmon;
    if p_modorg = 4 then -- para el cargo por transferencia
        val_cargo(p_cuenta,p_modorg,p_proorg,p_tiporg,
                  p_monorg,12,p_valor,p_fecha,p_cargo,
                             sobregiro,abierto);
      if  nvl(p_valor,0)+nvl(p_cargo,0) >
        nvl(P_DISPONIBLE,0)   then
        raise_application_error(-20901,'Valida: Valor más Cargo Sobrepaso el saldo máximo disponible.'||sqlerrm);        
      end if;
    end if; 
   
  dbms_output.put_line('paso 2.3:'||p_cotcompra||' p_cotventa:'||p_cotventa||' p_valor:'||p_valor);
   insert into tcaj_forpago
      (fpa_codeje, fpa_numpago, fpa_sec, fpa_fecha, fpa_sucorg,
       fpa_ofiorg, fpa_modorg, fpa_valor, fpa_monorg, fpa_moddes,
       fpa_cotcompra, fpa_cotventa, fpa_proorg, fpa_tiporg,
       fpa_cuentaorg, fpa_prodes, fpa_tipdes, fpa_cuentades, fpa_clides,
       fpa_mondes, fpa_codcargo, fpa_cargo,fpa_numsol,fpa_transa,fpa_traorg,
       fpa_deptoORG, fpa_valorpenalizado,FPA_SUCDES, FPA_OFIDES)
   values
      (p_operador,p_numpago,p_sec,p_fecha,p_suc,
       p_ofi,p_modorg,p_valor,p_monorg,p_moddes,
       p_cotcompra,p_cotventa,p_proorg,p_tiporg,
       p_cuenta,p_prodes,p_tipdes,p_cuentades,p_cliente,
       p_mondes,p_codcargo,p_cargo,p_numsol,p_transa,p_traorg,
       p_depto, p_valorpenalizado,p_suc,p_ofi);
  dbms_output.put_line('paso 2.4');
EXCEPTION
WHEN OTHERS THEN
 raise_application_error(-20901,'ERROR EN TCAJ_FORPAGO '||sqlerrm);
END;
--credito para ach
PROCEDURE realiza_credito(P_CUENTA IN NUMBER,
                          P_SUCURSAL IN NUMBER,
                          P_OFICINA IN NUMBER,
                          P_DEPTO IN NUMBER,
                          P_REF  IN NUMBER,
                          P_DIFERIDO IN VARCHAR2,
                          P_OPERADOR IN NUMBER,
                          p_usrsecuencia IN OUT  NUMBER,
                          P_NUMPAGO IN NUMBER,
                          P_SEC IN OUT  NUMBER,
                          P_MOD IN NUMBER,
                          P_PRO IN NUMBER,
                          P_TIP IN NUMBER,
                          P_MON IN NUMBER,
                          P_TRA IN NUMBER,
                          P_RUBRO IN NUMBER,
                          P_VAL IN NUMBER) IS
BEGIN
  INS_TRAMON(P_SUCURSAL,P_OFICINA,P_OPERADOR,P_DEPTO,P_REF,
                  p_DIFERIDO,p_usrsecuencia,P_SEC,P_MOD,P_PRO,P_TIP,P_CUENTA,
				  P_MON,P_TRA,P_RUBRO,NULL,P_VAL,NULL,'C');
EXCEPTION
 WHEN OTHERS THEN
       	raise_application_error(-20901,'ErrCredctas.'||sqlerrm);
END;
--fin de ach
PROCEDURE realiza_debitos(P_CUENTA IN NUMBER,
                                             P_SUCURSAL IN NUMBER,
                                             P_OFICINA IN NUMBER,
                                             P_DEPTO IN NUMBER,
                                             P_REF  IN NUMBER,
                                             P_DIFERIDO IN VARCHAR2,
                                             P_OPERADOR IN NUMBER,
                                             p_usrsecuencia IN OUT  NUMBER,
                                             P_NUMPAGO IN NUMBER,
                                             P_SEC IN OUT  NUMBER) IS
BEGIN
     dbms_output.put_line('paso 5.1'||P_OPERADOR||' P_NUMPAGO:'||P_NUMPAGO);
     UPDATE tcaj_forpago
        SET fpa_cuentades = P_OPERADOR --si e debito no debe ser parte del cuadre de caja
      WHERE fpa_codeje    = P_OPERADOR
        AND fpa_numpago    = P_NUMPAGO
        AND fpa_codusr IS NULL
        AND fpa_numtran IS NULL
        AND fpa_cuentades IS NULL;
   dbms_output.put_line('paso 5.1');
     BEGIN
      upd_transfer
				(	p_sucursal,
 					p_oficina,
 					p_operador,
 					p_depto,
 					p_ref,
 					p_diferido,
 					p_operador,
 					3,
 					'C',
 					p_usrsecuencia,--NUMTRAN
 					p_sec,
 					'N');
   dbms_output.put_line('paso 5.2');
     UPDATE tcaj_forpago
        SET fpa_cuentades = NULL
      WHERE fpa_codeje    = p_operador
     	AND fpa_numpago      = p_numpago
   		AND fpa_codusr IS NOT NULL
   		AND fpa_numtran IS NOT NULL
   		AND fpa_cuentades IS NOT NULL;
   dbms_output.put_line('paso 5.3');
		P_sec:= ver_sec(f_fechAtrabajo,p_operador,p_usrsecuencia)-1;
   dbms_output.put_line('paso 5.4');
EXCEPTION
 WHEN OTHERS THEN
       	raise_application_error(-20901,'Errdebctas.'||sqlerrm);
END;
END;
PROCEDURE inserta_tcap_tramon(P_CUENTA IN NUMBER,
                              P_OPERADOR IN NUMBER,
                              P_SECUENCIA IN NUMBER,
                              P_TRANSACCION IN OUT NUMBER,
                              P_DIFERIDO IN VARCHAR2,
                              P_VALOR IN NUMBER,
                              P_CLIENTE IN NUMBER) IS
  secuencia number(3);
  aplico    varchar2(1);
  par_desglo pkg_tablas_sql.grp_desglo;
  par_denom  pkg_tablas_sql.grp_denom;
  suc NUMBER;
  ofi NUMBER;
  p_aplico VARCHAR2(1);
BEGIN
  BEGIN
     dbms_output.put_line('paso 6.1');
     secuencia := ver_sec(f_fechatrabajo,p_operador,p_secuencia)-1;
     dbms_output.put_line('paso 6.2:'||p_transaccion);
     P_TRANSACCION := 16;
     inserta_tcap_tramon_imp(P_operador,P_secuencia,
                         p_transaccion,
                         P_diferido,P_VALOR,
                         secuencia,p_cliente,p_aplico);
     dbms_output.put_line('paso 6.3:'||secuencia||' p_aplico:'||p_aplico);
       SELECT vis_suc,vis_ofi
         INTO suc,ofi
		 FROM  tcap_vista
		WHERE vis_numcue = P_CUENTA;
     dbms_output.put_line('paso 6.4:'||p_operador||' p_secuencia:'||p_secuencia);
     --se comenta por mail de ramon batista sobre intersucursales se revisa y de acuerdo
     --a como tienen definido la contabilida con procecard no es necesario esta parte
     
       /*UPDATE tcap_tramon
	      SET    tmo_succue= suc,
		         tmo_oficue= ofi
        WHERE  tmo_Fechproc=  f_fechatrabajo
		  AND  tmo_codusr = p_operador
		  AND  tmo_numtra = p_secuencia
		  AND  tmo_codmod = 3
		  AND  tmo_codtra = 4
		  AND  tmo_fechcon=pkghits.fechcon_fin(f_fechatrabajo); --HITS*/
		  dbms_output.put_line('paso 6.5');
      EXCEPTION
       WHEN no_data_found THEN
           NULL;
       WHEN OTHERS THEN
	     raise_application_error(-20901,'Error  en la cuenta '||P_CUENTA||sqlerrm);
       END;
EXCEPTION
 WHEN OTHERS THEN
    raise_application_error(-20901,'Error en inserción de Tramon. Revise...!! '||substr(sqlerrm,12,100));
END;
PROCEDURE INSERTA_TCAP_TRAMONAUX(p_operador IN NUMBER,
							     p_secuencia IN NUMBER,
								 p_sucursal IN NUMBER,
                                 p_oficina IN NUMBER,
                                 p_diferido  IN VARCHAR2,
                                 p_modulo IN NUMBER,
                                 p_producto IN NUMBER,
                                 p_tipo IN NUMBER,
                                 p_mon IN NUMBER,
                                 p_transaccion  IN NUMBER,
                                 p_rubro IN NUMBER,
                                 p_depto IN NUMBER,
                                 P_REF IN VARCHAR2,
                                 P_TIPOTRA IN VARCHAR2,
                                 P_VALOR IN NUMBER,
                                 P_tiporub IN VARCHAR2,
                                 P_CUENTA IN NUMBER,
                                 p_autorizador IN NUMBER,
                                 p_terminal IN VARCHAR2) IS
     errtxt VARCHAR2(60);
     fechacon date;
     fechareal date;
     fechavig date;
BEGIN
     fechacon:=ver_fecha(P_sucursal,P_oficina,P_diferido,F_FECHATRABAJO);--:w_fechaproc);
     fechareal:=sysdate;
BEGIN
         ver_def_contable(p_modulo,p_producto,p_tipo,p_mon,p_transaccion,p_rubro,fechacon,fechavig);
END;
 BEGIN
      ins_tramonaux
        (  P_operador,P_secuencia,P_rubro,P_transaccion,
           P_modulo,P_producto,P_tipo,P_mon,
           P_rubro,fechavig,P_sucursal,P_oficina,
           fechareal,fechacon,
           f_fechatrabajo,p_depto,p_ref,p_tipotra,
           p_valor,p_tiporub, P_operador,null,null,null,
           p_autorizador,p_terminal);
           --p_numsecuencia :=p_rubro;
EXCEPTION
 WHEN OTHERS THEN
   errtxt:=substr(sqlerrm,12,45);
   raise_application_error(-20901,'4 TRX NO DEF. CORREC.'||errtxt);
END;
END;
PROCEDURE INSERTA_TCAP_PAGOSERVICIO( p_tarjeta IN VARCHAR2,
									         p_tipopago  IN VARCHAR2,
									         p_valor IN NUMBER,
									         p_codigoserv IN NUMBER,
                                             p_sucursal IN NUMBER,
                                             p_oficina IN NUMBER,
                                             p_operador IN NUMBER,
                                             p_secuencia IN OUT NUMBER,
                                             p_diferido IN VARCHAR2,
                                             P_DEPTO IN NUMBER,
                                             P_TERMINAL IN VARCHAR2,
                                             P_SEC IN OUT NUMBER,
                                             p_parametro3 IN VARCHAR2,
                                             p_parametro4 IN VARCHAR2) IS
  workdate  DATE := null;
  ValorAcreditar number;
  Fechacredita date;
  arrctrl  caj_pagoservicios.tabctrl;
  regctrl  caj_pagoservicios.regctrl;
  v_acredita varchar2(1):=null;
  sec number:=null;
  secuencia number;
  p_acredita varchar2(1);
  P_CTA NUMBER:=NULL;
BEGIN
  --secuencia:=numtran;
  workdate := f_fechareal;
  Fechacredita := workdate;
  IF P_diferido = 'S' THEN
  	 Fechacredita := workdate+1;
  END IF;
 BEGIN
  	caj_pagoservicios.asignar(arrctrl);
  	IF caj_pagoservicios.ultimo = 1 THEN
  		 caj_pagoservicios.obtener_registro(1,regctrl);
  		 caj_pagoservicios.actualiza(1,regctrl.cuenta_algoritmo,regctrl.parametro,P_Valor,P_parametro3,P_parametro4);
  	END IF;
     p_acredita:=null;
	 pkg_servicios_especiales.pago_servicios(p_sucursal,p_oficina,p_operador,
 			              p_depto,p_terminal,TO_NUMBER(p_CTA),p_diferido,
 			              p_codigoserv,p_VALor,p_secuencia,p_sec);
		    INSERT
		      INTO tcap_pagoservicios( PGS_OPERADOR, PGS_NUMTRAN,PGS_CUENTA,
		                               PGS_PARAMETRO, PGS_PARAMETRO2,PGS_FECHA, PGS_MONTO,PGS_LUGAR,PGS_OFICINA,
		                               PGS_CODIGO,PGS_FECHAPLI,PGS_ACREDITAR,PGS_PARAMETRO3,PGS_PARAMETRO4)
		      VALUES(P_operador,P_secuencia,TO_NUMBER(P_cta),
		             p_tarjeta,p_tipopago,WORKDATE,p_valor,p_sucursal,p_oficina,
		             p_codigoserv,Fechacredita,p_acredita,p_parametro3,p_parametro4);
EXCEPTION
 WHEN OTHERS THEN
   raise_application_error(-20901,'NO PUEDE GRABAR PAGO DE TARJETA: '||SQLERRM);
END;
END;
PROCEDURE ENVIA_MENSAJE(P_mensaje IN VARCHAR2,
						P_ServRegistroOK IN OUT VARCHAR2,
						P_ServMsgError IN OUT VARCHAR2) IS
 RESPUESTA VARCHAR2(100);
 PATH_INTERFAZ TGEN_PARINST.PAI_VALOR%TYPE;
BEGIN
	BEGIN
	SELECT PAI_VALOR
	  INTO PATH_INTERFAZ
	 FROM TGEN_PARINST
	 WHERE PAI_CODVAR='CMS_PATH_INTERFAZ';
	EXCEPTION
		WHEN OTHERS THEN
		 PATH_INTERFAZ :='C:\MQSERIES.EXE ';
	END;
	   --host(PATH_INTERFAZ ||' ' ||mensaje); envia mensajes
EXCEPTION
  WHEN OTHERS THEN
   p_ServRegistroOK := '1';
   p_ServMsgError :=sqlerrm;

END;
--para transmitir el mensaje
--RECIBE MENSAJES
PROCEDURE RECIBE_MENSAJE(p_operador IN NUMBER,
                         p_numtra IN NUMBER,
                         p_sec IN NUMBER,
                         p_mensajes IN OUT NUMBER,
                         p_error OUT VARCHAR,
                         p_descerror OUT VARCHAR,
                         P_ServRegistroOK IN OUT VARCHAR2,
						 P_ServMsgError IN OUT VARCHAR2 ) IS
  ConvID  PLS_INTEGER;
  AppID  PLS_INTEGER;
  Buffer   VARCHAR2(80);
  RESPUESTA VARCHAR2(100);
  V_CODEREQUEST   trequestcredit.CMS_CODEREQUEST%TYPE;
  V_DESCRIPTION  	trequestcredit.CMS_DESCRIPTION%TYPE;
  P_LLEGARON NUMBER:=0;
  CMS_TIEMPO TGEN_PARINST.PAI_VALOR%TYPE;
BEGIN
BEGIN
	SELECT PAI_VALOR
	   INTO CMS_TIEMPO
	  FROM TGEN_PARINST
	 WHERE PAI_CODVAR = 'CMS_TIEMPO_ESPERA_RESP_MAS';
EXCEPTION
	WHEN OTHERS THEN
	   CMS_TIEMPO := '10';
END;
--CMS_TIEMPO:=10;
	P_LLEGARON :=0;
	FOR i IN 1..TO_NUMBER(CMS_TIEMPO) LOOP
	PKG_CMS_INTERFAZCREDITCARD.PCMS_REQUEST(p_operador,p_numtra,p_sec,P_LLEGARON,V_CODEREQUEST,V_DESCRIPTION);
	p_error := V_CODEREQUEST;
    p_descerror := V_DESCRIPTION;
	END LOOP;
	IF nvl(P_LLEGARON,0) = 0 THEN
        p_ServRegistroOK := '1';
      	p_error := '1';
 	    p_ServMsgError :='Respuesta no Recibida Desde Procecard.....';
	END IF;
EXCEPTION
 WHEN OTHERS THEN
  p_ServRegistroOK := '1';
  p_ServMsgError :=sqlerrm;
END;
--FIN DE RECIBE MENSAJES
--REVERSO
PROCEDURE REVERSO(P_OPERADOR IN NUMBER,
                  P_NUMTRA IN NUMBER,
                  P_SEC IN NUMBER,
                  MENSAJE OUT VARCHAR2) IS
 fecha DATE;
 CURSOR REVERSO IS
   SELECT TMO_CODUSR,TMO_NUMTRA,TMO_SEC
     FROM TCAP_TRAMON
    WHERE TMO_CODUSR = P_OPERADOR
      AND TMO_NUMTRA = P_NUMTRA
      AND TMO_CODTRA <> 4
      AND TMO_FECHPROC = TRUNC(F_FECHATRABAJO)
      AND TMO_SEC = P_SEC;
	  CMS_AFECTACION_LINEA_PAGO_TC TGEN_PARINST.PAI_CODVAR%TYPE:='N';
	  p_tramareverso VARCHAR2(114);
	  P_NUMENSAJES NUMBER:=0;
	  p_errorrec   VARCHAR2(1);
	  p_descerrorrec trequestcredit.CMS_DESCRIPTION%TYPE;
BEGIN
 FOR X IN REVERSO LOOP
	  SELECT CMS_IDMESSAGE||CMS_IDRESPOSE||CMS_DATESYSTEM||CMS_HOURSYSTEM||
           CMS_USER||CMS_CREDIT||CMS_CURRENCY||CMS_REFERENCE||CMS_DATETRANSACTION||CMS_ORIGMOVEMENT||
           CMS_MODEPAYMENT||CMS_TYPELINE||CMS_CODELINE||CMS_OFFICE||CMS_VALUE||'R'
      INTO p_tramareverso
      FROM tresponsecredit
     WHERE TO_NUMBER(CMS_IDRESPOSE) = X.TMO_CODUSR||X.TMO_NUMTRA||X.TMO_SEC;
     MENSAJE := p_tramareverso;
   --ENVIA_MENSAJE(p_tramareverso);
 END LOOP;
 EXCEPTION
   WHEN OTHERS THEN
	  null;
END;
--FIN DE REVERSO
PROCEDURE VALIDA_TRANSACCION(P_OPERADOR       IN NUMBER,
						     P_NUMTRA         IN NUMBER,
						     P_MENSAJE OUT VARCHAR2,
						     P_ServRegistroOK IN OUT VARCHAR2,
						     P_ServMsgError   IN OUT VARCHAR2,
						     P_canal IN NUMBER,
						     P_CURRENCY IN NUMBER)  is
  fecha date;
  pcms_responses  VARCHAR2(4000);
  pcms_undoresponses VARCHAR2(4000);
  --p_error VARCHAR2(1):='0';
  p_msgerror VARCHAR2(1000);
  p_codusr  tcap_tramon.tmo_codusr%type;
  --p_numtra tcap_tramon.tmo_numtra%type;
  p_fechacon tcap_tramon.tmo_fechcon%type;
  p_error VARCHAR2(1):='0';
  --pcms_undoresponses varchar2(500);
  CURSOR TRAMON(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
    SELECT /*+index CP01CAP_TMO*/
           TO_CHAR(A.TMO_FECHPROC,'YYYYMMDD') DATESYSTEM,
            TO_CHAR(A.TMO_FECHOR,'HH24MISS')   HOURSYSTEM,
            A.TMO_CODUSR USR,
            A.TMO_NUMTRA NUMTRAN,
            B.PGS_PARAMETRO CREDIT,
            A.TMO_CODMON CURRENCY,
            A.TMO_NUMTRA REFERENCE,
            --A.TMO_FECHCON DATETRANSACTION,
            A.TMO_FECHPROC DATETRANSACTION,            
            A.TMO_CODTRA  CODTRA,
            A.TMO_CODMOD CODMOD,
            A.TMO_SEC SEC,
            A.TMO_RUBRO MODEPAYMENT,
            '501' TABTIPPAGO,
            B.PGS_PARAMETRO3    TIPAG,
            '502' TABTIPLIN,
            B.PGS_PARAMETRO4  TIPLIN,
            A.TMO_CODSUC  OFFICE,
            B.PGS_MONTO  VALUE, --A.TMO_VAL
            DECODE(A.TMO_MODO,'N','C',A.TMO_MODO) ID_TRANSACTION
      FROM TCAP_TRAMON A ,TCAP_PAGOSERVICIOS B,TDOM_TRARUBROENVIAPAGOS C
      WHERE A.TMO_CODUSR = B.PGS_OPERADOR
        AND A.TMO_NUMTRA = B.PGS_NUMTRAN                                
        AND A.TMO_CODMOD = C.TRP_MOD
        AND A.TMO_CODTRA = C.TRP_TRA
        AND A.TMO_RUBRO = C.TRP_RUBRO                
        AND A.TMO_CODUSR = CODUSR
        AND A.TMO_NUMTRA = NUMTRA
        AND A.TMO_CODTRA <> 4
        AND A.TMO_MODO = 'N'
    ORDER BY A.TMO_SEC;
	CURSOR REVERSOTRA(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
   SELECT TMO_CODUSR,TMO_NUMTRA,TMO_SEC
     FROM TCAP_TRAMON  A,TDOM_TRARUBROENVIAPAGOS C
    WHERE TMO_CODUSR = CODUSR
      AND TMO_NUMTRA = NUMTRA
      AND A.TMO_CODMOD = C.TRP_MOD
      AND A.TMO_CODTRA = C.TRP_TRA
      AND A.TMO_RUBRO = C.TRP_RUBRO                    
      AND A.TMO_CODTRA <> 4
      AND TMO_FECHPROC = TRUNC(F_FECHATRABAJO)
      ORDER BY TMO_SEC;
	  CMS_AFECTACION_LINEA_PAGO_TC TGEN_PARINST.PAI_CODVAR%TYPE:='N';
	  P_NUMENSAJES NUMBER:=0;
	  p_errorrec   VARCHAR2(1);
	  p_descerrorrec trequestcredit.CMS_DESCRIPTION%TYPE;
BEGIN
  BEGIN
	SELECT PAI_VALOR
	  INTO CMS_AFECTACION_LINEA_PAGO_TC
	 FROM TGEN_PARINST
	 WHERE PAI_CODVAR = 'CMS_AFECTACION_LINEA_PAGO_TC';
  EXCEPTION
	WHEN OTHERS THEN
	  CMS_AFECTACION_LINEA_PAGO_TC :='N';
  END;
    P_NUMENSAJES :=0;
 	p_errorrec   := '0';
	p_descerrorrec := null;
    P_ServRegistroOK :=  '0'; --hd6104
    P_ServMsgError   :=  null; --hd6104
  IF CMS_AFECTACION_LINEA_PAGO_TC ='S' THEN
    FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
    save_log('paso 1:'||p_OPERADOR||' p_numtra:'||p_numtra||' CMS_AFECTACION_LINEA_PAGO_TC:'||CMS_AFECTACION_LINEA_PAGO_TC||' P_ServRegistroOK:'||P_ServRegistroOK);
    --PKG_CMS_INTERFAZCREDITCARD.PCMS_RESPONSE_MM(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal );
    PCMS_RESPONSE_MM(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal,P_CURRENCY );    
    save_log('paso 2:'||P_ServRegistroOK);
    --LLAMAR AQUI A PROCESO DE MQSERIES
    -- Para que la transaccion se grabe en Fisa System debe ir con '0'
    P_ServRegistroOK :=  p_error; --hd6104
    P_ServMsgError   :=  p_msgerror; --hd6104
    IF P_ServRegistroOK = '0' THEN
  	--LLAMARA A INTERFAZ DE MQSERIES
  	--ENVIA_MENSAJE(pcms_responses,P_ServRegistroOK,P_ServMsgError);
     P_MENSAJE := pcms_responses;
  	 P_NUMENSAJES := P_NUMENSAJES + 1;
    END IF;
    --esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto  puede haber de una trama
   END LOOP ;
 ELSE
  --p_ServRegistroOK := '1';--PARA QUE SIEMPRE FUNCIONE EN LINEA
    FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
    --PKG_CMS_INTERFAZCREDITCARD.PCMS_RESPONSE(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal );
      PCMS_RESPONSE_MM(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal,P_CURRENCY );    
    save_log('paso 2:'||P_ServRegistroOK);
    --LLAMAR AQUI A PROCESO DE MQSERIES
    -- Para que la transaccion se grabe en Fisa System debe ir con '0'
    P_ServRegistroOK :=  p_error; --hd6104
    P_ServMsgError   :=  p_msgerror; --hd6104
    --esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto  puede haber de una trama
   END LOOP ;
 END IF;
  ---en observacion este commit
     --commit;
   --fin de observación
 END;--FIN DE VALIDA PARA ENVIAR MENSAJE
/*-------------------------------VALIDA TRANSACCION MASIVA---------------------*/
PROCEDURE VALIDA_TRANSACCION_MAS(P_OPERADOR       IN NUMBER,
						     P_NUMTRA         IN NUMBER,
						     P_MENSAJE OUT VARCHAR2,
						     P_ServRegistroOK IN OUT VARCHAR2,
						     P_ServMsgError   IN OUT VARCHAR2,
						     P_canal IN NUMBER,
						     P_CURRENCY IN NUMBER)  is
  fecha date;
  pcms_responses  VARCHAR2(4000);
  pcms_undoresponses VARCHAR2(4000);
  --p_error VARCHAR2(1):='0';
  p_msgerror VARCHAR2(1000);
  p_codusr  tcap_tramon.tmo_codusr%type;
  --p_numtra tcap_tramon.tmo_numtra%type;
  p_fechacon tcap_tramon.tmo_fechcon%type;
  p_error VARCHAR2(1):='0';
  --pcms_undoresponses varchar2(500);
  CURSOR TRAMON(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
    SELECT /*+index CP01CAP_TMO*/
           TO_CHAR(A.TMO_FECHPROC,'YYYYMMDD') DATESYSTEM,
            TO_CHAR(A.TMO_FECHOR,'HH24MISS')   HOURSYSTEM,
            A.TMO_CODUSR USR,
            A.TMO_NUMTRA NUMTRAN,
            B.PGS_PARAMETRO CREDIT,
            A.TMO_CODMON CURRENCY,
            A.TMO_NUMTRA REFERENCE,
            A.TMO_FECHCON DATETRANSACTION,
            A.TMO_CODTRA  CODTRA,
            A.TMO_CODMOD CODMOD,
            A.TMO_SEC SEC,
            A.TMO_RUBRO MODEPAYMENT,
            '501' TABTIPPAGO,
            B.PGS_PARAMETRO3    TIPAG,
            '502' TABTIPLIN,
            B.PGS_PARAMETRO4  TIPLIN,
            A.TMO_CODSUC  OFFICE,
            B.PGS_MONTO  VALUE, --A.TMO_VAL
            DECODE(A.TMO_MODO,'N','C',A.TMO_MODO) ID_TRANSACTION
      FROM TCAP_TRAMON A ,TCAP_PAGOSERVICIOS B,TDOM_TRARUBROENVIAPAGOS C
      WHERE A.TMO_CODUSR = B.PGS_OPERADOR
        AND A.TMO_NUMTRA = B.PGS_NUMTRAN                                
        AND A.TMO_CODMOD = C.TRP_MOD
        AND A.TMO_CODTRA = C.TRP_TRA
        AND A.TMO_RUBRO = C.TRP_RUBRO                
        AND A.TMO_CODUSR = CODUSR
        AND A.TMO_NUMTRA = NUMTRA
        AND A.TMO_CODTRA <> 4
        AND A.TMO_MODO = 'N'
    ORDER BY A.TMO_SEC;
	CURSOR REVERSOTRA(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
   SELECT TMO_CODUSR,TMO_NUMTRA,TMO_SEC
     FROM TCAP_TRAMON  A,TDOM_TRARUBROENVIAPAGOS C
    WHERE TMO_CODUSR = CODUSR
      AND TMO_NUMTRA = NUMTRA
      AND A.TMO_CODMOD = C.TRP_MOD
      AND A.TMO_CODTRA = C.TRP_TRA
      AND A.TMO_RUBRO = C.TRP_RUBRO                    
      AND A.TMO_CODTRA <> 4
      AND TMO_FECHPROC = TRUNC(F_FECHATRABAJO)
      ORDER BY TMO_SEC;
	  CMS_AFECTACION_LINEA_PAGO_TC TGEN_PARINST.PAI_CODVAR%TYPE:='N';
	  P_NUMENSAJES NUMBER:=0;
	  p_errorrec   VARCHAR2(1);
	  p_descerrorrec trequestcredit.CMS_DESCRIPTION%TYPE;
BEGIN
  BEGIN
	SELECT PAI_VALOR
	  INTO CMS_AFECTACION_LINEA_PAGO_TC
	 FROM TGEN_PARINST
	 WHERE PAI_CODVAR = 'CMS_AFECTACION_LINEA_PAGO_TC';
  EXCEPTION
	WHEN OTHERS THEN
	  CMS_AFECTACION_LINEA_PAGO_TC :='N';
  END;
    P_NUMENSAJES :=0;
 	p_errorrec   := '0';
	p_descerrorrec := null;
    P_ServRegistroOK :=  '0'; --hd6104
    P_ServMsgError   :=  null; --hd6104
  IF CMS_AFECTACION_LINEA_PAGO_TC ='S' THEN
    FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
    save_log('paso 1:'||p_OPERADOR||' p_numtra:'||p_numtra||' CMS_AFECTACION_LINEA_PAGO_TC:'||CMS_AFECTACION_LINEA_PAGO_TC||' P_ServRegistroOK:'||P_ServRegistroOK);
    --PKG_CMS_INTERFAZCREDITCARD.PCMS_RESPONSE_MM(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal );
    PCMS_RESPONSE_MM_MAS(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal,P_CURRENCY );    
    save_log('paso 2:'||P_ServRegistroOK);
    --LLAMAR AQUI A PROCESO DE MQSERIES
    -- Para que la transaccion se grabe en Fisa System debe ir con '0'
    P_ServRegistroOK :=  p_error; --hd6104
    P_ServMsgError   :=  p_msgerror; --hd6104
    IF P_ServRegistroOK = '0' THEN
  	--LLAMARA A INTERFAZ DE MQSERIES
  	--ENVIA_MENSAJE(pcms_responses,P_ServRegistroOK,P_ServMsgError);
     P_MENSAJE := pcms_responses;
  	 P_NUMENSAJES := P_NUMENSAJES + 1;
    END IF;
    --esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto  puede haber de una trama
   END LOOP ;
 ELSE
  --p_ServRegistroOK := '1';--PARA QUE SIEMPRE FUNCIONE EN LINEA
    FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
    --PKG_CMS_INTERFAZCREDITCARD.PCMS_RESPONSE(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal );
      PCMS_RESPONSE_MM_MAS(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal,P_CURRENCY );    
    save_log('paso 2:'||P_ServRegistroOK);
    --LLAMAR AQUI A PROCESO DE MQSERIES
    -- Para que la transaccion se grabe en Fisa System debe ir con '0'
    P_ServRegistroOK :=  p_error; --hd6104
    P_ServMsgError   :=  p_msgerror; --hd6104
    --esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto  puede haber de una trama
   END LOOP ;
 END IF;
  ---en observacion este commit
     --commit;
   --fin de observación
 END;--FIN DE VALIDA PARA ENVIAR MENSAJE
/*------------------------------FIN TRX MASIVO---------------------------------*/
 --VALIDA TRANSACCION PARA TIENDAS ANTHONY'S O CUALQUIER CONVENIO
 PROCEDURE VALIDA_TRANSACCION_ANT(P_OPERADOR       IN NUMBER,
						     P_NUMTRA         IN NUMBER,
						     P_FORMAPAGO IN NUMBER, --EFECTIVO O CHEQUE
						     P_MENSAJE OUT VARCHAR2,
						     P_ServRegistroOK IN OUT VARCHAR2,
						     P_ServMsgError   IN OUT VARCHAR2,
						     P_canal IN NUMBER,
						     P_CURRENCY IN NUMBER)  is
  fecha date;
  pcms_responses  VARCHAR2(4000);
  pcms_undoresponses VARCHAR2(4000);
  --p_error VARCHAR2(1):='0';
  p_msgerror VARCHAR2(1000);
  p_codusr  tcap_tramon.tmo_codusr%type;
  --p_numtra tcap_tramon.tmo_numtra%type;
  p_fechacon tcap_tramon.tmo_fechcon%type;
  p_error VARCHAR2(1):='0';
  --pcms_undoresponses varchar2(500);
  CURSOR TRAMON(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
    SELECT /*+index CP01CAP_TMO*/
           TO_CHAR(A.TMO_FECHPROC,'YYYYMMDD') DATESYSTEM,
            TO_CHAR(A.TMO_FECHOR,'HH24MISS')   HOURSYSTEM,
            A.TMO_CODUSR USR,
            A.TMO_NUMTRA NUMTRAN,
            B.PGS_PARAMETRO CREDIT,
            A.TMO_CODMON CURRENCY,
            A.TMO_NUMTRA REFERENCE,
            A.TMO_FECHCON DATETRANSACTION,
            A.TMO_CODTRA  CODTRA,
            A.TMO_CODMOD CODMOD,
            A.TMO_SEC SEC,
            A.TMO_RUBRO MODEPAYMENT,
            '501' TABTIPPAGO,
            B.PGS_PARAMETRO3    TIPAG,
            '502' TABTIPLIN,
            B.PGS_PARAMETRO4  TIPLIN,
            A.TMO_CODSUC  OFFICE,
            B.PGS_MONTO  VALUE, --A.TMO_VAL
            DECODE(A.TMO_MODO,'N','C',A.TMO_MODO) ID_TRANSACTION
      FROM TCAP_TRAMON A ,TCAP_PAGOSERVICIOS B,TDOM_TRARUBROENVIAPAGOS C
      WHERE A.TMO_CODUSR = B.PGS_OPERADOR
        AND A.TMO_NUMTRA = B.PGS_NUMTRAN                                
        AND A.TMO_CODMOD = C.TRP_MOD
        AND A.TMO_CODTRA = C.TRP_TRA
        AND A.TMO_RUBRO = C.TRP_RUBRO                
        AND A.TMO_CODUSR = CODUSR
        AND A.TMO_NUMTRA = NUMTRA
        AND A.TMO_CODTRA <> 4
        AND A.TMO_MODO = 'N'
    ORDER BY A.TMO_SEC;
	CURSOR REVERSOTRA(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
   SELECT TMO_CODUSR,TMO_NUMTRA,TMO_SEC
     FROM TCAP_TRAMON  A,TDOM_TRARUBROENVIAPAGOS C
    WHERE TMO_CODUSR = CODUSR
      AND TMO_NUMTRA = NUMTRA
      AND A.TMO_CODMOD = C.TRP_MOD
      AND A.TMO_CODTRA = C.TRP_TRA
      AND A.TMO_RUBRO = C.TRP_RUBRO                    
      AND A.TMO_CODTRA <> 4
      AND TMO_FECHPROC = TRUNC(F_FECHATRABAJO)
      ORDER BY TMO_SEC;
	  CMS_AFECTACION_LINEA_PAGO_TC TGEN_PARINST.PAI_CODVAR%TYPE:='N';
	  P_NUMENSAJES NUMBER:=0;
	  p_errorrec   VARCHAR2(1);
	  p_descerrorrec trequestcredit.CMS_DESCRIPTION%TYPE;
BEGIN
  BEGIN
	SELECT PAI_VALOR
	  INTO CMS_AFECTACION_LINEA_PAGO_TC
	 FROM TGEN_PARINST
	 WHERE PAI_CODVAR = 'CMS_AFECTACION_LINEA_PAGO_TC';
  EXCEPTION
	WHEN OTHERS THEN
	  CMS_AFECTACION_LINEA_PAGO_TC :='N';
  END;
    P_NUMENSAJES :=0;
 	p_errorrec   := '0';
	p_descerrorrec := null;
    P_ServRegistroOK :=  '0'; --hd6104
    P_ServMsgError   :=  null; --hd6104
  IF CMS_AFECTACION_LINEA_PAGO_TC ='S' THEN
    FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
    save_log('paso 1:'||p_OPERADOR||' p_numtra:'||p_numtra||' CMS_AFECTACION_LINEA_PAGO_TC:'||CMS_AFECTACION_LINEA_PAGO_TC||' P_ServRegistroOK:'||P_ServRegistroOK);
    --PKG_CMS_INTERFAZCREDITCARD.PCMS_RESPONSE_MM(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal );
    PCMS_RESPONSE_MM_ANT(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,P_FORMAPAGO,pcms_responses,p_canal,P_CURRENCY );    
    save_log('paso 2:'||P_ServRegistroOK);
    --LLAMAR AQUI A PROCESO DE MQSERIES
    -- Para que la transaccion se grabe en Fisa System debe ir con '0'
    P_ServRegistroOK :=  p_error; --hd6104
    P_ServMsgError   :=  p_msgerror; --hd6104
    IF P_ServRegistroOK = '0' THEN
  	--LLAMARA A INTERFAZ DE MQSERIES
  	--ENVIA_MENSAJE(pcms_responses,P_ServRegistroOK,P_ServMsgError);
     P_MENSAJE := pcms_responses;
  	 P_NUMENSAJES := P_NUMENSAJES + 1;
    END IF;
    --esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto  puede haber de una trama
   END LOOP ;
 ELSE
  --p_ServRegistroOK := '1';--PARA QUE SIEMPRE FUNCIONE EN LINEA
    FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
    --PKG_CMS_INTERFAZCREDITCARD.PCMS_RESPONSE(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal );
      PCMS_RESPONSE_MM_ANT(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,P_FORMAPAGO,pcms_responses,p_canal,P_CURRENCY );    
    save_log('paso 2:'||P_ServRegistroOK);
    --LLAMAR AQUI A PROCESO DE MQSERIES
    -- Para que la transaccion se grabe en Fisa System debe ir con '0'
    P_ServRegistroOK :=  p_error; --hd6104
    P_ServMsgError   :=  p_msgerror; --hd6104
    --esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto  puede haber de una trama
   END LOOP ;
 END IF;
  ---en observacion este commit
     --commit;
   --fin de observación
 END;--FIN DE VALIDA PARA ENVIAR MENSAJE
 --FIN DE VALIDA TRA CON TIENAS
 
 --VALIDA TRA ACH
PROCEDURE VALIDA_TRANSACCION_ACH(P_OPERADOR       IN NUMBER,
						     P_NUMTRA         IN NUMBER,
						     P_MENSAJE OUT VARCHAR2,
						     P_ServRegistroOK IN OUT VARCHAR2,
						     P_ServMsgError   IN OUT VARCHAR2,
						     P_canal IN NUMBER)  is
  fecha date;
  pcms_responses  VARCHAR2(4000);
  pcms_undoresponses VARCHAR2(4000);
  --p_error VARCHAR2(1):='0';
  p_msgerror VARCHAR2(1000);
  p_codusr  tcap_tramon.tmo_codusr%type;
  --p_numtra tcap_tramon.tmo_numtra%type;
  p_fechacon tcap_tramon.tmo_fechcon%type;
  p_error VARCHAR2(1):='0';
  --pcms_undoresponses varchar2(500);
  CURSOR TRAMON(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
    SELECT /*+index CP01CAP_TMO*/
           TO_CHAR(A.TMO_FECHPROC,'YYYYMMDD') DATESYSTEM,
            TO_CHAR(A.TMO_FECHOR,'HH24MISS')   HOURSYSTEM,
            A.TMO_CODUSR USR,
            A.TMO_NUMTRA NUMTRAN,
            B.PGS_PARAMETRO CREDIT,
            A.TMO_CODMON CURRENCY,
            A.TMO_NUMTRA REFERENCE,
            A.TMO_FECHCON DATETRANSACTION,
            A.TMO_CODTRA  CODTRA,
            A.TMO_CODMOD CODMOD,
            A.TMO_SEC SEC,
            A.TMO_RUBRO MODEPAYMENT,
            '501' TABTIPPAGO,
            B.PGS_PARAMETRO3    TIPAG,
            '502' TABTIPLIN,
            B.PGS_PARAMETRO4  TIPLIN,
            A.TMO_CODSUC  OFFICE,
            B.PGS_MONTO  VALUE, --A.TMO_VAL
            DECODE(A.TMO_MODO,'N','C',A.TMO_MODO) ID_TRANSACTION
      FROM TCAP_TRAMON A ,TCAP_PAGOSERVICIOS B,TDOM_TRARUBROENVIAPAGOS C
      WHERE A.TMO_CODUSR = B.PGS_OPERADOR
        AND A.TMO_NUMTRA = B.PGS_NUMTRAN       
        AND A.TMO_CODMOD = C.TRP_MOD
        AND A.TMO_CODTRA = C.TRP_TRA
        AND A.TMO_RUBRO = C.TRP_RUBRO        
        AND A.TMO_MODO ='N'        
        AND A.TMO_CODUSR = CODUSR
        AND A.TMO_NUMTRA = NUMTRA
        AND A.TMO_CODTRA <> 4
        AND A.TMO_TIPOTRA = 'D'
    ORDER BY A.TMO_SEC;
	CURSOR REVERSOTRA(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
   SELECT TMO_CODUSR,TMO_NUMTRA,TMO_SEC
     FROM TCAP_TRAMON  A,TDOM_TRARUBROENVIAPAGOS C
    WHERE TMO_CODUSR = CODUSR
      AND TMO_NUMTRA = NUMTRA                     
      AND A.TMO_CODMOD = C.TRP_MOD
      AND A.TMO_CODTRA = C.TRP_TRA
      AND A.TMO_RUBRO = C.TRP_RUBRO              
      AND A.TMO_MODO = 'N'      
      AND TMO_CODTRA <> 4
      AND TMO_TIPOTRA = 'D'
      AND TMO_FECHPROC = TRUNC(F_FECHATRABAJO)
      ORDER BY TMO_SEC;
	  CMS_AFECTACION_LINEA_PAGO_TC TGEN_PARINST.PAI_CODVAR%TYPE:='N';
	  P_NUMENSAJES NUMBER:=0;
	  p_errorrec   VARCHAR2(1);
	  p_descerrorrec trequestcredit.CMS_DESCRIPTION%TYPE;
BEGIN
  BEGIN
	SELECT PAI_VALOR
	  INTO CMS_AFECTACION_LINEA_PAGO_TC
	 FROM TGEN_PARINST
	 WHERE PAI_CODVAR = 'CMS_AFECTACION_LINEA_PAGO_TC';
  EXCEPTION
	WHEN OTHERS THEN
	  CMS_AFECTACION_LINEA_PAGO_TC :='N';
  END;
    P_NUMENSAJES :=0;
 	p_errorrec   := '0';
	p_descerrorrec := null;
    P_ServRegistroOK :=  '0'; --hd6104
    P_ServMsgError   :=  null; --hd6104
  IF CMS_AFECTACION_LINEA_PAGO_TC ='S' THEN
    FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
    save_log('paso 1:'||p_OPERADOR||' p_numtra:'||p_numtra||' CMS_AFECTACION_LINEA_PAGO_TC:'||CMS_AFECTACION_LINEA_PAGO_TC||' P_ServRegistroOK:'||P_ServRegistroOK);
    PKG_CMS_INTERFAZCREDITCARD.PCMS_RESPONSE(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal );
    save_log('paso 2:'||P_ServRegistroOK);
    --LLAMAR AQUI A PROCESO DE MQSERIES
    -- Para que la transaccion se grabe en Fisa System debe ir con '0'
    P_ServRegistroOK :=  p_error; --hd6104
    P_ServMsgError   :=  p_msgerror; --hd6104
    IF P_ServRegistroOK = '0' THEN
  	--LLAMARA A INTERFAZ DE MQSERIES
  	--ENVIA_MENSAJE(pcms_responses,P_ServRegistroOK,P_ServMsgError);
     P_MENSAJE := pcms_responses;
  	 P_NUMENSAJES := P_NUMENSAJES + 1;
    END IF;
    --esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto  puede haber de una trama
   END LOOP ;
 ELSE
  --p_ServRegistroOK := '1';--PARA QUE SIEMPRE FUNCIONE EN LINEA
    FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
    PKG_CMS_INTERFAZCREDITCARD.PCMS_RESPONSE(p_OPERADOR,p_numtra,x.sec,CMS_AFECTACION_LINEA_PAGO_TC,p_error,p_msgerror,pcms_responses,p_canal );
    save_log('paso 2:'||P_ServRegistroOK);
    --LLAMAR AQUI A PROCESO DE MQSERIES
    -- Para que la transaccion se grabe en Fisa System debe ir con '0'
    P_ServRegistroOK :=  p_error; --hd6104
    P_ServMsgError   :=  p_msgerror; --hd6104
    --esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto  puede haber de una trama
   END LOOP ;
 END IF;
  ---en observacion este commit
     --commit;
   --fin de observación
 END;--FIN DE VALIDA PARA ENVIAR MENSAJE
 --FIN VALIDA TRA ACH
 --PARA RECEPCION DEL MENSAJE
 PROCEDURE VALIDA_TRANSACCION_RESP(P_OPERADOR IN NUMBER,--Operador
						     P_NUMTRA IN NUMBER,--numero de transaccion
						     P_MENSAJEREC IN VARCHAR2,--dmensaje de respuesta en este caso desde procecard
						     P_MENSAJESEN OUT VARCHAR2,--mensaje de envio
						     P_ServRegistroOK  OUT VARCHAR2,--1 error 0 todo OK
						     P_ServMsgError  OUT VARCHAR2)  is --si es 1 mensaje de error
  fecha date;
  pcms_responses  VARCHAR2(4000);
  pcms_undoresponses VARCHAR2(4000);
  --p_error VARCHAR2(1):='0';
  p_msgerror VARCHAR2(1000);
  p_codusr  tcap_tramon.tmo_codusr%type;
  --p_numtra tcap_tramon.tmo_numtra%type;
  p_fechacon tcap_tramon.tmo_fechcon%type;
  p_error VARCHAR2(1):='0';
  V_IDMESSAGE                            VARCHAR2(10);
  V_IDRESPOSE                            VARCHAR2(24);
  V_CODEREQUEST                          VARCHAR2(7);
  V_DESCRIPTION                          VARCHAR2(75);
  V_error varchar2(150);
  --pcms_undoresponses varchar2(500);
  CURSOR TRAMON(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
    SELECT /*+index CP01CAP_TMO*/
           TO_CHAR(A.TMO_FECHPROC,'YYYYMMDD') DATESYSTEM,
            TO_CHAR(A.TMO_FECHOR,'HH24MISS')   HOURSYSTEM,
            A.TMO_CODUSR USR,
            A.TMO_NUMTRA NUMTRAN,
            B.PGS_PARAMETRO CREDIT,
            A.TMO_CODMON CURRENCY,
            A.TMO_NUMTRA REFERENCE,
            A.TMO_FECHCON DATETRANSACTION,
            A.TMO_CODTRA  CODTRA,
            A.TMO_CODMOD CODMOD,
            A.TMO_SEC SEC,
            A.TMO_RUBRO MODEPAYMENT,
            '501' TABTIPPAGO,
            B.PGS_PARAMETRO3    TIPAG,
            '502' TABTIPLIN,
            B.PGS_PARAMETRO4  TIPLIN,
            A.TMO_CODSUC  OFFICE,
            B.PGS_MONTO  VALUE, --A.TMO_VAL
            DECODE(A.TMO_MODO,'N','C',A.TMO_MODO) ID_TRANSACTION
      FROM TCAP_TRAMON A ,TCAP_PAGOSERVICIOS B
      WHERE A.TMO_CODUSR = B.PGS_OPERADOR
        AND A.TMO_NUMTRA = B.PGS_NUMTRAN
        AND A.TMO_CODUSR = CODUSR
        AND A.TMO_NUMTRA = NUMTRA
        AND A.TMO_CODTRA <> 4
    ORDER BY A.TMO_SEC;
	CURSOR REVERSOTRA(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
   SELECT TMO_CODUSR,TMO_NUMTRA,TMO_SEC
     FROM TCAP_TRAMON
    WHERE TMO_CODUSR = CODUSR
      AND TMO_NUMTRA = NUMTRA
      AND TMO_CODTRA <> 4
      AND TMO_FECHPROC = TRUNC(F_FECHATRABAJO)
      ORDER BY TMO_SEC;
	  CMS_AFECTACION_LINEA_PAGO_TC TGEN_PARINST.PAI_CODVAR%TYPE:='N';
	  P_NUMENSAJES NUMBER:=0;
	  p_errorrec   VARCHAR2(1);
	  p_descerrorrec trequestcredit.CMS_DESCRIPTION%TYPE;
BEGIN
BEGIN
	SELECT PAI_VALOR
	  INTO CMS_AFECTACION_LINEA_PAGO_TC
	 FROM TGEN_PARINST
	 WHERE PAI_CODVAR = 'CMS_AFECTACION_LINEA_PAGO_TC';
EXCEPTION
	WHEN OTHERS THEN
	  CMS_AFECTACION_LINEA_PAGO_TC :='N';
END;
    P_NUMENSAJES :=0;
 	p_errorrec   := '0';
	p_descerrorrec := null;
    P_ServRegistroOK :=  '0'; --hd6104
    P_ServMsgError   :=  null; --hd6104
    SAVE_LOG(' 1 P_MENSAJEREC:'||P_MENSAJEREC);
    SAVE_LOG(' 2 P_MENSAJESEN:'||P_MENSAJESEN);
  ---INTERPRETO LA TRAMA QUE LLEGO DE RESPUESTA
  BEGIN                                                                                             
    SAVE_LOG(' 2.1 P_MENSAJESEN:'||P_MENSAJESEN);  
  V_IDMESSAGE := RTRIM(LTRIM(SUBSTR(P_MENSAJEREC,1,10)));--VARCHAR2(10);
  V_IDRESPOSE := RTRIM(LTRIM(SUBSTR(P_MENSAJEREC,11,24)));--VARCHAR2(24);
  V_CODEREQUEST  := RTRIM(LTRIM(SUBSTR(P_MENSAJEREC,35,7)));--VARCHAR2(7);
  V_DESCRIPTION  := RTRIM(LTRIM(SUBSTR(P_MENSAJEREC,42,75)));--VARCHAR2(75);
  SAVE_LOG(' 2.2 P_MENSAJESEN:'||P_MENSAJESEN||'^'||V_IDMESSAGE||'^'||V_IDRESPOSE||'^'||V_CODEREQUEST||'^'||V_DESCRIPTION);
   INSERT INTO trequestcredit(CMS_IDMESSAGE,CMS_IDRESPOSE,CMS_CODEREQUEST,CMS_DESCRIPTION)
                       VALUES(V_IDMESSAGE,V_IDRESPOSE,V_CODEREQUEST,V_DESCRIPTION);
     SAVE_LOG(' 2.3 P_MENSAJESEN:'||P_MENSAJESEN); 
     commit;                        
  EXCEPTION
    WHEN OTHERS THEN
     P_ServRegistroOK :=  '1'; --hd6104         
     V_error:=substr(SQLERRM,1,150);
     P_ServMsgError := V_error;                      
     SAVE_LOG(' 2.4 P_MENSAJESEN:'||P_MENSAJESEN);                              
     raise_application_error(-20901,'MSG:'||SQLERRM);
  END;

  --FIN DE INTERPREACION
IF CMS_AFECTACION_LINEA_PAGO_TC ='S' THEN
      SAVE_LOG(' 2.5 P_MENSAJESEN:'||P_MENSAJESEN);                              
   FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
  	      P_NUMENSAJES := P_NUMENSAJES + 1;                                      
      SAVE_LOG(' 2.6 P_MENSAJESEN:'||P_MENSAJESEN||p_operador||'^'||p_numtra||'^'||x.sec);                                	      
  	      RECIBE_MENSAJE(p_operador,p_numtra,x.sec,p_numensajes,p_errorrec,p_descerrorrec,P_ServRegistroOK,P_ServMsgError);
      SAVE_LOG(' 2.7 P_MENSAJESEN:'||P_MENSAJESEN);                                	        	      
      	  --MESSAGE( ' p_descerrorrec:'||p_descerrorrec); PAUSE;
      	  IF p_errorrec != '0' THEN
             p_ServRegistroOK := '1';
 	         p_ServMsgError :=p_descerrorrec;                                                         
      SAVE_LOG(' 2.8 P_MENSAJESEN:'||P_MENSAJESEN);                                	        	       	         
 	         --EXIT;
      	  ELSE
      	    --APLICA_INTERFACE (P_OPERADOR ,P_NUMTRA,X.SEC);
     	    --SI ME DEVOLVIO UN ERROR DESDE PROCECARD REVERSO LO QUE GRABE EN EL FISA SYSTEM
    	    --PARA TENER LA CONSTANCIA QUE EN EL FISA NO EXISTIO PROBLEMAS
      	    p_error :='0';
           save_log('paso 5:'||P_ServRegistroOK);
             p_ServRegistroOK := '0';
 	         p_ServMsgError :=NULL;                                                                    
      	    PKG_CMS_INTERFAZCREDITCARD.CMS_FINTRANSACTION(p_operador,p_numtra,x.sec,p_error,pcms_undoresponses);
           save_log('paso 6:'||P_ServRegistroOK||' pcms_undoresponses:'||pcms_undoresponses);
           EXIT;
          END IF;
   END LOOP ;
   -- Si existio error al grabar en las entidades externas se debe enviar un codigo de error
   -- y la descripcion del mensaje.
   IF p_ServRegistroOK !='0' THEN
      	p_error :='0';
          save_log('paso 7:'||P_ServRegistroOK);
     	PKG_CMS_INTERFAZCREDITCARD.CMS_UNDOTRANSACTION(p_operador,p_numtra,P_ServMsgError,p_error,pcms_undoresponses);
   	           save_log('paso 8');
	      FOR X IN REVERSOTRA(P_OPERADOR,P_NUMTRA) LOOP
                save_log('paso 9');
	      	     REVERSO(P_OPERADOR,P_NUMTRA,X.TMO_SEC,pcms_responses);
	                 save_log('paso 10');
	          	 --ENVIA_MENSAJE(pcms_responses,P_ServRegistroOK,P_ServMsgError);
	                 save_log('paso 11');
	      END LOOP;
	      P_MENSAJESEN := pcms_responses;--esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto       esto cuando envio la transaccion de reverso
          save_log('paso 12:'||pcms_responses);
	   --VOY A ENVIAR MENSAJE REVERSANDO TAMBIEN LA TRANSACCION EN PROCECARD
   END IF;
  --BORRO EL ELSE --PARA QUE SIENPRE FUNCIONE EN LINEA
 END IF;
 END;
 --FIN DE RECEPCION DEL MENSAJE
   --PARA RECEPCION DEL MENSAJE
 PROCEDURE VALIDA_TRANSACCION_RESP_ANTO(P_OPERADOR IN NUMBER,--Operador
						     P_NUMTRA IN NUMBER,--numero de transaccion
						     P_MENSAJEREC IN VARCHAR2,--dmensaje de respuesta en este caso desde procecard
						     P_SECTRA OUT NUMBER,--SECUENCIA DE LA TRANSACCION EN TRAMON
						     P_MENSAJESEN OUT VARCHAR2,--mensaje de envio
						     P_ServRegistroOK  OUT VARCHAR2,--1 error 0 todo OK
						     P_ServMsgError  OUT VARCHAR2)  is --si es 1 mensaje de error
  fecha date;
  pcms_responses  VARCHAR2(4000);
  pcms_undoresponses VARCHAR2(4000);
  --p_error VARCHAR2(1):='0';
  p_msgerror VARCHAR2(1000);
  p_codusr  tcap_tramon.tmo_codusr%type;
  --p_numtra tcap_tramon.tmo_numtra%type;
  p_fechacon tcap_tramon.tmo_fechcon%type;
  p_error VARCHAR2(1):='0';
  V_IDMESSAGE                            VARCHAR2(10);
  V_IDRESPOSE                            VARCHAR2(24);
  V_CODEREQUEST                          VARCHAR2(7);
  V_DESCRIPTION                          VARCHAR2(75);
  --pcms_undoresponses varchar2(500);
  CURSOR TRAMON(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
    SELECT /*+index CP01CAP_TMO*/
           TO_CHAR(A.TMO_FECHPROC,'YYYYMMDD') DATESYSTEM,
            TO_CHAR(A.TMO_FECHOR,'HH24MISS')   HOURSYSTEM,
            A.TMO_CODUSR USR,
            A.TMO_NUMTRA NUMTRAN,
            B.PGS_PARAMETRO CREDIT,
            A.TMO_CODMON CURRENCY,
            A.TMO_NUMTRA REFERENCE,
            A.TMO_FECHCON DATETRANSACTION,
            A.TMO_CODTRA  CODTRA,
            A.TMO_CODMOD CODMOD,
            A.TMO_SEC SEC,
            A.TMO_RUBRO MODEPAYMENT,
            '501' TABTIPPAGO,
            B.PGS_PARAMETRO3    TIPAG,
            '502' TABTIPLIN,
            B.PGS_PARAMETRO4  TIPLIN,
            A.TMO_CODSUC  OFFICE,
            B.PGS_MONTO  VALUE, --A.TMO_VAL
            DECODE(A.TMO_MODO,'N','C',A.TMO_MODO) ID_TRANSACTION
      FROM TCAP_TRAMON A ,TCAP_PAGOSERVICIOS B
      WHERE A.TMO_CODUSR = B.PGS_OPERADOR
        AND A.TMO_NUMTRA = B.PGS_NUMTRAN
        AND A.TMO_CODUSR = CODUSR
        AND A.TMO_NUMTRA = NUMTRA
        AND A.TMO_FECHPROC =  TRUNC(F_FECHATRABAJO)
        AND A.TMO_CODTRA <> 4
    ORDER BY A.TMO_SEC;
CURSOR REVERSOTRA(CODUSR IN NUMBER, NUMTRA IN NUMBER) IS
   SELECT TMO_CODUSR,TMO_NUMTRA,TMO_SEC
     FROM TCAP_TRAMON
    WHERE TMO_CODUSR = CODUSR
      AND TMO_NUMTRA = NUMTRA
      AND TMO_CODTRA <> 4
      AND TMO_FECHPROC = TRUNC(F_FECHATRABAJO)
      ORDER BY TMO_SEC;
	  CMS_AFECTACION_LINEA_PAGO_TC TGEN_PARINST.PAI_CODVAR%TYPE:='N';
	  P_NUMENSAJES NUMBER:=0;
	  p_errorrec   VARCHAR2(1);
	  p_descerrorrec trequestcredit.CMS_DESCRIPTION%TYPE;
BEGIN
BEGIN
	SELECT PAI_VALOR
	  INTO CMS_AFECTACION_LINEA_PAGO_TC
	 FROM TGEN_PARINST
	 WHERE PAI_CODVAR = 'CMS_AFECTACION_LINEA_PAGO_TC';
EXCEPTION
	WHEN OTHERS THEN
	  CMS_AFECTACION_LINEA_PAGO_TC :='N';
END;
    P_NUMENSAJES :=0;
 	p_errorrec   := '0';
	p_descerrorrec := null;
    P_ServRegistroOK :=  '0'; --hd6104
    P_ServMsgError   :=  null; --hd6104
    SAVE_LOG(' 1 P_MENSAJEREC:'||P_MENSAJEREC);
    SAVE_LOG(' 2 P_MENSAJESEN:'||P_MENSAJESEN);
    DELETE LOG_BATCH;
  ---INTERPRETO LA TRAMA QUE LLEGO DE RESPUESTA
  BEGIN
  V_IDMESSAGE := RTRIM(LTRIM(SUBSTR(P_MENSAJEREC,1,10)));    --VARCHAR2(10);
  V_IDRESPOSE := RTRIM(LTRIM(SUBSTR(P_MENSAJEREC,11,24)));   --VARCHAR2(24);
  V_CODEREQUEST  := RTRIM(LTRIM(SUBSTR(P_MENSAJEREC,35,7))); --VARCHAR2(7);
  V_DESCRIPTION  := RTRIM(LTRIM(SUBSTR(P_MENSAJEREC,42,75)));--VARCHAR2(75);
  --COMENTO ESTO PORQUE PARA ESTE CASO EL PROCESO DE RESPUESTA VA HA GRABAR EN LA TABLA DE RECEPCION DE MENSAJE
  --INSERT INTO trequestcredit(CMS_IDMESSAGE,CMS_IDRESPOSE,CMS_CODEREQUEST,CMS_DESCRIPTION)
  --                    VALUES(V_IDMESSAGE,V_IDRESPOSE,V_CODEREQUEST,V_DESCRIPTION);
  EXCEPTION
    WHEN OTHERS THEN
     P_ServRegistroOK :=  '1'; --hd6104
  END;

  --FIN DE INTERPREACION
IF CMS_AFECTACION_LINEA_PAGO_TC ='S' THEN
   FOR X IN TRAMON(P_OPERADOR,P_NUMTRA) LOOP
  	      P_NUMENSAJES := P_NUMENSAJES + 1;
  	      RECIBE_MENSAJE(p_operador,p_numtra,x.sec,p_numensajes,p_errorrec,p_descerrorrec,P_ServRegistroOK,P_ServMsgError);
      	  --MESSAGE( ' p_descerrorrec:'||p_descerrorrec); PAUSE;
      	  IF nvl(p_errorrec,'1') != '0' THEN
             p_ServRegistroOK := '1';
 	         p_ServMsgError :=p_descerrorrec;
 	         EXIT;
      	  ELSE
      	    --APLICA_INTERFACE (P_OPERADOR ,P_NUMTRA,X.SEC);
     	    --SI ME DEVOLVIO UN ERROR DESDE PROCECARD REVERSO LO QUE GRABE EN EL FISA SYSTEM
    	    --PARA TENER LA CONSTANCIA QUE EN EL FISA NO EXISTIO PROBLEMAS
      	    p_error :='0';
           save_log('paso 5:'||P_ServRegistroOK);
      	    PKG_CMS_INTERFAZCREDITCARD.CMS_FINTRANSACTION(p_operador,p_numtra,x.sec,p_error,pcms_undoresponses);
           save_log('paso 6:'||P_ServRegistroOK||' pcms_undoresponses:'||pcms_undoresponses);
          END IF;
          P_SECTRA := x.sec;--SECUENCIA PARA PODER APLICAR LA TRX EN LA TRAMA
   END LOOP ;
   -- Si existio error al grabar en las entidades externas se debe enviar un codigo de error
   -- y la descripcion del mensaje.
   IF p_ServRegistroOK !='0' THEN
    	p_error :='0';
          save_log('paso 7:'||P_ServRegistroOK);
     	PKG_CMS_INTERFAZCREDITCARD.CMS_UNDOTRANSACTION(p_operador,p_numtra,P_ServMsgError,p_error,pcms_undoresponses);
   	           save_log('paso 8');
	      FOR X IN REVERSOTRA(P_OPERADOR,P_NUMTRA) LOOP
                save_log('paso 9');
	      	     REVERSO(P_OPERADOR,P_NUMTRA,X.TMO_SEC,pcms_responses);
	                 save_log('paso 10');
	          	 --ENVIA_MENSAJE(pcms_responses,P_ServRegistroOK,P_ServMsgError);
	                 save_log('paso 11');
	      END LOOP;
	      P_MENSAJESEN := pcms_responses;--esto por cuanto seria una trama por cuenta por ser solo debito--en caja es distincto       esto cuando envio la transaccion de reverso
          save_log('paso 12:'||pcms_responses);
	   --VOY A ENVIAR MENSAJE REVERSANDO TAMBIEN LA TRANSACCION EN PROCECARD
   END IF;

  --BORRO EL ELSE --PARA QUE SIENPRE FUNCIONE EN LINEA
 END IF;
 COMMIT;--POR SER UN PROCESO A PARTE DEL ENVIO
 END;
  PROCEDURE CMS_FINTRANSACTION(p_codusr  IN NUMBER,
                         p_numtra  IN NUMBER,
                         p_sec     IN NUMBER,
                         p_fechaproc in date,
                         p_error   IN OUT VARCHAR2,
  						 pcms_undoresponses OUT VARCHAR2)   IS
    cursor tramon is
      Select tmo_codusr,tmo_numtra,tmo_sec
       from  tcap_tramon
       where tmo_codusr = p_codusr
         and tmo_numtra = p_numtra
         and tmo_fechproc = p_fechaproc;
  BEGIN
       p_error := '0';
    for x in tramon loop
         UPDATE tresponsecredit
            SET CMS_ESTADOREG = 'A'
          WHERE --to_number(CMS_IDMESSAGE) = p_codusr AND
             to_number(CMS_IDRESPOSE) = x.tmo_codusr||x.tmo_numtra||x.tmo_sec;
    end loop;
  EXCEPTION
    WHEN OTHERS THEN
           p_error := '1';
           pcms_undoresponses:='Error Al Aplicar:'||p_codusr||' '||p_numtra||' '||SQLERRM;
  END;

 --FIN DE RECEPCION DEL MENSAJE
 PROCEDURE ENVIO_ANTONY_TRAMA(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_USUARIO IN NUMBER,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMTRAP IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TARJETA IN VARCHAR2,
				  P_MSG IN VARCHAR2) IS

BEGIN
   UPDATE TDOM_RECEPCIONPAGOTC
     SET RPT_CODUSR = P_OPERADOR,
         RPT_NUMTRA = P_NUMTRA  ,
         RPT_TEXTO = P_MSG      ,
         RPT_PROCESADOENV  = 'S'
    WHERE RPT_USUARIO   = P_USUARIO
       AND RPT_NUMTRAP  = P_NUMTRAP
       AND RPT_TARJETA  = P_TARJETA;
     COMMIT;
END;
--PARA ACH
PROCEDURE ENVIO_ACH_TRAMA(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_FECHA IN DATE,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMERO_LOTE IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TIPO_LOTE IN VARCHAR2,
				  P_CANAL IN VARCHAR2,
				  P_IN_OUT IN VARCHAR2,
				  P_TARJETA IN VARCHAR2,
				  P_STATUS IN VARCHAR2,
				  P_MONEDA IN NUMBER) IS

BEGIN
   UPDATE TBDI_ACHTRAN
     SET ACT_CODUSR = P_OPERADOR,
         ACT_NUMTRA = P_NUMTRA  ,
		 ACT_STATUS_CUENTA = P_STATUS
    WHERE ACT_FECHA = P_FECHA AND
 		  ACT_NUMERO_LOTE = P_NUMERO_LOTE AND
		  ACT_TIPO_LOTE = P_TIPO_LOTE AND
		  ACT_CANAL = P_CANAL AND
          ACT_IN_OUT = P_IN_OUT AND
          ACT_TIPO_REGISTRO = 'D' AND
          ACT_MONEDA = P_MONEDA AND 
          ACT_NUMCUE = P_TARJETA;
     COMMIT;
END;
--PARA LA CABECERA
PROCEDURE ENVIO_ACH_TRAMA_CAB(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_FECHA IN DATE,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMERO_LOTE IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TIPO_LOTE IN VARCHAR2,
				  P_CANAL IN VARCHAR2,
				  P_IN_OUT IN VARCHAR2,
				  P_TARJETA IN VARCHAR2,
				  P_STATUS IN VARCHAR2,
				  P_MONEDA IN NUMBER ) IS
W_ERRORES NUMBER:=0;
BEGIN
  SELECT COUNT(*)
    INTO W_ERRORES
   FROM  TBDI_ACHTRAN
    WHERE ACT_FECHA = P_FECHA AND
 		  ACT_NUMERO_LOTE = P_NUMERO_LOTE AND
		  ACT_TIPO_LOTE = P_TIPO_LOTE AND
		  ACT_CANAL = P_CANAL AND
          ACT_IN_OUT = P_IN_OUT AND
          ACT_TIPO_REGISTRO = 'D' AND
          ACT_MONEDA = P_MONEDA AND
          --ACT_MODULO = 8 AND
          ACT_STATUS_CUENTA <> 'R00';
IF W_ERRORES > 0 THEN
   UPDATE TBDI_ACHTRAN
     SET ACT_CODUSR = P_OPERADOR,
         ACT_NUMTRA = P_NUMTRA  ,
		 ACT_STATUS_CUENTA = 'P00'
    WHERE ACT_FECHA = P_FECHA AND
 		  ACT_NUMERO_LOTE = P_NUMERO_LOTE AND
		  ACT_TIPO_LOTE = P_TIPO_LOTE AND
		  ACT_CANAL = P_CANAL AND
          ACT_IN_OUT = P_IN_OUT AND
          ACT_MONEDA = P_MONEDA AND
          ACT_TIPO_REGISTRO = 'B';
 ELSE
   UPDATE TBDI_ACHTRAN
     SET ACT_CODUSR = P_OPERADOR,
         ACT_NUMTRA = P_NUMTRA  ,
		 ACT_STATUS_CUENTA = P_STATUS
    WHERE ACT_FECHA = P_FECHA AND
 		  ACT_NUMERO_LOTE = P_NUMERO_LOTE AND
		  ACT_TIPO_LOTE = P_TIPO_LOTE AND
		  ACT_CANAL = P_CANAL AND
          ACT_IN_OUT = P_IN_OUT AND
          ACT_MONEDA = P_MONEDA AND
          ACT_TIPO_REGISTRO = 'B';
 END IF;
     COMMIT;
END;

--SI TODO EXITO MARCO COMO RECIBIDO
 PROCEDURE REENVIO_ANTONY_TRAMA(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_USUARIO IN NUMBER,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMTRAP IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TARJETA IN VARCHAR2,
				  P_MSG IN VARCHAR2) IS

BEGIN
   UPDATE TDOM_RECEPCIONPAGOTC
     SET  RPT_PROCESADOREC  = 'S'
    WHERE RPT_USUARIO   = P_USUARIO
       AND RPT_NUMTRAP  = P_NUMTRAP
       AND RPT_TARJETA  = P_TARJETA;
     COMMIT;
END;
--SI NO LLEGO Y EXITIO UN ERROR LE QUITO LA MARCA DE ENVIADO Y DE RECIBIDO
--PARA VOLVER A ENVIAR.
PROCEDURE REVERSA_ANTONY_TRAMA(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_USUARIO IN NUMBER,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMTRAP IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TARJETA IN VARCHAR2,
				  P_MSG IN VARCHAR2) IS

BEGIN
   UPDATE TDOM_RECEPCIONPAGOTC
     SET  RPT_PROCESADOREC  = NULL,
          RPT_PROCESADOENV = NULL
    WHERE RPT_USUARIO   = P_USUARIO
       AND RPT_NUMTRAP  = P_NUMTRAP
       AND RPT_TARJETA  = P_TARJETA;
     COMMIT;
END;

PROCEDURE LOG_BITACORA(P_MSG IN VARCHAR2) IS
BEGIN
INSERT INTO LOG_BATCH VALUES(P_MSG);
COMMIT;
END;
  --FIN
  PROCEDURE DTE_BITACORA IS
  BEGIN
     DELETE LOG_BATCH;
     COMMIT;
END;
--------------------------------------------------------
--COTIZACION DE VENTANILLA 
--------------------------------------------------------
PROCEDURE P_TASADIVISACAJA(P_SUC IN NUMBER,
                           P_MONEDA IN NUMBER,
                           P_FECHA IN DATE,
                           P_TASACOMPRA OUT NUMBER,
                           P_TASAVENTA OUT NUMBER) IS
BEGIN
 SELECT MVE_COTCOMPRAV,MVE_COTVENTAV
   INTO P_TASACOMPRA, P_TASAVENTA
  FROM  TGEN_MONEDAVEN
  WHERE MVE_MONEDA = P_MONEDA
    AND MVE_SUCURSAL = P_SUC
    AND MVE_FECHAHASTA IS NULL;
EXCEPTION
  WHEN OTHERS THEN
      raise_application_error(-20901,'ERROR EN COTIZCION MONEDA:'||P_MONEDA);    
END;                                                                       
--------------------------------------------------------
--LISTADO DE MONEDAS 
--------------------------------------------------------

PROCEDURE P_CODMONEDAS(P_MONEDAS OUT REFCURSOR) IS
BEGIN
     OPEN P_MONEDAS
      FOR  SELECT MON_COD,MON_DES
			 FROM TGEN_MONEDA
            WHERE MON_COD IN (SELECT DISTINCT VIS_MONEDA FROM TCAP_VISTA)
			ORDER BY MON_COD;
END;     
----------------------
--para multimoneda trama
----------------------
 PROCEDURE PCMS_RESPONSE_MM(p_codusr  IN NUMBER,
                         p_numtra  IN NUMBER,
                         p_sec IN NUMBER,
                         p_linea in VARCHAR2,
                         p_error   IN OUT VARCHAR2,
                         p_msgerror IN OUT VARCHAR2,
                         mensaje OUT VARCHAR2,
                         p_canal IN NUMBER,
                         P_CURRENCY IN NUMBER )
    IS
    CURSOR TRAMON IS
    SELECT /*+index CP01CAP_TMO*/
           TO_CHAR(A.TMO_FECHPROC,'YYYYMMDD') DATESYSTEM,
            TO_CHAR(A.TMO_FECHOR,'HH24MISS')   HOURSYSTEM,
            A.TMO_CODUSR USR,
            A.TMO_NUMTRA NUMTRAN,
            B.PGS_PARAMETRO CREDIT,
            --A.TMO_CODMON CURRENCY,
            P_CURRENCY CURRENCY,
            --A.TMO_NUMTRA REFERENCE,
            to_number(substr(lpad(to_char(A.TMO_NUMTRA),7,'0'),3,5)||substr(lpad(to_char(A.tmo_sec),5,'0'),4,2)) REFERENCE,
            --A.TMO_FECHCON DATETRANSACTION,
            A.TMO_FECHPROC DATETRANSACTION,            
            A.TMO_CODTRA  CODTRA,
            A.TMO_CODMOD CODMOD,
            --A.TMO_TIPORUB MODEPAYMENT,
            A.TMO_RUBRO MODEPAYMENT,
            '501' TABTIPPAGO,
            B.PGS_PARAMETRO2 TIPAG,
            '502' TABTIPLIN,
            B.PGS_PARAMETRO3  TIPLIN,
            A.TMO_CODSUC  OFFICE,
            --A.TMO_VAL  VALUEPAGO, --B.PGS_MONTO  VALUE, --A.TMO_VAL--SE COMENTA PORQUE SIEMPRE ES DEBITO
            B.PGS_MONTO  VALUEPAGO, --B.PGS_MONTO  VALUE, --A.TMO_VAL            
            DECODE(A.TMO_MODO,'N','C',A.TMO_MODO) ID_TRANSACTION,
            A.TMO_SEC,
            A.TMO_REF
            --B.PGS_NUMPAGO NUMPAGO
      FROM TCAP_TRAMON A,TCAP_PAGOSERVICIOS B
      WHERE A.TMO_CODUSR = B.PGS_OPERADOR
        AND A.TMO_NUMTRA = B.PGS_NUMTRAN
        --AND A.TMO_FECHCON = P_FECHA
        AND A.TMO_CODUSR = P_CODUSR
        AND A.TMO_NUMTRA = P_NUMTRA
        AND A.TMO_SEC = P_SEC
        AND A.TMO_CODTRA <> 4
        AND A.TMO_MODO = 'N'
    ORDER BY A.TMO_SEC;
    V_CURRENCY        TGEN_MAPMONEDA.MOP_MONEQUIV%TYPE;
    V_CURRENCY_ORG    TGEN_MAPMONEDA.MOP_MONEQUIV%TYPE;
    V_MODEPAY         TGEN_DESCTABLA.CODEISO%TYPE;
    V_TYPELINE        TGEN_DESCTABLA.CODEISO%TYPE;
    V_CODELINE        TGEN_DESCTABLA.CODEISO%TYPE;
    V_TIPPAG          TCAP_PAGOSERVICIOS.PGS_PARAMETRO2%TYPE;
    v_estadoreg       VARCHAR2(1):='P';
    V_ENTERO          VARCHAR2(11);
    V_DECIMAL         VARCHAR2(2);
    V_TOTALVALOR      VARCHAR2(11);
    pcms_responses    CMS_RESPONSE;
    V_ENCONTRO        NUMBER:=0;
    V_USRTRA          VARCHAR2(16);
    V_USUARIOIB       VARCHAR2(10);
    BEGIN
    p_error := '0';
    V_ENCONTRO :=0;
    pcms_responses:=cms_response(null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);
    FOR X IN TRAMON LOOP
        V_CURRENCY := 0;
        V_CURRENCY_ORG := 0;
        V_MODEPAY := NULL;
        V_TYPELINE := 0;
        V_CODELINE := 0;
        V_TIPPAG := NULL;
      DBMS_OUTPUT.PUT_LINE('PASO 1');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,'MSG020025X',formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR('MSG020025X',columntyp,columnlen,filling))
      INTO pcms_responses.cms_idmessage
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 1;
       DBMS_OUTPUT.PUT_LINE('PASO 2');
    --pcms_response.cms_idmessage := '1';
    --
      --V_USRTRA := LPAD(X.USR,4,'0')||LPAD(X.NUMTRAN,7,'0');
      V_USRTRA := X.USR||X.NUMTRAN||X.TMO_SEC;
    --
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_USRTRA,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_USRTRA,columntyp,columnlen,filling))
      INTO pcms_responses.cms_idrespose
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 2;
      	--pcms_response.cms_idrespose := '1';
       DBMS_OUTPUT.PUT_LINE('PASO 3');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.datesystem,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.datesystem,columntyp,columnlen,filling))
      INTO pcms_responses.cms_datesystem
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 3;
      	--pcms_response.cms_datesystem := x.datesystem;
       DBMS_OUTPUT.PUT_LINE('PASO 4');
    SELECT decode(columntyp,'D', pkg_legalreport3.obt_formatvalue(columntyp,x.hoursystem,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.hoursystem,columntyp,columnlen,filling))
      INTO pcms_responses.cms_hoursystem
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 4;
		--pcms_response.cms_hoursystem := x.hoursystem;
       DBMS_OUTPUT.PUT_LINE('PASO 5');
    V_USUARIOIB := LPAD(X.USR,6,'0');
    V_USUARIOIB :='EBNK'||V_USUARIOIB;
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_USUARIOIB,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_USUARIOIB,columntyp,columnlen,filling))
      INTO pcms_responses.cms_user
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 5;

		--pcms_response.cms_user  := x.usr;
       DBMS_OUTPUT.PUT_LINE('PASO 6');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.credit,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(x.credit,columntyp,columnlen,filling))
      INTO pcms_responses.cms_credit
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 6;
		--pcms_response.cms_credit := x.credit;
		BEGIN
		SELECT MOP_MONEQUIV
          INTO V_CURRENCY
          FROM TGEN_MAPMONEDA
         WHERE MOP_CODINSTIT = 1 --BDI
           AND MOP_MONEDA = X.CURRENCY;
        EXCEPTION
         WHEN OTHERS THEN
           p_error := '1';
           P_MSGERROR := 'Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm;
		   --RAISE_APPLICATION_ERROR (-20101,'Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm);
        END;
       DBMS_OUTPUT.PUT_LINE('PASO 7');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_CURRENCY,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_CURRENCY,columntyp,columnlen,filling))
      INTO pcms_responses.cms_currency
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 7;
	--pcms_response.cms_currency := V_CURRENCY;
      DBMS_OUTPUT.PUT_LINE('PASO 8');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.reference,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.reference,columntyp,columnlen,filling))
      INTO pcms_responses.cms_reference
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 8;

		--pcms_response.cms_reference := x.reference;
       DBMS_OUTPUT.PUT_LINE('PASO 9');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.datetransaction,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.datetransaction,columntyp,columnlen,filling))
      INTO pcms_responses.cms_datetransaction
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 9;
		--pcms_response.cms_datetransaction   := x.datetransaction;
        ---LOS CODIGO DE LA INSTITUCION VAMOS A USAR COMO CODIGOS DE CANALES PARA ESTE CASO
        --2 POR CAJA
        --3 POR IVR
        --4 POR IB
		BEGIN
		SELECT MOP_MONEQUIV
          INTO V_CURRENCY_ORG
          FROM TGEN_MAPMONEDA
         WHERE MOP_CODINSTIT = p_canal --2 --BDI
           AND MOP_MONEDA = X.CURRENCY;
        EXCEPTION
         WHEN OTHERS THEN
           p_error := '1';
           P_MSGERROR :=' 2 Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm;
		  --RAISE_APPLICATION_ERROR (-20101,' 2 Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm);
        END;
       DBMS_OUTPUT.PUT_LINE('PASO 10');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_CURRENCY_ORG,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_CURRENCY_ORG,columntyp,columnlen,filling))
      INTO pcms_responses.cms_origmovement
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 10;
       DBMS_OUTPUT.PUT_LINE('PASO 10.1');
		--pcms_response.cms_origmovement := V_CURRENCY_ORG;
		BEGIN
		SELECT CODEISO
          INTO V_MODEPAY
          FROM TGEN_DESCTABLA
         WHERE DES_CODTAB = 502 --BDI
           AND DES_CODIGO = X.MODEPAYMENT;
        EXCEPTION
         WHEN OTHERS THEN
           p_error := '1';
           P_MSGERROR := '3 Error Moneda revise parametros 1/117 FS:'||' '||X.MODEPAYMENT||sqlerrm;
		   --RAISE_APPLICATION_ERROR (-20101,' 3 Error Moneda revise parametros 1/117 FS:'||' '||X.MODEPAYMENT||sqlerrm);
        END;
       DBMS_OUTPUT.PUT_LINE('PASO 11');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_MODEPAY,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_MODEPAY,columntyp,columnlen,filling))
      INTO pcms_responses.cms_modepayment
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 11;
		       --pcms_response.cms_modepayment := V_MODEPAY;
		/*BEGIN
		SELECT CODEISO
          INTO V_TYPELINE
          FROM TGEN_DESCTABLA
         WHERE DES_CODTAB = NVL(X.TABTIPPAGO,500)
           AND DES_CODIGO = NVL(X.TIPAG,1);*/
        /* SELECT RTRIM(LTRIM(PSP_VALORPARAM))
           INTO V_TYPELINE
           FROM TCAP_PAGOSERVICIOSPARAM
         WHERE  PSP_OPERADOR = X.USR --P_CODUSR
		    AND PSP_NUMTRAN  = X.NUMTRAN --P_NUMTRA
		    AND PSP_NUMPAGO = X.NUMPAGO
			AND PSP_SERVICIO = 5  --VISA
		    AND PSP_TABSERVICIO = 117
		    AND PSP_CODPARAM =  2 ;--tipo de pago
        EXCEPTION
         WHEN OTHERS THEN
		  RAISE_APPLICATION_ERROR (-20101,' 4 Error Moneda revise parametros 1/117 FS:'||sqlerrm);
        END;      */
       DBMS_OUTPUT.PUT_LINE('PASO 12');
    --ENCUENTRO EL TIPO DE PAGO
    BEGIN
    SELECT TYPEPAY
      INTO V_TIPPAG
      FROM TBINTYPEPAYMENT
     WHERE BIN = SUBSTR(X.CREDIT,1,6);
    EXCEPTION
      WHEN OTHERS THEN
          p_error := '1';
          P_MSGERROR :='BIN NO TIENE TIPO DE PAGO REVISE PARAMETROS..'||SQLERRM;
    END;
    --FIN

           V_TYPELINE := V_TIPPAG ; --X.TIPAG;
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_TYPELINE,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_TYPELINE,columntyp,columnlen,filling))
      INTO pcms_responses.cms_typeline
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 12;

	    --pcms_response.cms_typeline := V_TYPELINE;
		/*BEGIN
		SELECT CODEISO
          INTO V_CODELINE
          FROM TGEN_DESCTABLA
         WHERE DES_CODTAB = NVL(X.TABTIPLIN,501)
           AND DES_CODIGO = NVL(X.TIPLIN,1);*/
       /*  SELECT RTRIM(LTRIM(PSP_VALORPARAM))
           INTO V_CODELINE
           FROM TCAP_PAGOSERVICIOSPARAM
         WHERE  PSP_OPERADOR = X.USR --P_CODUSR
		    AND PSP_NUMTRAN  = X.NUMTRAN --P_NUMTRA
		    AND PSP_NUMPAGO = X.NUMPAGO
			AND PSP_SERVICIO = 5  --VISA
		    AND PSP_TABSERVICIO = 117
		    AND PSP_CODPARAM =  3 ;--tipo de pago

        EXCEPTION
         WHEN OTHERS THEN
		  RAISE_APPLICATION_ERROR (-20101,'Error Moneda revise parametros 1/117 FS:'||sqlerrm);
        END;           */
        IF V_TIPPAG = 'D' THEN
          V_CODELINE := 'CP';
        ELSE
          V_CODELINE := X.TIPLIN;
        END IF;
       DBMS_OUTPUT.PUT_LINE('PASO 13');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_CODELINE,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_CODELINE,columntyp,columnlen,filling))
      INTO pcms_responses.cms_codeline
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 13;

	--pcms_response.cms_codeline := V_CODELINE;
       DBMS_OUTPUT.PUT_LINE('PASO 14');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,X.OFFICE,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(X.OFFICE,columntyp,columnlen,filling))
      INTO pcms_responses.cms_office
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 14;

    --Para el valor los decimales van sin punto
    --·El campo valor del pago debe ser rellenado con ceros a la izquierda e incluir el monto
    --de la parte decimal sin el punto. Por ejemplo: para representar el valor 325.78
    --seria 00000032578. Si es 300.00 seria 00000030000.
	IF instr(X.VALUEPAGO,'.')  <> 0 THEN
       V_ENTERO  := substr(X.VALUEPAGO,1,instr(X.VALUEPAGO,'.') -1 ) ;
       V_DECIMAL := rpad(substr(X.VALUEPAGO,instr(X.VALUEPAGO,'.') +1,2),2,'0');
       V_TOTALVALOR := V_ENTERO||V_DECIMAL;
    ELSE
       V_ENTERO  := X.VALUEPAGO;
       V_DECIMAL := '00';
       V_TOTALVALOR := V_ENTERO||V_DECIMAL;
    END IF;

  --pcms_response.cms_office  := X.OFFICE;
    DBMS_OUTPUT.PUT_LINE('PASO 15');
  --SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,X.VALUE,formatcol),to_format(X.VALUE,columntyp,columnlen,filling))
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_TOTALVALOR,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(V_TOTALVALOR,columntyp,columnlen,filling))
      INTO pcms_responses.cms_value
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 15;

		--pcms_response.cms_value   := X.VALUE ;
       DBMS_OUTPUT.PUT_LINE('PASO 16');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,X.ID_TRANSACTION,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(X.ID_TRANSACTION,columntyp,columnlen,filling))
      INTO pcms_responses.cms_idtransaction
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 16;
		--pcms_response.cms_idtransaction  := X.ID_TRANSACTION ;
       DBMS_OUTPUT.PUT_LINE('PASO 17');
       --CUANDO ENVIE EL MENSAJE ESTE VA COMO ESTADO P PENDIENTE DE APLICAR
       v_estadoreg:= 'P';
      --en id reponse va el usuario y el numtra, idmessage lo asigna procecard
      --fin
     BEGIN
		INSERT INTO tresponsecredit(cms_idmessage ,cms_idrespose , cms_datesystem,cms_hoursystem,cms_user,
  					cms_credit, cms_currency, cms_reference , cms_datetransaction,
  					cms_origmovement, cms_modepayment,  cms_typeline ,cms_codeline,cms_OFFICE,
					cms_value, cms_idTransaction,cms_status,cms_codtrans   , cms_plazo  ,
					cms_numlote ,cms_numsec ,cms_referenciauniv, cms_numafiliado ,cms_autorizacion,cms_estadoreg,cms_enlinea )
		   VALUES(  pcms_responses.cms_idmessage,pcms_responses.cms_idrespose , pcms_responses.cms_datesystem,pcms_responses.cms_hoursystem,pcms_responses.cms_user,
  					pcms_responses.cms_credit, pcms_responses.cms_currency, pcms_responses.cms_reference , pcms_responses.cms_datetransaction,
  					pcms_responses.cms_origmovement, pcms_responses.cms_modepayment,  pcms_responses.cms_typeline ,pcms_responses.cms_codeline,pcms_responses.cms_OFFICE,
					pcms_responses.cms_value, pcms_responses.cms_idTransaction,NULL,x.codtra,0,X.TMO_SEC,X.TMO_SEC,X.TMO_REF,null,0,v_estadoreg,p_linea);
       DBMS_OUTPUT.PUT_LINE('PASO 18');

       ---UNA VEZ QUE ESPERE LA RESPUESTA SI ES OK SE ACTUALIZA EL CAMPO CON A , PERO SI SE RECHAZA O REVERSADO
        --ACTUALIZO TRAMON Y ESTA TABLA CON R, SINO SE APLICA PERO ES VALIDA
        -- LA OPERACION SE QUEDA COMO P PARA LA GENERACION DEL PLANO A ENVIAR.


       -- O SUCEDE ALGO QUE NO TENGA RESPUESTA
   EXCEPTION
     WHEN OTHERS THEN
          p_error := '1';
          P_MSGERROR :='ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM;
          --raise_application_error(-20195,'ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM);
   END;
   mensaje:=pcms_responses.cms_idmessage||pcms_responses.cms_idrespose||pcms_responses.cms_datesystem||pcms_responses.cms_hoursystem||
            pcms_responses.cms_user||pcms_responses.cms_credit||pcms_responses.cms_currency||pcms_responses.cms_reference||pcms_responses.cms_datetransaction||
            pcms_responses.cms_origmovement||pcms_responses.cms_modepayment||pcms_responses.cms_typeline||pcms_responses.cms_codeline||pcms_responses.cms_office||
            pcms_responses.cms_value||pcms_responses.cms_idtransaction;
    V_ENCONTRO := V_ENCONTRO + 1;
  END LOOP;
  IF V_ENCONTRO = 0 THEN
        p_error := '1';
        P_MSGERROR :=  'TRANSACCION NO ENOCNTRADA EN EL SISTEMA..U NT '||P_CODUSR||' '||P_NUMTRA||SQLERRM;
  END IF;
   EXCEPTION
     WHEN OTHERS THEN
        p_error := '1';
        P_MSGERROR := 'ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM;
        --raise_application_error(-20195,'ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM);
  END ;
  /*---------------RESPONSE MASIVO--------*/
 PROCEDURE PCMS_RESPONSE_MM_MAS(p_codusr  IN NUMBER,
                         p_numtra  IN NUMBER,
                         p_sec IN NUMBER,
                         p_linea in VARCHAR2,
                         p_error   IN OUT VARCHAR2,
                         p_msgerror IN OUT VARCHAR2,
                         mensaje OUT VARCHAR2,
                         p_canal IN NUMBER,
                         P_CURRENCY IN NUMBER )
    IS
    CURSOR TRAMON IS
    SELECT /*+index CP01CAP_TMO*/
           TO_CHAR(A.TMO_FECHPROC,'YYYYMMDD') DATESYSTEM,
            TO_CHAR(A.TMO_FECHOR,'HH24MISS')   HOURSYSTEM,
            A.TMO_CODUSR USR,
            A.TMO_NUMTRA NUMTRAN,
            B.PGS_PARAMETRO CREDIT,
            --A.TMO_CODMON CURRENCY,
            P_CURRENCY CURRENCY,
            --A.TMO_NUMTRA REFERENCE,
            to_number(substr(lpad(to_char(A.TMO_NUMTRA),7,'0'),3,5)||substr(lpad(to_char(A.tmo_sec),5,'0'),4,2)) REFERENCE,
            A.TMO_FECHCON DATETRANSACTION,
            A.TMO_CODTRA  CODTRA,
            A.TMO_CODMOD CODMOD,
            --A.TMO_TIPORUB MODEPAYMENT,
            A.TMO_RUBRO MODEPAYMENT,
            '501' TABTIPPAGO,
            B.PGS_PARAMETRO2 TIPAG,
            '502' TABTIPLIN,
            B.PGS_PARAMETRO3  TIPLIN,
            A.TMO_CODSUC  OFFICE,
            --A.TMO_VAL  VALUEPAGO, --B.PGS_MONTO  VALUE, --A.TMO_VAL--SE COMENTA PORQUE SIEMPRE ES DEBITO
            B.PGS_MONTO  VALUEPAGO, --B.PGS_MONTO  VALUE, --A.TMO_VAL            
            DECODE(A.TMO_MODO,'N','C',A.TMO_MODO) ID_TRANSACTION,
            A.TMO_SEC,
            A.TMO_REF
            --B.PGS_NUMPAGO NUMPAGO
      FROM TCAP_TRAMON A,TCAP_PAGOSERVICIOS B
      WHERE A.TMO_CODUSR = B.PGS_OPERADOR
        AND A.TMO_NUMTRA = B.PGS_NUMTRAN
        --AND A.TMO_FECHCON = P_FECHA
        AND A.TMO_CODUSR = P_CODUSR
        AND A.TMO_NUMTRA = P_NUMTRA
        AND A.TMO_SEC = P_SEC
        AND A.TMO_CODTRA <> 4
        AND A.TMO_MODO = 'N'
    ORDER BY A.TMO_SEC;
    V_CURRENCY        TGEN_MAPMONEDA.MOP_MONEQUIV%TYPE;
    V_CURRENCY_ORG    TGEN_MAPMONEDA.MOP_MONEQUIV%TYPE;
    V_MODEPAY         TGEN_DESCTABLA.CODEISO%TYPE;
    V_TYPELINE        TGEN_DESCTABLA.CODEISO%TYPE;
    V_CODELINE        TGEN_DESCTABLA.CODEISO%TYPE;
    V_TIPPAG          TCAP_PAGOSERVICIOS.PGS_PARAMETRO2%TYPE;
    v_estadoreg       VARCHAR2(1):='P';
    V_ENTERO          VARCHAR2(11);
    V_DECIMAL         VARCHAR2(2);
    V_TOTALVALOR      VARCHAR2(11);
    pcms_responses    CMS_RESPONSE;
    V_ENCONTRO        NUMBER:=0;
    V_USRTRA          VARCHAR2(16);
    V_USUARIOIB       VARCHAR2(10);
    BEGIN
    p_error := '0';
    V_ENCONTRO :=0;
    pcms_responses:=cms_response(null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);
    FOR X IN TRAMON LOOP
        V_CURRENCY := 0;
        V_CURRENCY_ORG := 0;
        V_MODEPAY := NULL;
        V_TYPELINE := 0;
        V_CODELINE := 0;
        V_TIPPAG := NULL;
      DBMS_OUTPUT.PUT_LINE('PASO 1');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,'MSG020025X',formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR('MSG020025X',columntyp,columnlen,filling))
      INTO pcms_responses.cms_idmessage
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 1;
       DBMS_OUTPUT.PUT_LINE('PASO 2');
    --pcms_response.cms_idmessage := '1';
    --
      --V_USRTRA := LPAD(X.USR,4,'0')||LPAD(X.NUMTRAN,7,'0');
      V_USRTRA := X.USR||X.NUMTRAN||X.TMO_SEC;
    --
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_USRTRA,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_USRTRA,columntyp,columnlen,filling))
      INTO pcms_responses.cms_idrespose
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 2;
      	--pcms_response.cms_idrespose := '1';
       DBMS_OUTPUT.PUT_LINE('PASO 3');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.datesystem,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.datesystem,columntyp,columnlen,filling))
      INTO pcms_responses.cms_datesystem
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 3;
      	--pcms_response.cms_datesystem := x.datesystem;
       DBMS_OUTPUT.PUT_LINE('PASO 4');
    SELECT decode(columntyp,'D', pkg_legalreport3.obt_formatvalue(columntyp,x.hoursystem,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.hoursystem,columntyp,columnlen,filling))
      INTO pcms_responses.cms_hoursystem
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 4;
		--pcms_response.cms_hoursystem := x.hoursystem;
       DBMS_OUTPUT.PUT_LINE('PASO 5');
    V_USUARIOIB := LPAD(X.USR,6,'0');
    V_USUARIOIB :='EBNK'||V_USUARIOIB;
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_USUARIOIB,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_USUARIOIB,columntyp,columnlen,filling))
      INTO pcms_responses.cms_user
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 5;

		--pcms_response.cms_user  := x.usr;
       DBMS_OUTPUT.PUT_LINE('PASO 6');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.credit,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(x.credit,columntyp,columnlen,filling))
      INTO pcms_responses.cms_credit
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 6;
		--pcms_response.cms_credit := x.credit;
		BEGIN
		SELECT MOP_MONEQUIV
          INTO V_CURRENCY
          FROM TGEN_MAPMONEDA
         WHERE MOP_CODINSTIT = 1 --BDI
           AND MOP_MONEDA = X.CURRENCY;
        EXCEPTION
         WHEN OTHERS THEN
           p_error := '1';
           P_MSGERROR := 'Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm;
		   --RAISE_APPLICATION_ERROR (-20101,'Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm);
        END;
       DBMS_OUTPUT.PUT_LINE('PASO 7');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_CURRENCY,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_CURRENCY,columntyp,columnlen,filling))
      INTO pcms_responses.cms_currency
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 7;
	--pcms_response.cms_currency := V_CURRENCY;
      DBMS_OUTPUT.PUT_LINE('PASO 8');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.reference,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.reference,columntyp,columnlen,filling))
      INTO pcms_responses.cms_reference
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 8;

		--pcms_response.cms_reference := x.reference;
       DBMS_OUTPUT.PUT_LINE('PASO 9');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.datetransaction,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.datetransaction,columntyp,columnlen,filling))
      INTO pcms_responses.cms_datetransaction
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 9;
		--pcms_response.cms_datetransaction   := x.datetransaction;
        ---LOS CODIGO DE LA INSTITUCION VAMOS A USAR COMO CODIGOS DE CANALES PARA ESTE CASO
        --2 POR CAJA
        --3 POR IVR
        --4 POR IB
		BEGIN
		SELECT MOP_MONEQUIV
          INTO V_CURRENCY_ORG
          FROM TGEN_MAPMONEDA
         WHERE MOP_CODINSTIT = p_canal --2 --BDI
           AND MOP_MONEDA = X.CURRENCY;
        EXCEPTION
         WHEN OTHERS THEN
           p_error := '1';
           P_MSGERROR :=' 2 Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm;
		  --RAISE_APPLICATION_ERROR (-20101,' 2 Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm);
        END;
       DBMS_OUTPUT.PUT_LINE('PASO 10');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_CURRENCY_ORG,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_CURRENCY_ORG,columntyp,columnlen,filling))
      INTO pcms_responses.cms_origmovement
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 10;
       DBMS_OUTPUT.PUT_LINE('PASO 10.1');
		--pcms_response.cms_origmovement := V_CURRENCY_ORG;
		BEGIN
		SELECT CODEISO
          INTO V_MODEPAY
          FROM TGEN_DESCTABLA
         WHERE DES_CODTAB = 502 --BDI
           AND DES_CODIGO = X.MODEPAYMENT;
        EXCEPTION
         WHEN OTHERS THEN
           p_error := '1';
           P_MSGERROR := '3 Error Moneda revise parametros 1/117 FS:'||' '||X.MODEPAYMENT||sqlerrm;
		   --RAISE_APPLICATION_ERROR (-20101,' 3 Error Moneda revise parametros 1/117 FS:'||' '||X.MODEPAYMENT||sqlerrm);
        END;
       DBMS_OUTPUT.PUT_LINE('PASO 11');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_MODEPAY,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_MODEPAY,columntyp,columnlen,filling))
      INTO pcms_responses.cms_modepayment
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 11;
		       --pcms_response.cms_modepayment := V_MODEPAY;
		/*BEGIN
		SELECT CODEISO
          INTO V_TYPELINE
          FROM TGEN_DESCTABLA
         WHERE DES_CODTAB = NVL(X.TABTIPPAGO,500)
           AND DES_CODIGO = NVL(X.TIPAG,1);*/
        /* SELECT RTRIM(LTRIM(PSP_VALORPARAM))
           INTO V_TYPELINE
           FROM TCAP_PAGOSERVICIOSPARAM
         WHERE  PSP_OPERADOR = X.USR --P_CODUSR
		    AND PSP_NUMTRAN  = X.NUMTRAN --P_NUMTRA
		    AND PSP_NUMPAGO = X.NUMPAGO
			AND PSP_SERVICIO = 5  --VISA
		    AND PSP_TABSERVICIO = 117
		    AND PSP_CODPARAM =  2 ;--tipo de pago
        EXCEPTION
         WHEN OTHERS THEN
		  RAISE_APPLICATION_ERROR (-20101,' 4 Error Moneda revise parametros 1/117 FS:'||sqlerrm);
        END;      */
       DBMS_OUTPUT.PUT_LINE('PASO 12');
    --ENCUENTRO EL TIPO DE PAGO
    BEGIN
    SELECT TYPEPAY
      INTO V_TIPPAG
      FROM TBINTYPEPAYMENT
     WHERE BIN = SUBSTR(X.CREDIT,1,6);
    EXCEPTION
      WHEN OTHERS THEN
          p_error := '1';
          P_MSGERROR :='BIN NO TIENE TIPO DE PAGO REVISE PARAMETROS..'||SQLERRM;
    END;
    --FIN

           V_TYPELINE := V_TIPPAG ; --X.TIPAG;
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_TYPELINE,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_TYPELINE,columntyp,columnlen,filling))
      INTO pcms_responses.cms_typeline
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 12;

	    --pcms_response.cms_typeline := V_TYPELINE;
		/*BEGIN
		SELECT CODEISO
          INTO V_CODELINE
          FROM TGEN_DESCTABLA
         WHERE DES_CODTAB = NVL(X.TABTIPLIN,501)
           AND DES_CODIGO = NVL(X.TIPLIN,1);*/
       /*  SELECT RTRIM(LTRIM(PSP_VALORPARAM))
           INTO V_CODELINE
           FROM TCAP_PAGOSERVICIOSPARAM
         WHERE  PSP_OPERADOR = X.USR --P_CODUSR
		    AND PSP_NUMTRAN  = X.NUMTRAN --P_NUMTRA
		    AND PSP_NUMPAGO = X.NUMPAGO
			AND PSP_SERVICIO = 5  --VISA
		    AND PSP_TABSERVICIO = 117
		    AND PSP_CODPARAM =  3 ;--tipo de pago

        EXCEPTION
         WHEN OTHERS THEN
		  RAISE_APPLICATION_ERROR (-20101,'Error Moneda revise parametros 1/117 FS:'||sqlerrm);
        END;           */
        IF V_TIPPAG = 'D' THEN
          V_CODELINE := 'CP';
        ELSE
          V_CODELINE := X.TIPLIN;
        END IF;
       DBMS_OUTPUT.PUT_LINE('PASO 13');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_CODELINE,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_CODELINE,columntyp,columnlen,filling))
      INTO pcms_responses.cms_codeline
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 13;

	--pcms_response.cms_codeline := V_CODELINE;
       DBMS_OUTPUT.PUT_LINE('PASO 14');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,X.OFFICE,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(X.OFFICE,columntyp,columnlen,filling))
      INTO pcms_responses.cms_office
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 14;

    --Para el valor los decimales van sin punto
    --·El campo valor del pago debe ser rellenado con ceros a la izquierda e incluir el monto
    --de la parte decimal sin el punto. Por ejemplo: para representar el valor 325.78
    --seria 00000032578. Si es 300.00 seria 00000030000.
	IF instr(X.VALUEPAGO,'.')  <> 0 THEN
       V_ENTERO  := substr(X.VALUEPAGO,1,instr(X.VALUEPAGO,'.') -1 ) ;
       V_DECIMAL := rpad(substr(X.VALUEPAGO,instr(X.VALUEPAGO,'.') +1,2),2,'0');
       V_TOTALVALOR := V_ENTERO||V_DECIMAL;
    ELSE
       V_ENTERO  := X.VALUEPAGO;
       V_DECIMAL := '00';
       V_TOTALVALOR := V_ENTERO||V_DECIMAL;
    END IF;

  --pcms_response.cms_office  := X.OFFICE;
    DBMS_OUTPUT.PUT_LINE('PASO 15');
  --SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,X.VALUE,formatcol),to_format(X.VALUE,columntyp,columnlen,filling))
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_TOTALVALOR,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(V_TOTALVALOR,columntyp,columnlen,filling))
      INTO pcms_responses.cms_value
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 15;

		--pcms_response.cms_value   := X.VALUE ;
       DBMS_OUTPUT.PUT_LINE('PASO 16');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,X.ID_TRANSACTION,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(X.ID_TRANSACTION,columntyp,columnlen,filling))
      INTO pcms_responses.cms_idtransaction
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 16;
		--pcms_response.cms_idtransaction  := X.ID_TRANSACTION ;
       DBMS_OUTPUT.PUT_LINE('PASO 17');
       --CUANDO ENVIE EL MENSAJE ESTE VA COMO ESTADO P PENDIENTE DE APLICAR
       v_estadoreg:= 'P';
      --en id reponse va el usuario y el numtra, idmessage lo asigna procecard
      --fin
     BEGIN
		INSERT INTO tresponsecredit(cms_idmessage ,cms_idrespose , cms_datesystem,cms_hoursystem,cms_user,
  					cms_credit, cms_currency, cms_reference , cms_datetransaction,
  					cms_origmovement, cms_modepayment,  cms_typeline ,cms_codeline,cms_OFFICE,
					cms_value, cms_idTransaction,cms_status,cms_codtrans   , cms_plazo  ,
					cms_numlote ,cms_numsec ,cms_referenciauniv, cms_numafiliado ,cms_autorizacion,cms_estadoreg,cms_enlinea )
		   VALUES(  pcms_responses.cms_idmessage,pcms_responses.cms_idrespose , pcms_responses.cms_datesystem,pcms_responses.cms_hoursystem,pcms_responses.cms_user,
  					pcms_responses.cms_credit, pcms_responses.cms_currency, pcms_responses.cms_reference , pcms_responses.cms_datetransaction,
  					pcms_responses.cms_origmovement, pcms_responses.cms_modepayment,  pcms_responses.cms_typeline ,pcms_responses.cms_codeline,pcms_responses.cms_OFFICE,
					pcms_responses.cms_value, pcms_responses.cms_idTransaction,NULL,x.codtra,0,X.TMO_SEC,X.TMO_SEC,X.TMO_REF,null,0,v_estadoreg,p_linea);
       DBMS_OUTPUT.PUT_LINE('PASO 18');

       ---UNA VEZ QUE ESPERE LA RESPUESTA SI ES OK SE ACTUALIZA EL CAMPO CON A , PERO SI SE RECHAZA O REVERSADO
        --ACTUALIZO TRAMON Y ESTA TABLA CON R, SINO SE APLICA PERO ES VALIDA
        -- LA OPERACION SE QUEDA COMO P PARA LA GENERACION DEL PLANO A ENVIAR.


       -- O SUCEDE ALGO QUE NO TENGA RESPUESTA
   EXCEPTION
     WHEN OTHERS THEN
          p_error := '1';
          P_MSGERROR :='ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM;
          --raise_application_error(-20195,'ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM);
   END;
   mensaje:=pcms_responses.cms_idmessage||pcms_responses.cms_idrespose||pcms_responses.cms_datesystem||pcms_responses.cms_hoursystem||
            pcms_responses.cms_user||pcms_responses.cms_credit||pcms_responses.cms_currency||pcms_responses.cms_reference||pcms_responses.cms_datetransaction||
            pcms_responses.cms_origmovement||pcms_responses.cms_modepayment||pcms_responses.cms_typeline||pcms_responses.cms_codeline||pcms_responses.cms_office||
            pcms_responses.cms_value||pcms_responses.cms_idtransaction;
    V_ENCONTRO := V_ENCONTRO + 1;
  END LOOP;
  IF V_ENCONTRO = 0 THEN
        p_error := '1';
        P_MSGERROR :=  'TRANSACCION NO ENOCNTRADA EN EL SISTEMA..U NT '||P_CODUSR||' '||P_NUMTRA||SQLERRM;
  END IF;
   EXCEPTION
     WHEN OTHERS THEN
        p_error := '1';
        P_MSGERROR := 'ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM;
        --raise_application_error(-20195,'ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM);
  END ;  
  /*--------------FIN RESPONSE MASIVO-----*/
  --PARA PAGO DESDE TIENDAS ANTHONY O CUALQUIER OTRA TIENDA
 PROCEDURE PCMS_RESPONSE_MM_ANT(p_codusr  IN NUMBER,
                         p_numtra  IN NUMBER,
                         p_sec IN NUMBER,
                         p_linea in VARCHAR2,
                         p_error   IN OUT VARCHAR2,
                         p_msgerror IN OUT VARCHAR2,
                         P_FORMAPAGO IN NUMBER, --EFECTIVO O CHEQUE
                         mensaje OUT VARCHAR2,
                         p_canal IN NUMBER,
                         P_CURRENCY IN NUMBER )
    IS
    CURSOR TRAMON IS
    SELECT /*+index CP01CAP_TMO*/
           TO_CHAR(A.TMO_FECHPROC,'YYYYMMDD') DATESYSTEM,
            TO_CHAR(A.TMO_FECHOR,'HH24MISS')   HOURSYSTEM,
            A.TMO_CODUSR USR,
            A.TMO_NUMTRA NUMTRAN,
            B.PGS_PARAMETRO CREDIT,
            --A.TMO_CODMON CURRENCY,
            P_CURRENCY CURRENCY,
            --A.TMO_NUMTRA REFERENCE,
            to_number(substr(lpad(to_char(A.TMO_NUMTRA),7,'0'),3,5)||substr(lpad(to_char(A.tmo_sec),5,'0'),4,2)) REFERENCE,
            A.TMO_FECHCON DATETRANSACTION,
            A.TMO_CODTRA  CODTRA,
            A.TMO_CODMOD CODMOD,
            --A.TMO_TIPORUB MODEPAYMENT,
            --A.TMO_RUBRO MODEPAYMENT,
            P_FORMAPAGO MODEPAYMENT,--SE COMENTA PARA QUE SEA EFECTO O CHEQUE POR LOS PAGOS DESDE TIENDAS
            '501' TABTIPPAGO,
            B.PGS_PARAMETRO2 TIPAG,
            '502' TABTIPLIN,
            B.PGS_PARAMETRO3  TIPLIN,
            A.TMO_CODSUC  OFFICE,
            --A.TMO_VAL  VALUEPAGO, --B.PGS_MONTO  VALUE, --A.TMO_VAL--SE COMENTA PORQUE SIEMPRE ES DEBITO
            B.PGS_MONTO  VALUEPAGO, --B.PGS_MONTO  VALUE, --A.TMO_VAL            
            DECODE(A.TMO_MODO,'N','C',A.TMO_MODO) ID_TRANSACTION,
            A.TMO_SEC,
            A.TMO_REF
            --B.PGS_NUMPAGO NUMPAGO
      FROM TCAP_TRAMON A,TCAP_PAGOSERVICIOS B
      WHERE A.TMO_CODUSR = B.PGS_OPERADOR
        AND A.TMO_NUMTRA = B.PGS_NUMTRAN
        --AND A.TMO_FECHCON = P_FECHA
        AND A.TMO_CODUSR = P_CODUSR
        AND A.TMO_NUMTRA = P_NUMTRA
        AND A.TMO_SEC = P_SEC
        AND A.TMO_CODTRA <> 4
        AND A.TMO_MODO = 'N'
    ORDER BY A.TMO_SEC;
    V_CURRENCY        TGEN_MAPMONEDA.MOP_MONEQUIV%TYPE;
    V_CURRENCY_ORG    TGEN_MAPMONEDA.MOP_MONEQUIV%TYPE;
    V_MODEPAY         TGEN_DESCTABLA.CODEISO%TYPE;
    V_TYPELINE        TGEN_DESCTABLA.CODEISO%TYPE;
    V_CODELINE        TGEN_DESCTABLA.CODEISO%TYPE;
    V_TIPPAG          TCAP_PAGOSERVICIOS.PGS_PARAMETRO2%TYPE;
    v_estadoreg       VARCHAR2(1):='P';
    V_ENTERO          VARCHAR2(11);
    V_DECIMAL         VARCHAR2(2);
    V_TOTALVALOR      VARCHAR2(11);
    pcms_responses    CMS_RESPONSE;
    V_ENCONTRO        NUMBER:=0;
    V_USRTRA          VARCHAR2(16);
    V_USUARIOIB       VARCHAR2(10);
    BEGIN
    p_error := '0';
    V_ENCONTRO :=0;
    pcms_responses:=cms_response(null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);
    FOR X IN TRAMON LOOP
        V_CURRENCY := 0;
        V_CURRENCY_ORG := 0;
        V_MODEPAY := NULL;
        V_TYPELINE := 0;
        V_CODELINE := 0;
        V_TIPPAG := NULL;
      DBMS_OUTPUT.PUT_LINE('PASO 1');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,'MSG020025X',formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR('MSG020025X',columntyp,columnlen,filling))
      INTO pcms_responses.cms_idmessage
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 1;
       DBMS_OUTPUT.PUT_LINE('PASO 2');
    --pcms_response.cms_idmessage := '1';
    --
      --V_USRTRA := LPAD(X.USR,4,'0')||LPAD(X.NUMTRAN,7,'0');
      V_USRTRA := X.USR||X.NUMTRAN||X.TMO_SEC;
    --
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_USRTRA,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_USRTRA,columntyp,columnlen,filling))
      INTO pcms_responses.cms_idrespose
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 2;
      	--pcms_response.cms_idrespose := '1';
       DBMS_OUTPUT.PUT_LINE('PASO 3');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.datesystem,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.datesystem,columntyp,columnlen,filling))
      INTO pcms_responses.cms_datesystem
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 3;
      	--pcms_response.cms_datesystem := x.datesystem;
       DBMS_OUTPUT.PUT_LINE('PASO 4');
    SELECT decode(columntyp,'D', pkg_legalreport3.obt_formatvalue(columntyp,x.hoursystem,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.hoursystem,columntyp,columnlen,filling))
      INTO pcms_responses.cms_hoursystem
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 4;
		--pcms_response.cms_hoursystem := x.hoursystem;
       DBMS_OUTPUT.PUT_LINE('PASO 5');
    V_USUARIOIB := LPAD(X.USR,6,'0');
    V_USUARIOIB :='EBNK'||V_USUARIOIB;
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_USUARIOIB,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_USUARIOIB,columntyp,columnlen,filling))
      INTO pcms_responses.cms_user
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 5;

		--pcms_response.cms_user  := x.usr;
       DBMS_OUTPUT.PUT_LINE('PASO 6');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.credit,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(x.credit,columntyp,columnlen,filling))
      INTO pcms_responses.cms_credit
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 6;
		--pcms_response.cms_credit := x.credit;
		BEGIN
		SELECT MOP_MONEQUIV
          INTO V_CURRENCY
          FROM TGEN_MAPMONEDA
         WHERE MOP_CODINSTIT = 1 --BDI
           AND MOP_MONEDA = X.CURRENCY;
        EXCEPTION
         WHEN OTHERS THEN
           p_error := '1';
           P_MSGERROR := 'Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm;
		   --RAISE_APPLICATION_ERROR (-20101,'Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm);
        END;
       DBMS_OUTPUT.PUT_LINE('PASO 7');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_CURRENCY,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_CURRENCY,columntyp,columnlen,filling))
      INTO pcms_responses.cms_currency
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 7;
	--pcms_response.cms_currency := V_CURRENCY;
      DBMS_OUTPUT.PUT_LINE('PASO 8');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.reference,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.reference,columntyp,columnlen,filling))
      INTO pcms_responses.cms_reference
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 8;

		--pcms_response.cms_reference := x.reference;
       DBMS_OUTPUT.PUT_LINE('PASO 9');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,x.datetransaction,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(x.datetransaction,columntyp,columnlen,filling))
      INTO pcms_responses.cms_datetransaction
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 9;
		--pcms_response.cms_datetransaction   := x.datetransaction;
        ---LOS CODIGO DE LA INSTITUCION VAMOS A USAR COMO CODIGOS DE CANALES PARA ESTE CASO
        --2 POR CAJA
        --3 POR IVR
        --4 POR IB
		BEGIN
		SELECT MOP_MONEQUIV
          INTO V_CURRENCY_ORG
          FROM TGEN_MAPMONEDA
         WHERE MOP_CODINSTIT = p_canal --2 --BDI
           AND MOP_MONEDA = X.CURRENCY;
        EXCEPTION
         WHEN OTHERS THEN
           p_error := '1';
           P_MSGERROR :=' 2 Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm;
		  --RAISE_APPLICATION_ERROR (-20101,' 2 Error Moneda revise parametros 1/117 FS:'||X.CURRENCY||'-'||sqlerrm);
        END;
       DBMS_OUTPUT.PUT_LINE('PASO 10');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_CURRENCY_ORG,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_CURRENCY_ORG,columntyp,columnlen,filling))
      INTO pcms_responses.cms_origmovement
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 10;
       DBMS_OUTPUT.PUT_LINE('PASO 10.1');
		--pcms_response.cms_origmovement := V_CURRENCY_ORG;
		BEGIN
		SELECT CODEISO
          INTO V_MODEPAY
          FROM TGEN_DESCTABLA
         WHERE DES_CODTAB = 502 --BDI
           AND DES_CODIGO = X.MODEPAYMENT;
        EXCEPTION
         WHEN OTHERS THEN
           p_error := '1';
           P_MSGERROR := '3 Error Moneda revise parametros 1/117 FS:'||' '||X.MODEPAYMENT||sqlerrm;
		   --RAISE_APPLICATION_ERROR (-20101,' 3 Error Moneda revise parametros 1/117 FS:'||' '||X.MODEPAYMENT||sqlerrm);
        END;
       DBMS_OUTPUT.PUT_LINE('PASO 11');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_MODEPAY,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_MODEPAY,columntyp,columnlen,filling))
      INTO pcms_responses.cms_modepayment
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 11;
		       --pcms_response.cms_modepayment := V_MODEPAY;
		/*BEGIN
		SELECT CODEISO
          INTO V_TYPELINE
          FROM TGEN_DESCTABLA
         WHERE DES_CODTAB = NVL(X.TABTIPPAGO,500)
           AND DES_CODIGO = NVL(X.TIPAG,1);*/
        /* SELECT RTRIM(LTRIM(PSP_VALORPARAM))
           INTO V_TYPELINE
           FROM TCAP_PAGOSERVICIOSPARAM
         WHERE  PSP_OPERADOR = X.USR --P_CODUSR
		    AND PSP_NUMTRAN  = X.NUMTRAN --P_NUMTRA
		    AND PSP_NUMPAGO = X.NUMPAGO
			AND PSP_SERVICIO = 5  --VISA
		    AND PSP_TABSERVICIO = 117
		    AND PSP_CODPARAM =  2 ;--tipo de pago
        EXCEPTION
         WHEN OTHERS THEN
		  RAISE_APPLICATION_ERROR (-20101,' 4 Error Moneda revise parametros 1/117 FS:'||sqlerrm);
        END;      */
       DBMS_OUTPUT.PUT_LINE('PASO 12');
    --ENCUENTRO EL TIPO DE PAGO
    BEGIN
    SELECT TYPEPAY
      INTO V_TIPPAG
      FROM TBINTYPEPAYMENT
     WHERE BIN = SUBSTR(X.CREDIT,1,6);
    EXCEPTION
      WHEN OTHERS THEN
          p_error := '1';
          P_MSGERROR :='BIN NO TIENE TIPO DE PAGO REVISE PARAMETROS..'||SQLERRM;
    END;
    --FIN

           V_TYPELINE := V_TIPPAG ; --X.TIPAG;
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_TYPELINE,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_TYPELINE,columntyp,columnlen,filling))
      INTO pcms_responses.cms_typeline
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 12;

	    --pcms_response.cms_typeline := V_TYPELINE;
		/*BEGIN
		SELECT CODEISO
          INTO V_CODELINE
          FROM TGEN_DESCTABLA
         WHERE DES_CODTAB = NVL(X.TABTIPLIN,501)
           AND DES_CODIGO = NVL(X.TIPLIN,1);*/
       /*  SELECT RTRIM(LTRIM(PSP_VALORPARAM))
           INTO V_CODELINE
           FROM TCAP_PAGOSERVICIOSPARAM
         WHERE  PSP_OPERADOR = X.USR --P_CODUSR
		    AND PSP_NUMTRAN  = X.NUMTRAN --P_NUMTRA
		    AND PSP_NUMPAGO = X.NUMPAGO
			AND PSP_SERVICIO = 5  --VISA
		    AND PSP_TABSERVICIO = 117
		    AND PSP_CODPARAM =  3 ;--tipo de pago

        EXCEPTION
         WHEN OTHERS THEN
		  RAISE_APPLICATION_ERROR (-20101,'Error Moneda revise parametros 1/117 FS:'||sqlerrm);
        END;           */
        IF V_TIPPAG = 'D' THEN
          V_CODELINE := 'CP';
        ELSE
          V_CODELINE := X.TIPLIN;
        END IF;
       DBMS_OUTPUT.PUT_LINE('PASO 13');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_CODELINE,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(V_CODELINE,columntyp,columnlen,filling))
      INTO pcms_responses.cms_codeline
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 13;

	--pcms_response.cms_codeline := V_CODELINE;
       DBMS_OUTPUT.PUT_LINE('PASO 14');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,X.OFFICE,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(X.OFFICE,columntyp,columnlen,filling))
      INTO pcms_responses.cms_office
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 14;

    --Para el valor los decimales van sin punto
    --·El campo valor del pago debe ser rellenado con ceros a la izquierda e incluir el monto
    --de la parte decimal sin el punto. Por ejemplo: para representar el valor 325.78
    --seria 00000032578. Si es 300.00 seria 00000030000.
	IF instr(X.VALUEPAGO,'.')  <> 0 THEN
       V_ENTERO  := substr(X.VALUEPAGO,1,instr(X.VALUEPAGO,'.') -1 ) ;
       V_DECIMAL := rpad(substr(X.VALUEPAGO,instr(X.VALUEPAGO,'.') +1,2),2,'0');
       V_TOTALVALOR := V_ENTERO||V_DECIMAL;
    ELSE
       V_ENTERO  := X.VALUEPAGO;
       V_DECIMAL := '00';
       V_TOTALVALOR := V_ENTERO||V_DECIMAL;
    END IF;

  --pcms_response.cms_office  := X.OFFICE;
    DBMS_OUTPUT.PUT_LINE('PASO 15');
  --SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,X.VALUE,formatcol),to_format(X.VALUE,columntyp,columnlen,filling))
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,V_TOTALVALOR,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_format(V_TOTALVALOR,columntyp,columnlen,filling))
      INTO pcms_responses.cms_value
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 15;

		--pcms_response.cms_value   := X.VALUE ;
       DBMS_OUTPUT.PUT_LINE('PASO 16');
    SELECT decode(columntyp,'D',pkg_legalreport3.obt_formatvalue(columntyp,X.ID_TRANSACTION,formatcol),PKG_CMS_INTERFAZCREDITCARD.to_formatR(X.ID_TRANSACTION,columntyp,columnlen,filling))
      INTO pcms_responses.cms_idtransaction
      FROM TTYPECOLUMNREPORT
     WHERE  REPORT = 'CMS001'
       AND COLUMNID = 16;
		--pcms_response.cms_idtransaction  := X.ID_TRANSACTION ;
       DBMS_OUTPUT.PUT_LINE('PASO 17');
       --CUANDO ENVIE EL MENSAJE ESTE VA COMO ESTADO P PENDIENTE DE APLICAR
       v_estadoreg:= 'P';
      --en id reponse va el usuario y el numtra, idmessage lo asigna procecard
      --fin
     BEGIN
		INSERT INTO tresponsecredit(cms_idmessage ,cms_idrespose , cms_datesystem,cms_hoursystem,cms_user,
  					cms_credit, cms_currency, cms_reference , cms_datetransaction,
  					cms_origmovement, cms_modepayment,  cms_typeline ,cms_codeline,cms_OFFICE,
					cms_value, cms_idTransaction,cms_status,cms_codtrans   , cms_plazo  ,
					cms_numlote ,cms_numsec ,cms_referenciauniv, cms_numafiliado ,cms_autorizacion,cms_estadoreg,cms_enlinea )
		   VALUES(  pcms_responses.cms_idmessage,pcms_responses.cms_idrespose , pcms_responses.cms_datesystem,pcms_responses.cms_hoursystem,pcms_responses.cms_user,
  					pcms_responses.cms_credit, pcms_responses.cms_currency, pcms_responses.cms_reference , pcms_responses.cms_datetransaction,
  					pcms_responses.cms_origmovement, pcms_responses.cms_modepayment,  pcms_responses.cms_typeline ,pcms_responses.cms_codeline,pcms_responses.cms_OFFICE,
					pcms_responses.cms_value, pcms_responses.cms_idTransaction,NULL,x.codtra,0,X.TMO_SEC,X.TMO_SEC,X.TMO_REF,null,0,v_estadoreg,p_linea);
       DBMS_OUTPUT.PUT_LINE('PASO 18');

       ---UNA VEZ QUE ESPERE LA RESPUESTA SI ES OK SE ACTUALIZA EL CAMPO CON A , PERO SI SE RECHAZA O REVERSADO
        --ACTUALIZO TRAMON Y ESTA TABLA CON R, SINO SE APLICA PERO ES VALIDA
        -- LA OPERACION SE QUEDA COMO P PARA LA GENERACION DEL PLANO A ENVIAR.


       -- O SUCEDE ALGO QUE NO TENGA RESPUESTA
   EXCEPTION
     WHEN OTHERS THEN
          p_error := '1';
          P_MSGERROR :='ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM;
          --raise_application_error(-20195,'ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM);
   END;
   mensaje:=pcms_responses.cms_idmessage||pcms_responses.cms_idrespose||pcms_responses.cms_datesystem||pcms_responses.cms_hoursystem||
            pcms_responses.cms_user||pcms_responses.cms_credit||pcms_responses.cms_currency||pcms_responses.cms_reference||pcms_responses.cms_datetransaction||
            pcms_responses.cms_origmovement||pcms_responses.cms_modepayment||pcms_responses.cms_typeline||pcms_responses.cms_codeline||pcms_responses.cms_office||
            pcms_responses.cms_value||pcms_responses.cms_idtransaction;
    V_ENCONTRO := V_ENCONTRO + 1;
  END LOOP;
  IF V_ENCONTRO = 0 THEN
        p_error := '1';
        P_MSGERROR :=  'TRANSACCION NO ENOCNTRADA EN EL SISTEMA..U NT '||P_CODUSR||' '||P_NUMTRA||SQLERRM;
  END IF;
   EXCEPTION
     WHEN OTHERS THEN
        p_error := '1';
        P_MSGERROR := 'ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM;
        --raise_application_error(-20195,'ERROR AL OBTENER DATOS PARA TRANSMITIR..'||SQLERRM);
  END ;  
  --FIN DE PAGOS DESDE TIENDAS
  
--fin para mm
END;
/
