CREATE OR REPLACE PACKAGE BODY pkg_legal_dom_de AS
  --Modificado el 2004/07/23
  gln_secuencia number := 0; -- HD 8164

  PROCEDURE DE04(p_report   VARCHAR2,
                 p_session  NUMBER,
                 p_sucursal NUMBER, -- Sucursal 0 para que me haga de todas
                 p_moneda   NUMBER,
                 p_datefrom DATE,
                 p_fecha    date,
                 p_tiporel  number) IS
    CODIGO       NUMBER := 1;
    p_identifica VARCHAR2(15);
    p_tipoper    char(2);
    p_nombre     VARCHAR2(60);
    p_apellido   VARCHAR2(60);
    errtext      varchar2(50);
    w_msg        VARCHAR2(50);
    w_outproc EXCEPTION;

    CURSOR CODEUDORES IS
      select pre_credito, clc_codcli, codeiso,pre_moneda--abril 2018
        from tpre_prestamos, TCLI_CLICTA, tgen_desctabla, tcli_persona
       where trunc(pre_fecontab) <= p_fecha and
             pre_status not in ('0', 'D', 'L', 'F', 'A') and
             nvl(pre_fecance, p_fecha + 1) >= p_fecha and pre_mod = clc_mod and
             pre_credito = clc_cue and clc_tiporel = p_tiporel and
             des_codtab = pre_tabtipocredito and
             des_codigo = pre_tipocredito and 
             cli_codigo = clc_codcli;
  w_tipo_credito varchar2(1);
  w_capital number:=0;
  w_interes number:=0;
  BEGIN

    IF (P_DATEFROM <= P_FECHA) AND (P_FECHA <= F_FECHATRABAJO) THEN
      pkg_legalreport.eraser(p_report, p_session);
      COMMIT;
      FOR I IN CODEUDORES LOOP
        w_tipo_credito:=codtipo_cartera(p_fecha,'P',I.pre_credito);
        --capital e interes
           select sum(nvl(lca_capital,0)),sum(nvl(lca_interes,0))
             into w_capital,w_interes
           from tleg_operacioncalif
           where lca_fecha = p_fecha
             and lca_cuenta = I.pre_credito; 
        --
        pkg_legalreport3.DATOS_CLIENTE(I.CLC_codcli,
                                       p_identifica,
                                       p_tipoper,
                                       p_nombre,
                                       p_apellido);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 1, CODIGO, 1, CODIGO);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 2, CODIGO, 2, I.pre_credito);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 3, CODIGO, 3, p_identifica);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 4, CODIGO, 4, p_tipoper);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 5, CODIGO, 5, p_nombre);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 6, CODIGO, 6, p_apellido);
        --INSERT INTO tvaluelevelcustom
          --(REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        --VALUES
          --(p_report, p_session, 7, CODIGO, 7, I.codeiso);          
          --junio 2015
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 7, CODIGO, 7, w_tipo_credito);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 8, CODIGO, 8, w_capital);
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 9, CODIGO, 9, w_interes);

       --abril 2018
       INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 10, CODIGO, 10, decode(i.pre_moneda,0,'N','E'));
                    
          
        CODIGO := CODIGO + 1;
      END LOOP;
      commit;
      pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
    ELSE
      errtext := 'Fechas no válidas';
      raise_application_error(-20901, errtext);
    END IF;

  EXCEPTION
    WHEN w_outproc THEN
      RAISE_APPLICATION_ERROR(-20901, w_msg);

  END;

  ------------------

  -- APGC 2006/08/01
  FUNCTION fgetheader(adidate date) RETURN VARCHAR2 IS
    lcheader VARCHAR2(20);
  BEGIN
    SELECT to_char(adidate, 'DDMMYYYY') || 'BC' ||
            lpad(suc_codsuper, 3, '0')
      INTO lcheader
      FROM tgen_sucursal
     WHERE suc_codigo = 0;

    RETURN lcheader;

  EXCEPTION
    WHEN OTHERS THEN
      raise_application_error(-20801,
                              'Código de SIB para Sucursal 0(cero) no parametrizada.');
  END;

FUNCTION codtipo_cartera(
          p_fecha in date,
          p_tipo in varchar2,
          p_cuenta in number)
        RETURN varchar2 is
tipocredito varchar2(1);
begin
  select lca_codtipocredito
    into tipocredito
   from tleg_operacioncalif
   where lca_tipo = p_tipo
    and lca_cuenta = p_cuenta
    and lca_fecha =  (select max(lca_fecha)
    	               from tleg_operacioncalif
    	               where lca_fecha <= p_fecha
    	                and lca_tipo = p_tipo
    	                and lca_cuenta = p_cuenta);
   return tipocredito;
exception
 when others then
  return null;
end;

  ---------------------------------------------------------------------------------------CA01DIA
  PROCEDURE DE03(p_report       VARCHAR2, -- Para insertar y eliminar datos en tvaluelevelcustom
                    p_session      NUMBER, -- Para insertar y eliminar datos en tvaluelevelcust
                    p_datefrom     DATE, -- Fecha inicial de consulta.
                    p_dateto       DATE, -- Fecha final de consulta. Debe ser igual a fecha inicial
                    p_tiporel      number, -- Se utiliza para recibir el tipo de relacion desde 90/7005
                    p_tiporel1     number, -- Se utiliza para recibir el tipo de relacion desde 90/7005 se aumenta porque hay otras relaciones de solidarios oLL
                    p_tiporel2     number, -- Se utiliza para recibir el tipo de relacion desde 90/7005
                    p_tiporel3     number, -- Se utiliza para recibir el tipo de relacion desde 90/7005
                    p_tiporel4     number, -- Se utiliza para recibir el tipo de relacion desde 90/7005
                    p_tipogarid1   VARCHAR2, -- Se utiliza para saber con que tipos de garantia se despliega el tipoid P1, P2, E1 ya q' no son para todas las garantias
                    p_tipogarid2   VARCHAR2, -- Se utiliza para saber con que tipos de garantia se despliega el tipoid P1, P2, E1 ya q' no son para todas las garantias
                    p_tipogarid3   VARCHAR2, -- Se utiliza para saber con que tipos de garantia se despliega el tipoid P1, P2, E1 ya q' no son para todas las garantias
                    p_decimales    number, -- Número de decimales
                    p_montocompara number, --para determinar mayor o menor deudor
                    P_TIPOCREDITO1 NUMBER,
                    p_tipocredito2 NUMBER,
                    P_TIPOCREDITO3 NUMBER) IS

    p_montoriesgo      number;
    v_codeiso          varchar2(2);
    CODIGO             NUMBER := 1; --contador de numero secuencial (1)
    p_identifica       VARCHAR2(15);
    p_tipoper          CHAR(2);
    p_tipoper_cli      NUMBER;
    p_nombre           VARCHAR2(60);
    p_apellido         VARCHAR2(60);
    p_garante16        VARCHAR2(60);
    p_garante17        VARCHAR2(60);
    p_tipogar          VARCHAR2(2);
    gar_id             VARCHAR2(16); --HD4324_2 (ESTABA DEFINIDA DE TAMAÑO 15
    existe             CHAR;
    p_fechat           DATE;
    p_valor            NUMBER;
    p_valores          VARCHAR2(1);
    p_noformato        VARCHAR2(1);
    p_tipvalor         NUMBER(3);
    p_descripcion      VARCHAR2(4000); --HD4324_2 (ESTABA DEFINIDA DE TAMAÑO 250
    p_identificaemisor VARCHAR2(15);
    p_seguro           VARCHAR2(1);
    p_fechavencepoliza DATE;
    p_esplazo          VARCHAR2(1);
    p_esplazodelbanco  VARCHAR2(1);
    p_valorpignorado   tcli_pignora.pig_valor%TYPE;
    p_ruc              tgen_instit.ins_ruc%TYPE;
    p_fechaconst       tcli_opergaran.OGR_FECPROT%type;
    p_relacion         tcli_clicta.clc_tiporel%TYPE;
    p_esreal           VARCHAR2(1) := 'N';
    TXT_TIPOREL        TGEN_DESCTABLA.DES_DESCRIPCION%TYPE;
    TXT_CODEISO        TGEN_DESCTABLA.CODEISO%TYPE;
    v_tipoper          tcli_persona.cli_tipoper%type;
    vld_ogr_fecha    date; --octubre 2008
    v_anio number;
    garantia_adm number;
    /* Selecciona todas las garantias por crédito (mod=6) que se encuentran en Tcli_opergaran con otros datos
    de la garantia a la fecha, de creditos y garantias no vencidas*/

    CURSOR GARANTIAS IS
      SELECT OGR_MOD,
             OGR_CLIENTE,
             OGR_OPERACION,
             PRE_TIPOCREDITO,
             OGR_NUMGARAN,
             substr(CODEISO, 1, 1) CODEISO, --para tipo de credito,
             CLI_TIPOPER,
             GAR_TIPBIEN,
             GAR_DESCRIP,
             OGR_GRADO,
             OGR_FECPROT,
             OGR_FECHA,
             OGR_VALOR,
             OGR_FECDESDE,
             BXG_FORMATO,
             gar_amparapoliza,
             PRE_FECEMI,
             PRE_CLIENTEP, --2003/03/06 se aumenta el campo gar_amparapoliza
             GAR_MONEDA
        FROM TCLI_OPERGARAN      A,
             TPRE_PRESTAMOS      B,
             TGEN_DESCTABLA      C,
             TCLI_PERSONA        D,
             TGAR_GARANTIAS      E,
             TGEN_BIENXGAR       F,
             TPRE_PROTIPOCREDITO G
       WHERE A.OGR_MOD = B.PRE_MOD AND
             A.OGR_OPERACION = B.PRE_CREDITO AND
             B.PRE_MOD = G.PTC_MOD AND
             B.PRE_PRO = G.PTC_PRO AND
             B.PRE_TIP = G.PTC_TIP AND
             B.PRE_MONEDA = G.PTC_MON AND
             G.PTC_TABTIPOCREDITO = C.DES_CODTAB AND
             G.PTC_TIPOCREDITO = C.DES_CODIGO AND
             A.OGR_CLIENTE = D.CLI_CODIGO AND
             A.OGR_CLIENTE = E.GAR_CODCLI AND
             A.OGR_NUMGARAN = E.GAR_NUMERO AND
             E.GAR_TIPGAR = F.BXG_TIPGAR AND
             E.GAR_TIPBIEN = F.BXG_TIPBIEN AND
             B.PRE_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
             TRUNC(B.PRE_FECONTAB) <= p_dateto AND
             NVL(B.PRE_FECANCE, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) > TO_DATE(p_dateto, 'YYYY/MM/DD')
             AND nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_dateto  --IS NULL
             AND ogr_fecdesde IS  NOT NULL
             AND gar_fecdesde IS NOT NULL
             AND nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_dateto --HD4324_2
 UNION--SOBREGIROS CONTRATADOS
     SELECT OGR_MOD,
             OGR_CLIENTE,
             OGR_OPERACION,
             PTC_TIPOCREDITO PRE_TIPOCREDITO,
             OGR_NUMGARAN,
             substr(CODEISO, 1, 1) CODEISO, --para tipo de credito,
             CLI_TIPOPER,
             GAR_TIPBIEN,
             GAR_DESCRIP,
             OGR_GRADO,
             OGR_FECPROT,
             OGR_FECHA,
             OGR_VALOR,
             OGR_FECDESDE,
             BXG_FORMATO,
             gar_amparapoliza,
             CRD_FECHAVIG PRE_FECEMI,
             VIS_CODCLI PRE_CLIENTEP, --2003/03/06 se aumenta el campo gar_amparapoliza
             GAR_MONEDA
        FROM TCLI_OPERGARAN      A,
             TCAP_VISTA       B,--TPRE_PRESTAMOS
             TCAP_CREDVISTA   H,
             TCAP_ACRACT      I,
             TGEN_DESCTABLA      C,
             TCLI_PERSONA        D,
             TGAR_GARANTIAS      E,
             TGEN_BIENXGAR       F,
             TPRE_PROTIPOCREDITO G
       WHERE A.OGR_MOD = B.VIS_MOD AND
             A.OGR_OPERACION = B.VIS_NUMCUE AND
             B.VIS_NUMCUE = H.CRD_NUMCUE    AND
             H.CRD_NUMCUE = I.ACA_NUMCUE    AND
             H.CRD_TIPOCRED = I.ACA_TIPOCRE AND
             H.CRD_SECUENCIA = I.ACA_SECUENCIA AND
             H.CRD_TIPOCRED = 2 AND
             I.ACA_FECHA = p_dateto AND
             B.VIS_MOD = G.PTC_MOD AND
             B.VIS_PRO = G.PTC_PRO AND
             B.VIS_TIP = G.PTC_TIP AND
             B.VIS_MONEDA = G.PTC_MON AND
             G.PTC_TABTIPOCREDITO = C.DES_CODTAB AND
             G.PTC_TIPOCREDITO = C.DES_CODIGO AND
             A.OGR_CLIENTE = D.CLI_CODIGO AND
             A.OGR_CLIENTE = E.GAR_CODCLI AND
             A.OGR_NUMGARAN = E.GAR_NUMERO AND
             E.GAR_TIPGAR = F.BXG_TIPGAR AND
             E.GAR_TIPBIEN = F.BXG_TIPBIEN AND
             B.VIS_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
             TRUNC(B.VIS_FECHAPE) <= p_dateto AND
               trunc(crd_fechautor) <= p_dateto
	         and  crd_fechavenc >= p_dateto
			 and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > p_dateto
			 and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > p_dateto
             and NVL(B.VIS_FECHCIERR, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) > TO_DATE(p_dateto, 'YYYY/MM/DD')
             AND nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_dateto  --IS NULL
             AND ogr_fecdesde IS  NOT NULL
             AND gar_fecdesde IS NOT NULL
             AND nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_dateto; --HD4324_2
    --AND p_dateto BETWEEN A.OGR_FECDESDE AND NVL(A.OGR_FECHASTA, TO_DATE('2199/12/31', 'yyyy/mm/dd')); SE SOLICITA
    --DESPLEGAR LAS GARANTIAS QUE NO ESTEN INSCRITAS O LEGALIZADAS

    /* Selecciona todos los garantes personales a la fecha de créditos
    no vencidos de la tabla tcli_clicta, es decir mod=6 y tiporel=9*/

    CURSOR GARANTES IS
      SELECT CLC_MOD OGR_MOD,
             CLC_CUE,
             CLC_CUE OGR_OPERACION,
             CLC_CODCLI,
             CLC_CODCLI OGR_CLIENTE,
             PRE_STATUS,
             C.CODEISO,
             NVL(PRE_FECORIGINAL, PRE_FECEMI) PRE_FECEMI,
             CLC_TIPOREL,
             1 OGR_NUMGARAN
        FROM TCLI_CLICTA         A,
             TPRE_PRESTAMOS      B,
             TGEN_DESCTABLA      C,
             TGEN_DESCTABLA      D,
             TPRE_PROTIPOCREDITO G
       WHERE A.CLC_MOD = B.PRE_MOD AND
            --A.CLC_TIPOREL  IN (p_tiporel,p_tiporel1,p_tiporel2,p_tiporel3,p_tiporel4) AND
            --A.CLC_TIPOREL  NOT IN (28,29) AND
             A.CLC_CUE = B.PRE_CREDITO AND B.PRE_MOD = G.PTC_MOD AND
             B.PRE_PRO = G.PTC_PRO AND B.PRE_TIP = G.PTC_TIP AND
             B.PRE_MONEDA = G.PTC_MON AND
             C.DES_CODTAB = G.PTC_TABTIPOCREDITO AND
             C.DES_CODIGO = G.PTC_TIPOCREDITO AND
             D.DES_CODTAB = A.CLC_CODTAB AND D.DES_CODIGO = A.CLC_TIPOREL AND
             NVL(D.CODEISO, 'G3') = 'S4' AND
             B.PRE_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
             TRUNC(B.PRE_FECONTAB) <= p_dateto AND
             NVL(B.PRE_FECANCE, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_dateto, 'YYYY/MM/DD') --HD4324_2
 UNION
       SELECT CLC_MOD OGR_MOD,
             CLC_CUE,
             CLC_CUE OGR_OPERACION,
             CLC_CODCLI,
             CLC_CODCLI OGR_CLIENTE,             
             VIS_STATUS PRE_STATUS,
             C.CODEISO,
             CRD_FECHAVIG PRE_FECEMI,
             CLC_TIPOREL,
             1 OGR_NUMGARAN
        FROM TCLI_CLICTA         A,
             TCAP_VISTA      B, --TPRE_PRESTAMOS
             TCAP_CREDVISTA   H,
             TCAP_ACRACT      I,
             TGEN_DESCTABLA      C,
             TGEN_DESCTABLA      D,
             TPRE_PROTIPOCREDITO G
       WHERE A.CLC_MOD = B.VIS_MOD AND
             A.CLC_CUE = B.VIS_NUMCUE AND
             B.VIS_NUMCUE = H.CRD_NUMCUE    AND
             H.CRD_NUMCUE = I.ACA_NUMCUE    AND
             H.CRD_TIPOCRED = I.ACA_TIPOCRE AND
             H.CRD_SECUENCIA = I.ACA_SECUENCIA AND
             H.CRD_TIPOCRED = 2 AND
             I.ACA_FECHA = p_dateto AND
             B.VIS_MOD = G.PTC_MOD AND
             B.VIS_PRO = G.PTC_PRO AND
             B.VIS_TIP = G.PTC_TIP AND
             B.VIS_MONEDA = G.PTC_MON AND
             C.DES_CODTAB = G.PTC_TABTIPOCREDITO AND
             C.DES_CODIGO = G.PTC_TIPOCREDITO AND
             D.DES_CODTAB = 33 AND
             D.DES_CODIGO = H.CRD_TIPOGAR AND
             NVL(D.CODEISO, 'G3') = 'S4' AND
             B.VIS_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
                         trunc(crd_fechautor) <= p_dateto
					   and  crd_fechavenc >= p_dateto
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > p_dateto
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > p_dateto
             and TRUNC(B.VIS_FECHAPE) <= p_dateto AND
             NVL(B.VIS_FECHCIERR, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_dateto, 'YYYY/MM/DD'); --HD4324_2

    --UNION
    CURSOR GARANTESG3 IS
      SELECT CLC_MOD OGR_MOD,
             CLC_CUE,        
             CLC_CUE OGR_OPERACION,
             CLC_CODCLI,            
             CLC_CODCLI OGR_CLIENTE,             
             PRE_STATUS,
             C.CODEISO,
             NVL(PRE_FECORIGINAL, PRE_FECEMI) PRE_FECEMI,
             CLC_TIPOREL,
             1 OGR_NUMGARAN
        FROM TCLI_CLICTA         A,
             TPRE_PRESTAMOS      B,
             TGEN_DESCTABLA      C,
             TGEN_DESCTABLA      D,
             TPRE_PROTIPOCREDITO G
       WHERE A.CLC_MOD = B.PRE_MOD AND A.CLC_CUE = B.PRE_CREDITO AND
             B.PRE_MOD = G.PTC_MOD AND B.PRE_PRO = G.PTC_PRO AND
             B.PRE_TIP = G.PTC_TIP AND B.PRE_MONEDA = G.PTC_MON AND
             C.DES_CODTAB = G.PTC_TABTIPOCREDITO AND
             C.DES_CODIGO = G.PTC_TIPOCREDITO AND
             B.PRE_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
            --A.CLC_TIPOREL  NOT IN (p_tiporel,p_tiporel1,p_tiporel2,p_tiporel3,p_tiporel4) AND
             TRUNC(B.PRE_FECONTAB) <= p_dateto AND
             NVL(B.PRE_FECANCE, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_dateto, 'YYYY/MM/DD') AND
             D.DES_CODTAB = A.CLC_CODTAB AND D.DES_CODIGO = A.CLC_TIPOREL AND
             NVL(D.CODEISO, 'G3') <> 'S4' AND
             CLC_CUE NOT IN
             (SELECT CLC_CUE
                FROM TCLI_CLICTA         A,
                     TPRE_PRESTAMOS      B,
                     TGEN_DESCTABLA      C,
                     TGEN_DESCTABLA      D,
                     TPRE_PROTIPOCREDITO G
               WHERE A.CLC_MOD = B.PRE_MOD AND A.CLC_CUE = B.PRE_CREDITO AND
                     B.PRE_MOD = G.PTC_MOD AND B.PRE_PRO = G.PTC_PRO AND
                     B.PRE_TIP = G.PTC_TIP AND B.PRE_MONEDA = G.PTC_MON AND
                     C.DES_CODTAB = G.PTC_TABTIPOCREDITO AND
                     C.DES_CODIGO = G.PTC_TIPOCREDITO AND
                     D.DES_CODTAB = A.CLC_CODTAB AND
                     D.DES_CODIGO = A.CLC_TIPOREL AND
                     nvl(D.CODEISO, 'G3') = 'S4' AND
                     B.PRE_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
                     TRUNC(B.PRE_FECONTAB) <= p_dateto AND
                     NVL(B.PRE_FECANCE, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) >
                     TO_DATE(p_dateto, 'YYYY/MM/DD')) AND
             CLC_CUE IN
             (SELECT CLC_CUE
                FROM TPRE_PRESTAMOS B, TCLI_CLICTA A
               WHERE A.CLC_MOD = B.PRE_MOD AND A.CLC_CUE = B.PRE_CREDITO AND
                     B.PRE_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
                    --A.CLC_TIPOREL  NOT IN (7,8) AND
                     TRUNC(B.PRE_FECONTAB) <= p_dateto AND
                     NVL(B.PRE_FECANCE, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) >
                     TO_DATE(p_dateto, 'YYYY/MM/DD')
               GROUP BY CLC_CUE
              --HAVING COUNT(*) = 1
              MINUS
              SELECT ogr_operacion
                FROM tcli_opergaran A, tpre_prestamos B
               WHERE A.OGR_OPERACION = B.PRE_CREDITO AND
                     B.PRE_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
                     TRUNC(B.PRE_FECONTAB) <= p_dateto AND
                     nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_dateto  AND--IS NULL
                     ogr_fecdesde is  not null AND
                     NVL(B.PRE_FECANCE, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) >
                     TO_DATE(p_dateto, 'YYYY/MM/DD'))
      UNION
      SELECT CLC_MOD OGR_MOD,
             CLC_CUE,        
             CLC_CUE OGR_OPERACION,
             CLC_CODCLI,            
             CLC_CODCLI OGR_CLIENTE,             
             PRE_STATUS,
             CODEISO,
             NVL(PRE_FECORIGINAL, PRE_FECEMI) PRE_FECEMI,
             CLC_TIPOREL,
             1 OGR_NUMGARAN
        FROM TCLI_CLICTA         A,
             TPRE_PRESTAMOS      B,
             TGEN_DESCTABLA      C,
             TPRE_PROTIPOCREDITO G
       WHERE A.CLC_MOD = B.PRE_MOD AND A.CLC_TIPOREL IN (28, 29) AND
             A.CLC_CUE = B.PRE_CREDITO AND B.PRE_MOD = G.PTC_MOD AND
             B.PRE_PRO = G.PTC_PRO AND B.PRE_TIP = G.PTC_TIP AND
             B.PRE_MONEDA = G.PTC_MON AND
             C.DES_CODTAB = G.PTC_TABTIPOCREDITO AND
             C.DES_CODIGO = G.PTC_TIPOCREDITO AND
             B.PRE_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
             TRUNC(B.PRE_FECONTAB) <= p_dateto AND
             NVL(B.PRE_FECANCE, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_dateto, 'YYYY/MM/DD')
      UNION
       SELECT CLC_MOD OGR_MOD,
             CLC_CUE,        
             CLC_CUE OGR_OPERACION,
             CLC_CODCLI,            
             CLC_CODCLI OGR_CLIENTE,             
             VIS_STATUS PRE_STATUS,
             C.CODEISO,
             CRD_FECHAVIG PRE_FECEMI,
             CLC_TIPOREL,
             1 OGR_NUMGARAN
        FROM TCLI_CLICTA         A,
             TCAP_VISTA      B, --TPRE_PRESTAMOS
             TCAP_CREDVISTA   H,
             TCAP_ACRACT      I,
             TGEN_DESCTABLA      C,
             TGEN_DESCTABLA      D,
             TPRE_PROTIPOCREDITO G
       WHERE A.CLC_MOD = B.VIS_MOD AND
             A.CLC_CUE = B.VIS_NUMCUE AND
             B.VIS_NUMCUE = H.CRD_NUMCUE    AND
             H.CRD_NUMCUE = I.ACA_NUMCUE    AND
             H.CRD_TIPOCRED = I.ACA_TIPOCRE AND
             H.CRD_SECUENCIA = I.ACA_SECUENCIA AND
             H.CRD_TIPOCRED = 2 AND
             I.ACA_FECHA = p_dateto AND
             B.VIS_MOD = G.PTC_MOD AND
             B.VIS_PRO = G.PTC_PRO AND
             B.VIS_TIP = G.PTC_TIP AND
             B.VIS_MONEDA = G.PTC_MON AND
             C.DES_CODTAB = G.PTC_TABTIPOCREDITO AND
             C.DES_CODIGO = G.PTC_TIPOCREDITO AND
             D.DES_CODTAB = 33 AND
             D.DES_CODIGO = H.CRD_TIPOGAR AND
             NVL(D.CODEISO, 'G3') = 'G3' AND
             B.VIS_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
             TRUNC(B.VIS_FECHAPE) <= p_dateto AND
                        trunc(crd_fechautor) <= p_dateto
					   and  crd_fechavenc >= P_dateto
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > P_dateto
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > P_dateto
             and NVL(B.VIS_FECHCIERR, TO_DATE(p_dateto, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_dateto, 'YYYY/MM/DD'); --HD4324_2

--NUEVO
		w_compromiso varchar2(1);
		w_interino  varchar2(1);
		w_rotativo varchar2(1);
		w_promotor varchar2(1);
		w_desembolso varchar2(1);
		w_compromiso_d number(10); 
		w_capred number;
		w_totgaran number;
		w_porcentaje number;           
		w_monoper number;
		r_endosobien tpre_endosobien%rowtype;
		contador NUMBER:=0;
		--p_tipoper_cli      NUMBER;
		--TXT_TIPOREL        TGEN_DESCTABLA.DES_DESCRIPCION%TYPE;
		--TXT_CODEISO        TGEN_DESCTABLA.CODEISO%TYPE;
		V_NUMREGMON number:=0;
		V_MONTOFORMAGAR                            NUMBER(15,2);
		V_IDENTTASADOR                             VARCHAR2(13);
		V_FECHAVENCGAR                             VARCHAR2(10);
		v_NUMEROPOLIZASEG                          VARCHAR2(10);
		V_FECEMIPOLSEG                             VARCHAR2(10);
		V_IDENTCOMASEGURADORA                      VARCHAR2(13);
		V_VALORENDOSO                              NUMBER(15,2);
		V_GARANFIDUCIARIA                          VARCHAR2(1);
		V_CLASIFFIDUCIARIA                         VARCHAR2(3);
		V_DESCRIPFIDE                              VARCHAR2(250); 
		V_TIPOGARANTIA                             VARCHAR2(2);
		V_TIPOMONEDA VARCHAR2(1);
--
  BEGIN

    IF (trunc(p_dateto) < trunc(sysdate)) THEN
      pkg_legalreport.eraser(p_report, p_session); --vacia el repositorio de tvaluelevelcustom
      --Inserta en la tabla tvaluelevelcustom los datos de las garantias de créditos obtenidas en el cursor GARANTIAS
      FOR I IN GARANTIAS LOOP
        --  I.OGR_cliente IN, p_identifica OUT, p_tipoper OUT (4),p_nombre OUT (16), p_apellido OUT (17)
        --  El procedimiento devuelve la identificacion, tipo de persona, nombre y apellido del cliente ogr_cliente
        pkg_legalreport3.DATOS_CLIENTE(I.ogr_cliente,
                                       p_identifica,
                                       p_tipoper,
                                       p_nombre,
                                       p_apellido);
        --  i.ogr_mod in, i.ogr_operacion in, i.ogr_cliente in,  p_tipogar out (5)
        -- Este procedimiento recibe el modulo, el número de credito, codigo de cliente y tipo de persona;
        --  debe devolver tipo garantia en formato H1, H2, etc.
        pkg_legal_dom.tip_garantia(i.ogr_mod,
                                   i.ogr_operacion,
                                   i.ogr_cliente,
                                   i.OGR_NUMGARAN,
                                   p_tipogar);
        --  i.bxg_formato IN, i.ogr_cliente IN , i.ogr_numgaran IN, gar_id OUT(3), p_valores OUT, p_noformato OUT, p_desc OUT
        --  Este procedimiento devuelve la identificación de la garantia
        pkg_legalreport3.garantia_id(i.bxg_formato,
                                     i.ogr_cliente,
                                     i.ogr_numgaran,
                                     trunc(p_dateto),
                                     gar_id,
                                     p_valores,
                                     p_noformato,
                                     p_descripcion,
                                     p_identificaemisor);
        --i.ogr_cliente IN, i.ogr_numgaran IN ,p_fechat OUT (9), p_valor OUT (10)
        --  Si el bien tiene tasacion ingresa la fecha de la ultima tasacion y su valor, si no Null

        -- Caso en que no aparecen el tipode identifcacion ni la idetificacios de los clientes p1,p2 E1
        IF p_tipogar in (p_tipogarid1, p_tipogarid2, p_tipogarid3) THEN
          gar_id := p_identifica;
        END IF;
        --
        fecha_tasacion(i.ogr_cliente,
                                        i.ogr_numgaran,
                                        p_decimales,
                                        p_dateto,
                                        p_fechat,
                                        p_valor);
        pkg_legalreport3.TIENE_PIGNORACION_BLH(i.ogr_cliente,
                                               i.ogr_numgaran,
                                               I.OGR_OPERACION,
                                               trunc(p_dateto),
                                               p_esplazo,
                                               p_esplazodelbanco,
                                               p_valorpignorado);
        pkg_legalreport3.ES_GARANTIAREAL(i.ogr_cliente,
                                         i.pre_clientep,
                                         I.OGR_OPERACION,
                                         p_esreal);

        v_anio:=fun_aniogarantia(i.ogr_cliente,i.ogr_numgaran);
        
        IF p_esplazo = 'S' THEN
          IF p_esplazodelbanco = 'S' THEN
            --
            p_valor := p_valorpignorado;
          END IF;
        END IF;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) --Numero secuencial
        VALUES
          (p_report, p_session, 1, CODIGO, 1, CODIGO);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Código del crédito
        VALUES
          (p_report, p_session, 2, CODIGO, 2, I.OGR_OPERACION);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Identificacion de la garantia
        VALUES
          (p_report, p_session, 3, CODIGO, 3, gar_id);

        -- i.ogr_cliente IN ,p_tiporel IN
        -- La funcion devuleve tipo de persona P1,P2,E1 si el dueño de la garantia a su vez es un garante solidario
        existe := pkg_legal_dom.garante_solidario(i.ogr_cliente, p_tiporel);
        --Esto por cuanto se desea que los solidarios y tambien los solo Firma se traten como S4
        IF p_tipogar in (p_tipogarid1, p_tipogarid2, p_tipogarid3) THEN
          existe       := 'S';
          p_fechaconst := i.pre_fecemi;
        ELSE
          existe       := 'N';
          p_fechaconst := I.OGR_FECPROT;
        END IF;
        IF existe = 'S' THEN
          INSERT INTO tvaluelevelcustom
            (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) -- Tipo garante solidario
          VALUES
            (p_report, p_session, 4, CODIGO, 4, p_tipoper);
        ELSE
          INSERT INTO tvaluelevelcustom
            (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
          VALUES
            (p_report, p_session, 4, CODIGO, 4, NULL);
        END IF;
        --pARA IDENTIFICAR QUE LAS GARANTIAS NO ESTAN INSCRITAS SE PROCDE A COLOCAR
        -- EL TIPOGAR SIN VALOR
        IF i.ogr_fecdesde IS NULL THEN
          p_tipogar := NULL;
        END IF;                      
		V_TIPOGARANTIA:= p_tipogar;        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Tipo de Garantia
        VALUES
          (p_report, p_session, 5, CODIGO, 5, p_tipogar);

        /*IF p_noformato ='S' THEN --Descripción de la garantia
        if i.gar_descrip is null then
           p_descripcion :=NULL;
        else*/
        p_descripcion := i.gar_descrip;
        /*end if;
        end if;*/
        --se comenta porque el cliente solicita que se saque de la pantalla 28/1 descripción
        --
        pkg_legalreport3.TIPO_RELACION(i.ogr_cliente,
                                       i.ogr_operacion,
                                       p_relacion);
		v_tipoper:=0;
        IF p_relacion IN
           (p_tiporel, p_tiporel1, p_tiporel2, p_tiporel3, p_tiporel4) OR
           p_esreal = 'S' THEN--se aumenta el 5 ya que estos son firmas de ellos mismos
          P_garante16 := p_nombre;
          P_garante17 := p_apellido;
          select cli_tipoper
            into v_tipoper
           from tcli_persona
          where cli_codigo = i.ogr_cliente;
          if v_tipoper = 2 then
			 P_garante17:= p_nombre;
          end if;
        ELSE
          P_garante16 := NULL;
          P_garante17 := NULL;
        END IF;
        --IF (p_tipogar = 'H1' OR p_tipogar = 'H2') THEN
        IF (p_tipogar = 'HI') THEN
          --ANTES H1,H2,H3
          P_garante16 := NULL;
          P_garante17 := NULL;
        END IF; --AUMENTADO EL 2005/03/30 REVISON DE AUDITORIA(MARY PILI)

        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report,
           p_session,
           6,
           CODIGO,
           6,
           substr(p_descripcion, 1, 250)); --p_descripcion

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Fecha constitucion garantia
        VALUES
          (p_report, p_session, 7, CODIGO, 7, p_fechaconst); --I.OGR_FECPROT);
         --OCTUBRE 2008
         vld_ogr_fecha := I.OGR_FECHA;
         IF p_tipogar NOT IN  ('HI','I1','I2','I3','I4','I5','I6','I7','P2','P3','P4') THEN --('HI','P2') THEN
              vld_ogr_fecha := p_fechaconst;
         END IF;
         IF p_tipogar  IN ('P2','P3','P4') THEN-- ('P2') THEN
              vld_ogr_fecha := p_dateto;
         END IF;

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Fecha formalizacion garantia
        VALUES
          (p_report, p_session, 8, CODIGO, 8,vld_ogr_fecha);-- I.OGR_FECHA); --antes estaba I.OGR_FECDESDE
        --EN BLH
        pkg_legalreport3.segurogarantia_BLH(i.ogr_operacion,
                                            i.ogr_cliente,
                                            i.ogr_numgaran,
                                            p_dateto,
                                            p_seguro,
                                            p_fechavencepoliza);

        --SE COMENTA PORQUE DEBE TENER FECHA DE TASACION 6070006001 POR EJEMPLO
        /*IF (p_tipogar = 'P5' OR p_tipogar = 'P2') AND nvl(p_seguro,'N')='N' THEN
           p_fechat := NULL;
           p_valor := NULL;
        END IF; */
        --IF p_tipogar = 'P3' THEN  --
        IF upper(p_tipogar) IN ('V1', 'V4','V5','V2','V6', 'V3','V7') THEN --('V1', 'V2', 'V3') THEN
          -- ANTES P3
          p_fechat := NULL;
        END IF;
        --CAMBIO REALIZADO EL 2004/06/14
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Fecha ultima tasacion
        VALUES
          (p_report, p_session, 9, CODIGO, 9, p_fechat);
        -- si no es null
           p_valor:=p_valor*bdi_promedio(I.gar_moneda,1,p_dateto);
        --NUEVO CAMBIO OCTUBRE 2008
        IF p_valor IS NULL THEN
          p_valor := SALDO_CAPITAL_FECHA_DE(I.OGR_OPERACION,p_dateto);
        END IF;
        --

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) -- Valor tasación garantia
        VALUES
          (p_report, p_session, 10, CODIGO, 10, p_valor);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) --Rango de garantia
        VALUES
          (p_report, p_session, 11, CODIGO, 11, I.OGR_GRADO);

        -- Si la garantía es en valores, es decir acciones o documentos el tipo de garantia en valores es igual al codeiso de
        -- tabla tgen_desctabla con codtab=22

        p_tipvalor := TO_NUMBER(substr(pkg_legal_dom.tipo_valores(I.gar_tipbien),
                                       1,
                                       3));

        IF p_valores = 'S' THEN
          INSERT INTO tvaluelevelcustom
            (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
          VALUES
            (p_report, p_session, 12, CODIGO, 12, p_tipvalor);
        ELSE
          INSERT INTO tvaluelevelcustom
            (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
          VALUES
            (p_report, p_session, 12, CODIGO, 12, NULL);
        END IF;
        --si son plazos del banco deben salir el rnc del banco
        -- si son de otra institucion deben salir el rnc de la otra ins. para eso deben llenar en el campo
        --identificación de la pantalla de depósitos a plazo
        --IF p_esplazodelbanco = 'S' THEN
        IF upper(p_tipogar) IN ('V2','V6') THEN --= 'V2' THEN
          pkg_legalreport3.RUC_INSTITUCION(p_ruc);
          p_identificaemisor := p_ruc;
        END IF;

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --RNC de ent.emisora T.Valor
        VALUES
          (p_report, p_session, 13, CODIGO, 13, p_identificaemisor);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Garantia asegurada
        VALUES
          (p_report, p_session, 14, CODIGO, 14, nvl(p_seguro, 'N')); -- 2003/03/06 Se cambia por i.gar_amparapoliza

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Fecha vencimiento de poliza
        VALUES
          (p_report, p_session, 15, CODIGO, 15, p_fechavencepoliza);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Nombres / Razón social Garante
        VALUES
          (p_report, p_session, 16, CODIGO, 16, p_garante16);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Apellidos / Siglas Garante
        VALUES
          (p_report, p_session, 17, CODIGO, 17, p_garante17);
        --TIPO DE CREDITO  PARA COMERCIAL SE CLASIFICA EN MENOR DEUDOR
        v_codeiso := codtipo_cartera(p_dateto,'P',I.OGR_OPERACION); --i.codeiso;
        --IF I.codeiso = 'C' THEN
        --FIN
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Tipo de crédito
        VALUES
          (p_report, p_session, 18, CODIGO, 18, V_CODEISO); --I.codeiso);
                  --garantia_adm:= i.lca_montogaradmi;
         garantia_adm:= fun_garantia(I.OGR_OPERACION,p_dateto,'DE03');
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --garantia adminisble
        VALUES
          (p_report, p_session, 19, CODIGO, 19, garantia_adm);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 20, CODIGO, 20, v_anio);

        --NUEVOS CAMPOS       
				begin
					 select mon_cod
					   into w_monoper
					   from tgen_moneda, tgen_cuenta
					  where pro_mod = i.ogr_mod
					    and pro_cue = i.ogr_operacion
					    and pro_mon = mon_cod;
				 exception 
				 when others then
				   w_monoper:=0;
				end;
				  gar_tipooperacion(i.ogr_mod,i.ogr_operacion,w_compromiso,w_interino, w_rotativo,
				                       w_promotor, w_desembolso, w_compromiso_d);
				                          
				   begin                    
				     w_capred := gar_saldooperaciones(i.ogr_mod,i.ogr_operacion,p_dateto,w_monoper,w_monoper,1);
				   exception
				   	   when others then
				   	   w_capred:=0;
				   end;
				if ((nvl(w_compromiso,'N') = 'N' and nvl(w_desembolso,'N') = 'N') or (nvl(w_desembolso,'N') = 'S' and nvl(w_promotor,'N') = 'S')) then       
				 	     w_totgaran := pkg_gar_avaluos.gar_valtotgarantias(i.ogr_mod,i.ogr_operacion,w_monoper,1); 
				 	     w_porcentaje := 100 * w_totgaran/w_capred;
				 elsif (nvl(w_compromiso,'N') = 'S' and nvl(w_promotor,'N') = 'N') then
				 	     --:w_linea := 'S';
				 	     w_totgaran := pkg_gar_avaluos.gar_valtotgarantias(i.ogr_mod,i.ogr_operacion,w_monoper,1); 
				 	     w_porcentaje := 100 * w_totgaran/w_capred;
				 elsif (nvl(w_desembolso,'N') = 'S' and nvl(w_promotor,'N') = 'N') then      	     	
				 	     w_totgaran := pkg_gar_avaluos.gar_valtotgarantias(i.ogr_mod,i.ogr_operacion,w_monoper,1); 
				 	     --:w_desem := 'S';
				 	  --   :w_totgaran := null;
				 	     w_porcentaje := null;
				 end if;
        
		        V_MONTOFORMAGAR:= w_totgaran;
				DBMS_OUTPUT.PUT_LINE('Paso 21');          
				  begin
				  select ava_cedula
				    into V_IDENTTASADOR
				from tgar_avaluos,TGAR_AVALUADORES
				where avl_codcli = i.ogr_cliente
				  and avl_numgar = i.ogr_numgaran
				  and P_dateto between trunc(avl_fecha) and nvl(avl_fechasta, to_date('2199/12/31','yyyy/mm/dd'))
				  and avl_codava = ava_codigo;
				exception
				  when others then
				     V_IDENTTASADOR:=null;
				  end;                       
				DBMS_OUTPUT.PUT_LINE('Paso 22');  
				V_FECHAVENCGAR:= null;
				r_endosobien:=null;          
				DBMS_OUTPUT.PUT_LINE('Paso 23');
				begin
				 select *
				   into r_endosobien
				  from tpre_endosobien
				  where ecb_codcli= i.ogr_cliente
				    and ecb_numgar = i.ogr_numgaran
				    and p_dateto between trunc(ecb_desde) 
				                                           and nvl(ecb_hasta, to_date('2199/12/31','yyyy/mm/dd'));
				exception
				  when others then
				      r_endosobien:=null;
				end;   
				v_NUMEROPOLIZASEG:= substr(r_endosobien.ecb_poliza,1,10);           
				DBMS_OUTPUT.PUT_LINE('Paso 24');
				V_FECEMIPOLSEG:=r_endosobien.ecb_emipol;           
				DBMS_OUTPUT.PUT_LINE('Paso 25');
				begin
				 select ASE_IDENTIFICA
				   into V_IDENTCOMASEGURADORA
				    from Tdom_aseguradoras
				    where ASE_CODTAB = r_endosobien.ecb_tabasegu
				     and ASE_CODIGO  = r_endosobien.ecb_codasegu;
				exception
				   when others then
				      V_IDENTCOMASEGURADORA:='0-00-00000-0';
				end;                            
				DBMS_OUTPUT.PUT_LINE('Paso 26');
				V_VALORENDOSO:= r_endosobien.ecb_cobertura;                       
				DBMS_OUTPUT.PUT_LINE('Paso 27');
				V_GARANFIDUCIARIA:='N';      
				DBMS_OUTPUT.PUT_LINE('Paso 28');  
				V_CLASIFFIDUCIARIA:=null;    
				DBMS_OUTPUT.PUT_LINE('Paso 29');   
				V_DESCRIPFIDE:=null;         
				IF  V_TIPOGARANTIA IN ('F1','F2','F3') THEN
				    V_GARANFIDUCIARIA:='S';     
				    V_DESCRIPFIDE:=p_descripcion;--V_DESCRIPGARANTIA;
				    V_CLASIFFIDUCIARIA:= LPAD(V_TIPOGARANTIA,'0',3);    
				END IF;
        --FIN DE NUEVOS CAMPOS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 21, CODIGO, 21, V_MONTOFORMAGAR);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 22, CODIGO, 22, V_IDENTTASADOR);
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 23, CODIGO, 23, V_FECHAVENCGAR);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 24, CODIGO, 24, v_NUMEROPOLIZASEG);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 25, CODIGO, 25, V_FECEMIPOLSEG);
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 26, CODIGO, 26, V_IDENTCOMASEGURADORA);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 27, CODIGO, 27, V_VALORENDOSO);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 28, CODIGO, 28, V_GARANFIDUCIARIA);
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 29, CODIGO, 29, V_CLASIFFIDUCIARIA);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 30, CODIGO, 30, V_DESCRIPFIDE);

         --TIPO DE MONEDA       
            SELECT COUNT(*)
              INTO V_NUMREGMON
              FROM TCLI_CLICTA 
             WHERE CLC_CUE = i.ogr_operacion
               AND CLC_MON = 0;
              IF V_NUMREGMON > 0 THEN
                 V_TIPOMONEDA:='N';
              ELSE
                 V_TIPOMONEDA:='E';
              END IF; 
         --FIN DE TIPO DE MONEDA
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 31, CODIGO, 31, V_TIPOMONEDA);
          
        
        CODIGO := CODIGO + 1;
        
      END LOOP;

      FOR I IN GARANTES loop

        pkg_legalreport3.DATOS_CLIENTE_BLH(I.clc_codcli,
                                           p_identifica,
                                           p_tipoper,
                                           p_nombre,
                                           p_apellido,
                                           p_tipoper_cli);
        --para determinar descripción de relacion
        BEGIN
          SELECT DES_DESCRIPCION, CODEISO
            INTO TXT_TIPOREL, TXT_CODEISO
            FROM TCLI_CLICTA, TGEN_RELPROCLI, TGEN_DESCTABLA
           WHERE CLC_MOD = RPC_MOD AND CLC_TIPOREL = RPC_RELACION AND
                 RPC_TABREL = DES_CODTAB AND RPC_RELACION = DES_CODIGO AND
                 CLC_CODCLI = I.clc_codcli AND CLC_CUE = I.CLC_CUE;
          --IF I.clc_tiporel NOT IN (28,27,29) THEN
          /*IF  I.clc_tiporel NOT IN  (p_tiporel,p_tiporel1,p_tiporel2,p_tiporel3,p_tiporel4) THEN
              TXT_TIPOREL := 'SOLA FIRMA';
          END IF; */
        EXCEPTION
          WHEN OTHERS THEN
            TXT_TIPOREL := 'FIRMA SOLIDARIA';
            TXT_CODEISO := 'S4';
        END;

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) --Numero secuencial
        VALUES
          (p_report, p_session, 1, CODIGO, 1, CODIGO);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Código del crédito
        VALUES
          (p_report, p_session, 2, CODIGO, 2, I.CLC_CUE);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --No aplicable
        VALUES
          (p_report, p_session, 3, CODIGO, 3, p_identifica);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) -- Tipo garante solidario tcli_clicta
        VALUES
          (p_report, p_session, 4, CODIGO, 4, p_tipoper);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Tipo de Garantia
        VALUES
          (p_report, p_session, 5, CODIGO, 5, TXT_CODEISO); --'S4');
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --NO APLICA
        VALUES
          (p_report, p_session, 6, CODIGO, 6, TXT_TIPOREL); --'FIRMA SOLIDARIA');
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --NO APLICA
        VALUES
          (p_report, p_session, 7, CODIGO, 7, i.pre_fecemi);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --NO APLICA
        VALUES
          (p_report, p_session, 8, CODIGO, 8, i.pre_fecemi);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --NO APLICA
        VALUES
          (p_report, p_session, 9, CODIGO, 9, NULL);
        --NUEVO CAMBIO OCTUBRE 2008
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) -- VALOR DE LA TASAVA DEBE IR EL VALOR DE LA OPERACION
        VALUES
          (p_report, p_session, 10, CODIGO, 10,0);--rosemary 2015/12/11  SALDO_CAPITAL_FECHA_DE(I.CLC_CUE,p_dateto));

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) --NO APLICA
        VALUES
          (p_report, p_session, 11, CODIGO, 11, '1');
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) -- NO APLICA
        VALUES
          (p_report, p_session, 12, CODIGO, 12, NULL);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) -- NO APLICA
        VALUES
          (p_report, p_session, 13, CODIGO, 13, NULL);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) -- NO APLICA
        VALUES
          (p_report, p_session, 14, CODIGO, 14, 'N');
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) -- NO APLICA
        VALUES
          (p_report, p_session, 15, CODIGO, 15, NULL);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Nombres / Razón social Garante
        VALUES
          (p_report, p_session, 16, CODIGO, 16, p_nombre);
        IF p_tipoper_cli = 1 THEN
          INSERT INTO tvaluelevelcustom
            (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Apellidos / Siglas Garante
          VALUES
            (p_report, p_session, 17, CODIGO, 17, p_apellido);
        ELSE
          INSERT INTO tvaluelevelcustom
            (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Apellidos / Siglas Garante
          VALUES
            (p_report, p_session, 17, CODIGO, 17, p_nombre);
        END IF;
        --TIPO DE CREDITO  PARA COMERCIAL SE CLASIFICA EN MENOR DEUDOR
        v_codeiso := i.codeiso;
          BEGIN
            /*SELECT SUM(MONTO_AUTORIZADO_LEG(PRE_MONEDA,
                                            PRE_SUCURSAL,
                                            PRE_CREDITO,
                                            p_dateto))
              INTO p_montoriesgo
              FROM tpre_accrual,
                   tpre_prestamos,
                   TPRE_COMPROM,
                   tpre_protipocredito
             WHERE ptc_mod = pre_mod AND ptc_pro = pre_pro AND
                   ptc_tip = pre_tip AND ptc_mon = pre_moneda AND
                   acr_fecha = p_dateto AND pre_credito = acr_credito AND
                   pre_clientep = i.PRE_CLIENTEP AND
                   ptc_tipocredito IN
                   (p_tipocredito1, p_tipocredito2, p_tipocredito3) AND
                   CMP_NUMERO(+) = PRE_NUMCOMPROM AND
                   CMP_CLIENTEP(+) = PRE_CLIENTEP
             GROUP BY PRE_CLIENTEP;*/
           SELECT LCA_CODTIPOCREDITO
             INTO V_CODEISO
            FROM TLEG_OPERACIONCALIF
            WHERE LCA_TIPO = 'P'
              AND LCA_CUENTA = I.CLC_CUE
              AND LCA_FECHA = (SELECT MAX(LCA_FECHA)
                                 FROM TLEG_OPERACIONCALIF
                                WHERE LCA_TIPO= 'P'
                                  AND LCA_CUENTA = I.CLC_CUE
                                  AND LCA_FECHA <=P_dateto);
        EXCEPTION
            WHEN OTHERS THEN
              V_CODEISO := NULL;
          END;
        --FIN
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Tipo de crédito
        VALUES
          (p_report, p_session, 18, CODIGO, 18, V_CODEISO); --I.codeiso);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Tipo de crédito
        VALUES
          (p_report, p_session, 19, CODIGO, 19, NULL);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 20, CODIGO, 20, null);
				begin
					 select mon_cod
					   into w_monoper
					   from tgen_moneda, tgen_cuenta
					  where pro_mod = i.ogr_mod
					    and pro_cue = i.ogr_operacion
					    and pro_mon = mon_cod;
				 exception 
				 when others then
				   w_monoper:=0;
				end;
				  gar_tipooperacion(i.ogr_mod,i.ogr_operacion,w_compromiso,w_interino, w_rotativo,
				                       w_promotor, w_desembolso, w_compromiso_d);
				                          
				   begin                    
				     w_capred := gar_saldooperaciones(i.ogr_mod,i.ogr_operacion,p_dateto,w_monoper,w_monoper,1);
				   exception
				   	   when others then
				   	   w_capred:=0;
				   end;
				if ((nvl(w_compromiso,'N') = 'N' and nvl(w_desembolso,'N') = 'N') or (nvl(w_desembolso,'N') = 'S' and nvl(w_promotor,'N') = 'S')) then       
				 	     w_totgaran := pkg_gar_avaluos.gar_valtotgarantias(i.ogr_mod,i.ogr_operacion,w_monoper,1); 
				 	     w_porcentaje := 100 * w_totgaran/w_capred;
				 elsif (nvl(w_compromiso,'N') = 'S' and nvl(w_promotor,'N') = 'N') then
				 	     --:w_linea := 'S';
				 	     w_totgaran := pkg_gar_avaluos.gar_valtotgarantias(i.ogr_mod,i.ogr_operacion,w_monoper,1); 
				 	     w_porcentaje := 100 * w_totgaran/w_capred;
				 elsif (nvl(w_desembolso,'N') = 'S' and nvl(w_promotor,'N') = 'N') then      	     	
				 	     w_totgaran := pkg_gar_avaluos.gar_valtotgarantias(i.ogr_mod,i.ogr_operacion,w_monoper,1); 
				 	     --:w_desem := 'S';
				 	  --   :w_totgaran := null;
				 	     w_porcentaje := null;
				 end if;
        
		        V_MONTOFORMAGAR:= w_totgaran;
				DBMS_OUTPUT.PUT_LINE('Paso 21');          
				  begin
				  select ava_cedula
				    into V_IDENTTASADOR
				from tgar_avaluos,TGAR_AVALUADORES
				where avl_codcli = i.ogr_cliente
				  and avl_numgar = i.ogr_numgaran
				  and P_dateto between trunc(avl_fecha) and nvl(avl_fechasta, to_date('2199/12/31','yyyy/mm/dd'))
				  and avl_codava = ava_codigo;
				exception
				  when others then
				     V_IDENTTASADOR:=null;
				  end;                       
				DBMS_OUTPUT.PUT_LINE('Paso 22');  
				V_FECHAVENCGAR:= null;
				r_endosobien:=null;          
				DBMS_OUTPUT.PUT_LINE('Paso 23');
				begin
				 select *
				   into r_endosobien
				  from tpre_endosobien
				  where ecb_codcli= i.ogr_cliente
				    and ecb_numgar = i.ogr_numgaran
				    and p_dateto between trunc(ecb_desde) 
				                                           and nvl(ecb_hasta, to_date('2199/12/31','yyyy/mm/dd'));
				exception
				  when others then
				      r_endosobien:=null;
				end;   
				v_NUMEROPOLIZASEG:= substr(r_endosobien.ecb_poliza,1,10);           
				DBMS_OUTPUT.PUT_LINE('Paso 24');
				V_FECEMIPOLSEG:=r_endosobien.ecb_emipol;           
				DBMS_OUTPUT.PUT_LINE('Paso 25');
				begin
				 select ASE_IDENTIFICA
				   into V_IDENTCOMASEGURADORA
				    from Tdom_aseguradoras
				    where ASE_CODTAB = r_endosobien.ecb_tabasegu
				     and ASE_CODIGO  = r_endosobien.ecb_codasegu;
				exception
				   when others then
				      V_IDENTCOMASEGURADORA:='0-00-00000-0';
				end;                            
				DBMS_OUTPUT.PUT_LINE('Paso 26');
				V_VALORENDOSO:= r_endosobien.ecb_cobertura;                       
				DBMS_OUTPUT.PUT_LINE('Paso 27');
				V_GARANFIDUCIARIA:='N';      
				DBMS_OUTPUT.PUT_LINE('Paso 28');  
				V_CLASIFFIDUCIARIA:=null;    
				DBMS_OUTPUT.PUT_LINE('Paso 29');   
				V_DESCRIPFIDE:=null;         
				IF  V_TIPOGARANTIA IN ('F1','F2','F3') THEN
				    V_GARANFIDUCIARIA:='S';     
				    V_DESCRIPFIDE:=p_descripcion;--V_DESCRIPGARANTIA;
				    V_CLASIFFIDUCIARIA:= LPAD(V_TIPOGARANTIA,'0',3);    
				END IF;
        --FIN DE NUEVOS CAMPOS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 21, CODIGO, 21, V_MONTOFORMAGAR);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 22, CODIGO, 22, V_IDENTTASADOR);
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 23, CODIGO, 23, V_FECHAVENCGAR);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 24, CODIGO, 24, v_NUMEROPOLIZASEG);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 25, CODIGO, 25, V_FECEMIPOLSEG);
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 26, CODIGO, 26, V_IDENTCOMASEGURADORA);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 27, CODIGO, 27, V_VALORENDOSO);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 28, CODIGO, 28, V_GARANFIDUCIARIA);
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 29, CODIGO, 29, V_CLASIFFIDUCIARIA);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 30, CODIGO, 30, V_DESCRIPFIDE);

         --TIPO DE MONEDA       
            SELECT COUNT(*)
              INTO V_NUMREGMON
              FROM TCLI_CLICTA 
             WHERE CLC_CUE = i.ogr_operacion
               AND CLC_MON = 0;
              IF V_NUMREGMON > 0 THEN
                 V_TIPOMONEDA:='N';
              ELSE
                 V_TIPOMONEDA:='E';
              END IF; 
         --FIN DE TIPO DE MONEDA
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 31, CODIGO, 31, V_TIPOMONEDA);
          

        CODIGO := CODIGO + 1;
      END LOOP;

      FOR I IN GARANTESG3 loop

        pkg_legalreport3.DATOS_CLIENTE_BLH(I.clc_codcli,
                                           p_identifica,
                                           p_tipoper,
                                           p_nombre,
                                           p_apellido,
                                           p_tipoper_cli);
        --para determinar descripción de relacion
        BEGIN
          SELECT DES_DESCRIPCION, CODEISO
            INTO TXT_TIPOREL, TXT_CODEISO
            FROM TCLI_CLICTA, TGEN_RELPROCLI, TGEN_DESCTABLA
           WHERE CLC_MOD = RPC_MOD AND CLC_TIPOREL = RPC_RELACION AND
                 RPC_TABREL = DES_CODTAB AND RPC_RELACION = DES_CODIGO AND
                 CLC_CODCLI = I.clc_codcli AND CLC_CUE = I.CLC_CUE;
          --IF I.clc_tiporel NOT IN (28,27,29) THEN
          IF I.clc_tiporel NOT IN
             (p_tiporel, p_tiporel1, p_tiporel2, p_tiporel3, p_tiporel4) THEN
            TXT_TIPOREL := 'SOLA FIRMA';
          END IF;
          IF TXT_CODEISO IS NULL THEN
            TXT_CODEISO := 'G3';
          END IF;
          --TXT_TIPOREL := 'SOLA FIRMA';
        EXCEPTION
          WHEN OTHERS THEN
            TXT_TIPOREL := 'FIRMA SOLIDARIA';
            TXT_CODEISO := 'G3';
        END;

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) --Numero secuencial
        VALUES
          (p_report, p_session, 1, CODIGO, 1, CODIGO);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Código del crédito
        VALUES
          (p_report, p_session, 2, CODIGO, 2, I.CLC_CUE);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --No aplicable
        VALUES
          (p_report, p_session, 3, CODIGO, 3, p_identifica);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) -- Tipo garante solidario tcli_clicta
        VALUES
          (p_report, p_session, 4, CODIGO, 4, p_tipoper);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Tipo de Garantia
        VALUES
          (p_report, p_session, 5, CODIGO, 5, TXT_CODEISO); --'G3');
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --NO APLICA
        VALUES
          (p_report, p_session, 6, CODIGO, 6, TXT_TIPOREL); --'SOLO FIRMA');
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --NO APLICA
        VALUES
          (p_report, p_session, 7, CODIGO, 7, i.pre_fecemi);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --NO APLICA
        VALUES
          (p_report, p_session, 8, CODIGO, 8, i.pre_fecemi);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --NO APLICA
        VALUES
          (p_report, p_session, 9, CODIGO, 9, NULL);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) -- DEBE IR EL VALOR DEL CREDITO OCTUBRE 2008
        VALUES
          (p_report, p_session, 10, CODIGO, 10, 0);--solicitado por Rosemary 2015/12/11 SALDO_CAPITAL_FECHA_DE(I.CLC_CUE,p_dateto));
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) --NO APLICA
        VALUES
          (p_report, p_session, 11, CODIGO, 11, '1');
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM) -- NO APLICA
        VALUES
          (p_report, p_session, 12, CODIGO, 12, NULL);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) -- NO APLICA
        VALUES
          (p_report, p_session, 13, CODIGO, 13, NULL);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) -- NO APLICA
        VALUES
          (p_report, p_session, 14, CODIGO, 14, 'N');
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) -- NO APLICA
        VALUES
          (p_report, p_session, 15, CODIGO, 15, NULL);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Nombres / Razón social Garante
        VALUES
          (p_report, p_session, 16, CODIGO, 16, p_nombre);
        IF p_tipoper_cli = 1 THEN
          INSERT INTO tvaluelevelcustom
            (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Apellidos / Siglas Garante
          VALUES
            (p_report, p_session, 17, CODIGO, 17, p_apellido);
        ELSE
          INSERT INTO tvaluelevelcustom
            (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Apellidos / Siglas Garante
          VALUES
            (p_report, p_session, 17, CODIGO, 17, p_nombre);
        END IF;
        --TIPO DE CREDITO  PARA COMERCIAL SE CLASIFICA EN MENOR DEUDOR
        v_codeiso := i.codeiso;

          BEGIN
           SELECT LCA_CODTIPOCREDITO
             INTO V_CODEISO
            FROM TLEG_OPERACIONCALIF
            WHERE LCA_TIPO = 'P'
              AND LCA_CUENTA = I.CLC_CUE
              AND LCA_FECHA = (SELECT MAX(LCA_FECHA)
                                 FROM TLEG_OPERACIONCALIF
                                WHERE LCA_TIPO= 'P'
                                  AND LCA_CUENTA = I.CLC_CUE
                                  AND LCA_FECHA <=P_dateto);
        EXCEPTION
            WHEN OTHERS THEN
              V_CODEISO := NULL;
          END;
        --FIN
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Tipo de crédito
        VALUES
          (p_report, p_session, 18, CODIGO, 18, V_CODEISO); --I.codeiso);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --Tipo de crédito
        VALUES
          (p_report, p_session, 19, CODIGO, 19, NULL);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 20, CODIGO, 20, null);
				begin
					 select mon_cod
					   into w_monoper
					   from tgen_moneda, tgen_cuenta
					  where pro_mod = i.ogr_mod
					    and pro_cue = i.ogr_operacion
					    and pro_mon = mon_cod;
				 exception 
				 when others then
				   w_monoper:=0;
				end;
				  gar_tipooperacion(i.ogr_mod,i.ogr_operacion,w_compromiso,w_interino, w_rotativo,
				                       w_promotor, w_desembolso, w_compromiso_d);
				                          
				   begin                    
				     w_capred := gar_saldooperaciones(i.ogr_mod,i.ogr_operacion,p_dateto,w_monoper,w_monoper,1);
				   exception
				   	   when others then
				   	   w_capred:=0;
				   end;
				if ((nvl(w_compromiso,'N') = 'N' and nvl(w_desembolso,'N') = 'N') or (nvl(w_desembolso,'N') = 'S' and nvl(w_promotor,'N') = 'S')) then       
				 	     w_totgaran := pkg_gar_avaluos.gar_valtotgarantias(i.ogr_mod,i.ogr_operacion,w_monoper,1); 
				 	     w_porcentaje := 100 * w_totgaran/w_capred;
				 elsif (nvl(w_compromiso,'N') = 'S' and nvl(w_promotor,'N') = 'N') then
				 	     --:w_linea := 'S';
				 	     w_totgaran := pkg_gar_avaluos.gar_valtotgarantias(i.ogr_mod,i.ogr_operacion,w_monoper,1); 
				 	     w_porcentaje := 100 * w_totgaran/w_capred;
				 elsif (nvl(w_desembolso,'N') = 'S' and nvl(w_promotor,'N') = 'N') then      	     	
				 	     w_totgaran := pkg_gar_avaluos.gar_valtotgarantias(i.ogr_mod,i.ogr_operacion,w_monoper,1); 
				 	     --:w_desem := 'S';
				 	  --   :w_totgaran := null;
				 	     w_porcentaje := null;
				 end if;
        
		        V_MONTOFORMAGAR:= w_totgaran;
				DBMS_OUTPUT.PUT_LINE('Paso 21');          
				  begin
				  select ava_cedula
				    into V_IDENTTASADOR
				from tgar_avaluos,TGAR_AVALUADORES
				where avl_codcli = i.ogr_cliente
				  and avl_numgar = i.ogr_numgaran
				  and P_dateto between trunc(avl_fecha) and nvl(avl_fechasta, to_date('2199/12/31','yyyy/mm/dd'))
				  and avl_codava = ava_codigo;
				exception
				  when others then
				     V_IDENTTASADOR:=null;
				  end;                       
				DBMS_OUTPUT.PUT_LINE('Paso 22');  
				V_FECHAVENCGAR:= null;
				r_endosobien:=null;          
				DBMS_OUTPUT.PUT_LINE('Paso 23');
				begin
				 select *
				   into r_endosobien
				  from tpre_endosobien
				  where ecb_codcli= i.ogr_cliente
				    and ecb_numgar = i.ogr_numgaran
				    and p_dateto between trunc(ecb_desde) 
				                                           and nvl(ecb_hasta, to_date('2199/12/31','yyyy/mm/dd'));
				exception
				  when others then
				      r_endosobien:=null;
				end;   
				v_NUMEROPOLIZASEG:= substr(r_endosobien.ecb_poliza,1,10);           
				DBMS_OUTPUT.PUT_LINE('Paso 24');
				V_FECEMIPOLSEG:=r_endosobien.ecb_emipol;           
				DBMS_OUTPUT.PUT_LINE('Paso 25');
				begin
				 select ASE_IDENTIFICA
				   into V_IDENTCOMASEGURADORA
				    from Tdom_aseguradoras
				    where ASE_CODTAB = r_endosobien.ecb_tabasegu
				     and ASE_CODIGO  = r_endosobien.ecb_codasegu;
				exception
				   when others then
				      V_IDENTCOMASEGURADORA:='0-00-00000-0';
				end;                            
				DBMS_OUTPUT.PUT_LINE('Paso 26');
				V_VALORENDOSO:= r_endosobien.ecb_cobertura;                       
				DBMS_OUTPUT.PUT_LINE('Paso 27');
				V_GARANFIDUCIARIA:='N';      
				DBMS_OUTPUT.PUT_LINE('Paso 28');  
				V_CLASIFFIDUCIARIA:=null;    
				DBMS_OUTPUT.PUT_LINE('Paso 29');   
				V_DESCRIPFIDE:=null;         
				IF  V_TIPOGARANTIA IN ('F1','F2','F3') THEN
				    V_GARANFIDUCIARIA:='S';     
				    V_DESCRIPFIDE:=p_descripcion;--V_DESCRIPGARANTIA;
				    V_CLASIFFIDUCIARIA:= LPAD(V_TIPOGARANTIA,'0',3);    
				END IF;
        --FIN DE NUEVOS CAMPOS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 21, CODIGO, 21, V_MONTOFORMAGAR);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 22, CODIGO, 22, V_IDENTTASADOR);
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 23, CODIGO, 23, V_FECHAVENCGAR);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 24, CODIGO, 24, v_NUMEROPOLIZASEG);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 25, CODIGO, 25, V_FECEMIPOLSEG);
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 26, CODIGO, 26, V_IDENTCOMASEGURADORA);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 27, CODIGO, 27, V_VALORENDOSO);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 28, CODIGO, 28, V_GARANFIDUCIARIA);
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 29, CODIGO, 29, V_CLASIFFIDUCIARIA);

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 30, CODIGO, 30, V_DESCRIPFIDE);

         --TIPO DE MONEDA       
            SELECT COUNT(*)
              INTO V_NUMREGMON
              FROM TCLI_CLICTA 
             WHERE CLC_CUE = i.ogr_operacion
               AND CLC_MON = 0;
              IF V_NUMREGMON > 0 THEN
                 V_TIPOMONEDA:='N';
              ELSE
                 V_TIPOMONEDA:='E';
              END IF; 
         --FIN DE TIPO DE MONEDA
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR) --anio
        VALUES
          (p_report, p_session, 31, CODIGO, 31, V_TIPOMONEDA);
          

        CODIGO := CODIGO + 1;
      END LOOP;

      COMMIT;
      pkg_legalreport3.generic_file(p_report, p_session, p_dateto);
    ELSE
      raise_application_error(-20200,
                              'El reporte es mensual. Solo se puede generar meses concluidos');
    END IF;

  END;
  --fin de de03 para blh
  ------------------------------------------------------------------------------
  -- calificacion
  ------------------------------------------------------------------------------
FUNCTION calificacion(P_FECHA IN DATE,P_TIPO IN VARCHAR2,P_CUENTA IN NUMBER, P_TIPOCALIF IN NUMBER) RETURN VARCHAR2 IS
W_CALIF VARCHAR2(2);
BEGIN
	select SUBSTR(des_descripcion,1,2)
	  INTO W_CALIF
  from tpre_califdettipos,tgen_desctabla
 where cdt_tabcalificacion = des_codtab
   and cdt_calificacion = des_codigo
   and cdt_tipocalif = P_TIPOCALIF
   and cdt_cuenta= P_cuenta
--   and cdt_fecha = p_fecha
   and cdt_fecha = (select max(cdt_fecha)
     				    from tpre_califdettipos
           				    where cdt_cuenta = P_cuenta
             				      and cdt_fecha <= p_fecha
             				      and cdt_tipo = p_tipo
             				      and cdt_tipocalif = P_TIPOCALIF )

   and cdt_tipo = p_tipo;
   return w_calif;
EXCEPTION
	when others then
	W_CALIF:=NULL;
	RETURN W_CALIF;
END;
  ------------------------------------------------------------------------------
  -- provisioon
  ------------------------------------------------------------------------------
FUNCTION provision(P_FECHA IN DATE,P_TIPO IN VARCHAR2,P_CUENTA IN NUMBER, P_TIPOCALIF IN NUMBER) RETURN NUMBER IS
 W_PROV NUMBER:=0;
BEGIN
	select PDT_VALORDIR
	  INTO w_prov
    from tpre_prevdettipos
     where pdt_tipoprev = P_TIPOCALIF
     and pdt_cuenta= P_cuenta
--     and pdt_fecha = p_fecha
     and pdt_fecha = (select max(pdt_fecha)
      				    from tpre_prevdettipos
      				    where pdt_cuenta = p_cuenta
       				      and pdt_fecha <= p_fecha
       				      and pdt_tipo = p_tipo
       				      and pdt_tipoprev = p_tipocalif )
     and pdt_tipo = p_tipo;
   return w_prov;
EXCEPTION
 when others then
	W_prov:=0;
	RETURN W_prov;
END;
  ------------------------------------------------------------------------------
  -- EXLIM
  ------------------------------------------------------------------------------
  PROCEDURE EXLIM(p_report       in varchar2,
                 p_session      in number,
                 p_sucursal     in number,
                 p_moneda   NUMBER,
                 p_datefrom     in date,
                 p_fecha        in date,
                 p_decimales      in    number) IS

    moneda                char(1);
    relacion_banco        char(8);

    -- Definicion de variables para identificar al deudor
    w_nombre       tcli_persona.cli_nombre%type;
    apellido     tcli_persona.cli_nombre%type;
    identifica   varchar2(20);
    w_identifica   varchar2(20);
    tipocli      char(2);
    tipo_recurso varchar2(2);
    secuencial   number(7) := 0;

    -- Variables para hd4324
    garantia_adm           number(18, 6);
W_FINANCI varchar2(1);
W_TIPFINANCI varchar2(2);
    CURSOR CREDITOS IS
      select
             lca_cuenta,
			 clc_codcli lca_codcli,
			 clc_tiporel,
             lca_montogaradmi,
             lca_capital,
             lca_interes,
             decode(lca_codtipocredito,'C','DE21','M','DE22','O','DE23','H','DE25','DE21') lca_codtipocredito,
    lca_codtipocredito lca_codtipocreditoaux
         from tleg_operacioncalif,tcli_clicta
       where lca_mod = clc_mod
         and lca_cuenta = clc_cue
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente--mayores deudores
         --and lca_codtipocredito='C'
       order by lca_cuenta desc;
   CURSOR LINEAS IS
      select
             lca_numcomprom lca_cuenta,
             lca_cuenta lca_credito,
			 clc_codcli lca_codcli,
			 clc_tiporel,
             lca_montogaradmi,
             lca_capital,
             lca_interes,
             decode(lca_codtipocredito,'C','DE21','M','DE22','O','DE23','H','DE25','DE21') lca_codtipocredito,
    lca_codtipocredito lca_codtipocreditoaux
        from tleg_operacioncalif,tcli_clicta
       where lca_mod = clc_mod
         and lca_numcomprom = clc_cue
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente--mayores deudores
         --and lca_codtipocredito='C'
       order by lca_cuenta desc;

   CURSOR TC IS
SELECT
    credit lca_cuenta,
    client lca_codcli,
    5 clc_tiporel,
    0 lca_montogaradmi,
    (to_number(EMTALCR1)/100) + ((to_number(EMTALCR2)*bdi_promedio(2,1,p_fecha))/100) lca_capital,
    0 lca_interes,
    'DE24' lca_codtipocredito
 FROM TDOM_TCCCSFEMTA A,TCREDITEXTRACT B
WHERE emtanrta = CREDIT
AND FECHA_PROCESO = (SELECT MAX(FECHA_PROCESO) FROM TDOM_TCCCSFEMTA WHERE FECHA_PROCESO <= P_FECHA )
AND EMTATTAR!='A'
AND (to_number(EMTALCR1) <> 0 OR to_number(EMTALCR2)<>0)
AND emtanrta NOT IN (Select CESTNRTA
                                                From Tdom_TcCCSFcest
                                                     Where FECHA_PROCESO =  A.FECHA_PROCESO
                                                         And UPPER(CESTMSG3) LIKE '%CANCELADA%')
and client in (select lca_codcli
                 from tleg_operacioncalif
                where lca_fecha = p_fecha
                  and lca_tipo = 'P')--permanente--mayores deudores
                  --and lca_codtipocredito='C')
       order by lca_cuenta desc;

    CURSOR COMEX IS(
      SELECT MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter * bdi_promedio(CYLETTER,branch,p_fecha) Montoaprobado,
             dateexpired pre_fecven,
             branch pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter) Balance,
             operator,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
             status
        FROM tlettercredit
       WHERE --NVL(status, '!') = '!'
       --decode(CYLETTER,
         --     0,
           --   valueletter,
             -- bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) > p_Montocompara AND
       seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
       codeletter not in
       (select codeletter
          FROM tlettercredit A, Tstageprod B, tstage C
         WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
               a.module = c.module AND a.prod = c.prod AND
               a.typeprod = c.typeprod AND a.seqamen = b.seq and
               c.stage = 'ST_CLOSE' and DATEPROCESS <= p_fecha + 0.99999)
      union
      SELECT a.MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter* bdi_promedio(CYLETTER,branch,p_fecha) Montoaprobado,
             dateexpired pre_fecven,
             branch pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter) Balance,
             operator,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             DATEPROCESS,
             a.status
        FROM tlettercredit A, Tstageprod B, tstage C
       WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
             a.module = c.module AND a.prod = c.prod AND
             a.typeprod = c.typeprod AND a.seqamen = b.seq AND
             c.stage = 'ST_CLOSE' AND NVL(a.status, '!') in ('C', 'P') AND
             --decode(CYLETTER,
               --     0,
                 --   valueletter,
                   -- bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) > p_Montocompara AND
             seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
             DATEPROCESS between p_datefrom and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codecollection,
                    amountval * bdi_promedio(CYCOLLECTION,branch,p_fecha) amountval,
                    dateexpired,
                    branch,
                    office,
                    pkg_coll_functs.balance_coll(codecollection) balance,
                    operator,
                    seqrequest,
                    CYCOLLECTION,
                    CODEDRAWEE,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    status
               FROM tcollection
              WHERE --NVL(status, '!') = '!'
              --decode(CYCOLLECTION,
                --     0,
                  --   amountval,
                    -- bdi_promedio(CYCOLLECTION, branch, p_fecha) * amountval) >
              --p_Montocompara AND
              codecollection not in
              (select codecollection
                 FROM tcollection A, Tstageprod B, tstage C
                WHERE a.codecollection = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND a.seqamen = b.seq and
                      c.stage = 'ST_CLOSE' and
                      DATEPROCESS <= p_fecha + 0.99999)
             UNION
             SELECT a.MODULE,
                    codeclient,
                    codecollection,
                    amountval * bdi_promedio(CYCOLLECTION,branch,p_fecha) amountval,
                    dateexpired,
                    branch,
                    office,
                    pkg_coll_functs.balance_coll(codecollection) balance,
                    operator,
                    seqrequest,
                    CYCOLLECTION,
                    CODEDRAWEE,
                    DATEPROCESS,
                    a.status
               FROM tcollection A, Tstageprod B, tstage C
              WHERE a.codecollection = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    --decode(CYCOLLECTION,
                      --     0,
                        --   amountval,
                          -- bdi_promedio(CYCOLLECTION, branch, p_fecha) *
                           --amountval) > p_Montocompara AND
                    DATEPROCESS between p_datefrom and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codedraft,
                    valuedraft*bdi_promedio(CYDRAFT,branch,p_fecha) valuedraft,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codedraft) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    status
               FROM tdraft
              WHERE --NVL(status, '!') = '!'
              --decode(CYDRAFT,
                --     0,
                  --   valuedraft,
                    -- bdi_promedio(CYDRAFT, branch, p_fecha) * valuedraft) >
              --P_Montocompara AND
              codedraft not in
              (select codedraft
                 FROM tdraft A, Tstageprod B, tstage C
                WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND
                     --a.seqamen = b.seq and
                      c.stage = 'ST_CLOSE' and
                      DATEPROCESS <= p_fecha + 0.99999)
             UNION
             SELECT a.MODULE,
                    codeclient,
                    codedraft,
                    valuedraft*bdi_promedio(CYDRAFT,branch,p_fecha) valuedraft,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codedraft) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    DATEPROCESS,
                    a.status
               FROM tdraft A, Tstageprod B, tstage C
              WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    --decode(CYDRAFT,
                      --     0,
                        --   valuedraft,
                          -- bdi_promedio(CYDRAFT, branch, p_fecha) *
                           --valuedraft) > P_Montocompara AND
                    DATEPROCESS between p_datefrom and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codeguarantee,
                    valueguarantee*bdi_promedio(CYGUARANTEE,branch,p_fecha) valueguarantee,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codeguarantee) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    status
               FROM tguarantee
              WHERE --NVL(status, '!') = '!' AND
              --decode(CYGUARANTEE,
                --     0,
                  --   valueguarantee,
                    -- bdi_promedio(CYGUARANTEE, branch, p_fecha) *
                     --valueguarantee) > p_Montocompara AND
              seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
              codeguarantee not in
              (select codeguarantee
                 FROM tguarantee A, Tstageprod B, tstage C
                WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND a.seqamen = b.seq and
                      c.stage = 'ST_CLOSE' and
                      DATEPROCESS <= p_fecha + 0.99999)
             UNION
             SELECT a.MODULE,
                    codeclient,
                    codeguarantee,
                    valueguarantee*bdi_promedio(CYGUARANTEE,branch,p_fecha) valueguarantee,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codeguarantee) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    DATEPROCESS,
                    a.status
               FROM tguarantee A, Tstageprod B, tstage C
              WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND a.seqamen = b.seq AND
                    c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    --decode(CYGUARANTEE,
                      --     0,
                        --   valueguarantee,
                          -- bdi_promedio(CYGUARANTEE, branch, p_fecha) *
                          -- valueguarantee) > p_Montocompara AND
                    seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
                    DATEPROCESS between p_datefrom and p_fecha + 0.99999);
CURSOR G IS
  select distinct codgrupo
  from TCREXLIM_aux
  where nvl(codgrupo,0) <> 0
  and TIPOCREDITO = 'C' ;
w_codtipocredito varchar2(1);
  BEGIN
   DELETE TCREXLIM WHERE PERIODO = TO_CHAR(P_FECHA,'YYYYMM');
   DELETE TCREXLIM_aux WHERE PERIODO = TO_CHAR(P_FECHA,'YYYYMM');
   COMMIT;
    If trunc(p_fecha) < trunc(sysdate) then
      pkg_legalreport.eraser(p_report, p_session);
      For i in CREDITOS loop
        pkg_legalreport3.datos_cliente(i.lca_codcli,
                                       identifica,
                                       tipocli,
                                       w_nombre,
                                       apellido);
        relacion_banco:=null;
        w_identifica := identifica;
        Begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabgrupo
           AND DES_CODIGO = cli_grupoeco
           and cli_codigo = i.lca_codcli;
        EXCEPTION
          when no_data_found then
            relacion_banco := null;
          when others then
            raise_application_error(-20209,
                                    'Error relación con el banco del cliente ' ||
                                    i.lca_codcli || ' ' || sqlerrm);
        End;
        if relacion_banco is null then
          begin
          select CODGRUPO
            into relacion_banco
            from tgrupos_riesgosupbanco
           where IDENTIFICA=w_identifica
             and rownum = 1;
          exception
            when others then
               relacion_banco:=null;
           end;
        end if;
        --garantia_adm:= i.lca_montogaradmi;
        w_valorparam:= pkg_precalifica_do.parametro_calif('VALOR_GARANTIZADO_DE');
        IF NVL(w_valorparam,'R') = 'R' THEN
         select sum(nvl(log_valorgarantizadoreal,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = i.lca_cuenta;
        ELSE
         select sum(nvl(log_valorgarantizadofisa,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = i.lca_cuenta;
        END IF ;
          if i.clc_tiporel = 5 then
             W_FINANCI:='D';
          else
                    W_FINANCI:='I';
          end if;
        secuencial := secuencial + 1;
         select codigo
           into W_TIPFINANCI
          from TCRTIPFINANCIA
          where gmonto= i.lca_codtipocredito
            and tipo = 'P';

       INSERT INTO TCREXLIM_aux(PERIODO,NUMERO,CODGRUPO,IDPERSONA,NOMBRE,IDCREDITO,TIPFINANCI,MDEUDA,FINANCI,MADMISIBLE,TIPOCREDITO)
       VALUES(to_char(p_fecha,'yyyymm'),secuencial,relacion_banco,identifica,W_NOMBRE,i.lca_cuenta,W_TIPFINANCI,nvl(i.lca_capital,0) + nvl(i.lca_interes,0),W_FINANCI,nvl(garantia_adm,0),i.lca_codtipocreditoaux);

      End loop; -- Cursor CREDITOS
      /*-------------------------------------------------lineas-----------------------------------------------*/
      For i in lineas loop
        pkg_legalreport3.datos_cliente(i.lca_codcli,
                                       identifica,
                                       tipocli,
                                       w_nombre,
                                       apellido);

        relacion_banco:=null;
        w_identifica:=identifica;
        Begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabgrupo
           AND DES_CODIGO = cli_grupoeco
           and cli_codigo = i.lca_codcli;
        EXCEPTION
          when no_data_found then
            relacion_banco := null;
          when others then
            raise_application_error(-20209,
                                    'Error relación con el banco del cliente ' ||
                                    i.lca_codcli || ' ' || sqlerrm);
        End;
        if relacion_banco is null then
          begin
          select CODGRUPO
            into relacion_banco
            from tgrupos_riesgosupbanco
           where IDENTIFICA=w_identifica
             and rownum = 1;
          exception
            when others then
               relacion_banco:=null;
           end;
        end if;
        --garantia_adm:= i.lca_montogaradmi;
         w_valorparam:= pkg_precalifica_do.parametro_calif('VALOR_GARANTIZADO_DE');
        IF NVL(w_valorparam,'R') = 'R' THEN
         select sum(nvl(log_valorgarantizadoreal,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = i.lca_credito;
        ELSE
         select sum(nvl(log_valorgarantizadofisa,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = i.lca_credito;
        END IF ;



          if i.clc_tiporel = 5 then
             W_FINANCI:='D';
          else
                    W_FINANCI:='I';
          end if;
        secuencial := secuencial + 1;
        begin
         select codigo
           into W_TIPFINANCI
          from TCRTIPFINANCIA
          where gmonto= i.lca_codtipocredito
            and tipo = 'C';
        exception
          when no_data_found then
            W_TIPFINANCI:=null;
        end;
       INSERT INTO TCREXLIM_aux(PERIODO,NUMERO,CODGRUPO,IDPERSONA,NOMBRE,IDCREDITO,TIPFINANCI,MDEUDA,FINANCI,MADMISIBLE,TIPOCREDITO)
       VALUES(to_char(p_fecha,'yyyymm'),secuencial,relacion_banco,identifica,W_NOMBRE,i.lca_cuenta,W_TIPFINANCI,nvl(i.lca_capital,0) + nvl(i.lca_interes,0),W_FINANCI,nvl(garantia_adm,0),i.lca_codtipocreditoaux);

      End loop; -- Cursor lineas
     /*-------------------------------------------------lineas-----------------------------------------------*/
      For i in tc loop
        pkg_legalreport3.datos_cliente(i.lca_codcli,
                                       identifica,
                                       tipocli,
                                       w_nombre,
                                       apellido);

        relacion_banco:=null;
        w_identifica:=identifica;
        Begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabgrupo
           AND DES_CODIGO = cli_grupoeco
           and cli_codigo = i.lca_codcli;
        EXCEPTION
          when no_data_found then
            relacion_banco := null;
          when others then
            raise_application_error(-20209,
                                    'Error relación con el banco del cliente ' ||
                                    i.lca_codcli || ' ' || sqlerrm);
        End;
        if relacion_banco is null then
          begin
          select CODGRUPO
            into relacion_banco
            from tgrupos_riesgosupbanco
           where IDENTIFICA=w_identifica
             and rownum = 1;
          exception
            when others then
               relacion_banco:=null;
           end;
        end if;
        --garantia_adm:= i.lca_montogaradmi;
         w_valorparam:= pkg_precalifica_do.parametro_calif('VALOR_GARANTIZADO_DE');
        IF NVL(w_valorparam,'R') = 'R' THEN
         select sum(nvl(log_valorgarantizadoreal,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = i.lca_cuenta;
        ELSE
         select sum(nvl(log_valorgarantizadofisa,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = i.lca_cuenta;
        END IF ;


        w_codtipocredito:=null;
       begin
         select lca_codtipocredito
           into w_codtipocredito
         from tleg_operacioncalif
        where lca_fecha = p_fecha
          and lca_tipo = 'P'
          and lca_codcli = i.lca_codcli
          and lca_codtipocredito = 'C'
          and rownum = 1;
       exception
         when no_data_found then
             w_codtipocredito:='O';
       end;
          if i.clc_tiporel = 5 then
             W_FINANCI:='D';
          else
                    W_FINANCI:='I';
          end if;
        secuencial := secuencial + 1;
        begin
         select codigo
           into W_TIPFINANCI
          from TCRTIPFINANCIA
          where gmonto= i.lca_codtipocredito
            and tipo = 'T';
        exception
          when no_data_found then
            W_TIPFINANCI:=null;
        end;
       INSERT INTO TCREXLIM_aux(PERIODO,NUMERO,CODGRUPO,IDPERSONA,NOMBRE,IDCREDITO,TIPFINANCI,MDEUDA,FINANCI,MADMISIBLE,TIPOCREDITO)
       VALUES(to_char(p_fecha,'yyyymm'),secuencial,relacion_banco,identifica,W_NOMBRE,i.lca_cuenta,W_TIPFINANCI,nvl(i.lca_capital,0) + nvl(i.lca_interes,0),W_FINANCI,nvl(garantia_adm,0),w_codtipocredito);

      End loop; -- Cursor lineas
      /*-------------------------------------------------COMEX-----------------------------------------------*/
      For i in COMEX loop
        dbms_output.put_line('dentro del loop');
        pkg_legalreport3.datos_cliente(i.pre_clientep,
                                       identifica,
                                       tipocli,
                                       w_nombre,
                                       apellido);

        ---------------------------------------
        ---- Cambios para resolver hd_4324 ----
        ---------------------------------------
        -- Garantía Admisible
        BEGIN
          --Se cambia a valorcomercial por que el cliente indica que es el valor admisible y no el valor garantiza
          --novedad reportada por yomalin troncoso usuario de garantias con el e-mail del dia 2004/03/22 8:49 de mañana
          --se comenta este caso tambien a Francis Collado Help Desk
          --select sum(ogr_valor)
          select sum(gar_valorcomercial)
            into garantia_adm
            from tgar_garantias, tcli_opergaran
           where ogr_cliente = gar_codcli and ogr_numgaran = gar_numero and
                 ogr_mod = i.pre_mod and ogr_operacion = i.pre_credito and
                 ogr_fecdesde is not null and ogr_fechasta is null;
        Exception
          When no_data_found then
            garantia_adm := null;
          when others then
            raise_application_error(-20210,
                                    'Error al obtener Garantía Admisible ' ||
                                    sqlerrm);
        END;

       --INSERT INTO TCREXLIM(PERIODO,NUMERO,CODGRUPO,IDPERSONA,NOMBRE,IDCREDITO,TIPFINANCI,MDEUDA,FINANCI,MADMISIBLE)
       --VALUES();

      end loop;

      for x in g loop
        insert into TCREXLIM(PERIODO,NUMERO,CODGRUPO,IDPERSONA,NOMBRE,IDCREDITO,TIPFINANCI,MDEUDA,FINANCI,MADMISIBLE)
         select PERIODO,NUMERO,CODGRUPO,IDPERSONA,NOMBRE,IDCREDITO,TIPFINANCI,MDEUDA,FINANCI,MADMISIBLE
           from TCREXLIM_aux
          where codgrupo = x.codgrupo;
      end loop;
      Commit;
      --pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
    Else
      raise_application_error(-20806,
                              '!!! MES NO CONCLUIDO ¡¡¡, DEBE SELECCIONAR MESES CONCLUIDOS ' ||
                              sqlerrm);
    End if;
  END;
  ------------------------------------------------------------------------------
  -- PROCEDIMIENTO PARA OBTENER CREDITOS COMERCIALES DE LA ENTIDAD POR DEUDOR --
  ------------------------------------------------------------------------------
  -- PRCP 19/04/2002
  PROCEDURE de11(p_report       in varchar2,
                 p_session      in number,
                 p_sucursal     in number,
                 p_datefrom     in date,
                 p_fecha        in date,
                 p_tipocredito1 in number,
                 p_tipocredito2 in number,
                 p_tipocredito3 in number,
                 p_montocompara in number,
                 p_decimales    in number) IS

    fecha_primer_pago     date;
    valor_primer_pago     tpre_cuotas.cuo_valor%type;
    existe                char(1);
    PERIODOint            char(2);
    periodocap            char(2);
    moneda                char(1);
    relacion_banco        char(8);
    calificacion_actual   tpre_califica.CAL_CALIFACTUAL%type;
    v_calificacion_actual varchar2(2);
    --
    v_calificacion_expuesto varchar2(2);
    v_calificacion_cubierto varchar2(2);
    v_califica_pais         varchar2(2);
    --
    provision_requerida tpre_calificadet.CAD_PREVCONSTITCONTIN%type;
    primera_cuota       number(18, 6);
    numero_cuotas       number(3);
    total               number(18, 6);

    -- Definicion de variables para identificar al deudor
    nombre       tcli_persona.cli_nombre%type;
    apellido     tcli_persona.cli_nombre%type;
    identifica   varchar2(20);
    tipocli      char(2);
    tipo_recurso varchar2(2);
    secuencial   number(7) := 0;

    -- Variables para hd4324
    garantia_adm           number(18, 6);
    fecha_restruct         date;
    fecha_renova           date;
    actividad_pciiu        number;
    destino_credito        varchar2(10); --number;
    localidad              VARCHAR2(8);--2012
    P_GRACIA               TPRE_PRESTAMOS.PRE_NUMPERGRACIA%TYPE;
    v_balance_fecha        tpre_prestamos.pre_monto%type;
    v_monto_aprobado       tpre_prestamos.pre_monto%type;
    V_FECHA_RENOVA         DATE;
    v_destinatario_comex   varchar2(80);
    v_IDdestinatario_comex tcli_persona.cli_identifica%type;
    v_codigo_pais          VARCHAR2(2);
    v_fechaAprobacion      DATE := NULL;
    v_fechaDesembolso      DATE := NULL;
    v_numcomprom           NUMBER;
    v_fechacancelacion     DATE;
    vld_frevtasa           date;
    vld_fcuoextra          date;
    vln_monextra           number(15,2);
    w_diasper number :=30;
    v_tipocliente varchar2(3);
    v_facilidad  varchar2(3);
        provision_requerida_cap number:=0;
        provision_gradual_cap number:=0;
        provision_constituida_cap number:=0;
        provision_requerida_int number:=0;
        provision_gradual_int number:=0;
    w_tasa number;
    v_incrementos number:=0;
    CURSOR CREDITOS IS
      select pre_diasper,
             pre_diaspercap,
             pre_credito,
             pre_numcomprom,
             nvl(pre_fecoriginal, pre_fecemi) pre_fecemi, --cambio realizado el 2005/01/07
             round(pkg_legalreport3.cotiza_mon(p_fecha,
                                               pre_monto,
                                               pre_moneda,
                                               p_sucursal),
                   p_decimales) m,
             pre_fecontab,
             round(pkg_legalreport3.cotiza_mon(p_fecha,pre_valent,pre_moneda,p_sucursal),p_decimales) v, --se comenta por que deben tener el monto a la fecha de generación . Sra. Tania 28/05/2008
            --round(pkg_legalreport3.cotiza_mon(p_fecha,pre_MONTO,pre_moneda,p_sucursal),p_decimales) v, --se comenta por que deben tener el monto a la fecha de generación . Sra. Tania 28/05/2008
             --lca_capital v,--por confirmar con Johanna
             pre_fecven,
             pre_tasapac t,
             pre_moneda,
             pre_tipocuo,
             pre_clientep,
             pre_mod,
             pre_pro,
             pre_tip,
             PRE_NUMPERGRACIA,
             LCA_FECREESTRUC pre_fecarr,
             ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = PRE_SUCURSAL ) pre_sucursal,
             pre_oficina,
             pre_numdir,
             pre_fecance,
             lca_fecrenov,
             lca_fecrefin,
             lca_demandajundicial,
             lca_montogaradmi,
             lca_codtipocredito,
             LCA_MONTOAPROBADO,
             LCA_MONTOCUOTAEXTRAOR,          
			LCA_TIPOTASA          ,         
			LCA_BALPROMDCAPMES    ,         
			LCA_INTDEVENCORTE     ,         
			LCA_COMCARGODEVCORTE  ,         
			LCA_ESTRUCTURACIONCRE ,         
			LCA_ORIGENCREDITO     ,         
			LCA_TIPORECURSO ,
			substr(pre_extrefer,1,21) pre_extrefer               
        from tpre_prestamos,tleg_operacioncalif
       where pre_mod = lca_mod
         and pre_credito = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito IN ('C','M','R','D')--mayores deudores--JUNIO 2015
         and lca_cuenta not in (select pre_credito
							        from tpre_prestamos,tleg_operacioncalif
							       where pre_mod = lca_mod
							         and pre_credito = lca_cuenta
							         and lca_fecha = (select max(lca_fecha)
							                           from tleg_operacioncalif
							                           where lca_fecha < p_fecha)--p_fecha
							         and lca_tipo = 'P'           --permanente
							         and lca_codtipocredito = 'C' --
							         and pre_status = '9'
							         and trunc(pre_fecance) between p_datefrom and p_fecha )
UNION
      select 0 pre_diasper,
             0 pre_diaspercap,
             vis_numcue pre_credito,
             crd_compromaso pre_numcomprom,
             crd_fechavig pre_fecemi, --cambio realizado el 2005/01/07
             round(pkg_legalreport3.cotiza_mon(p_fecha,
                                               crd_valor,
                                               vis_moneda,
                                               p_sucursal),
                   p_decimales) m,
             crd_fechavig pre_fecontab,
             round(pkg_legalreport3.cotiza_mon(p_fecha,crd_valor,vis_moneda,p_sucursal),p_decimales) v, --se comenta por que deben tener el monto a la fecha de generación . Sra. Tania 28/05/2008
             --lca_capital v,--por confirmar con Johanna
             crd_fechavenc pre_fecven,
             --aca_tasaint  t,
             1 t,
             vis_moneda pre_moneda,
             1 pre_tipocuo,
             vis_codcli pre_clientep,
             vis_mod pre_mod,
             vis_pro pre_pro,
             vis_tip pre_tip,
             0 PRE_NUMPERGRACIA,
             LCA_FECREESTRUC pre_fecarr,
            ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = vis_suc ) pre_sucursal,
             vis_ofi pre_oficina,
             vis_numdircor pre_numdir,
             crd_fechavenc pre_fecance,
             lca_fecrenov,
             lca_fecrefin,
             lca_demandajundicial,
             lca_montogaradmi,
             lca_codtipocredito,
             LCA_MONTOAPROBADO,
             LCA_MONTOCUOTAEXTRAOR,          
			LCA_TIPOTASA          ,         
			LCA_BALPROMDCAPMES    ,         
			LCA_INTDEVENCORTE     ,         
			LCA_COMCARGODEVCORTE  ,         
			LCA_ESTRUCTURACIONCRE ,         
			LCA_ORIGENCREDITO     ,         
			LCA_TIPORECURSO ,
			'0' pre_extrefer                               
        from tcap_vista,--tcap_acract,
             tcap_credvista, tleg_operacioncalif
       where vis_mod = lca_mod
         and vis_numcue = lca_cuenta
         and lca_cuenta = crd_numcue
         --and aca_numcue = vis_numcue
         --and aca_fecha = lca_fecha
         --and aca_numcue = crd_numcue
         --and aca_tipocre = crd_tipocred
         --and aca_secuencia = crd_secuencia
         and lca_fecha = p_fecha
         and crd_tipocred = 2
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito IN ('C','M','D')--mayores deudores
         and  trunc(crd_fechautor) <= P_FECHA
					   and  crd_fechavenc >= P_FECHA
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
       order by v desc;
/*      union
      select pre_diasper,
             pre_diaspercap,
             pre_credito,
             pre_numcomprom,
             nvl(pre_fecoriginal, pre_fecemi) pre_fecemi, --cambio realizado el 2005/01/07
             round(pkg_legalreport3.cotiza_mon(p_fecha,
                                               pre_monto,
                                               pre_moneda,
                                               p_sucursal),
                   p_decimales) m,
             pre_fecontab,
             round(pkg_legalreport3.cotiza_mon(p_fecha,
                                               pre_valent,
                                               pre_moneda,
                                               p_sucursal),
                   p_decimales) v,
             pre_fecven,
             pre_tasapac t,
             pre_moneda,
             pre_tipocuo,
             pre_clientep,
             pre_mod,
             pre_pro,
             pre_tip,
             PRE_NUMPERGRACIA,
             LCA_FECREESTRUC pre_fecarr,
             pre_sucursal,
             pre_numdir,
             pre_fecance,
             lca_fecrenov,
             lca_demandajundicial,
             lca_montogaradmi
        from tpre_prestamos,tleg_operacioncalif
       where pre_mod = lca_mod
         and pre_credito = lca_cuenta
         and lca_fecha = (select max(lca_fecha)
                           from tleg_operacioncalif
                           where lca_fecha < p_fecha)--p_fecha
         and lca_tipo = 'P'           --permanente
         and lca_codtipocredito = 'C' --
         and pre_status = '9'
         and trunc(pre_fecance) between p_datefrom and p_fecha -- salgan los cancelados
       order by v desc;*/
    CURSOR COMEX IS(
      SELECT MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter * bdi_promedio(CYLETTER,branch,p_fecha) Montoaprobado,
             dateexpired pre_fecven,
             ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = branch) pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter) Balance,
             operator,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
             status
        FROM tlettercredit
       WHERE --NVL(status, '!') = '!'
       --decode(CYLETTER,0,valueletter,bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) > p_Montocompara AND
       seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
       codeletter not in
       (select codeletter
          FROM tlettercredit A, Tstageprod B, tstage C
         WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
               a.module = c.module AND a.prod = c.prod AND
               a.typeprod = c.typeprod AND a.seqamen = b.seq and
               c.stage = 'ST_CLOSE' and DATEPROCESS <= p_fecha + 0.99999)
      union
      SELECT a.MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter* bdi_promedio(CYLETTER,branch,p_fecha) Montoaprobado,
             dateexpired pre_fecven,
             ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = branch) pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter) Balance,
             operator,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             DATEPROCESS,
             a.status
        FROM tlettercredit A, Tstageprod B, tstage C
       WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
             a.module = c.module AND a.prod = c.prod AND
             a.typeprod = c.typeprod AND a.seqamen = b.seq AND
             c.stage = 'ST_CLOSE' AND NVL(a.status, '!') in ('C', 'P') AND
             --decode(CYLETTER,0,valueletter,bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) >p_Montocompara AND
             seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
             DATEPROCESS between p_datefrom and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codecollection,
                    amountval * bdi_promedio(CYCOLLECTION,branch,p_fecha) amountval,
                    dateexpired,
                    ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = branch) branch,
                    office,
                    pkg_coll_functs.balance_coll(codecollection) balance,
                    operator,
                    seqrequest,
                    CYCOLLECTION,
                    CODEDRAWEE,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    status
               FROM tcollection
              WHERE --NVL(status, '!') = '!'
              --decode(CYCOLLECTION,0,amountval,bdi_promedio(CYCOLLECTION, branch, p_fecha) * amountval) > p_Montocompara AND
              1=0  AND --PARA QUE NO SALGAN LAS COBRABZAS --
              codecollection not in
              (select codecollection
                 FROM tcollection A, Tstageprod B, tstage C
                WHERE a.codecollection = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND a.seqamen = b.seq and
                      c.stage = 'ST_CLOSE' and
                      DATEPROCESS <= p_fecha + 0.99999)
             UNION
             SELECT a.MODULE,
                    codeclient,
                    codecollection,
                    amountval * bdi_promedio(CYCOLLECTION,branch,p_fecha) amountval,
                    dateexpired,
                    ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = branch) branch,
                    office,
                    pkg_coll_functs.balance_coll(codecollection) balance,
                    operator,
                    seqrequest,
                    CYCOLLECTION,
                    CODEDRAWEE,
                    DATEPROCESS,
                    a.status
               FROM tcollection A, Tstageprod B, tstage C
              WHERE a.codecollection = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    1=0  AND --PARA QUE NO SALGAN LAS COBRABZAS --
                    a.typeprod = c.typeprod AND c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    --decode(CYCOLLECTION,0,amountval,bdi_promedio(CYCOLLECTION, branch, p_fecha) *amountval) > p_Montocompara AND
                    DATEPROCESS between p_datefrom and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codedraft,
                    valuedraft*bdi_promedio(CYDRAFT,branch,p_fecha) valuedraft,
                    dateexpired,
                    ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = branch) branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codedraft) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    status
               FROM tdraft
              WHERE --NVL(status, '!') = '!'
              decode(CYDRAFT,
                     0,
                     valuedraft,
                     bdi_promedio(CYDRAFT, branch, p_fecha) * valuedraft) >
              P_Montocompara AND
              codedraft not in
              (select codedraft
                 FROM tdraft A, Tstageprod B, tstage C
                WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND
                     --a.seqamen = b.seq and
                      c.stage = 'ST_CLOSE' and
                      DATEPROCESS <= p_fecha + 0.99999)
             UNION
             SELECT a.MODULE,
                    codeclient,
                    codedraft,
                    valuedraft*bdi_promedio(CYDRAFT,branch,p_fecha) valuedraft,
                    dateexpired,
                    ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = branch) branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codedraft) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    DATEPROCESS,
                    a.status
               FROM tdraft A, Tstageprod B, tstage C
              WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    --decode(CYDRAFT,0,valuedraft,bdi_promedio(CYDRAFT, branch, p_fecha) *valuedraft) > P_Montocompara AND
                    DATEPROCESS between p_datefrom and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codeguarantee,
                    valueguarantee*bdi_promedio(CYGUARANTEE,branch,p_fecha) valueguarantee,
                    dateexpired,
                    ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = branch) branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codeguarantee) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    status
               FROM tguarantee
              WHERE --NVL(status, '!') = '!' AND
              --decode(CYGUARANTEE,0,valueguarantee,bdi_promedio(CYGUARANTEE, branch, p_fecha) *valueguarantee) > p_Montocompara AND
              seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
              codeguarantee not in
              (select codeguarantee
                 FROM tguarantee A, Tstageprod B, tstage C
                WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND a.seqamen = b.seq and
                      c.stage = 'ST_CLOSE' and
                      DATEPROCESS <= p_fecha + 0.99999)
             UNION
             SELECT a.MODULE,
                    codeclient,
                    codeguarantee,
                    valueguarantee*bdi_promedio(CYGUARANTEE,branch,p_fecha) valueguarantee,
                    dateexpired,
                    ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = branch) branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codeguarantee) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    DATEPROCESS,
                    a.status
               FROM tguarantee A, Tstageprod B, tstage C
              WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND a.seqamen = b.seq AND
                    c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    --decode(CYGUARANTEE,0,valueguarantee,bdi_promedio(CYGUARANTEE, branch, p_fecha) *valueguarantee) > p_Montocompara AND
                    seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
                    DATEPROCESS between p_datefrom and p_fecha + 0.99999);

  v_tipoid varchar2(1);                    
  v_fecautoriza DATE;
  v_FECHAREFINANCIACION date;                
  BEGIN

    If trunc(p_fecha) < trunc(sysdate) then
      dbms_output.put_line('en de11');
      pkg_legalreport.eraser(p_report, p_session);

      For i in CREDITOS loop
        dbms_output.put_line('dentro del loop');
        begin
          select cli_tipoid 
          into v_tipoid
           from tcli_persona
           where cli_codigo = i.pre_clientep;
        exception 
           when others then
              v_tipoid:=null;
        end;        
        pkg_legalreport3.datos_cliente(i.pre_clientep,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido);
        dbms_output.put_line('nombre ' || nombre || ' ' || apellido);
        dbms_output.put_line('id ' || identifica);
        dbms_output.put_line('tipoper ' || tipocli);
        v_calificacion_actual := null;
        --
        V_FECHA_RENOVA          := NULL;
        v_calificacion_expuesto := null;
        v_calificacion_cubierto := null;
        v_califica_pais         := null;
        v_codigo_pais           := NULL;
        v_destinatario_comex    := null;
        v_IDdestinatario_comex  := null;
        --
        v_balance_fecha  := 0;
        v_balance_fecha  := saldo_capital_fecha(i.pre_credito, p_fecha);
        v_monto_aprobado := 0;
        provision_requerida_cap:=0;
        provision_gradual_cap:=0;
        provision_constituida_cap:=0;
        provision_requerida_int:=0;
        provision_gradual_int:=0;

        IF i.pre_numcomprom is null then
          v_monto_aprobado := pkg_legal_Dom_de.monto_aprobado(i.pre_credito,
                                                           p_fecha);
        ELSE
          v_monto_aprobado := pkg_legal_Dom_de.monto_aprobado(i.pre_numcomprom,
                                                           p_fecha);
        END IF;

        Begin
          V_FECHA_RENOVA := i.lca_fecrenov; --PKG_LEGAL_DOM.FECHA_RENOVA(i.pre_credito,p_fecha); --PARA FECHA DE RENOVACION
          --TIPOS DE CUOTAS EXISTENTES (1,3,5,6,7,8) (2002/05/27)
          --AMORTIZACION GRADUAL (CUOTAS FIJAS DE CAPITAL + INTERESES)
          w_diasper:=30;
          If i.pre_tipocuo in (1,3,5,7) then
            BEGIN

              select cuo_fecha,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       cuo_valor,
                                                       i.pre_moneda,
                                                       p_sucursal),
                           p_decimales),
                     cuo_dias
                into fecha_primer_pago, valor_primer_pago,w_diasper
                from tpre_cuotas
               where cuo_credito = i.pre_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                fecha_primer_pago := null;
                valor_primer_pago := null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
          Elsif i.pre_tipocuo = 6 then
            BEGIN

              select let_fecven,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       let_valor,
                                                       i.pre_moneda,
                                                       p_sucursal),
                           p_decimales)
                into fecha_primer_pago, valor_primer_pago
                from tpre_letras
               where let_credito = i.pre_credito and let_sec = 1;

            EXCEPTION
              when no_data_found then
                fecha_primer_pago := null;
                valor_primer_pago := null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            --CUOTA FIJA DE CAPITAL
          Elsif i.pre_tipocuo = 8 then
            BEGIN
              Select count(*), sum(cuo_valor)--cuo_capital
                Into numero_cuotas, total
                From tpre_cuotas
               Where cuo_credito = i.pre_credito;

            EXCEPTION
              when no_data_found then
                numero_cuotas := 0;
                total         := 0;
              When others then
                raise_application_error(-20203,
                                        'Error al obtener el capital del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            BEGIN
              Select cuo_valor,cuo_dias --cuo_capital
                Into primera_cuota,w_diasper
                From tpre_cuotas
               Where cuo_credito = i.pre_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                primera_cuota := null;
              when others then
                raise_application_error(-20204,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            If numero_cuotas != 0 then
              If total / numero_cuotas = primera_cuota then

                BEGIN
                  Select cuo_fecha,
                         round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                           cuo_valor, --cuo_capital,
                                                           i.pre_moneda,
                                                           p_sucursal),
                               p_decimales)
                    Into fecha_primer_pago, valor_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = i.pre_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                    valor_primer_pago := null;
                  when others then
                    raise_application_error(-20205,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;
              Else
                BEGIN
                  Select cuo_fecha
                    Into fecha_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = i.pre_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                  when others then
                    raise_application_error(-20206,
                                            'Error al obtener fecha de la cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;
                begin
                  -- HD4324_2 (SE HIZO UN SUM PORQUE PUEDE HABER UN ARREGLO DE PAGOS Y TENDREMOS DOS VALORES PARA CUO_CAPITAL)
                  Select SUM(round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital,
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales))
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_fecha =
                         (Select max(cuo_fecha)
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito) and
                         cuo_credito = i.pre_credito 
                         /*cuo_numcuo = (Select max(cuo_numcuo) - 1
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito)*/;
                Exception
                  when no_data_found then
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = i.pre_credito;
                    -- insert into log_batch values (to_char(p_fecha,'yyyy/mm/dd')||' - '||to_char(i.Pre_credito)||' 1 ');
                  WHEN OTHERS THEN
                    raise_application_error(-20207,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                end;
              End if;
            Else
              fecha_primer_pago := null;
              valor_primer_pago := null;
            End if;
          Else
            fecha_primer_pago := null;
            valor_primer_pago := null;
          End if;

        End;
        --monto de cuota    2016/03/16 
        if i.pre_tipocuo <> 6 and substr(i.pre_credito,1,1) = 6 then
                begin
                  -- HD4324_2 (SE HIZO UN SUM PORQUE PUEDE HABER UN ARREGLO DE PAGOS Y TENDREMOS DOS VALORES PARA CUO_CAPITAL)
                  Select SUM(round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital,
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales))
                    into valor_primer_pago
                    From tpre_cuotas
                   Where  cuo_credito = i.pre_credito and
                         cuo_dias >=28 and                             
                         to_char(cuo_fecha,'yyyymm') = to_char(p_fecha,'yyyymm');--de DE11
                   
                    /*cuo_fecha =
                         (Select max(cuo_fecha)
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito) and
                         cuo_credito = i.pre_credito and
                         cuo_dias > 0*/--nuevo por error de rosmeario 05/2016
                         /*cuo_numcuo = (Select max(cuo_numcuo) - 1
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito)*/
                   if nvl(valor_primer_pago,0) = 0 then
                      begin
	                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
	                                                               cuo_valor,--cuo_capital
	                                                               i.pre_moneda,
	                                                               p_sucursal),
	                                   p_decimales) --HD4324_2
	                    into valor_primer_pago
	                    From tpre_cuotas
	                   Where cuo_NUMCUO = 1 and
	                         cuo_credito = i.pre_credito; 
	                         exception                  
                      WHEN OTHERS THEN
                    raise_application_error(-20207,
                                            'Error:' ||
                                            i.pre_credito || ' ' || sqlerrm);
                     end;  	                         
                   end if;
                Exception
                  when no_data_found then
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = i.pre_credito;
                    -- insert into log_batch values (to_char(p_fecha,'yyyy/mm/dd')||' - '||to_char(i.Pre_credito)||' 1 ');
                  WHEN OTHERS THEN
                    raise_application_error(-20207,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                end;       
              end if;
               
        --fin de monto cuota
        PERIODOS_INT(i.pre_tipocuo, w_diasper, PERIODOINT);
        --PERIODOS(i.pre_tipocuo,i.pre_diaspercap,PERIODOCAP);
        PERIODOS_CAP(i.pre_tipocuo, i.pre_diasper, PERIODOCAP);
        PERGRACIA(i.pre_credito, p_fecha, p_gracia);
        existe:=i.lca_demandajundicial;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calificacion de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --calificacion inicial-final
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          v_calificacion_actual:=calificacion(P_FECHA,'P',i.pre_credito,14);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_actual := null;
        End;
      --calificacion expuesta
        Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          v_calificacion_expuesto:=calificacion(P_FECHA,'P',i.pre_credito,13);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_expuesto := null;
        End;

      --calificacion cubierta
        Begin
          v_calificacion_cubierto:=calificacion(P_FECHA,'P',i.pre_credito,12);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_cubierto := null;
        End;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fin de calificacion
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- provision de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --calificacion inicial-final
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_requerida_cap:= provision(P_FECHA,'P',i.pre_credito,9);
        Exception
          when no_data_found then
			provision_requerida_cap := null;
        End;
      --gradual cap
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_gradual_cap:= provision(P_FECHA,'P',i.pre_credito,11);
        Exception
          when no_data_found then
			provision_gradual_cap := null;
        End;

      --calificacion cubierta
      --gradual constituida
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_constituida_cap:= provision(P_FECHA,'P',i.pre_credito,12);
        Exception
          when no_data_found then
			provision_constituida_cap := null;
        End;
      --requerida int
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_requerida_int:= provision(P_FECHA,'P',i.pre_credito,8);
        Exception
          when no_data_found then
			provision_requerida_int := null;
        End;
      --gradual int
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_gradual_int:= provision(P_FECHA,'P',i.pre_credito,10);
        Exception
          when no_data_found then
			provision_gradual_int := null;
        End;


  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fin de provision
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


        tipo_recurso := RECURSO(i.pre_mod, i.pre_credito);

        Begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabrel AND DES_CODIGO = cli_relacionban and
                 cli_codigo = i.pre_clientep;
        EXCEPTION
          when no_data_found then
            relacion_banco := 'N1';
          when others then
            raise_application_error(-20209,
                                    'Error relación con el banco del cliente ' ||
                                    i.pre_clientep || ' ' || sqlerrm);
        End;

        If i.pre_moneda = 0 then
          moneda := 'N';
        Else
          moneda := 'E';
        End if;

        --garantia_adm:= i.lca_montogaradmi;
         garantia_adm:= fun_garantia(i.pre_credito,p_fecha,'DE11');
        --para bdi la garantia admisible sera capital + interes si la garantia esta inscrita(f.Constitucion ) Rosemary hernandez 2012/05/25
        

        -- Fecha de Reestructuración queda pendiente, actualmente el Banco no maneja irá como nula

        -- Fecha de Renovación
        /* BEGIN
        select pre_fecarr
        Into fecha_renova
        from tpre_prestamos
        where pre_credito = i.pre_credito;
        Exception
           When no_data_found then
                fecha_renova := null;
           when others then
                raise_application_error(-20211,'Error al obtener Fecha de Renovación '||sqlerrm);
        END;*/

        -- Actividad principal del deudor en base al CIIU  (HD4324_2)
        BEGIN
          Select substr(CII_CODIGO, 1, 6)
            Into actividad_pciiu
            From tcli_natural, TPRE_PRESTAMOS, tgen_ciiu
           Where (PRE_CREDITO = i.pre_credito and pre_clientep = nat_codcli and
                 nat_actprin = cii_tipoact);
        Exception
          When no_data_found then
            actividad_pciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Natural' ||
                                    sqlerrm);
        END;
        BEGIN
          Select substr(CII_CODIGO, 1, 6) --HD4324_2
            Into actividad_pciiu
            From tcli_JURIDICA, TPRE_PRESTAMOS, tgen_ciiu
           Where (PRE_CREDITO = i.pre_credito and pre_clientep = JUR_codcli and
                 JUR_actividad = cii_tipoact);
        Exception
          When no_data_found then
            actividad_pciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Jurídico' ||
                                    sqlerrm);
        END;

        -- Destino del crédito en base al CIIU
        BEGIN   
         --actividad_pciiu:=fun_actividad_cliente(i.pre_clientep);      SE COMENTA POR E-MAIL DE ROSEMARY 2012/12/04 COLOCAR COMO ANTES DICE
         destino_credito:=fun_destino_ciiu(0,i.pre_credito,v_numcomprom);
          /*select lpad(substr(DES_CODIGOCIIU, 1, 6)
            Into destino_credito
            from tpre_prestamos, tpre_destecon
           where pre_destecon = des_codigo and pre_credito = i.pre_credito;*/
        Exception
          When no_data_found then
            destino_credito := null;
          when others then
            raise_application_error(-20213,
                                    'Error al obtener Destino Económico en base al CIIU del Deudor ' ||
                                    sqlerrm);
        END;

        --Localidad

        BEGIN                                  
        localidad:=fun_localidad(i.pre_sucursal,i.pre_oficina,i.pre_clientep,i.pre_numdir);
          /*select substr(codeiso, 1, 4)
            Into localidad
            from tgen_desctabla, tpre_prestamos
           where pre_tabciudad = des_codtab and pre_ciudes = des_codigo and
                 pre_credito = i.pre_credito;*/
           /*select substr(a.codeiso, 1, 6)
            Into localidad
            from tgen_desctabla a,tgen_oficina b
           where ofi_tabciu = des_codtab 
             and ofi_codciu = des_codigo
             and ofi_codsuc = i.pre_sucursal
             and ofi_codofi = i.pre_oficina;*/                 
        Exception
          When no_data_found then
            localidad := null;
          when others then
            raise_application_error(-20214,
                                    'Error al al obtener la Localidad ' ||
                                    i.pre_credito || sqlerrm);
        END;
        --PAIS
        v_codigo_pais := PKG_LEGAL_DOM.CODIGO_PAIS(i.pre_clientep,
                                                   i.pre_numdir);
        --

        -- dbms_output.put_line('localidad: '||localidad );

        -- Provisión requerida a Pesos Dominicanos
        provision_requerida := pkg_legalreport3.cotiza_mon(p_fecha,
                                                           provision_requerida,
                                                           i.pre_moneda,
                                                           p_sucursal);


        --<<HD8164  
        vln_monextra:=0;
        pro_gen_cuoextra(i.pre_credito,p_fecha,vln_monextra,vld_fcuoextra);  
        vln_monextra:=vln_monextra*bdi_promedio(i.pre_moneda,1,p_fecha);
        vld_frevtasa:=fun_gen_revtasa(i.pre_credito,p_fecha);
        -->>
        -- Insertamos datos en la tabla TVALUELEVELCUSTOM

        secuencial := secuencial + 1;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 1, secuencial, 1, secuencial); --NUMERO SECUENCIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 2, secuencial, 2, decode(v_tipoid,'P',REPLACE(identifica,'-',''),identifica)); --IDENTIFICACION DEL DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 3, secuencial, 3, tipocli); --TIPO DE DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 4, secuencial, 4, nombre); --NOMBRES / RAZON SOCIAL DEL DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 5, secuencial, 5, apellido); --APELLIDOS / SIGLAS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 6, secuencial, 6, i.pre_credito); --CODIGO DEL CREDITO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 7, secuencial, 7, i.pre_numcomprom); --CODIGO DE LA FACILIDAD
          
          --1 busco fecha de emision del compromiso
          --2 si no encuentro busco la fecha de aprobacion del desembolso en el comite
          --3 si no aparace fecha en el comite , informo la fecha de emision la que se ha estado reportado hasta la fecha
          --Alejandro, Omar , Esmeralda 2018/07/16
         /* begin
          select CMP_FECHAEMI
            into v_fecautoriza
           from tpre_comprom  
          where cmp_numero = i.pre_numcomprom;
          exception 
          when others then          
          begin
           select trunc(end_time) 
              into v_fecautoriza 
			from twkl_task@DB_EAPPFISA w
			where w.description like '% Comité%' 
			  and instr(description,i.pre_extrefer) > 0
			  and action_status = 3
			  and rownum = 1;
          exception
            when others then 
                v_fecautoriza:= i.pre_fecemi; 			  
          end;                          
          end;*/                   
          --reunion 2018/12/10 sr Sunchio, guido , viola
          --analisis llenara el cambio fecha de revision (fecha de aprobacion) para los comerciales que vayan a comite fisioc
          --si el campo esta en blanco o no se encuentra nada , se buscara la fecha maxima de negocio
          --si no encuenta la fecha maxima de negocio se reporta la misma fecha como hasta la reunion
           BEGIN
           v_fecautoriza:=fecha_aprobado(i.pre_numcomprom,i.pre_credito,i.pre_extrefer);
           EXCEPTION
              WHEN OTHERS THEN
                v_fecautoriza:=i.pre_fecemi ;
           END;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 8, secuencial, 8, v_fecautoriza);--i.pre_fecemi); se cambia 2018/12/10 --FECHA DE APROBACION DEL CREDITO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 9, secuencial, 9, i.LCA_MONTOAPROBADO); --v_monto_aprobado); --MONTO APROBADO

        --VALUES (p_report,p_session,9,secuencial,9,i.m);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 10, secuencial, 10, i.pre_fecontab); --FECHA DE DESEMBOLSO
                                 
          v_incrementos:=0;
           SELECT nvl(SUM(NVL(INC_MONTO,0))*bdi_promedio(i.pre_moneda,1,p_fecha),0)
              INTO  v_incrementos
		    FROM TPRE_INCCAPITAL 
		    WHERE INC_CREDITO= i.pre_credito      
		      AND INC_STATUS = '2'
		      AND TRUNC(INC_FECHAAUT)<=SYSDATE 
		      AND INC_NUMTRA IS NOT NULL;
      
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 11, secuencial, 11, i.v+v_incrementos); --MONTO DESEMBOLSADO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 12, secuencial, 12, i.pre_fecven); --FECHA DE VENCIMIENTO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 13, secuencial, 13, fecha_primer_pago); --FECHA INICIO DEL PRIMER PAGO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 14, secuencial, 14, valor_primer_pago); --MONTO DE LA CUOTA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 15, secuencial, 15, periodoCAP); --FORMA DE PAGO DEL CAPITAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 16, secuencial, 16, periodoINT); --FORMA DE PAGO DE INTERESES Y COMISIONES OJO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 17, secuencial, 17, P_GRACIA); --PERIODO DE GRACIA

		    begin
		     select aca_tasaint
		       into w_tasa
		      from tcap_acract
		       where aca_numcue = i.pre_credito
		         and aca_fecha = p_fecha
		         and aca_tipocre = 2;
		    exception
		    	  when others then
		    	    w_tasa:=i.t;
		    end;

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 18, secuencial, 18, w_tasa); --TASA DE INTERES Y COMISION VIGENTE

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 19, secuencial, 19, moneda); --TIPO DE MONEDA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 20, secuencial, 20, existe); --COBRANZA JUDICIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 21, secuencial, 21, v_CALIFICACION_actual); --CLASIFICACION DEL CREDITO SEGUN LA ENTIDAD
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report,
           p_session,
           22,
           secuencial,
           22,
           v_CALIFICACION_expuesto); --CALIFICACION MONTO EXPUESTO --ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report,
           p_session,
           23,
           secuencial,
           23,
           v_CALIFICACION_cubierto); --CALIFICACION MONTO CUBIERTO  --ojo
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 24, secuencial, 24, provision_requerida_cap); --PROVISION REQUERIDA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 25, secuencial, 25, tipo_recurso); --ORIGEN O TIPO DE RECURSO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 26, secuencial, 26, relacion_banco); --TIPO DE VINCULACION
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 27, secuencial, 27, garantia_adm); --GARANTIA ADMISIBLE
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 28, secuencial, 28, i.pre_fecarr); --FECHA DE REESTRUCTURACION
        -- Campos que se incrementaron para hd-4324
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 29, secuencial, 29, V_FECHA_RENOVA); --FECHA DE RENOVACION

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 30, secuencial, 30, localidad); --LOCALIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 31, secuencial, 31, destino_credito);--SUBSTR(actividad_pciiu,1,6)); --ACTIVIDAD PRINCIPAL DEL DEUDOR EN BASE AL CIUU

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 32, secuencial, 32, destino_credito); --DESTINO DEL CREDITO EN BASE AL CIUU

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 33, secuencial, 33, i.pre_sucursal); --NUMERO DE OFICINA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 34, secuencial, 34, v_califica_pais); --CLASIFICACION RIESGO PAIS  ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 35, secuencial, 35, v_codigo_pais); --CODIGO PAIS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 36, secuencial, 36, null); --FECHA INICIO PROCESO ADJUDICACION ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 37, secuencial, 37, v_IDdestinatario_comex); --IDENTIFICACION DESTINATARIO CONTINGENCIA ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 38, secuencial, 38, v_destinatario_comex); --IDENTIFICACION DESTINATARIO CONTINGENCIA ojo
          
       /* Se elimina por nuevos cambios de la SIB
       if trunc(i.pre_fecance) > p_fecha then
	        INSERT INTO tvaluelevelcustom
	          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
	        VALUES
	          (p_report, p_session, 39, secuencial, 39, null); --FECHA CANCELACION       ojo
        else
	        INSERT INTO tvaluelevelcustom
	          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
	        VALUES
	          (p_report, p_session, 39, secuencial, 39, i.pre_fecance); --FECHA CANCELACION       ojo
        end if;*/
        --<<HD8164
        /*
        --balance
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 40, secuencial, 40, v_balance_fecha);
        */
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 39, secuencial, 39, 'S'); --OPCION DE PAGO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 40, secuencial, 40, 0.00); --PENALIZACION POR PAGO

        /* POR NUEVOS CAMBIOS SIB
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 42, secuencial, 42, provision_gradual_cap); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE CAPITAL*/
          
        --arbil 2018
        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 41, secuencial, 41, provision_constituida_cap); */--PROVISION DE CAPITAL CONSTITUIDA POR EL CREDITO
          
        if trunc(i.pre_fecance) <= p_fecha then
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 42, secuencial, 42,0); --PROVISION REQUERIDA DE RENDIMIENTOS
        else
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 42, secuencial, 42, provision_requerida_int); --PROVISION REQUERIDA DE RENDIMIENTOS                    
        end if;

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 43, secuencial, 43, null); --PROVISION REQUERIDA DE CONTINGENCIAS

        if trunc(i.pre_fecance) <= p_fecha then
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 44, secuencial, 44, null); --FECHA REVISION TASA DE INTERES
        else
	         IF I.LCA_TIPOTASA = 'F' then
			        INSERT INTO tvaluelevelcustom
			          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
			        VALUES
			          (p_report, p_session, 44, secuencial, 44, NULL); --los cancelados la FECHA REVISION TASA DE INTERES     es nuk
              ELSE   
                        			        INSERT INTO tvaluelevelcustom
			          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
			        VALUES
			          (p_report, p_session, 44, secuencial, 44, vld_frevtasa); --los cancelados la FECHA REVISION TASA DE INTERES     es nuk

              END IF;
        end if;

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 45, secuencial, 45, vld_fcuoextra); --FECHA PAGO DE CUOTA EXTRAORDINARIA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 46, secuencial, 46, vln_monextra); --MONTO DE CUOTA EXTRAORDINARIA
        
       v_tipocliente:=fun_tipo_cliente(i.pre_clientep);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 47, secuencial, 47, v_tipocliente); --tipo de cliente
          
        /*if trunc(i.pre_fecance) <= p_fecha then
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 48, secuencial, 48, null); --FECHA REVISION TASA DE INTERES
        else
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 48, secuencial, 48, vld_frevtasa); --los cancelados la FECHA REVISION TASA DE INTERES     es nuka
        end if;*/
        v_facilidad:=fun_producto_servicio(i.pre_credito,6);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 48, secuencial, 48, v_facilidad); --FACILIDAD CREDITICIA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 49, secuencial, 49, NULL); --CANTIDAD DE PLASTICOS TC
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 50, secuencial, 50, NULL); --CODIGO SUBPRODUCTO TC

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 51, secuencial, 51, I.lca_codtipocredito); --TIPO DE CREDITO          
        -->>HD8164                                                                          
        --JUNIO 2015    
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 52, secuencial, 52, I.LCA_TIPOTASA); --LCA_TIPOTASA
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 53, secuencial, 53, I.LCA_BALPROMDCAPMES); --NULL EN PRESTAMOS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 54, secuencial, 54, I.LCA_INTDEVENCORTE); --NULL EN PRESTAMO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 55, secuencial, 55, I.LCA_COMCARGODEVCORTE); --NULL EN PRESTAMO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 56, secuencial, 56, I.LCA_ESTRUCTURACIONCRE); --NULL EN PRESTAMO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 57, secuencial, 57, I.LCA_ORIGENCREDITO); --NULL EN PRESTAMO                  
        --FIN JUNIO 2015
        --sysaid 15245 abril 2018       
        v_FECHAREFINANCIACION:=I.lca_fecrefin;
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 58, secuencial, 58, v_FECHAREFINANCIACION); --fecha refinanciacio

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 59, secuencial, 59,null); --RAZON CLASIFICACION DE RIESGO DEL DEUDOR
        
      End loop; -- Cursor CREDITOS
      /*-------------------------------------------------COMEX-----------------------------------------------*/
      For i in COMEX loop
        dbms_output.put_line('dentro del loop');  
        begin
          select cli_tipoid 
          into v_tipoid
           from tcli_persona
           where cli_codigo = i.pre_clientep;
        exception 
           when others then
              v_tipoid:=null;
        end;        
        pkg_legalreport3.datos_cliente(i.pre_clientep,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido);
        dbms_output.put_line('nombre ' || nombre || ' ' || apellido);
        dbms_output.put_line('id ' || identifica);
        dbms_output.put_line('tipoper ' || tipocli);
        v_calificacion_actual := null;
        --
        V_FECHA_RENOVA          := NULL;
        v_calificacion_expuesto := null;
        v_calificacion_cubierto := null;
        v_califica_pais         := null;
        v_codigo_pais           := NULL;
        v_destinatario_comex    := null;
        v_IDdestinatario_comex  := null;
        --
        v_balance_fecha  := 0;
        v_balance_fecha  := i.balance;
        v_monto_aprobado := 0;
        v_monto_aprobado := i.montoaprobado;
        Begin
          V_FECHA_RENOVA := null; --PKG_LEGAL_DOM.FECHA_RENOVA(i.pre_credito,p_fecha);--PARA FECHA DE RENOVACION
          --TIPOS DE CUOTAS EXISTENTES (1,3,5,6,7,8) (2002/05/27)
          --AMORTIZACION GRADUAL (CUOTAS FIJAS DE CAPITAL + INTERESES)
        End;
        Begin
          /*select 'S' into existe
          from  tpre_presleg
          where tpre_presleg.prl_credito = i.pre_credito
            and p_fecha between prl_desde and nvl(prl_hasta,to_date('2199/12/31','YYYY/MM/DD'));

          EXCEPTION
            when no_data_found then
            existe := 'N';
            when others then
              raise_application_error(-20208,'Error al obtener el crédito legal '||sqlerrm);*/
          existe := null;
        End;
        Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          /*select round(nvl(cad_prevconstitdir,0),p_decimales), cad_califcuenta
            into provision_requerida, calificacion_actual
            from TPRE_CALIFICAPROVDET
            where cad_fecha in (select max(cad_fecha) from TPRE_CALIFICAPROVDET
                               where cad_fecha <= p_fecha
                                 and cad_tipo = 'T'
                                 and cad_cuenta = i.pre_credito)
            and cad_tipo = 'T'
            and cad_cuenta = i.pre_credito;
            --Para desplegar la letra que correponde
            select substr(des_descripcion,1,2)
              into v_calificacion_actual
              from tgen_desctabla
             where des_codtab = 245
               and des_codigo = calificacion_actual;
            --fin
          Exception
            when no_data_found then
                provision_requerida := 0;
                calificacion_actual:=null;
               v_calificacion_actual:=null;   */
          provision_requerida := null;
          calificacion_actual := null;

        End;

        tipo_recurso := null; --RECURSO(i.pre_mod,i.pre_credito);

        Begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabrel AND DES_CODIGO = cli_relacionban and
                 cli_codigo = i.pre_clientep;
        EXCEPTION
          when no_data_found then
            relacion_banco := null;
          when others then
            raise_application_error(-20209,
                                    'Error relación con el banco del cliente ' ||
                                    i.pre_clientep || ' ' || sqlerrm);
        End;

        If i.pre_moneda = 0 then
          moneda := 'N';
        Else
          moneda := 'E';
        End if;

        ---------------------------------------
        ---- Cambios para resolver hd_4324 ----
        ---------------------------------------
        -- Garantía Admisible
        BEGIN
          --Se cambia a valorcomercial por que el cliente indica que es el valor admisible y no el valor garantiza
          --novedad reportada por yomalin troncoso usuario de garantias con el e-mail del dia 2004/03/22 8:49 de mañana
          --se comenta este caso tambien a Francis Collado Help Desk
          --select sum(ogr_valor)
          select sum(gar_valorcomercial)
            into garantia_adm
            from tgar_garantias, tcli_opergaran
           where ogr_cliente = gar_codcli and ogr_numgaran = gar_numero and
                 ogr_mod = i.pre_mod and ogr_operacion = i.pre_credito and
                 ogr_fecdesde is not null and ogr_fechasta is null;
        Exception
          When no_data_found then
            garantia_adm := null;
          when others then
            raise_application_error(-20210,
                                    'Error al obtener Garantía Admisible ' ||
                                    sqlerrm);
        END;

        -- Fecha de Reestructuración queda pendiente, actualmente el Banco no maneja irá como nula

        -- Fecha de Renovación
        /* BEGIN
        select pre_fecarr
        Into fecha_renova
        from tpre_prestamos
        where pre_credito = i.pre_credito;
        Exception
           When no_data_found then
                fecha_renova := null;
           when others then
                raise_application_error(-20211,'Error al obtener Fecha de Renovación '||sqlerrm);
        END;*/

        -- Actividad principal del deudor en base al CIIU  (HD4324_2)
        BEGIN
          Select substr(CII_CODIGO, 1, 6)
            Into actividad_pciiu
            From tcli_natural, tgen_ciiu
           Where nat_codcli = i.pre_clientep and nat_actprin = cii_tipoact;
        Exception
          When no_data_found then
            actividad_pciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Natural' ||
                                    sqlerrm);
        END;
        BEGIN
          Select substr(CII_CODIGO, 1, 6) --HD4324_2
            Into actividad_pciiu
            From tcli_JURIDICA, tgen_ciiu
           Where JUR_codcli = i.pre_clientep and
                 JUR_actividad = cii_tipoact;
        Exception
          When no_data_found then
            actividad_pciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Jurídico' ||
                                    sqlerrm);
        END;

        -- Destino del crédito en base al CIIU
        /*  BEGIN
          select substr(pre_destecon,1,4)
          Into destino_credito
          from tpre_prestamos,tpre_destecon
          where pre_destecon = des_codigo
            and pre_credito  =  i.pre_credito;
          Exception
          When no_data_found then
                destino_credito := null;
          when others then
                raise_application_error(-20213,'Error al obtener Destino Económico en base al CIIU del Deudor '||sqlerrm);
        END;*/
        --Localidad
        BEGIN
        localidad:=fun_localidad(i.pre_sucursal,i.pre_oficina,i.pre_clientep,1);
                                                   
           /*select substr(a.codeiso, 1, 6)
            Into localidad
            from tgen_desctabla a,tgen_oficina b
           where ofi_tabciu = des_codtab 
             and ofi_codciu = des_codigo
             and ofi_codsuc = i.pre_sucursal
             and ofi_codofi = i.pre_oficina; */                        
          /*select substr(codeiso, 1, 4)
            Into localidad
            from tgen_desctabla, tbankcorraso
           where codecitytable = des_codtab and codecity = des_codigo and
                 codeletter = i.pre_credito and coderol = 'RL_CONF' and
                 seqamen = pkg_discrep.ultima_lettercredit(codeletter) and
                 seqneg = (select max(seqneg)
                             from tbankcorraso
                            where codeletter = i.pre_credito);*/
        Exception
          When no_data_found then
            BEGIN
              select substr(codeiso, 1, 6)
                Into localidad
                from tgen_desctabla, tbankcoll
               where codecitytable = des_codtab and codecity = des_codigo and
                     codecollection = i.pre_credito and coderol = 'RL_PAY';
            Exception
              When no_data_found then
                BEGIN
                  select substr(codeiso, 1, 6)
                    Into localidad
                    from tgen_desctabla, tbankcorrdraft
                   where codecitytable = des_codtab and
                         codecity = des_codigo and
                         codedraft = i.pre_credito and
                         coderol = 'RL_CORRESP';
                Exception
                  When no_data_found then
                    BEGIN
                      select substr(codeiso, 1, 6)
                        Into localidad
                        from tgen_desctabla, tbankguarantee
                       where codecitytable = des_codtab and
                             codecity = des_codigo and
                             codeguarantee = i.pre_credito and
                             coderol = 'RL_CONF' and
                             seqamen =
                             PKG_GUAR_FUNCTS.last_seqamen(codeguarantee);
                    Exception
                      When no_data_found then
                        localidad := NULL;
                      when others then
                        raise_application_error(-20214,
                                                '3 Error al al obtener la Localidad ' ||
                                                sqlerrm);
                    END;
                  when others then
                    raise_application_error(-20214,
                                            '2 Error al al obtener la Localidad ' ||
                                            sqlerrm);
                END;
              when others then
                raise_application_error(-20214,
                                        '1 Error al al obtener la Localidad ' ||
                                        sqlerrm);
            END;
          when others then
            raise_application_error(-20214,
                                    'Error al al obtener la Localidad ' ||
                                    i.pre_credito || sqlerrm);
        END;
        --PAIS
        --v_codigo_pais := PKG_LEGAL_DOM.CODIGO_PAIS(i.pre_clientep,i.pre_numdir);
        BEGIN
          select substr(codeiso, 1, 4)
            Into v_codigo_pais
            from tgen_desctabla, tbankcorraso
           where CODECOUNTRYTABLE = des_codtab and CODECOUNTRY = des_codigo and
                 codeletter = i.pre_credito and coderol = 'RL_CONF' and
                 seqamen = pkg_discrep.ultima_lettercredit(codeletter) and
                 seqneg = (select max(seqneg)
                             from tbankcorraso
                            where codeletter = i.pre_credito);
        Exception
          When no_data_found then
            BEGIN
              select substr(codeiso, 1, 4)
                Into v_codigo_pais
                from tgen_desctabla, tbankcoll
               where CODECOUNTRYTABLE = des_codtab and
                     CODECOUNTRY = des_codigo and
                     codecollection = i.pre_credito and coderol = 'RL_PAY';
            Exception
              When no_data_found then
                BEGIN
                  select substr(codeiso, 1, 4)
                    Into v_codigo_pais
                    from tgen_desctabla, tbankcorrdraft
                   where CODECOUNTRYTABLE = des_codtab and
                         CODECOUNTRY = des_codigo and
                         codedraft = i.pre_credito and
                         coderol = 'RL_CORRESP';
                Exception
                  When no_data_found then
                    BEGIN
                      select substr(codeiso, 1, 4)
                        Into v_codigo_pais
                        from tgen_desctabla, tbankguarantee
                       where CODECOUNTRYTABLE = des_codtab and
                             CODECOUNTRY = des_codigo and
                             codeguarantee = i.pre_credito and
                             coderol = 'RL_CONF' and
                             seqamen =
                             PKG_GUAR_FUNCTS.last_seqamen(codeguarantee);
                    Exception
                      When no_data_found then
                        localidad := NULL;
                      when others then
                        raise_application_error(-20214,
                                                '3 Error al al obtener la Localidad ' ||
                                                sqlerrm);
                    END;
                  when others then
                    raise_application_error(-20214,
                                            '2 Error al al obtener la Localidad ' ||
                                            sqlerrm);
                END;
              when others then
                raise_application_error(-20214,
                                        '1 Error al al obtener la Localidad ' ||
                                        sqlerrm);
            END;
          when others then
            raise_application_error(-20214,
                                    'Error al al obtener la Localidad ' ||
                                    i.pre_credito || sqlerrm);
        END;
        --fecha de aprobacion del credito
        BEGIN
          select sol_fecha
            into v_fechaAprobacion
            from tsol_solicitud
           where sol_operador = i.operator and sol_numero = i.seqrequest;
        EXCEPTION
          WHEN OTHERS THEN
            v_fechaAprobacion := null;
        END;
        --fecha de desembolso
        BEGIN
          select DATEPROCESS
            into v_fechaDesembolso
            from tstageprod A, TSTAGE B
           where code = i.pre_credito and A.seqstage = B.SEQSTAGE and
                 stage = 'S_OPEN';
        EXCEPTION
          WHEN OTHERS THEN
            v_fechaDesembolso := null;
        END;
        --fin
        --codigo de facilidad
        --destino del credito
        BEGIN   
        --actividad_pciiu:=fun_actividad_cliente(i.pre_clientep); SE COMNETA POR E-MAIL DE ROSEMATRY 2012/12/04
          destino_credito:=fun_destino_ciiu(0,i.pre_credito,v_numcomprom);
          /*select DES_CODIGOCIIU, CODELINE
            into destino_credito, v_numcomprom
            from tclientline, tpre_destecon
           where ECONOMICDESTINITY = DES_CODIGO(+) and
                 CODEOPERATION = i.pre_credito;*/
        EXCEPTION
          WHEN OTHERS THEN
            destino_credito := null;
            v_numcomprom    := null;
        END;

        --FECHA DE CANCELACION
        BEGIN
          select DATEPROCESS
            into v_fechacancelacion
            from tstageprod A, TSTAGE B
           where code = i.pre_credito and A.seqstage = B.SEQSTAGE and
                 stage = 'ST_CLOSE';
        EXCEPTION
          WHEN OTHERS THEN
            v_fechaCancelacion := null;
        END;
        --BENEFICIARIO
        BEGIN
          select bene
            into v_destinatario_comex
            from tbenerequest
           where operator = i.operator and seqrequest = i.seqrequest and
                 SEQBENE = i.CODEAPPLI and rownum = 1; --mientras tanto le pongo asi ya que deberia con seq pero no se a que campo correponde en las tablas maestras
        EXCEPTION
          WHEN OTHERS THEN
            v_fechaCancelacion := null;
        END;

        -- dbms_output.put_line('localidad: '||localidad );
        -- Provisión requerida a Pesos Dominicanos
        provision_requerida := null; --pkg_legalreport3.cotiza_mon(p_fecha, provision_requerida, i.pre_moneda,p_sucursal);
        -- Insertamos datos en la tabla TVALUELEVELCUSTOM
        secuencial := secuencial + 1;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 1, secuencial, 1, secuencial); --NUMERO SECUENCIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 2, secuencial, 2, decode(v_tipoid,'P',REPLACE(identifica,'-',''),identifica)); --IDENTIFICACION DEL DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 3, secuencial, 3, tipocli); --TIPO DE DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 4, secuencial, 4, nombre); --NOMBRES / RAZON SOCIAL DEL DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 5, secuencial, 5, apellido); --APELLIDOS / SIGLAS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 6, secuencial, 6, i.pre_credito); --CODIGO DEL CREDITO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 7, secuencial, 7, v_numcomprom); --CODIGO DE LA FACILIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 8, secuencial, 8, v_fechaAprobacion); --FECHA DE APROBACION DEL CREDITO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 9, secuencial, 9, v_monto_aprobado); --MONTO APROBADO

        --VALUES (p_report,p_session,9,secuencial,9,i.m);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 10, secuencial, 10, v_fechaDesembolso); --FECHA DE DESEMBOLSO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 11, secuencial, 11, v_monto_aprobado); --MONTO DESEMBOLSADO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 12, secuencial, 12, i.pre_fecven); --FECHA DE VENCIMIENTO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 13, secuencial, 13, fecha_primer_pago); --FECHA INICIO DEL PRIMER PAGO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 14, secuencial, 14, valor_primer_pago); --MONTO DE LA CUOTA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 15, secuencial, 15, periodoCAP); --FORMA DE PAGO DEL CAPITAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 16, secuencial, 16, periodoINT); --FORMA DE PAGO DE INTERESES Y COMISIONES OJO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 17, secuencial, 17, P_GRACIA); --PERIODO DE GRACIA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 18, secuencial, 18, null); --TASA DE INTERES Y COMISION VIGENTE

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 19, secuencial, 19, moneda); --TIPO DE MONEDA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 20, secuencial, 20, existe); --COBRANZA JUDICIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 21, secuencial, 21, v_CALIFICACION_actual); --CLASIFICACION DEL CREDITO SEGUN LA ENTIDAD
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report,
           p_session,
           22,
           secuencial,
           22,
           v_CALIFICACION_expuesto); --CALIFICACION MONTO EXPUESTO --ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report,
           p_session,
           23,
           secuencial,
           23,
           v_CALIFICACION_cubierto); --CALIFICACION MONTO CUBIERTO  --ojo
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 24, secuencial, 24, provision_requerida); --PROVISION REQUERIDA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 25, secuencial, 25, tipo_recurso); --ORIGEN O TIPO DE RECURSO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 26, secuencial, 26, relacion_banco); --TIPO DE VINCULACION
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 27, secuencial, 27, garantia_adm); --GARANTIA ADMISIBLE
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 28, secuencial, 28, null); --FECHA DE REESTRUCTURACION
        -- Campos que se incrementaron para hd-4324
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 29, secuencial, 29, V_FECHA_RENOVA); --FECHA DE RENOVACION

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 30, secuencial, 30, localidad); --LOCALIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 31, secuencial, 31, destino_credito);--actividad_pciiu); --ACTIVIDAD PRINCIPAL DEL DEUDOR EN BASE AL CIUU

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 32, secuencial, 32, destino_credito); --DESTINO DEL CREDITO EN BASE AL CIUU

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 33, secuencial, 33, i.pre_sucursal); --NUMERO DE OFICINA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 34, secuencial, 34, v_califica_pais); --CLASIFICACION RIESGO PAIS  ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 35, secuencial, 35, v_codigo_pais); --CODIGO PAIS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 36, secuencial, 36, null); --FECHA INICIO PROCESO ADJUDICACION ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 37, secuencial, 37, v_IDdestinatario_comex); --IDENTIFICACION DESTINATARIO CONTINGENCIA ojo--HERE

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 38, secuencial, 38, v_destinatario_comex); --IDENTIFICACION DESTINATARIO CONTINGENCIA ojo--HERE
        IF i.status in ('C', 'P') then
          v_fechacancelacion := i.pre_fecance;
        else
          v_fechacancelacion := null;
        end if;
        --<<HD8164
        /*
        --balance
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 40, secuencial, 40, v_balance_fecha);
          */
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 39, secuencial, 39, 'S'); --OPCION DE PAGO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 40, secuencial, 40, null); --PENALIZACION POR PAGO
          --abril 2018
        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 41, secuencial, 41, null); */--PROVISION DE CAPITAL CONSTITUIDA POR EL CREDITO
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 42, secuencial, 42, null); --PROVISION REQUERIDA DE RENDIMIENTOS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 43, secuencial, 43, null); --PROVISION REQUERIDA DE CONTINGENCIAS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 44, secuencial, 44, NULL); --FECHA REVISION TASAS DE INTERES
           
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 45, secuencial, 45, NULL); --FECHA PAGO DE CUOTA EXTRAORDINARIA

     INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 46, secuencial, 46, null); --MONTO DE CUOTA EXTRAORDINARIA

       v_tipocliente:=fun_tipo_cliente(i.pre_clientep);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 47, secuencial, 47, v_tipocliente); --tipo de cliente

        v_facilidad:=fun_producto_servicio(i.pre_credito,6);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 48, secuencial, 48, v_facilidad); --FACILIDAD CREDITICIA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 49, secuencial, 49, null); --CANTIDAD DE PLASTICOS TC
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 50, secuencial, 50, null); --CODIGO SUBPRODUCTO TC
        -->>HD8164
        IF I.Montoaprobado > p_montocompara THEN
	        INSERT INTO tvaluelevelcustom
	          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
	        VALUES
	          (p_report, p_session, 51, secuencial, 51, 'C'); --TIPO DE CREDITO
        ELSE   
	        INSERT INTO tvaluelevelcustom
	          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
	        VALUES
	          (p_report, p_session, 51, secuencial, 51, 'M'); --TIPO DE CREDITO        
        END IF;
        --JUNIO 2015    
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 52, secuencial, 52, 'V'); --LCA_TIPOTASA
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 53, secuencial, 53, nULL); --NULL EN PRESTAMOS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 54, secuencial, 54, NULL); --NULL EN PRESTAMO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 55, secuencial, 55, NULL); --NULL EN PRESTAMO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 56, secuencial, 56,'NR'); --NULL EN PRESTAMO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 57, secuencial, 57, 'OP'); --NULL EN PRESTAMO                  
        --FIN JUNIO 2015
      --sysaid 15245 abril 2018
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 58, secuencial, 58, null); --fecha refinanciacio

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 59, secuencial, 59,null); --RAZON CLASIFICACION DE RIESGO DEL DEUDOR
          
      end loop;
      Commit;
      pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
    Else
      raise_application_error(-20806,
                              '!!! MES NO CONCLUIDO ¡¡¡, DEBE SELECCIONAR MESES CONCLUIDOS ' ||
                              sqlerrm);
    End if;
  END;

  ----------------------------------------------------------------------
  -- PROCEDIMIENTO PARA OBTENER CREDITOS A LA MICROEMPRESA POR DEUDOR --
  ----------------------------------------------------------------------
  -- PRCP 01/05/2002

  PROCEDURE de12(p_report       in varchar2,
                 p_session      in number,
                 p_sucursal     in number,
                 p_fecha        in date,
                 p_tipocredito1 in number,
                 p_tipocredito2 in number,
                 p_tipocredito3 in number,
                 p_montocompara in number,
                 p_decimales    in number) IS

    fecha_primer_pago     date;
    valor_primer_pago     tpre_cuotas.cuo_valor%type;
    existe                char(1);
    PERIODOint            char(2);
    periodocap            char(2);
    moneda                char(1);
    relacion_banco        char(8);
    calificacion_actual   tpre_califica.CAL_CALIFACTUAL%type;
    v_calificacion_actual varchar2(2);
    --layer new
    v_calificacion_expuesto varchar2(2);
    v_calificacion_cubierto varchar2(2);
    v_califica_pais         varchar2(2);
    V_FECHA_RENOVA          DATE;
    v_destinatario_comex    tcli_persona.cli_nombre%type;
    v_IDdestinatario_comex  tcli_persona.cli_identifica%type;
    v_codigo_pais           VARCHAR2(2);
    localidad               varchar2(8);--2012
    garantia_adm            number(18, 6);
    actividad_pciiu         number;
    destino_credito         number;
    w_diasper   NUMBER;
    --
    provision_requerida tpre_calificadet.CAD_PREVCONSTITCONTINMN%type;
    primera_cuota       number(18, 6);
    numero_cuotas       number(3);
    total               number(18, 6);

    -- Definicion de variables para identificar al deudor
    nombre             tcli_persona.cli_nombre%type;
    apellido           tcli_persona.cli_nombre%type;
    identifica         varchar2(20);
    tipocli            char(2);
    tipo_recurso       varchar2(2);
    secuencial         number(7) := 0;
    p_gracia           TPRE_PRESTAMOS.PRE_NUMPERGRACIA%type;
    v_balance_fecha    tpre_prestamos.pre_monto%type;
    v_monto_aprobado   tpre_prestamos.pre_monto%type;
    v_fechaAprobacion  DATE := NULL;
    v_fechaDesembolso  DATE := NULL;
    v_numcomprom       NUMBER;
    v_fechacancelacion DATE;
    vld_frevtasa           date;
    vld_fcuoextra          date;
    vln_monextra           number(15,2);
        provision_requerida_cap number:=0;
        provision_gradual_cap number:=0;
        provision_constituida_cap number:=0;
        provision_requerida_int number:=0;
        provision_gradual_int number:=0;
    w_tasa number;

    CURSOR CREDITOS_M IS
      select pre_diasper,
             pre_diaspercap,
             pre_credito,
             pre_numcomprom,
             pre_fecemi,
             round(pkg_legalreport3.cotiza_mon(p_fecha,
                                               pre_monto,
                                               pre_moneda,
                                               p_sucursal),
                   p_decimales) m,
             pre_fecontab,
             round(pkg_legalreport3.cotiza_mon(p_fecha,pre_valent,pre_moneda,p_sucursal),p_decimales) v, --Sra. Tania comenta
             --lca_capital v,--en bdi
             pre_fecven,
             pre_tasapac t,
             pre_moneda,
             pre_tipocuo,
             pre_clientep,
             pre_mod,
             pre_pro,
             pre_tip,
             PRE_NUMPERGRACIA,
             pre_sucursal,
             pre_oficina,
             pre_numdir,
             pre_fecance,
             LCA_FECREESTRUC pre_fecarr,
             lca_fecrenov,
             lca_demandajundicial,
             lca_montogaradmi
        from tpre_prestamos,tleg_operacioncalif
       where pre_mod = lca_mod
         and pre_credito = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'M'--menores deudores
UNION
      select 0 pre_diasper,
             0 pre_diaspercap,
             vis_numcue pre_credito,
             crd_compromaso pre_numcomprom,
             crd_fechavig pre_fecemi, --cambio realizado el 2005/01/07
             round(pkg_legalreport3.cotiza_mon(p_fecha,
                                               crd_valor,
                                               vis_moneda,
                                               p_sucursal),
                   p_decimales) m,
             crd_fechavig pre_fecontab,
             round(pkg_legalreport3.cotiza_mon(p_fecha,crd_valor,vis_moneda,p_sucursal),p_decimales) v, --se comenta por que deben tener el monto a la fecha de generación . Sra. Tania 28/05/2008
             --lca_capital v,--por confirmar con Johanna
             crd_fechavenc pre_fecven,
             1  t,
             vis_moneda pre_moneda,
             1 pre_tipocuo,
             vis_codcli pre_clientep,
             vis_mod pre_mod,
             vis_pro pre_pro,
             vis_tip pre_tip,
             0 PRE_NUMPERGRACIA,
             vis_suc pre_sucursal,
             vis_ofi pre_oficina,
             vis_numdircor pre_numdir,
             crd_fechavenc pre_fecance,
             LCA_FECREESTRUC pre_fecarr,
             lca_fecrenov,
             lca_demandajundicial,
             lca_montogaradmi
        from tcap_vista,--tcap_acract,
         tcap_credvista, tleg_operacioncalif
       where vis_mod = lca_mod
         and vis_numcue = lca_cuenta
         --and aca_numcue = vis_numcue
         --and aca_fecha = lca_fecha
         and vis_numcue = crd_numcue
         --and aca_tipocre = crd_tipocred
         --and aca_secuencia = crd_secuencia
         and crd_tipocred = 2
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'M'--mayores deudores
                       and  trunc(crd_fechautor) <= P_FECHA
					   and  crd_fechavenc >= P_FECHA
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
       order by v desc;
    CURSOR COMEX IS
      SELECT MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter * bdi_promedio(CYLETTER,branch,p_fecha) Montoaprobado,
             dateexpired pre_fecven,
             branch pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter) Balance,
             operator,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             NUll pre_fecance
        FROM tlettercredit
       WHERE --NVL(status, '!') = '!'
       decode(CYLETTER,
              0,
              valueletter,
              bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) <
       p_Montocompara AND
       seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
       codeletter not in
       (select codeletter
          FROM tlettercredit A, Tstageprod B, tstage C
         WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
               a.module = c.module AND a.prod = c.prod AND
               a.typeprod = c.typeprod AND a.seqamen = b.seq and
               c.stage = 'ST_CLOSE' and DATEPROCESS <= p_fecha + 0.99999)
      UNION
      SELECT MODULE,
             codeclient,
             codecollection,
             amountval * bdi_promedio(cycollection,branch,p_fecha) amountval,
             dateexpired,
             branch,
             office,
             pkg_coll_functs.balance_coll(codecollection) balance,
             operator,
             seqrequest,
             CYCOLLECTION,
             CODEDRAWEE,
             null pre_fecance
        FROM tcollection
       WHERE --NVL(status, '!') = '!'
       decode(CYCOLLECTION,
              0,
              amountval,
              bdi_promedio(CYCOLLECTION, branch, p_fecha) * amountval) <
       p_Montocompara AND
       1=0 AND
       codecollection not in
       (select codecollection
          FROM tcollection A, Tstageprod B, tstage C
         WHERE a.codecollection = b.code AND b.seqstage = c.seqstage AND
               a.module = c.module AND a.prod = c.prod AND
               a.typeprod = c.typeprod AND a.seqamen = b.seq and
               c.stage = 'ST_CLOSE' and DATEPROCESS <= p_fecha + 0.99999)
      UNION
      SELECT MODULE,
             codeclient,
             codedraft,
             valuedraft * bdi_promedio(cydraft,branch,p_fecha) valuedraft,
             dateexpired,
             branch,
             office,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codedraft) Balance,
             operator,
             seqrequest,
             CYDRAFT,
             CODEAPPLI,
             null pre_fecance
        FROM tdraft
       WHERE --NVL(status, '!') = '!'
       decode(CYDRAFT,
              0,
              valuedraft,
              bdi_promedio(CYDRAFT, branch, p_fecha) * valuedraft) <
       P_Montocompara AND
       codedraft not in
       (select codedraft
          FROM tdraft A, Tstageprod B, tstage C
         WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
               a.module = c.module AND a.prod = c.prod AND
               a.typeprod = c.typeprod AND
              --a.seqamen = b.seq and
               c.stage = 'ST_CLOSE' and DATEPROCESS <= p_fecha + 0.99999)
      UNION
      SELECT MODULE,
             codeclient,
             codeguarantee,
             valueguarantee * bdi_promedio(CYguarantee,branch,p_fecha),
             dateexpired,
             branch,
             office,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeguarantee) Balance,
             operator,
             seqrequest,
             CYGUARANTEE,
             CODEAPPLI,
             Null pre_fecance
        FROM tguarantee
       WHERE --NVL(status, '!') = '!'  AND
       decode(CYGUARANTEE,
              0,
              valueguarantee,
              bdi_promedio(CYGUARANTEE, branch, p_fecha) * valueguarantee) <
       p_Montocompara AND
       seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
       codeguarantee not in
       (select codeguarantee
          FROM tguarantee A, Tstageprod B, tstage C
         WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
               a.module = c.module AND a.prod = c.prod AND
               a.typeprod = c.typeprod AND a.seqamen = b.seq and
               c.stage = 'ST_CLOSE' and DATEPROCESS <= p_fecha + 0.99999);
  BEGIN

    If trunc(p_fecha) < trunc(sysdate) then
      dbms_output.put_line('en de12');
      pkg_legalreport.eraser(p_report, p_session);

      for i in CREDITOS_M loop
        dbms_output.put_line('dentro del loop');
        pkg_legalreport3.datos_cliente(i.pre_clientep,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido);
        dbms_output.put_line('nombre ' || nombre || ' ' || apellido);
        dbms_output.put_line('id ' || identifica);
        dbms_output.put_line('tipoper ' || tipocli);
        --     new
        V_FECHA_RENOVA          := NULL;
        v_calificacion_expuesto := null;
        v_calificacion_cubierto := null;
        v_califica_pais         := null;
        v_codigo_pais           := NULL;
        v_destinatario_comex    := null;
        v_IDdestinatario_comex  := null;
        --
        provision_requerida_cap:=0;
        provision_gradual_cap:=0;
        provision_constituida_cap:=0;
        provision_requerida_int:=0;
        provision_gradual_int:=0;

        v_calificacion_actual := null;
        v_balance_fecha       := 0;
        v_balance_fecha       := saldo_capital_fecha(i.pre_credito, p_fecha);
        IF i.pre_numcomprom IS NULL THEN
          v_monto_aprobado := pkg_legal_Dom_de.monto_aprobado(i.pre_credito,
                                                           p_fecha);
        ELSE
          v_monto_aprobado := pkg_legal_Dom_de.monto_aprobado(i.pre_numcomprom,
                                                           p_fecha);
        END IF;

        begin
          --new
          V_FECHA_RENOVA := i.lca_fecrenov;--PKG_LEGAL_DOM.FECHA_RENOVA(i.pre_credito,p_fecha); --PARA FECHA DE RENOVACION
          --TIPOS DE CUOTAS EXISTENTES (1,3,5,6,7,8) (2002/05/27)
          --AMORTIZACION GRADUAL (CUOTAS FIJAS DE CAPITAL + INTERESES)
          w_diasper:=30;
          If i.pre_tipocuo in (1,3,5,7) then

            BEGIN
              select cuo_fecha,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       cuo_valor,
                                                       i.pre_moneda,
                                                       p_sucursal),
                           p_decimales),cuo_dias
                into fecha_primer_pago, valor_primer_pago,w_diasper
                from tpre_cuotas
               where cuo_credito = i.pre_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                fecha_primer_pago := null;
                valor_primer_pago := null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;

            --CUOTA FIJA DE CAPITAL
          Elsif i.pre_tipocuo = 6 then
            BEGIN
              select let_fecven,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       let_valor,
                                                       i.pre_moneda,
                                                       p_sucursal),
                           p_decimales)
                into fecha_primer_pago, valor_primer_pago
                from tpre_letras
               where let_credito = i.pre_credito and let_sec = 1;
		    EXCEPTION
              when no_data_found then
                fecha_primer_pago := null;
                valor_primer_pago := null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            -- TABLA ESPECIAL (Pagos indistintos con por el cliente)
          Elsif i.pre_tipocuo = 8 then
            BEGIN
              Select count(*), sum(cuo_valor)--cuo_capital
                Into numero_cuotas, total
                From tpre_cuotas
               Where cuo_credito = i.pre_credito;

            EXCEPTION
              when no_data_found then
                numero_cuotas := 0;
                total         := 0;
              When others then
                raise_application_error(-20203,
                                        'Error al obtener el capital del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;

            BEGIN
              Select cuo_valor,cuo_dias --cuo_capital
                Into primera_cuota,w_diasper
                From tpre_cuotas
               Where cuo_credito = i.pre_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                primera_cuota := null;
              when others then
                raise_application_error(-20204,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            If numero_cuotas != 0 then
              If total / numero_cuotas = primera_cuota then

                BEGIN
                  Select cuo_fecha,
                         round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                           cuo_valor, --cuo_capital,
                                                           i.pre_moneda,
                                                           p_sucursal),
                               p_decimales)
                    Into fecha_primer_pago, valor_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = i.pre_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                    valor_primer_pago := null;
                  When others then
                    raise_application_error(-20205,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;

              Else
                BEGIN
                  Select cuo_fecha
                    Into fecha_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = i.pre_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                  when others then
                    raise_application_error(-20206,
                                            'Error al obtener fecha de la cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;
                BEGIN
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor, --cuo_capital,
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_fecha =
                         (Select max(cuo_fecha)
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito) and
                         cuo_credito = i.pre_credito
                         and rownum = 1;
                EXCEPTION
                  when no_data_found then
                                      Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = i.pre_credito;
                  when others then
                    raise_application_error(-20207,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;

              End if;
            Else
              fecha_primer_pago := null;
              valor_primer_pago := null;
            End if;

          Else
            fecha_primer_pago := null;
            valor_primer_pago := null;
          End if;

        End;

        PERIODOS_INT(i.pre_tipocuo, w_diasper, PERIODOINT);
        --PERIODOS(i.pre_tipocuo,i.pre_diaspercap,PERIODOCAP);
        PERIODOS_CAP(i.pre_tipocuo, i.pre_diasper, PERIODOCAP);
        PERGRACIA(i.pre_credito, p_fecha, p_gracia);
        existe:= i.lca_demandajundicial;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calificacion de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --calificacion inicial-final
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          v_calificacion_actual:=calificacion(P_FECHA,'P',i.pre_credito,14);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_actual := null;
        End;
      --calificacion expuesta
        Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          v_calificacion_expuesto:=calificacion(P_FECHA,'P',i.pre_credito,13);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_expuesto := null;
        End;

      --calificacion cubierta
        Begin
          v_calificacion_cubierto:=calificacion(P_FECHA,'P',i.pre_credito,12);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_cubierto := null;
        End;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fin de calificacion
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- provision de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --calificacion inicial-final
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_requerida_cap:= provision(P_FECHA,'P',i.pre_credito,9);
        Exception
          when no_data_found then
			provision_requerida_cap := null;
        End;
      --gradual cap
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_gradual_cap:= provision(P_FECHA,'P',i.pre_credito,11);
        Exception
          when no_data_found then
			provision_gradual_cap := null;
        End;

      --calificacion cubierta
      --gradual constituida
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_constituida_cap:= provision(P_FECHA,'P',i.pre_credito,12);
        Exception
          when no_data_found then
			provision_constituida_cap := null;
        End;
      --requerida int
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_requerida_int:= provision(P_FECHA,'P',i.pre_credito,8);
        Exception
          when no_data_found then
			provision_requerida_int := null;
        End;
      --gradual int
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_gradual_int:= provision(P_FECHA,'P',i.pre_credito,10);
        Exception
          when no_data_found then
			provision_gradual_int := null;
        End;


  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fin de provision
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


        tipo_recurso := RECURSO(i.pre_mod, i.pre_credito);

        begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabrel AND DES_CODIGO = cli_relacionban and
                 cli_codigo = i.pre_clientep;
        EXCEPTION
          when no_data_found then
            relacion_banco := null;
          when others then
            raise_application_error(-20209,
                                    'Error relación con el banco del cliente ' ||
                                    i.pre_clientep || ' ' || sqlerrm);
        end;

        if i.pre_moneda = 0 then
          moneda := 'N';
        else
          moneda := 'E';
        end if;
        -- Actividad principal del deudor en base al CIIU  (HD4324_2)
        BEGIN
          Select substr(CII_CODIGO, 1, 2)
            Into actividad_pciiu
            From tcli_natural, TPRE_PRESTAMOS, tgen_ciiu
           Where (PRE_CREDITO = i.pre_credito and pre_clientep = nat_codcli and
                 nat_actprin = cii_tipoact);
        Exception
          When no_data_found then
            actividad_pciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Natural' ||
                                    sqlerrm);
        END;
        BEGIN
          Select substr(CII_CODIGO, 1, 2) --HD4324_2
            Into actividad_pciiu
            From tcli_JURIDICA, TPRE_PRESTAMOS, tgen_ciiu
           Where (PRE_CREDITO = i.pre_credito and pre_clientep = JUR_codcli and
                 JUR_actividad = cii_tipoact);
        Exception
          When no_data_found then
            actividad_pciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Jurídico' ||
                                    sqlerrm);
        END;

        -- Destino del crédito en base al CIIU
        BEGIN
          select substr(DES_CODIGOCIIU, 1, 6)
            Into destino_credito
            from tpre_prestamos, tpre_destecon
           where pre_destecon = des_codigo and pre_credito = i.pre_credito;
        Exception
          When no_data_found then
            destino_credito := null;
          when others then
            raise_application_error(-20213,
                                    'Error al obtener Destino Económico en base al CIIU del Deudor ' ||
                                    sqlerrm);
        END;
        --Localidad new

        BEGIN
          /*select substr(codeiso, 1, 4)
            Into localidad
            from tgen_desctabla, tpre_prestamos
           where pre_tabciudad = des_codtab and pre_ciudes = des_codigo and
                 pre_credito = i.pre_credito;*/
           select substr(a.codeiso, 1, 6)
            Into localidad
            from tgen_desctabla a,tgen_oficina b
           where ofi_tabciu = des_codtab 
             and ofi_codciu = des_codigo
             and ofi_codsuc = i.pre_sucursal
             and ofi_codofi = i.pre_oficina;                 
                 
        Exception
          When no_data_found then
            localidad := null;
          when others then
            raise_application_error(-20214,
                                    'Error al al obtener la Localidad: ' ||
                                    i.pre_credito || '-' || sqlerrm);
        END;
        -- Garantía Admisible
        --garantia_adm:= i.LCA_MONTOGARADMI;
         w_valorparam:= pkg_precalifica_do.parametro_calif('VALOR_GARANTIZADO_DE');
        IF NVL(w_valorparam,'R') = 'R' THEN
         select sum(nvl(log_valorgarantizadoreal,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = i.pre_credito;
        ELSE
         select sum(nvl(log_valorgarantizadofisa,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = i.pre_credito;
        END IF ;

        -- Provisiön Requerida a pesos dominicanos


        --<<HD8164
        pro_gen_cuoextra(i.pre_credito,p_fecha,vln_monextra,vld_fcuoextra);
        vln_monextra:=vln_monextra*bdi_promedio(i.pre_moneda,1,p_fecha);
        vld_frevtasa:=fun_gen_revtasa(i.pre_credito,p_fecha);
        -->>
        -- Insertamos datos en la tabla TVALUELEVELCUSTOM

        secuencial := secuencial + 1;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 1, secuencial, 1, secuencial); --NUMERO SECUENCIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 2, secuencial, 2, identifica); --IDENTIFICACION DEL DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 3, secuencial, 3, tipocli); --TIPO DE DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 4, secuencial, 4, nombre); --NOMBRES / RAZON SOCIAL DEL DEUDOR
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 5, secuencial, 5, apellido); --APELLIDOS / SIGLAS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 6, secuencial, 6, i.pre_credito); --CODIGO DEL CREDITO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 7, secuencial, 7, i.pre_numcomprom); --CODIGO DE LA FACILIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 8, secuencial, 8, i.pre_fecemi); --FECHA DE APROBACION DEL CREDITO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 9, secuencial, 9, v_monto_aprobado); --MONTO APROBADO
        --VALUES (p_report,p_session,9,secuencial,9,i.m);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 10, secuencial, 10, i.pre_fecontab); --FECHA DE DESEMBOLSO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 11, secuencial, 11, i.v); --MONTO DESEMBOLSADO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 12, secuencial, 12, i.pre_fecven); --FECHA DE VENCIMIENTO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 13, secuencial, 13, fecha_primer_pago); --FECHA INICIO DEL PRIMER PAGO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 14, secuencial, 14, valor_primer_pago); --MONTO DE LA CUOTA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 15, secuencial, 15, periodoCAP); --FORMA DE PAGO DEL CAPITAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 16, secuencial, 16, periodoINT); --FORMA DE PAGO DE INTERESES Y COMISIONES

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 17, secuencial, 17, p_gracia); --PERIODO DE GRACIA

		    begin
		     select aca_tasaint
		       into w_tasa
		      from tcap_acract
		       where aca_numcue = i.pre_credito
		         and aca_fecha = p_fecha
		         and aca_tipocre = 2;
		    exception
		    	  when others then
		    	    w_tasa:=i.t;
		    end;

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 18, secuencial, 18, w_tasa); --TASA DE INTERES Y COMISION VIGENTE

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 19, secuencial, 19, moneda); --TIPO DE MONEDA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 20, secuencial, 20, existe); --COBRANZA JUDICIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 21, secuencial, 21, v_CALIFICACION_actual); --CLASIFICACION DEL CREDITO SEGUN LA ENTIDAD
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report,
           p_session,
           22,
           secuencial,
           22,
           v_CALIFICACION_expuesto); --CALIFICACION MONTO EXPUESTO --ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report,
           p_session,
           23,
           secuencial,
           23,
           v_CALIFICACION_cubierto); --CALIFICACION MONTO CUBIERTO  --ojo
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 24, secuencial, 24, provision_requerida_cap); --PROVISION REQUERIDA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 25, secuencial, 25, tipo_recurso); --ORIGEN O TIPO DE RECURSO
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 26, secuencial, 26, relacion_banco); --TIPO DE VINCULACION
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 27, secuencial, 27, garantia_adm); --GARANTIA ADMISIBLE
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 28, secuencial, 28, i.pre_fecarr); --FECHA DE REESTRUCTURACION
        -- Campos que se incrementaron para hd-4324
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 29, secuencial, 29, V_FECHA_RENOVA); --FECHA DE RENOVACION

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 30, secuencial, 30, localidad); --LOCALIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 31, secuencial, 31, substr(destino_credito,1,6)); --ACTIVIDAD PRINCIPAL DEL DEUDOR EN BASE AL CIUU

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 32, secuencial, 32, destino_credito); --DESTINO DEL CREDITO EN BASE AL CIUU

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 33, secuencial, 33, i.pre_sucursal); --NUMERO DE OFICINA

        --INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUENUM)
        --VALUES (p_report,p_session,34,secuencial,34,v_califica_pais); --CLASIFICACION RIESGO PAIS  ojo

        --INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUENUM)
        --VALUES (p_report,p_session,35,secuencial,35,v_codigo_pais); --CODIGO PAIS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 34, secuencial, 34, null); --FECHA INICIO PROCESO ADJUDICACION ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 35, secuencial, 35, v_IDdestinatario_comex); --IDENTIFICACION DESTINATARIO CONTINGENCIA ojo--HERE

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 36, secuencial, 36, v_destinatario_comex); --IDENTIFICACION DESTINATARIO CONTINGENCIA ojo--HERE

        --INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)
        --VALUES (p_report,p_session,39,secuencial,39,i.pre_fecance); --FECHA CANCELACION       ojo
        --
        --<<HD8164
        /*
        --balance
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 37, secuencial, 37, v_balance_fecha);
        */
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 37, secuencial, 37, 'S'); --OPCION DE PAGO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 38, secuencial, 38, 0.00); --PENALIZACION POR PAGO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 39, secuencial, 39, provision_gradual_cap); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE CAPITAL
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 40, secuencial, 40, provision_constituida_cap); --PROVISION DE CAPITAL CONSTITUIDA POR EL CREDITO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 41, secuencial, 41, provision_requerida_int); --PROVISION REQUERIDA DE RENDIMIENTOS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 42, secuencial, 42, provision_gradual_int); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE RENDIMIENTOS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 43, secuencial, 43, null); --PROVISION REQUERIDA DE CONTINGENCIAS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 44, secuencial, 44, null); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE CONTINGENCIAS
       if trunc(i.pre_fecance) <= p_fecha then
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 45, secuencial, 45, null); --FECHA REVISION TASA DE INTERES
        else
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 45, secuencial, 45, vld_frevtasa); --FECHA REVISION TASA DE INTERES
        end if;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 46, secuencial, 46, vld_fcuoextra); --FECHA PAGO DE CUOTA EXTRAORDINARIA
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 47, secuencial, 47, vln_monextra); --MONTO DE CUOTA EXTRAORDINARIA
        -->>HD8164
      End loop; -- Cursor CREDITOS_M
      /*-------------------------------------------------COMEX-----------------------------------------------*/
      For i in COMEX loop
        dbms_output.put_line('dentro del loop');
        pkg_legalreport3.datos_cliente(i.pre_clientep,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido);
        dbms_output.put_line('nombre ' || nombre || ' ' || apellido);
        dbms_output.put_line('id ' || identifica);
        dbms_output.put_line('tipoper ' || tipocli);
        v_calificacion_actual := null;
        --
        V_FECHA_RENOVA          := NULL;
        v_calificacion_expuesto := null;
        v_calificacion_cubierto := null;
        v_califica_pais         := null;
        v_codigo_pais           := NULL;
        v_destinatario_comex    := null;
        v_IDdestinatario_comex  := null;
        --
        v_balance_fecha  := 0;
        v_balance_fecha  := i.balance;
        v_monto_aprobado := 0;
        v_monto_aprobado := i.montoaprobado;
        Begin
          V_FECHA_RENOVA := null; --PKG_LEGAL_DOM.FECHA_RENOVA(i.pre_credito,p_fecha);--PARA FECHA DE RENOVACION
          --TIPOS DE CUOTAS EXISTENTES (1,3,5,6,7,8) (2002/05/27)
          --AMORTIZACION GRADUAL (CUOTAS FIJAS DE CAPITAL + INTERESES)
        End;
        Begin
          /*select 'S' into existe
          from  tpre_presleg
          where tpre_presleg.prl_credito = i.pre_credito
            and p_fecha between prl_desde and nvl(prl_hasta,to_date('2199/12/31','YYYY/MM/DD'));

          EXCEPTION
            when no_data_found then
            existe := 'N';
            when others then
              raise_application_error(-20208,'Error al obtener el crédito legal '||sqlerrm);*/
          existe := null;
        End;
        Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          /*select round(nvl(cad_prevconstitdir,0),p_decimales), cad_califcuenta
            into provision_requerida, calificacion_actual
            from TPRE_CALIFICAPROVDET
            where cad_fecha in (select max(cad_fecha) from TPRE_CALIFICAPROVDET
                               where cad_fecha <= p_fecha
                                 and cad_tipo = 'T'
                                 and cad_cuenta = i.pre_credito)
            and cad_tipo = 'T'
            and cad_cuenta = i.pre_credito;
            --Para desplegar la letra que correponde
            select substr(des_descripcion,1,2)
              into v_calificacion_actual
              from tgen_desctabla
             where des_codtab = 245
               and des_codigo = calificacion_actual;
            --fin
          Exception
            when no_data_found then
                provision_requerida := 0;
                calificacion_actual:=null;
               v_calificacion_actual:=null;   */
          provision_requerida := null;
          calificacion_actual := null;

        End;

        tipo_recurso := null; --RECURSO(i.pre_mod,i.pre_credito);

        Begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabrel AND DES_CODIGO = cli_relacionban and
                 cli_codigo = i.pre_clientep;
        EXCEPTION
          when no_data_found then
            relacion_banco := null;
          when others then
            raise_application_error(-20209,
                                    'Error relación con el banco del cliente ' ||
                                    i.pre_clientep || ' ' || sqlerrm);
        End;

        If i.pre_moneda = 0 then
          moneda := 'N';
        Else
          moneda := 'E';
        End if;

        ---------------------------------------
        ---- Cambios para resolver hd_4324 ----
        ---------------------------------------
        -- Garantía Admisible
        BEGIN
          --Se cambia a valorcomercial por que el cliente indica que es el valor admisible y no el valor garantiza
          --novedad reportada por yomalin troncoso usuario de garantias con el e-mail del dia 2004/03/22 8:49 de mañana
          --se comenta este caso tambien a Francis Collado Help Desk
          --select sum(ogr_valor)
          select sum(gar_valorcomercial)
            into garantia_adm
            from tgar_garantias, tcli_opergaran
           where ogr_cliente = gar_codcli and ogr_numgaran = gar_numero and
                 ogr_mod = i.pre_mod and ogr_operacion = i.pre_credito and
                 ogr_fecdesde is not null and ogr_fechasta is null;
        Exception
          When no_data_found then
            garantia_adm := null;
          when others then
            raise_application_error(-20210,
                                    'Error al obtener Garantía Admisible ' ||
                                    sqlerrm);
        END;

        -- Fecha de Reestructuración queda pendiente, actualmente el Banco no maneja irá como nula

        -- Fecha de Renovación
        /* BEGIN
        select pre_fecarr
        Into fecha_renova
        from tpre_prestamos
        where pre_credito = i.pre_credito;
        Exception
           When no_data_found then
                fecha_renova := null;
           when others then
                raise_application_error(-20211,'Error al obtener Fecha de Renovación '||sqlerrm);
        END;*/

        -- Actividad principal del deudor en base al CIIU  (HD4324_2)
        BEGIN
          Select substr(CII_CODIGO, 1, 2)
            Into actividad_pciiu
            From tcli_natural, tgen_ciiu
           Where nat_codcli = i.pre_clientep and nat_actprin = cii_tipoact;
        Exception
          When no_data_found then
            actividad_pciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Natural' ||
                                    sqlerrm);
        END;
        BEGIN
          Select substr(CII_CODIGO, 1, 2) --HD4324_2
            Into actividad_pciiu
            From tcli_JURIDICA, tgen_ciiu
           Where JUR_codcli = i.pre_clientep and
                 JUR_actividad = cii_tipoact;
        Exception
          When no_data_found then
            actividad_pciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Jurídico' ||
                                    sqlerrm);
        END;

        -- Destino del crédito en base al CIIU
        /*  BEGIN
          select substr(pre_destecon,1,4)
          Into destino_credito
          from tpre_prestamos,tpre_destecon
          where pre_destecon = des_codigo
            and pre_credito  =  i.pre_credito;
          Exception
          When no_data_found then
                destino_credito := null;
          when others then
                raise_application_error(-20213,'Error al obtener Destino Económico en base al CIIU del Deudor '||sqlerrm);
        END;*/
        --Localidad
        BEGIN
           select substr(a.codeiso, 1, 6)
            Into localidad
            from tgen_desctabla a,tgen_oficina b
           where ofi_tabciu = des_codtab 
             and ofi_codciu = des_codigo
             and ofi_codsuc = i.pre_sucursal
             and ofi_codofi = i.pre_oficina;                 
        
          /*select substr(codeiso, 1, 4)
            Into localidad
            from tgen_desctabla, tbankcorraso
           where codecitytable = des_codtab and codecity = des_codigo and
                 codeletter = i.pre_credito and coderol = 'RL_CONF' and
                 seqamen = pkg_discrep.ultima_lettercredit(codeletter) and
                 seqneg = (select max(seqneg)
                             from tbankcorraso
                            where codeletter = i.pre_credito);*/
        Exception
          When no_data_found then
            BEGIN
              select substr(codeiso, 1, 6)
                Into localidad
                from tgen_desctabla, tbankcoll
               where codecitytable = des_codtab and codecity = des_codigo and
                     codecollection = i.pre_credito and coderol = 'RL_PAY';
            Exception
              When no_data_found then
                BEGIN
                  select substr(codeiso, 1, 6)
                    Into localidad
                    from tgen_desctabla, tbankcorrdraft
                   where codecitytable = des_codtab and
                         codecity = des_codigo and
                         codedraft = i.pre_credito and
                         coderol = 'RL_CORRESP';
                Exception
                  When no_data_found then
                    BEGIN
                      select substr(codeiso, 1, 6)
                        Into localidad
                        from tgen_desctabla, tbankguarantee
                       where codecitytable = des_codtab and
                             codecity = des_codigo and
                             codeguarantee = i.pre_credito and
                             coderol = 'RL_CONF' and
                             seqamen =
                             PKG_GUAR_FUNCTS.last_seqamen(codeguarantee);
                    Exception
                      When no_data_found then
                        localidad := NULL;
                      when others then
                        raise_application_error(-20214,
                                                '3 Error al al obtener la Localidad ' ||
                                                i.pre_credito || '-' ||
                                                sqlerrm);
                    END;
                  when others then
                    raise_application_error(-20214,
                                            '2 Error al al obtener la Localidad ' ||
                                            i.pre_credito || '-' || sqlerrm);
                END;
              when others then
                raise_application_error(-20214,
                                        '1 Error al al obtener la Localidad ' ||
                                        i.pre_credito || '-' || sqlerrm);
            END;
          when others then
            raise_application_error(-20214,
                                    'Error al al obtener la Localidad ' ||
                                    i.pre_credito || '-' || sqlerrm);
        END;
        --PAIS
        --v_codigo_pais := PKG_LEGAL_DOM.CODIGO_PAIS(i.pre_clientep,i.pre_numdir);
        BEGIN
          select substr(codeiso, 1, 4)
            Into v_codigo_pais
            from tgen_desctabla, tbankcorraso
           where CODECOUNTRYTABLE = des_codtab and CODECOUNTRY = des_codigo and
                 codeletter = i.pre_credito and coderol = 'RL_CONF' and
                 seqneg = (select max(seqneg)
                             from tbankcorraso
                            where codeletter = i.pre_credito) and
                 seqamen = pkg_discrep.ultima_lettercredit(codeletter);
        Exception
          When no_data_found then
            BEGIN
              select substr(codeiso, 1, 4)
                Into v_codigo_pais
                from tgen_desctabla, tbankcoll
               where CODECOUNTRYTABLE = des_codtab and
                     CODECOUNTRY = des_codigo and
                     codecollection = i.pre_credito and coderol = 'RL_PAY';
            Exception
              When no_data_found then
                BEGIN
                  select substr(codeiso, 1, 4)
                    Into v_codigo_pais
                    from tgen_desctabla, tbankcorrdraft
                   where CODECOUNTRYTABLE = des_codtab and
                         CODECOUNTRY = des_codigo and
                         codedraft = i.pre_credito and
                         coderol = 'RL_CORRESP';
                Exception
                  When no_data_found then
                    BEGIN
                      select substr(codeiso, 1, 4)
                        Into v_codigo_pais
                        from tgen_desctabla, tbankguarantee
                       where CODECOUNTRYTABLE = des_codtab and
                             CODECOUNTRY = des_codigo and
                             codeguarantee = i.pre_credito --here
                             and coderol = 'RL_CONF' and
                             seqamen =
                             PKG_GUAR_FUNCTS.last_seqamen(codeguarantee);
                    Exception
                      When no_data_found then
                        localidad := NULL;
                      when others then
                        raise_application_error(-20214,
                                                '3 Error al al obtener la Localidad ' ||
                                                sqlerrm);
                    END;
                  when others then
                    raise_application_error(-20214,
                                            '2 Error al al obtener la Localidad ' ||
                                            sqlerrm);
                END;
              when others then
                raise_application_error(-20214,
                                        '1 Error al al obtener la Localidad ' ||
                                        sqlerrm);
            END;
          when others then
            raise_application_error(-20214,
                                    'Error al al obtener la Localidad ' ||
                                    sqlerrm);
        END;
        --fecha de aprobacion del credito
        BEGIN
          select sol_fecha
            into v_fechaAprobacion
            from tsol_solicitud
           where sol_operador = i.operator and sol_numero = i.seqrequest;
        EXCEPTION
          WHEN OTHERS THEN
            v_fechaAprobacion := null;
        END;
        --fecha de desembolso
        BEGIN
          select DATEPROCESS
            into v_fechaDesembolso
            from tstageprod A, TSTAGE B
           where code = i.pre_credito and A.seqstage = B.SEQSTAGE and
                 stage = 'S_OPEN';
        EXCEPTION
          WHEN OTHERS THEN
            v_fechaDesembolso := null;
        END;
        --fin
        --codigo de facilidad
        --destino del credito
        BEGIN      
           destino_credito:=fun_destino_ciiu(0,i.pre_credito,v_numcomprom);
          /*select DES_CODIGOCIIU, CODELINE
            into destino_credito, v_numcomprom
            from tclientline, tpre_destecon
           where ECONOMICDESTINITY = DES_CODIGO(+) and
                 CODEOPERATION = i.pre_credito;*/
        EXCEPTION
          WHEN OTHERS THEN
            destino_credito := null;
            v_numcomprom    := null;
        END;

        --FECHA DE CANCELACION
        BEGIN
          select DATEPROCESS
            into v_fechacancelacion
            from tstageprod A, TSTAGE B
           where code = i.pre_credito and A.seqstage = B.SEQSTAGE and
                 stage = 'ST_CLOSE';
        EXCEPTION
          WHEN OTHERS THEN
            v_fechaCancelacion := null;
        END;
        --BENEFICIARIO
        BEGIN
          select bene
            into v_destinatario_comex
            from tbenerequest
           where operator = i.operator and seqrequest = i.seqrequest and
                 SEQBENE = i.CODEAPPLI and rownum = 1; --mientras tanto le pongo asi ya que deberia con seq pero no se a que campo correponde en las tablas maestras
        EXCEPTION
          WHEN OTHERS THEN
            v_fechaCancelacion := null;
        END;

        -- dbms_output.put_line('localidad: '||localidad );
        -- Provisión requerida a Pesos Dominicanos
        provision_requerida := null; --pkg_legalreport3.cotiza_mon(p_fecha, provision_requerida, i.pre_moneda,p_sucursal);
        -- Insertamos datos en la tabla TVALUELEVELCUSTOM
        secuencial := secuencial + 1;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 1, secuencial, 1, secuencial); --NUMERO SECUENCIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 2, secuencial, 2, identifica); --IDENTIFICACION DEL DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 3, secuencial, 3, tipocli); --TIPO DE DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 4, secuencial, 4, nombre); --NOMBRES / RAZON SOCIAL DEL DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 5, secuencial, 5, apellido); --APELLIDOS / SIGLAS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 6, secuencial, 6, i.pre_credito); --CODIGO DEL CREDITO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 7, secuencial, 7, v_numcomprom); --CODIGO DE LA FACILIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 8, secuencial, 8, v_fechaAprobacion); --FECHA DE APROBACION DEL CREDITO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 9, secuencial, 9, v_monto_aprobado); --MONTO APROBADO

        --VALUES (p_report,p_session,9,secuencial,9,i.m);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 10, secuencial, 10, v_fechaDesembolso); --FECHA DE DESEMBOLSO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 11, secuencial, 11, v_monto_aprobado); --MONTO DESEMBOLSADO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 12, secuencial, 12, i.pre_fecven); --FECHA DE VENCIMIENTO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 13, secuencial, 13, fecha_primer_pago); --FECHA INICIO DEL PRIMER PAGO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 14, secuencial, 14, valor_primer_pago); --MONTO DE LA CUOTA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 15, secuencial, 15, periodoCAP); --FORMA DE PAGO DEL CAPITAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 16, secuencial, 16, periodoINT); --FORMA DE PAGO DE INTERESES Y COMISIONES OJO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 17, secuencial, 17, P_GRACIA); --PERIODO DE GRACIA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 18, secuencial, 18, null); --TASA DE INTERES Y COMISION VIGENTE

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 19, secuencial, 19, moneda); --TIPO DE MONEDA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 20, secuencial, 20, existe); --COBRANZA JUDICIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 21, secuencial, 21, v_CALIFICACION_actual); --CLASIFICACION DEL CREDITO SEGUN LA ENTIDAD
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report,
           p_session,
           22,
           secuencial,
           22,
           v_CALIFICACION_expuesto); --CALIFICACION MONTO EXPUESTO --ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report,
           p_session,
           23,
           secuencial,
           23,
           v_CALIFICACION_cubierto); --CALIFICACION MONTO CUBIERTO  --ojo
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 24, secuencial, 24, provision_requerida); --PROVISION REQUERIDA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 25, secuencial, 25, tipo_recurso); --ORIGEN O TIPO DE RECURSO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 26, secuencial, 26, relacion_banco); --TIPO DE VINCULACION
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 27, secuencial, 27, garantia_adm); --GARANTIA ADMISIBLE
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 28, secuencial, 28, null); --FECHA DE REESTRUCTURACION
        -- Campos que se incrementaron para hd-4324
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 29, secuencial, 29, V_FECHA_RENOVA); --FECHA DE RENOVACION

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 30, secuencial, 30, localidad); --LOCALIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 31, secuencial, 31, SUBSTR(destino_credito,1,6)); --ACTIVIDAD PRINCIPAL DEL DEUDOR EN BASE AL CIUU

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 32, secuencial, 32, destino_credito); --DESTINO DEL CREDITO EN BASE AL CIUU

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 33, secuencial, 33, i.pre_sucursal); --NUMERO DE OFICINA

        --INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUENUM)
        --VALUES (p_report,p_session,34,secuencial,34,v_califica_pais); --CLASIFICACION RIESGO PAIS  ojo

        --INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUENUM)
        --VALUES (p_report,p_session,35,secuencial,35,v_codigo_pais); --CODIGO PAIS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 34, secuencial, 34, null); --FECHA INICIO PROCESO ADJUDICACION ojo

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 35, secuencial, 35, v_IDdestinatario_comex); --IDENTIFICACION DESTINATARIO CONTINGENCIA ojo--HERE

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 36, secuencial, 36, v_destinatario_comex); --IDENTIFICACION DESTINATARIO CONTINGENCIA ojo--HERE
        --<<HD8164
        /*
        --balance
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 37, secuencial, 37, v_balance_fecha);
        */
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 37, secuencial, 37, 'S'); --OPCION DE PAGO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 38, secuencial, 38, null); --PENALIZACION POR PAGO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 39, secuencial, 39, null); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE CAPITAL
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 40, secuencial, 40, null); --PROVISION DE CAPITAL CONSTITUIDA POR EL CREDITO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 41, secuencial, 41, null); --PROVISION REQUERIDA DE RENDIMIENTOS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 42, secuencial, 42, null); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE RENDIMIENTOS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 43, secuencial, 43, null); --PROVISION REQUERIDA DE CONTINGENCIAS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 44, secuencial, 44, null); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE CONTINGENCIAS
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 45, secuencial, 45, null); --FECHA REVISION TASA DE INTERES
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 46, secuencial, 46, null); --FECHA PAGO DE CUOTA EXTRAORDINARIA
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 47, secuencial, 47, null); --MONTO DE CUOTA EXTRAORDINARIA
        -->>HD8164
      end loop;

      Commit;
      pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
    Else
      raise_application_error(-20806,
                              '!!! MES NO CONCLUIDO ¡¡¡, DEBE SELECCIONAR MESES CONCLUIDOS ' ||
                              sqlerrm);
    End if;
  END;

  -----------------------------------------------------------------------------
  -- PROCEDIMIENTO PARA OBTENER CREDITOS DE CONSUMO DE LA ENTIDAD POR DEUDOR --
  -----------------------------------------------------------------------------
  -- PRCP 10/05/2002

  PROCEDURE de13(p_report       in varchar2,
                 p_session      in number,
                 p_sucursal     in number,
                 p_fecha        in date,
                 p_tipocredito1 in number,
                 p_tipocredito2 in number,
                 p_tipocredito3 in number,
                 p_decimales    in number) IS

    fecha_primer_pago     date;
    valor_primer_pago     tpre_cuotas.cuo_valor%type;
    existe                char(1);
    moneda                char(1);
    relacion_banco        char(8);
    calificacion_actual   tpre_califica.CAL_CALIFACTUAL%type;
    v_calificacion_actual varchar2(2);
    provision_requerida   tpre_calificadet.CAD_PREVCONSTITCONTINMN%type;
    primera_cuota         number(18, 6);
    numero_cuotas         number(3);
    total                 number(18, 6);

    -- Definicion de variables para identificar al deudor
    nombre           tcli_persona.cli_nombre%type;
    apellido         tcli_persona.cli_nombre%type;
    identifica       varchar2(20);
    tipocli          char(2);
    secuencial       number(7) := 0;
    salida           varchar(4);
    v_balance_fecha  tpre_prestamos.pre_monto%type;
    v_monto_aprobado tpre_prestamos.pre_monto%type;
    --new
    PERIODOint     varchar2(2);
    periodocap     varchar2(2);
    V_FECHA_RENOVA DATE;
    localidad      VARCHAR2(8);--2012
    vld_frevtasa           date;
    vld_fcuoextra          date;
    vln_monextra           number(15,2);
    vld_faprob             date;
    vld_montaprob          number(15,2);
    w_diasper			   number;
   --15648
    v_tipocliente varchar2(3);
    v_facilidad  varchar2(3);

    --
        provision_requerida_cap number:=0;
        provision_gradual_cap number:=0;
        provision_constituida_cap number:=0;
        provision_requerida_int number:=0;
        provision_gradual_int number:=0;
        w_restructurado varchar2(2);
    CURSOR CREDITOS_C IS

      select pre_credito,
             pre_fecontab,
             round(pkg_legalreport3.cotiza_mon(p_fecha,
                                               pre_valent,
                                               pre_moneda,
                                               p_sucursal),
                   p_decimales) v,
             pre_fecven,
             pre_tasapac t,
             pre_moneda,
             pre_tipocuo,
             pre_clientep,
             pre_mod,
             pre_pro,
             pre_tip,
             ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = pre_sucursal) pre_sucursal,
             pre_oficina,
             pre_numdir,
             pre_fecance,
             LCA_FECREESTRUC pre_fecarr,
             pre_diasper,
             pre_numcomprom,
             lca_fecrenov,
             lca_fecrefin,
             lca_demandajundicial,
             lca_montogaradmi,
             pre_fecemi,
             pre_valent,
             LCA_REESTRUCTURADO,
				LCA_MONTOCUOTAEXTRAOR,  
				LCA_TIPOTASA         ,  
				LCA_BALPROMDCAPMES   ,  
				LCA_INTDEVENCORTE    ,  
				LCA_COMCARGODEVCORTE ,  
				LCA_ESTRUCTURACIONCRE,  
				LCA_ORIGENCREDITO    ,  
				LCA_TIPORECURSO ,
				pre_extrefer                    
        from tpre_prestamos,tleg_operacioncalif
       where pre_mod = lca_mod
         and pre_credito = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'O'--
       order by v desc;
  v_tipoid varchar2(1);
  v_incrementos NUMBER;
  V_JUDDESDE DATE; --SYSAID 15245 ABRIL 2018
  p_gracia NUMBER; --SYSAID 15245 ABRIL 2018
  garantia_adm NUMBER; --SYSAID 15245 ABRIL 2018
  v_calificacion_expuesto varchar2(2);--SYSAID 15245 ABRIL 2018
  v_calificacion_cubierto varchar2(2);--SYSAID 15245 ABRIL 2018
  
  BEGIN

    If trunc(p_fecha) < trunc(sysdate) then
      dbms_output.put_line('en de13');
      pkg_legalreport.eraser(p_report, p_session);
      select TYPEOUT into salida from treportlegal where code = p_report;

      for i in CREDITOS_C loop
        provision_requerida_cap:=0;
        provision_gradual_cap:=0;
        provision_constituida_cap:=0;
        provision_requerida_int:=0;
        provision_gradual_int:=0;
             begin
          select cli_tipoid 
          into v_tipoid
           from tcli_persona
           where cli_codigo = i.pre_clientep;
        exception 
           when others then
              v_tipoid:=null;
        end;

        dbms_output.put_line('dentro del loop');
        pkg_legalreport3.datos_cliente(i.pre_clientep,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido);
        dbms_output.put_line('nombre ' || nombre || ' ' || apellido);
        dbms_output.put_line('id ' || identifica);
        dbms_output.put_line('tipoper ' || tipocli);
        v_calificacion_actual := null;
        v_balance_fecha       := 0;
        v_balance_fecha       := saldo_capital_fecha(i.pre_credito, p_fecha);
        IF i.pre_numcomprom IS NULL THEN
          v_monto_aprobado := pkg_legal_Dom_de.monto_aprobado(i.pre_credito,
                                                           p_fecha);
        ELSE
          v_monto_aprobado := pkg_legal_Dom_de.monto_aprobado(i.pre_numcomprom,
                                                           p_fecha);
        END IF;

        Begin
          V_FECHA_RENOVA := i.lca_fecrenov;--PKG_LEGAL_DOM.FECHA_RENOVA(i.pre_credito,p_fecha); --PARA FECHA DE RENOVACION
          --TIPOS DE CUOTAS EXISTENTES (1,3,5,6,7,8) (2002/05/27)
          --AMORTIZACION GRADUAL (CUOTAS FIJAS DE CAPITAL + INTERESES)
          w_diasper:=30;
          If i.pre_tipocuo in (1,3,5,7) then

            BEGIN
              select cuo_fecha,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       cuo_valor,
                                                       i.pre_moneda,
                                                       p_sucursal),
                           p_decimales),cuo_dias
                into fecha_primer_pago, valor_primer_pago,w_diasper
                from tpre_cuotas
               where cuo_credito = i.pre_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;

            --CUOTA FIJA DE CAPITAL
          Elsif i.pre_tipocuo = 6 then
            BEGIN
              select let_fecven,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       let_valor,
                                                       i.pre_moneda,
                                                       p_sucursal),
                           p_decimales)
                into fecha_primer_pago, valor_primer_pago
                from tpre_letras
               where let_credito = i.pre_credito and let_sec = 1;
		    EXCEPTION
              when no_data_found then
                fecha_primer_pago := null;
                valor_primer_pago := null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            -- TABLA ESPECIAL (Pagos indistintos con por el cliente)
          Elsif i.pre_tipocuo = 8 then
            BEGIN
              Select count(*), sum(cuo_valor)--cuo_capital
                Into numero_cuotas, total
                From tpre_cuotas
               Where cuo_credito = i.pre_credito;
            EXCEPTION
              when no_data_found then
                numero_cuotas := 0;
                total         := 0;
              when others then
                raise_application_error(-20203,
                                        'Error al obtener el capital del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;

            BEGIN
              Select cuo_valor,cuo_dias--cuo_capital
                Into primera_cuota,w_diasper
                From tpre_cuotas
               Where cuo_credito = i.pre_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                primera_cuota := null;
              when others then
                raise_application_error(-20204,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            If numero_cuotas != 0 then
              If total / numero_cuotas = primera_cuota then
                BEGIN
                  Select cuo_fecha,
                         round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                           cuo_valor,--cuo_capital,
                                                           i.pre_moneda,
                                                           p_sucursal),
                               p_decimales)
                    Into fecha_primer_pago, valor_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = i.pre_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                    valor_primer_pago := null;
                  when others then
                    raise_application_error(-20205,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;
              Else
                BEGIN
                  Select cuo_fecha
                    Into fecha_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = i.pre_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                  when others then
                    raise_application_error(-20206,
                                            'Error al obtener fecha de la cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;
                BEGIN
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital,
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_numcuo =
                         (Select max(cuo_numcuo)
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito) and
                         cuo_credito = i.pre_credito;
                EXCEPTION
                  when no_data_found then
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = i.pre_credito;
                  when others then
                    raise_application_error(-20207,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;

              End if;
            Else
              fecha_primer_pago := null;
              valor_primer_pago := null;
            End if;
          Else
            fecha_primer_pago := null;
            valor_primer_pago := null;
          End if;

        End; 
        --MONTO CUOTA 2016/03/07
                 if i.pre_tipocuo <> 6 and substr(i.pre_credito,1,1) = 6 then
                begin
                  -- HD4324_2 (SE HIZO UN SUM PORQUE PUEDE HABER UN ARREGLO DE PAGOS Y TENDREMOS DOS VALORES PARA CUO_CAPITAL)
                 Select SUM(round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital,
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales))
                    into valor_primer_pago
                    From tpre_cuotas
                   Where  cuo_credito = i.pre_credito and
                         cuo_dias >=28 and                             
                         to_char(cuo_fecha,'yyyymm') = to_char(p_fecha,'yyyymm');--DE DE13
                         
                  /*Select SUM(round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital,
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales))
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_fecha =
                         (Select max(cuo_fecha)
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito) and
                         cuo_credito = i.pre_credito */
                         /*cuo_numcuo = (Select max(cuo_numcuo) - 1
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito)*/
                   if nvl(valor_primer_pago,0) = 0 then
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = i.pre_credito;                   
                   end if;
                                 
                Exception
                  when no_data_found then
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = i.pre_credito;
                    -- insert into log_batch values (to_char(p_fecha,'yyyy/mm/dd')||' - '||to_char(i.Pre_credito)||' 1 ');
                  WHEN OTHERS THEN
                    raise_application_error(-20207,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                end;                
                end if;
        --FIN MONTO CUOTA
        existe:= i.lca_demandajundicial;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calificacion de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --calificacion inicial-final
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          v_calificacion_actual:=calificacion(P_FECHA,'P',i.pre_credito,14);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_actual := null;
        End;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fin de calificacion
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- provision de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --calificacion inicial-final
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_requerida_cap:= provision(P_FECHA,'P',i.pre_credito,9);
        Exception
          when no_data_found then
			provision_requerida_cap := null;
        End;
      --gradual cap
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_gradual_cap:= provision(P_FECHA,'P',i.pre_credito,11);
        Exception
          when no_data_found then
			provision_gradual_cap := null;
        End;

      --calificacion cubierta
      --gradual constituida
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_constituida_cap:= provision(P_FECHA,'P',i.pre_credito,12);
        Exception
          when no_data_found then
			provision_constituida_cap := null;
        End;
      --requerida int
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_requerida_int:= provision(P_FECHA,'P',i.pre_credito,8);
        Exception
          when no_data_found then
			provision_requerida_int := null;
        End;
      --gradual int
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_gradual_int:= provision(P_FECHA,'P',i.pre_credito,10);
        Exception
          when no_data_found then
			provision_gradual_int := null;
        End;


  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fin de provision
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

        Begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabrel AND DES_CODIGO = cli_relacionban and
                 cli_codigo = i.pre_clientep;
        EXCEPTION
          when no_data_found then
            relacion_banco := null;
          when others then
            raise_application_error(-20209,
                                    'Error relación banco del cliente ' ||
                                    i.pre_clientep || ' ' || sqlerrm);
        End;

        if i.pre_moneda = 0 then
          moneda := 'N';
        else
          moneda := 'E';
        end if;
        --Localidad new

        BEGIN
        localidad:=fun_localidad(i.pre_sucursal,i.pre_oficina,i.pre_clientep,i.pre_numdir);

           /*select substr(a.codeiso, 1, 6)
            Into localidad
            from tgen_desctabla a,tgen_oficina b
           where ofi_tabciu = des_codtab 
             and ofi_codciu = des_codigo
             and ofi_codsuc = i.pre_sucursal
             and ofi_codofi = i.pre_oficina;*/                 
        
          /*select substr(codeiso, 1, 4)
            Into localidad
            from tgen_desctabla, tpre_prestamos
           where pre_tabciudad = des_codtab and pre_ciudes = des_codigo and
                 pre_credito = i.pre_credito;*/
        Exception
          When no_data_found then
            localidad := null;
          when others then
            raise_application_error(-20214,
                                    'Error al al obtener la Localidad ' ||
                                    sqlerrm);
        END;
        --new
        PERIODOS_INT(i.pre_tipocuo, w_diasper, PERIODOINT);
        --PERIODOS(i.pre_tipocuo,i.pre_diaspercap,PERIODOCAP);
        PERIODOS_CAP(i.pre_tipocuo, i.pre_diasper, PERIODOCAP);


        --<<HD8164
        pro_gen_cuoextra(i.pre_credito,p_fecha,vln_monextra,vld_fcuoextra);
         vln_monextra:=vln_monextra*bdi_promedio(i.pre_moneda,1,p_fecha);
        vld_frevtasa:=fun_gen_revtasa(i.pre_credito,p_fecha);
        begin
          vld_faprob    :=null;
          vld_montaprob :=null;

          select sol_fecha,pre_montoorg*BDI_PROMEDIO(pre_moneda,1,p_fecha)--rosemary
          into vld_faprob,vld_montaprob
          from tpre_prestamos, tsol_solicitud
          where pre_Credito=i.pre_credito
          and   pre_credito=SOL_CUENTA;
        exception
          WHEN no_data_found then
            vld_faprob    :=i.pre_fecemi;
            vld_montaprob :=i.pre_valent;
          when others then
            vld_faprob    :=null;
            vld_montaprob :=null;
        end;
        -->>

        -- Insertamos datos en la tabla TVALUELEVELCUSTOM

        secuencial := secuencial + 1;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 1, secuencial, 1, secuencial); --NUMERO SECUENCIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 2, secuencial, 2, decode(v_tipoid,'P',REPLACE(identifica,'-',''),identifica)); --IDENTIFICACION DEL DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 3, secuencial, 3, tipocli); --TIPO DE DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 4, secuencial, 4, nombre); --NOMBRES / RAZON SOCIAL DEL DEUDOR

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 5, secuencial, 5, apellido); --APELLIDOS / SIGLAS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 6, secuencial, 6, i.pre_credito); --CODIGO DEL CREDITO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 7, secuencial, 7, i.pre_fecontab); --FECHA DE DESEMBOLSO
                
          v_incrementos:=0;
           SELECT nvl(SUM(NVL(INC_MONTO,0))*bdi_promedio(i.pre_moneda,1,p_fecha),0)
              INTO  v_incrementos
		    FROM TPRE_INCCAPITAL 
		    WHERE INC_CREDITO= i.pre_credito      
		      AND INC_STATUS = '2'
		      AND TRUNC(INC_FECHAAUT)<=P_FECHA--SYSDATE 
		      AND INC_NUMTRA IS NOT NULL;
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 8, secuencial, 8, i.v + v_incrementos); --MONTO DESEMBOLSADO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 9, secuencial, 9, i.pre_fecven); --FECHA DE VENCIMIENTO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 10, secuencial, 10, fecha_primer_pago); --FECHA INICIO DEL PRIMER PAGO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 11, secuencial, 11, valor_primer_pago); --MONTO DE LA CUOTA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 12, secuencial, 12, i.t); --TASA DE INTERES Y COMISION VIGENTE

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 13, secuencial, 13, moneda); --TIPO DE MONEDA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 14, secuencial, 14, existe); --COBRANZA JUDICIAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 15, secuencial, 15, v_CALIFICACION_actual); --CLASIFICACION DEL CREDITO SEGUN LA ENTIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 16, secuencial, 16, provision_requerida_cap); --PROVISION REQUERIDA

        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 17, secuencial, 17, null); --*/

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 17, secuencial, 17, relacion_banco); --TIPO DE VINCULACION

        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 18, secuencial, 18, localidad); --LOCALIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 19, secuencial, 19, i.pre_sucursal); --NUMERO DE OFICINA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 20, secuencial, 20, periodoCAP); --FORMA DE PAGO DEL CAPITAL

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 21, secuencial, 21, periodoINT); --FORMA DE PAGO DE INTERESES Y COMISIONES

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 22, secuencial, 22, i.pre_fecarr); --FECHA DE REESTRUCTURACION
        -- Campos que se incrementaron para hd-4324

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 23, secuencial, 23, V_FECHA_RENOVA); --FECHA DE RENOVACION
        --<<hd8164
        /*
        --balance

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 25, secuencial, 25, v_balance_fecha);
        */       
           BEGIN
           vld_faprob:=fecha_aprobado(i.pre_numcomprom,i.pre_credito,i.pre_extrefer);
           EXCEPTION
              WHEN OTHERS THEN
                vld_faprob:=i.pre_fecemi ;
           END;

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 24, secuencial, 24, vld_faprob); --FECHA DE APROBACION
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 25, secuencial, 25, vld_montaprob); --MONTO APROBADO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 26, secuencial, 26, 'S'); --OPCION DE PAGO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 27, secuencial, 27, 0.00); --PENALIZACION POR PAGO
        
        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 29, secuencial, 29,  provision_gradual_cap); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE CAPITAL*/
        --abril 2018  
        /*
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 28, secuencial, 28, provision_constituida_cap); */--PROVISION DE CAPITAL CONSTITUIDA POR EL CREDITO
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 29, secuencial, 29, provision_requerida_int); --PROVISION REQUERIDA DE RENDIMIENTOS
          
        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 32, secuencial, 32, provision_gradual_int); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE RENDIMIENTOS*/
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 30, secuencial, 30, null); --PROVISION REQUERIDA DE CONTINGENCIAS
          
          
        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 34, secuencial, 34, null); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE CONTINGENCIAS*/
          
        if trunc(i.pre_fecance) <= p_fecha then
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 31, secuencial, 31, null); --FECHA REVISION TASA DE INTERES
        else                                       
           IF I.LCA_TIPOTASA = 'F' then        
		        INSERT INTO tvaluelevelcustom
		          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
		        VALUES
		          (p_report, p_session, 31, secuencial, 31, null); --FECHA REVISION TASA DE INTERES
           else   
		        INSERT INTO tvaluelevelcustom
		          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
		        VALUES
		          (p_report, p_session, 31, secuencial, 31, vld_frevtasa); --FECHA REVISION TASA DE INTERES           
           end if;
        end if;
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 32, secuencial, 32, vld_fcuoextra); --FECHA PAGO DE CUOTA EXTRAORDINARIA
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 33, secuencial, 33, vln_monextra); --MONTO DE CUOTA EXTRAORDINARIA
        if i.lca_reestructurado = 'S' then
           w_restructurado:='RN';
        else
           w_restructurado:='NR';
        end if;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 34, secuencial, 34,w_restructurado); --MONTO DE CUOTA EXTRAORDINARIA
          
       v_tipocliente:=fun_tipo_cliente(i.pre_clientep);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 35, secuencial, 35,v_tipoCliente); --Tipo Cliente
          
        v_facilidad:=fun_producto_servicio(i.pre_credito,6);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 36, secuencial, 36,v_facilidad); --Facilidad Crediticia

        --JUNIO 2015                                                                   
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 37, secuencial, 37,I.LCA_TIPOTASA); --Tipo de Tasa

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 38, secuencial, 38,I.LCA_TIPORECURSO); --ORIGEN O TIPO DE RECURSO

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 39, secuencial, 39,NULL); --CODIGO DE FACILIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 40, secuencial, 40,I.LCA_ORIGENCREDITO); --ORIGEN DEL CRÉDITO
      --SYSAID 15245 ABRIL 2018
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 41, secuencial, 41,I.LCA_FECREFIN); --FECHA REFINANCIACION
          
          SELECT  MAX(PRL_DESDE) 
			  INTO V_JUDDESDE
			FROM TPRE_PRESLEG 
			WHERE PRL_CREDITO = i.pre_credito
			  AND P_FECHA between trunc(PRL_DESDE) and trunc(PRL_HASTA);			
  
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 42, secuencial, 42,V_JUDDESDE); --FECHA INICIO COBRANZA JUDICIAL
        
        PERGRACIA(i.pre_credito, p_fecha, p_gracia);
        
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 43, secuencial, 43,P_GRACIA); --PERIODO DE GRACIA     
          
        garantia_adm:= fun_garantia(i.pre_credito,p_fecha,'DE13');
        
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 44, secuencial, 44,garantia_adm); --COBERTURA DE GARANTIA ADMINISBLE
       
       Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          v_calificacion_expuesto:=calificacion(P_FECHA,'P',i.pre_credito,13);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_expuesto := null;
        End;
           
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 45, secuencial, 45,v_calificacion_expuesto); --CLASIFICACION MONTO EXPUETO

		--calificacion cubierta
        Begin
          v_calificacion_cubierto:=calificacion(P_FECHA,'P',i.pre_credito,12);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_cubierto := null;
        End;
                                                  
          
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 46, secuencial, 46,v_calificacion_cubierto); --CLASIFICACION MONTO EXPUETO


        -->>HD8164
      End loop; --cursor CREDITOS_C
      commit;
      if salida = 'A' then
        pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
      end if;
    Else
      raise_application_error(-20806,
                              '!!! MES NO CONCLUIDO ¡¡¡, DEBE SELECCIONAR MESES CONCLUIDOS ' ||
                              sqlerrm);
    End if;
  END;

  -------------------------------------------------------------------------------
  -- PROCEDIMIENTO PARA OBTENER CREDITOS HIPOTECARIOS DE LA ENTIDAD POR DEUDOR --
  -------------------------------------------------------------------------------
  -- PRCP 22/05/2002
  PROCEDURE de15(p_report       in varchar2,
                 p_session      in number,
                 p_sucursal     in number,
                 p_fecha        in date,
                 p_tipocredito1 in number,
                 p_tipocredito2 in number,
                 p_tipocredito3 in number,
                 p_decimales    in number) IS

    fecha_primer_pago     date;
    valor_primer_pago     tpre_cuotas.cuo_valor%type;
    existe                char(1);
    moneda                char(1);
    relacion_banco        char(8);
    calificacion_actual   tpre_califica.CAL_CALIFACTUAL%type;
    v_calificacion_actual varchar2(2);
    provision_requerida   tpre_calificadet.CAD_PREVCONSTITCONTINMN%type;
    primera_cuota         number(18, 6);
    numero_cuotas         number(3);
    total                 number(18, 6);
    v_balance_fecha       tpre_prestamos.pre_monto%type;
    v_monto_aprobado      tpre_prestamos.pre_monto%type;
    -- Definicion de variables para identificar al deudor

    nombre     tcli_persona.cli_nombre%type;
    apellido   tcli_persona.cli_nombre%type;
    identifica varchar2(20);
    tipocli    char(2);
    secuencial number(7) := 0;
    w_diasper number:=0;
    --new
    V_FECHA_RENOVA DATE;
    localidad      VARCHAR2(8);--2012
    --
    vld_frevtasa           date;
    vld_fcuoextra          date;
    vln_monextra           number(15,2);
   --15648
    v_tipocliente varchar2(3);
    v_facilidad  varchar2(3);
    v_incrementos NUMBER:=0;

        provision_requerida_cap number:=0;
        provision_gradual_cap number:=0;
        provision_constituida_cap number:=0;
        provision_requerida_int number:=0;
        provision_gradual_int number:=0;
        w_restructurado varchar2(2);
    CURSOR CREDITOS_H IS

      select pre_credito,
             pre_fecontab,
             round(pkg_legalreport3.cotiza_mon(p_fecha,
                                               pre_valent,
                                               pre_moneda,
                                               p_sucursal),
                   p_decimales) v,
             pre_fecven,
             pre_tasapac t,
             pre_moneda,
             pre_tipocuo,
             pre_clientep,
             pre_numdir,
             pre_mod,
             pre_pro,
             pre_tip,
             ( SELECT SUC_SIB FROM tdom_mapsucsib WHERE SUC_CORE = pre_sucursal) pre_sucursal,
             pre_oficina,
             LCA_FECREESTRUC PRE_FECARR,
             pre_numcomprom,
             lca_fecrenov,
		     lca_fecrefin,             
             lca_demandajundicial,
             lca_montogaradmi,
             pre_fecemi,
             pre_valent,
             pre_fecance,
             lca_reestructurado,
			LCA_MONTOCUOTAEXTRAOR,  
			LCA_TIPOTASA         ,  
			LCA_BALPROMDCAPMES   ,  
			LCA_INTDEVENCORTE    ,  
			LCA_COMCARGODEVCORTE ,  
			LCA_ESTRUCTURACIONCRE,  
			LCA_ORIGENCREDITO    ,  
			LCA_TIPORECURSO                     
        from tpre_prestamos,tleg_operacioncalif
       where pre_mod = lca_mod
         and pre_credito = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'H'--
       order by v desc;
       v_tipoid varchar2(1);
  V_JUDDESDE DATE; --SYSAID 15245 ABRIL 2018
  p_gracia NUMBER; --SYSAID 15245 ABRIL 2018
  garantia_adm NUMBER; --SYSAID 15245 ABRIL 2018
  v_calificacion_expuesto varchar2(2);--SYSAID 15245 ABRIL 2018
  v_calificacion_cubierto varchar2(2);--SYSAID 15245 ABRIL 2018
       
  BEGIN

    If trunc(p_fecha) < trunc(sysdate) then
      dbms_output.put_line('en de15');
      pkg_legalreport.eraser(p_report, p_session);

      for i in CREDITOS_H loop
        dbms_output.put_line('dentro del loop');
        provision_requerida_cap:=0;
        provision_gradual_cap:=0;
        provision_constituida_cap:=0;
        provision_requerida_int:=0;
        provision_gradual_int:=0;
        v_tipoid:=null;
        begin
          select cli_tipoid 
          into v_tipoid
           from tcli_persona
           where cli_codigo = i.pre_clientep;
        exception 
           when others then
              v_tipoid:=null;
        end;
        pkg_legalreport3.datos_cliente(i.pre_clientep,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido);
        dbms_output.put_line('nombre ' || nombre || ' ' || apellido);
        dbms_output.put_line('id ' || identifica);
        dbms_output.put_line('tipoper ' || tipocli);
        v_calificacion_actual := null;
        v_balance_fecha       := 0;
        v_balance_fecha       := saldo_capital_fecha(i.pre_credito, p_fecha);
        IF i.pre_numcomprom IS NULL THEN
          v_monto_aprobado := pkg_legal_Dom_de.monto_aprobado(i.pre_credito,
                                                           p_fecha);
        ELSE
          v_monto_aprobado := pkg_legal_Dom_de.monto_aprobado(i.pre_numcomprom,
                                                           p_fecha);
        END IF;

        Begin
          V_FECHA_RENOVA := i.LCA_FECRENOV;--PKG_LEGAL_DOM.FECHA_RENOVA(i.pre_credito,
                                          --             p_fecha); --PARA FECHA DE RENOVACION
          --TIPOS DE CUOTAS EXISTENTES (1,3,5,6,7,8) (2002/05/27)
          --AMORTIZACION GRADUAL (CUOTAS FIJAS DE CAPITAL + INTERESES)
          w_diasper:=30;
          If i.pre_tipocuo in (1,3,5,7) then

            BEGIN
              select cuo_fecha,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       cuo_valor,
                                                       i.pre_moneda,
                                                       p_sucursal),
                           p_decimales),cuo_dias
                into fecha_primer_pago, valor_primer_pago,w_diasper
                from tpre_cuotas
               where cuo_credito = i.pre_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                fecha_primer_pago := null;
                valor_primer_pago := null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;

          Elsif i.pre_tipocuo = 6 then
            BEGIN
              select let_fecven,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       let_valor,
                                                       i.pre_moneda,
                                                       p_sucursal),
                           p_decimales)
                into fecha_primer_pago, valor_primer_pago
                from tpre_letras
               where let_credito = i.pre_credito and let_sec = 1;
		    EXCEPTION
              when no_data_found then
                fecha_primer_pago := null;
                valor_primer_pago := null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            -- TABLA ESPECIAL (Pagos indistintos con por el cliente
        Elsif i.pre_tipocuo = 8 then
            BEGIN
              Select count(*), sum(cuo_valor)--cuo_capital
                Into numero_cuotas, total
                From tpre_cuotas
               Where cuo_credito = i.pre_credito;
            EXCEPTION
              when no_data_found then
                numero_cuotas := 0;
                total         := 0;
              when others then
                raise_application_error(-20203,
                                        'Error al obtener capital del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            BEGIN
              Select cuo_valor,cuo_dias --cuo_capital
                Into primera_cuota,w_diasper
                From tpre_cuotas
               Where cuo_credito = i.pre_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                primera_cuota := null;
              when others then
                raise_application_error(-20204,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        i.pre_credito || ' ' || sqlerrm);
            END;
            If numero_cuotas != 0 then
              If total / numero_cuotas = primera_cuota then
                BEGIN
                  Select cuo_fecha,
                         round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                           cuo_valor,--cuo_capital,
                                                           i.pre_moneda,
                                                           p_sucursal),
                               p_decimales)
                    Into fecha_primer_pago, valor_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = i.pre_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                    valor_primer_pago := null;
                  when others then
                    raise_application_error(-20205,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;

              Else
                BEGIN
                  Select cuo_fecha
                    Into fecha_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = i.pre_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                  when others then
                    raise_application_error(-20206,
                                            'Error al obtener fecha de la cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;
                --para valor de cuota
                BEGIN
                   Select SUM(round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital,
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales))
                    into valor_primer_pago
                    From tpre_cuotas
                   Where  cuo_credito = i.pre_credito and
                         cuo_dias >=28 and                             
                         to_char(cuo_fecha,'yyyymm') = to_char(p_fecha,'yyyymm');--DE15
                         
                  /*Select SUM(round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales)) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_fecha =
                         (Select max(cuo_fecha)
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito) and
                                 cuo_dias > 0 and --nuevo por error de rosmeario 05/2016
                         cuo_credito = i.pre_credito;*/
                         --and rownum = 1;
                if nvl(valor_primer_pago,0) = 0 then
	                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
	                                                               cuo_valor,--cuo_capital
	                                                               i.pre_moneda,
	                                                               p_sucursal),
	                                   p_decimales) --HD4324_2
	                    into valor_primer_pago
	                    From tpre_cuotas
	                   Where cuo_NUMCUO = 1 and
	                         cuo_credito = i.pre_credito;                   
                   end if;
                                         
                EXCEPTION
                  when no_data_found then
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = i.pre_credito;
                  when others then
                    raise_application_error(-20207,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                END;

              End if;
            Else
              fecha_primer_pago := null;
              valor_primer_pago := null;
            End if;
          Else
            fecha_primer_pago := null;
            valor_primer_pago := null;
          End if;

        End;    
        --MNTO CUOTA     
           if i.pre_tipocuo <> 6 and substr(i.pre_credito,1,1) = 6 then
                begin
                  -- HD4324_2 (SE HIZO UN SUM PORQUE PUEDE HABER UN ARREGLO DE PAGOS Y TENDREMOS DOS VALORES PARA CUO_CAPITAL)
                   Select SUM(round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital,
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales))
                    into valor_primer_pago
                    From tpre_cuotas
                   Where  cuo_credito = i.pre_credito and
                         cuo_dias >=28 and                             
                         to_char(cuo_fecha,'yyyymm') = to_char(p_fecha,'yyyymm');--DE15
                         
                  
                  /*Select SUM(round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital,
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales))
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_fecha =
                         (Select max(cuo_fecha)
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito) and
                         cuo_credito = i.pre_credito and
                         cuo_dias > 0*/--nuevo por error de rosmeario 05/2016
                         /*cuo_numcuo = (Select max(cuo_numcuo) - 1
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = i.pre_credito)*/
                   if nvl(valor_primer_pago,0) = 0 then
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = i.pre_credito;                   
                   end if;
                                 
                Exception
                  when no_data_found then
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               i.pre_moneda,
                                                               p_sucursal),
                                   p_decimales) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = i.pre_credito;
                    -- insert into log_batch values (to_char(p_fecha,'yyyy/mm/dd')||' - '||to_char(i.Pre_credito)||' 1 ');
                  WHEN OTHERS THEN
                    raise_application_error(-20207,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            i.pre_credito || ' ' || sqlerrm);
                end;                
        end if;
        --FIN MONTO CUOTA
        existe:= i.lca_demandajundicial;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calificacion de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --calificacion inicial-final
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          v_calificacion_actual:=calificacion(P_FECHA,'P',i.pre_credito,14);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_actual := null;
        End;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fin de calificacion
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- provision de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  --calificacion inicial-final
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_requerida_cap:= provision(P_FECHA,'P',i.pre_credito,9);
        Exception
          when no_data_found then
			provision_requerida_cap := null;
        End;
      --gradual cap
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_gradual_cap:= provision(P_FECHA,'P',i.pre_credito,11);
        Exception
          when no_data_found then
			provision_gradual_cap := null;
        End;

      --calificacion cubierta
      --gradual constituida
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_constituida_cap:= provision(P_FECHA,'P',i.pre_credito,12);
        Exception
          when no_data_found then
			provision_constituida_cap := null;
        End;
      --requerida int
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_requerida_int:= provision(P_FECHA,'P',i.pre_credito,8);
        Exception
          when no_data_found then
			provision_requerida_int := null;
        End;
      --gradual int
      Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          provision_gradual_int:= provision(P_FECHA,'P',i.pre_credito,10);
        Exception
          when no_data_found then
			provision_gradual_int := null;
        End;


  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fin de provision
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabrel AND DES_CODIGO = cli_relacionban and
                 cli_codigo = i.pre_clientep;

        EXCEPTION
          when no_data_found then
            relacion_banco := null;
          when others then
            raise_application_error(-20209,
                                    'Error relación banco ' ||
                                    i.pre_clientep || ' ' || sqlerrm);
        end;

        --Localidad new

        BEGIN
        localidad:=fun_localidad(i.pre_sucursal,i.pre_oficina,i.pre_clientep,i.pre_numdir);

           /*select substr(a.codeiso, 1, 6)
            Into localidad
            from tgen_desctabla a,tgen_oficina b
           where ofi_tabciu = des_codtab 
             and ofi_codciu = des_codigo
             and ofi_codsuc = i.pre_sucursal
             and ofi_codofi = i.pre_oficina; */                
        
          /*select substr(codeiso, 1, 4)
            Into localidad
            from tgen_desctabla, tpre_prestamos
           where pre_tabciudad = des_codtab and pre_ciudes = des_codigo and
                 pre_credito = i.pre_credito;*/
        Exception
          When no_data_found then
            localidad := null;
          when others then
            raise_application_error(-20214,
                                    'Error al al obtener la Localidad ' ||
                                    sqlerrm);
        END;

        if i.pre_moneda = 0 then
          moneda := 'N';
        else
          moneda := 'E';
        end if;

        -- Conversion de la provisión requerida a pesos dominicanos
        --<<HD8164
        pro_gen_cuoextra(i.pre_credito,p_fecha,vln_monextra,vld_fcuoextra);
        vln_monextra:=vln_monextra*bdi_promedio(i.pre_moneda,1,p_fecha);
        vld_frevtasa:=fun_gen_revtasa(i.pre_credito,p_fecha);
        -->>
        -- Insertamos datos en la tabla TVALUELEVELCUSTOM

        secuencial := secuencial + 1;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 1, secuencial, 1, secuencial);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 2, secuencial, 2, decode(v_tipoid,'P',REPLACE(identifica,'-',''),identifica));      --solo parasaportes
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 3, secuencial, 3, tipocli);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 4, secuencial, 4, nombre);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 5, secuencial, 5, apellido);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 6, secuencial, 6, i.pre_credito);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 7, secuencial, 7, i.pre_fecontab);
          v_incrementos:=0;
           SELECT nvl(SUM(NVL(INC_MONTO,0))*bdi_promedio(i.pre_moneda,1,p_fecha),0)
              INTO  v_incrementos
		    FROM TPRE_INCCAPITAL 
		    WHERE INC_CREDITO= i.pre_credito      
		      AND INC_STATUS = '2'
		      AND TRUNC(INC_FECHAAUT)<=P_FECHA--SYSDATE 
		      AND INC_NUMTRA IS NOT NULL;
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 8, secuencial, 8, i.v + v_incrementos);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 9, secuencial, 9, i.pre_fecven);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 10, secuencial, 10, fecha_primer_pago);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 11, secuencial, 11, valor_primer_pago);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 12, secuencial, 12, i.t);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 13, secuencial, 13, moneda);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 14, secuencial, 14, existe);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEchar)
        VALUES
          (p_report, p_session, 15, secuencial, 15, v_CALIFICACION_actual);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 16, secuencial, 16, provision_requerida_cap);

        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 17, secuencial, 17, null);*/

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 17, secuencial, 17, relacion_banco);
        --
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 18, secuencial, 18, localidad); --LOCALIDAD

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 19, secuencial, 19, i.pre_sucursal); --NUMERO DE OFICINA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 20, secuencial, 20,i.PRE_FECARR); --FECHA DE REESTRUCTURACION

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 21, secuencial, 21, V_FECHA_RENOVA); --FECHA DE RENOVACION
        --<<HD8164
        /*
        --balance
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUEnum)
        VALUES
          (p_report, p_session, 23, secuencial, 23, v_balance_fecha);
        */
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 22, secuencial, 22, 'S'); --OPCION DE PAGO
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 23, secuencial, 23, 0.00); --PENALIZACION POR PAGO
        
        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 25, secuencial, 25, provision_gradual_cap); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE CAPITAL*/
          
          --Abril 2018
        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 24, secuencial, 24, provision_constituida_cap); */--PROVISION DE CAPITAL CONSTITUIDA POR EL CREDITO
          
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 25, secuencial, 25, provision_requerida_int); --PROVISION REQUERIDA DE RENDIMIENTOS
          
        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 28, secuencial, 28, provision_gradual_int); --PROVISION REQUERIDA GRADUAL INDIVIDUAL DE RENDIMIENTOS*/
          
       if trunc(i.pre_fecance) <= p_fecha then
	        INSERT INTO tvaluelevelcustom
	          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
	        VALUES
	          (p_report, p_session, 26, secuencial, 26, null); --FECHA REVISION TASA DE INTERES
        else                                                     
          if I.LCA_TIPOTASA = 'F' then
	        INSERT INTO tvaluelevelcustom
	          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
	        VALUES
	          (p_report, p_session, 26, secuencial, 26, null); --FECHA REVISION TASA DE INTERES
	       else
	       	        INSERT INTO tvaluelevelcustom
	          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
	        VALUES
	          (p_report, p_session, 26, secuencial, 26, vld_frevtasa); --FECHA REVISION TASA DE INTERES
	       end if;
        end if;                   
                                                                 
        
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 27, secuencial, 27, vld_fcuoextra); --FECHA PAGO DE CUOTA EXTRAORDINARIA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 28, secuencial, 28, vln_monextra); --MONTO DE CUOTA EXTRAORDINARIA
          

        if i.lca_reestructurado = 'S' then
           w_restructurado:='RN';
        else
           w_restructurado:='NR';
        end if;
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 29, secuencial, 29, w_restructurado); --normativa abril 2009   
                
       v_tipocliente:=fun_tipo_cliente(i.pre_clientep);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 30, secuencial, 30,v_tipoCliente); --Tipo Cliente
          
        v_facilidad:=fun_producto_servicio(i.pre_credito,6);
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 31, secuencial, 31,v_facilidad); --Facilidad Crediticia          
        /*INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 31, secuencial, 31, vln_monextra); --MONTO DE CUOTA EXTRAORDINARIA*/
        -->>HD8164                                                                     
        --JUNIO 2015    
        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 32, secuencial, 32,I.LCA_TIPOTASA); --TIPO DE TASA

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 33, secuencial, 33,I.LCA_TIPORECURSO); --ORIGEN O TIPO DE RECURSOS

        INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 34, secuencial, 34,I.LCA_ORIGENCREDITO); --ORIGEN O TIPO DE RECURSOS        
        --FIN JUNIO 2015
      --SYSAID 15245 ABRIL 2018
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 35, secuencial, 35,I.LCA_FECREFIN); --FECHA REFINANCIACION
          
          SELECT  MAX(PRL_DESDE) 
			  INTO V_JUDDESDE
			FROM TPRE_PRESLEG 
			WHERE PRL_CREDITO = i.pre_credito
			  AND P_FECHA between trunc(PRL_DESDE) and trunc(PRL_HASTA);
  
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 36, secuencial, 36,V_JUDDESDE); --FECHA INICIO COBRANZA JUDICIAL
        
        PERGRACIA(i.pre_credito, p_fecha, p_gracia);
        
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
        VALUES
          (p_report, p_session, 37, secuencial, 37,P_GRACIA); --PERIODO DE GRACIA     
          
        garantia_adm:= fun_garantia(i.pre_credito,p_fecha,'DE15');
        
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 38, secuencial, 38,garantia_adm); --COBERTURA DE GARANTIA ADMINISBLE
       
       Begin
          --select round(nvl(cad_prevconstitdir,0) + nvl(cad_prevconstitcontin,0),p_decimales), cad_califcuenta
          v_calificacion_expuesto:=calificacion(P_FECHA,'P',i.pre_credito,13);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_expuesto := null;
        End;
           
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 39, secuencial, 39,v_calificacion_expuesto); --CLASIFICACION MONTO EXPUETO

		--calificacion cubierta
        Begin
          v_calificacion_cubierto:=calificacion(P_FECHA,'P',i.pre_credito,12);
        Exception
          when no_data_found then
            calificacion_actual   := null;
            v_calificacion_cubierto := null;
        End;
                                                  
          
		 INSERT INTO tvaluelevelcustom
          (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
        VALUES
          (p_report, p_session, 40, secuencial, 40,v_calificacion_cubierto); --CLASIFICACION MONTO EXPUETO

        
      End loop; --cursor CREDITOS_H
      commit;
      pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
    Else
      raise_application_error(-20806,
                              '!!! MES NO CONCLUIDO ¡¡¡, DEBE SELECCIONAR MESES CONCLUIDOS ' ||
                              sqlerrm);
    End if;
  END;


  -- FUNCION PARA OBTENER LA CUENTA CONTABLE DE UN CREDITO VIGENTE, EN MORA Y VENCIDO

  FUNCTION CUENTA_CONTABLE(pModulo       in number,
                           pProducto     in number,
                           pTipo         in number,
                           pMoneda       in number,
                           pTipo_cartera in number) RETURN varchar2 IS

    Cta_Contable varchar2(14) := '00000000000000';

  BEGIN
    Select gdt_cuenta
      into Cta_Contable
      From trep_funcgroupdet
     Where gdt_mod = pModulo And gdt_pro = pProducto And gdt_tip = pTipo And
           gdt_mon = pMoneda And gdt_codgru = pTipo_cartera;
    RETURN Cta_Contable;

  Exception
    when no_data_found then
      raise_application_error(-20821,
                              'Cuenta Contable no definida en Trn. 21/2041 ' ||
                              'Módulo ' || pModulo || ' Producto ' ||
                              pProducto || ' Tipo ' || pTipo || ' Moneda ' ||
                              pMoneda || ' Cartera ' || pTipo_cartera);
    When others then
      raise_application_error(-20822,
                              'Revisar definición de Cuenta Contable en Trn. 21/2041 para producto ' ||
                              'Módulo ' || pModulo || ' Producto ' ||
                              pProducto || ' Tipo ' || pTipo || ' Moneda ' ||
                              pMoneda || ' Cartera ' || pTipo_cartera);
  END;

  FUNCTION CUE_CONTABLE(pModulo       in number,
                        pProducto     in number,
                        pTipo         in number,
                        pMoneda       in number,
                        pTipo_cartera in number) RETURN varchar2 IS

    Cta_Contable varchar2(14) := '00000000000000';

  BEGIN

    SELECT GDT_CUENTA
      INTO CTA_CONTABLE
      FROM TPRE_FUNCGROUPDET A, TPRE_PRESTAMOS
     WHERE GDT_PRO = PPRODUCTO AND GDT_TIP = PTIPO AND GDT_MON = PMONEDA AND
           GDT_CODGRU = ptipo_cartera;
    RETURN Cta_Contable;
  Exception
    when others then
      CTA_CONTABLE := NULL;
      RETURN CTA_CONTABLE;
  END;

  FUNCTION RECURSO(p_mod in number, p_cuenta in number) RETURN varchar2 IS
    tipo_recurso varchar2(2);
  BEGIN
    select codeiso
      into tipo_recurso
      from tpre_prestamos, tgen_desctabla,TPRE_PROTIPOCREDITO
     where pre_mod = p_mod 
       and pre_credito = p_cuenta 
       and nvl(PTC_TABORIGENFONDOS,pre_taborigfondos) = des_codtab 
       and nvl(PTC_ORIGENFONDOS,pre_origfondos) = des_codigo
       and pre_mod = ptc_mod
       and pre_pro=ptc_pro
       and pre_tip = ptc_tip
       and pre_moneda = ptc_mon;
    Return tipo_recurso;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      tipo_recurso := NULL;
      Return tipo_recurso;
  END;

  ---------------------------------------------------
  -- PROCEDIMIENTO PARA OBTENER EL TIPO DE PERIODO --
  ---------------------------------------------------
  -- PRCP 26/06/2002

  PROCEDURE PERIODOS_CAP(p_tipocuo in number,
                         p_diasper in number,
                         p_PERIODO OUT CHAR) IS
    meses number;
  BEGIN

    if p_tipocuo IN (5, 7) then
      p_PERIODO := 'V';
    elsif p_tipocuo = 8 then
      p_PERIODO := 'I';
    elsif p_tipocuo = 6 then
      p_PERIODO := 'P';
    elsif p_diasper = 360 then
      p_PERIODO := 'A';
    elsif p_diasper = 180 then
      p_PERIODO := 'S';
    elsif p_diasper = 120 then
      p_PERIODO := 'C';
    elsif p_diasper = 90 then
      p_PERIODO := 'T';
    elsif p_diasper = 30 then
      p_PERIODO := 'M';
    elsif p_diasper = 15 then
      p_PERIODO := 'Q';
    else
      meses := round(p_diasper / 30);
      if meses = 0 then
        p_PERIODO := 'V';
      elsif meses = 1 then
        p_PERIODO := 'M';
      elsif meses = 2 then
        p_PERIODO := 'B';
      elsif meses = 3 then
        p_PERIODO := 'T';
      elsif meses = 4 then
        p_PERIODO := 'C';
      elsif meses = 6 then
        p_PERIODO := 'S';
      elsif meses = 12 then
        p_PERIODO := 'A';
      else
        p_PERIODO := 'V';
      end if;

    end if;
  END;
  PROCEDURE PERIODOS_INT(p_tipocuo in number,
                         p_diasper in number,
                         p_PERIODO OUT CHAR) IS
    meses number;
  BEGIN

    if p_tipocuo = 5 then
      p_PERIODO := 'V';
    elsif p_tipocuo = 6 then
      p_PERIODO := 'P';
    elsif p_diasper = 360 then
      p_PERIODO := 'A';
    elsif p_diasper = 180 then
      p_PERIODO := 'S';
    elsif p_diasper = 120 then
      p_PERIODO := 'C';
    elsif p_diasper = 90 then
      p_PERIODO := 'T';
    elsif p_diasper = 30 then
      p_PERIODO := 'M';
    elsif p_diasper = 15 then
      p_PERIODO := 'Q';
    --elsif p_tipocuo = 8 then
      --p_PERIODO := 'I';
    else
      meses := round(p_diasper / 30);
      if meses = 0 then
        p_PERIODO := 'V';
      elsif meses = 1 then
        p_PERIODO := 'M';
      elsif meses = 2 then
        p_PERIODO := 'B';
      elsif meses = 3 then
        p_PERIODO := 'T';
      elsif meses = 4 then
        p_PERIODO := 'C';
      elsif meses = 6 then
        p_PERIODO := 'S';
      elsif meses = 12 then
        p_PERIODO := 'A';
      else
        p_PERIODO := 'V';
      end if;
    end if;
  END;
  --FECHA RENOVA
  FUNCTION FECHA_RENOVA(p_credito in number, p_fecha in Date) RETURN DATE IS
    v_plazo      number := 0;
    p_fecharenov DATE;
  BEGIN
    SELECT ACR_PLAZO
      INTO v_plazo
      FROM TPRE_ACCRUAL
     WHERE ACR_CREDITO = P_CREDITO AND ACR_FECHA = P_FECHA;

    SELECT MAX(PRE_FECHAHISTORICO)
      INTO p_fecharenov
      FROM TPRE_PRESTAMOSHIS
     WHERE PRE_CREDITO = P_CREDITO AND TRUNC(PRE_FECHAHISTORICO) <= P_FECHA AND
           PRE_PLAZO <> v_plazo;
    RETURN p_fecharenov;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END;
  --FIN
  FUNCTION CODIGO_PAIS(p_clientep in number, p_numdir in number)
    RETURN VARCHAR2 IS
    codigo_pais VARCHAR2(4);
  BEGIN
    SELECT CODEISO
      INTO codigo_pais
      FROM TCLI_DIRECCION, TGEN_DESCTABLA
     WHERE DIR_TABPAIS = DES_CODTAB AND DIR_PAIS = DES_CODIGO AND
           DIR_CODCLI = P_CLIENTEP AND DIR_NUMERO = P_NUMDIR;
    RETURN CODIGO_PAIS;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END;
  --
  PROCEDURE PERGRACIA(p_credito IN NUMBER,
                      p_fecha   IN DATE,
                      p_gracia  OUT NUMBER) IS
  BEGIN
    SELECT COUNT(*)
      INTO p_gracia
      FROM TPRE_CUOTAS
     WHERE cuo_credito = p_credito AND
           cuo_fecha <
           (SELECT MIN(CUO_FECHA)
              FROM TPRE_CUOTAS
             WHERE cuo_credito = p_credito AND cuo_fecha <= p_fecha AND
                   NVL(cuo_capital, 0) <> 0) AND cuo_capital = 0;
  END;

  PROCEDURE PLANOLEG24(p_report  in varchar2,
                       p_session in number,
                       p_fecha   in date) IS

    CURSOR rows IS
      SELECT DISTINCT rowsid
        FROM tvaluelevelcustom
       WHERE report = p_report AND sessionid = p_session;

    CURSOR column(pp_rowsid in number) IS

      SELECT a.columNid,
             a.COLUMNA,
             columntyp,
             columnlen,
             filling,
             concaten,
             A.CODE
        FROM (SELECT REPORT,
                     CODE,
                     TO_NUMBER(VALUECHAR, '999999999999999999999999') COLUMNA,
                     '1' COLUMNID
                FROM TVALUELEVELCUSTOM
               WHERE REPORT = P_REPORT AND SESSIONID = p_session
              UNION
              SELECT REPORT, CODE, COLUMN1 COLUMNA, '2' COLUMNID
                FROM TVALUELEVELCUSTOM
               WHERE REPORT = P_REPORT AND SESSIONID = p_session
              UNION
              SELECT REPORT, CODE, COLUMN2 COLUMNA, '3' COLUMNID
                FROM TVALUELEVELCUSTOM
               WHERE REPORT = P_REPORT AND SESSIONID = p_session
              UNION
              SELECT REPORT, CODE, COLUMN3 COLUMNA, '4' COLUMNID
                FROM TVALUELEVELCUSTOM
               WHERE REPORT = P_REPORT AND SESSIONID = p_session
              UNION
              SELECT REPORT, CODE, COLUMN4 COLUMNA, '5' COLUMNID
                FROM TVALUELEVELCUSTOM
               WHERE REPORT = P_REPORT AND SESSIONID = p_session) a,
             ttypecolumnreport b
       WHERE a.CODE = pp_rowsid AND b.report = a.report AND
             b.columnid = a.columNid
       ORDER BY A.columnid;

    w_line      varchar2(4000);
    w_columna   varchar2(4000);
    w_file      UTL_FILE.FILE_TYPE;
    w_obtfile   boolean;
    w_directory varchar2(100);
    lnNot_open EXCEPTION;
  BEGIN
    w_directory := PKG_LEGALREPORT3.to_directory();
    w_file      := UTL_FILE.FOPEN(w_directory, p_report, 'w');
    w_obtfile   := UTL_FILE.IS_OPEN(w_file);
    IF w_obtfile = TRUE THEN
      delete log_batch;
      commit;
      FOR cur_row IN rows LOOP
        w_line := null;

        FOR cur_col IN column(cur_row.rowsid) LOOP
          w_columna := PKG_LEGALREPORT3.to_format(cur_col.COLUMNA,
                                                  cur_col.columntyp,
                                                  cur_col.columnlen,
                                                  cur_col.filling,
                                                  cur_col.concaten);
          w_line    := w_line || w_columna;
        END LOOP;
        UTL_FILE.PUT(w_file, w_line);
        UTL_FILE.NEW_LINE(w_file, 1);
      END LOOP;
    ELSE
      RAISE lnNot_open;
    END IF;
    UTL_FILE.FCLOSE(w_file);

  EXCEPTION
    WHEN lnNot_open THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR: lnNot_open');
    WHEN NO_DATA_FOUND THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR: no data found');
    WHEN value_error THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR: value error');
    WHEN UTL_FILE.INVALID_FILEHANDLE THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR: invalid file handle');
    WHEN UTL_FILE.INVALID_OPERATION THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR: invalid operation');
    WHEN UTL_FILE.READ_ERROR THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR: file read');
    WHEN UTL_FILE.WRITE_ERROR THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR: write error');
    WHEN UTL_FILE.INVALID_PATH THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900,
                              'ERROR: invalid path' || ' ' || w_directory);
    WHEN UTL_FILE.INVALID_MODE THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR: invalid mode');
  END;
  FUNCTION GARANTE_SOLIDARIO(p_cliente IN NUMBER, p_tiporel IN NUMBER)
    RETURN CHAR IS
    existe CHAR;

  BEGIN
    BEGIN
      SELECT DISTINCT ('S')
        INTO existe
        FROM tcli_clicta
       where tcli_clicta.clc_codcli = p_cliente and
             tcli_clicta.clc_tiporel = p_tiporel;

    EXCEPTION
      when no_data_found then
        existe := 'N';
    END;
    RETURN existe;
  END GARANTE_SOLIDARIO;

  procedure TIP_GARANTIA(modulo    IN NUMBER,
                         credito   IN NUMBER,
                         cliente   IN NUMBER,
                         numerogar in number,
                         tipo_gar  OUT VARCHAR2) IS

  BEGIN

    BEGIN
      SELECT substr(codeiso, 1, 2)
        INTO tipo_gar
        FROM tcli_opergaran, tgar_garantias, tgen_desctabla
       WHERE ogr_cliente = gar_codcli and ogr_numgaran = gar_numero and
             des_codtab = 33 and des_codigo = gar_tipgar and
             ogr_mod = modulo and OGR_OPERACION = credito and
             ogr_cliente = cliente and ogr_numgaran = numerogar and
             rownum = 1;

    EXCEPTION
      WHEN no_data_found THEN
        tipo_gar := NULL;
    END;
  END TIP_GARANTIA;
  FUNCTION TIPO_VALORES(p_tipbien IN NUMBER) RETURN VARCHAR2 IS
    p_tipvalor VARCHAR2(8);

  BEGIN
    BEGIN
      SELECT codeiso
        INTO p_tipvalor
        FROM tgen_desctabla
       WHERE des_codtab = 22 and des_codigo = p_tipbien;

    EXCEPTION
      WHEN no_data_found THEN
        p_tipvalor := NULL;

    END;
    RETURN p_tipvalor;
  END tipo_valores;


  PROCEDURE de21(p_report       in varchar2,
                 p_session      in number,
                 p_sucursal     in number,
                 p_datefrom     in date,
                 p_fecha        in date,
                 p_tipocredito1 in number,
                 p_tipocredito2 in number,
                 p_tipocredito3 in number,
                 p_montocompara in number,
                 p_decimales    in number) IS
    --<<HD 8164
    vlv_cuenta varchar2(30);
    -->>HD 8164
    CUOTAS    NUMBER;
    DIASV     NUMBER;
    FECHAI    DATE;
    FECHAF    DATE;
    INICIO    NUMBER;
    final     number;
    S1        NUMBER;
    saldo     number;
    columna   varchar2(500);
    v_sql     varchar2(1000);
    SALIDA    VARCHAR2(20);
    REPORT    VARCHAR2(50);
    errtext   VARCHAR2(100);
    vlv_sigue varchar2(1);
    --CAPITAL NORMAL
    cursor a is
      select acr_credito,
             acr_fecha,
             acr_capred,
             acr_capnogi,
             acr_capvencido,
             acr_diasv,
             acr_intacum,
             acr_intactven,
             acr_intacteje,
             ACR_INTACUMDRJ,
             pre_pro,
             pre_tip,
             pre_mod,
             PRE_CREDITO,
             PRE_BASEMES,
             pre_moneda,
             PRE_TIPOCUO,
             PRE_FECVEN,
             pre_fecemi,
             pre_sucursal,
             nvl(acr_diasint,0)LCA_DIASMORAINT,
             nvl(acr_diasv,0) LCA_DIASMORACAP,
			 LCA_DEMANDAJUNDICIAL,
             ACR_INTACTJUD,
             lca_contingente acr_contingente,
             (select pd.pdt_valordir from tpre_prevdettipos pd
               where    pd.pdt_fecha= lca_fecha
		         and pd.pdt_tipo=lca_tipo
		         and pd.pdt_mod = lca_mod
		         and pd.pdt_cuenta= lca_cuenta
		         and pd.pdt_tipoprev = 9 ) prov_constituidacap,
             (select pd.pdt_valordir from tpre_prevdettipos pd
               where    pd.pdt_fecha= lca_fecha
		         and pd.pdt_tipo=lca_tipo
		         and pd.pdt_mod = lca_mod
		         and pd.pdt_cuenta= lca_cuenta
		         and pd.pdt_tipoprev = 8 ) prov_constituidaint,
          DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'-',acr_intacum,0))) acr_intacumprov,		         		         
          DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'J',0,acr_intactven))) acr_intactvenprov,
          decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) acr_intacumreestprov,
          decode(nvl(b.acr_otrostatus,'-'),'R',b.acr_capred,0) acr_capreestructprov                        
        from tpre_accrual b, tpre_prestamos,tleg_operacioncalif
       where pre_mod = lca_mod
         and pre_credito = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito IN ('C','M','D')--mayores deudores
         and acr_credito = pre_credito
         and acr_fecha = p_fecha
		union
      select vis_numcue acr_credito,
             lca_fecha acr_fecha,
             lca_capital acr_capred,
             0 acr_capnogi,
             0 acr_capvencido,
             0 acr_diasv,
             lca_interes acr_intacum,
             0 acr_intactven,
             0 acr_intacteje,
             0 ACR_INTACUMDRJ,
             vis_pro pre_pro,
             vis_tip pre_tip,
             vis_mod pre_mod,
             vis_numcue PRE_CREDITO,
             360 PRE_BASEMES,
             vis_moneda pre_moneda,
             1 PRE_TIPOCUO,
             crd_fechavenc PRE_FECVEN,
             crd_fechavig pre_fecemi,
             vis_suc pre_sucursal,
             LCA_DIASMORAINT,
             LCA_DIASMORACAP,
			 LCA_DEMANDAJUNDICIAL,
             0 ACR_INTACTJUD,
             lca_contingente acr_contigente,
			             (select pd.pdt_valordir from tpre_prevdettipos pd
               where    pd.pdt_fecha= lca_fecha
		         and pd.pdt_tipo=lca_tipo
		         and pd.pdt_mod = lca_mod
		         and pd.pdt_cuenta= lca_cuenta
		         and pd.pdt_tipoprev = 9 )  prov_constituidacap,
             (select pd.pdt_valordir from tpre_prevdettipos pd
               where    pd.pdt_fecha= lca_fecha
		         and pd.pdt_tipo=lca_tipo
		         and pd.pdt_mod = lca_mod
		         and pd.pdt_cuenta= lca_cuenta
		         and pd.pdt_tipoprev = 8 ) prov_constituidaint,
          lca_interes acr_intacumprov,		         		         
         0 acr_intactvenprov,
          0 acr_intacumreestprov ,
          0 acr_capreestructprov                    		                              
        from --tcap_acract ,
             tcap_vista ,tcap_credvista ,tleg_operacioncalif
       where vis_mod = lca_mod
         and vis_numcue = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito in ('C','M','D')--mayores deudores
         and crd_numcue = vis_numcue
         --and aca_fecha = p_fecha
         --and aca_numcue = crd_numcue
         --and aca_tipocre = crd_tipocred
         --and aca_secuencia = crd_secuencia
         and crd_tipocred = 2
                       and  trunc(crd_fechautor) <= P_FECHA
					   and  crd_fechavenc >= P_FECHA
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
       order by pre_credito;
      --PARA CONTINGENCIA
    cursor a_contingencia is
      select vis_numcue acr_credito,
             lca_fecha acr_fecha,
             lca_contingente acr_capred,
             0 acr_capnogi,
             0 acr_capvencido,
             0 acr_diasv,
             lca_interes acr_intacum,
             0 acr_intactven,
             0 acr_intacteje,
             0 ACR_INTACUMDRJ,
             vis_pro pre_pro,
             vis_tip pre_tip,
             vis_mod pre_mod,
             vis_numcue PRE_CREDITO,
             360 PRE_BASEMES,
             vis_moneda pre_moneda,
             1 PRE_TIPOCUO,
             crd_fechavenc PRE_FECVEN,
             crd_fechavig pre_fecemi,
             vis_suc pre_sucursal,
             LCA_DIASMORAINT,
             LCA_DIASMORACAP,
			 LCA_DEMANDAJUNDICIAL,
             0 ACR_INTACTJUD
        from --tcap_acract ,
             tcap_vista ,tcap_credvista ,tleg_operacioncalif
       where vis_mod = lca_mod
         and vis_numcue = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito in ('C','M','D')--mayores deudores
         and crd_numcue = vis_numcue
         --and aca_fecha = p_fecha
         --and aca_numcue = crd_numcue
         --and aca_tipocre = crd_tipocred
         --and aca_secuencia = crd_secuencia
         and crd_tipocred = 2
                       and  trunc(crd_fechautor) <= P_FECHA
					   and  crd_fechavenc >= P_FECHA
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
         union
      select lca_cuenta acr_credito,
             lca_fecha acr_fecha,
             lca_contingente acr_capred,
             0 acr_capnogi,
             0 acr_capvencido,
             0 acr_diasv,
             lca_interes acr_intacum,
             0 acr_intactven,
             0 acr_intacteje,
             0 ACR_INTACUMDRJ,
             pro_pro pre_pro,
             pro_tip pre_tip,
             pro_mod pre_mod,
             pro_cue PRE_CREDITO,
             360 PRE_BASEMES,
             pro_mon pre_moneda,
             1 PRE_TIPOCUO,
             f_fechatrabajo PRE_FECVEN,
             f_fechatrabajo pre_fecemi,
             lca_suc pre_sucursal,
             LCA_DIASMORAINT,
             LCA_DIASMORACAP,
			 LCA_DEMANDAJUNDICIAL,
             0 ACR_INTACTJUD
        from tleg_operacioncalif,tgen_cuenta
       where lca_mod = pro_mod
         and lca_cuenta = pro_cue
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito in ('C','M','D')--mayores deudores
         --and aca_fecha = p_fecha
         --and aca_numcue = crd_numcue
         --and aca_tipocre = crd_tipocred
         --and aca_secuencia = crd_secuencia
         and lca_mod = 10
       order by pre_credito;
      --fin
    cursor b(modulo number, producto number, tipo number, moneda number) is
      select gdt_cuenta, GDT_CODGRU
        from tpre_funcgroupdet, tpre_funcgroup
       where GDT_MOD = modulo and GDT_PRO = producto and GDT_TIP = tipo and
             GDT_MON = moneda and --fng_modrpt = 21 and fng_trarpt = 6063 and
             fng_modrpt = 20 and fng_trarpt = 450 and --HD 8164
             fng_entrpt = 0 and fng_secrpt = 1 and
             GDT_CODGRU <> 45 and --para contingencia
             --gdt_mod = 6 and --HD 8164
             fng_cuenta = gdt_cuenta and GDT_MODRPT = fng_modrpt and
             GDT_TRARPT = fng_trarpt and fng_secrpt = gdt_secrpt --HD BDI 8164
       order by gdt_cuenta;
    cursor b_contingencia(modulo number, producto number, tipo number, moneda number) is
      select gdt_cuenta, GDT_CODGRU
        from tpre_funcgroupdet, tpre_funcgroup
       where GDT_MOD = modulo and GDT_PRO = producto and GDT_TIP = tipo and
             GDT_MON = moneda and --fng_modrpt = 21 and fng_trarpt = 6063 and
             fng_modrpt = 20 and fng_trarpt = 450 and --HD 8164
             fng_entrpt = 0 and fng_secrpt = 1 and
             GDT_CODGRU = 45 and --para contingencia
             --gdt_mod = 6 and --HD 8164
             fng_cuenta = gdt_cuenta and GDT_MODRPT = fng_modrpt and
             GDT_TRARPT = fng_trarpt and fng_secrpt = gdt_secrpt --HD BDI 8164
       order by gdt_cuenta;                                                  

			w_intvigente number:= 0;
		    w_int3090 number:=0;
			w_intacumreest number:=0;
            w_provreqint number:=0;
            w_porcentaje_req number:=0;
            w_provreqintvigente number:=0;
            w_provreqint3090 number:=0;
            w_porcentaje number:=0;

            w_yaprovision1 number:=0;
            w_yaprovision2 number:=0;
			w_yaprovision3 number:=0;            
			w_yaprovision4 number:=0;
			w_yaprovision5 number:=0;
			w_yaprovision6 number:=0;
			w_yaprovision7 number:=0;       
       
  begin
    REPORT := P_REPORT;
    IF (P_DATEFROM <= P_FECHA) AND (P_FECHA <= F_FECHATRABAJO) THEN
      dbms_output.put_line('en de21');
      pkg_legalreport.eraser(p_report, p_session);
      select TYPEOUT into salida from treportlegal where code = p_report;
      S1 := 1;
      for i in a loop
            w_yaprovision1 :=0;
            w_yaprovision2 :=0;
			w_yaprovision3 :=0;            
			w_yaprovision4 :=0;
			w_yaprovision5 :=0;
			w_yaprovision6 :=0;
			w_yaprovision7 :=0;       

        for j in b(i.pre_mod, i.pre_pro, i.pre_tip, i.pre_moneda) loop
          --HD8164 BDI
          vlv_sigue := 'S';--fun_gen_ReeLeg(j.gdt_cuenta, i.pre_credito, p_fecha); no se desplega el valor cuando esta reestructurado y pasa a vencido
          if vlv_sigue = 'S' then
            --HD8164

            select cgp_columna
              into columna
              from tpre_colgroups
             where CGP_CODIGO = j.GDT_CODGRU;
            --<<HD8164 3
            /*if j.GDT_CODGRU=18 then
              select cgp_columna
                into columna
                from tpre_colgroups
               where CGP_CODIGO = 19;
            end if;*/
            -->>hd 8164

            V_SQL := ' SELECT SUM(' || columna || ')' ||
                     ' FROM TPRE_ACCRUAL,tpre_prestamos ' ||
                     ' WHERE ACR_CREDITO = PRE_CREDITO ' ||
                     ' AND ACR_FECHA  = ' || 'TO_DATE(' || '''' ||
                     TO_CHAR(p_FECHA, 'YYYY/MM/DD') || '''' || ',' ||
                     '''YYYY/MM/DD''' || ')' || ' AND ACR_CREDITO = ' ||
                     i.acr_CREDITO;
          if i.pre_mod = 6 then
            BEGIN
              EXECUTE IMMEDIATE V_SQL
                INTO Saldo;
            EXCEPTION
              WHEN OTHERS THEN
                RAISE_APPLICATION_ERROR(-20134,
                                        'Error calculando Saldo del Credito' ||
                                        sqlerrm);
            END;
          else
            --por sobregiros contratados
            if j.GDT_CODGRU = 96 then
              Saldo := I.ACR_CAPRED;
            elsif j.GDT_CODGRU = 45 then
              Saldo := I.ACR_contingente;
            else
            Saldo := i.acr_intacum;
            end if;
          end if;
            --<<HD 8164
            vlv_cuenta := fun_gen_cuenta(i.pre_credito,
                                         J.GDT_CUENTA,
                                         4,
                                         6,
                                         '2045001',
                                         p_fecha);
            -->>

            FECHAI := NULL;
            DIASV  := 0;
            if ABS(saldo) > 0 then
              IF I.ACR_CAPRED = saldo and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    --DIASV:=FECHAF-FECHAI;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;
                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  IF substr(vlv_cuenta,1,3) = '121' THEN
                     IF NVL(DIASv,0) > 30 THEN 
                        DIASv := 0;
                     END IF;
                  END IF;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS); 

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                end;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPNOGI = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final ;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    --DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO

                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, cuotas);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPVENCIDO = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 3;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA between inicio and final ;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    --DIASV:=FECHAF-FECHAI;
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF ABS(i.ACR_INTACUM) = ABS(saldo)  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1 ;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --        FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  IF substr(vlv_cuenta,1,5) = '12801' THEN
                     IF NVL(DIASv,0) > 30 THEN 
                        DIASv := 0;
                     END IF;
                  END IF;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_INTACTVEN = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --  DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;

                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF (i.ACR_INTACTEJE = saldo OR i.ACR_INTACUMDRJ = saldo ) and j.gdt_codgru<>18 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  --  SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF);
                  begin
                    select T.rde_inicio, T.rde_finaL
                      into inicio, final
                      from tgen_reprandet t
                     WHERE T.RDE_MODULO = 20 and
                           T.RDE_CODIGO =
                           (SELECT TREPORTLEGAL.RANGE
                              FROM TREPORTLEGAL
                             WHERE CODE = P_REPORT) -- HD 4701
                           and rde_seq = 3;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA >INICIO - 1;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    -- DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN > INICIO -1;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --             SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF||', FECHAI:'||FECHAI||', DIASV:'||DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              ----------------------------------------
              --<<HD 8164 BDI
              FECHAI := NULL;
              DIASV  := 0;
              IF NVL(i.ACR_CAPRED, 0) + NVL(i.ACR_CAPNOGI,0) + NVL(i.ACR_CAPVENCIDO, 0) = saldo
              AND  j.gdt_codgru=17 THEN

                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1;
                    END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;
                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    --DIASV:=FECHAF-FECHAI;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;
                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                end;
              END IF;
              -->>
              -----------------------------------------
              -----------------------------------------
              --<<HD 8164 2

              --IF NVL(i.ACR_INTACUM, 0)+NVL(i.ACR_INTACTVEN,0)+NVL(i.ACR_INTACTEJE,0) = saldo
              IF NVL(i.ACR_INTACTJUD,0) = saldo
              AND  j.gdt_codgru=18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --        FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;

              end if;
              -->>HD8164 2
       ---------------------------------------------------------inicio provision TC ---------------------------------------------------------
              --------------------------------------------------inicio provision constituida capital restruc -----------------------------------------
                --decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) acr_intacumreestprov,
                 --decode(nvl(b.acr_otrostatus,'-'),'R',b.acr_capred,0) acr_capreestructprov      
              --Inicio 2
              IF NVL(i.acr_capreestructprov,0) <> 0 and w_yaprovision1 =0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'C'
						   and gdt_codgru = 903  -- int vigente
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     i.acr_capreestructprov);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;
                            w_yaprovision1 :=1;


                  END IF;--fin 2                                
              --------------------------------------------------fin provision constituida capital restrust--------------------------------------------
				--------------------------------------------------Inicio provision constituida capital restrust------------------------------------------
              --Inicio 2
              IF NVL(i.acr_intacumreestprov,0) <> 0 and w_yaprovision2 =0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'C'
						   and gdt_codgru = 904  -- int vigente
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     i.acr_intacumreestprov);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;           
                            w_yaprovision2 :=1;

                  END IF;--fin 2 				
				
                --------------------------------------------------fin provision constituida capital restrust------------------------------------------
              --------------------------------------------------inicio PROVISION  constituida MAS DE 90     -------------------------------------------
              FECHAI := NULL;
              DIASV  := 0;                                                                       
              --IF (i.ACR_INTACTEJE = saldo OR i.ACR_INTACUMDRJ = saldo ) and j.gdt_codgru<>18 THEN              
              IF (i.ACR_INTACTEJE = saldo) and j.gdt_codgru<>18 and w_yaprovision3 =0 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  --  SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF);
                  begin
                    select T.rde_inicio, T.rde_finaL
                      into inicio, final
                      from tgen_reprandet t
                     WHERE T.RDE_MODULO = 20 and
                           T.RDE_CODIGO =
                           (SELECT TREPORTLEGAL.RANGE
                              FROM TREPORTLEGAL
                             WHERE CODE = P_REPORT) -- HD 4701
                           and rde_seq = 3;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA >INICIO - 1;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    -- DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN > INICIO -1;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --             SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF||', FECHAI:'||FECHAI||', DIASV:'||DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end; 
				     begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'C'
						   and gdt_codgru = 906  --LEGAL
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '12901101';
                  end ;                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;
			w_yaprovision3 :=1;            
                
              END IF;              
              --------------------------------------------------FIN PROVISION  constituida de MAS DE 90     -------------------------------------------              
              -->>HD8164 2
              --------------------------------------------------inicio PROVISION  constituida de legal     -------------------------------------------
              IF NVL(i.ACR_INTACTJUD,0) = saldo
              AND  j.gdt_codgru=18 and w_yaprovision4 =0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --        FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;    
                  
                     begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'C'
						   and gdt_codgru = 905  --LEGAL
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '12901101';
                  end ;   
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;
			w_yaprovision4 :=1;

              end if;
              --------------------------------------------------fin PROVISION  constituida de legal     -------------------------------------------
			                          
              --------------------------------------------------PROVISION  constituida de capital     -------------------------------------------
              --Inicio 1
              IF NVL(i.prov_constituidacap,0) <> 0 and w_yaprovision5 =0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'C'
						   and gdt_codgru = 900  --capital
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '12901101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     i.prov_constituidacap);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;   
                			w_yaprovision5 :=1;

                  END IF;--fin 1                  
                --fin de provision constituida de capital   
					w_intvigente:= bdi_promedio(i.pre_moneda,1,p_fecha)*(i.acr_intacumprov);
					w_int3090:= bdi_promedio(i.pre_moneda,1,p_fecha)*(i.acr_intactvenprov);
				    w_intacumreest:= bdi_promedio(i.pre_moneda,1,p_fecha)*(i.acr_intacumreestprov);
                    w_provreqint:=i.prov_constituidaint;
                    w_porcentaje:=0;
                if nvl(w_provreqint,0)<>0 then
					  begin
					   w_porcentaje_req:=(nvl(w_provreqint,0)*100)/(nvl(w_intvigente,0)+nvl(w_intacumreest,0)+nvl(w_int3090,0));
					  exception
					  	when others then
					  	w_porcentaje_req:=0;
					  end;
					  else
					  	w_porcentaje:=0;
			  end if;
					  begin
					  w_provreqintvigente:=((nvl(w_intvigente,0)+nvl(w_intacumreest,0))*w_porcentaje_req)/100;
					  w_provreqint3090:=((nvl(w_int3090,0))*w_porcentaje_req)/100;
					  exception
					  when others then
					  w_provreqintvigente:=0;
					  w_provreqint3090:=0;  	  
					  end;
                  --------------------------------------------prov interes vigente ------------------------------------------ 
              --Inicio 2
              IF NVL(w_provreqintvigente,0) <> 0 and w_yaprovision6 =0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'C'
						   and gdt_codgru = 901  -- int vigente
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     w_provreqintvigente);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;
                			w_yaprovision6 :=1;


                  END IF;--fin 2                  
                  ---------------------------------------------fin de prov vigente -------------------------------------------
					---------------------------------------------inicio de prov vencido -------------------------------------------                  
              --Inicio 3
              IF NVL(w_provreqint3090,0) <> 0 and w_yaprovision7 =0  THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'C'
						   and gdt_codgru = 902  -- int vencido
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     w_provreqint3090);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;           
                			w_yaprovision7 :=1;       
                  END IF;--fin 3					
					---------------------------------------------fin de prov vencido -------------------------------------------                  					              
              ---------------------------------------------------------fin provision TC ---------------------------------------------------------                             
            END IF;
          end if;
        end loop;
      end loop;
      commit;
      --contingencia
      for i in a_contingencia loop
        for j in b_contingencia(i.pre_mod, i.pre_pro, i.pre_tip, i.pre_moneda) loop
          --HD8164 BDI
          vlv_sigue := 'S';--fun_gen_ReeLeg(j.gdt_cuenta, i.pre_credito, p_fecha); no se desplega el valor cuando esta reestructurado y pasa a vencido
          if vlv_sigue = 'S' then
            --HD8164

            select cgp_columna
              into columna
              from tpre_colgroups
             where CGP_CODIGO = j.GDT_CODGRU;
            --<<HD8164 3
            /*if j.GDT_CODGRU=18 then
              select cgp_columna
                into columna
                from tpre_colgroups
               where CGP_CODIGO = 19;
            end if;*/
            -->>hd 8164

            V_SQL := ' SELECT SUM(' || columna || ')' ||
                     ' FROM TPRE_ACCRUAL,tpre_prestamos ' ||
                     ' WHERE ACR_CREDITO = PRE_CREDITO ' ||
                     ' AND ACR_FECHA  = ' || 'TO_DATE(' || '''' ||
                     TO_CHAR(p_FECHA, 'YYYY/MM/DD') || '''' || ',' ||
                     '''YYYY/MM/DD''' || ')' || ' AND ACR_CREDITO = ' ||
                     i.acr_CREDITO;
          if i.pre_mod = 6 then
            BEGIN
              EXECUTE IMMEDIATE V_SQL
                INTO Saldo;
            EXCEPTION
              WHEN OTHERS THEN
                RAISE_APPLICATION_ERROR(-20134,
                                        'Error calculando Saldo del Credito' ||
                                        sqlerrm);
            END;
          else
            --por sobregiros contratados
            if j.GDT_CODGRU = 45 then
              Saldo := I.ACR_CAPRED;
            else
            Saldo := i.acr_intacum;
            end if;
          end if;
            --<<HD 8164
            vlv_cuenta := fun_gen_cuenta(i.pre_credito,
                                         J.GDT_CUENTA,
                                         4,
                                         6,
                                         '2045001',
                                         p_fecha);
            -->>

            FECHAI := NULL;
            DIASV  := 0;
            if ABS(saldo) > 0 then
              IF I.ACR_CAPRED = saldo and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    --DIASV:=FECHAF-FECHAI;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;
                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--se quita el calculo de moneda ya esta en la calificacion
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                end;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPNOGI = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final ;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    --DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO

                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
								  saldo),--por doble calculo de me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, cuotas);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPVENCIDO = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 3;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA between inicio and final ;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    --DIASV:=FECHAF-FECHAI;
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble calculo de me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF ABS(i.ACR_INTACUM) = ABS(saldo)  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1 ;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --        FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
							      saldo),--por doble caluclo me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_INTACTVEN = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --  DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;

                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble calculo me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF (i.ACR_INTACTEJE = saldo OR i.ACR_INTACUMDRJ = saldo ) and j.gdt_codgru<>18 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  --  SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF);
                  begin
                    select T.rde_inicio, T.rde_finaL
                      into inicio, final
                      from tgen_reprandet t
                     WHERE T.RDE_MODULO = 20 and
                           T.RDE_CODIGO =
                           (SELECT TREPORTLEGAL.RANGE
                              FROM TREPORTLEGAL
                             WHERE CODE = P_REPORT) -- HD 4701
                           and rde_seq = 3;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA >INICIO - 1;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    -- DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN > INICIO -1;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --             SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF||', FECHAI:'||FECHAI||', DIASV:'||DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble calculo de me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;
              END IF;
              ----------------------------------------
              --<<HD 8164 BDI
              FECHAI := NULL;
              DIASV  := 0;
              IF NVL(i.ACR_CAPRED, 0) + NVL(i.ACR_CAPNOGI,0) + NVL(i.ACR_CAPVENCIDO, 0) = saldo
              AND  j.gdt_codgru=17 THEN

                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1;
                    END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;
                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    --DIASV:=FECHAF-FECHAI;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;
                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble calculo de me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                end;
              END IF;
              -->>
              -----------------------------------------
              -----------------------------------------
              --<<HD 8164 2

              --IF NVL(i.ACR_INTACUM, 0)+NVL(i.ACR_INTACTVEN,0)+NVL(i.ACR_INTACTEJE,0) = saldo
              IF NVL(i.ACR_INTACTJUD,0) = saldo
              AND  j.gdt_codgru=18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --        FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble calculo me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E')); 
                    
                  s1 := s1 + 1;
                END;

              end if;
              -->>HD8164 2

            END IF;
          end if;
        end loop;
      end loop;
      --fin de contigencia
      if salida = 'A' then
        pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
        --NULL;
      end if;
    Else
      errtext := 'Fechas no válidas';
      raise_application_error(-20901, errtext);
    End if;

  end;
  PROCEDURE de23(p_report       in varchar2,
                 p_session      in number,
                 p_sucursal     in number,
                 p_datefrom     in date,
                 p_fecha        in date,
                 p_tipocredito1 in number,
                 p_tipocredito2 in number,
                 p_tipocredito3 in number,
                 p_decimales    in number) IS
    --<<HD 8164
    vlv_cuenta varchar2(30);
    -->>HD 8164

    CUOTAS    NUMBER;
    DIASV     NUMBER;
    FECHAI    DATE;
    FECHAF    DATE;
    INICIO    NUMBER;
    final     number;
    S1        NUMBER;
    saldo     number;
    columna   varchar2(500);
    v_sql     varchar2(1000);
    SALIDA    VARCHAR2(20);
    REPORT    VARCHAR2(50);
    errtext   VARCHAR2(100);
    vlv_sigue varchar2(1);

    cursor a is
      select acr_credito,
             acr_fecha,
             acr_capred,
             acr_capnogi,
             acr_capvencido,
             acr_diasv,
             acr_intacum,
             acr_intactven,
             acr_intacteje,
             ACR_INTACUMDRJ,
             ACR_INTDIADRV,
             pre_pro,
             pre_tip,
             pre_mod,
             PRE_CREDITO,
             PRE_BASEMES,
             pre_moneda,
             PRE_TIPOCUO,
             PRE_FECVEN,
             pre_fecemi,
             pre_sucursal,
             LCA_DIASMORACAP,
             LCA_DIASMORAINT,
             LCA_DEMANDAJUNDICIAL,
             ACR_INTACTJUD,
           (select pd.pdt_valordir from tpre_prevdettipos pd
               where    pd.pdt_fecha= lca_fecha--add_months(lca_fecha,-1)
		         and pd.pdt_tipo=lca_tipo
		         and pd.pdt_mod = lca_mod
		         and pd.pdt_cuenta= lca_cuenta
		         and pd.pdt_tipoprev = 9 ) prov_constituidacap,
             (select pd.pdt_valordir from tpre_prevdettipos pd
               where    pd.pdt_fecha= lca_fecha --add_months(lca_fecha,-1) Alejandro - se toma la regquerida del mismo mes
		         and pd.pdt_tipo=lca_tipo
		         and pd.pdt_mod = lca_mod
		         and pd.pdt_cuenta= lca_cuenta
		         and pd.pdt_tipoprev = 8 ) prov_constituidaint,
          DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'-',acr_intacum,0))) acr_intacumprov,		         		         
          DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'J',0,acr_intactven))) acr_intactvenprov,
          decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) acr_intacumreestprov,
          decode(nvl(b.acr_otrostatus,'-'),'R',b.acr_capred,0) acr_capreestructprov                          
        from tpre_accrual B, tpre_prestamos,tleg_operacioncalif
       where pre_mod = lca_mod
         and pre_credito = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'O'--menores deudores
         and acr_credito = pre_credito
         and acr_fecha = p_fecha
       order by prE_credito;
    cursor b(modulo number, producto number, tipo number, moneda number) is
      select gdt_cuenta, GDT_CODGRU
        from tpre_funcgroupdet, tpre_funcgroup
       where GDT_MOD = modulo and GDT_PRO = producto and GDT_TIP = tipo and
             GDT_MON = moneda and --fng_modrpt = 21 and fng_trarpt = 6063 and
             fng_modrpt = 20 and fng_trarpt = 450 and --HD 8164
             fng_entrpt = 0 and fng_secrpt = 1 and gdt_mod = 6 and --HD 8164
             fng_cuenta = gdt_cuenta and GDT_MODRPT = fng_modrpt and
             GDT_TRARPT = fng_trarpt and fng_secrpt = gdt_secrpt --HD BDI 8164
       order by gdt_cuenta;
			w_intvigente number:= 0;
		    w_int3090 number:=0;
			w_intacumreest number:=0;
            w_provreqint number:=0;
            w_porcentaje_req number:=0;
            w_provreqintvigente number:=0;
            w_provreqint3090 number:=0;
            w_porcentaje number:=0;
            
            
            w_yaprovision1 number:=0;
            w_yaprovision2 number:=0;
			w_yaprovision3 number:=0;            
			w_yaprovision4 number:=0;
			w_yaprovision5 number:=0;
			w_yaprovision6 number:=0;
			w_yaprovision7 number:=0;       
       
  begin
    REPORT := P_REPORT;
    IF (P_DATEFROM <= P_FECHA) AND (P_FECHA <= F_FECHATRABAJO) THEN
      dbms_output.put_line('en de21');
      pkg_legalreport.eraser(p_report, p_session);
      select TYPEOUT into salida from treportlegal where code = p_report;
      S1 := 1;
      for i in a loop
        DIASV := 0;
            w_yaprovision1 :=0;
            w_yaprovision2 :=0;
			w_yaprovision3 :=0;            
			w_yaprovision4 :=0;
			w_yaprovision5 :=0;
			w_yaprovision6 :=0;
			w_yaprovision7 :=0;       

        for j in b(i.pre_mod, i.pre_pro, i.pre_tip, i.pre_moneda) loop
          --HD8164 BDI
          vlv_sigue := 'S';--fun_gen_ReeLeg(j.gdt_cuenta, i.pre_credito, p_fecha);
          if vlv_sigue = 'S' then
            --HD8164

            select cgp_columna
              into columna
              from tpre_colgroups
             where CGP_CODIGO = j.GDT_CODGRU;
            --<<HD8164 3
            /*if j.GDT_CODGRU=18 then
              select cgp_columna
                into columna
                from tpre_colgroups
               where CGP_CODIGO = 19;

            end if;*/
            -->>hd 8164
            V_SQL := ' SELECT SUM(' || columna || ')' ||
                     ' FROM TPRE_ACCRUAL,tpre_prestamos ' ||
                     ' WHERE ACR_CREDITO = PRE_CREDITO ' ||
                     ' AND ACR_FECHA  = ' || 'TO_DATE(' || '''' ||
                     TO_CHAR(p_FECHA, 'YYYY/MM/DD') || '''' || ',' ||
                     '''YYYY/MM/DD''' || ')' || ' AND ACR_CREDITO = ' ||
                     i.acr_CREDITO;
            BEGIN
              EXECUTE IMMEDIATE V_SQL
                INTO Saldo;
            EXCEPTION
              WHEN OTHERS THEN
                RAISE_APPLICATION_ERROR(-20134,
                                        'Error calculando Saldo del Credito' ||
                                        sqlerrm);
            END;
            --<<HD 8164
            vlv_cuenta := J.GDT_CUENTA;     
            vlv_cuenta:=fun_cuenta_del_2031(vlv_cuenta);
            -->>

            FECHAI := NULL;
            DIASV  := 0;
            if ABS(saldo) > 0 then
              IF I.ACR_CAPRED = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;            ---aquitoy
                                CUOTAS := 1 ;
                    END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_FECVEN)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --DIASV:=FECHAF-FECHAI;
                    --      FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  IF substr(vlv_cuenta,1,3) = '121' THEN
                     IF NVL(DIASv,0) > 30 THEN 
                        DIASv := 0;
                     END IF;
                  END IF;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                end;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPNOGI = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha AND CUO_CAPITAL <> 0
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final ;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    -- DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1 ;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;
                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, cuotas);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPVENCIDO = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 3;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA between inicio and final;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1 ;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    --DIASV:=FECHAF-FECHAI;
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF ABS(i.ACR_INTACUM) = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;

                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF; */
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --   DIASV:=FECHAF-FECHAi;
                    --            FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  IF substr(vlv_cuenta,1,5) = '12801' THEN
                     IF NVL(DIASv,0) > 30 THEN 
                        DIASv := 0 ;
                     END IF;
                  END IF;
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_INTACTVEN = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          ---AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    -- DIASV:=FECHAF-FECHAI;
                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF (i.ACR_INTACTEJE = saldo OR i.ACR_INTACUMDRJ = saldo ) and j.gdt_codgru<>18 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  --             SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF);
                  begin
                    select T.rde_inicio, T.rde_finaL
                      into inicio, final
                      from tgen_reprandet t
                     WHERE T.RDE_MODULO = 20 and
                           T.RDE_CODIGO =
                           (SELECT TREPORTLEGAL.RANGE
                              FROM TREPORTLEGAL
                             WHERE CODE = P_REPORT) -- HD 4701
                           and rde_seq = 3;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA >inicio;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN > inicio ;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF; */
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    -- DIASV:=FECHAF-FECHAI;
                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --               SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF||', FECHAI:'||FECHAI||', DIASV:'||DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              ----------------------------------------
              --<<HD 8164 BDI
              FECHAI := NULL;
              DIASV  := 0;
              IF NVL(i.ACR_CAPRED, 0) + NVL(i.ACR_CAPNOGI,0) + NVL(i.ACR_CAPVENCIDO, 0) = saldo
              AND  j.gdt_codgru=17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;            ---aquitoy
                                CUOTAS := 1 ;
                    END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;
                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_FECVEN)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --DIASV:=FECHAF-FECHAI;
                    --      FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                end;
              END IF;
              -->>
              -----------------------------------------
              --<<HD 8164 2
              --IF NVL(i.ACR_INTACUM, 0)+NVL(i.ACR_INTACTVEN,0)+NVL(i.ACR_INTACTEJE,0) = saldo    MAL! SE CORRIGE OLL,
              --             ACR_INTACTJUD
              IF NVL(i.ACR_INTACTJUD,0) = saldo
              AND  j.gdt_codgru=18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;

                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --   DIASV:=FECHAF-FECHAi;
                    --            FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);
                  s1 := s1 + 1;
                END;

              end if;
              -->>HD8164 2
---------------------------------------------------------inicio provision TC ---------------------------------------------------------
              --------------------------------------------------inicio provision constituida capital restruc -----------------------------------------
                --decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) acr_intacumreestprov,
                 --decode(nvl(b.acr_otrostatus,'-'),'R',b.acr_capred,0) acr_capreestructprov      
              --Inicio 2
              IF NVL(i.acr_capreestructprov,0) <> 0 and w_yaprovision1 = 0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'O'
						   and gdt_codgru = 903  -- int vigente
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     i.acr_capreestructprov);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;           
            w_yaprovision1 :=1;
                
                  END IF;--fin 2                                
              --------------------------------------------------fin provision constituida capital restrust--------------------------------------------
				--------------------------------------------------Inicio provision constituida capital restrust------------------------------------------
              --Inicio 2
              IF NVL(i.acr_intacumreestprov,0) <> 0 and w_yaprovision2 = 0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'O'
						   and gdt_codgru = 904  -- int vigente
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     i.acr_intacumreestprov);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;          
            w_yaprovision2 :=1;
                
                  END IF;--fin 2 				
				
                --------------------------------------------------fin provision constituida capital restrust------------------------------------------
              --------------------------------------------------inicio PROVISION  constituida MAS DE 90     -------------------------------------------
              FECHAI := NULL;
              DIASV  := 0;                                                                       
              --IF (i.ACR_INTACTEJE = saldo OR i.ACR_INTACUMDRJ = saldo ) and j.gdt_codgru<>18 THEN              
              IF (i.ACR_INTACTEJE = saldo) and j.gdt_codgru<>18 and w_yaprovision3 = 0 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  --  SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF);
                  begin
                    select T.rde_inicio, T.rde_finaL
                      into inicio, final
                      from tgen_reprandet t
                     WHERE T.RDE_MODULO = 20 and
                           T.RDE_CODIGO =
                           (SELECT TREPORTLEGAL.RANGE
                              FROM TREPORTLEGAL
                             WHERE CODE = P_REPORT) -- HD 4701
                           and rde_seq = 3;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA >INICIO - 1;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    -- DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN > INICIO -1;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --             SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF||', FECHAI:'||FECHAI||', DIASV:'||DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end; 
				     begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'O'
						   and gdt_codgru = 906  --LEGAL
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '12901101';
                  end ;                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;           
                			w_yaprovision3 :=1;            

              END IF;              
              --------------------------------------------------FIN PROVISION  constituida de MAS DE 90     -------------------------------------------              
              -->>HD8164 2
              --------------------------------------------------inicio PROVISION  constituida de legal     -------------------------------------------
              IF NVL(i.ACR_INTACTJUD,0) = saldo
              AND  j.gdt_codgru=18 and w_yaprovision4 = 0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --        FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;    
                  
                     begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'O'
						   and gdt_codgru = 905  --LEGAL
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '12901101';
                  end ;   
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;
                			w_yaprovision4 :=1;


              end if;
              --------------------------------------------------fin PROVISION  constituida de legal     -------------------------------------------
			                          
              --------------------------------------------------PROVISION  constituida de capital     -------------------------------------------
              --Inicio 1
              IF NVL(i.prov_constituidacap,0) <> 0 and w_yaprovision5 = 0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'O'
						   and gdt_codgru = 900  --capital
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '12901101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,           
                     i.prov_constituidacap);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;                          
			w_yaprovision5 :=1;
                
                  END IF;--fin 1                  
                --fin de provision constituida de capital   
					w_intvigente:= bdi_promedio(i.pre_moneda,1,p_fecha)*(i.acr_intacumprov);
					w_int3090:= bdi_promedio(i.pre_moneda,1,p_fecha)*(i.acr_intactvenprov);
				    w_intacumreest:= bdi_promedio(i.pre_moneda,1,p_fecha)*(i.acr_intacumreestprov);
                    w_provreqint:=i.prov_constituidaint;
                	  	w_porcentaje:=0;
                if nvl(w_provreqint,0)<>0 then
					  begin
					   w_porcentaje_req:=(nvl(w_provreqint,0)*100)/(nvl(w_intvigente,0)+nvl(w_intacumreest,0)+nvl(w_int3090,0));
					  exception
					  	when others then
					  	w_porcentaje_req:=0;
					  end;
					  else
					  	w_porcentaje:=0;
			  end if;
					  begin
					  w_provreqintvigente:=((nvl(w_intvigente,0)+nvl(w_intacumreest,0))*w_porcentaje_req)/100;
					  w_provreqint3090:=((nvl(w_int3090,0))*w_porcentaje_req)/100;
					  exception
					  when others then
					  w_provreqintvigente:=0;
					  w_provreqint3090:=0;  	  
					  end;
                  --------------------------------------------prov interes vigente ------------------------------------------ 
              --Inicio 2
              IF NVL(w_provreqintvigente,0) <> 0 and w_yaprovision6 = 0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'O'
						   and gdt_codgru = 901  -- int vigente
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     w_provreqintvigente);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;          
			w_yaprovision6 :=1;

                
                  END IF;--fin 2                  
                  ---------------------------------------------fin de prov vigente -------------------------------------------
					---------------------------------------------inicio de prov vencido -------------------------------------------                  
              --Inicio 3
              IF NVL(w_provreqint3090,0) <> 0 and w_yaprovision7 = 0THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'O'
						   and gdt_codgru = 902  -- int vencido
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     w_provreqint3090);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;
                			w_yaprovision7 :=1;       
                  END IF;--fin 3					
					---------------------------------------------fin de prov vencido -------------------------------------------                  					              
              ---------------------------------------------------------fin provision TC ---------------------------------------------------------                                           
            END IF;
          end if;
        end loop;
      end loop;
      commit;
      if salida = 'A' then
        pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
      end if;
    Else
      errtext := 'Fechas no válidas';
      raise_application_error(-20901, errtext);
    End if;
  end;
  PROCEDURE de25(p_report       in varchar2,
                 p_session      in number,
                 p_sucursal     in number,
                 p_datefrom     in date,
                 p_fecha        in date,
                 p_tipocredito1 in number,
                 p_tipocredito2 in number,
                 p_tipocredito3 in number,
                 p_decimales    in number) IS
    --<<HD 8164
    vlv_cuenta varchar2(30);
    -->>HD 8164
    CUOTAS    NUMBER;
    DIASV     NUMBER;
    FECHAI    DATE;
    FECHAF    DATE;
    INICIO    NUMBER;
    final     number;
    S1        NUMBER;
    saldo     number;
    columna   varchar2(500);
    v_sql     varchar2(1000);
    SALIDA    VARCHAR2(20);
    REPORT    VARCHAR2(50);
    errtext   VARCHAR2(100);
    vlv_sigue varchar2(1);

    cursor a is
      select acr_credito,
             acr_fecha,
             acr_capred,
             acr_capnogi,
             acr_capvencido,
             acr_diasv,
             acr_intacum,
             acr_intactven,
             acr_intacteje,
             ACR_INTACUMDRJ,
             pre_pro,
             pre_tip,
             pre_mod,
             PRE_CREDITO,
             PRE_BASEMES,
             pre_moneda,
             PRE_TIPOCUO,
             PRE_FECVEN,
             pre_fecemi,
             pre_sucursal,
             LCA_DIASMORACAP,
             LCA_DIASMORAINT,
             LCA_DEMANDAJUNDICIAL,
             ACR_INTACTJUD,
           (select pd.pdt_valordir from tpre_prevdettipos pd
               where    pd.pdt_fecha= lca_fecha
		         and pd.pdt_tipo=lca_tipo
		         and pd.pdt_mod = lca_mod
		         and pd.pdt_cuenta= lca_cuenta
		         and pd.pdt_tipoprev = 9 ) prov_constituidacap,
             (select pd.pdt_valordir from tpre_prevdettipos pd
               where    pd.pdt_fecha= lca_fecha
		         and pd.pdt_tipo=lca_tipo
		         and pd.pdt_mod = lca_mod
		         and pd.pdt_cuenta= lca_cuenta
		         and pd.pdt_tipoprev = 8 ) prov_constituidaint,
          DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'-',acr_intacum,0))) acr_intacumprov,		         		         
          DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'J',0,acr_intactven))) acr_intactvenprov,
          decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) acr_intacumreestprov,
          decode(nvl(b.acr_otrostatus,'-'),'R',b.acr_capred,0) acr_capreestructprov                          
        from tpre_accrual b, tpre_prestamos,tleg_operacioncalif
       where pre_mod = lca_mod
         and pre_credito = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'H'--hipo
         and acr_credito = pre_credito
         and acr_fecha = p_fecha
         --and acr_credito = 6080001036
       order by prE_credito;
    cursor b(modulo number, producto number, tipo number, moneda number) is
      select gdt_cuenta, GDT_CODGRU
        from tpre_funcgroupdet, tpre_funcgroup
       where GDT_MOD = modulo and GDT_PRO = producto and GDT_TIP = tipo and
             GDT_MON = moneda and --fng_modrpt = 21 and fng_trarpt = 6063 and
             fng_modrpt = 20 and fng_trarpt = 450 and --HD 8164
             fng_entrpt = 0 and fng_secrpt = 1 and gdt_mod = 6 and --HD 8164
             fng_cuenta = gdt_cuenta and GDT_MODRPT = fng_modrpt and
             GDT_TRARPT = fng_trarpt and fng_secrpt = gdt_secrpt --HD BDI 8164
       order by gdt_cuenta;
			w_intvigente number:= 0;
		    w_int3090 number:=0;
			w_intacumreest number:=0;
            w_provreqint number:=0;
            w_porcentaje_req number:=0;
            w_provreqintvigente number:=0;
            w_provreqint3090 number:=0;
            w_porcentaje number:=0;
            
            w_yaprovision1 number:=0;
            w_yaprovision2 number:=0;
			w_yaprovision3 number:=0;            
			w_yaprovision4 number:=0;
			w_yaprovision5 number:=0;
			w_yaprovision6 number:=0;
			w_yaprovision7 number:=0;       
  begin
    REPORT := P_REPORT;
    IF (P_DATEFROM <= P_FECHA) AND (P_FECHA <= F_FECHATRABAJO) THEN
      dbms_output.put_line('en de21');
      pkg_legalreport.eraser(p_report, p_session);
      select TYPEOUT into salida from treportlegal where code = p_report;
      S1 := 1;
      for i in a loop
        DIASV := 0; --OLL 23 Julio
           w_yaprovision1 :=0;
            w_yaprovision2 :=0;
			w_yaprovision3 :=0;            
			w_yaprovision4 :=0;
			w_yaprovision5 :=0;
			w_yaprovision6 :=0;
			w_yaprovision7 :=0;       

        for j in b(i.pre_mod, i.pre_pro, i.pre_tip, i.pre_moneda) loop
          --HD8164 BDI
                dbms_output.put_line('en de21:'||j.gdt_cuenta);
          vlv_sigue := 'S';--fun_gen_ReeLeg(j.gdt_cuenta, i.pre_credito, p_fecha);
          dbms_output.put_line('en de25:'||j.gdt_cuenta||' vlv_sigue:'||vlv_sigue);
          if vlv_sigue = 'S' then
            --HD8164

            select cgp_columna
              into columna
              from tpre_colgroups
             where CGP_CODIGO = j.GDT_CODGRU;
          dbms_output.put_line('en de25:'||j.gdt_cuenta||' columna:'||columna);
            --<<HD8164 3
            /*if j.GDT_CODGRU=18 then
              select cgp_columna
                into columna
                from tpre_colgroups
               where CGP_CODIGO = 19;
            end if;*/
            -->>hd 8164


            V_SQL := ' SELECT SUM(' || columna || ')' ||
                     ' FROM TPRE_ACCRUAL,tpre_prestamos ' ||
                     ' WHERE ACR_CREDITO = PRE_CREDITO ' ||
                     ' AND ACR_FECHA  = ' || 'TO_DATE(' || '''' ||
                     TO_CHAR(p_FECHA, 'YYYY/MM/DD') || '''' || ',' ||
                     '''YYYY/MM/DD''' || ')' || ' AND ACR_CREDITO = ' ||
                     i.acr_CREDITO;
            BEGIN
              EXECUTE IMMEDIATE V_SQL
                INTO Saldo;
            EXCEPTION
              WHEN OTHERS THEN
                RAISE_APPLICATION_ERROR(-20134,
                                        'Error calculando Saldo del Credito' ||
                                        sqlerrm);
            END;
            dbms_output.put_line('en de25:'||j.gdt_cuenta||' Saldo:'||Saldo);
            --<<HD 8164
            --vlv_cuenta:=fun_gen_cuenta(i.pre_credito,J.GDT_CUENTA,4,6,'2045001');
            vlv_cuenta := J.GDT_CUENTA;   
            vlv_cuenta:=fun_cuenta_del_2031(vlv_cuenta);
            -->>

            FECHAI := NULL;
            DIASV  := 0;
            if ABS(saldo) > 0 then
              IF I.ACR_CAPRED = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha AND CUO_CAPITAL <> 0
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           numero_dias(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    --NO LE VEO NECESARIO ESTA PARTE OLL
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1;
                    END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;


                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;

                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA
                            --AND nvl(LET_FECCONTAB,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                             AND nvl(LET_FECCONTAB,
                                     to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final
                             AND
                             numero_dias(p_fecha + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                           SELECT MAX(LET_FECVEN)
                           INTO FECHAI
                           FROM TPRE_LETRAS
                           WHERE LET_CREDITO=I.ACR_CREDITO
                           AND LET_FECVEN<P_FECHA;
                                   --AND NVL(LET_VALOR,0) <> 0;
                                   CUOTAS := 1;
                      END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;


                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS

                    --DIASV:=FECHAF-FECHAI;
                    --ANTES ESTABA FECHAF
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  IF substr(vlv_cuenta,1,3) = '121' THEN
                     IF NVL(DIASv,0) > 30 THEN 
                        DIASv := 0;
                     END IF;
                  END IF;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                end;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPNOGI = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha AND cuo_capital <> 0
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final ;
                           AND
                           numero_dias(p_fecha + 1,
                                       cuo_fecha,
                                       i.pre_basemes) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                         SELECT MAX(CUO_FECHA)
                         INTO FECHAI
                         FROM TPRE_CUOTAS
                         WHERE CUO_CREDITO=I.ACR_CREDITO
                         AND CUO_FECHA<P_FECHA;
                                 --AND NVL(CUO_CAPITAL,0) <> 0;
                                 CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA
                            --AND nvl(LET_FECCONTAB,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                             AND nvl(LET_FECCONTAB,
                                     to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             numero_dias(p_fecha + 1,
                                         LET_fecVEN,
                                         i.pre_basemes) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;


                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    -- DIASV:=FECHAF-FECHAI;
                    --FECHAS_FORMULA(FECHAF + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, cuotas);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPVENCIDO = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 3;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA between inicio and final ;
                           AND CUO_CAPITAL <> 0 AND
                           numero_dias(p_fecha + 1,
                                       cuo_fecha,
                                       i.pre_basemes) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0 ;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                         SELECT MAX(CUO_FECHA)
                         INTO FECHAI
                         FROM TPRE_CUOTAS
                         WHERE CUO_CREDITO=I.ACR_CREDITO
                         AND CUO_FECHA<P_FECHA;
                                 --AND NVL(CUO_CAPITAL,0) <> 0;
                                 CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA
                            --AND nvl(LET_FECCONTAB,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                             AND nvl(LET_FECCONTAB,
                                     to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             numero_dias(p_fecha + 1,
                                         LET_fecVEN,
                                         i.pre_basemes) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  --AND NVL(LET_VALOR,0) <> 0;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;


                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    --DIASV:=FECHAF-FECHAI;
                    --FECHAS_FORMULA(FECHAF + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF ABS(i.ACR_INTACUM) = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           numero_dias(p_fecha + 1,
                                       cuo_fecha,
                                       i.pre_basemes) BETWEEN inicio AND
                           final;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA
                            --AND nvl(LET_FECCONTAB,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                             AND nvl(LET_FECCONTAB,
                                     to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             numero_dias(p_fecha + 1,
                                         LET_fecVEN,
                                         i.pre_basemes) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF; */
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;


                    IF FECHAI IS NULL THEN
                      --FECHAI:=I.PRE_FECEMI;
                      FECHAI := P_FECHA;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --FECHAS_FORMULA(FECHAF + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  IF substr(vlv_cuenta,1,5) = '12801' THEN
                     IF NVL(DIASv,0) > 30 THEN 
                        DIASv := 0;
                     END IF;
                  END IF;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_INTACTVEN = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           numero_dias(p_fecha + 1,
                                       cuo_fecha,
                                       i.pre_basemes) BETWEEN inicio AND
                           final;
                    --DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS :=1;
                    END IF;   */
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA
                            --AND nvl(LET_FECCONTAB,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                             AND nvl(LET_FECCONTAB,
                                     to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             numero_dias(p_fecha + 1,
                                         let_fecven,
                                         i.pre_basemes) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;


                    IF FECHAI IS NULL THEN
                      --FECHAI:=I.PRE_FECEMI;
                      FECHAI := P_FECHA;
                    END IF;

                    --FECHAS_FORMULA(FECHAF + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF (i.ACR_INTACTEJE = saldo OR i.ACR_INTACUMDRJ = saldo ) and j.gdt_codgru<>18 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  --             SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF);
                  begin
                    select T.rde_inicio, T.rde_finaL
                      into inicio, final
                      from tgen_reprandet t
                     WHERE T.RDE_MODULO = 20 and
                           T.RDE_CODIGO =
                           (SELECT TREPORTLEGAL.RANGE
                              FROM TREPORTLEGAL
                             WHERE CODE = P_REPORT) -- HD 4701
                           and rde_seq = 3;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA > INICIO - 1;
                           AND numero_dias(p_fecha + 1,
                                           cuo_fecha,
                                           i.pre_basemes) > INICIO - 1;

                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS :=1;
                    END IF;   */
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA
                            --AND nvl(LET_FECCONTAB,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                             AND nvl(LET_FECCONTAB,
                                     to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             numero_dias(p_fecha + 1,
                                         let_fecven,
                                         i.pre_basemes) BETWEEN inicio AND
                             final;

                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;


                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --DIASV:=FECHAF-FECHAI;
                    --FECHAS_FORMULA(FECHAF + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --                   FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --               SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF||', FECHAI:'||FECHAI||', DIASV:'||DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              ----------------------------------------
              --<<HD 8164 BDI
              FECHAI := NULL;
              DIASV  := 0;
              IF NVL(i.ACR_CAPRED, 0) + NVL(i.ACR_CAPNOGI,0) + NVL(i.ACR_CAPVENCIDO, 0) = saldo
              AND  j.gdt_codgru=17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha AND CUO_CAPITAL <> 0
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           numero_dias(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    --AND NVL(CUO_CAPITAL,0) <> 0;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    --NO LE VEO NECESARIO ESTA PARTE OLL
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                                --AND NVL(CUO_CAPITAL,0) <> 0;
                                CUOTAS := 1;
                    END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;

                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA
                            --AND nvl(LET_FECCONTAB,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                             AND nvl(LET_FECCONTAB,
                                     to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final
                             AND
                             numero_dias(p_fecha + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      --AND NVL(LET_VALOR,0) <> 0;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                           SELECT MAX(LET_FECVEN)
                           INTO FECHAI
                           FROM TPRE_LETRAS
                           WHERE LET_CREDITO=I.ACR_CREDITO
                           AND LET_FECVEN<P_FECHA;
                                   --AND NVL(LET_VALOR,0) <> 0;
                                   CUOTAS := 1;
                      END IF;*/
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS

                    --DIASV:=FECHAF-FECHAI;
                    --ANTES ESTABA FECHAF
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                end;
              END IF;
              -->>
              -----------------------------------------
              -----------------------------------------
              --<<HD 8164 2
                          dbms_output.put_line('en de25:'||j.gdt_cuenta||' Saldo:'||Saldo);
              --IF NVL(i.ACR_INTACUM, 0)+NVL(i.ACR_INTACTVEN,0)+NVL(i.ACR_INTACTEJE,0) = saldo
              IF NVL(i.ACR_INTACTJUD,0) = saldo
              AND  j.gdt_codgru=18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           numero_dias(p_fecha + 1,
                                       cuo_fecha,
                                       i.pre_basemes) BETWEEN inicio AND
                           final;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN < P_FECHA
                            --AND nvl(LET_FECCONTAB,to_date('2099/01/01','yyyy/mm/dd')) > p_fecha
                             AND nvl(LET_FECCONTAB,
                                     to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             numero_dias(p_fecha + 1,
                                         LET_fecVEN,
                                         i.pre_basemes) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF; */
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      --FECHAI:=I.PRE_FECEMI;
                      FECHAI := P_FECHA;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --FECHAS_FORMULA(FECHAF + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;

                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(bdi_promedio(i.pre_moneda,i.pre_sucursal,p_fecha)*saldo, p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);
                  s1 := s1 + 1;
                END;
              end if;
              -->>HD8164 2
---------------------------------------------------------inicio provision TC ---------------------------------------------------------
              --------------------------------------------------inicio provision constituida capital restruc -----------------------------------------
                --decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) acr_intacumreestprov,
                 --decode(nvl(b.acr_otrostatus,'-'),'R',b.acr_capred,0) acr_capreestructprov      
              --Inicio 2
              IF NVL(i.acr_capreestructprov,0) <> 0 and w_yaprovision1 = 0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'H'
						   and gdt_codgru = 903  -- int vigente
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     i.acr_capreestructprov);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;
                           w_yaprovision1 :=1;

                  END IF;--fin 2                                
              --------------------------------------------------fin provision constituida capital restrust--------------------------------------------
				--------------------------------------------------Inicio provision constituida capital restrust------------------------------------------
              --Inicio 2
              IF NVL(i.acr_intacumreestprov,0) <> 0 and w_yaprovision2 = 0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'H'
						   and gdt_codgru = 904  -- int vigente
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     i.acr_intacumreestprov);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;
                            w_yaprovision2 :=1;

                  END IF;--fin 2 				
				
                --------------------------------------------------fin provision constituida capital restrust------------------------------------------
              --------------------------------------------------inicio PROVISION  constituida MAS DE 90     -------------------------------------------
              FECHAI := NULL;
              DIASV  := 0;                                                                       
              --IF (i.ACR_INTACTEJE = saldo OR i.ACR_INTACUMDRJ = saldo ) and j.gdt_codgru<>18 THEN              
              IF (i.ACR_INTACTEJE = saldo) and j.gdt_codgru<>18 and w_yaprovision3 = 0 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  --  SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF);
                  begin
                    select T.rde_inicio, T.rde_finaL
                      into inicio, final
                      from tgen_reprandet t
                     WHERE T.RDE_MODULO = 20 and
                           T.RDE_CODIGO =
                           (SELECT TREPORTLEGAL.RANGE
                              FROM TREPORTLEGAL
                             WHERE CODE = P_REPORT) -- HD 4701
                           and rde_seq = 3;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA >INICIO - 1;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    -- DIASV:=FECHAF-FECHAI;
                    ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                    --6010001462 al 2003/09/23 que salio 143 dias en vigente
                    /*IF FECHAI IS NULL THEN
                        SELECT MAX(CUO_FECHA)
                        INTO FECHAI
                        FROM TPRE_CUOTAS
                        WHERE CUO_CREDITO=I.ACR_CREDITO
                        AND CUO_FECHA<P_FECHA;
                        CUOTAS := 1;
                    END IF;*/
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN > INICIO -1;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                      ---se aumenta para no tomar en cuenta pre_fecemi tal como sucedu=io con el
                      --6010001462 al 2003/09/23 que salio 143 dias en vigente
                      /*IF FECHAI IS NULL THEN
                          SELECT MAX(LET_FECVEN)
                          INTO FECHAI
                          FROM TPRE_LETRAS
                          WHERE LET_CREDITO=I.ACR_CREDITO
                          AND LET_FECVEN<P_FECHA;
                                  CUOTAS := 1;
                      END IF;*/
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --             SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF||', FECHAI:'||FECHAI||', DIASV:'||DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end; 
				     begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'H'
						   and gdt_codgru = 906  --LEGAL
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '12901101';
                  end ;                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;                          
			w_yaprovision3 :=1;            
                
              END IF;              
              --------------------------------------------------FIN PROVISION  constituida de MAS DE 90     -------------------------------------------              
              -->>HD8164 2
              --------------------------------------------------inicio PROVISION  constituida de legal     -------------------------------------------
              IF NVL(i.ACR_INTACTJUD,0) = saldo
              AND  j.gdt_codgru=18 and w_yaprovision4 = 0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --        FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;    
                  
                     begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'H'
						   and gdt_codgru = 905  --LEGAL
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '12901101';
                  end ;   
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;
			w_yaprovision4 :=1;

              end if;
              --------------------------------------------------fin PROVISION  constituida de legal     -------------------------------------------
			                          
              --------------------------------------------------PROVISION  constituida de capital     -------------------------------------------
              --Inicio 1
              IF NVL(i.prov_constituidacap,0) <> 0 and w_yaprovision5 = 0  THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'H'
						   and gdt_codgru = 900  --capital
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '12901101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
					 i.prov_constituidacap);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;          
                			w_yaprovision5 :=1;
                  END IF;--fin 1                  
                --fin de provision constituida de capital   
					w_intvigente:= bdi_promedio(i.pre_moneda,1,p_fecha)*(i.acr_intacumprov);
					w_int3090:= bdi_promedio(i.pre_moneda,1,p_fecha)*(i.acr_intactvenprov);
				    w_intacumreest:= bdi_promedio(i.pre_moneda,1,p_fecha)*(i.acr_intacumreestprov);
                    w_provreqint:=i.prov_constituidaint;
                    	  	w_porcentaje:=0;
                if nvl(w_provreqint,0)<>0 then
					  begin
					   w_porcentaje_req:=(nvl(w_provreqint,0)*100)/(nvl(w_intvigente,0)+nvl(w_intacumreest,0)+nvl(w_int3090,0));
					  exception
					  	when others then
					  	w_porcentaje_req:=0;
					  end;
					  else
					  	w_porcentaje:=0;
			  end if;
					  begin
					  w_provreqintvigente:=((nvl(w_intvigente,0)+nvl(w_intacumreest,0))*w_porcentaje_req)/100;
					  w_provreqint3090:=((nvl(w_int3090,0))*w_porcentaje_req)/100;
					  exception
					  when others then
					  w_provreqintvigente:=0;
					  w_provreqint3090:=0;  	  
					  end;
                  --------------------------------------------prov interes vigente ------------------------------------------ 
              --Inicio 2
              IF NVL(w_provreqintvigente,0) <> 0  and w_yaprovision6 = 0  THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'H'
						   and gdt_codgru = 901  -- int vigente
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     w_provreqintvigente);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;                          
                			w_yaprovision6 :=1;

                  END IF;--fin 2                  
                  ---------------------------------------------fin de prov vigente -------------------------------------------
					---------------------------------------------inicio de prov vencido -------------------------------------------                  
              --Inicio 3
              IF NVL(w_provreqint3090,0) <> 0  and w_yaprovision7 = 0 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(trunc(CUO_FECPAG),to_DATE('21991231','yyyymmdd')) > p_fecha
                           AND
                           nvl(trunc(CUO_FECCONTAB),
                               to_DATE('21991231', 'yyyymmdd')) > p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                   
                  begin                      
						 select gdt_cuenta
						  into vlv_cuenta
						 from tdom_funcgroupdetprev
						 where gdt_tipocartera = 'H'
						   and gdt_codgru = 902  -- int vencido
						   and gdt_moneda = i.pre_moneda;
                  exception
                    when others then 
                     vlv_cuenta:= '129021010101';
                  end ;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     w_provreqint3090);
                     
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 7, S1, 7, decode(i.pre_moneda,0,'N','E'));

                  s1 := s1 + 1;
                END;                          
                			w_yaprovision7 :=1;       
                  END IF;--fin 3					
					---------------------------------------------fin de prov vencido -------------------------------------------                  					              
              ---------------------------------------------------------fin provision TC ---------------------------------------------------------                             
            END IF;
          end if;
        end loop;
      end loop;
      commit;
      if salida = 'A' then
        pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
      end if;
    Else
      errtext := 'Fechas no válidas';
      raise_application_error(-20901, errtext);
    End if;

  end;

  PROCEDURE de22(p_report       in varchar2,
                 p_session      in number,
                 p_sucursal     in number,
                 p_datefrom     in date,
                 p_fecha        in date,
                 p_tipocredito1 in number,
                 p_tipocredito2 in number,
                 p_tipocredito3 in number,
                 p_montocompara in number,
                 p_decimales    in number) IS
    --<<HD 8164
    vlv_cuenta varchar2(30);
    -->>HD 8164

    CUOTAS    NUMBER;
    DIASV     NUMBER;
    FECHAI    DATE;
    FECHAF    DATE;
    INICIO    NUMBER;
    final     number;
    S1        NUMBER;
    saldo     number;
    columna   varchar2(500);
    v_sql     varchar2(1000);
    SALIDA    VARCHAR2(20);
    REPORT    VARCHAR2(50);
    errtext   VARCHAR2(100);
    vlv_sigue varchar2(1);

    cursor a is
      select acr_credito,
             acr_fecha,
             acr_capred,
             acr_capnogi,
             acr_capvencido,
             acr_diasv,
             acr_intacum,
             acr_intactven,
             acr_intacteje,
             ACR_INTACUMDRJ,
             pre_pro,
             pre_tip,
             pre_mod,
             PRE_CREDITO,
             PRE_BASEMES,
             pre_moneda,
             PRE_TIPOCUO,
             PRE_FECVEN,
             pre_fecemi,
             pre_sucursal,
             LCA_DIASMORACAP,
			 LCA_DIASMORAINT,
			 LCA_DEMANDAJUNDICIAL,
			 ACR_INTACTJUD,
			 lca_contingente acr_contingente
        from tpre_accrual, tpre_prestamos,tleg_operacioncalif
       where pre_mod = lca_mod
         and pre_credito = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'M'--menores deudores
         and acr_credito = pre_credito
         and acr_fecha = p_fecha
		union
      select vis_numcue acr_credito,
             lca_fecha acr_fecha,
             lca_capital acr_capred,
             0 acr_capnogi,
             0 acr_capvencido,
             0 acr_diasv,
             lca_interes acr_intacum,
             0 acr_intactven,
             0 acr_intacteje,
             0 ACR_INTACUMDRJ,
             vis_pro pre_pro,
             vis_tip pre_tip,
             vis_mod pre_mod,
             vis_numcue PRE_CREDITO,
             360 PRE_BASEMES,
             vis_moneda pre_moneda,
             1 PRE_TIPOCUO,
             crd_fechavenc PRE_FECVEN,
             crd_fechavig pre_fecemi,
             vis_suc pre_sucursal,
             LCA_DIASMORAINT,
             LCA_DIASMORACAP,
			 LCA_DEMANDAJUNDICIAL,
             0 ACR_INTACTJUD,
			 lca_contingente acr_contingente
        from --tcap_acract ,
         tcap_vista ,tcap_credvista ,tleg_operacioncalif
       where vis_mod = lca_mod
         and vis_numcue = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'M'--mayores deudores
         and crd_numcue = vis_numcue
         --and aca_fecha = p_fecha
         --and aca_numcue = crd_numcue
         --and aca_tipocre = crd_tipocred
         --and aca_secuencia = crd_secuencia
         and crd_tipocred = 2
                       and  trunc(crd_fechautor) <= P_FECHA
					   and  crd_fechavenc >= P_FECHA
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
       order by prE_credito;
  --contingencia
      cursor a_contingencia is
      select vis_numcue acr_credito,
             lca_fecha acr_fecha,
             lca_contingente acr_capred,
             0 acr_capnogi,
             0 acr_capvencido,
             0 acr_diasv,
             0 acr_intacum,
             0 acr_intactven,
             0 acr_intacteje,
             0 ACR_INTACUMDRJ,
             vis_pro pre_pro,
             vis_tip pre_tip,
             vis_mod pre_mod,
             vis_numcue PRE_CREDITO,
             360 PRE_BASEMES,
             vis_moneda pre_moneda,
             1 PRE_TIPOCUO,
             crd_fechavenc PRE_FECVEN,
             crd_fechavig pre_fecemi,
             vis_suc pre_sucursal,
             LCA_DIASMORAINT,
             LCA_DIASMORACAP,
			 LCA_DEMANDAJUNDICIAL,
             0 ACR_INTACTJUD
        from --tcap_acract ,
         tcap_vista ,tcap_credvista ,tleg_operacioncalif
       where vis_mod = lca_mod
         and vis_numcue = lca_cuenta
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'M'--mayores deudores
         and crd_numcue = vis_numcue
         --and aca_fecha = p_fecha
         --and aca_numcue = crd_numcue
         --and aca_tipocre = crd_tipocred
         --and aca_secuencia = crd_secuencia
         and crd_tipocred = 2
                       and  trunc(crd_fechautor) <= P_FECHA
					   and  crd_fechavenc >= P_FECHA
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
         union
      select lca_cuenta acr_credito,
             lca_fecha acr_fecha,
             lca_contingente acr_capred,
             0 acr_capnogi,
             0 acr_capvencido,
             0 acr_diasv,
             0 acr_intacum,
             0 acr_intactven,
             0 acr_intacteje,
             0 ACR_INTACUMDRJ,
             pro_pro pre_pro,
             pro_tip pre_tip,
             pro_mod pre_mod,
             pro_cue PRE_CREDITO,
             360 PRE_BASEMES,
             pro_mon pre_moneda,
             1 PRE_TIPOCUO,
             f_fechatrabajo PRE_FECVEN,
             f_fechatrabajo pre_fecemi,
             lca_suc pre_sucursal,
             LCA_DIASMORAINT,
             LCA_DIASMORACAP,
			 LCA_DEMANDAJUNDICIAL,
             0 ACR_INTACTJUD
        from tleg_operacioncalif,tgen_cuenta
       where lca_mod = pro_mod
         and lca_cuenta = pro_cue
         and lca_fecha = p_fecha
         and lca_tipo = 'P'--permanente
         and lca_codtipocredito = 'M'--mayores deudores
         --and aca_fecha = p_fecha
         --and aca_numcue = crd_numcue
         --and aca_tipocre = crd_tipocred
         --and aca_secuencia = crd_secuencia
         and lca_mod = 10
       order by prE_credito;

    cursor b(modulo number, producto number, tipo number, moneda number) is
      select gdt_cuenta, GDT_CODGRU
        from tpre_funcgroupdet, tpre_funcgroup
       where GDT_MOD = modulo and
             GDT_PRO = producto and
             GDT_TIP = tipo and
             GDT_MON = moneda and --fng_modrpt = 21 and fng_trarpt = 6063 and
             fng_modrpt = 20 and
             fng_trarpt = 450 and --HD 8164
             fng_entrpt = 0 and
             fng_secrpt = 1 and
             gdt_mod in (4,6,10) and --HD 8164
             GDT_CODGRU <>45  and
             fng_cuenta = gdt_cuenta and
             GDT_MODRPT = fng_modrpt and
             GDT_TRARPT = fng_trarpt and
             fng_secrpt = gdt_secrpt --HD BDI 8164
       order by gdt_cuenta;
    cursor b_contingencia(modulo number, producto number, tipo number, moneda number) is
      select gdt_cuenta, GDT_CODGRU
        from tpre_funcgroupdet, tpre_funcgroup
       where GDT_MOD = modulo and
             GDT_PRO = producto and
             GDT_TIP = tipo and
             GDT_MON = moneda and --fng_modrpt = 21 and fng_trarpt = 6063 and
             fng_modrpt = 20 and
             fng_trarpt = 450 and --HD 8164
             fng_entrpt = 0 and
             fng_secrpt = 1 and
             GDT_CODGRU = 45 and
             gdt_mod in (4,6,10) and --HD 8164
             --1=0 and
             fng_cuenta = gdt_cuenta and
             GDT_MODRPT = fng_modrpt and
             GDT_TRARPT = fng_trarpt and
             fng_secrpt = gdt_secrpt --HD BDI 8164
       order by gdt_cuenta;

  begin
    REPORT := P_REPORT;
    IF (P_DATEFROM <= P_FECHA) AND (P_FECHA <= F_FECHATRABAJO) THEN
      dbms_output.put_line('en de21');
      pkg_legalreport.eraser(p_report, p_session);
      select TYPEOUT into salida from treportlegal where code = p_report;
      S1 := 1;
      for i in a loop
        for j in b(i.pre_mod, i.pre_pro, i.pre_tip, i.pre_moneda) loop
          --HD8164 BDI
          vlv_sigue := 'S';--fun_gen_ReeLeg(j.gdt_cuenta, i.pre_credito, p_fecha);
          if vlv_sigue = 'S' then
            --HD8164

            select cgp_columna
              into columna
              from tpre_colgroups
             where CGP_CODIGO = j.GDT_CODGRU;
            --<<HD8164 3
           /* if j.GDT_CODGRU=18 then
              select cgp_columna
                into columna
                from tpre_colgroups
               where CGP_CODIGO = 19;
            end if;*/
            -->>hd 8164

            V_SQL := ' SELECT SUM(' || columna || ')' ||
                     ' FROM TPRE_ACCRUAL,tpre_prestamos ' ||
                     ' WHERE ACR_CREDITO = PRE_CREDITO ' ||
                     ' AND ACR_FECHA  = ' || 'TO_DATE(' || '''' ||
                     TO_CHAR(p_FECHA, 'YYYY/MM/DD') || '''' || ',' ||
                     '''YYYY/MM/DD''' || ')' || ' AND ACR_CREDITO = ' ||
                     i.acr_CREDITO;
          if i.pre_mod = 6 then
            BEGIN
              EXECUTE IMMEDIATE V_SQL
                INTO Saldo;
            EXCEPTION
              WHEN OTHERS THEN
                RAISE_APPLICATION_ERROR(-20134,
                                        'Error calculando Saldo del Credito' ||
                                        sqlerrm);
            END;
          else
            --por sobregiros contratados
            if j.GDT_CODGRU = 96 then
              Saldo := I.ACR_CAPRED;
            elsif j.GDT_CODGRU = 45 then
              Saldo := I.ACR_contingente;
            else
            Saldo := i.acr_intacum;
            end if;
          end if;
            --<<HD 8164
            vlv_cuenta := fun_gen_cuenta(i.pre_credito,
                                         J.GDT_CUENTA,
                                         4,
                                         6,
                                         '2045001',
                                         p_fecha);
            -->>

            FECHAI := NULL;
            DIASV  := 0;
            if ABS(saldo) > 0 then
              IF I.ACR_CAPRED = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final  ;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;
                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;

                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --DIASV:=FECHAF-FECHAI;
                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  IF substr(vlv_cuenta,1,3) = '121' THEN
                     IF NVL(DIASv,0) > 30 THEN 
                        DIASv := 0;
                     END IF;
                  END IF;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                end;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPNOGI = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final ;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, cuotas);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPVENCIDO = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 3;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA between inicio and final    ;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    --DIASV:=FECHAF-FECHAI;
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF ABS(i.ACR_INTACUM) = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  IF substr(vlv_cuenta,1,5) = '12801' THEN
                     IF NVL(DIASv,0) > 30 THEN 
                        DIASv := 0 ;
                     END IF;
                  END IF;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_INTACTVEN = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF (i.ACR_INTACTEJE = saldo OR i.ACR_INTACUMDRJ = saldo ) and j.gdt_codgru<>18 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  --             SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF);
                  begin
                    select T.rde_inicio, T.rde_finaL
                      into inicio, final
                      from tgen_reprandet t
                     WHERE T.RDE_MODULO = 20 and
                           T.RDE_CODIGO =
                           (SELECT TREPORTLEGAL.RANGE
                              FROM TREPORTLEGAL
                             WHERE CODE = P_REPORT) -- HD 4701
                           and rde_seq = 3;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA >90;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --               SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF||', FECHAI:'||FECHAI||', DIASV:'||DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              ----------------------------------------
              --<<HD 8164 BDI
              FECHAI := NULL;
              DIASV  := 0;
              IF NVL(i.ACR_CAPRED, 0) + NVL(i.ACR_CAPNOGI,0) + NVL(i.ACR_CAPVENCIDO, 0) = saldo
              AND  j.gdt_codgru=17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final  ;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;

                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --DIASV:=FECHAF-FECHAI;
                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                end;
              END IF;
              -->>
              -----------------------------------------
              -----------------------------------------
              --<<HD 8164 2
              --ACR_INTACTJUD
              --IF NVL(i.ACR_INTACUM, 0)+NVL(i.ACR_INTACTVEN,0)+NVL(i.ACR_INTACTEJE,0) = saldo parace que no probaron!
              IF NVL(i.ACR_INTACTJUD,0) = saldo
              AND  j.gdt_codgru=18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
                                  bdi_promedio(i.pre_moneda,
                                               i.pre_sucursal,
                                               p_fecha) * saldo),
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);
                  s1 := s1 + 1;
                END;
              end if;
              -->>HD8164 2

            END IF;
          end if;
        end loop;
      end loop;
      --CONTINGENCIA
      for i in a_contingencia loop
        for j in b_contingencia(i.pre_mod, i.pre_pro, i.pre_tip, i.pre_moneda) loop
          --HD8164 BDI
          vlv_sigue := 'S';--fun_gen_ReeLeg(j.gdt_cuenta, i.pre_credito, p_fecha);
          if vlv_sigue = 'S' then
            --HD8164

            select cgp_columna
              into columna
              from tpre_colgroups
             where CGP_CODIGO = j.GDT_CODGRU;
            --<<HD8164 3
           /* if j.GDT_CODGRU=18 then
              select cgp_columna
                into columna
                from tpre_colgroups
               where CGP_CODIGO = 19;
            end if;*/
            -->>hd 8164

            V_SQL := ' SELECT SUM(' || columna || ')' ||
                     ' FROM TPRE_ACCRUAL,tpre_prestamos ' ||
                     ' WHERE ACR_CREDITO = PRE_CREDITO ' ||
                     ' AND ACR_FECHA  = ' || 'TO_DATE(' || '''' ||
                     TO_CHAR(p_FECHA, 'YYYY/MM/DD') || '''' || ',' ||
                     '''YYYY/MM/DD''' || ')' || ' AND ACR_CREDITO = ' ||
                     i.acr_CREDITO;
          if i.pre_mod = 6 then
            BEGIN
              EXECUTE IMMEDIATE V_SQL
                INTO Saldo;
            EXCEPTION
              WHEN OTHERS THEN
                RAISE_APPLICATION_ERROR(-20134,
                                        'Error calculando Saldo del Credito' ||
                                        sqlerrm);
            END;
          else
            --por sobregiros contratados
            if j.GDT_CODGRU = 45 then
              Saldo := I.ACR_CAPRED;
            else
            Saldo := i.acr_intacum;
            end if;
          end if;
            --<<HD 8164
            vlv_cuenta := fun_gen_cuenta(i.pre_credito,
                                         J.GDT_CUENTA,
                                         4,
                                         6,
                                         '2045001',
                                         p_fecha);
            -->>

            FECHAI := NULL;
            DIASV  := 0;
            if ABS(saldo) > 0 then
              IF I.ACR_CAPRED = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final  ;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;
                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;

                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --DIASV:=FECHAF-FECHAI;
                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                end;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPNOGI = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final ;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, cuotas);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_CAPVENCIDO = saldo  and j.gdt_codgru<>17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 3;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA between inicio and final    ;
                           AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    --DIASV:=FECHAF-FECHAI;
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF ABS(i.ACR_INTACUM) = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF i.ACR_INTACTVEN = saldo  and j.gdt_codgru<>18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 2;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= FECHAF
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,
									saldo),--por doble me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              FECHAI := NULL;
              DIASV  := 0;
              IF (i.ACR_INTACTEJE = saldo OR i.ACR_INTACUMDRJ = saldo ) and j.gdt_codgru<>18 THEN
                BEGIN
                  FECHAF := P_FECHA;
                  --             SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF);
                  begin
                    select T.rde_inicio, T.rde_finaL
                      into inicio, final
                      from tgen_reprandet t
                     WHERE T.RDE_MODULO = 20 and
                           T.RDE_CODIGO =
                           (SELECT TREPORTLEGAL.RANGE
                              FROM TREPORTLEGAL
                             WHERE CODE = P_REPORT) -- HD 4701
                           and rde_seq = 3;
                    SELECT MIN(CUO_FECHA), COUNT(CUO_NUMCUO)
                      INTO FECHAI, CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA >90;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN), COUNT(LET_FECVEN)
                        INTO FECHAI, CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;

                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    --               SAVE_LOG('DE212, CREDITO:'||I.PRE_CREDITO||', FECHAF:'||FECHAF||', FECHAI:'||FECHAI||', DIASV:'||DIASV);
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                END;
              END IF;
              ----------------------------------------
              --<<HD 8164 BDI
              FECHAI := NULL;
              DIASV  := 0;
              IF NVL(i.ACR_CAPRED, 0) + NVL(i.ACR_CAPNOGI,0) + NVL(i.ACR_CAPVENCIDO, 0) = saldo
              AND  j.gdt_codgru=17 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and t.rde_seq = 1;
                  begin
                    IF I.PRE_FECVEN < P_FECHA THEN
                      FECHAF := P_FECHA;
                    ELSE
                      FECHAF := I.PRE_FECVEN;
                    END IF;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha AND CUO_CAPITAL <> 0 AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA;
                    END IF;

                    SELECT COUNT(CUO_NUMCUO)
                      INTO CUOTAS
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA >= FECHAI;
                    --AND CUO_CAPITAL <> 0;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final  ;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                      IF FECHAI IS NULL THEN
                        FECHAI := P_FECHA;
                      END IF;
                      SELECT COUNT(LET_CREDITO)
                        INTO CUOTAS
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN >= FECHAI;
                      --AND LET_VALOR <> 0;

                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    --DIASV:=FECHAF-FECHAI;
                    --              FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, CUOTAS);
                  s1 := s1 + 1;
                end;
              END IF;
              -->>
              -----------------------------------------
              -----------------------------------------
              --<<HD 8164 2
              --ACR_INTACTJUD
              --IF NVL(i.ACR_INTACUM, 0)+NVL(i.ACR_INTACTVEN,0)+NVL(i.ACR_INTACTEJE,0) = saldo parace que no probaron!
              IF NVL(i.ACR_INTACTJUD,0) = saldo
              AND  j.gdt_codgru=18 THEN
                BEGIN
                  select T.rde_inicio, T.rde_finaL
                    into inicio, final
                    from tgen_reprandet t
                   WHERE T.RDE_MODULO = 20 and
                         T.RDE_CODIGO =
                         (SELECT TREPORTLEGAL.RANGE
                            FROM TREPORTLEGAL
                           WHERE CODE = P_REPORT) -- HD 4701
                         and rde_seq = 1;
                  begin
                    FECHAF := P_FECHA;
                    SELECT MIN(CUO_FECHA)
                      INTO FECHAI
                      FROM TPRE_CUOTAS
                     WHERE CUO_CREDITO = I.ACR_CREDITO AND
                           CUO_FECHA <= P_FECHA
                          --AND nvl(CUO_FECPAG,to_date('2099/01/01','yyyy/mm/dd')) >p_fecha
                           AND nvl(CUO_FECCONTAB,
                                   to_date('2099/01/01', 'yyyy/mm/dd')) >
                           p_fecha
                          --AND FECHAF-CUO_FECHA BETWEEN inicio AND final;
                           AND
                           NUMERO_DIAS(P_FECHA + 1,
                                       CUO_FECHA,
                                       I.PRE_BASEMES) BETWEEN inicio AND
                           final;
                    ---PARA DOCUMENTOS DESCONTADO
                    IF I.PRE_TIPOCUO = 6 THEN
                      SELECT MIN(LET_FECVEN)
                        INTO FECHAI
                        FROM TPRE_LETRAS
                       WHERE LET_CREDITO = I.ACR_CREDITO AND
                             LET_FECVEN <= P_FECHA AND
                             nvl(LET_FECCONTAB,
                                 to_date('2099/01/01', 'yyyy/mm/dd')) >
                             p_fecha
                            --AND FECHAF-LET_FECVEN BETWEEN inicio AND final;
                             AND
                             NUMERO_DIAS(P_FECHA + 1,
                                         LET_FECVEN,
                                         I.PRE_BASEMES) BETWEEN inicio AND
                             final;
                    END IF;
                    ---FROM DE DOCUMENTOS DESCONTADOS
                    IF I.LCA_DIASMORACAP > i.LCA_DIASMORAINT then
                       DIASV:=I.LCA_DIASMORACAP;
                    ELSE
                       DIASV:=i.LCA_DIASMORAINT;
                    END IF;

                    IF FECHAI IS NULL THEN
                      FECHAI := P_FECHA; --I.PRE_FECEMI;
                    END IF;
                    --DIASV:=FECHAF-FECHAi;
                    --               FECHAS_FORMULA(P_FECHA + 1,FECHAI,I.PRe_BASEMES,DIASV);
                    IF DIASV < 0 THEN
                      DIASV := 0;
                    END IF;
                  exception
                    when others then
                      diasv  := inicio;
                      cuotas := 1;
                  end;

                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 1, S1, 1, S1);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 2, S1, 2, i.pre_credito);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUECHAR)
                  VALUES
                    (p_report, p_session, 3, S1, 3, vlv_cuenta);
                  --(p_report, p_session, 3, S1, 3, J.GDT_CUENTA);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report,
                     p_session,
                     4,
                     S1,
                     4,
                     round(decode(i.pre_moneda,
                                  0,
                                  saldo,saldo),--por doble me
                           p_decimales));
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 5, S1, 5, DIASv);
                  INSERT INTO tvaluelevelcustom
                    (REPORT, SESSIONID, CODE, ROWSID, COLUMSID, VALUENUM)
                  VALUES
                    (p_report, p_session, 6, S1, 6, 1);
                  s1 := s1 + 1;
                END;
              end if;
              -->>HD8164 2

            END IF;
          end if;
        end loop;
      end loop;
      --FIN CONTINGENCIA
      commit;
      if salida = 'A' then
        pkg_legalreport3.generic_file(p_report, p_session, p_fecha);
      end if;
    Else
      errtext := 'Fechas no válidas';
      raise_application_error(-20901, errtext);
    End if;

  end;                   
  FUNCTION fecha_aprobado(
          p_numcomprom in number,
          p_credito in number,
          p_solpv in varchar2)
        RETURN DATE IS         
        v_fecautoriza DATE;
  BEGIN               
                         
            begin
          select c.revision_date
            into v_fecautoriza
			from TREQ_CUSTOMER_SCORE_lcl@DB_EAPPFISA c
           where  c.request_id = p_solpv; 
          exception 
            when others then   
            begin
	        SELECT max(nvl(t.dispatch_time,t.end_time))
	           into v_fecautoriza
			  FROM TWKL_TASK@DB_EAPPFISA T,EEFISA.TAPD_BUSINESS_TEMPLATE@DB_EAPPFISA B
			  WHERE T.URL_INFO = BUSINESS_TEMPLATE_ID
			  and instr(t.description,p_solpv) > 0
			  and t.action_status = 3
			  --and rownum = 1
			    AND B.DESCRIPTION LIKE '%Negocio%';            
			exception 
			  when others then
			     v_fecautoriza:=NULL;  
			end;
          end;    
          RETURN v_fecautoriza;
  END;
  FUNCTION monto_aprobado(p_credito in number, p_fecha in date) RETURN NUMBER IS
    v_Monto_Org NUMBER;
    v_Monto_Inc NUMBER;
    v_Cobros    NUMBER;
    bCompromiso BOOLEAN := FALSE;
    --Colaboracion de p.p PMC
    CURSOR curInc IS
      SELECT *
        FROM tpre_inccapital
       WHERE inc_credito = p_credito AND inc_transa = 36
            --AND inc_fechasol = nvl(pApplyForDate_i,inc_fechasol)
             AND TRUNC(inc_fechaaut) <= p_Fecha AND inc_numtra IS NULL AND
             inc_codusr IS NOT NULL AND inc_status = 2; /*Autorizados*/
    vln_Capitalized NUMBER(18, 6) := 0;
    vln_Aux         NUMBER(18, 6);
    w_moneda  NUMBER;
    w_sucursal NUMBER;
  BEGIN
    BEGIN
      -- Obtenemos el monto original, que no se modifica
      SELECT pre_montoorg,pre_moneda,pre_sucursal
        INTO v_Monto_Org,w_moneda,w_sucursal
        FROM tpre_prestamos
       WHERE pre_credito = p_credito;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        bCompromiso := TRUE;
        v_Monto_Org := 0;
        w_moneda := 0;
        w_sucursal :=1;
      WHEN OTHERS THEN
        Raise_Application_Error(-20999,
                                'Error en Saldo_Actual_Capital ' ||
                                TO_CHAR(p_credito));
    END;
    IF bCompromiso THEN
      /*
      ** En el caso de Compromiso, devuelve el monto pactado hasta un fecha X, pero no su saldo!!
      ** Esto es necesario ya que se la utiliza al reversar el uso de garantías.
      ** Pero hay que considerar que una garantía SIEMPRE cubre el total del monto del Compromiso y no su Saldo!!
      */
    BEGIN
      -- Obtenemos el monto original, que no se modifica
      SELECT cmp_moneda,cmp_sucursal
        INTO w_moneda,w_sucursal
        FROM tpre_comprom
       WHERE cmp_numero = p_credito;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_Monto_Org := 0;
        w_moneda := 0;
        w_sucursal :=1;
      WHEN OTHERS THEN
        Raise_Application_Error(-20999,
                                'Error en Saldo_Actual_Capital ' ||
                                TO_CHAR(p_credito));
    END;
      SELECT SUM(NVL(dcm_valor, 0))
        INTO v_Monto_Org
        FROM tpre_detcomprom
       WHERE dcm_numero = p_Credito AND dcm_fechaliq <= p_Fecha;
      RETURN NVL(v_Monto_Org, 0)* bdi_promedio(w_moneda,w_sucursal,p_fecha);
    END IF;
    -- Retorna la suma de los incrementos
    --   SELECT Nvl(Sum(inc_monto),0)
    SELECT nvl(sum(nvl(inc_monto, 0) - nvl(inc_retenido, 0)), 0)
      INTO v_Monto_Inc
      FROM tpre_inccapital
     WHERE inc_codusr IS NOT NULL AND inc_numtra IS NOT NULL AND
           inc_status = 2 AND inc_transa <> 36 AND inc_credito = p_credito AND
           TRUNC(inc_fechaaut) <= p_Fecha;
    --Colaboracion de p.p PMC
    -- Del monto de incremento restamos lo que se pago con el arreglo y se encuentra el monto
    -- que pertenecía a (mora + gastos + cuentasxcobrar) - impuestos
    FOR recInc IN curInc LOOP
      SELECT sum(nvl(cob_capital, 0))
        INTO vln_Aux
        FROM tpre_cobros
       WHERE cob_credito = recInc.inc_credito AND
             cob_sectran = recInc.inc_numtra AND
             cob_operador = recInc.inc_codusr AND cob_status = 'R';
      vln_Aux         := recInc.inc_monto - vln_Aux;
      vln_Capitalized := vln_Capitalized + vln_Aux;
    END LOOP;
    RETURN (v_Monto_Org + v_Monto_Inc + vln_Capitalized)*bdi_promedio(w_moneda,w_sucursal,p_fecha);
  END;

  --BUSQUEDA DE ARREGLOS DE PAGOS SI Y SOLAMENTE SI EL ARREGLO A PROVOCADO UN CAMBIO DE CAPITAL
  --O UNA VARIACIóN EN SU PLAZO.
  PROCEDURE arreglo_pagos(p_credito       IN NUMBER,
                          p_fecha         IN DATE,
                          p_fechaArr      OUT DATE,
                          p_NroArreglos   OUT NUMBER,
                          p_plazoAnterior OUT NUMBER,
                          p_montoAnterior OUT NUMBER) IS
    CURSOR P IS
      SELECT TRUNC(PRP_FECARR) PRP_FECARR, PRE_PLAZO
        FROM TPRE_ARRPRESTAMOS, TPRE_PRESTAMOS
       WHERE PRP_CREDITO = P_CREDITO AND PRE_CREDITO = PRP_CREDITO AND
             TRUNC(PRP_FECARR) <= p_fecha
       GROUP BY TRUNC(PRP_FECARR), PRE_PLAZO;
    V_MONTODESPUES  TPRE_ACCRUAL.ACR_CAPRED%TYPE;
    V_MONTOANTES    TPRE_ACCRUAL.ACR_CAPRED%TYPE;
    V_PLAZOANTES    TPRE_ACCRUAL.ACR_PLAZO%TYPE;
    V_PLAZODESPUES  TPRE_ACCRUAL.ACR_PLAZO%TYPE;
    V_CONTAARREGLOS NUMBER;
    V_MAXFECHA      DATE;
    V_MAXFECHAAUX   DATE;
  BEGIN
    V_CONTAARREGLOS := 0;
    V_MAXFECHA      := TO_DATE('1940/01/01', 'YYYY/MM/DD');
    V_MAXFECHAAUX   := NULL;
    FOR X IN P LOOP
      --VARIACION DE PLAZO
      V_MONTOANTES   := 0;
      V_PLAZOANTES   := 0;
      V_MONTODESPUES := 0;
      V_PLAZODESPUES := 0;
      BEGIN
        SELECT NVL(ACR_CAPRED, 0) + NVL(ACR_CAPNOGI, 0) +
               NVL(ACR_CAPVENCIDO, 0),
               ACR_PLAZO
          INTO V_MONTOANTES, V_PLAZOANTES
          FROM TPRE_ACCRUAL
         WHERE ACR_CREDITO = P_CREDITO AND
               ACR_FECHA = TRUNC(X.PRP_FECARR) - 1;
      EXCEPTION
        WHEN OTHERS THEN
          V_PLAZOANTES := 0;
          V_MONTOANTES := 0;
      END;
      BEGIN
        SELECT NVL(ACR_CAPRED, 0) + NVL(ACR_CAPNOGI, 0) +
               NVL(ACR_CAPVENCIDO, 0),
               ACR_PLAZO
          INTO V_MONTODESPUES, V_PLAZODESPUES
          FROM TPRE_ACCRUAL
         WHERE ACR_CREDITO = P_CREDITO AND ACR_FECHA = TRUNC(X.PRP_FECARR);
      EXCEPTION
        WHEN OTHERS THEN
          V_PLAZODESPUES := X.PRE_PLAZO;
          V_MONTODESPUES := SALDO_CAPITAL_FECHA(P_CREDITO,
                                                TRUNC(X.PRP_FECARR));
      END;
      --VARIACION DE MONTO
      IF (NVL(V_MONTODESPUES, 0) > NVL(V_MONTOANTES, 0)) OR
         (NVL(V_PLAZOANTES, 0) <> NVL(V_PLAZODESPUES, 0)) THEN
        V_CONTAARREGLOS := V_CONTAARREGLOS + 1;
        V_MAXFECHAAUX   := X.PRP_FECARR;
        IF V_MAXFECHAAUX > V_MAXFECHA THEN
          V_MAXFECHA := V_MAXFECHAAUX;
        END IF;
      END IF;
      --
    END LOOP;
    --CON LA FECHA MAS RECIENTE DEL ARREGLO DE PAGOS EN ENCUENTRO EL PLAZO O EL MONTO AL DIA ANTERIOR
    BEGIN
      SELECT NVL(ACR_CAPRED, 0) + NVL(ACR_CAPNOGI, 0) +
             NVL(ACR_CAPVENCIDO, 0),
             ACR_PLAZO
        INTO V_MONTOANTES, V_PLAZOANTES
        FROM TPRE_ACCRUAL
       WHERE ACR_CREDITO = P_CREDITO AND ACR_FECHA = TRUNC(V_MAXFECHA) - 1;
    EXCEPTION
      WHEN OTHERS THEN
        BEGIN
          SELECT SALDO_CAPITAL_FECHA(PRE_CREDITO, P_FECHA), PRE_PLAZO
            INTO V_MONTOANTES, V_PLAZOANTES
            FROM TPRE_PRESTAMOS
           WHERE PRE_CREDITO = P_CREDITO;
        EXCEPTION
          WHEN OTHERS THEN
            V_PLAZOANTES := 0;
            V_MONTOANTES := 0;
        END;
    END;
    IF V_MAXFECHA = TO_DATE('1940/01/01', 'YYYY/MM/DD') THEN
      V_MAXFECHA := NULL;
    END IF;
    p_NroArreglos   := V_CONTAARREGLOS;
    p_fechaArr      := V_MAXFECHA;
    p_plazoAnterior := V_PLAZOANTES;
    p_montoAnterior := V_MONTOANTES;
  END;
  FUNCTION numero_dias(p_fechaFinal   in date,
                       p_fechaInicial in date,
                       p_basemes      in number) RETURN NUMBER IS
    v_dias NUMBER := 0;
    --Colaboracion de p.p PMC

  BEGIN
    fechas_FORmula(p_fechaFinal, p_fechaInicial, p_basemes, v_dias);
    RETURN v_dias;
  END;
  --para lavado de dinero
--FIN IF0001------------------------------------------------------------------------------------------------------------------------IF001FIN
  function fun_gen_cuenta(pni_operacion in varchar2,
                          pni_cuenta    in number,
                          pni_criterio  in number,
                          pni_modulos   in varchar2,
                          pni_reporte   in varchar2,
                          pni_fecha in date) return varchar2 is
    vlv_cuenta   varchar2(30);
    vlv_cuentaaux   varchar2(30);
    vln_mod      number(2);
    vln_pro      number(2);
    vln_tip      number(2);
    vln_mon      number(2);
    vln_sector   number(4);
    vln_existe   number(2);
    aux_anexo    varchar2(15) := null;
    aux_linkcode varchar2(10);
    aux_acccode  varchar2(5);
    i number :=0;
    j number :=0;    
  begin

    begin
      select count(*)
        into vln_existe
        from tpre_funcgroupdet, tpre_criterios
       where cri_cuenta = pni_cuenta and gdt_mod in (pni_modulos) -- ...Por ser enviado como parametro
             and cri_codtab = 917 and cri_coddes = pni_criterio and
             cri_cuenta = gdt_cuenta and cri_mod = gdt_mod and
             cri_modrpt = gdt_modrpt and cri_trarpt = gdt_trarpt and
             cri_entrpt = gdt_entrpt and cri_secrpt = gdt_secrpt and
             (cri_modrpt || cri_trarpt || cri_entrpt || cri_secrpt) in --Reporte 90/2041 ..Mod-trt-Ent-Sec
             (select pni_reporte from dual);

      if vln_existe > 0 then
        begin
          /*select a.pro_sector
            into vln_sector
            from tgen_cuentasubtipo a
           where a.pro_cue = pni_operacion;*/ 
           select SECTOR
             into vln_sector
             from tgen_saldomodulos
            where dateload =  pni_fecha 
              and acc= pni_operacion
              and rownum = 1; 
          /*a.pro_mod = vln_mod
          and   a.pro_pro = vln_pro
          and   a.pro_tip = vln_tip
          and   a.pro_mon = vln_mon
          and   a.pro_cue = pni_operacion;*/

        exception
          when others then
            return pni_cuenta;
        end;
        begin
          select a.linkcode, a.acccode
            into aux_linkcode, aux_acccode
            from tgen_detjerarquiatab a
           where a.joincode = vln_sector;

          aux_anexo := aux_linkcode;
          if pni_criterio = 4 then
            aux_anexo := aux_anexo || aux_acccode;
          end if;
        exception
          when others then
            aux_anexo := null;
        end;
      end if;
    exception
      when others then
        null;
    end;

    vlv_cuenta := pni_cuenta || aux_anexo;
    i:=length(vlv_cuenta);
    j:=i;
    --pasa saber las cuentas del analitico
       for y in 1..j loop
        begin
        select accountcode
          into vlv_cuentaaux
		  from tgen_esquemacont 
		 where reportcode = '2031' 
		   and accountcode = substr(vlv_cuenta,1,i)
		   and rownum = 1;
		   vlv_cuenta:=vlv_cuentaaux;
		   exit;
        exception        
          when no_data_found then
            i:=i-1;
        end;		           
      end loop;
    --fin
    return ltrim(rtrim(vlv_cuenta));
  end;
 --cuenta real
 function fun_cuenta_del_2031(pni_cuenta    in varchar2) return varchar2 is
  i number :=0;
  j number:=0;            
  vlv_cuenta varchar2(30);
  vlv_cuentaaux varchar2(30);  
 begin                  
    vlv_cuenta:=pni_cuenta;
    i:=length(vlv_cuenta);
    j:=i;
    --pasa saber las cuentas del analitico
      for y in 1..j loop
        begin
        select accountcode
          into vlv_cuentaaux
		  from tgen_esquemacont 
		 where reportcode = '2031' 
		   and accountcode = substr(vlv_cuenta,1,i)
		   and rownum = 1;
		   vlv_cuenta:=vlv_cuentaaux;
		   exit;
        exception        
          when no_data_found then
            i:=i-1;
        end;		           
      end loop;
    --fin
    return ltrim(rtrim(vlv_cuenta));
 end;
 --fin cuenta
 
  function fun_gen_ReeLeg(pvi_cuenta  in varchar2,
                          pni_credito in number,
                          pdi_fecha   in date) return varchar2 is
    vlv_sigue varchar2(1);
    vlv_status varchar2(1):=null;
  begin
    if substr(pvi_cuenta, 1, 3) = '124' or
       substr(pvi_cuenta, 1, 5) = '12804' then
      begin
        select NVL(acr_otrostatus,'-')
          into vlv_status
          from tpre_accrual
         where acr_credito = pni_credito and
               acr_fecha = trunc(pdi_fecha);
         if vlv_status = 'R' then
           vlv_sigue:='S';
         else
           vlv_sigue:='N';
         end if;
      exception
        when no_data_found then
          vlv_sigue := 'N';
        when others then
          vlv_sigue := 'N';
      end;
    elsif substr(pvi_cuenta, 1, 3) = '125' or
          substr(pvi_cuenta, 1, 5) = '12805' then
      begin
        select NVL(acr_otrostatus,'-')
          into vlv_status
          from tpre_accrual
         where acr_credito = pni_credito and
               acr_fecha = trunc(pdi_fecha);
         if vlv_status = 'J' then
           vlv_sigue:='S';
         else
           vlv_sigue:='N';
         end if;
    exception
        when no_data_found then
          vlv_sigue := 'N';
        when others then
          vlv_sigue := 'N';
      end;
    else
      vlv_sigue := 'S';
      begin
        select NVL(acr_otrostatus,'-')
          into vlv_status
          from tpre_accrual
         where acr_credito = pni_credito and
               acr_fecha = trunc(pdi_fecha);
         if vlv_status = 'R' then
           vlv_sigue:='N';
         else
           vlv_sigue:='S';
         end if;
      exception
        when no_data_found then
          vlv_sigue := 'S';
        when others then
          vlv_sigue := 'S';
      end;
      if vlv_sigue = 'S' then
        begin
        select NVL(acr_otrostatus,'-')
          into vlv_status
          from tpre_accrual
         where acr_credito = pni_credito and
               acr_fecha = trunc(pdi_fecha);
         if vlv_status = 'J' then
           vlv_sigue:='N';
         else
           vlv_sigue:='S';
         end if;
        exception
          when no_data_found then
            vlv_sigue := 'S';
          when others then
            vlv_sigue := 'S';
        end;
      end if;
    end if;
    return vlv_sigue;
  end;
  procedure pro_gen_cuoextra(pni_credito in number,
                             pdi_fecha in date,
                             pnb_cuota out number,
                             pdb_fecha out date) is

    cursor x(pni_credito in number) is
      select cuo_numcuo,cuo_valor,cuo_fecha
      from tpre_cuotas
      where cuo_credito=pni_credito
      and   nvl(cuo_fecpag,TO_DATE('2090/06/06','YYYY/MM/DD')) > pdi_fecha
      order by cuo_fecha,cuo_numcuo;

    vln_valor number(15,2);
    vlv_existe varchar2(1);
    vlv_band   varchar2(1);
    vln_extnumcuo number(6);  
    v_tipocuo number;                          
    w_cuota number:=0;
    w_fecha date:=null;
    vld_fecha date;
  begin


    vlv_band:='0';
    vlv_existe:='N';
    vln_valor:=0;
    pnb_cuota:=null;
    pdb_fecha:=null;
    begin
      select pre_tipocuo
        into v_tipocuo
        from tpre_prestamos 
      where pre_credito = pni_credito;
    exception
       when others then
          v_tipocuo := null;
    end;              
    if v_tipocuo = 8 then
	    for j in x(pni_credito) loop
	      if vlv_band='0' then
	        vln_valor := (j.cuo_valor * 0.20) + j.cuo_valor;
	        vld_fecha:=j.cuo_fecha;
	        vlv_band:='1';
	      end if;
	      if vlv_existe='N' then
	        if j.cuo_valor >= vln_valor then
	          pnb_cuota:=j.cuo_valor;
	          pdb_fecha:=j.cuo_fecha;
	          vln_extnumcuo:=j.cuo_numcuo;
	          vlv_existe:='S';
	        end if;
	      end if;
	    end loop;            
      if vlv_existe='N' then
            pnb_cuota:=vln_valor;
            pdb_fecha:=vld_fecha;
      end if;    
    end if;                  
    
    if  v_tipocuo = 7 then
    
        select sum(cuo_valor) ,max(cuo_fecha)
          into w_cuota,w_fecha
        from tpre_cuotas
        where cuo_credito = pni_credito
          and cuo_numcuo = (select max(cuo_numcuo)
                              from tpre_cuotas 
                              where cuo_credito = pni_credito)
          and cuo_dias > 0;       
          pnb_cuota:=w_cuota;           
          
          pdb_fecha:=w_fecha;
    elsif v_tipocuo in (1,3,5) then
          pnb_cuota:=null;
          pdb_fecha:=null;    
    end if;
  end;
  /**/
  function fun_gen_revtasa (pni_credito in number,
                            pdi_fecha in date)
                           return date is
    vld_fecha date:=null;
    vld_fechaori date;
  begin

    begin
      select add_months(cuo_fecha,1),cuo_fecha
      into vld_fecha,vld_fechaori
      from tpre_cuotas
      where cuo_credito=pni_credito
      and   cuo_fecha =
        (
        select max(cuo_fecha)
        from tpre_cuotas
        where cuo_credito=pni_credito
        and   trunc(cuo_fecha)<=pdi_fecha
        );
     exception
       when others then
         select min(cuo_fecha)
         into vld_fecha
         from tpre_cuotas
         where cuo_credito=pni_credito
         and   trunc(cuo_fecha)>pdi_fecha;
     end;

     if    vld_fecha < pdi_fecha then
        vld_fecha:=add_months(pdi_fecha,1);
     else
       begin
         select to_date(
                   to_char(vld_fecha,'YYYY/MM')
                  ||'/'||
                  to_char(last_day(vld_fecha),'DD')
                ,'YYYY/MM/DD')
         into vld_fecha
         from dual;
      exception
        when others then
          vld_fecha:=null;
      end;
     end if;
     return vld_fecha;
  end;
FUNCTION saldo_capital_fecha_de
     (p_operacion IN NUMBER,
      p_fecha IN DATE) RETURN NUMBER
IS
W_MOD NUMBER;
W_SALDO NUMBER;   
W_MON number;
BEGIN
 SELECT PRO_MOD,PRO_MON
   INTO W_MOD,W_MON
   FROM TGEN_CUENTA
  WHERE PRO_CUE = P_OPERACION;
 IF  W_MOD =  6 THEN
   W_SALDO:= SALDO_CAPITAL_FECHA(P_OPERACION,P_FECHA);
   W_SALDO := W_SALDO*BDI_PROMEDIO(W_MON,1,P_FECHA);
     RETURN W_SALDO;
 ELSIF W_MOD =  4 THEN
   SELECT SUM(ACA_UTILIZADO)
     INTO W_SALDO
    FROM TCAP_ACRACT
   WHERE ACA_NUMCUE = P_OPERACION
     AND ACA_FECHA = P_FECHA;                       
   W_SALDO := W_SALDO*BDI_PROMEDIO(W_MON,1,P_FECHA);     
    RETURN W_SALDO;
 END IF ;
EXCEPTION
WHEN OTHERS THEN
  RETURN NULL;
END;

PROCEDURE FECHA_TASACION(
   p_cliente IN NUMBER,
   p_numgaran IN NUMBER,
   p_decimales IN NUMBER,
   p_dateto IN DATE,
   p_fechat OUT DATE,
   p_valor OUT NUMBER)
 IS
BEGIN
  BEGIN
    SELECT avl_fecha, round(avl_valor,p_decimales)
        INTO p_fechat, p_valor
        FROM  tgar_avaluos av
        WHERE av.avl_codcli = p_cliente and
              av.avl_numgar=p_numgaran and
              p_dateto BETWEEN TRUNC(AVL_FECHA) AND NVL(TRUNC(AVL_FECHASTA),TO_DATE('2199/12/31','YYYY/MM/DD')) AND
              avl_fecha = (SELECT MAX(avl_fecha)
                              FROM tgar_avaluos
                              WHERE avl_codcli= av.avl_codcli
                                AND avl_numgar = av.avl_numgar
                                AND p_dateto BETWEEN TRUNC(AVL_FECHA) AND NVL(TRUNC(AVL_FECHASTA),TO_DATE('2199/12/31','YYYY/MM/DD')) ) ;
              --tgar_avaluos.avl_fechasta is null;
       	EXCEPTION
          when no_data_found then
      		p_fechat:=null;
    		p_valor:= null;
  END;
END FECHA_TASACION;
---------------------------------------reporte resumen de saldos de los ultimos 12 meses----------------------------------------
PROCEDURE de_resumen_saldos_anual(
       p_operador        in    number,
       p_fecha          in    date) is
  w_fechaini date;
  w_fechaGo date;
  cursor p is
    select lca_mod,lca_cuenta,lca_capital,lca_interes,lca_codtipocredito,lca_mon,
            lca_suc
      from tleg_operacioncalif
    where lca_fecha = p_fecha;
  w_diasv number;
  w_diasint number;
  w_diastotal number;
  w_capital number;
  w_interes number;
  w_nombre varchar2(40);
BEGIN
  w_fechaini:=add_months(p_fecha,-12);
  delete tleg_historicocartera where lhc_operador = p_operador;
  FOR cartera IN p LOOP
    FOR i in 1..12 LOOP
      w_fechaGo:=add_months(w_fechaini,i);
     if cartera.lca_mod = 6 then
        begin
        select acr_diasv,acr_diasint,(nvl(acr_capred,0) + nvl(acr_capnogi,0) + nvl(acr_capvencido,0)),
               (nvl(acr_intacum,0) + nvl(acr_intactven,0) + nvl(acr_intacteje,0) + nvl(acr_intacumdrj,0)),
               replace(pre_nombre,',','')
          into w_diasv,w_diasint,w_capital,w_interes,w_nombre
         from tpre_accrual,tpre_prestamos
         where acr_credito = pre_credito
           and acr_credito = cartera.lca_cuenta
           and acr_fecha = w_fechaGo;
        exception
          when others then
             w_diasv:=0;
             w_diasint:=0;
             w_capital:=0;
             w_interes:=0;
        end;
        if cartera.lca_mon <> 0 then
           w_capital:=w_capital*bdi_promedio(cartera.lca_mon,cartera.lca_suc,w_fechaGo);
           w_interes:=w_interes*bdi_promedio(cartera.lca_mon,cartera.lca_suc,w_fechaGo);
        end if;
    select max(nvl(acr_diasv,0)),max(nvl(acr_diasint,0))
      into  w_diasv, w_diasint
     from tpre_accrual
    where acr_credito = cartera.lca_cuenta
      and acr_fecha between add_months(w_fechaGo,-12) and w_fechaGo;

        insert into tleg_historicocartera(lhc_operador,lhc_fechagen,lhc_credito,lhc_secmes,lhc_fechahis,lhc_tipocol,lhc_valor,lhc_nombre,lhc_codtipocredito)
            values(p_operador,p_fecha,cartera.lca_cuenta,i,w_fechaGo,'C',w_capital,w_nombre,cartera.lca_codtipocredito);

        insert into tleg_historicocartera(lhc_operador,lhc_fechagen,lhc_credito,lhc_secmes,lhc_fechahis,lhc_tipocol,lhc_valor)
            values(p_operador,p_fecha,cartera.lca_cuenta,i,w_fechaGo,'I',w_interes);
        if w_diasv > w_diasint then
           w_diastotal:=w_diasv;
        else
           w_diastotal:=w_diasint;
        end if;
        insert into tleg_historicocartera(lhc_operador,lhc_fechagen,lhc_credito,lhc_secmes,lhc_fechahis,lhc_tipocol,lhc_valor)
            values(p_operador,p_fecha,cartera.lca_cuenta,i,w_fechaGo,'D',w_diastotal);
     end if;
     commit;
    END LOOP;
  END LOOP;
END;                                                             
/*Tipo de Cliente*/
function fun_tipo_cliente(pni_cliente in number) return varchar2 is
 v_codeiso varchar2(3); 
begin        
begin
select codeiso
  into v_codeiso
from tcli_natural,tgen_desctabla
where nat_tabsubtipcliente   = des_codtab 
  and nat_subtipocliente = des_codigo    
  and nat_codcli = pni_cliente; 
exception
  when no_data_found then
select codeiso
  into v_codeiso
from tcli_juridica,tgen_desctabla
where jur_tabsubtipcliente   = des_codtab 
  and jur_subtipocliente = des_codigo    
  and jur_codcli = pni_cliente;     
end;     
return v_codeiso;                                                 
exception
when others then
 return null;
end;                                                                    
/*Tipo de servicio y productos*/
function fun_producto_servicio(pni_operacion in number,p_modulo in number) return varchar2 is
 v_codeiso varchar2(3); 
begin   
if p_modulo = 4 then  
select codeiso
  into v_codeiso
 from tcap_vista,tgen_desctabla
 where vis_tabprodserv = des_codtab
   and vis_prodserv = des_codigo
   and vis_numcue = pni_operacion;
elsif p_modulo = 5 then
select codeiso
  into v_codeiso
 from tpla_cuenta,tgen_desctabla
 where pda_tabprodserv = des_codtab
   and pda_prodserv = des_codigo
   and pda_cuenta = pni_operacion;
elsif p_modulo = 6 then
select codeiso
  into v_codeiso
 from tpre_prestamos,tgen_desctabla
 where pre_tabprodserv = des_codtab
   and pre_prodserv = des_codigo
   and pre_credito = pni_operacion;
elsif p_modulo = 10 then
  begin
	select codeiso
	  into v_codeiso
	 from tcollection,tgen_desctabla
	 where tabprodserv = des_codtab
	   and prodserv = des_codigo
	   and CODECOLLECTION = pni_operacion;
  exception
     when no_data_found then
       begin
		select codeiso
		  into v_codeiso
		 from tlettercredit,tgen_desctabla
		 where tabprodserv = des_codtab
		   and prodserv = des_codigo
		   and CODELETTER = pni_operacion;       
       exception
          when no_data_found then
              begin
					select codeiso
					  into v_codeiso
					 from tguarantee,tgen_desctabla
					 where tabprodserv = des_codtab
					   and prodserv = des_codigo
					   and CODEGUARANTEE = pni_operacion;       
			  exception
			     when no_data_found then
			       begin
					select codeiso
					  into v_codeiso
					 from tdraft,tgen_desctabla
					 where tabprodserv = des_codtab
					   and prodserv = des_codigo
					   and CODEDRAFT = pni_operacion;
			       end;
			end;
     
     end;      
  end;


elsif p_modulo = 15 then
select codeiso
  into v_codeiso
 from tcard,tgen_desctabla
 where tabprodserv = des_codtab
   and prodserv = des_codigo
   and CARD = pni_operacion;  
else
     v_codeiso:='620';
end if;
return  v_codeiso;
exception 
when others then
   return null;
end;
function fun_garantia(pni_operacion in number,p_fecha in date,P_REP IN VARCHAR2) return number   is
 garantia_adm NUMBER;
 v_estainscrita number:=0;
begin                    
         w_valorparam:= pkg_precalifica_do.parametro_calif('VALOR_GARANTIZADO_DE');
        IF NVL(w_valorparam,'R') = 'R' THEN
         select sum(nvl(log_valorgarantizadoreal,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = pni_operacion;
        ELSIF NVL(w_valorparam,'R') = 'C' THEN          
			select count(*)
			   into v_estainscrita
			  from TCLI_OPERGARAN
			  where OGR_OPERACION=pni_operacion
			   and OGR_FECPROT is not null;    
			   IF P_REP = 'DE13' THEN
			         select sum(nvl(log_valorgarantizadoreal,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
			           into garantia_adm
			         from tleg_opergaran
			        where log_fecha = p_fecha
			          and log_tipo = 'P'
			          and log_operacion = pni_operacion;
			   
			   ELSE
			   			   if v_estainscrita> 0 then
			      select sum(nvl(LCA_CAPITAL,0) + nvl(LCA_INTERES,0))
			        into garantia_adm
			      from tleg_operacioncalif
			      where LCA_CUENTA =  pni_operacion
			        and LCA_FECHA = p_fecha;
			   end if;    
			   END IF;
        ELSE
         select sum(nvl(log_valorgarantizadofisa,0))--sum(nvl(log_valorgarantizadofisa,0))--,sum(nvl(log_valorgarantizadoreal,0))
           into garantia_adm
         from tleg_opergaran
        where log_fecha = p_fecha
          and log_tipo = 'P'
          and log_operacion = pni_operacion;
        END IF ;     
        RETURN nvl(garantia_adm,0);
end;    
/*anio de garantia*/
function fun_aniogarantia(pni_cliente in number,p_numgaran in number) return number is
  v_anio number;
begin           
  begin
   select ghp_anio
     into v_anio
 from tgar_hipoteca
 where ghp_codcli = pni_cliente
   and ghp_numero = p_numgaran
   and GHP_FECHASTA is null;
 exception
    when no_data_found then
       begin
		   select gmq_anio
		     into v_anio
		 from tgar_prendmaq
		 where gmq_codcli = pni_cliente
		   and gmq_numero = p_numgaran
		   and GMQ_FECHASTA is null;
       exception
        when no_data_found then
		       begin
				   select to_char(gvh_anio,'yyyy')
				     into v_anio
				 from tgar_prendveh
				 where gvh_codcli = pni_cliente
				   and gvh_numero = p_numgaran
				   and GVH_FECHASTA is null;
		       exception
		        when others then
		           return null;
		       end;       
       end ;
 end; 
 return v_anio;  
end;
/*actividad econocmica*/
function fun_actividad_cliente(pni_cliente in number) return varchar is
   v_codeiso varchar2(8);
begin
  begin                                    
  dbms_output.put_line('PAso 0:'||pni_cliente);
  begin
  select codeiso
    into  v_codeiso
  from tcli_natural,tgen_desctabla
  where NAT_ACTSEC = des_codigo 
     and NVL(NAT_TABACTSEC,18)=des_codtab
     and nat_codcli = pni_cliente;
 exception 
    when no_data_found then 
  select codeiso
    into  v_codeiso
  from tcli_natural,tgen_desctabla
  where NAT_ACTPRIN = des_codigo 
     and NVL(NAT_TABACTPRIN,18)=des_codtab
     and nat_codcli = pni_cliente;                  
 end ;     
dbms_output.put_line('PAso 1:'||v_codeiso);
if v_codeiso is null then 
  select codeiso
    into  v_codeiso
  from tcli_natural,tgen_desctabla
  where NAT_ACTPRIN = des_codigo 
     and NVL(NAT_TABACTPRIN,18)=des_codtab
     and nat_codcli = pni_cliente;         
dbms_output.put_line('PAso 2:'||v_codeiso);     
end if;     
 exception
   when no_data_found then
  select codeiso
    into  v_codeiso
  from tcli_juridica,tgen_desctabla
  where jur_actividad = des_codigo 
     and NVL(JUR_CODTABACT,19)=des_codtab
     and jur_codcli = pni_cliente;     
   end;                     
   dbms_output.put_line('PAso 3:'||v_codeiso);
   return v_codeiso;
end; 
/*peps*/
function fun_es_peps(pni_tipoid in varchar2,pni_identificacion in varchar2, p_tipopep out varchar2) return varchar is
v_numero number; 
v_tipopep varchar2(1);
begin
 select count(*)
   into v_numero
  from TDOM_PEES
  where EMP_TIPOID = pni_tipoid
    and EMP_IDENTIFI = pni_identificacion;
 select EMP_TIOPOPEP
   into v_tipopep
  from TDOM_PEES
  where EMP_TIPOID = pni_tipoid
    and EMP_IDENTIFI = pni_identificacion;
     p_tipopep:=v_tipopep;
    if v_numero > 0 then
       return 'S';
    else
      return 'N';
    end if;       
exception
  when others then
     p_tipopep:=null;
     return 'N';
end;
/*localidad*/
function fun_localidad(p_sucursal in number,p_oficina in number,pni_codcli in number,pni_numdir in number) return varchar2 is
 v_codeiso varchar2(10);
 v_pais    number(4);
 
 begin         
 --junio 2015 , localidad del cliente
     select codeiso ,dir_pais
       into v_codeiso,v_pais
      from  tgen_desctabla, tcli_direccion
     where  des_codtab = dir_tabciudad
       and  des_codigo = dir_ciudad
       and  dir_codcli = pni_codcli
       and  dir_numero = pni_numdir;
       if v_pais = 1 then
          return v_codeiso;
       else                 
       
            select a.codeiso
            Into v_codeiso
            from tgen_desctabla a,tgen_oficina b
           where ofi_tabciu = des_codtab 
             and ofi_codciu = des_codigo
             and ofi_codsuc = p_sucursal
             and ofi_codofi = p_oficina;
       end if;
      return v_codeiso;       
 exception       
   when no_data_found then 
      return '10101';
    when others then
      return null;  
 end;
/*fin de localidad*/ 
/*destinino economico de ciiu*/
function fun_destino_ciiu(pni_codcli in number,pni_operacion in number,pni_compromiso out varchar2) return varchar2 is
v_numcomprom varchar2(10);
destino_credito varchar2(10);
begin            
     if  pni_operacion <> 0 then
        BEGIN
          select substr(lpad(DES_CODIGOCIIU,6,'0'), 1, 6)
            Into destino_credito
            from tpre_prestamos, tpre_destecon
           where pre_destecon = des_codigo 
             and pre_credito = pni_operacion;
        Exception
          when no_data_found then
        BEGIN
          select DES_CODIGOCIIU, CODELINE
            into destino_credito, v_numcomprom
            from tclientline, tpre_destecon
           where ECONOMICDESTINITY = DES_CODIGO(+) and
                 CODEOPERATION = pni_operacion;
        EXCEPTION
          WHEN OTHERS THEN
            destino_credito := null;
            v_numcomprom    := null;
        END;          
          When others then
            destino_credito := null;
        END;                    
   end if;    
   pni_compromiso:= v_numcomprom;
   return destino_credito;
end;               
/*Tipo Tasa*/     
function fun_tipo_tasa(pni_fecha in date,pni_operacion in number) return varchar2 IS
  w_reajuste number:=0;
  w_reajuste2 number:=0;                     
  V_PRO NUMBER;
  V_TIP NUMBER;
  V_MONEDA NUMBER;
BEGIN                                                                                  
  /*select count(*)
    into w_reajuste
   from tpre_tasaantreaj
   where rea_tasaant <> rea_tasanueva
     and rea_credito = pni_operacion
     and trunc(rea_fecharea) < pni_fecha;*/
     SELECT PRE_TIPOTASAPLAZO,PRE_PRO, PRE_TIP,PRE_MONEDA 
       INTO w_reajuste,V_PRO, V_TIP,V_MONEDA 
       FROM TPRE_PRESTAMOS
      WHERE PRE_CREDITO = PNI_OPERACION ;
      
     if nvl(w_reajuste,0) = 2 then 
        RETURN 'V';                
     ELSif nvl(w_reajuste,0) = 1 then         
        RETURN 'F';
     else          
          SELECT  nvl(PTC_TIPOTASA,2)   
            INTO  w_reajuste2
            FROM TPRE_PROTIPOCREDITO
            WHERE PTC_MOD =  6
              AND PTC_PRO = V_PRO       
              AND PTC_TIP = V_TIP
			  AND PTC_MON = V_MONEDA;
           IF nvl(w_reajuste2,0) = 2 THEN
			           RETURN 'V';
		    ELSE   
		                     RETURN 'F';
		    END IF;              

     end if;   
        
END ;
/*Fin Tipo de tasa*/
function fun_iso_desctabla(pni_codtab in number,pni_codigo in number) return varchar2 IS
W_ISO tgen_desctabla.codeiso%type;
BEGIN                       
  select codeiso 
    into W_ISO
   from tgen_desctabla
   where des_codtab = pni_codtab
     and des_codigo = pni_codigo;
     return W_ISO;
exception
   when others then
     return null;     
END;
--
END;

/
