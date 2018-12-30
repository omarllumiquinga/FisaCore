CREATE OR REPLACE
PACKAGE BODY PKG_CMS_CREDITCARDLOAD AS
--VALORES DINAMICOS DE FUNCIONES
--ultima modificacion 2005/08/17-5:34pm
  FUNCTION obt_valuedin (
    p_module       in  number,
    p_archivo      in  varchar2,
    p_sec          in number,
    p_tabla        in varchar2,
    p_columna      in varchar2,
    p_typeinf      in  varchar2,
    p_paramin      in  varchar2,
    p_valuein      in  varchar2
  )RETURN VARCHAR2 IS
    w_function   varchar2(1000);
    w_modulo     tgar_funcmodulo.gif_funcion%type;
    w_value      varchar2(1000);
    t_param      tab_param;
    w_funcaux    varchar2(1000);
  BEGIN
    begin
      select PDD_VALORCOLUMAN
        into w_function
        from tcar_paramcargadatosdet
       where PDD_MOD     = p_module
         and PDD_NOMARCHIVO  = p_archivo
         and PDD_SEC  = p_sec
         and PDD_TABLADES = p_tabla
         and PDD_COLUMNA = p_columna;
    exception
      when no_data_found then
           return null;
    end;

    entrance_param(p_typeinf,p_paramin,p_valuein,t_param);
        dbms_output.put_line('p_valuein:'||p_valuein||' w_function:'||w_function);
    save_log('  1 dinamico w_function:'||w_function);
    w_funcaux  := convert_sentence(w_function,t_param);
    w_function := 'SELECT '||w_funcaux||
                  ' value FROM DUAL ';
    BEGIN

    save_log(' dinamico w_function:'||w_function);
    dbms_output.put_line('w_function:'||w_function);
      execute immediate w_function INTO w_value;
    EXCEPTION
      WHEN others THEN
           raise_application_error(-20101,'No se ejecuto funcion dinamica func:'||w_funcaux||' '||sqlerrm);
    END;
    return ltrim(rtrim(w_value));
  END;

 --VERIFICACIONE  DE PARAMETROS
   PROCEDURE entrance_param(
    p_typeinf   in     varchar2,
    p_paramin   in     varchar2,
    p_valuein   in     varchar2,
    p_tabparam  in out tab_param
  ) IS
    i integer;
    w_countp integer;
    w_countv integer;
    w_posp   integer;
    w_posv   integer;
    w_auxp   varchar2(30);
    w_auxv   varchar2(200);
    w_auxparamin  varchar2(500);
    w_auxvaluein  varchar2(500);
  BEGIN
    p_tabparam    := tab_param();
    w_auxparamin  := p_paramin;
    w_auxvaluein  := p_valuein;
    w_countp      := length(w_auxparamin);
    w_countv      := length(w_auxvaluein);
    i             := 0;
    while w_countp > 0 and w_countv > 0 loop
      i := i + 1;
      w_posp      := instr(w_auxparamin,',',1);
      w_posv      := instr(w_auxvaluein,',',1);

      if (nvl(w_posp,0) = 0 and nvl(w_posv,0) != 0) or
         (nvl(w_posv,0) = 0 and nvl(w_posp,0) != 0)then
         raise_application_error(-20100,'No existe correspondencia entre No. Parametros vs. No.valores. inf:'||p_typeinf);
      end if;
      if w_posp > 0 then
         w_auxp       := substr(w_auxparamin,1,w_posp-1);
         w_auxv       := substr(w_auxvaluein,1,w_posv-1);
         w_auxparamin := substr(w_auxparamin,w_posp+1,w_countp);
         w_auxvaluein := substr(w_auxvaluein,w_posv+1,w_countv);
      else
         w_auxp       := w_auxparamin;
         w_auxv       := w_auxvaluein;
         w_auxparamin := null;
         w_auxvaluein := null;
      end if;

      p_tabparam.extend;
      p_tabparam(i).param := w_auxp;
      p_tabparam(i).value := w_auxv;

      w_countp      := length(w_auxparamin);
      w_countv      := length(w_auxvaluein);
    end loop;

    if w_countp != 0 or w_countv != 0 then
       raise_application_error(-20100,'No existe correspondencia entre No. Parametros vs. No.valores. inf:'||p_typeinf);
    end if;

  END;

  FUNCTION convert_sentence(
    p_sentencein in varchar2,
    p_tabparam   in tab_param
  ) RETURN VARCHAR2 IS
    w_count       integer;
    w_newsentence varchar2(500) := p_sentencein;
  BEGIN
    w_count := p_tabparam.count;
    FOR i in 1..w_count LOOP
        dbms_output.put_line(' 1 w_newsentence:'||w_newsentence||' upper(p_tabparam(i).param):'||upper(p_tabparam(i).param)||' p_tabparam(i).value:'||p_tabparam(i).value);
        w_newsentence := REPLACE(w_newsentence,upper(p_tabparam(i).param),p_tabparam(i).value);
        dbms_output.put_line('2 w_newsentence:'||w_newsentence||' upper(p_tabparam(i).param):'||upper(p_tabparam(i).param)||' p_tabparam(i).value:'||p_tabparam(i).value);
    END LOOP;
    RETURN w_newsentence;
  END;
--Para Archivos de Salida
PROCEDURE generic_file(
   p_report      in  varchar2,
   p_session     in  number,
   p_fecha       in  date
  )IS
    CURSOR rows IS
    SELECT DISTINCT rowsid
      FROM tvaluelevelcustom
     WHERE report    = p_report
       AND sessionid = p_session;
    CURSOR column (pp_rowsid in number) IS
    SELECT a.columsid,a.valuenum,a.valuechar,columntyp,columnlen,filling,concaten,formatcol
      FROM tvaluelevelcustom a,ttypecolumnreport b
     WHERE a.report    = p_report
       AND a.sessionid = p_session
       AND a.rowsid    = pp_rowsid
       AND b.report    = a.report
       AND b.columnid  = a.columsid
     ORDER BY columnid;
    w_line      varchar2(4000);
    w_columna   varchar2(4000);
    w_file      UTL_FILE.FILE_TYPE;
    w_obtfile   boolean;
    w_directory varchar2(100);
    lnNot_open  EXCEPTION;
  BEGIN
          pgdebug_2('1 ',33);
    w_directory := PKG_LEGALREPORT3.to_directory();
          pgdebug_2('2 ',34);
    --w_file      := UTL_FILE.FOPEN(w_directory, p_report||TO_CHAR(P_FECHA), 'w');
    w_file      := UTL_FILE.FOPEN(w_directory, p_report||'.DAT', 'w');
              pgdebug_2('3 ',34);
    w_obtfile   := UTL_FILE.IS_OPEN( w_file );
          pgdebug_2('4 ',35);
    IF w_obtfile = TRUE THEN
       FOR cur_row IN rows LOOP
           w_line := null;
           FOR cur_col  IN column(cur_row.rowsid) LOOP
              if cur_col.columntyp = 'N' then
                   /*If cur_col.formatcol is not null then
                      cur_col.valuenum := obt_formatvalue(cur_col.columntyp, cur_col.valuenum, cur_col.formatcol);
                   End if;*/
                   w_columna := PKG_CMS_INTERFAZCREDITCARD.to_format(cur_col.valuenum,cur_col.columntyp,cur_col.columnlen,cur_col.filling);
                   w_line := w_line||w_columna;
              else
                   If cur_col.formatcol is not null then
                      cur_col.valuechar := pkg_legalreport3.obt_formatvalue(cur_col.columntyp, cur_col.valuechar, cur_col.formatcol);
                   End if;
                  if cur_col.columntyp = 'C' then
                   w_columna := PKG_CMS_INTERFAZCREDITCARD.to_formatR(cur_col.valuechar,cur_col.columntyp,cur_col.columnlen,cur_col.filling);
                   w_line := w_line||w_columna;
                  else
                   w_columna := PKG_CMS_INTERFAZCREDITCARD.to_format(cur_col.valuechar,cur_col.columntyp,cur_col.columnlen,cur_col.filling);
                   w_line := w_line||w_columna;
                  end if;
              end if;
           END LOOP;
           UTL_FILE.PUT(w_file,w_line);
           UTL_FILE.NEW_LINE(w_file,1);
       END LOOP;
    ELSE
       RAISE lnNot_open;
    END IF;
    UTL_FILE.FCLOSE (w_file);
    EXCEPTION
       WHEN lnNot_open THEN
           UTL_FILE.FCLOSE(w_file);
           RAISE_APPLICATION_ERROR(-20900,'ERROR: lnNot_open');
       WHEN NO_DATA_FOUND THEN
           UTL_FILE.FCLOSE(w_file);
           RAISE_APPLICATION_ERROR(-20900,'ERROR: no data found');
       WHEN value_error THEN
           UTL_FILE.FCLOSE(w_file);
           RAISE_APPLICATION_ERROR(-20900,'ERROR: value error');
       WHEN UTL_FILE.INVALID_FILEHANDLE THEN
           UTL_FILE.FCLOSE(w_file);
           RAISE_APPLICATION_ERROR(-20900,'ERROR: invalid file handle');
       WHEN UTL_FILE.INVALID_OPERATION THEN
           UTL_FILE.FCLOSE(w_file);
           RAISE_APPLICATION_ERROR(-20900,'ERROR: invalid operation');
       WHEN UTL_FILE.READ_ERROR THEN
           UTL_FILE.FCLOSE(w_file);
           RAISE_APPLICATION_ERROR(-20900,'ERROR: file read');
       WHEN UTL_FILE.WRITE_ERROR THEN
           UTL_FILE.FCLOSE(w_file);
           RAISE_APPLICATION_ERROR(-20900,'ERROR: write error');
       WHEN UTL_FILE.INVALID_PATH THEN
           UTL_FILE.FCLOSE(w_file);
           RAISE_APPLICATION_ERROR(-20900,'ERROR: invalid path'||' '||w_directory);
       WHEN UTL_FILE.INVALID_MODE THEN
           UTL_FILE.FCLOSE(w_file);
           RAISE_APPLICATION_ERROR(-20900,'ERROR: invalid mode');
  END;
--fin de archivos de salida
--Carga de rgistros contables
PROCEDURE carga_contabilidad(
   p_usuario IN NUMBER,
   p_sucursal IN NUMBER,
   p_oficina IN NUMBER,
   p_terminal IN VARCHAR2,
   p_fecha     IN  DATE
  ) IS
 CURSOR C0 IS
  SELECT DISTINCT CRC_SUCURSAL,
                  CRC_OFICINA,
                  CRC_COMPROBANTE
    FROM TCAR_REGCONTABLE
   WHERE NVL(CRC_PROCESADO,'N')= 'N';

  CURSOR C(SUCURSAL IN VARCHAR, OFICINA IN VARCHAR, COMPROBANTE IN VARCHAR) IS
   SELECT ROWID,CRC_SUCURSAL,CRC_OFICINA,CRC_NROTARJETA,CRC_CTACONTABLE,CRC_DEBCRE,CRC_FECHAS,
		  CRC_TIPOAS,CRC_TIPOCARTERA,CRC_TIPOPROC,CRC_TIPOCOMPRO,CRC_COMPROBANTE,
	      CRC_MONEDA,CRC_COTIZACION,CRC_IMPMONLOCAL,CRC_IMPMONINTER,CRC_DESCRIPCION,CRC_SISTEMAORG,
		  CRC_AUXCONTABLE,CRC_RESPON,CRC_OFICINARES,CRC_CODTRA,CRC_NUMTRA,CRC_SUCDES,CRC_AGDES,
          CRC_DIVDEST,CRC_DPTODEST,CRC_USUARIO,CRC_CTASECUNDARIA,CRC_APLICACION,
		  CRC_NRODOC,CRC_HORTRA,CRC_ORIGEN,CRC_MARCAPROD,CRC_TIPTRA
          CRC_ESTMOV,CRC_FECAPROC,CRC_PROCESADO,CRC_TABDEPTO,CRC_DEPTO,CRC_MAYOR,
          CRC_TERMINAL,CRC_SECUENCIA
   FROM TCAR_REGCONTABLE
   WHERE NVL(CRC_PROCESADO,'N')= 'N'
     AND CRC_SUCURSAL = SUCURSAL
     AND CRC_OFICINA = OFICINA
     AND CRC_COMPROBANTE = COMPROBANTE;
   sectran      NUMBER;
   v_cotiza     NUMBER;
   V_VALORLOCAL TCON_TRANSA.TSA_VALOR%TYPE;
   V_VALORINTER TCON_TRANSA.TSA_VALOR%TYPE;
   fechainiciocol DATE;
   fechafincol DATE;
   v_commit    VARCHAR2(1):='T';
   procesados     NUMBER;
   por_procesar   NUMBER;
   V_CURRENCY NUMBER(2);
   V_ERROR VARCHAR2(80);
   V_NUMDEC TGEN_MONEDA.MON_NUMDEC%TYPE;
   V_SECUENCIA NUMBER:=0;
   V_DESCRIPCION  TCAR_REGCONTABLE.CRC_DESCRIPCION%TYPE;
   V_TABDEPTO TCAR_REGCONTABLE.CRC_TABDEPTO%TYPE;
   V_DEPTO    TCAR_REGCONTABLE.CRC_DEPTO%TYPE;
   V_MAYOR    TCAR_REGCONTABLE.CRC_MAYOR%TYPE;
   V_HORTRA   TCAR_REGCONTABLE.CRC_HORTRA%TYPE;
   V_TERMINAL TCAR_REGCONTABLE.CRC_TERMINAL%TYPE;
   V_COSTO NUMBER;
   V_TOTAL_DEBITO NUMBER :=0;
   V_TOTAL_CREDITO  NUMBER:=0;
 BEGIN
    fechainiciocol := SYSDATE;
    pkg_genbatch.inicio_logheader(20,p_fecha, fechainiciocol, p_usuario, p_sucursal,
                                  p_oficina, p_terminal, 'S');
    pkg_genbatch.graba_log_modulo_calif(20,P_fecha, 'INICIO CARGA CONTABILIDAD','INF_INICIO', fechainiciocol);
    COMMIT;
FOR Z IN C0 LOOP
    SAVE_LOG('PASO 0:'||v_commit||' '||z.crc_sucursal);
    V_SECUENCIA :=0;
    v_commit  :='T';
    BEGIN
     sectran:=upd_secdestran(TO_NUMBER(z.crc_sucursal));
    EXCEPTION
	WHEN OTHERS THEN
      ROLLBACK;
      v_commit  :='F';
      V_ERROR := SUBSTR(SQLERRM,1,50);
      pkg_genbatch.graba_log_modulo_calif(20,P_fecha,'Secuencia no puede generar para suc' ||z.crc_sucursal||V_ERROR,'ERRPRO');
      COMMIT;
	 END;
     SAVE_LOG('PASO 1:'||v_commit||'-'||sectran);
     IF v_commit ='T' THEN
       BEGIN
       SELECT CRC_DESCRIPCION,CRC_TABDEPTO,CRC_DEPTO,CRC_MAYOR,CRC_HORTRA,CRC_TERMINAL
         INTO V_DESCRIPCION,V_TABDEPTO,V_DEPTO,V_MAYOR,V_HORTRA,V_TERMINAL
         FROM TCAR_REGCONTABLE
        WHERE CRC_SUCURSAL    = Z.CRC_SUCURSAL
          AND CRC_OFICINA     = Z.CRC_OFICINA
          AND CRC_COMPROBANTE = Z.CRC_COMPROBANTE
          AND ROWNUM = 1;
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' tcon_destra-Error Comprobante ' ||'-'||V_DESCRIPCION||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	   END;
     END IF;
     IF v_commit ='T' THEN
       BEGIN
       	INSERT INTO TCON_DESTRAN(  DST_NUMTRAN ,DST_SUCURSAL,DST_OFICINA ,DST_FECHA, DST_DESCRIPC ,
								   DST_TABDEPTO,DST_DEPTO,DST_MAYOR,DST_USUARIO,DST_HORA,DST_TERMINAL,
								   DST_CUADRA,DST_NUMREF,DST_LIQUIDA,DST_AUTORIZA)
	             VALUES(sectran,TO_NUMBER(z.crc_sucursal),z.crc_oficina,p_fecha,V_descripcion,V_tabdepto,
	                    V_depto,V_mayor,p_usuario,V_HORTRA,V_TERMINAL,
	                    'S',NULL,NULL,NULL);
        SAVE_LOG('PASO 2:'||v_commit||'-'||sectran);
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' tcon_destra-Error Comprobante ' ||'-'||V_DESCRIPCION||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
	    END IF;
        --detalle del comprobante
        SAVE_LOG('PASO 3:'||v_commit||'-'||sectran);
        V_TOTAL_DEBITO  :=0;
        V_TOTAL_CREDITO :=0;
  FOR X IN C(Z.CRC_SUCURSAL,Z.CRC_OFICINA,Z.CRC_COMPROBANTE) LOOP
           V_SECUENCIA := V_SECUENCIA + 1;
     IF v_commit ='T' THEN
      BEGIN
           		SELECT MOP_MONEDA
                  INTO V_CURRENCY
                  FROM TGEN_MAPMONEDA
                 WHERE MOP_CODINSTIT = 1 --BDI
                   AND MOP_MONEQUIV = X.CRC_MONEDA;
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' TGEN_MAPMONEDA-Error Comprobante ' ||'-'||V_DESCRIPCION||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
	    END IF;
     IF v_commit ='T' THEN
      BEGIN
               SELECT MON_NUMDEC
                 INTO V_NUMDEC
                FROM TGEN_MONEDA
               WHERE MON_COD = V_CURRENCY;
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' TGEN_MONEDA-Error Comprobante ' ||'-'||V_DESCRIPCION||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
	    END IF;
      IF v_commit ='T' THEN
         BEGIN
     	 IF V_CURRENCY <> 0 THEN
     	    V_COTIZA := BDI_PROMEDIO(V_CURRENCY,X.CRC_SUCURSAL,P_FECHA);
     	    V_VALORLOCAL :=ROUND((TO_NUMBER(X.CRC_IMPMONLOCAL)/100)* V_COTIZA,V_NUMDEC) ;--ROUND(NVL( TO_NUMBER(SUBSTR(X.CRC_IMPMONLOCAL,1,LENGTH(X.CRC_IMPMONLOCAL)-2)||'.'||SUBSTR((X.CRC_IMPMONLOCAL),LENGTH(X.CRC_IMPMONLOCAL)-1,2)),0) * V_COTIZA,V_NUMDEC);
     	    V_VALORINTER := ROUND((TO_NUMBER(X.CRC_IMPMONLOCAL)/100),V_NUMDEC); --ROUND(NVL(TO_NUMBER(SUBSTR((X.CRC_IMPMONLOCAL),1,LENGTH(X.CRC_IMPMONLOCAL)-2)||'.'||SUBSTR((X.CRC_IMPMONLOCAL),LENGTH(X.CRC_IMPMONLOCAL)-1,2)),0),V_NUMDEC);--CREO QUE ESTABA QUILLAO
     	    --V_VALORINTER := ROUND(NVL(X.CRC_IMPMONINTER,0),V_NUMDEC);
     	 ELSE
     	       V_VALORINTER := NULL;
     	       V_COTIZA := NULL;
     	       V_VALORLOCAL := ROUND((TO_NUMBER(X.CRC_IMPMONLOCAL)/100),V_NUMDEC); --ROUND(NVL(TO_NUMBER(SUBSTR((X.CRC_IMPMONLOCAL),1,LENGTH(X.CRC_IMPMONLOCAL)-2)||'.'||SUBSTR((X.CRC_IMPMONLOCAL),LENGTH(X.CRC_IMPMONLOCAL)-1,2)),0),V_NUMDEC);
	         SAVE_LOG('PASO 3 V_VALORLOCAL:'||V_VALORLOCAL||' V_CURRENCY:'||V_CURRENCY);
     	 END IF;
	         SAVE_LOG('PASO 4:'||v_commit||'-'||sectran||' V_VALORLOCAL:'||V_VALORLOCAL||' V_CURRENCY:'||V_CURRENCY);
	    EXCEPTION

	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' tasa Promedio 1:' ||V_CURRENCY||'-'||X.CRC_SUCURSAL||'-'||X.CRC_MONEDA||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
      END IF;
	    	         SAVE_LOG('PASO 5:'||v_commit||'-'||sectran);
     IF v_commit ='T' THEN
         IF X.CRC_SISTEMAORG = 'CC' THEN --PROCECARD
            V_COSTO := 100;--TARJETA INGRESA A NEGOCIO --JEAN FROMETA 2005/08/17
         ELSIF X.CRC_SISTEMAORG = 'NM' THEN
            V_COSTO := X.CRC_DEPTO;
         ELSE
            V_COSTO := X.CRC_DEPTO;
         END IF;

      BEGIN
	    INSERT INTO TCON_TRANSA(TSA_NUMTRAN,TSA_SUCURSAL,TSA_OFICINA,TSA_SECUENCIA,TSA_CUENTA,TSA_TIPO,TSA_VALOR,
							    TSA_VALORME,TSA_COTIZA,TSA_GLOSA,TSA_TABCOSTO,TSA_COSTO,TSA_REFERENCIA,TSA_FECHATRAN,
							    TSA_REFPRODCLI,TSA_TERMINAL,TSA_NUMREF,TSA_LIQUIDA)
                           VALUES(sectran,TO_NUMBER(x.crc_sucursal),x.crc_oficina,V_SECUENCIA,x.crc_ctacontable,DECODE(x.crc_debcre,'1','D','C'),
                                  V_VALORLOCAL,V_VALORINTER,V_COTIZA,X.CRC_NROTARJETA||'-'||X.CRC_CODTRA||'-'||X.CRC_NUMTRA||'-'||x.CRC_DESCRIPCION||'-!'||NVL(X.CRC_SISTEMAORG,'CC')||'!-',
                                  96,V_COSTO,NULL,NULL,NULL,V_TERMINAL,X.CRC_COMPROBANTE,NULL);
     	    	         SAVE_LOG('PASO 6:'||v_commit||'-'||sectran);
         --PARA IDENTIFICAR COMP DESCUADRADADO
            IF x.crc_debcre = '1' THEN
              V_TOTAL_DEBITO := NVL(V_TOTAL_DEBITO,0) + NVL(V_VALORLOCAL,0);
            ELSE
              V_TOTAL_CREDITO := NVL(V_TOTAL_CREDITO,0) + NVL(V_VALORLOCAL,0);
            END IF;
         --FIN
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' tcon_transa-Comp' ||V_ERROR,'ERRPRO');--x.crc_codtra||'-'||x.crc_numtra||
           COMMIT;
	    END;
     	    	         SAVE_LOG('PASO 7:'||v_commit||'-'||sectran);
      END IF;
       IF v_commit ='F' THEN
          EXIT;
       END IF;
     	    	         SAVE_LOG('PASO 8:'||v_commit||'-'||sectran);
  END LOOP;
  /*  IF NVL(V_TOTAL_CREDITO,0)<> NVL(V_TOTAL_DEBITO,0) THEN
        v_commit :='F';
        ROLLBACK;
        V_ERROR := SUBSTR(SQLERRM,1,80);
        pkg_genbatch.graba_log_modulo_calif(20,P_fecha,'Conta Procecard Descuadrada Comp No cuadra:'||Z.CRC_SUCURSAL||'-'||Z.CRC_OFICINA||'-'||Z.CRC_COMPROBANTE,'ERRPRO');--x.crc_codtra||'-'||x.crc_numtra||
        COMMIT;
        --EXIT;
     END IF;*/
     IF v_commit ='T' THEN
         UPDATE  TCAR_REGCONTABLE
            SET CRC_PROCESADO = 'S'
          WHERE CRC_SUCURSAL = Z.CRC_SUCURSAL
            AND CRC_OFICINA = Z.CRC_OFICINA
            AND CRC_COMPROBANTE = Z.CRC_COMPROBANTE;
         COMMIT;
     END IF;
END LOOP;
      SAVE_LOG('PASO 7');
      SELECT COUNT(*)
        INTO por_procesar
        FROM TCAR_REGCONTABLE
       WHERE  NVL(CRC_PROCESADO,'N')= 'N';
	    	         SAVE_LOG('PASO 8');
      SELECT COUNT(*)
        INTO procesados
        FROM TCAR_REGCONTABLE
       WHERE   NVL(CRC_PROCESADO,'N')= 'S';
	    	         SAVE_LOG('PASO 9');
      fechafincol := SYSDATE;
      pkg_genbatch.graba_log_modulo_calif(20, P_fecha, 'FIN CARGA CONTABILIDAD','INF_FINBATCH', fechafincol);
      pkg_genbatch.fin_logheader(20, fechainiciocol, fechafincol, por_procesar,
                                 procesados);
      COMMIT;
      SAVE_LOG('PASO 10');
  EXCEPTION
	      WHEN OTHERS THEN
           v_commit  :='F';
           ROLLBACK;
           V_ERROR := SUBSTR(SQLERRM,1,50);
            fechafincol := SYSDATE;
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' FIN INESPERADO '||V_ERROR,'INF_FINBATCH');
           pkg_genbatch.fin_logheader(20, fechainiciocol, fechafincol, por_procesar,procesados);
           COMMIT;
 END;
---carga nomina

PROCEDURE contabilidad_nomina(
   p_usuario IN NUMBER,
   p_sucursal IN NUMBER,
   p_oficina IN NUMBER,
   p_terminal IN VARCHAR2,
   p_fecha     IN  DATE
  ) IS
 CURSOR C0 IS
  SELECT DISTINCT CRC_SUCURSAL,
                  CRC_OFICINA,
                  CRC_COMPROBANTE
    FROM TCAR_REGCONTABLE
   WHERE NVL(CRC_PROCESADO,'N')= 'N';

  CURSOR C(SUCURSAL IN VARCHAR, OFICINA IN VARCHAR, COMPROBANTE IN VARCHAR) IS
   SELECT ROWID,CRC_SUCURSAL,CRC_OFICINA,CRC_NROTARJETA,CRC_CTACONTABLE,CRC_DEBCRE,CRC_FECHAS,
		  CRC_TIPOAS,CRC_TIPOCARTERA,CRC_TIPOPROC,CRC_TIPOCOMPRO,CRC_COMPROBANTE,
	      CRC_MONEDA,CRC_COTIZACION,CRC_IMPMONLOCAL,CRC_IMPMONINTER,CRC_DESCRIPCION,CRC_SISTEMAORG,
		  CRC_AUXCONTABLE,CRC_RESPON,CRC_OFICINARES,CRC_CODTRA,CRC_NUMTRA,CRC_SUCDES,CRC_AGDES,
          CRC_DIVDEST,CRC_DPTODEST,CRC_USUARIO,CRC_CTASECUNDARIA,CRC_APLICACION,
		  CRC_NRODOC,CRC_HORTRA,CRC_ORIGEN,CRC_MARCAPROD,CRC_TIPTRA
          CRC_ESTMOV,CRC_FECAPROC,CRC_PROCESADO,CRC_TABDEPTO,CRC_DEPTO,CRC_MAYOR,
          CRC_TERMINAL,CRC_SECUENCIA, CRC_CCOSTO
   FROM TCAR_REGCONTABLE
   WHERE NVL(CRC_PROCESADO,'N')= 'N'
     AND CRC_SUCURSAL = SUCURSAL
     AND CRC_OFICINA = OFICINA
     AND CRC_COMPROBANTE = COMPROBANTE;
   sectran      NUMBER;
   v_cotiza     NUMBER;
   V_VALORLOCAL TCON_TRANSA.TSA_VALOR%TYPE;
   V_VALORINTER TCON_TRANSA.TSA_VALOR%TYPE;
   fechainiciocol DATE;
   fechafincol DATE;
   v_commit    VARCHAR2(1):='T';
   procesados     NUMBER;
   por_procesar   NUMBER;
   V_CURRENCY NUMBER(2);
   V_ERROR VARCHAR2(80);
   V_NUMDEC TGEN_MONEDA.MON_NUMDEC%TYPE;
   V_SECUENCIA NUMBER:=0;
   V_DESCRIPCION  TCAR_REGCONTABLE.CRC_DESCRIPCION%TYPE;
   V_TABDEPTO TCAR_REGCONTABLE.CRC_TABDEPTO%TYPE;
   V_DEPTO    TCAR_REGCONTABLE.CRC_DEPTO%TYPE;
   V_COSTO    TCAR_REGCONTABLE.CRC_CCOSTO%TYPE;
   V_MAYOR    TCAR_REGCONTABLE.CRC_MAYOR%TYPE;
   V_HORTRA   TCAR_REGCONTABLE.CRC_HORTRA%TYPE;
   V_TERMINAL TCAR_REGCONTABLE.CRC_TERMINAL%TYPE;
   --V_COSTO NUMBER;
   V_TOTAL_DEBITO NUMBER :=0;
   V_TOTAL_CREDITO  NUMBER:=0;
 BEGIN
    fechainiciocol := SYSDATE;
    pkg_genbatch.inicio_logheader(20,p_fecha, fechainiciocol, p_usuario, p_sucursal,
                                  p_oficina, p_terminal, 'S');
    pkg_genbatch.graba_log_modulo_calif(20,P_fecha, 'INICIO CARGA CONTABILIDAD','INF_INICIO', fechainiciocol);
    COMMIT;
FOR Z IN C0 LOOP
    SAVE_LOG('PASO 0:'||v_commit||' '||z.crc_sucursal);
    V_SECUENCIA :=0;
    v_commit  :='T';
    BEGIN
     sectran:=upd_secdestran(TO_NUMBER(z.crc_sucursal));
    EXCEPTION
	WHEN OTHERS THEN
      ROLLBACK;
      v_commit  :='F';
      V_ERROR := SUBSTR(SQLERRM,1,50);
      pkg_genbatch.graba_log_modulo_calif(20,P_fecha,'Secuencia no puede generar para suc' ||z.crc_sucursal||V_ERROR,'ERRPRO');
      COMMIT;
	 END;
     SAVE_LOG('PASO 1:'||v_commit||'-'||sectran);
     IF v_commit ='T' THEN
       BEGIN
       SELECT CRC_DESCRIPCION,CRC_TABDEPTO,CRC_DEPTO,CRC_MAYOR,CRC_HORTRA,CRC_TERMINAL, CRC_CCOSTO
         INTO V_DESCRIPCION,V_TABDEPTO,V_DEPTO,V_MAYOR,V_HORTRA,V_TERMINAL,V_COSTO
         FROM TCAR_REGCONTABLE
        WHERE CRC_SUCURSAL    = Z.CRC_SUCURSAL
          AND CRC_OFICINA     = Z.CRC_OFICINA
          AND CRC_COMPROBANTE = Z.CRC_COMPROBANTE
          AND ROWNUM = 1;
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' tcon_destra-Error Comprobante ' ||'-'||V_DESCRIPCION||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	   END;
     END IF;
     IF v_commit ='T' THEN
       BEGIN
       	INSERT INTO TCON_DESTRAN(  DST_NUMTRAN ,DST_SUCURSAL,DST_OFICINA ,DST_FECHA, DST_DESCRIPC ,
								   DST_TABDEPTO,DST_DEPTO,DST_MAYOR,DST_USUARIO,DST_HORA,DST_TERMINAL,
								   DST_CUADRA,DST_NUMREF,DST_LIQUIDA,DST_AUTORIZA)
	             VALUES(sectran,TO_NUMBER(z.crc_sucursal),z.crc_oficina,p_fecha,V_descripcion,V_tabdepto,
	                    V_depto,V_mayor,p_usuario,V_HORTRA,V_TERMINAL,
	                    'S',NULL,NULL,NULL);
        SAVE_LOG('PASO 2:'||v_commit||'-'||sectran);
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' tcon_destra-Error Comprobante ' ||'-'||V_DESCRIPCION||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
	    END IF;
        --detalle del comprobante
        SAVE_LOG('PASO 3:'||v_commit||'-'||sectran);
        V_TOTAL_DEBITO  :=0;
        V_TOTAL_CREDITO :=0;
  FOR X IN C(Z.CRC_SUCURSAL,Z.CRC_OFICINA,Z.CRC_COMPROBANTE) LOOP
           V_SECUENCIA := V_SECUENCIA + 1;
     IF v_commit ='T' THEN
      BEGIN
           		SELECT MOP_MONEDA
                  INTO V_CURRENCY
                  FROM TGEN_MAPMONEDA
                 WHERE MOP_CODINSTIT = 1 --BDI
                   AND MOP_MONEQUIV = X.CRC_MONEDA;
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' TGEN_MAPMONEDA-Error Comprobante ' ||'-'||V_DESCRIPCION||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
	    END IF;
     IF v_commit ='T' THEN
      BEGIN
               SELECT MON_NUMDEC
                 INTO V_NUMDEC
                FROM TGEN_MONEDA
               WHERE MON_COD = V_CURRENCY;
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' TGEN_MONEDA-Error Comprobante ' ||'-'||V_DESCRIPCION||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
	    END IF;
      IF v_commit ='T' THEN
         BEGIN
     	 IF V_CURRENCY <> 0 THEN
     	    V_COTIZA := BDI_PROMEDIO(V_CURRENCY,X.CRC_SUCURSAL,P_FECHA);
     	    V_VALORLOCAL :=ROUND((TO_NUMBER(X.CRC_IMPMONLOCAL)/100)* V_COTIZA,V_NUMDEC) ;--ROUND(NVL( TO_NUMBER(SUBSTR(X.CRC_IMPMONLOCAL,1,LENGTH(X.CRC_IMPMONLOCAL)-2)||'.'||SUBSTR((X.CRC_IMPMONLOCAL),LENGTH(X.CRC_IMPMONLOCAL)-1,2)),0) * V_COTIZA,V_NUMDEC);
     	    V_VALORINTER := ROUND((TO_NUMBER(X.CRC_IMPMONLOCAL)/100),V_NUMDEC); --ROUND(NVL(TO_NUMBER(SUBSTR((X.CRC_IMPMONLOCAL),1,LENGTH(X.CRC_IMPMONLOCAL)-2)||'.'||SUBSTR((X.CRC_IMPMONLOCAL),LENGTH(X.CRC_IMPMONLOCAL)-1,2)),0),V_NUMDEC);--CREO QUE ESTABA QUILLAO
     	    --V_VALORINTER := ROUND(NVL(X.CRC_IMPMONINTER,0),V_NUMDEC);
     	 ELSE
     	       V_VALORINTER := NULL;
     	       V_COTIZA := NULL;
     	       V_VALORLOCAL := ROUND((TO_NUMBER(X.CRC_IMPMONLOCAL)/100),V_NUMDEC); --ROUND(NVL(TO_NUMBER(SUBSTR((X.CRC_IMPMONLOCAL),1,LENGTH(X.CRC_IMPMONLOCAL)-2)||'.'||SUBSTR((X.CRC_IMPMONLOCAL),LENGTH(X.CRC_IMPMONLOCAL)-1,2)),0),V_NUMDEC);
	         SAVE_LOG('PASO 3 V_VALORLOCAL:'||V_VALORLOCAL||' V_CURRENCY:'||V_CURRENCY);
     	 END IF;
	         SAVE_LOG('PASO 4:'||v_commit||'-'||sectran||' V_VALORLOCAL:'||V_VALORLOCAL||' V_CURRENCY:'||V_CURRENCY);
	    EXCEPTION

	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' tasa Promedio 1:' ||V_CURRENCY||'-'||X.CRC_SUCURSAL||'-'||X.CRC_MONEDA||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
      END IF;
	    	         SAVE_LOG('PASO 5:'||v_commit||'-'||sectran);
     IF v_commit ='T' THEN
         IF X.CRC_SISTEMAORG = 'CC' THEN --PROCECARD
            V_COSTO := 100;--TARJETA INGRESA A NEGOCIO --JEAN FROMETA 2005/08/17
         ELSIF X.CRC_SISTEMAORG = 'NM' THEN
            V_COSTO := X.CRC_CCOSTO;
         ELSE
            V_COSTO := X.CRC_CCOSTO;
         END IF;

      BEGIN
	    INSERT INTO TCON_TRANSA(TSA_NUMTRAN,TSA_SUCURSAL,TSA_OFICINA,TSA_SECUENCIA,TSA_CUENTA,TSA_TIPO,TSA_VALOR,
							    TSA_VALORME,TSA_COTIZA,TSA_GLOSA,TSA_TABCOSTO,TSA_COSTO,TSA_REFERENCIA,TSA_FECHATRAN,
							    TSA_REFPRODCLI,TSA_TERMINAL,TSA_NUMREF,TSA_LIQUIDA)
                           VALUES(sectran,TO_NUMBER(x.crc_sucursal),x.crc_oficina,V_SECUENCIA,x.crc_ctacontable,DECODE(x.crc_debcre,'1','D','C'),
                                  V_VALORLOCAL,V_VALORINTER,V_COTIZA,X.CRC_NROTARJETA||'-'||X.CRC_CODTRA||'-'||X.CRC_NUMTRA||'-'||x.CRC_DESCRIPCION||'-!'||NVL(X.CRC_SISTEMAORG,'CC')||'!-',
                                  96,V_COSTO,NULL,NULL,NULL,V_TERMINAL,X.CRC_COMPROBANTE,NULL);
     	    	         SAVE_LOG('PASO 6:'||v_commit||'-'||sectran);
         --PARA IDENTIFICAR COMP DESCUADRADADO
            IF x.crc_debcre = '1' THEN
              V_TOTAL_DEBITO := NVL(V_TOTAL_DEBITO,0) + NVL(V_VALORLOCAL,0);
            ELSE
              V_TOTAL_CREDITO := NVL(V_TOTAL_CREDITO,0) + NVL(V_VALORLOCAL,0);
            END IF;
         --FIN
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' tcon_transa-Comp' ||V_ERROR,'ERRPRO');--x.crc_codtra||'-'||x.crc_numtra||
           COMMIT;
	    END;
     	    	         SAVE_LOG('PASO 7:'||v_commit||'-'||sectran);
      END IF;
       IF v_commit ='F' THEN
          EXIT;
       END IF;
     	    	         SAVE_LOG('PASO 8:'||v_commit||'-'||sectran);
  END LOOP;
  /*  IF NVL(V_TOTAL_CREDITO,0)<> NVL(V_TOTAL_DEBITO,0) THEN
        v_commit :='F';
        ROLLBACK;
        V_ERROR := SUBSTR(SQLERRM,1,80);
        pkg_genbatch.graba_log_modulo_calif(20,P_fecha,'Conta Procecard Descuadrada Comp No cuadra:'||Z.CRC_SUCURSAL||'-'||Z.CRC_OFICINA||'-'||Z.CRC_COMPROBANTE,'ERRPRO');--x.crc_codtra||'-'||x.crc_numtra||
        COMMIT;
        --EXIT;
     END IF;*/
     IF v_commit ='T' THEN
         UPDATE  TCAR_REGCONTABLE
            SET CRC_PROCESADO = 'S'
          WHERE CRC_SUCURSAL = Z.CRC_SUCURSAL
            AND CRC_OFICINA = Z.CRC_OFICINA
            AND CRC_COMPROBANTE = Z.CRC_COMPROBANTE;
         COMMIT;
     END IF;
END LOOP;
      SAVE_LOG('PASO 7');
      SELECT COUNT(*)
        INTO por_procesar
        FROM TCAR_REGCONTABLE
       WHERE  NVL(CRC_PROCESADO,'N')= 'N';
	    	         SAVE_LOG('PASO 8');
      SELECT COUNT(*)
        INTO procesados
        FROM TCAR_REGCONTABLE
       WHERE   NVL(CRC_PROCESADO,'N')= 'S';
	    	         SAVE_LOG('PASO 9');
      fechafincol := SYSDATE;
      pkg_genbatch.graba_log_modulo_calif(20, P_fecha, 'FIN CARGA CONTABILIDAD','INF_FINBATCH', fechafincol);
      pkg_genbatch.fin_logheader(20, fechainiciocol, fechafincol, por_procesar,
                                 procesados);
      COMMIT;
      SAVE_LOG('PASO 10');
  EXCEPTION
	      WHEN OTHERS THEN
           v_commit  :='F';
           ROLLBACK;
           V_ERROR := SUBSTR(SQLERRM,1,50);
            fechafincol := SYSDATE;
           pkg_genbatch.graba_log_modulo_calif(20,P_fecha,' FIN INESPERADO '||V_ERROR,'INF_FINBATCH');
           pkg_genbatch.fin_logheader(20, fechainiciocol, fechafincol, por_procesar,procesados);
           COMMIT;
 END;
--fin carga nomina
 --carga de clientes creados en procecard
 PROCEDURE carga_cliente(
   p_usuario IN NUMBER,
   p_sucursal IN NUMBER,
   p_oficina IN NUMBER,
   p_terminal IN VARCHAR2,
   p_fecha     IN  DATE
  ) IS
 CURSOR P IS
   	Select	A.ROWID,  IDENTIFICACION,
   		  TIPOID,NOMBRE,APELLIDO1,APELLIDO2,REPLACE(replace(PASAPORTE,' ',''),'-','') PASAPORTE,TIPORESIDENCIA,
   		  DIRECCIONRES1,DIRECCIONRES2,
          DIRECCIONRES3,TELEFONORES,NOMBREEMPRESA,DIRECCIONOFI1,DIRECCIONOFI2,DIRECCIONOFI3,TELEFONOOFI,ESTADOCIVIL,
          SEXO,PROFESION,ACTIVIDADECO,FECHANACIM,EMAIL,REFERENCIAPER,TABNAC,NACIONALIDA,TABPRO,TABESTCIV,TABACTPRIN,
          TIPO,DEPENDIENTE,STATUS,TABVIVIENDA,TABTIPOPER,EXONIMP,TABREL,RELACIONBAN,CODEJE,TABGRUPO,GRUPOECO,NIVELACT,
          FECHAING,VERIFICA,FECVER,USRVER,LENGUAJE,TABLENGUAJE,TABSUJ,CODSUJ,CODTABEMPJUR,TIPOEMPJUR,CODTABCLA,CLASE,
          CODTABACTJUR,ACTIVIDADJUR,TABPAIS,PAIS,TABTIP,TABCIUDAD,CIUDAD,TABBARRIO,BARRIO,PROCESADO,CRC_TARJETA,
          RTRIM(LTRIM(NOMBRE||APELLIDO1||APELLIDO2)) NOMBRETOTAL,
         DECODE(INSTR(NOMBRE,' '),0,NOMBRE,SUBSTR(LTRIM(NOMBRE),1,INSTR(LTRIM(NOMBRE),' ') -1)) NOMBRE1,
          DECODE(INSTR(NOMBRE,' '),0,NULL,SUBSTR(LTRIM(NOMBRE),INSTR(LTRIM(NOMBRE),' ') +1)) NOMBRE2          
---CAMBIO 3
     FROM TCAR_REGCLIENTES A,TCAR_REGCREDIT B
    WHERE CRC_TIPOID = TIPOID
      AND CRC_IDENTIFICACION = IDENTIFICACION
      AND NVL(A.PROCESADO,'N') = 'N'
      AND CRC_TARJETA NOT IN (SELECT CREDIT
                               FROM TCREDITEXTRACT);
      --and identificacion ='098390048447';
    V_CODIGOCLI TCLI_PERSONA.CLI_CODIGO%TYPE;
    V_TIPOID TCLI_PERSONA.CLI_TIPOID%TYPE;
    V_IDENTIFICA  TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
    V_TIPOPER  TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
    V_TIPODIR  TCLI_DIRECCION.DIR_TIPODIR%TYPE;
    V_NOMBRE  TCLI_PERSONA.CLI_NOMBRE%TYPE;
    V_ESTCIV  TCLI_NATURAL.NAT_ESTCIV%TYPE;
    V_SEXO  TCLI_NATURAL.NAT_SEXO%TYPE;
    V_NUMERODIR  TCLI_DIRECCION.DIR_NUMERO%TYPE;
   fechainiciocol DATE;
   fechafincol DATE;
   v_commit    VARCHAR2(1):='T';
   procesados     NUMBER;
   por_procesar   NUMBER;
   V_ERROR VARCHAR2(80);
   V_CLIENTEEXISTE TCLI_PERSONA.CLI_CODIGO%TYPE;
   w_function  VARCHAR2(100);
   v_maxcodcli NUMBER(8);
   v_commitexiste  VARCHAR2(1):='N';
   V_TIENEDIR NUMBER:=0;
   w_identificacion varchar2(19);
 BEGIN
    fechainiciocol := SYSDATE;
    pkg_genbatch.inicio_logheader(2,p_fecha, fechainiciocol, p_usuario, p_sucursal,
                                  p_oficina, p_terminal, 'S');

    pkg_genbatch.graba_log_modulo_calif(2,P_fecha, 'INICIO CARGA CLIENTES','INF_INICIO', fechainiciocol);
    commit;
     DELETE TCLI_TELEFONO_AUX;
     DELETE TCLI_DIRECCION_AUX;
     DELETE TCLI_NATURAL_AUX;
     DELETE TCLI_JURIDICA_AUX;
     DELETE TCLI_PERSONA_AUX;
    COMMIT;    
  FOR X IN P LOOP
    v_commit  :='T';
    V_NUMERODIR :=0;
    v_commitexiste  :='N';
    BEGIN
/*    SELECT MAX(NVL(CLI_CODIGO,0)) + 1
      INTO V_CODIGOCLI
      FROM TCLI_PERSONA;  */
      w_identificacion:=null;
      IF x.tipoid = 'P' THEN
		 w_identificacion :=x.PASAPORTE;
      ELSE
		 w_identificacion := SUBSTR(x.identificacion,2);
      END IF;
      V_CODIGOCLI := 0;
      convert_identifica(x.tipoid,w_identificacion,v_tipoid,v_identifica,v_tipoper);
      VAL_IDENTIFICACION(V_TIPOID,v_identifica);
      EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' VAL INDETIFICA' ||x.identificacion||'^'||x.PASAPORTE||'^tc^'||x.CRC_TARJETA||'^id'||v_identifica||'^'||V_TIPOID||'^'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
    BEGIN
   --BUSCA SI EL CLIENTE YA EXISTE PARA NO VOLVERLO A CARGAR
     SELECT CLI_CODIGO
       INTO V_CLIENTEEXISTE
      FROM TCLI_PERSONA
      WHERE CLI_TIPOID = V_TIPOID
        AND CLI_IDENTIFICA = v_identifica;
        v_commit  :='F';
        v_commitexiste  :='S';
        --pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' CLIENTE YA EXISTE EN EL SISTEMA' ||x.identificacion||'-'||x.tipoid||'-'||V_ERROR,'ERRPRO');
        --COMMIT;
   --FIN
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
           NULL;
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' ESTADO CIVIL-SEXO' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;


    BEGIN
      V_ESTCIV := convert_estadocivil(X.ESTADOCIVIL);
      V_SEXO := convert_sexo(X.SEXO);
      EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' ESTADO CIVIL-SEXO' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;

------------------------------------------------------SI NO TIENEN NOMBRE NO SE CARGAN--------------------------------
  /* IF v_commit ='T' THEN
       IF X.NOMBRETOTAL IS NULL THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' tcli_persona ' ||x.identificacion||'- CLIENTE NO TIENE NOMBRE NI APELLIDO..REVISE','ERRPRO');
           COMMIT;
       END IF;
   END IF;*/
----------------------------------------------------------FIN--------------------------------------------------------
  IF v_commit ='T' THEN
     SELECT scli_seccli.nextval
       INTO V_CODIGOCLI
       FROM dual;
     BEGIN
     BEGIN
     V_NOMBRE := RTRIM(LTRIM(X.APELLIDO1))||' '||RTRIM(LTRIM(X.APELLIDO2))||', '||RTRIM(LTRIM(X.NOMBRE));
     EXCEPTION
       WHEN OTHERS THEN
            V_NOMBRE := RTRIM(LTRIM(X.APELLIDO1))||', '||RTRIM(LTRIM(X.NOMBRE));
     END;
     INSERT INTO TCLI_PERSONA( CLI_CODIGO, CLI_TABTIPOPER, CLI_TIPOPER, CLI_TIPOID, CLI_IDENTIFICA, CLI_NOMBRE,
                              CLI_NOMCORRESP, CLI_EXONIMP, CLI_TABREL, CLI_RELACIONBAN, CLI_CODEJE, CLI_REFERENCIAS,
                              CLI_TABGRUPO, CLI_GRUPOECO, CLI_NIVELACT, CLI_FECHAING, CLI_VERIFICA, CLI_FECVER, CLI_USRVER,
 			                  CLI_LENGUAJE, CLI_TABLENGUAJE , CLI_TABSUJ, CLI_CODSUJ)
                     VALUES (V_CODIGOCLI,X.TABTIPOPER,V_TIPOPER,V_TIPOID,V_IDENTIFICA,SUBSTR(V_NOMBRE,1,40),SUBSTR(V_NOMBRE,1,40),
                             X.EXONIMP,X.TABREL,X.RELACIONBAN,X.CODEJE,'DESDE PROCECARD',X.TABGRUPO,X.GRUPOECO,X.NIVELACT,X.FECHAING,
                             X.VERIFICA,X.FECVER,X.USRVER,X.LENGUAJE,X.TABLENGUAJE,X.TABSUJ,X.CODSUJ);
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' tcli_persona ' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
   END IF;

----------------------------------------------------------------------para actualizar nombres en caso que este null-----------------
--CAMBIO UNO
	IF v_commitexiste = 'S' AND X.NOMBRETOTAL IS NOT NULL THEN
     BEGIN
     V_NOMBRE := RTRIM(LTRIM(X.APELLIDO1))||' '||RTRIM(LTRIM(X.APELLIDO2))||', '||RTRIM(LTRIM(X.NOMBRE));
     EXCEPTION
       WHEN OTHERS THEN
     V_NOMBRE := RTRIM(LTRIM(X.APELLIDO1))||', '||RTRIM(LTRIM(X.NOMBRE));
     END;

       UPDATE TCLI_PERSONA
          SET CLI_NOMBRE= V_NOMBRE,CLI_NOMCORRESP=V_NOMBRE
        WHERE RTRIM(LTRIM(REPLACE(CLI_NOMBRE,',',''))) IS NULL
          AND CLI_TIPOID = V_TIPOID
          AND CLI_IDENTIFICA =  V_IDENTIFICA;
	END IF;
---------------------------------------------------------------------fin------------------------------------------------------------

IF v_commit ='T' THEN
  BEGIN
   IF V_TIPOPER  = 1 THEN
      INSERT INTO TCLI_NATURAL( NAT_CODCLI,NAT_TABNAC, NAT_NACIONALIDA, NAT_TABPRO, NAT_PROFESION,NAT_TABESTCIV,
                              NAT_ESTCIV,NAT_ACTPRIN, NAT_TABACTPRIN, NAT_FECNACIM, NAT_SEXO ,NAT_TIPO,NAT_DEPENDIENTE,
 			                  NAT_STATUS, NAT_VERIFICA,NAT_FECVER ,NAT_USRVER , NAT_TABVIVIENDA,NAT_APODERADO,
                        NAT_TABSTATUS,NAT_APEPATERNO,NAT_APEMATERNO,NAT_PRIMNOM,NAT_SEGUNOM )
			  VALUES(V_CODIGOCLI,X.TABNAC,X.NACIONALIDA,X.TABPRO,nvl(X.PROFESION,999),X.TABESTCIV,V_ESTCIV,nvl(X.ACTIVIDADECO,99),X.TABACTPRIN,
			         X.FECHANACIM,V_SEXO,X.TIPO,X.DEPENDIENTE,X.STATUS,X.VERIFICA,X.FECVER,X.USRVER,X.TABVIVIENDA,'N',
               285,X.APELLIDO1,X.APELLIDO2,X.NOMBRE1,X.NOMBRE2);
   ELSE
    INSERT INTO TCLI_JURIDICA(JUR_CODCLI,JUR_CODTAB,JUR_TIPOEMP,JUR_CODTABCLA,JUR_CLASE,JUR_CODTABACT,JUR_ACTIVIDAD,
				              JUR_FECHACONS,JUR_VERIFICA,JUR_FECVER,JUR_USRVER, JUR_NOMBRE ,JUR_TABPAIS,JUR_PAIS,
				              JUR_TABNAC,JUR_NACIONALIDAD)
 	                   VALUES(V_CODIGOCLI,X.CODTABEMPJUR,X.TIPOEMPJUR,X.CODTABCLA,X.CLASE,X.CODTABACTJUR,nvl(X.ACTIVIDADECO,999),
 	                          X.FECHANACIM,X.VERIFICA,X.FECVER,X.USRVER,SUBSTR(V_NOMBRE,1,90),X.TABPAIS,X.PAIS,
 	                          X.TABNAC,X.NACIONALIDA);
  END IF;
  EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' tcli_natural/tcli_juridica ' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
           COMMIT;
  END;
 END IF;

IF v_commit ='T' THEN
 IF X.DIRECCIONRES1 IS NOT NULL OR X.DIRECCIONRES2 IS NOT NULL OR X.DIRECCIONRES3 IS NOT NULL THEN
   BEGIN
   V_NUMERODIR := V_NUMERODIR + 1 ;
   V_TIPODIR := 1;--DOMICILIO
    INSERT INTO TCLI_DIRECCION(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_TABBARRIO,DIR_BARRIO,DIR_DIRECCION,DIR_DIRECCION2,
					           DIR_DIRECCION3,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CODIGOCLI,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,X.TABBARRIO,X.BARRIO,
	              X.DIRECCIONRES1,X.DIRECCIONRES2,X.DIRECCIONRES3,X.VERIFICA,X.FECVER,X.USRVER);

  EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' tcli_direccion 1 ' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
           COMMIT;
  END;
	IF v_commit ='T' THEN
	  IF X.TELEFONORES IS NOT NULL OR  NVL(X.TELEFONORES,0) <> 0 THEN
	   BEGIN
		      INSERT INTO TCLI_TELEFONO(TEL_CODCLI,TEL_NUMDIR,TEL_TELEFONO,TEL_TIPO,TEL_EXTENSION,TEL_VERIFICA ,
										TEL_FECVER,TEL_USRVER )
						VALUES(V_CODIGOCLI,V_NUMERODIR,X.TELEFONORES,'T',NULL,X.VERIFICA,X.FECVER,X.USRVER);
	  EXCEPTION
	     WHEN OTHERS THEN
	           ROLLBACK;
	           v_commit  :='F';
	           V_ERROR := SUBSTR(SQLERRM,1,80);
	           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' tcli_telefono 1 ' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
	           COMMIT;
	  END;
	  END IF;
	 END IF;
  END IF;
 END IF;

IF v_commit ='T' THEN
 IF X.DIRECCIONOFI1 IS NOT NULL OR X.DIRECCIONOFI2 IS NOT NULL OR X.DIRECCIONOFI3 IS NOT NULL THEN
    BEGIN
 	V_NUMERODIR := V_NUMERODIR + 1 ;
 	V_TIPODIR := 2;--OFICINA
    INSERT INTO TCLI_DIRECCION(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_TABBARRIO,DIR_BARRIO,DIR_DIRECCION,DIR_DIRECCION2,
					           DIR_DIRECCION3,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CODIGOCLI,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,X.TABBARRIO,X.BARRIO,
	              X.DIRECCIONOFI1,X.DIRECCIONOFI2,X.DIRECCIONOFI3,X.VERIFICA,X.FECVER,X.USRVER);
  EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' tcli_direccion 2 ' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
           COMMIT;
  END;
	IF v_commit ='T' THEN
	  IF X.TELEFONOOFI IS NOT NULL THEN
	  BEGIN
		      INSERT INTO TCLI_TELEFONO(TEL_CODCLI,TEL_NUMDIR,TEL_TELEFONO,TEL_TIPO,TEL_EXTENSION,TEL_VERIFICA,
										TEL_FECVER,TEL_USRVER )
						VALUES(V_CODIGOCLI,V_NUMERODIR,X.TELEFONOOFI,'T',NULL,X.VERIFICA,X.FECVER,X.USRVER);
	  EXCEPTION
	     WHEN OTHERS THEN
	           ROLLBACK;
	           v_commit  :='F';
	           V_ERROR := SUBSTR(SQLERRM,1,80);
	           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' tcli_telefono 2 ' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
	           COMMIT;
	  END;
	  END IF;
	 END IF;
  END IF;
 END IF;
IF v_commit ='T' THEN
  IF X.EMAIL IS NOT NULL THEN
    BEGIN
 	V_NUMERODIR := V_NUMERODIR + 1 ;
 	V_TIPODIR := 4;--E-MAIL
    INSERT INTO TCLI_DIRECCION(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_DIRECCION,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CODIGOCLI,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,
	              X.EMAIL,X.VERIFICA,X.FECVER,X.USRVER);
  EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' tcli_direccion 2 ' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
           COMMIT;
  END;
 END IF;
END IF;
V_TIENEDIR :=0;
    BEGIN
 	V_NUMERODIR := V_NUMERODIR + 1 ;
 	V_TIPODIR := 1;--E-MAIL
 	 IF  v_commitexiste  ='S'  THEN
 	    SELECT COUNT(*)
		  INTO V_TIENEDIR
		  FROM TCLI_DIRECCION
		 WHERE DIR_CODCLI = V_CLIENTEEXISTE;
     IF  NVL(V_TIENEDIR,0) = 0 THEN
      INSERT INTO TCLI_DIRECCION(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_TABBARRIO,DIR_BARRIO,DIR_DIRECCION,DIR_DIRECCION2,
					           DIR_DIRECCION3,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CLIENTEEXISTE,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,X.TABBARRIO,X.BARRIO,
	              X.DIRECCIONRES1,X.DIRECCIONRES2,X.DIRECCIONRES3,X.VERIFICA,X.FECVER,X.USRVER);
	  END IF;
     ELSE
 	    SELECT COUNT(*)
		  INTO V_TIENEDIR
		  FROM TCLI_DIRECCION
		 WHERE DIR_CODCLI = V_CODIGOCLI;
      IF  NVL(V_TIENEDIR,0) = 0 THEN
      INSERT INTO TCLI_DIRECCION(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_TABBARRIO,DIR_BARRIO,DIR_DIRECCION,DIR_DIRECCION2,
					           DIR_DIRECCION3,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CODIGOCLI,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,X.TABBARRIO,X.BARRIO,
	              X.DIRECCIONRES1,X.DIRECCIONRES2,X.DIRECCIONRES3,X.VERIFICA,X.FECVER,X.USRVER);
      END IF;
     END IF;
    EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' tcli_direccion 2 ' ||x.identificacion||'-'||V_ERROR,'ERRPRO');
           COMMIT;
    END;
 IF v_commit ='T'  OR  v_commitexiste ='S'  THEN
    UPDATE  TCAR_REGCLIENTES
       SET PROCESADO = 'S'
     WHERE ROWID = X.ROWID;
    COMMIT;
 END IF;
END LOOP;


      SELECT COUNT(*)
        INTO por_procesar
        FROM TCAR_REGCLIENTES
       WHERE NVL(PROCESADO,'N') = 'N';

      SELECT COUNT(*)
        INTO procesados
        FROM TCAR_REGCLIENTES
       WHERE NVL(PROCESADO,'N') = 'S';

      fechafincol := SYSDATE;
      pkg_genbatch.graba_log_modulo_calif(2, P_fecha, 'FIN CARGA CLIENTES','INF_FINBATCH', fechafincol);
      pkg_genbatch.fin_logheader(2, fechainiciocol, fechafincol, por_procesar,
                                 procesados);
       COMMIT;
  EXCEPTION
	      WHEN OTHERS THEN
           v_commit  :='F';
           ROLLBACK;
           V_ERROR := SUBSTR(SQLERRM,1,50);
           V_ERROR := SUBSTR(SQLERRM,1,50);
            fechafincol := SYSDATE;
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' FIN INESPERADO '||V_TIPOID||'-'||v_identifica||'-'||V_ERROR,'INF_FINBATCH');
           pkg_genbatch.fin_logheader(2, fechainiciocol, fechafincol, por_procesar,procesados);
           COMMIT;

 END;
--carga de clientes creados en procecard
 PROCEDURE carga_cliente_aux(
   p_usuario IN NUMBER,
   p_sucursal IN NUMBER,
   p_oficina IN NUMBER,
   p_terminal IN VARCHAR2,
   p_fecha     IN  DATE
  ) IS
 CURSOR P IS
   SELECT DISTINCT A.ROWID,Decode(TIPOID,'P',
		  Nvl(Replace(Replace(PASAPORTE,' ',''),'-',''),SubStr(IDENTIFICACION,2)),'E',
		  Nvl(Replace(Replace(PASAPORTE,' ',''),'-',''),SubStr(IDENTIFICACION,2)),SubStr(IDENTIFICACION,2)) IDENTIFICACION,
		  IDENTIFICACION IDENTIFICACION_REAL,
--		  Nvl(LPad(RPad(Replace(Replace(PASAPORTE,' ',''),'-',''),11,' '),12,'0'),IDENTIFICACION),'E',
--		  Nvl(LPad(RPad(Replace(Replace(PASAPORTE,' ',''),'-',''),11,' '),12,'0'),IDENTIFICACION),IDENTIFICACION) IDENTIFICACION,
   		  TIPOID,NOMBRE,APELLIDO1,APELLIDO2,REPLACE(replace(PASAPORTE,' ',''),'-','') PASAPORTE,
   		  TIPORESIDENCIA,DIRECCIONRES1,DIRECCIONRES2,
          DIRECCIONRES3,TELEFONORES,NOMBREEMPRESA,DIRECCIONOFI1,DIRECCIONOFI2,DIRECCIONOFI3,TELEFONOOFI,ESTADOCIVIL,
          SEXO,PROFESION,ACTIVIDADECO,FECHANACIM,EMAIL,REFERENCIAPER,TABNAC,NACIONALIDA,TABPRO,TABESTCIV,TABACTPRIN,
          TIPO,DEPENDIENTE,STATUS,TABVIVIENDA,TABTIPOPER,EXONIMP,TABREL,RELACIONBAN,CODEJE,TABGRUPO,GRUPOECO,NIVELACT,
          FECHAING,VERIFICA,FECVER,USRVER,LENGUAJE,TABLENGUAJE,TABSUJ,CODSUJ,CODTABEMPJUR,TIPOEMPJUR,CODTABCLA,CLASE,
          CODTABACTJUR,ACTIVIDADJUR,TABPAIS,PAIS,TABTIP,TABCIUDAD,CIUDAD,TABBARRIO,BARRIO,PROCESADO,
          RTRIM(LTRIM(NOMBRE||APELLIDO1||APELLIDO2)) NOMBRETOTAL, ---CAMBIO 3
          DECODE(INSTR(NOMBRE,' '),0,NOMBRE,SUBSTR(LTRIM(NOMBRE),1,INSTR(LTRIM(NOMBRE),' ') -1)) NOMBRE1,
          DECODE(INSTR(NOMBRE,' '),0,NULL,SUBSTR(LTRIM(NOMBRE),INSTR(LTRIM(NOMBRE),' ') +1)) NOMBRE2          
     FROM TCAR_REGCLIENTES A,TCAR_REGCREDIT B
    WHERE CRC_TIPOID = TIPOID
      AND CRC_IDENTIFICACION = IDENTIFICACION
      AND NVL(A.PROCESADO,'N') = 'N'
      AND CRC_TARJETA NOT IN (SELECT CREDIT
                         FROM TCREDITEXTRACT);
      --and identificacion ='098390048447';
    V_CODIGOCLI TCLI_PERSONA_AUX.CLI_CODIGO%TYPE;
    V_TIPOID TCLI_PERSONA_AUX.CLI_TIPOID%TYPE;
    V_IDENTIFICA  TCLI_PERSONA_AUX.CLI_IDENTIFICA%TYPE;
    V_TIPOPER  TCLI_PERSONA_AUX.CLI_IDENTIFICA%TYPE;
    V_TIPODIR  TCLI_DIRECCION_AUX.DIR_TIPODIR%TYPE;
    V_NOMBRE  TCLI_PERSONA_AUX.CLI_NOMBRE%TYPE;
    V_ESTCIV  TCLI_NATURAL_AUX.NAT_ESTCIV%TYPE;
    V_SEXO  TCLI_NATURAL_AUX.NAT_SEXO%TYPE;
    V_NUMERODIR  TCLI_DIRECCION_AUX.DIR_NUMERO%TYPE;
   fechainiciocol DATE;
   fechafincol DATE;
   v_commit    VARCHAR2(1):='T';
   procesados     NUMBER;
   por_procesar   NUMBER;
   V_ERROR VARCHAR2(80);
   V_CLIENTEEXISTE TCLI_PERSONA_AUX.CLI_CODIGO%TYPE;
   w_function  VARCHAR2(100);
   v_maxcodcli NUMBER(8);
   v_Identificacion Varchar2(20) := Null;
   v_commitexiste  VARCHAR2(1):='N';
   V_TIENEDIR 	   NUMBER:=0;
   vInstr  		   Number:=0;
   vInstrAnt	   Number:=0;
   Cont			   Number:=0;
   vUltmaPosicion  Number:=0;
   vTieneCaracter  Varchar2(1) := 'N';
 BEGIN

    fechainiciocol := SYSDATE;
    pkg_genbatch.inicio_logheader(2,p_fecha, fechainiciocol, p_usuario, p_sucursal,
                                  p_oficina, p_terminal, 'S');

    pkg_genbatch.graba_log_modulo_calif(2,P_fecha, 'INICIO CARGA CLIENTES','INF_INICIO', fechainiciocol);
    commit;
    DELETE TCLI_TELEFONO_AUX;
 DELETE TCLI_DIRECCION_AUX;
 DELETE TCLI_NATURAL_AUX;
 DELETE TCLI_JURIDICA_AUX;
 DELETE TCLI_PERSONA_AUX;
COMMIT;
  FOR X IN P LOOP
    v_commit  :='T';
    V_NUMERODIR :=0;
    v_commitexiste  :='N';
	v_Identificacion	  := Null;
	vUltmaPosicion		  := 0;
	Cont				  := 0;
	vTieneCaracter		  := 'N';
	vInstr				  := 0;
	vInstrAnt			  := 3;
	--
	If x.TipoId In ('P','E')
	   And Nvl(Replace(Replace(x.Pasaporte,' ',''),'-',''),' ') <> ' '
	   And Length(x.Identificacion) = Length(x.Identificacion_Real)
	   Then
	   	   v_Identificacion := Translate(x.Identificacion, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ','000000000000000000000000000');
		   --
		   If v_Identificacion <> x.Identificacion_Real
		     Then
			   Begin
			   		vUltmaPosicion := To_Number(SubStr(Replace(Replace(LTrim(RTrim(x.Pasaporte)),' ',''),'-',''),vInstrAnt));
			   EXCEPTION
	      	     WHEN OTHERS
				   THEN
				   	  vTieneCaracter := 'S';
			   End;
			   --
			   If Nvl(vTieneCaracter,'N') = 'N'
			     Then
			  	 	v_Identificacion := x.Identificacion||SubStr(x.Identificacion_Real,12);
			   Else
			   	   v_Identificacion := x.Identificacion;
			   End If;
		   Else
			  	v_Identificacion := x.Identificacion;
		   End If;
	Else
		v_Identificacion := x.Identificacion;
	End If;
	--
    BEGIN
	/*
    SELECT MAX(NVL(CLI_CODIGO,0)) + 1
      INTO V_CODIGOCLI
      FROM TCLI_PERSONA_AUX;*/
      IF x.tipoid = 'P' THEN
		 v_identificacion :=x.PASAPORTE;
      ELSE
		 v_identificacion := x.Identificacion;--SUBSTR(x.identificacion,2);
      END IF;

      V_CODIGOCLI := 0;
      convert_identifica(x.tipoid,v_Identificacion,v_tipoid,v_identifica,v_tipoper);
      VAL_IDENTIFICACION(V_TIPOID,v_identifica);
      EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' VAL INDETIFICA' ||x.Identificacion_Real||'^'||v_identifica||'^'||V_TIPOID||'^'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
    BEGIN
   --BUSCA SI EL CLIENTE YA EXISTE PARA NO VOLVERLO A CARGAR
     SELECT CLI_CODIGO
       INTO V_CLIENTEEXISTE
      FROM TCLI_PERSONA
      WHERE CLI_TIPOID = V_TIPOID
        AND CLI_IDENTIFICA = v_identifica;
        v_commit  :='F';
        v_commitexiste  :='S';
        --pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' CLIENTE YA EXISTE EN EL SISTEMA' ||x.identificacion||'-'||x.tipoid||'-'||V_ERROR,'ERRPRO');
        --COMMIT;
   --FIN
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
           NULL;
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' ESTADO CIVIL-SEXO' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;


    BEGIN
      V_ESTCIV := convert_estadocivil(X.ESTADOCIVIL);
      V_SEXO := convert_sexo(X.SEXO);
      EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' ESTADO CIVIL-SEXO' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;

------------------------------------------------------SI NO TIENEN NOMBRE NO SE CARGAN--------------------------------
  /* IF v_commit ='T' THEN
       IF X.NOMBRETOTAL IS NULL THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' TCLI_PERSONA_AUX ' ||x.identificacion||'- CLIENTE NO TIENE NOMBRE NI APELLIDO..REVISE','ERRPRO');
           COMMIT;
       END IF;
   END IF;*/
----------------------------------------------------------FIN--------------------------------------------------------
  IF v_commit ='T' THEN
     SELECT SCLI_SECCLIUAX.nextval
       INTO V_CODIGOCLI
       FROM dual;
     BEGIN
     BEGIN
     V_NOMBRE := RTRIM(LTRIM(X.APELLIDO1))||' '||RTRIM(LTRIM(X.APELLIDO2))||', '||RTRIM(LTRIM(X.NOMBRE));
     EXCEPTION
       WHEN OTHERS THEN
            V_NOMBRE := RTRIM(LTRIM(X.APELLIDO1))||', '||RTRIM(LTRIM(X.NOMBRE));
     END;
     INSERT INTO TCLI_PERSONA_AUX( CLI_CODIGO, CLI_TABTIPOPER, CLI_TIPOPER, CLI_TIPOID, CLI_IDENTIFICA, CLI_NOMBRE,
                              CLI_NOMCORRESP, CLI_EXONIMP, CLI_TABREL, CLI_RELACIONBAN, CLI_CODEJE, CLI_REFERENCIAS,
                              CLI_TABGRUPO, CLI_GRUPOECO, CLI_NIVELACT, CLI_FECHAING, CLI_VERIFICA, CLI_FECVER, CLI_USRVER,
 			                  CLI_LENGUAJE, CLI_TABLENGUAJE , CLI_TABSUJ, CLI_CODSUJ,CLI_TABSISTEMORIGEN,CLI_SISTEMORIGEN)
                     VALUES (V_CODIGOCLI,X.TABTIPOPER,V_TIPOPER,V_TIPOID,V_IDENTIFICA,SUBSTR(V_NOMBRE,1,40),SUBSTR(V_NOMBRE,1,40),
                             X.EXONIMP,X.TABREL,X.RELACIONBAN,X.CODEJE,'DESDE PROCECARD',X.TABGRUPO,X.GRUPOECO,X.NIVELACT,X.FECHAING,
                             X.VERIFICA,X.FECVER,X.USRVER,X.LENGUAJE,X.TABLENGUAJE,X.TABSUJ,X.CODSUJ,928,2);
	    EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' TCLI_PERSONA_AUX ' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
           COMMIT;
	    END;
	  --COMMIT;
   END IF;

----------------------------------------------------------------------para actualizar nombres en caso que este null-----------------
--CAMBIO UNO
	IF v_commitexiste = 'S' AND X.NOMBRETOTAL IS NOT NULL THEN
     BEGIN
     V_NOMBRE := RTRIM(LTRIM(X.APELLIDO1))||' '||RTRIM(LTRIM(X.APELLIDO2))||', '||RTRIM(LTRIM(X.NOMBRE));
     EXCEPTION
       WHEN OTHERS THEN
     V_NOMBRE := RTRIM(LTRIM(X.APELLIDO1))||', '||RTRIM(LTRIM(X.NOMBRE));
     END;

       UPDATE TCLI_PERSONA
          SET CLI_NOMBRE= V_NOMBRE,CLI_NOMCORRESP=V_NOMBRE
        WHERE RTRIM(LTRIM(REPLACE(CLI_NOMBRE,',',''))) IS NULL
          AND CLI_TIPOID = V_TIPOID
          AND CLI_IDENTIFICA =  V_IDENTIFICA;
	END IF;
---------------------------------------------------------------------fin------------------------------------------------------------

IF v_commit ='T' THEN
  BEGIN
   IF V_TIPOPER  = 1 THEN
      INSERT INTO TCLI_NATURAL_AUX( NAT_CODCLI,NAT_TABNAC, NAT_NACIONALIDA, NAT_TABPRO, NAT_PROFESION,NAT_TABESTCIV,
                              NAT_ESTCIV,NAT_ACTPRIN, NAT_TABACTPRIN, NAT_FECNACIM, NAT_SEXO ,NAT_TIPO,NAT_DEPENDIENTE,
 			                  NAT_STATUS, NAT_VERIFICA,NAT_FECVER ,NAT_USRVER , NAT_TABVIVIENDA,NAT_APODERADO,NAT_TABSTATUS,
                        NAT_APEPATERNO,NAT_APEMATERNO,NAT_PRIMNOM,NAT_SEGUNOM )
			  VALUES(V_CODIGOCLI,X.TABNAC,X.NACIONALIDA,X.TABPRO,nvl(X.PROFESION,999),X.TABESTCIV,V_ESTCIV,nvl(X.ACTIVIDADECO,99),X.TABACTPRIN,
			         X.FECHANACIM,V_SEXO,X.TIPO,X.DEPENDIENTE,X.STATUS,X.VERIFICA,X.FECVER,X.USRVER,X.TABVIVIENDA,'N',285,
               X.APELLIDO1,X.APELLIDO2,X.NOMBRE1,X.NOMBRE2);
   ELSE
    INSERT INTO TCLI_JURIDICA_AUX(JUR_CODCLI,JUR_CODTAB,JUR_TIPOEMP,JUR_CODTABCLA,JUR_CLASE,JUR_CODTABACT,JUR_ACTIVIDAD,
				              JUR_FECHACONS,JUR_VERIFICA,JUR_FECVER,JUR_USRVER, JUR_NOMBRE ,JUR_TABPAIS,JUR_PAIS,
				              JUR_TABNAC,JUR_NACIONALIDAD)
 	                   VALUES(V_CODIGOCLI,X.CODTABEMPJUR,X.TIPOEMPJUR,X.CODTABCLA,X.CLASE,X.CODTABACTJUR,nvl(X.ACTIVIDADECO,999),
 	                          X.FECHANACIM,X.VERIFICA,X.FECVER,X.USRVER,SUBSTR(V_NOMBRE,1,90),X.TABPAIS,X.PAIS,
 	                          X.TABNAC,X.NACIONALIDA);
  END IF;
  EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' TCLI_NATURAL_AUX/TCLI_JURIDICA_AUX ' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
           COMMIT;
  END;
 END IF;

IF v_commit ='T' THEN
 IF X.DIRECCIONRES1 IS NOT NULL OR X.DIRECCIONRES2 IS NOT NULL OR X.DIRECCIONRES3 IS NOT NULL THEN
   BEGIN
   V_NUMERODIR := V_NUMERODIR + 1 ;
   V_TIPODIR := 1;--DOMICILIO
    INSERT INTO TCLI_DIRECCION_AUX(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_TABBARRIO,DIR_BARRIO,DIR_DIRECCION,DIR_DIRECCION2,
					           DIR_DIRECCION3,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CODIGOCLI,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,X.TABBARRIO,X.BARRIO,
	              X.DIRECCIONRES1,X.DIRECCIONRES2,X.DIRECCIONRES3,X.VERIFICA,X.FECVER,X.USRVER);

  EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' TCLI_DIRECCION_AUX 1 ' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
           COMMIT;
  END;
	IF v_commit ='T' THEN
	  IF X.TELEFONORES IS NOT NULL OR  NVL(X.TELEFONORES,0) <> 0 THEN
	   BEGIN
		      INSERT INTO TCLI_TELEFONO_AUX(TEL_CODCLI,TEL_NUMDIR,TEL_TELEFONO,TEL_TIPO,TEL_EXTENSION,TEL_VERIFICA ,
										TEL_FECVER,TEL_USRVER )
						VALUES(V_CODIGOCLI,V_NUMERODIR,X.TELEFONORES,'T',NULL,X.VERIFICA,X.FECVER,X.USRVER);
	  EXCEPTION
	     WHEN OTHERS THEN
	           ROLLBACK;
	           v_commit  :='F';
	           V_ERROR := SUBSTR(SQLERRM,1,80);
	           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' TCLI_TELEFONO_AUX 1 ' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
	           COMMIT;
	  END;
	  END IF;
	 END IF;
  END IF;
 END IF;

IF v_commit ='T' THEN
 IF X.DIRECCIONOFI1 IS NOT NULL OR X.DIRECCIONOFI2 IS NOT NULL OR X.DIRECCIONOFI3 IS NOT NULL THEN
    BEGIN
 	V_NUMERODIR := V_NUMERODIR + 1 ;
 	V_TIPODIR := 2;--OFICINA
    INSERT INTO TCLI_DIRECCION_AUX(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_TABBARRIO,DIR_BARRIO,DIR_DIRECCION,DIR_DIRECCION2,
					           DIR_DIRECCION3,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CODIGOCLI,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,X.TABBARRIO,X.BARRIO,
	              X.DIRECCIONOFI1,X.DIRECCIONOFI2,X.DIRECCIONOFI3,X.VERIFICA,X.FECVER,X.USRVER);
  EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' TCLI_DIRECCION_AUX 2 ' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
           COMMIT;
  END;
	IF v_commit ='T' THEN
	  IF X.TELEFONOOFI IS NOT NULL THEN
	  BEGIN
		      INSERT INTO TCLI_TELEFONO_AUX(TEL_CODCLI,TEL_NUMDIR,TEL_TELEFONO,TEL_TIPO,TEL_EXTENSION,TEL_VERIFICA,
										TEL_FECVER,TEL_USRVER )
						VALUES(V_CODIGOCLI,V_NUMERODIR,X.TELEFONOOFI,'T',NULL,X.VERIFICA,X.FECVER,X.USRVER);
	  EXCEPTION
	     WHEN OTHERS THEN
	           ROLLBACK;
	           v_commit  :='F';
	           V_ERROR := SUBSTR(SQLERRM,1,80);
	           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' TCLI_TELEFONO_AUX 2 ' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
	           COMMIT;
	  END;
	  END IF;
	 END IF;
  END IF;
 END IF;
IF v_commit ='T' THEN
  IF X.EMAIL IS NOT NULL THEN
    BEGIN
 	V_NUMERODIR := V_NUMERODIR + 1 ;
 	V_TIPODIR := 4;--E-MAIL
    INSERT INTO TCLI_DIRECCION_AUX(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_DIRECCION,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CODIGOCLI,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,
	              X.EMAIL,X.VERIFICA,X.FECVER,X.USRVER);
  EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' TCLI_DIRECCION_AUX 2 ' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
           COMMIT;
  END;
 END IF;
END IF;
V_TIENEDIR :=0;
    BEGIN
 	V_NUMERODIR := V_NUMERODIR + 1 ;
 	V_TIPODIR := 1;--E-MAIL
 	 IF  v_commitexiste  ='S'  THEN
 	    SELECT COUNT(*)
		  INTO V_TIENEDIR
		  FROM TCLI_DIRECCION
		 WHERE DIR_CODCLI = V_CLIENTEEXISTE;
     IF  NVL(V_TIENEDIR,0) = 0 THEN
      INSERT INTO TCLI_DIRECCION(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_TABBARRIO,DIR_BARRIO,DIR_DIRECCION,DIR_DIRECCION2,
					           DIR_DIRECCION3,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CLIENTEEXISTE,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,X.TABBARRIO,X.BARRIO,
	              X.DIRECCIONRES1,X.DIRECCIONRES2,X.DIRECCIONRES3,X.VERIFICA,X.FECVER,X.USRVER);
	  END IF;
     ELSE
 	    SELECT COUNT(*)
		  INTO V_TIENEDIR
		  FROM TCLI_DIRECCION_AUX
		 WHERE DIR_CODCLI = V_CODIGOCLI;
      IF  NVL(V_TIENEDIR,0) = 0 THEN
      INSERT INTO TCLI_DIRECCION_AUX(DIR_CODCLI,DIR_NUMERO,DIR_TABTIP,DIR_TIPODIR,DIR_TABCIUDAD,DIR_CIUDAD,DIR_TABPAIS,
                               DIR_PAIS,DIR_TABBARRIO,DIR_BARRIO,DIR_DIRECCION,DIR_DIRECCION2,
					           DIR_DIRECCION3,DIR_VERIFICA,DIR_FECVER,DIR_USRVER)
	      VALUES (V_CODIGOCLI,V_NUMERODIR,23,V_TIPODIR,X.TABCIUDAD,X.CIUDAD,X.TABPAIS,X.PAIS,X.TABBARRIO,X.BARRIO,
	              X.DIRECCIONRES1,X.DIRECCIONRES2,X.DIRECCIONRES3,X.VERIFICA,X.FECVER,X.USRVER);
      END IF;
     END IF;
    EXCEPTION
     WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,80);
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' TCLI_DIRECCION_AUX 2 ' ||x.Identificacion_Real||'-'||V_ERROR,'ERRPRO');
           COMMIT;
    END;
 IF v_commit ='T'  OR  v_commitexiste ='S'  THEN
    UPDATE  TCAR_REGCLIENTES
       SET PROCESADO = 'S'
     WHERE ROWID = X.ROWID;
    COMMIT;
 END IF;
END LOOP;


      SELECT COUNT(*)
        INTO por_procesar
        FROM TCAR_REGCLIENTES
       WHERE NVL(PROCESADO,'N') = 'N';

      SELECT COUNT(*)
        INTO procesados
        FROM TCAR_REGCLIENTES
       WHERE NVL(PROCESADO,'N') = 'S';

      fechafincol := SYSDATE;
      pkg_genbatch.graba_log_modulo_calif(2, P_fecha, 'FIN CARGA CLIENTES','INF_FINBATCH', fechafincol);
      pkg_genbatch.fin_logheader(2, fechainiciocol, fechafincol, por_procesar,
                                 procesados);
       COMMIT;
  EXCEPTION
	      WHEN OTHERS THEN
           v_commit  :='F';
           ROLLBACK;
           V_ERROR := SUBSTR(SQLERRM,1,50);
           V_ERROR := SUBSTR(SQLERRM,1,50);
            fechafincol := SYSDATE;
           pkg_genbatch.graba_log_modulo_calif(2,P_fecha,' FIN INESPERADO '||V_TIPOID||'-'||v_identifica||'-'||V_ERROR,'INF_FINBATCH');
           pkg_genbatch.fin_logheader(2, fechainiciocol, fechafincol, por_procesar,procesados);
           COMMIT;

 END; --carga_cliente_aux
--carga de tarjeta de credito
 PROCEDURE carga_tarjeta(
   p_usuario IN NUMBER,
   p_sucursal IN NUMBER,
   p_oficina IN NUMBER,
   p_terminal IN VARCHAR2,
   p_fecha     IN  DATE) IS
 CURSOR P IS
   SELECT ROWID,CRC_TARJETA,CRC_TIPOID,CRC_IDENTIFICACION,CRC_CLIENTE,CRC_STATUS,CRC_FECHAVEN,
         CRC_FECHAEMISION,CRC_MONEDA,CRC_LIMITERD,CRC_LIMITEUS,CRC_SALDOACTUALRD,CRC_SALDOACTUALUS,
         CRC_DISPONIBLERD,CRC_DISPONIBLEUS,CRC_VIGENTERD,CRC_VIGENTEUS,CRC_VENCIDORD,CRC_VENCIDOUS,
         CRC_CALIFRD,CRC_CALIFUS,CRC_PROCESADO,CRC_SALTRANSITORD,
         CRC_SALTRANSITOUS ,
         CRC_SALFAVORRD    ,
         CRC_SALFAVORUS,
         CRC_NOMTA,
         crc_saldifrd,
         CRC_SALDIFUS,
         CRC_TARJETAPRI
    FROM  TCAR_REGCREDIT
   WHERE NVL(CRC_PROCESADO,'N') ='N'
   AND CRC_TARJETA NOT IN (SELECT CREDIT
                               FROM TCREDITEXTRACT);
   V_CLIENTE TCLI_PERSONA.CLI_CODIGO%TYPE;
   V_CLIENTEANTERIOR TCLI_PERSONA.CLI_CODIGO%TYPE;
   V_CALIFCODE NUMBER(4);
   V_PROVISION NUMBER(18,6);
   V_TIPOID TCLI_PERSONA.CLI_TIPOID%TYPE;
   V_IDENTIFICA  TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
   V_TIPOPER  TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
   V_SECUENCIA TCLI_TARJCRE.TAR_SECUENCIA%TYPE;
   fechainiciocol DATE;
   fechafincol DATE;
   v_commit    VARCHAR2(1):='T';
   procesados     NUMBER;
   por_procesar   NUMBER;
   V_ERROR VARCHAR2(50);
   V_YAEXISTE NUMBER:=0;
   w_identificacion varchar2(19);
  BEGIN
    fechainiciocol := SYSDATE;
    pkg_genbatch.inicio_logheader(8,p_fecha, fechainiciocol, p_usuario, p_sucursal,
    p_oficina, p_terminal, 'S');
    pkg_genbatch.graba_log_modulo_calif(8,P_fecha, 'INICIO CARGA TARJETA DE CREDITO','INF_INICIO', fechainiciocol);
    commit;
   FOR X IN P LOOP
    v_commit  :='T';
    v_tipoid := null;
    v_identifica := null;
    v_tipoper := null;
    V_CLIENTE := null;
    BEGIN
      IF x.CRC_tipoid= 'P' THEN
        begin
         select REPLACE(replace(PASAPORTE,' ',''),'-','')
            into w_identificacion
          from tcar_regclientes
		  WHERE  TIPOID = x.CRC_TIPOID
            AND  IDENTIFICACION = x.CRC_IDENTIFICACION;
        exception
          when others then
              w_identificacion:=null;
        end;
      ELSE
        w_identificacion:= SUBSTR(x.CRC_identificacion,2);
      END IF;
      convert_identifica(x.CRC_tipoid,w_identificacion,v_tipoid,v_identifica,v_tipoper);
      VAL_IDENTIFICACION(x.CRC_tipoid,v_identifica);
    EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' VAL-IDENTIFICA:' ||X.CRC_TARJETA||'-'||x.CRC_identificacion||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	END;

    BEGIN
    SELECT CLI_CODIGO
      INTO V_CLIENTE
      FROM TCLI_PERSONA
     WHERE CLI_TIPOID =  v_tipoid
       AND CLI_IDENTIFICA = v_identifica;
    EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F'; --'F'
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCLI_PERSONA:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	END;
	--V_CLIENTE:=1091;
  --ACTUALIZO CON EL NOMBRE DE LA TARJETA(PLASTICO) PARA AQUELLOS QUE ESTAN NULL
     IF   v_commit  ='T' THEN
         UPDATE TCLI_PERSONA
          SET CLI_NOMBRE= X.CRC_NOMTA,
              CLI_NOMCORRESP=X.CRC_NOMTA
        WHERE RTRIM(LTRIM(REPLACE(CLI_NOMBRE,',',''))) IS NULL
          AND CLI_TIPOID = V_TIPOID
          AND CLI_IDENTIFICA =  V_IDENTIFICA;
   END IF;
   --busco cliente anterior
   BEGIN
     SELECT CLIENT
       INTO V_CLIENTEANTERIOR
       FROM TCREDITEXTRACT
     WHERE   CREDIT = X.CRC_TARJETA;
   EXCEPTION
     WHEN OTHERS THEN
      V_CLIENTEANTERIOR := V_CLIENTE;
   END;
   --fin de cliente anterior

  IF   v_commit  ='T' THEN
    BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
       UPDATE TCREDITEXTRACT
          SET CREDITSTATUS = X.CRC_STATUS,
              EXPIREDATE   = TO_DATE(X.CRC_FECHAVEN||'01','YYYYMMDD'),
              OPENDATE     = TO_DATE(X.CRC_FECHAEMISION,'YYYYMMDD'),
              CLIENT = V_CLIENTE,
              TYPECLIENT = X.CRC_CLIENTE,
              CREDITREF = X.CRC_TARJETAPRI
       WHERE  CREDIT = X.CRC_TARJETA;
         --AND  CLIENT = V_CLIENTE;
       IF SQL%NOTFOUND THEN
        INSERT INTO TCREDITEXTRACT(CREDIT,CLIENT,CREDITSTATUS,EXPIREDATE,OPENDATE,TYPECLIENT,CREDITREF)
             VALUES( X.CRC_TARJETA,V_CLIENTE,X.CRC_STATUS,TO_DATE(X.CRC_FECHAVEN||'01','YYYYMMDD'),TO_DATE(X.CRC_FECHAEMISION,'YYYYMMDD'),X.CRC_CLIENTE,X.CRC_TARJETAPRI);
       END IF;
     ELSE
       UPDATE TCREDITEXTRACT
          SET CREDITSTATUS = X.CRC_STATUS,
              EXPIREDATE   = TO_DATE(X.CRC_FECHAVEN||'01','YYYYMMDD'),
              OPENDATE     = TO_DATE(X.CRC_FECHAEMISION,'YYYYMMDD'),
              TYPECLIENT = x.CRC_CLIENTE,
              CREDITREF = X.CRC_TARJETAPRI
       WHERE  CREDIT = X.CRC_TARJETA
         AND  CLIENT = V_CLIENTE;
       IF SQL%NOTFOUND THEN
        INSERT INTO TCREDITEXTRACT(CREDIT,CLIENT,CREDITSTATUS,EXPIREDATE,OPENDATE,TYPECLIENT,CREDITREF)
             VALUES( X.CRC_TARJETA,V_CLIENTE,X.CRC_STATUS,TO_DATE(X.CRC_FECHAVEN||'01','YYYYMMDD'),TO_DATE(X.CRC_FECHAEMISION,'YYYYMMDD'),X.CRC_CLIENTE,X.CRC_TARJETAPRI);
        END IF;
     END IF;
    EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACT 1:' ||X.CRC_TARJETA||'-'||V_CLIENTE||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	END;
  END IF;
    IF UPPER(X.CRC_MONEDA) = 'L' THEN
       BEGIN
       UPDATE TCREDITEXTRACTDETAIL
          SET AUTHORIZEDLIMIT = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITERD),1,LENGTH(X.CRC_LIMITERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITERD),LENGTH(X.CRC_LIMITERD)-1,2))  ,
              BALANCE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),1,LENGTH(X.CRC_SALDOACTUALRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),LENGTH(X.CRC_SALDOACTUALRD)-1,2)) ,
              EFFECTIVE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTERD),1,LENGTH(X.CRC_VIGENTERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTERD),LENGTH(X.CRC_VIGENTERD)-1,2)) ,
              WON       = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDORD),1,LENGTH(X.CRC_VENCIDORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDORD),LENGTH(X.CRC_VENCIDORD)-1,2)),
              AVAILABLE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),1,LENGTH(X.CRC_DISPONIBLERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),LENGTH(X.CRC_DISPONIBLERD)-1,2)),
              TRANSITO = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),1,LENGTH(X.CRC_SALTRANSITORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),LENGTH(X.CRC_SALTRANSITORD)-1,2)),
              FAVOR = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),1,LENGTH(X.CRC_SALFAVORRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),LENGTH(X.CRC_SALFAVORRD)-1,2)),
              DIFERIDO = TO_NUMBER(X.CRC_SALDIFRD)/100
        WHERE CREDIT = X.CRC_TARJETA
          AND CURRENCY = 0
          AND VALUETAB = 505
          AND VALUECODE = 1;
       IF SQL%NOTFOUND THEN
          INSERT INTO TCREDITEXTRACTDETAIL(CREDIT,CURRENCY,VALUETAB,VALUECODE,AUTHORIZEDLIMIT,BALANCE,
                                       EFFECTIVE,WON,AVAILABLE,TRANSITO,FAVOR,DIFERIDO)
                VALUES(X.CRC_TARJETA,0,505,1,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITERD),1,LENGTH(X.CRC_LIMITERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITERD),LENGTH(X.CRC_LIMITERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),1,LENGTH(X.CRC_SALDOACTUALRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),LENGTH(X.CRC_SALDOACTUALRD)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTERD),1,LENGTH(X.CRC_VIGENTERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTERD),LENGTH(X.CRC_VIGENTERD)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDORD),1,LENGTH(X.CRC_VENCIDORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDORD),LENGTH(X.CRC_VENCIDORD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),1,LENGTH(X.CRC_DISPONIBLERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),LENGTH(X.CRC_DISPONIBLERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),1,LENGTH(X.CRC_SALTRANSITORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),LENGTH(X.CRC_SALTRANSITORD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),1,LENGTH(X.CRC_SALFAVORRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),LENGTH(X.CRC_SALFAVORRD)-1,2)),
                 TO_NUMBER(X.CRC_SALDIFRD)/100);
       END IF;
       EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTDETAIL 2:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	   END;
	   --CALIFICACION
     BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFRD,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION,
		           CLIENT = V_CLIENTE
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CURRENCY = 0;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,0,p_fecha,X.CRC_CALIFRD,V_PROVISION);
		        END IF;
	 ELSE
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFRD,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CLIENT = V_CLIENTE
		       AND CURRENCY = 0;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,0,p_fecha,X.CRC_CALIFRD,V_PROVISION);
		        END IF;
	 END IF;

     EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTCALIF 6:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;
   --BEGIN
  --END IF;
    ELSIF UPPER(X.CRC_MONEDA) = 'I'  THEN
       BEGIN
       UPDATE TCREDITEXTRACTDETAIL
          SET AUTHORIZEDLIMIT = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITEUS),1,LENGTH(X.CRC_LIMITEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITEUS),LENGTH(X.CRC_LIMITEUS)-1,2)) ,
              BALANCE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),1,LENGTH(X.CRC_SALDOACTUALUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),LENGTH(X.CRC_SALDOACTUALUS)-1,2)) ,
              EFFECTIVE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),1,LENGTH(X.CRC_VIGENTEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),LENGTH(X.CRC_VIGENTEUS)-1,2)) ,
              WON       = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),1,LENGTH(X.CRC_VENCIDOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),LENGTH(X.CRC_VENCIDOUS)-1,2)),
              AVAILABLE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),1,LENGTH(X.CRC_DISPONIBLEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),LENGTH(X.CRC_DISPONIBLEUS)-1,2)),
              TRANSITO = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),1,LENGTH(X.CRC_SALTRANSITOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),LENGTH(X.CRC_SALTRANSITOUS)-1,2)),
              FAVOR = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),1,LENGTH(X.CRC_SALFAVORUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),LENGTH(X.CRC_SALFAVORUS)-1,2)),
              DIFERIDO = TO_NUMBER(X.CRC_SALDIFUS)/100
        WHERE CREDIT = X.CRC_TARJETA
          AND CURRENCY = 2
          AND VALUETAB = 505
          AND VALUECODE = 1;
       IF SQL%NOTFOUND THEN
          INSERT INTO TCREDITEXTRACTDETAIL(CREDIT,CURRENCY,VALUETAB,VALUECODE,AUTHORIZEDLIMIT,BALANCE,
                                       EFFECTIVE,WON,AVAILABLE,TRANSITO,FAVOR,DIFERIDO)
          VALUES(X.CRC_TARJETA,2,505,1,
                           TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITEUS),1,LENGTH(X.CRC_LIMITEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITEUS),LENGTH(X.CRC_LIMITERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),1,LENGTH(X.CRC_SALDOACTUALUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),LENGTH(X.CRC_SALDOACTUALUS)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),1,LENGTH(X.CRC_VIGENTEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),LENGTH(X.CRC_VIGENTEUS)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),1,LENGTH(X.CRC_VENCIDOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),LENGTH(X.CRC_VENCIDOUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),1,LENGTH(X.CRC_DISPONIBLEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),LENGTH(X.CRC_DISPONIBLEUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),1,LENGTH(X.CRC_SALTRANSITOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),LENGTH(X.CRC_SALTRANSITOUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),1,LENGTH(X.CRC_SALFAVORUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),LENGTH(X.CRC_SALFAVORUS)-1,2)),
                 TO_NUMBER(X.CRC_SALDIFUS)/100);
       END IF;
       EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTDETAIL 3:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	   END;
     BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFUS,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION,
		           CLIENT = V_CLIENTE
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CURRENCY = 2;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,2,p_fecha,X.CRC_CALIFUS,V_PROVISION);
		        END IF;
     ELSE
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFUS,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION,
		           CLIENT = V_CLIENTE
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CLIENT = V_CLIENTE
		       AND CURRENCY = 2;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,2,p_fecha,X.CRC_CALIFUS,V_PROVISION);
		        END IF;
     END IF;
     EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTCALIF 6:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;

    ELSIF UPPER(X.CRC_MONEDA) = 'A'  THEN
       BEGIN
       UPDATE TCREDITEXTRACTDETAIL
          SET AUTHORIZEDLIMIT = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITERD),1,LENGTH(X.CRC_LIMITERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITERD),LENGTH(X.CRC_LIMITERD)-1,2))  ,
              BALANCE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),1,LENGTH(X.CRC_SALDOACTUALRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),LENGTH(X.CRC_SALDOACTUALRD)-1,2)) ,
              EFFECTIVE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTERD),1,LENGTH(X.CRC_VIGENTERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTERD),LENGTH(X.CRC_VIGENTERD)-1,2)) ,
              WON       = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDORD),1,LENGTH(X.CRC_VENCIDORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDORD),LENGTH(X.CRC_VENCIDORD)-1,2)),
              AVAILABLE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),1,LENGTH(X.CRC_DISPONIBLERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),LENGTH(X.CRC_DISPONIBLERD)-1,2)),
              TRANSITO = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),1,LENGTH(X.CRC_SALTRANSITORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),LENGTH(X.CRC_SALTRANSITORD)-1,2)),
              FAVOR = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),1,LENGTH(X.CRC_SALFAVORRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),LENGTH(X.CRC_SALFAVORRD)-1,2)),
              DIFERIDO = TO_NUMBER(X.CRC_SALDIFRD)/100
        WHERE CREDIT = X.CRC_TARJETA
          AND CURRENCY = 0
          AND VALUETAB = 505
          AND VALUECODE = 1;
       IF SQL%NOTFOUND THEN
          INSERT INTO TCREDITEXTRACTDETAIL(CREDIT,CURRENCY,VALUETAB,VALUECODE,AUTHORIZEDLIMIT,BALANCE,
                                       EFFECTIVE,WON,AVAILABLE,TRANSITO,FAVOR,DIFERIDO)
               VALUES(X.CRC_TARJETA,0,505,1,
                                TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITERD),1,LENGTH(X.CRC_LIMITERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITERD),LENGTH(X.CRC_LIMITERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),1,LENGTH(X.CRC_SALDOACTUALRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),LENGTH(X.CRC_SALDOACTUALRD)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTERD),1,LENGTH(X.CRC_VIGENTERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTERD),LENGTH(X.CRC_VIGENTERD)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDORD),1,LENGTH(X.CRC_VENCIDORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDORD),LENGTH(X.CRC_VENCIDORD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),1,LENGTH(X.CRC_DISPONIBLERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),LENGTH(X.CRC_DISPONIBLERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),1,LENGTH(X.CRC_SALTRANSITORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),LENGTH(X.CRC_SALTRANSITORD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),1,LENGTH(X.CRC_SALFAVORRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),LENGTH(X.CRC_SALFAVORRD)-1,2)) ,
                 TO_NUMBER(X.CRC_SALDIFRD)/100);
       END IF;
       EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTDETAIL 4:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	   END;
       BEGIN
       UPDATE TCREDITEXTRACTDETAIL
          SET AUTHORIZEDLIMIT = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITEUS),1,LENGTH(X.CRC_LIMITEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITEUS),LENGTH(X.CRC_LIMITEUS)-1,2)) ,
              BALANCE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),1,LENGTH(X.CRC_SALDOACTUALUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),LENGTH(X.CRC_SALDOACTUALUS)-1,2)) ,
              EFFECTIVE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),1,LENGTH(X.CRC_VIGENTEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),LENGTH(X.CRC_VIGENTEUS)-1,2)) ,
              WON       = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),1,LENGTH(X.CRC_VENCIDOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),LENGTH(X.CRC_VENCIDOUS)-1,2)),
              AVAILABLE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),1,LENGTH(X.CRC_DISPONIBLEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),LENGTH(X.CRC_DISPONIBLEUS)-1,2)),
              TRANSITO = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),1,LENGTH(X.CRC_SALTRANSITOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),LENGTH(X.CRC_SALTRANSITOUS)-1,2)),
              FAVOR = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),1,LENGTH(X.CRC_SALFAVORUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),LENGTH(X.CRC_SALFAVORUS)-1,2)),
              DIFERIDO = TO_NUMBER(X.CRC_SALDIFUS)/100
        WHERE CREDIT = X.CRC_TARJETA
          AND CURRENCY = 2
          AND VALUETAB = 505
          AND VALUECODE = 1;
       IF SQL%NOTFOUND THEN
          INSERT INTO TCREDITEXTRACTDETAIL(CREDIT,CURRENCY,VALUETAB,VALUECODE,AUTHORIZEDLIMIT,BALANCE,
                                       EFFECTIVE,WON,AVAILABLE,TRANSITO,FAVOR,DIFERIDO)
          VALUES(X.CRC_TARJETA,2,505,1,
                                     TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITEUS),1,LENGTH(X.CRC_LIMITEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITEUS),LENGTH(X.CRC_LIMITERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),1,LENGTH(X.CRC_SALDOACTUALUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),LENGTH(X.CRC_SALDOACTUALUS)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),1,LENGTH(X.CRC_VIGENTEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),LENGTH(X.CRC_VIGENTEUS)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),1,LENGTH(X.CRC_VENCIDOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),LENGTH(X.CRC_VENCIDOUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),1,LENGTH(X.CRC_DISPONIBLEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),LENGTH(X.CRC_DISPONIBLEUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),1,LENGTH(X.CRC_SALTRANSITOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),LENGTH(X.CRC_SALTRANSITOUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),1,LENGTH(X.CRC_SALFAVORUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),LENGTH(X.CRC_SALFAVORUS)-1,2)),
                 TO_NUMBER(X.CRC_SALDIFUS)/100);
       END IF;
       EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTDETAIL 5:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	   END;
	        BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFRD,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION,
		           CLIENT = V_CLIENTE
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CURRENCY = 0;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,0,p_fecha,X.CRC_CALIFRD,V_PROVISION);
		        END IF;
     ELSE
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFRD,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CLIENT = V_CLIENTE
		       AND CURRENCY = 0;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,0,p_fecha,X.CRC_CALIFRD,V_PROVISION);
		        END IF;
     END IF;
     EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTCALIF 6:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;
     BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
	    UPDATE TCREDITEXTRACTCALIF
	       SET CALIFCODE = X.CRC_CALIFUS,
	           DATECALIF = p_fecha,
	           PROVISION = V_PROVISION,
	           CLIENT = V_CLIENTE
	     WHERE CREDIT = X.CRC_TARJETA
	       AND CURRENCY = 2;
	        IF SQL%NOTFOUND THEN
	           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
	           VALUES(X.CRC_TARJETA,V_CLIENTE,2,p_fecha,X.CRC_CALIFUS,V_PROVISION);
	        END IF;
     ELSE
	    UPDATE TCREDITEXTRACTCALIF
	       SET CALIFCODE = X.CRC_CALIFUS,
	           DATECALIF = p_fecha,
	           PROVISION = V_PROVISION
	     WHERE CREDIT = X.CRC_TARJETA
	       AND CLIENT = V_CLIENTE
	       AND CURRENCY = 2;
	        IF SQL%NOTFOUND THEN
	           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
	           VALUES(X.CRC_TARJETA,V_CLIENTE,2,p_fecha,X.CRC_CALIFUS,V_PROVISION);
	        END IF;
     END IF;
     EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTCALIF 6:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;

    END IF;
   ---datos de clientes en el bloque de tarjeta
   IF v_tipoper = 1 THEN
     V_YAEXISTE := 0;
     SELECT COUNT(*)
       INTO V_YAEXISTE
       FROM TCLI_TARJCRE
      WHERE TAR_CODCLI = V_CLIENTE
        AND TAR_NUMERO = X.CRC_TARJETA;
   IF  NVL(V_YAEXISTE,0) = 0 THEN
      BEGIN
      SELECT NVL(MAX(NVL(TAR_SECUENCIA,0)),0) + 1
        INTO V_SECUENCIA
        FROM TCLI_TARJCRE
      WHERE TAR_CODCLI = V_CLIENTE;

      INSERT INTO TCLI_TARJCRE(TAR_CODCLI,TAR_SECUENCIA,TAR_CODTAB,TAR_TIPO,TAR_TABINS,TAR_INSTITUCION,
                               TAR_NUMERO,TAR_MONEDA,TAR_CIFRASMAN,TAR_TABCIF,TAR_CIFRAS,TAR_OBSERVA,TAR_VERIFICA,
                               TAR_FECVER,TAR_USRVER)
                        VALUES(V_CLIENTE,V_SECUENCIA,21,9,5,1,X.CRC_TARJETA,0,1,NULL,1,'TARJETA MANEJADA DESDE PROCECARD','S',
                               P_FECHA,P_USUARIO);
          EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCLI_TARJCRED 6:' ||X.CRC_TARJETA||'-'||V_CLIENTE||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;
   END IF;
   ELSE
     V_YAEXISTE := 0;
     SELECT COUNT(*)
       INTO V_YAEXISTE
       FROM TCLI_CTAREF
      WHERE CUE_CODCLI = V_CLIENTE
        AND SUBSTR(CUE_OBSERVA,5,16) = X.CRC_TARJETA;
   IF  NVL(V_YAEXISTE,0) = 0 THEN
   BEGIN
      SELECT NVL(MAX(NVL(CUE_SECUENCIA,0)),0) + 1
        INTO V_SECUENCIA
        FROM TCLI_CTAREF
      WHERE CUE_CODCLI = V_CLIENTE;

        INSERT INTO TCLI_CTAREF (CUE_CODCLI,CUE_SECUENCIA,CUE_CODTAB,CUE_TIPO,CUE_NUMERO,CUE_INSTITUCION,CUE_TABINS,
                                 CUE_MONEDA,CUE_CIFMANEJO,CUE_TABCIF,CUE_CIFRAS,CUE_OBSERVA,CUE_VERIFICA,CUE_FECVER,
                                 CUE_USRVER,CUE_TABPAIS,CUE_PAIS)
            VALUES(V_CLIENTE,V_SECUENCIA,20,4,V_CLIENTE,1,2,0,2,46,1,'TAR:'||X.CRC_TARJETA||' PROCECARD','S',P_FECHA,
                    P_USUARIO,65,1);
     EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,'  TCLI_CTAREFIF 6:' ||X.CRC_TARJETA||'-'||V_CLIENTE||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;
   END IF;
   END IF;
   --fin datos
/*     EXCEPTION
         WHEN OTHERS THEN
		 v_commit  :='F';
		 ROLLBACK;
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCLI_TARCRED :' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;   */

     IF v_commit ='T' THEN
       UPDATE TCAR_REGCREDIT
          SET CRC_PROCESADO = 'S'
         WHERE ROWID = X.ROWID;
         COMMIT;
     END IF;
   END LOOP;

      SELECT COUNT(*)
        INTO por_procesar
        FROM TCAR_REGCREDIT
       WHERE NVL(CRC_PROCESADO,'N') ='N';

      SELECT COUNT(*)
        INTO procesados
        FROM TCAR_REGCREDIT
       WHERE NVL(CRC_PROCESADO,'N') ='S';
      fechafincol := SYSDATE;
      pkg_genbatch.graba_log_modulo_calif(8, P_fecha, 'FIN CARGA TARJETA CREDITO','INF_FINBATCH', fechafincol);
      pkg_genbatch.fin_logheader(8, fechainiciocol, fechafincol, por_procesar,
                                 procesados);
       COMMIT;
  EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
            fechafincol := SYSDATE;
           pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' FIN INESPERADO '||V_ERROR,'INF_FINBATCH');
           pkg_genbatch.fin_logheader(8, fechainiciocol, fechafincol, por_procesar,procesados);
           COMMIT;

  END;
--carga de tarjeta de credito
 PROCEDURE carga_tarjeta_aux(
   p_usuario IN NUMBER,
   p_sucursal IN NUMBER,
   p_oficina IN NUMBER,
   p_terminal IN VARCHAR2,
   p_fecha     IN  DATE) IS
 CURSOR P IS
   SELECT ROWID,CRC_TARJETA,CRC_TIPOID,CRC_IDENTIFICACION,CRC_CLIENTE,CRC_STATUS,CRC_FECHAVEN,
         CRC_FECHAEMISION,CRC_MONEDA,CRC_LIMITERD,CRC_LIMITEUS,CRC_SALDOACTUALRD,CRC_SALDOACTUALUS,
         CRC_DISPONIBLERD,CRC_DISPONIBLEUS,CRC_VIGENTERD,CRC_VIGENTEUS,CRC_VENCIDORD,CRC_VENCIDOUS,
         CRC_CALIFRD,CRC_CALIFUS,CRC_PROCESADO,CRC_SALTRANSITORD,
         CRC_SALTRANSITOUS ,
         CRC_SALFAVORRD    ,
         CRC_SALFAVORUS,
         CRC_SALDIFRD,
         CRC_SALDIFUS,
         CRC_NOMTA,
         CRC_TARJETAPRI
    FROM  TCAR_REGCREDIT
   WHERE NVL(CRC_PROCESADO,'N') ='N';
   V_CLIENTE TCLI_PERSONA.CLI_CODIGO%TYPE;
   V_CLIENTEANTERIOR TCLI_PERSONA.CLI_CODIGO%TYPE;
   V_CALIFCODE NUMBER(4);
   V_PROVISION NUMBER(18,6);
   V_TIPOID TCLI_PERSONA.CLI_TIPOID%TYPE;
   V_IDENTIFICA  TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
   --
   v_Pasaporte  Varchar2(20) := Null;
   v_Identificacion  TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
   v_Identificacion_Real  TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
   --
   V_TIPOPER  TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
   V_SECUENCIA TCLI_TARJCRE.TAR_SECUENCIA%TYPE;
   fechainiciocol DATE;
   fechafincol DATE;
   v_commit    VARCHAR2(1):='T';
   procesados     NUMBER;
   por_procesar   NUMBER;
   V_ERROR VARCHAR2(50);
   V_YAEXISTE NUMBER:=0;
   vInstr  		   Number:=0;
   vInstrAnt	   Number:=0;
   Cont			   Number:=0;
   vUltmaPosicion  Number:=0;
   vTieneCaracter  Varchar2(1) := 'N';
  BEGIN
    fechainiciocol := SYSDATE;
    pkg_genbatch.inicio_logheader(8,p_fecha, fechainiciocol, p_usuario, p_sucursal,
    p_oficina, p_terminal, 'S');
    pkg_genbatch.graba_log_modulo_calif(8,P_fecha, 'INICIO CARGA TARJETA DE CREDITO','INF_INICIO', fechainiciocol);
    commit;
   FOR X IN P LOOP
    v_commit  :='T';
    v_tipoid := null;
    v_identifica := null;
    v_tipoper := null;
    V_CLIENTE := null;
 	v_Pasaporte  := Null;
	v_Identificacion := Null;
	v_Identificacion_Real := Null;
	--
	vUltmaPosicion		  := 0;
	Cont				  := 0;
	vTieneCaracter		  := 'N';
	vInstr				  := 0;
	vInstrAnt			  := 3;
	--
    BEGIN
    SELECT Pasaporte
      INTO v_Pasaporte
      FROM TCar_RegClientes
     WHERE TipoId =  x.Crc_TipoId
       AND Identificacion = x.Crc_Identificacion;
    EXCEPTION
         WHEN NO_DATA_FOUND THEN
		 Null;
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F'; --'F'
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCar_RegClientes:' ||x.Crc_TipoId||'-'||x.Crc_Identificacion||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	END;
	--
	If x.CRC_tipoid In ('P','E')
	  Then
	   v_Identificacion := Nvl(Replace(Replace(v_Pasaporte,' ',''),'-',''),SubStr(x.Crc_Identificacion,2));

	   If Nvl(Replace(Replace(v_Pasaporte,' ',''),'-',''),' ') <> ' '
	      And Length(v_Identificacion) = Length(x.Crc_Identificacion)
	   	 Then
	   	   v_Identificacion_Real := Translate(v_Identificacion, 'ABCDEFGHIJKLMN?OPQRSTUVWXYZ','000000000000000000000000000');
		   --
		   If v_Identificacion_Real <> x.Crc_Identificacion
		     Then
			  	--v_Identificacion_Real := v_Identificacion||SubStr(x.Crc_Identificacion,12);
			   Begin
			   		vUltmaPosicion := To_Number(SubStr(Replace(Replace(LTrim(RTrim(v_Pasaporte)),' ',''),'-',''),vInstrAnt));
			   EXCEPTION
	      	     WHEN OTHERS
				   THEN
				   	  vTieneCaracter := 'S';
			   End;
			   --
			   If Nvl(vTieneCaracter,'N') = 'N'
			     Then
			  	 	v_Identificacion_Real := v_Identificacion||SubStr(x.Crc_Identificacion,12);
		       Else
			  	    v_Identificacion_Real := v_Identificacion;
			   End If;
		   Else
			  	v_Identificacion_Real := v_Identificacion;
		   End If;
	   Else
		  	v_Identificacion_Real := v_Identificacion;
	   End If;
	Else
		v_Identificacion_Real := SubStr(x.Crc_Identificacion,2);
	End If;
	--
    BEGIN
      convert_identifica(x.CRC_tipoid,v_Identificacion_Real,v_tipoid,v_identifica,v_tipoper);
      VAL_IDENTIFICACION(x.CRC_tipoid,v_identifica);
    EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' VAL-IDENTIFICA:' ||X.CRC_TARJETA||'-'||x.CRC_identificacion||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	END;

    BEGIN
    SELECT CLI_CODIGO
      INTO V_CLIENTE
      FROM TCLI_PERSONA
     WHERE CLI_TIPOID =  v_tipoid
       AND CLI_IDENTIFICA = v_identifica;
    EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F'; --'F'
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCLI_PERSONA:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	END;
	--V_CLIENTE:=1091;
  --ACTUALIZO CON EL NOMBRE DE LA TARJETA(PLASTICO) PARA AQUELLOS QUE ESTAN NULL
     IF   v_commit  ='T' THEN
         UPDATE TCLI_PERSONA
          SET CLI_NOMBRE= X.CRC_NOMTA,
              CLI_NOMCORRESP=X.CRC_NOMTA
        WHERE RTRIM(LTRIM(REPLACE(CLI_NOMBRE,',',''))) IS NULL
          AND CLI_TIPOID = V_TIPOID
          AND CLI_IDENTIFICA =  V_IDENTIFICA;
   END IF;
   --busco cliente anterior
   BEGIN
     SELECT CLIENT
       INTO V_CLIENTEANTERIOR
       FROM TCREDITEXTRACT
     WHERE   CREDIT = X.CRC_TARJETA;
   EXCEPTION
     WHEN OTHERS THEN
      V_CLIENTEANTERIOR := V_CLIENTE;
   END;
   --fin de cliente anterior

  IF   v_commit  ='T' THEN
    BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
       UPDATE TCREDITEXTRACT
          SET CREDITSTATUS = X.CRC_STATUS,
              EXPIREDATE   = TO_DATE(X.CRC_FECHAVEN||'01','YYYYMMDD'),
              OPENDATE     = TO_DATE(X.CRC_FECHAEMISION,'YYYYMMDD'),
              CLIENT = V_CLIENTE,
              TYPECLIENT = x.CRC_CLIENTE,
              CREDITREF= X.CRC_TARJETAPRI
       WHERE  CREDIT = X.CRC_TARJETA;
         --AND  CLIENT = V_CLIENTE;
       IF SQL%NOTFOUND THEN
        INSERT INTO TCREDITEXTRACT(CREDIT,CLIENT,CREDITSTATUS,EXPIREDATE,OPENDATE,TYPECLIENT,CREDITREF)
             VALUES( X.CRC_TARJETA,V_CLIENTE,X.CRC_STATUS,TO_DATE(X.CRC_FECHAVEN||'01','YYYYMMDD'),TO_DATE(X.CRC_FECHAEMISION,'YYYYMMDD'),X.CRC_CLIENTE,X.CRC_TARJETAPRI);
       END IF;
     ELSE
       UPDATE TCREDITEXTRACT
          SET CREDITSTATUS = X.CRC_STATUS,
              EXPIREDATE   = TO_DATE(X.CRC_FECHAVEN||'01','YYYYMMDD'),
              OPENDATE     = TO_DATE(X.CRC_FECHAEMISION,'YYYYMMDD'),
              TYPECLIENT = x.CRC_CLIENTE,
              CREDITREF = X.CRC_TARJETAPRI
       WHERE  CREDIT = X.CRC_TARJETA
         AND  CLIENT = V_CLIENTE;
       IF SQL%NOTFOUND THEN
        INSERT INTO TCREDITEXTRACT(CREDIT,CLIENT,CREDITSTATUS,EXPIREDATE,OPENDATE,TYPECLIENT,CREDITREF)
             VALUES( X.CRC_TARJETA,V_CLIENTE,X.CRC_STATUS,TO_DATE(X.CRC_FECHAVEN||'01','YYYYMMDD'),TO_DATE(X.CRC_FECHAEMISION,'YYYYMMDD'),X.CRC_CLIENTE,X.CRC_TARJETAPRI);
       END IF;
     END IF;
    EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACT 1:' ||X.CRC_TARJETA||'-'||V_CLIENTE||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	END;
  END IF;
    IF UPPER(X.CRC_MONEDA) = 'L' THEN
       BEGIN
       UPDATE TCREDITEXTRACTDETAIL
          SET AUTHORIZEDLIMIT = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITERD),1,LENGTH(X.CRC_LIMITERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITERD),LENGTH(X.CRC_LIMITERD)-1,2))  ,
              BALANCE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),1,LENGTH(X.CRC_SALDOACTUALRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),LENGTH(X.CRC_SALDOACTUALRD)-1,2)) ,
              EFFECTIVE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTERD),1,LENGTH(X.CRC_VIGENTERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTERD),LENGTH(X.CRC_VIGENTERD)-1,2)) ,
              WON       = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDORD),1,LENGTH(X.CRC_VENCIDORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDORD),LENGTH(X.CRC_VENCIDORD)-1,2)),
              AVAILABLE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),1,LENGTH(X.CRC_DISPONIBLERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),LENGTH(X.CRC_DISPONIBLERD)-1,2)),
              TRANSITO = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),1,LENGTH(X.CRC_SALTRANSITORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),LENGTH(X.CRC_SALTRANSITORD)-1,2)),
              FAVOR = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),1,LENGTH(X.CRC_SALFAVORRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),LENGTH(X.CRC_SALFAVORRD)-1,2)),
              DIFERIDO = TO_NUMBER(X.CRC_SALDIFRD)/100
        WHERE CREDIT = X.CRC_TARJETA
          AND CURRENCY = 0
          AND VALUETAB = 505
          AND VALUECODE = 1;
       IF SQL%NOTFOUND THEN
          INSERT INTO TCREDITEXTRACTDETAIL(CREDIT,CURRENCY,VALUETAB,VALUECODE,AUTHORIZEDLIMIT,BALANCE,
                                       EFFECTIVE,WON,AVAILABLE,TRANSITO,FAVOR,DIFERIDO)
                VALUES(X.CRC_TARJETA,0,505,1,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITERD),1,LENGTH(X.CRC_LIMITERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITERD),LENGTH(X.CRC_LIMITERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),1,LENGTH(X.CRC_SALDOACTUALRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),LENGTH(X.CRC_SALDOACTUALRD)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTERD),1,LENGTH(X.CRC_VIGENTERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTERD),LENGTH(X.CRC_VIGENTERD)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDORD),1,LENGTH(X.CRC_VENCIDORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDORD),LENGTH(X.CRC_VENCIDORD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),1,LENGTH(X.CRC_DISPONIBLERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),LENGTH(X.CRC_DISPONIBLERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),1,LENGTH(X.CRC_SALTRANSITORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),LENGTH(X.CRC_SALTRANSITORD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),1,LENGTH(X.CRC_SALFAVORRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),LENGTH(X.CRC_SALFAVORRD)-1,2)),
                 TO_NUMBER(X.CRC_SALDIFRD)/100);
       END IF;
       EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTDETAIL 2:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	   END;
	   --CALIFICACION
     BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFRD,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION,
		           CLIENT = V_CLIENTE
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CURRENCY = 0;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,0,p_fecha,X.CRC_CALIFRD,V_PROVISION);
		        END IF;
	 ELSE
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFRD,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CLIENT = V_CLIENTE
		       AND CURRENCY = 0;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,0,p_fecha,X.CRC_CALIFRD,V_PROVISION);
		        END IF;
	 END IF;

     EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTCALIF 6:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;
   --BEGIN
  --END IF;
    ELSIF UPPER(X.CRC_MONEDA) = 'I'  THEN
       BEGIN
       UPDATE TCREDITEXTRACTDETAIL
          SET AUTHORIZEDLIMIT = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITEUS),1,LENGTH(X.CRC_LIMITEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITEUS),LENGTH(X.CRC_LIMITEUS)-1,2)) ,
              BALANCE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),1,LENGTH(X.CRC_SALDOACTUALUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),LENGTH(X.CRC_SALDOACTUALUS)-1,2)) ,
              EFFECTIVE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),1,LENGTH(X.CRC_VIGENTEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),LENGTH(X.CRC_VIGENTEUS)-1,2)) ,
              WON       = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),1,LENGTH(X.CRC_VENCIDOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),LENGTH(X.CRC_VENCIDOUS)-1,2)),
              AVAILABLE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),1,LENGTH(X.CRC_DISPONIBLEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),LENGTH(X.CRC_DISPONIBLEUS)-1,2)),
              TRANSITO = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),1,LENGTH(X.CRC_SALTRANSITOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),LENGTH(X.CRC_SALTRANSITOUS)-1,2)),
              FAVOR = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),1,LENGTH(X.CRC_SALFAVORUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),LENGTH(X.CRC_SALFAVORUS)-1,2)),
              DIFERIDO = TO_NUMBER(X.CRC_SALDIFUS)/100
        WHERE CREDIT = X.CRC_TARJETA
          AND CURRENCY = 2
          AND VALUETAB = 505
          AND VALUECODE = 1;
       IF SQL%NOTFOUND THEN
          INSERT INTO TCREDITEXTRACTDETAIL(CREDIT,CURRENCY,VALUETAB,VALUECODE,AUTHORIZEDLIMIT,BALANCE,
                                       EFFECTIVE,WON,AVAILABLE,TRANSITO,FAVOR,DIFERIDO)
          VALUES(X.CRC_TARJETA,2,505,1,
                           TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITEUS),1,LENGTH(X.CRC_LIMITEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITEUS),LENGTH(X.CRC_LIMITERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),1,LENGTH(X.CRC_SALDOACTUALUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),LENGTH(X.CRC_SALDOACTUALUS)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),1,LENGTH(X.CRC_VIGENTEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),LENGTH(X.CRC_VIGENTEUS)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),1,LENGTH(X.CRC_VENCIDOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),LENGTH(X.CRC_VENCIDOUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),1,LENGTH(X.CRC_DISPONIBLEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),LENGTH(X.CRC_DISPONIBLEUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),1,LENGTH(X.CRC_SALTRANSITOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),LENGTH(X.CRC_SALTRANSITOUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),1,LENGTH(X.CRC_SALFAVORUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),LENGTH(X.CRC_SALFAVORUS)-1,2)),
                 TO_NUMBER(X.CRC_SALDIFUS)/100);
       END IF;
       EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTDETAIL 3:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	   END;
     BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFUS,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION,
		           CLIENT = V_CLIENTE
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CURRENCY = 2;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,2,p_fecha,X.CRC_CALIFUS,V_PROVISION);
		        END IF;
     ELSE
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFUS,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION,
		           CLIENT = V_CLIENTE
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CLIENT = V_CLIENTE
		       AND CURRENCY = 2;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,2,p_fecha,X.CRC_CALIFUS,V_PROVISION);
		        END IF;
     END IF;
     EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTCALIF 6:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;

    ELSIF UPPER(X.CRC_MONEDA) = 'A'  THEN
       BEGIN
       UPDATE TCREDITEXTRACTDETAIL
          SET AUTHORIZEDLIMIT = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITERD),1,LENGTH(X.CRC_LIMITERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITERD),LENGTH(X.CRC_LIMITERD)-1,2))  ,
              BALANCE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),1,LENGTH(X.CRC_SALDOACTUALRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),LENGTH(X.CRC_SALDOACTUALRD)-1,2)) ,
              EFFECTIVE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTERD),1,LENGTH(X.CRC_VIGENTERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTERD),LENGTH(X.CRC_VIGENTERD)-1,2)) ,
              WON       = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDORD),1,LENGTH(X.CRC_VENCIDORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDORD),LENGTH(X.CRC_VENCIDORD)-1,2)),
              AVAILABLE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),1,LENGTH(X.CRC_DISPONIBLERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),LENGTH(X.CRC_DISPONIBLERD)-1,2)),
              TRANSITO = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),1,LENGTH(X.CRC_SALTRANSITORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),LENGTH(X.CRC_SALTRANSITORD)-1,2)),
              FAVOR = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),1,LENGTH(X.CRC_SALFAVORRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),LENGTH(X.CRC_SALFAVORRD)-1,2)),
              DIFERIDO = TO_NUMBER(X.CRC_SALDIFRD)/100
        WHERE CREDIT = X.CRC_TARJETA
          AND CURRENCY = 0
          AND VALUETAB = 505
          AND VALUECODE = 1;
       IF SQL%NOTFOUND THEN
          INSERT INTO TCREDITEXTRACTDETAIL(CREDIT,CURRENCY,VALUETAB,VALUECODE,AUTHORIZEDLIMIT,BALANCE,
                                       EFFECTIVE,WON,AVAILABLE,TRANSITO,FAVOR,DIFERIDO)
               VALUES(X.CRC_TARJETA,0,505,1,
                                TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITERD),1,LENGTH(X.CRC_LIMITERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITERD),LENGTH(X.CRC_LIMITERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),1,LENGTH(X.CRC_SALDOACTUALRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALRD),LENGTH(X.CRC_SALDOACTUALRD)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTERD),1,LENGTH(X.CRC_VIGENTERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTERD),LENGTH(X.CRC_VIGENTERD)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDORD),1,LENGTH(X.CRC_VENCIDORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDORD),LENGTH(X.CRC_VENCIDORD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),1,LENGTH(X.CRC_DISPONIBLERD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLERD),LENGTH(X.CRC_DISPONIBLERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),1,LENGTH(X.CRC_SALTRANSITORD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITORD),LENGTH(X.CRC_SALTRANSITORD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),1,LENGTH(X.CRC_SALFAVORRD)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORRD),LENGTH(X.CRC_SALFAVORRD)-1,2)),
                 TO_NUMBER(X.CRC_SALDIFRD)/100);
       END IF;
       EXCEPTION
         WHEN OTHERS THEN
   		 ROLLBACK;
		 v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTDETAIL 4:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	   END;
       BEGIN
       UPDATE TCREDITEXTRACTDETAIL
          SET AUTHORIZEDLIMIT = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITEUS),1,LENGTH(X.CRC_LIMITEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITEUS),LENGTH(X.CRC_LIMITEUS)-1,2)) ,
              BALANCE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),1,LENGTH(X.CRC_SALDOACTUALUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),LENGTH(X.CRC_SALDOACTUALUS)-1,2)) ,
              EFFECTIVE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),1,LENGTH(X.CRC_VIGENTEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),LENGTH(X.CRC_VIGENTEUS)-1,2)) ,
              WON       = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),1,LENGTH(X.CRC_VENCIDOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),LENGTH(X.CRC_VENCIDOUS)-1,2)),
              AVAILABLE = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),1,LENGTH(X.CRC_DISPONIBLEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),LENGTH(X.CRC_DISPONIBLEUS)-1,2)),
              TRANSITO = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),1,LENGTH(X.CRC_SALTRANSITOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),LENGTH(X.CRC_SALTRANSITOUS)-1,2)),
              FAVOR = TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),1,LENGTH(X.CRC_SALFAVORUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),LENGTH(X.CRC_SALFAVORUS)-1,2)),
              DIFERIDO = TO_NUMBER(X.CRC_SALDIFUS)/100
        WHERE CREDIT = X.CRC_TARJETA
          AND CURRENCY = 2
          AND VALUETAB = 505
          AND VALUECODE = 1;
       IF SQL%NOTFOUND THEN
          INSERT INTO TCREDITEXTRACTDETAIL(CREDIT,CURRENCY,VALUETAB,VALUECODE,AUTHORIZEDLIMIT,BALANCE,
                                       EFFECTIVE,WON,AVAILABLE,TRANSITO,FAVOR,DIFERIDO)
          VALUES(X.CRC_TARJETA,2,505,1,
                                     TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_LIMITEUS),1,LENGTH(X.CRC_LIMITEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_LIMITEUS),LENGTH(X.CRC_LIMITERD)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),1,LENGTH(X.CRC_SALDOACTUALUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALDOACTUALUS),LENGTH(X.CRC_SALDOACTUALUS)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),1,LENGTH(X.CRC_VIGENTEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VIGENTEUS),LENGTH(X.CRC_VIGENTEUS)-1,2)) ,
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),1,LENGTH(X.CRC_VENCIDOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_VENCIDOUS),LENGTH(X.CRC_VENCIDOUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),1,LENGTH(X.CRC_DISPONIBLEUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_DISPONIBLEUS),LENGTH(X.CRC_DISPONIBLEUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),1,LENGTH(X.CRC_SALTRANSITOUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALTRANSITOUS),LENGTH(X.CRC_SALTRANSITOUS)-1,2)),
                 TO_NUMBER(SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),1,LENGTH(X.CRC_SALFAVORUS)-2)||'.'||SUBSTR(TO_CHAR(X.CRC_SALFAVORUS),LENGTH(X.CRC_SALFAVORUS)-1,2)),
                 TO_NUMBER(X.CRC_SALDIFUS)/100);
       END IF;
       EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTDETAIL 5:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	   END;
	        BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFRD,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION,
		           CLIENT = V_CLIENTE
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CURRENCY = 0;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,0,p_fecha,X.CRC_CALIFRD,V_PROVISION);
		        END IF;
     ELSE
		    UPDATE TCREDITEXTRACTCALIF
		       SET CALIFCODE = X.CRC_CALIFRD,
		           DATECALIF = p_fecha,
		           PROVISION = V_PROVISION
		     WHERE CREDIT = X.CRC_TARJETA
		       AND CLIENT = V_CLIENTE
		       AND CURRENCY = 0;
		        IF SQL%NOTFOUND THEN
		           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
		           VALUES(X.CRC_TARJETA,V_CLIENTE,0,p_fecha,X.CRC_CALIFRD,V_PROVISION);
		        END IF;
     END IF;
     EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTCALIF 6:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;
     BEGIN
     IF  V_CLIENTEANTERIOR <> V_CLIENTE THEN
	    UPDATE TCREDITEXTRACTCALIF
	       SET CALIFCODE = X.CRC_CALIFUS,
	           DATECALIF = p_fecha,
	           PROVISION = V_PROVISION,
	           CLIENT = V_CLIENTE
	     WHERE CREDIT = X.CRC_TARJETA
	       AND CURRENCY = 2;
	        IF SQL%NOTFOUND THEN
	           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
	           VALUES(X.CRC_TARJETA,V_CLIENTE,2,p_fecha,X.CRC_CALIFUS,V_PROVISION);
	        END IF;
     ELSE
	    UPDATE TCREDITEXTRACTCALIF
	       SET CALIFCODE = X.CRC_CALIFUS,
	           DATECALIF = p_fecha,
	           PROVISION = V_PROVISION
	     WHERE CREDIT = X.CRC_TARJETA
	       AND CLIENT = V_CLIENTE
	       AND CURRENCY = 2;
	        IF SQL%NOTFOUND THEN
	           INSERT INTO TCREDITEXTRACTCALIF(CREDIT,CLIENT,CURRENCY,DATECALIF,CALIFCODE,PROVISION)
	           VALUES(X.CRC_TARJETA,V_CLIENTE,2,p_fecha,X.CRC_CALIFUS,V_PROVISION);
	        END IF;
     END IF;
     EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCREDITEXTRACTCALIF 6:' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;

    END IF;
   ---datos de clientes en el bloque de tarjeta
   IF v_tipoper = 1 THEN
     V_YAEXISTE := 0;
     SELECT COUNT(*)
       INTO V_YAEXISTE
       FROM TCLI_TARJCRE
      WHERE TAR_CODCLI = V_CLIENTE
        AND TAR_NUMERO = X.CRC_TARJETA;
   IF  NVL(V_YAEXISTE,0) = 0 THEN
      BEGIN
      SELECT NVL(MAX(NVL(TAR_SECUENCIA,0)),0) + 1
        INTO V_SECUENCIA
        FROM TCLI_TARJCRE
      WHERE TAR_CODCLI = V_CLIENTE;

      INSERT INTO TCLI_TARJCRE(TAR_CODCLI,TAR_SECUENCIA,TAR_CODTAB,TAR_TIPO,TAR_TABINS,TAR_INSTITUCION,
                               TAR_NUMERO,TAR_MONEDA,TAR_CIFRASMAN,TAR_TABCIF,TAR_CIFRAS,TAR_OBSERVA,TAR_VERIFICA,
                               TAR_FECVER,TAR_USRVER)
                        VALUES(V_CLIENTE,V_SECUENCIA,21,9,5,1,X.CRC_TARJETA,0,1,NULL,1,'TARJETA MANEJADA DESDE PROCECARD','S',
                               P_FECHA,P_USUARIO);
          EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCLI_TARJCRED 6:' ||X.CRC_TARJETA||'-'||V_CLIENTE||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;
   END IF;
   ELSE
     V_YAEXISTE := 0;
     SELECT COUNT(*)
       INTO V_YAEXISTE
       FROM TCLI_CTAREF
      WHERE CUE_CODCLI = V_CLIENTE
        AND SUBSTR(CUE_OBSERVA,5,16) = X.CRC_TARJETA;
   IF  NVL(V_YAEXISTE,0) = 0 THEN
   BEGIN
      SELECT NVL(MAX(NVL(CUE_SECUENCIA,0)),0) + 1
        INTO V_SECUENCIA
        FROM TCLI_CTAREF
      WHERE CUE_CODCLI = V_CLIENTE;

        INSERT INTO TCLI_CTAREF (CUE_CODCLI,CUE_SECUENCIA,CUE_CODTAB,CUE_TIPO,CUE_NUMERO,CUE_INSTITUCION,CUE_TABINS,
                                 CUE_MONEDA,CUE_CIFMANEJO,CUE_TABCIF,CUE_CIFRAS,CUE_OBSERVA,CUE_VERIFICA,CUE_FECVER,
                                 CUE_USRVER,CUE_TABPAIS,CUE_PAIS)
            VALUES(V_CLIENTE,V_SECUENCIA,20,4,V_CLIENTE,1,2,0,2,46,1,'TAR:'||X.CRC_TARJETA||' PROCECARD','S',P_FECHA,
                    P_USUARIO,65,1);
     EXCEPTION
         WHEN OTHERS THEN
		 ROLLBACK;
		 v_commit  :='F';
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,'  TCLI_CTAREFIF 6:' ||X.CRC_TARJETA||'-'||V_CLIENTE||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;
   END IF;
   END IF;
   --fin datos
/*     EXCEPTION
         WHEN OTHERS THEN
		 v_commit  :='F';
		 ROLLBACK;
         V_ERROR := SUBSTR(SQLERRM,1,50);
		 pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' TCLI_TARCRED :' ||X.CRC_TARJETA||'-'||V_ERROR,'ERRPRO');
		 COMMIT;
	 END;   */

     IF v_commit ='T' THEN
       UPDATE TCAR_REGCREDIT
          SET CRC_PROCESADO = 'S'
         WHERE ROWID = X.ROWID;
         COMMIT;
     END IF;
   END LOOP;

      SELECT COUNT(*)
        INTO por_procesar
        FROM TCAR_REGCREDIT
       WHERE NVL(CRC_PROCESADO,'N') ='N';

      SELECT COUNT(*)
        INTO procesados
        FROM TCAR_REGCREDIT
       WHERE NVL(CRC_PROCESADO,'N') ='S';
      fechafincol := SYSDATE;
      pkg_genbatch.graba_log_modulo_calif(8, P_fecha, 'FIN CARGA TARJETA CREDITO','INF_FINBATCH', fechafincol);
      pkg_genbatch.fin_logheader(8, fechainiciocol, fechafincol, por_procesar,
                                 procesados);
       COMMIT;
  EXCEPTION
	      WHEN OTHERS THEN
           ROLLBACK;
           v_commit  :='F';
           V_ERROR := SUBSTR(SQLERRM,1,50);
            fechafincol := SYSDATE;
           pkg_genbatch.graba_log_modulo_calif(8,P_fecha,' FIN INESPERADO '||V_ERROR,'INF_FINBATCH');
           pkg_genbatch.fin_logheader(8, fechainiciocol, fechafincol, por_procesar,procesados);
           COMMIT;

  END; ---carga tarjeta aux
 --GENERA PAGOS EN BATCH
   --para pagos en batch cuando se pierda la comunicacion

   PROCEDURE pcms_pagosbatch(p_reporte  IN VARCHAR,
                            p_operador  IN NUMBER,
                            p_fecha   IN DATE) IS
   CURSOR PAGOS IS
   SELECT
		CMS_IDMESSAGE   ,
  		CMS_IDRESPOSE    ,
		CMS_DATESYSTEM  ,
		CMS_HOURSYSTEM  ,
		CMS_USER        ,
		CMS_CREDIT      ,
		CMS_CURRENCY    ,
		CMS_REFERENCE   ,
		CMS_DATETRANSACTION,
		CMS_ORIGMOVEMENT   ,
		CMS_MODEPAYMENT    ,
		CMS_TYPELINE       ,
		CMS_CODELINE       ,
		CMS_OFFICE         ,
		CMS_VALUE          ,
		CMS_IDTRANSACTION  ,
		CMS_STATUS   ,
		CMS_CODTRANS,
		CMS_PLAZO   ,
		CMS_NUMLOTE ,
		CMS_NUMSEC  ,
		CMS_REFERENCIAUNIV,
		CMS_NUMAFILIADO   ,
		CMS_AUTORIZACION,
		CMS_ESTADOREG
    FROM TRESPONSECREDIT
    WHERE CMS_DATETRANSACTION = TO_CHAR(p_fecha,'YYYYMMDD')
      AND CMS_ESTADOREG = 'P';
      --AND CMS_CODTRANS IN ('02','A1');--PENDIENTES DE APLICAR
        CODIGO NUMBER:=1; --contador de numero secuencial (1)
--CURSOR AVANCE IS

   V_TIPOTRANSACCION VARCHAR2(1) := 'P';

   V_TIPOAPLICACION VARCHAR2(1) := 'C';
   V_MARCAPROCESO VARCHAR2(1) :=NULL;
   V_ESTADOREGISTRO VARCHAR2(1):= 'P' ;
   V_CODTRANS   TRESPONSECREDIT.CMS_CODTRANS%TYPE;
   V_TYPELINE   TRESPONSECREDIT.CMS_TYPELINE%TYPE;
   V_CODELINE   TRESPONSECREDIT.CMS_CODELINE%TYPE;
   V_PLAZO      TRESPONSECREDIT.CMS_PLAZO%TYPE;
   V_MODEPAYMENT      TRESPONSECREDIT.CMS_MODEPAYMENT%TYPE;
   V_NUMAFILIADO      TRESPONSECREDIT.CMS_NUMAFILIADO%TYPE;
   V_AUTORIZACION      TRESPONSECREDIT.CMS_AUTORIZACION%TYPE;
   BEGIN
   dbms_output.put_line('paso 1:');
      pkg_legalreport.eraser(p_reporte,p_operador); --vacia el repositorio de tvaluelevelcustom
   dbms_output.put_line('paso 2:');
   pgdebug_2('uno',95);
     FOR X IN PAGOS LOOP
        pgdebug_2('dos '||codigo,96);
      --cambio para proceso batch
      IF   X.CMS_CODTRANS = 'A1' THEN
           V_TIPOTRANSACCION:='R';
           V_CODTRANS := X.CMS_CODTRANS;
           V_TYPELINE := x.CMS_TYPELINE;
           V_CODELINE := x.CMS_CODELINE;
           V_PLAZO :=x.CMS_PLAZO;
           V_MODEPAYMENT := x.CMS_MODEPAYMENT;
           --
          --V_NUMAFILIADO:='0000000000';--CONFIRMAR ESTOS AFILIADOS

           V_AUTORIZACION := X.cms_autorizacion;
           V_TIPOAPLICACION :='D';
      ELSIF   X.CMS_CODTRANS = '02' THEN
        V_TIPOTRANSACCION:='R';
        V_CODTRANS := '02';
        V_TYPELINE := x.CMS_TYPELINE;
        V_CODELINE := NULL;
        V_PLAZO :=0;
        V_MODEPAYMENT := NULL;
        /*IF TO_NUMBER(X.CMS_OFFICE) = 1 THEN
           V_NUMAFILIADO:='7200000011';
        ELSE
           V_NUMAFILIADO:='7200000029';
        END IF;*/

          V_AUTORIZACION := X.CMS_AUTORIZACION;
          V_TIPOAPLICACION :='D';
      ELSIF   X.CMS_CODTRANS = '91' THEN        --Comision
        V_TIPOTRANSACCION:='A';
        V_CODTRANS := '91';
        V_TYPELINE := x.CMS_TYPELINE;
        V_CODELINE := NULL;
        V_PLAZO :=0;
        V_MODEPAYMENT := NULL;
        /*IF TO_NUMBER(X.CMS_OFFICE) = 1 THEN
           V_NUMAFILIADO:='7200000011';
        ELSE
           V_NUMAFILIADO:='7200000029';
        END IF;*/

          V_AUTORIZACION := X.CMS_AUTORIZACION;
          V_TIPOAPLICACION :='D';
      ELSIF   X.CMS_CODTRANS = '16' THEN        --mail de procecard 15 de junio 2009 Susan de los santos
               V_CODTRANS := '04';
        V_TIPOTRANSACCION:='P';
        V_TYPELINE := x.CMS_TYPELINE;
        V_CODELINE := x.CMS_CODELINE;
        V_PLAZO :=x.CMS_PLAZO;
        V_MODEPAYMENT := x.CMS_MODEPAYMENT;
        V_NUMAFILIADO:='0000000000';
        V_AUTORIZACION := '000000';
        V_TIPOAPLICACION :='C';
      ELSE  --PARA CARGOS
        V_TIPOTRANSACCION:=x.CMS_status;
        V_CODTRANS := X.CMS_CODTRANS;
        V_TYPELINE := x.CMS_TYPELINE;
        V_CODELINE := x.CMS_CODELINE;
        V_PLAZO :=x.CMS_PLAZO;
        V_MODEPAYMENT := x.CMS_MODEPAYMENT;
        V_NUMAFILIADO:='0000000000';
        V_AUTORIZACION := '000000';
        V_TIPOAPLICACION :=x.CMS_IDTRANSACTION; --D: DEBITO C; CREDITO
      END IF;
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,1,CODIGO,1,x.CMS_CREDIT);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,2,CODIGO,2,V_CODTRANS);--x.CMS_CODTRANS);--x.CMS_CODTRANS);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,3,CODIGO,3,x.CMS_ORIGMOVEMENT);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,4,CODIGO,4,V_TIPOTRANSACCION);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,5,CODIGO,5,V_TYPELINE);--x.CMS_TYPELINE);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,6,CODIGO,6,V_CODELINE);--x.CMS_CODELINE);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUENUM) --VALUECHAR)--
            VALUES (p_reporte,p_operador,7,CODIGO,7,V_PLAZO);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,8,CODIGO,8,x.CMS_NUMLOTE);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,9,CODIGO,9,x.CMS_NUMSEC);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,10,CODIGO,10,X.CMS_REFERENCE);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,11,CODIGO,11,NULL);--X.CMS_REFERENCIAUNIV);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,12,CODIGO,12,x.CMS_OFFICE);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,13,CODIGO,13,V_MODEPAYMENT);--x.CMS_MODEPAYMENT);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,14,CODIGO,14,x.CMS_NUMAFILIADO);      --V_NUMAFILIADO);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,15,CODIGO,15,x.CMS_AUTORIZACION);  --                    V_AUTORIZACION);-- --x.CMS_AUTORIZACION);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,16,CODIGO,16,x.CMS_DATETRANSACTION);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,17,CODIGO,17,x.cms_datetransaction);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,18,CODIGO,18,x.CMS_HOURSYSTEM);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,19,CODIGO,19,x.CMS_CURRENCY);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,20,CODIGO,20,x.CMS_VALUE);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,21,CODIGO,21,V_TIPOAPLICACION);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,22,CODIGO,22,V_MARCAPROCESO);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,23,CODIGO,23,X.CMS_ESTADOREG);


          /*INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,5,CODIGO,5,x.CMS_USER);
          INSERT INTO tvaluelevelcustom (REPORT,SESSIONID,CODE,ROWSID,COLUMSID,VALUECHAR)--
            VALUES (p_reporte,p_operador,16,CODIGO,16,x.CMS_IDTRANSACTION); */

          CODIGO:=CODIGO+1;
     END LOOP;
        pgdebug_2('tre ',97);
          dbms_output.put_line('paso 4:');
          PKG_CMS_CREDITCARDLOAD.generic_file(p_reporte, p_operador, p_fecha);
        pgdebug_2('cua ',98);
      --PARA COLOCAR  UNA MARCA QUE SE CREO EL ARCHIVO PLANO
         UPDATE  TRESPONSECREDIT
            SET  CMS_ESTADOREG = 'G'--GENERADO
          --WHERE CMS_DATESYSTEM = TO_CHAR(p_fecha,'YYYYMMDD')  --MGGM 22/08/06
          WHERE CMS_DATETRANSACTION = TO_CHAR(p_fecha,'YYYYMMDD')
            AND CMS_ESTADOREG = 'P';--PENDIENTES DE APLICAR
        pgdebug_2('qun ',99);
          COMMIT;
                  pgdebug_2('sex ',90);
                       dbms_output.put_line('paso 5:');
   EXCEPTION
     WHEN OTHERS THEN
             pgdebug_2(sqlerrm,88);
                  raise_application_error(-20200, 'Error reporte Revise (90/7009)...'||sqlerrm);
   END;

 --FIN DE GENERA PAGOS


 ---transforma la identificacion del cliente
 PROCEDURE convert_identifica(
    p_tipoid IN VARCHAR2,
    p_identifica   IN VARCHAR2,
    p_tipoidfisa OUT VARCHAR2,
    p_identificafisa   OUT VARCHAR2,
    p_tipoperfisa OUT NUMBER) IS
    V_FORMATO TGEN_TIPOID.TID_FORMATO%TYPE;
    V_TIPOPE  TGEN_TIPOID.TID_TIPOPE%TYPE;
    V_TIPOID  TGEN_TIPOID.TID_TIPOID%TYPE;
    V_IDENTIFICA TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
    V_IDENTIFICAAUX TCLI_PERSONA.CLI_IDENTIFICA%TYPE;
    V_POS1 NUMBER;
    V_MAX  NUMBER:=0;
    V_CARAC  VARCHAR2(1);
    V_LONG NUMBER;
    V_VECES NUMBER:= 0;
 BEGIN
 --AQUI SE ESPERA TABLA DE MONCHY DE EQUIVALENCIA
      IF p_tipoid ='1' THEN
         V_TIPOID := 'C';
      ELSIF  p_tipoid ='2' THEN
         V_TIPOID := 'P';
      ELSE
          V_TIPOID := '1';
      END IF;
      V_TIPOID := p_tipoid;
      --V_TIPOID
    --IF UPPER(p_tipoid) <> 'N' THEN
       SELECT TID_TIPOID,TID_TIPOPE, TID_FORMATO
         INTO V_TIPOID,V_TIPOPE,V_FORMATO
        FROM TGEN_TIPOID
        WHERE TID_TIPOID =  UPPER(p_tipoid);
        V_MAX := LENGTH(V_FORMATO);
        --V_IDENTIFICA := p_identifica;
        V_IDENTIFICAAUX := p_identifica;
        V_LONG := 0;
        V_VECES := 0;
        FOR X IN 1..V_MAX LOOP
         V_CARAC := SUBSTR(V_FORMATO,X,1);
         V_LONG :=  NVL(LENGTH(V_IDENTIFICA),0);
          dbms_output.put_line('1 V_IDENTIFICA:'||V_IDENTIFICA||' V_LONG:'||V_LONG);
         IF V_CARAC = '-' THEN
            dbms_output.put_line('X:'||X||' 2 V_IDENTIFICA:'||V_IDENTIFICA||' V_IDENTIFICAAUX:'||V_IDENTIFICAAUX);
            dbms_output.put_line('X:'||X||' 2.1 V_IDENTIFICA:'||V_IDENTIFICA||' CONCAT(SUBSTR(V_IDENTIFICAAUX,1,X-1-V_LONG):'||SUBSTR(V_IDENTIFICAAUX,1,X-1-V_LONG));
            V_IDENTIFICA := V_IDENTIFICA || CONCAT(SUBSTR(V_IDENTIFICAAUX,1,X-1-V_LONG),'-');   --INICIA DESDE 2 PORQUE TIENE 12 CARACTERES
            dbms_output.put_line('X:'||X||' 3 V_IDENTIFICA:'||V_IDENTIFICA||' X-V_LONG:'||TO_CHAR(X-V_LONG)||' V_MAX:'||V_MAX);
    		V_IDENTIFICAAUX := SUBSTR(p_identifica,X-V_VECES,V_MAX) ;
            V_VECES := V_VECES + 1;
           dbms_output.put_line('X:'||X||' 4 V_IDENTIFICA:'||V_IDENTIFICA);
           dbms_output.put_line('X:'||X||' 4 V_IDENTIFICAAUX:'||V_IDENTIFICAAUX);
    	 --ELSE
         END IF;
        END LOOP;
          V_IDENTIFICA := V_IDENTIFICA ||V_IDENTIFICAAUX ;
    --END IF;
    p_tipoidfisa := V_TIPOID;--ESTO DEBEN PARAMETRIZAR DE ACUERDO AL FISA
    p_identificafisa := V_IDENTIFICA;
    p_tipoperfisa := V_TIPOPE;
 END;
--transforma estado civil
 FUNCTION convert_estadocivil(
    p_estado in varchar2
  )RETURN NUMBER IS
 BEGIN
   RETURN 2; --SOLTERO PORQUE ESO ES LO QUE SO?AMOS CUANDO SOMOS CASADOS
 END;

--transforma sexo
 FUNCTION convert_sexo(
    p_sexo in varchar2
  )RETURN VARCHAR2 IS

BEGIN
  RETURN 'M';
END;
--VALOR DE RIESGO DEL CLIENTE DE TARJETA DE CREDITO
FUNCTION riesgo_tarjeta(
   p_cliente IN NUMBER,
   p_sucursal IN NUMBER,
   p_oficina IN NUMBER,
   p_moneda  IN NUMBER,
   p_fecha     IN  DATE
  ) RETURN NUMBER IS
CURSOR P IS
  SELECT A.CREDIT,B.CURRENCY,B.EFFECTIVE,B.WON
  FROM TCREDITEXTRACT A,TCREDITEXTRACTDETAIL B
  WHERE A.CREDIT   = B.CREDIT
    AND  B.VALUECODE = 1
    AND CLIENT = P_CLIENTE;--CAPITAL UNICAMENTE
    PCOTIZA tgen_monfecha.MOF_PROMEDIO%type;
    V_VIGENTE NUMBER(18,6);
    V_VENCIDO NUMBER(18,6);
    V_NUMDEC TGEN_MONEDA.MON_NUMDEC%TYPE:=2;
BEGIN
  V_VIGENTE := 0;
  V_VENCIDO := 0;
  BEGIN
 SELECT MON_NUMDEC
   INTO V_NUMDEC
   FROM TGEN_MONEDA
  WHERE MON_COD=P_MONEDA;
  EXCEPTION
  WHEN OTHERS THEN
     raise_application_error(-20200, 'Revise Moneda..'||sqlerrm);
  END;

  FOR X IN P LOOP
     --cambio a pesos primeramente O MONEDA LOCAL PRIMERO
         pkg_cambios.PCOTPROMEDIO(p_fecha,p_sucursal,x.currency,PCOTIZA);
         V_VIGENTE := V_VIGENTE +  NVL(X.EFFECTIVE,0)*PCOTIZA;
         V_VENCIDO := V_VENCIDO +  NVL(X.WON,0)*PCOTIZA;
  END LOOP;
     IF p_moneda = 0 then
          RETURN ROUND(NVL(V_VIGENTE,0) + NVL(V_VENCIDO,0),V_NUMDEC);
     ELSE
        pkg_cambios.PCOTPROMEDIO(p_fecha,p_sucursal,p_moneda,PCOTIZA);
        V_VIGENTE := V_VIGENTE /PCOTIZA;
        V_VENCIDO := V_VENCIDO/PCOTIZA;
        RETURN ROUND(NVL(V_VIGENTE,0) + NVL(V_VENCIDO,0),V_NUMDEC);
     END IF;
END;
END;
/
