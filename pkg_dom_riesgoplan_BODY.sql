CREATE OR REPLACE
PACKAGE BODY pkg_dom_riesgoplan AS
procedure exec_credact(
p_operador in number,
p_FechaCorte in Date,
p_codfile in varchar2) is
begin
 pkg_dom_riesgoplan.rsk_credact(p_operador => p_operador,
                                 p_dml => 'D',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile);
                                 
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'D',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
commit;                                   
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'I',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
                                   
 pkg_dom_riesgoplan.rsk_credact(p_operador => p_operador,
                                 p_dml => 'I',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile);

commit;                                   
end;         
-------------------------------------------------------------------------------------Plantilla Cuentas Corrientes, Ahorros, OTRAS ENTIDADES ----------------------------------
procedure rsk_DISPliq(
p_operador in number,
p_dml in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2) is
	v_ruc varchar2(19);
--variables tabla
v_CODIGOBIC                                VARCHAR2(8);
v_NUMEROCUENTA                             VARCHAR2(30);
v_CUENTACONTABLECAP                        VARCHAR2(30);
v_NOMBREENTIDAD                            VARCHAR2(60);
v_CODIGOCATEGORIA                          NUMBER(2);
v_FECHAAPERTURA                            VARCHAR2(10);
v_MONEDA                                   NUMBER(2);
v_TASAINTERES                              NUMBER(7,3);
v_FRECUENCIAREPRE                          NUMBER(3);
v_SALDOFINAL                               NUMBER(17,2);
v_SALDOPROMEDIO                            NUMBER(17,2);
v_SECTORECONOMICO                          NUMBER(6);
v_SUBPOBLACION                             VARCHAR2(40);
v_PRODUCTO                                 VARCHAR2(30);
v_TIPOSUJETO                               VARCHAR2(2) ;
v_SUBPOBLACION1                             VARCHAR2(40);
v_SUBPOBLACION2                            VARCHAR2(40);
v_SUBPOBLACION3                             VARCHAR2(40);
v_TIPOVINCULACION                          VARCHAR2(2) ;
v_ESTATUSCUENTA                            VARCHAR2(1) ;
v_LOCALIDAD                                NUMBER(6);
v_TIPOCLIENTE                              NUMBER(3);
v_CUENTACONTABLEINT                        VARCHAR2(30);
v_TIPODEPOSITANTE                          VARCHAR2(1);   
V_BASETASAINTERES                          NUMBER(1);
--fin
cursor ca is	
	Select a.vis_codCli Cliente, 
   (select substr(bic_swift,1,8) from TDIP_LBTRBANCOS where bic_rnc =   Tdom_Utilidades.IDCLIENTE(a.vis_codCli) AND ROWNUM = 1) Bic,	
	VIS_MOD modu,
	VIS_PRO pro,        
	VIS_TIP tip,       
	VIS_MONEDA mon, 
	vis_MOD moduacp,
	acp_PRO proacp,        
	acp_TIP tipacp,       
	acp_MON monacp, 	
	VIS_FECHAPE fechape,
	vis_status status,
	decode(vis_moneda,0,0,2,1,7,2,0) monedarisk,
	vis_suc sucursal,
	vis_ofi oficina,
	vis_numdircor numdir,
	acp_intacumcont rendimientos,
	b.Acp_Fecha fecha_inicio, 
	       a.vis_numcue  Cuenta,
	       a.VIS_CTAEXTERNA CuentaExterna,
	       '25' sector,
	      DECODE (acp_mon,  0, 'DOP',  2, 'USD',  7, 'EUR',  'OTRO') cod_moneda,---13 tipo de moneda
	       Tdom_Utilidades.NOMBRECLIENTE(a.vis_codCli)Nombre, 
	       Tdom_Utilidades.IDCLIENTE(a.vis_codCli) Identificacion,
	       Tdom_Utilidades.DIRECCIONCLIENTE(a.vis_codCli) Direccion,
	       Tdom_Utilidades.TelePersonas(a.vis_codCli) Telefonos,
	Tdom_Utilidades.Tcap_SaldoTotalCuenta(a.Vis_Numcue,Vis_Moneda,b.Acp_Fecha) monto_operacion,
	        bdi_tasa_pas1_fecha(vis_mod,vis_pro,vis_tip,vis_moneda,Tdom_Utilidades.Tcap_SaldoTotalCuenta(a.Vis_Numcue,Vis_Moneda,b.Acp_Fecha),
	      vis_numcue,vis_moneda,acp_fecha)  Tasa
	From Tcap_vista a,
			 Tcap_Acrpas b
	Where b.Acp_numCue = a.vis_numCue
	And   b.Acp_Fecha = p_FechaCorte
	And   a.Vis_Pro In (99)
	And   a.vis_moneda = b.acp_mon   ;
--	and rownum <=10;
	  
	  --and pre_credito = 6010004392;
	  ---AND ROWNUM <= 20;
	nombre       tcli_persona.cli_nombre%type;
    apellido     tcli_persona.cli_nombre%type;
    identifica   varchar2(20);
    tipocli      char(2);	
    v_numcomprom number;
    vln_monextra number;
    v_fechacuoextraaux date;
    contador number;   
    --nombre       tcli_persona.cli_nombre%type;
    --apellido     tcli_persona.cli_nombre%type;
   -- identifica   varchar2(20);
    --tipocli      char(2);	
  v_tipocli NUMBER;  
  v_esfuera number;
begin  
	select INS_RUC into v_ruc from tgen_instit; 

if p_dml = 'D' then
	     delete rsk_dispLIQ
	      where 
	       FECHACORTE = p_FechaCorte
	       and EMPRESA = v_ruc 
	       and CODFILE = p_codfile;
end if;          
if p_dml = 'I' then
   --PKG_LEGAL_DOM_DE_risk.DE11('DE11',p_operador,1,p_fechacorte,p_fechacorte,20,0,0,5000000,0) ;
   --PKG_LEGAL_DOM_DE_risk.DE13('DE13',p_operador,1,p_fechacorte,22,0,0,0);
   --PKG_LEGAL_DOM_DE_risk.DE15('DE15',p_operador,1,p_fechacorte,21,0,0,0);
   
  contador :=0;
   for x in ca loop    
v_CODIGOBIC:= x.bic;          
DBMS_OUTPUT.PUT_LINE('PASO 1:'||x.cuenta);
v_NUMEROCUENTA:= x.cuenta;--||'-'||x.CuentaExterna;
DBMS_OUTPUT.PUT_LINE('PASO 2');
v_CUENTACONTABLECAP:=cuenta_connos(x.modu,x.pro,x.tip,x.mon,132,1);
DBMS_OUTPUT.PUT_LINE('PASO 3');
v_NOMBREENTIDAD:= replace(x.Nombre,',','');
DBMS_OUTPUT.PUT_LINE('PASO 4');
--90/7002
v_CODIGOCATEGORIA:= codigo_cat(x.modu,x.pro,x.tip,x.mon);       
DBMS_OUTPUT.PUT_LINE('PASO 5');
v_FECHAAPERTURA:=to_char(x.fechape,'ddmmyyyy');          
DBMS_OUTPUT.PUT_LINE('PASO 6');
v_MONEDA:= x.monedarisk;       
DBMS_OUTPUT.PUT_LINE('PASO 7');
v_TASAINTERES:= bdi_tasa_pas1_fecha(x.modu,x.pro,x.tip,x.mon,x.monto_operacion,x.cuenta,x.mon,p_FechaCorte);           
DBMS_OUTPUT.PUT_LINE('PASO 8');
v_FRECUENCIAREPRE:= 0;                       
DBMS_OUTPUT.PUT_LINE('PASO 9');
v_SALDOFINAL:=x.monto_operacion;             
--v_SALDOPROMEDIO:= x.rendimientos;         
select nvl(sum(prm_valor),0)
  into v_SALDOPROMEDIO
 from tcap_promedios 
 where prm_numcue = x.Cuenta
   and prm_fecha = p_FechaCorte;             
   DBMS_OUTPUT.PUT_LINE('PASO 10');
v_SECTORECONOMICO :=652000;--FIJO INFORMAR
v_SUBPOBLACION:= null;
v_PRODUCTO:= null;
   pkg_legalreport3.datos_cliente(x.cliente,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido); 
                                       
DBMS_OUTPUT.PUT_LINE('PASO 11:');
select count(*)
 into v_esfuera
 from tcli_direccion
 where dir_codcli = x.cliente
   and dir_tipodir = 1
   and dir_pais <> 1 ; 
if v_esfuera <> 0 then 
   tipocli:='E3'; 
end if;
DBMS_OUTPUT.PUT_LINE('PASO 12:');
v_TIPOSUJETO:=tipocli; 
V_SUBPOBLACION1:=NULL;
V_SUBPOBLACION2:=NULL;
V_SUBPOBLACION3:=NULL;
V_BASETASAINTERES:=0;--FIJO               

v_TIPOVINCULACION:= Val_Relacion_bco(x.cliente);       
DBMS_OUTPUT.PUT_LINE('PASO 13:');
v_ESTATUSCUENTA:= status_cta(x.moduacp,x.proacp,x.tipacp,x.monacp,x.cuenta,x.status,p_fechaCorte);
DBMS_OUTPUT.PUT_LINE('PASO 14:');
v_LOCALIDAD:= PKG_LEGAL_DOM_de.fun_localidad(x.sucursal,x.oficina,x.cliente,x.numdir);              
DBMS_OUTPUT.PUT_LINE('PASO 15:');
    begin                               
     select nat_subtipocliente
       into v_tipocli
     from tcli_natural
     where nat_codcli = x.cliente;
    exception
      when no_data_found then
      select jur_subtipocliente
        into v_tipocli
       from tcli_juridica 
       where jur_codcli = x.cliente;
    end;                     
    DBMS_OUTPUT.PUT_LINE('PASO 16:');
    --V_TIPOCLIENTE:=Val_desctabla_iso(305,v_tipocli); 
    --v_CUENTACONTABLEINT:= cuenta_con(x.modu,x.pro,x.tip,x.mon,87);--PARAMETRIZAR EN LA 90/2041 REP 21/4963
    --v_TIPODEPOSITANTE:='M';
    
   insert into rsk_DISPLIQ(FECHACORTE,EMPRESA,CODFILE,CODIGOBIC,NUMEROCUENTA,CUENTACONTABLECAP,NOMBREENTIDAD,CODIGOCATEGORIA,FECHAAPERTURA,MONEDA,TASAINTERES,FRECUENCIAREPRE,SALDOFINAL,
						   SALDOPROMEDIO,SECTORECONOMICO,SUBPOBLACION,PRODUCTO,TIPOSUJETO,
						   SUBPOBLACION1,SUBPOBLACION2,SUBPOBLACION3,BASETASAINTERES,
						   TIPOVINCULACION,ESTATUSCUENTA,LOCALIDAD)
   values(p_FechaCorte,v_ruc,p_CODFILE,V_CODIGOBIC,V_NUMEROCUENTA,V_CUENTACONTABLECAP,V_NOMBREENTIDAD,V_CODIGOCATEGORIA,V_FECHAAPERTURA,V_MONEDA,V_TASAINTERES,V_FRECUENCIAREPRE,V_SALDOFINAL,
						   V_SALDOPROMEDIO,V_SECTORECONOMICO,V_SUBPOBLACION,V_PRODUCTO,
						   V_TIPOSUJETO,V_SUBPOBLACION1,V_SUBPOBLACION2,V_SUBPOBLACION3,
						   V_BASETASAINTERES,V_TIPOVINCULACION,
						   V_ESTATUSCUENTA,V_LOCALIDAD);
 		  contador := contador + 1;
 		  commit;
  end loop;  
  risk_cabecera(p_operador,'U',p_FechaCorte,p_CODFILE,to_CHAR(sysdate,'dd/mm/yyyy'),contador,00000,000000000000);
  file_displiq(p_operador,v_ruc,p_FechaCorte,p_codfile);
end if;	
end rsk_DISPliq;  
  
procedure file_dispLIQ(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2) is
cursor c is
select * 
from rsk_cabecera
 where FECHA=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;

cursor p is
select *
  from rsk_dispLIQ
 where FECHACORTE=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;
           
    w_line      varchar2(8000);
    w_columna   varchar2(4000);
    w_file      UTL_FILE.FILE_TYPE;
    w_obtfile   boolean;
    w_directory varchar2(100);
    lnNot_open EXCEPTION; 
    v_concaten varchar2(1):=',';
begin   
    --w_directory := PKG_LEGALREPORT3.to_directory();
    w_file      := UTL_FILE.FOPEN('DIR_ASM', p_codfile||'.txt', 'w',32760);
    w_obtfile   := UTL_FILE.IS_OPEN(w_file);
    IF w_obtfile = TRUE THEN  
   		w_line := null;
		for y in c loop
        w_columna := PKG_LEGALREPORT3.to_format(y.empresa,'C',20,'B',null);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		     
		w_columna := PKG_LEGALREPORT3.to_format(y.CODFILE,'C',11,'B',null);        
        w_line    := w_line || w_columna;                                        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		             
		w_columna := PKG_LEGALREPORT3.to_format(y.FECHAENV,'C',10,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.NUMRECORDS,'C',12,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.STATUS,'C',5,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.STRECORD,'C',12,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
		end loop;  
        --UTL_FILE.PUT(w_file, w_line);
        UTL_FILE.NEW_LINE(w_file, 1);		     
        w_line:=null;
		for x in p loop 
		w_line:=null;
        dbms_output.put_line('Paso 1');
        w_columna := PKG_LEGALREPORT3.to_format(to_char(x.FECHACORTE,'ddmmyyyy'),'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);  
        dbms_output.put_line('Paso 2');        
        w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOBIC,'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
        dbms_output.put_line('Paso 3');
                               
        w_columna := PKG_LEGALREPORT3.to_format(x.NUMEROCUENTA,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
                                    
        dbms_output.put_line('Paso 4');                
        w_columna:= pkg_legalreport3.obt_formatvalue('C', x.CUENTACONTABLECAP,'XXX.XX.X.XX.XX.XX.XX.XX.XX.XX.XX.XX');                                          
		w_columna := PKG_LEGALREPORT3.to_format(w_columna,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);                           
                                    
        dbms_output.put_line('Paso 5');         
           w_columna := PKG_LEGALREPORT3.to_format(x.NOMBREENTIDAD,'C',60,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                    
        dbms_output.put_line('Paso 6');                
           w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOCATEGORIA,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                    
        dbms_output.put_line('Paso 7');                
        w_columna := PKG_LEGALREPORT3.to_format((x.FECHAAPERTURA),'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);  
                                    
        dbms_output.put_line('Paso 8');        
           w_columna := PKG_LEGALREPORT3.to_format(x.MONEDA,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 9');
           w_columna := PKG_LEGALREPORT3.to_format(x.TASAINTERES,'C',7,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 10');

           w_columna := PKG_LEGALREPORT3.to_format(x.FRECUENCIAREPRE,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 11');
                        
           w_columna := PKG_LEGALREPORT3.to_format(x.SALDOFINAL,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 12');                
           w_columna := PKG_LEGALREPORT3.to_format(x.SALDOPROMEDIO,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 13');
           w_columna := PKG_LEGALREPORT3.to_format(x.SECTORECONOMICO,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 13');
           w_columna := PKG_LEGALREPORT3.to_format(x.SUBPOBLACION,'C',40,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 14');
           w_columna := PKG_LEGALREPORT3.to_format(x.PRODUCTO,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                            
        dbms_output.put_line('Paso 15');                
           w_columna := PKG_LEGALREPORT3.to_format(x.TIPOSUJETO,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 13');
           w_columna := PKG_LEGALREPORT3.to_format(x.SUBPOBLACION1,'C',40,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 13');
           w_columna := PKG_LEGALREPORT3.to_format(x.SUBPOBLACION2,'C',40,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                                                  
        dbms_output.put_line('Paso 13');
           w_columna := PKG_LEGALREPORT3.to_format(x.SUBPOBLACION3,'C',40,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                
        dbms_output.put_line('Paso 13');
           w_columna := PKG_LEGALREPORT3.to_format(x.BASETASAINTERES,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 16');                
           w_columna := PKG_LEGALREPORT3.to_format(x.TIPOVINCULACION,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 17');                
           w_columna := PKG_LEGALREPORT3.to_format(x.ESTATUSCUENTA,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 18');                
           w_columna := PKG_LEGALREPORT3.to_format(x.LOCALIDAD,'C',6,'B',null);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
               
        dbms_output.put_line('Paso 22');
        --w_columna := PKG_LEGALREPORT3.to_format(x.NOMBRECODEUDOR,'C',2,'B',v_concaten);        
        --w_line    := w_line || w_columna;      
          --      UTL_FILE.PUT(w_file,w_columna);                   
                
        UTL_FILE.NEW_LINE(w_file, 1);		             
		end loop;   
     ELSE
      RAISE lnNot_open;
    END IF;
    UTL_FILE.FCLOSE(w_file);
  EXCEPTION
    WHEN lnNot_open THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 1: lnNot_open');
    WHEN NO_DATA_FOUND THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 2: no data found');
    WHEN value_error THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 3: value error');
    WHEN UTL_FILE.INVALID_FILEHANDLE THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 4: invalid file handle');
    WHEN UTL_FILE.INVALID_OPERATION THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 5: invalid operation');
    WHEN UTL_FILE.READ_ERROR THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 6: file read');
    WHEN UTL_FILE.WRITE_ERROR THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 7: write error');
    WHEN UTL_FILE.INVALID_PATH THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900,
                              'ERROR 8: invalid path' || ' ' || w_directory);
    WHEN UTL_FILE.INVALID_MODE THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 9: invalid mode');		
end file_dispLIQ;      
     
procedure exec_displiq(
p_operador in number,
p_FechaCorte in Date,
p_codfile in varchar2) is
begin
 pkg_dom_riesgoplan.rsk_displiq(p_operador => p_operador,
                                 p_dml => 'D',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile);
                                 
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'D',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
commit;                                   
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'I',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
                                   
 pkg_dom_riesgoplan.rsk_displiq(p_operador => p_operador,
                                 p_dml => 'I',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile);

commit;                                   
end exec_displiq; 

                                                     
-------------------------------------------------------------------------------------Plantilla Cuentas Corrientes, Ahorros, Depósitos a la Vista----------------------------------
procedure rsk_captliq(
p_operador in number,
p_dml in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2) is
	v_ruc varchar2(19);
--variables tabla
v_CODIGOCLIENTE                            VARCHAR2(30);
v_NUMEROCUENTA                             VARCHAR2(30);
v_CUENTACONTABLECAP                        VARCHAR2(30);
v_NOMBRECLIENTE                            VARCHAR2(60);
v_CODIGOCATEGORIA                          NUMBER(2);
v_FECHAAPERTURA                            VARCHAR2(10);
v_MONEDA                                   NUMBER(2);
v_TASAINTERES                              NUMBER(7,3);
v_FRECUENCIAREPRE                          NUMBER(3);
v_SALDOFINAL                               NUMBER(17,2);
v_RENDIMIENTOS                             NUMBER(17,2);
V_SECTORECONOMICO 						   NUMBER(6);
v_SUBPOBLACION                             VARCHAR2(40);
v_PRODUCTO                                 VARCHAR2(30);
v_TIPOSUJETO                               VARCHAR2(2) ;
v_TIPOVINCULACION                          VARCHAR2(2) ;
v_ESTATUSCUENTA                            VARCHAR2(1) ;
v_LOCALIDAD                                NUMBER(6);
v_TIPOCLIENTE                              NUMBER(3);
v_CUENTACONTABLEINT                        VARCHAR2(30);
v_TIPODEPOSITANTE                          VARCHAR2(1);
--fin      
cursor ca is	
	Select a.vis_codCli Cliente,
	VIS_MOD modu,
	VIS_PRO pro,        
	VIS_TIP tip,       
	VIS_MONEDA mon, 
	vis_MOD moduacp,
	acp_PRO proacp,        
	acp_TIP tipacp,       
	acp_MON monacp, 	
	VIS_FECHAPE fechape,
	vis_status status,
	decode(vis_moneda,0,0,2,1,7,2,0) monedarisk,
	vis_suc sucursal,
	vis_ofi oficina,
	vis_numdircor numdir,
	--acp_intacumcont rendimientos, 
	--FGETDOMSALDOVISTA(a.Vis_Numcue,b.Acp_Fecha,2,87) rendimientos, 
	b.Acp_Fecha fecha_inicio, 
	       a.vis_numcue  Cuenta,
	       '25' sector,
	      DECODE (acp_mon,  0, 'DOP',  2, 'USD',  7, 'EUR',  'OTRO') cod_moneda,---13 tipo de moneda
	       Tdom_Utilidades.NOMBRECLIENTE(a.vis_codCli)Nombre, 
	       Tdom_Utilidades.IDCLIENTE(a.vis_codCli) Identificacion,
	       Tdom_Utilidades.DIRECCIONCLIENTE(a.vis_codCli) Direccion,
	       Tdom_Utilidades.TelePersonas(a.vis_codCli) Telefonos,
	--Tdom_Utilidades.Tcap_SaldoTotalCuenta(a.Vis_Numcue,Vis_Moneda,b.Acp_Fecha) monto_operacion,
	--FGETDOMSALDOVISTA(a.Vis_Numcue,b.Acp_Fecha,1,79) monto_operacion,
	  c.saldoc monto_operacion,
	  c.cuentaCon CuentaContableCap,
	        bdi_tasa_pas1_fecha(vis_mod,vis_pro,vis_tip,vis_moneda,Tdom_Utilidades.Tcap_SaldoTotalCuenta(a.Vis_Numcue,Vis_Moneda,b.Acp_Fecha),
	      vis_numcue,vis_moneda,acp_fecha)  Tasa
	From Tcap_vista a,
			 Tcap_Acrpas b,(SELECT gdt_operacion,GDT_CUENTA cuentaCon,sum(nvl(gdt_saldomod,0)) saldoc
								  FROM TAUX_FUNCGROUPDET_det 
								  where GDT_OPERADOR = p_operador  
								    and GDT_CUENTA not like '219%'
								    --and gdt_codgru = 79
								  group by gdt_operacion,GDT_CUENTA) c
	Where b.Acp_numCue = a.vis_numCue
	And   b.Acp_Fecha = p_FechaCorte
	And   a.Vis_Pro Not In (0,50,99)
	--And  acp_status not in ('6')
	And   a.vis_moneda = b.acp_mon     
	--and CuentaCon not like '214%'
	And c.gdt_operacion = b.acp_numcue   ;

cursor ca_0 is	
	Select a.vis_codCli Cliente,
	VIS_MOD modu,
	VIS_PRO pro,        
	VIS_TIP tip,       
	VIS_MONEDA mon, 
	vis_MOD moduacp,
	acp_PRO proacp,        
	acp_TIP tipacp,       
	acp_MON monacp, 	
	VIS_FECHAPE fechape,
	vis_status status,
	decode(vis_moneda,0,0,2,1,7,2,0) monedarisk,
	vis_suc sucursal,
	vis_ofi oficina,
	vis_numdircor numdir,
	acp_intacumcont rendimientos,
	b.Acp_Fecha fecha_inicio, 
	       a.vis_numcue  Cuenta,
	       '25' sector,
	      DECODE (acp_mon,  0, 'DOP',  2, 'USD',  7, 'EUR',  'OTRO') cod_moneda,---13 tipo de moneda
	       Tdom_Utilidades.NOMBRECLIENTE(a.vis_codCli)Nombre, 
	       Tdom_Utilidades.IDCLIENTE(a.vis_codCli) Identificacion,
	       Tdom_Utilidades.DIRECCIONCLIENTE(a.vis_codCli) Direccion,
	       Tdom_Utilidades.TelePersonas(a.vis_codCli) Telefonos,
	Tdom_Utilidades.Tcap_SaldoTotalCuenta(a.Vis_Numcue,Vis_Moneda,b.Acp_Fecha) monto_operacion,
	        bdi_tasa_pas1_fecha(vis_mod,vis_pro,vis_tip,vis_moneda,Tdom_Utilidades.Tcap_SaldoTotalCuenta(a.Vis_Numcue,Vis_Moneda,b.Acp_Fecha),
	      vis_numcue,vis_moneda,acp_fecha)  Tasa
	From Tcap_vista a,
			 Tcap_Acrpas b
	Where b.Acp_numCue = a.vis_numCue
	And   b.Acp_Fecha = p_FechaCorte
	And   a.Vis_Pro Not In (0,50,99)
	And   a.vis_moneda = b.acp_mon   ;
--	and rownum <=10;
	  
	  --and pre_credito = 6010004392;
	  ---AND ROWNUM <= 20;
	nombre       tcli_persona.cli_nombre%type;
    apellido     tcli_persona.cli_nombre%type;
    identifica   varchar2(20);
    tipocli      char(2);	
    v_numcomprom number;
    vln_monextra number;
    v_fechacuoextraaux date;
    contador number;   
    --nombre       tcli_persona.cli_nombre%type;
    --apellido     tcli_persona.cli_nombre%type;
   -- identifica   varchar2(20);
    --tipocli      char(2);	
  v_tipocli NUMBER;  
begin  
	select INS_RUC into v_ruc from tgen_instit; 

if p_dml = 'D' then
	     delete rsk_CAPTLIQ
	      where 
	       FECHACORTE = p_FechaCorte
	       and EMPRESA = v_ruc 
	       and CODFILE = p_codfile;
end if;          
if p_dml = 'I' then
   --PKG_LEGAL_DOM_DE_risk.DE11('DE11',p_operador,1,p_fechacorte,p_fechacorte,20,0,0,5000000,0) ;
   --PKG_LEGAL_DOM_DE_risk.DE13('DE13',p_operador,1,p_fechacorte,22,0,0,0);
   --PKG_LEGAL_DOM_DE_risk.DE15('DE15',p_operador,1,p_fechacorte,21,0,0,0);
   
  contador :=0;
	pkg_capdom_cuadraturadet.pro_totales(p_fechaCorte,'0','99999999999999',1,999,p_operador,21,4963,1,0);
	commit;        
   for x in ca loop    
v_CODIGOCLIENTE:= x.Identificacion;          
v_NUMEROCUENTA:= x.Cuenta;
v_CUENTACONTABLECAP:=x.CuentaContableCap;--cuenta_con(x.modu,x.pro,x.tip,x.mon,79);
v_NOMBRECLIENTE:= replace(x.Nombre,',','');
--90/7002
v_CODIGOCATEGORIA:= codigo_cat(x.modu,x.pro,x.tip,x.mon);       
v_FECHAAPERTURA:=to_char(x.fechape,'ddmmyyyy');          
v_MONEDA:= x.monedarisk;
v_FRECUENCIAREPRE:= 0;
v_SALDOFINAL:=x.monto_operacion;--FGETDOMSALDOVISTA(x.cuenta,p_FechaCorte,1,79);--x.monto_operacion;             
v_RENDIMIENTOS:= FGETDOMSALDOVISTA(p_operador,x.cuenta,p_FechaCorte,87);--x.rendimientos;             
v_TASAINTERES:= bdi_tasa_pas1_fecha(x.modu,x.pro,x.tip,x.mon,v_SALDOFINAL,x.cuenta,x.mon,p_FechaCorte);           
BEGIN
V_SectorEconomico:=pkg_legal_dom_de.fun_actividad_cliente(X.Cliente);
EXCEPTION
WHEN OTHERS THEN
V_SectorEconomico:=NULL;
END;
v_SUBPOBLACION:= null;
v_PRODUCTO:= null;
   pkg_legalreport3.datos_cliente(x.cliente,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido); 
                                       
v_TIPOSUJETO:=tipocli;             
v_TIPOVINCULACION:= Val_Relacion_bco(x.cliente);       
v_ESTATUSCUENTA:= status_cta(x.moduacp,x.proacp,x.tipacp,x.monacp,x.cuenta,x.status,p_fechaCorte);
v_LOCALIDAD:= PKG_LEGAL_DOM_de.fun_localidad(x.sucursal,x.oficina,x.cliente,x.numdir);              
    begin                               
     select nat_subtipocliente
       into v_tipocli
     from tcli_natural
     where nat_codcli = x.cliente;
    exception
      when no_data_found then
      select jur_subtipocliente
        into v_tipocli
       from tcli_juridica 
       where jur_codcli = x.cliente;
    end;
    V_TIPOCLIENTE:=Val_desctabla_iso(305,v_tipocli); 
    v_CUENTACONTABLEINT:= cuenta_con(x.modu,x.pro,x.tip,x.mon,87);--PARAMETRIZAR EN LA 90/2041 REP 21/4963
    v_TIPODEPOSITANTE:='M';
    
   insert into rsk_CAPTLIQ(FECHACORTE,EMPRESA,CODFILE,CODIGOCLIENTE,NUMEROCUENTA,CUENTACONTABLECAP,NOMBRECLIENTE,CODIGOCATEGORIA,FECHAAPERTURA,MONEDA,TASAINTERES,FRECUENCIAREPRE,
						   SALDOFINAL,RENDIMIENTOS,SectorEconomico,SUBPOBLACION,PRODUCTO,TIPOSUJETO,TIPOVINCULACION,ESTATUSCUENTA,LOCALIDAD,TIPOCLIENTE,
						   CUENTACONTABLEINT,TIPODEPOSITANTE)
   values(p_FechaCorte,v_ruc,p_CODFILE,v_CODIGOCLIENTE,v_NUMEROCUENTA,v_CUENTACONTABLECAP,v_NOMBRECLIENTE,v_CODIGOCATEGORIA,v_FECHAAPERTURA,v_MONEDA,v_TASAINTERES,v_FRECUENCIAREPRE,
						   v_SALDOFINAL,nvl(v_RENDIMIENTOS,0),v_SectorEconomico ,v_SUBPOBLACION,v_PRODUCTO,v_TIPOSUJETO,v_TIPOVINCULACION,v_ESTATUSCUENTA,v_LOCALIDAD,v_TIPOCLIENTE,nvl(v_CUENTACONTABLEINT,0),v_TIPODEPOSITANTE);
 		  contador := contador + 1;
 		  commit;
  end loop;  
  risk_cabecera(p_operador,'U',p_FechaCorte,p_CODFILE,to_CHAR(sysdate,'dd/mm/yyyy'),contador,00000,000000000000);
  file_captliq(p_operador,v_ruc,p_FechaCorte,p_codfile);
end if;	
end;    

procedure file_CAPTLIQ(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2) is
cursor c is
select * 
from rsk_cabecera
 where FECHA=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;

cursor p is
select *
  from rsk_CAPTLIQ
 where FECHACORTE=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;
           
    w_line      varchar2(8000);
    w_columna   varchar2(4000);
    w_file      UTL_FILE.FILE_TYPE;
    w_obtfile   boolean;
    w_directory varchar2(100);
    lnNot_open EXCEPTION; 
    v_concaten varchar2(1):=',';
begin   
    --w_directory := PKG_LEGALREPORT3.to_directory();
    w_file      := UTL_FILE.FOPEN('DIR_ASM', p_codfile||'.txt', 'w',32760);
    w_obtfile   := UTL_FILE.IS_OPEN(w_file);
    IF w_obtfile = TRUE THEN  
   		w_line := null;
		for y in c loop
        w_columna := PKG_LEGALREPORT3.to_format(y.empresa,'C',20,'B',null);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		     
		w_columna := PKG_LEGALREPORT3.to_format(y.CODFILE,'C',11,'B',null);        
        w_line    := w_line || w_columna;                                        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		             
		w_columna := PKG_LEGALREPORT3.to_format(y.FECHAENV,'C',10,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.NUMRECORDS,'C',12,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.STATUS,'C',5,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.STRECORD,'C',12,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
		end loop;  
        --UTL_FILE.PUT(w_file, w_line);
        UTL_FILE.NEW_LINE(w_file, 1);		     
        w_line:=null;
		for x in p loop 
		w_line:=null;
        dbms_output.put_line('Paso 1');
        w_columna := PKG_LEGALREPORT3.to_format(to_char(x.FECHACORTE,'ddmmyyyy'),'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);  
        dbms_output.put_line('Paso 2');        
        w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOCLIENTE,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
        dbms_output.put_line('Paso 3');
                               
        w_columna := PKG_LEGALREPORT3.to_format(x.NUMEROCUENTA,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
                                    
        dbms_output.put_line('Paso 4');                
        w_columna:= pkg_legalreport3.obt_formatvalue('C', x.CUENTACONTABLECAP,'XXX.XX.X.XX.XX.XX.XX.XX.XX.XX.XX.XX');                                          
		w_columna := PKG_LEGALREPORT3.to_format(w_columna,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);                           
                                    
        dbms_output.put_line('Paso 5');         
           w_columna := PKG_LEGALREPORT3.to_format(x.NOMBRECLIENTE,'C',60,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                    
        dbms_output.put_line('Paso 6');                
           w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOCATEGORIA,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                    
        dbms_output.put_line('Paso 7');                
        w_columna := PKG_LEGALREPORT3.to_format((x.FECHAAPERTURA),'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);  
                                    
        dbms_output.put_line('Paso 8');        
           w_columna := PKG_LEGALREPORT3.to_format(x.MONEDA,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 9');
           w_columna := PKG_LEGALREPORT3.to_format(x.TASAINTERES,'C',7,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 10');

           w_columna := PKG_LEGALREPORT3.to_format(x.FRECUENCIAREPRE,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 11');
                        
           w_columna := PKG_LEGALREPORT3.to_format(x.SALDOFINAL,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 12');                
           w_columna := PKG_LEGALREPORT3.to_format(x.RENDIMIENTOS,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 12');                
           w_columna := PKG_LEGALREPORT3.to_format(x.SECTORECONOMICO,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 13');
           w_columna := PKG_LEGALREPORT3.to_format(x.SUBPOBLACION,'C',40,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 14');
           w_columna := PKG_LEGALREPORT3.to_format(x.PRODUCTO,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                            
        dbms_output.put_line('Paso 15');                
           w_columna := PKG_LEGALREPORT3.to_format(x.TIPOSUJETO,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 16');                
           w_columna := PKG_LEGALREPORT3.to_format(x.TIPOVINCULACION,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 17');                
           w_columna := PKG_LEGALREPORT3.to_format(x.ESTATUSCUENTA,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 18');                
           w_columna := PKG_LEGALREPORT3.to_format(x.LOCALIDAD,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          
                                     
        dbms_output.put_line('Paso 19');                
           w_columna := PKG_LEGALREPORT3.to_format(x.TIPOCLIENTE,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                          

        dbms_output.put_line('Paso 20');                
        w_columna:= pkg_legalreport3.obt_formatvalue('C', x.CUENTACONTABLEINT,'XXX.XX.X.XX.XX.XX.XX.XX.XX.XX.XX.XX');                                          
		w_columna := PKG_LEGALREPORT3.to_format(w_columna,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);                           
                
        dbms_output.put_line('Paso 21');        
        w_columna := PKG_LEGALREPORT3.to_format(x.TIPODEPOSITANTE,'C',1,'B',NULL);        
        w_line    := w_line || w_columna;      
               UTL_FILE.PUT(w_file,w_columna);                   
               
        dbms_output.put_line('Paso 22');
        --w_columna := PKG_LEGALREPORT3.to_format(x.NOMBRECODEUDOR,'C',2,'B',v_concaten);        
        --w_line    := w_line || w_columna;      
          --      UTL_FILE.PUT(w_file,w_columna);                   
                
        UTL_FILE.NEW_LINE(w_file, 1);		             
		end loop;   
     ELSE
      RAISE lnNot_open;
    END IF;
    UTL_FILE.FCLOSE(w_file);
  EXCEPTION
    WHEN lnNot_open THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 1: lnNot_open');
    WHEN NO_DATA_FOUND THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 2: no data found');
    WHEN value_error THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 3: value error');
    WHEN UTL_FILE.INVALID_FILEHANDLE THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 4: invalid file handle');
    WHEN UTL_FILE.INVALID_OPERATION THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 5: invalid operation');
    WHEN UTL_FILE.READ_ERROR THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 6: file read');
    WHEN UTL_FILE.WRITE_ERROR THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 7: write error');
    WHEN UTL_FILE.INVALID_PATH THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900,
                              'ERROR 8: invalid path' || ' ' || w_directory);
    WHEN UTL_FILE.INVALID_MODE THEN
      UTL_FILE.FCLOSE(w_file);
      RAISE_APPLICATION_ERROR(-20900, 'ERROR 9: invalid mode');		
end;      

procedure exec_captliq(
p_operador in number,
p_FechaCorte in Date,
p_codfile in varchar2) is
begin
 pkg_dom_riesgoplan.rsk_captliq(p_operador => p_operador,
                                 p_dml => 'D',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile);
                                 
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'D',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
commit;                                   
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'I',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
                                   
 pkg_dom_riesgoplan.rsk_captliq(p_operador => p_operador,
                                 p_dml => 'I',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile);

commit;                                   
end; 

------------------------------------------------------------------codeudor
procedure exec_INTCODEUDOR(
p_operador in number,
p_FechaCorte in Date,
p_codfile in varchar2,
p_tiporel  number) is
begin
 pkg_dom_riesgoplan.rsk_INTCODEUDOR(p_operador => p_operador,
                                 p_dml => 'D',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile,
                                 p_tiporel=> p_tiporel);
                                 
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'D',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
commit;                                   
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'I',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
                                   
 pkg_dom_riesgoplan.rsk_INTCODEUDOR(p_operador => p_operador,
                                 p_dml => 'I',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile,
                                 p_tiporel=> p_tiporel);

commit;                                   
end; 
  
------------------------------------------------------------------exec garantias -----------------------------------------------------------------------
procedure exec_INTGARANTIA(
p_operador in number,
p_FechaCorte in Date,
p_codfile in varchar2,
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
                    P_TIPOCREDITO3 NUMBER) is
begin
 pkg_dom_riesgoplan.rsk_INTGARANTIA(p_operador => p_operador,
                                 p_dml => 'D',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile,
				                    p_tiporel=>p_tiporel,
				                    p_tiporel1=>p_tiporel1,
				                    p_tiporel2=>p_tiporel2,
				                    p_tiporel3=>p_tiporel3,
				                    p_tiporel4=>p_tiporel4,
				                    p_tipogarid1=>p_tipogarid1,
				                    p_tipogarid2=>p_tipogarid2,
				                    p_tipogarid3=>p_tipogarid3,
				                    p_decimales=>p_decimales,
				                    p_montocompara=>p_montocompara,
				                    P_TIPOCREDITO1=>P_TIPOCREDITO1,
				                    p_tipocredito2=>p_tipocredito2,
				                    P_TIPOCREDITO3=>P_TIPOCREDITO3);
                                 
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'D',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
commit;                                   
       pkg_dom_riesgoplan.risk_cabecera(p_operador => p_operador,
                                   p_dml => 'I',
                                   p_fechacorte => p_FechaCorte,
                                   p_codfile => p_codfile,
                                   p_fechaenn => 00000000,
                                   p_numrecord => 0,
                                   p_status => 0,
                                   p_sterecord => 0); 
                                   
 pkg_dom_riesgoplan.rsk_INTGARANTIA(p_operador => p_operador,
                                 p_dml => 'I',
                                 p_fechacorte => p_fechacorte,
                                 p_codfile => p_codfile,
				                    p_tiporel=>p_tiporel,
				                    p_tiporel1=>p_tiporel1,
				                    p_tiporel2=>p_tiporel2,
				                    p_tiporel3=>p_tiporel3,
				                    p_tiporel4=>p_tiporel4,
				                    p_tipogarid1=>p_tipogarid1,
				                    p_tipogarid2=>p_tipogarid2,
				                    p_tipogarid3=>p_tipogarid3,
				                    p_decimales=>p_decimales,
				                    p_montocompara=>p_montocompara,
				                    P_TIPOCREDITO1=>P_TIPOCREDITO1,
				                    p_tipocredito2=>p_tipocredito2,
				                    P_TIPOCREDITO3=>P_TIPOCREDITO3);

commit;                                   
                    
end;
--file
procedure file_credact(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2) is
cursor c is
select * 
from rsk_cabecera
 where FECHA=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;

cursor p is
select *
  from rsk_credact
 where FECHACORTE=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;
           
    w_line      varchar2(8000);
    w_columna   varchar2(4000);
    w_file      UTL_FILE.FILE_TYPE;
    w_obtfile   boolean;
    w_directory varchar2(100);
    lnNot_open EXCEPTION; 
    v_concaten varchar2(1):=',';
begin   
    --w_directory := PKG_LEGALREPORT3.to_directory();
    w_file      := UTL_FILE.FOPEN('DIR_ASM', p_codfile||'.txt', 'w',32760);
    w_obtfile   := UTL_FILE.IS_OPEN(w_file);
    IF w_obtfile = TRUE THEN  
   		w_line := null;
		for y in c loop
        w_columna := PKG_LEGALREPORT3.to_format(y.empresa,'C',20,'B',null);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		     
		w_columna := PKG_LEGALREPORT3.to_format(y.CODFILE,'C',11,'B',null);        
        w_line    := w_line || w_columna;                                        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		             
		w_columna := PKG_LEGALREPORT3.to_format(y.FECHAENV,'C',10,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.NUMRECORDS,'C',12,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(lpad(y.STATUS,'0',5),'C',5,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(lpad(y.STRECORD,'0',12),'C',12,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
		end loop;  
        --UTL_FILE.PUT(w_file, w_line);
        UTL_FILE.NEW_LINE(w_file, 1);		     
        w_line:=null;
		for x in p loop 
		w_line:=null;
        w_columna := PKG_LEGALREPORT3.to_format(to_char(x.FECHACORTE,'ddmmyyyy'),'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);                         
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHAAPROB,'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;                         
        UTL_FILE.PUT(w_file, w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHADESEMBOLSO,'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;                         
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHAPRIMERPAGO,'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;                         
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHAVENCIMIENTO,'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;                         
                UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHARESTRUCTURA,'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHARENOV,'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHACANCELADO,'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        w_columna := PKG_LEGALREPORT3.to_format(x.PLAZOORIGINAL,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        w_columna := PKG_LEGALREPORT3.to_format(x.PLAZOAJUSTADO,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        w_columna := PKG_LEGALREPORT3.to_format(x.CODIGODEUDOR,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        w_columna := PKG_LEGALREPORT3.to_format(x.TIPODEUDOR,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        w_columna := PKG_LEGALREPORT3.to_format(x.NUMEROOPER,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOREVOL,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna); 
        w_columna:= pkg_legalreport3.obt_formatvalue('C', x.CUENTACONTABLE,'XXX.XX.X.XX.XX.XX.XX.XX.XX.XX.XX.XX');                                          
		w_columna := PKG_LEGALREPORT3.to_format(w_columna,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                           
		w_columna := PKG_LEGALREPORT3.to_format(x.TIPOGARANTIA,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                           
		w_columna := PKG_LEGALREPORT3.to_format(x.GARANTIAADMISIBLE,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                           
		w_columna := PKG_LEGALREPORT3.to_format(x.NOMBRECLIENTE,'C',60,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                           
		w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOCRITERIO,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                           
		w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOCATEGORIA,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;  
                UTL_FILE.PUT(w_file,w_columna);                               
		w_columna := PKG_LEGALREPORT3.to_format(x.DIASMORA,'C',5,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.MONEDA,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.TASAINTERES,'C',7,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.TASAMORA,'C',7,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.FREREPRECIOTASA,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.MONTOVIGENTE,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.MONTOAPROBADO,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.SALDOINSOLUTO,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.CUOTA,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.PERIODOCAPITAL,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PERIODOINTERES,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PRINCIPALMOROSO,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.INTERESMOROSO,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.SECTORECOSUJETO,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;        
                UTL_FILE.PUT(w_file,w_columna);                                 
		w_columna := PKG_LEGALREPORT3.to_format(x.SECTORECOCREDITO,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;       
                UTL_FILE.PUT(w_file,w_columna);                                  
		w_columna := PKG_LEGALREPORT3.to_format(x.TIPOCREDITO,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;  
                UTL_FILE.PUT(w_file,w_columna);                                       
		w_columna := PKG_LEGALREPORT3.to_format(x.CREDITSCORINGINI,'C',5,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.IRBGENERAL,'C',5,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.IRBMONTOEXPUESTO,'C',5,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.IRBMONTOCUBIERTO,'C',5,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.IRBRIESGOPAIS,'C',5,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.MONTOCOBERTURAFIN,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.ESTADOOPERACION,'C',2,'0',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PROVREQCAPITAL,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOPAIS,'C',4,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.LOCALIDAD,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PRODUCTO,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.ORIGENREC,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.OFICINASUCURSAL,'C',5,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.SUBPOBLACIoN,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.CUOTASEGURO,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.CUOTACOMISION,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.APELLIDOSSIGLAS,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PERIODOGRACIA,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.TIPOVINCULACION,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.FECHAINICIOADJU,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.IDENTBENECONTING,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.NOMBREBENEFICIARIO,'C',60,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.OPCIONPAGOCANCE,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PENALPAGOANTICIPA,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PROVIREQGRADUALCAP,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PROVICAPCONST,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PROVIREQRENDI,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PROVIREQGRARENDI,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PROVIREQCONTINGE,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.PROVIREQGRADCONTINGE,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.FECHAREVITASA,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.FREREPRECIO,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.FECHAPAGOCUOEXTRA,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.MONTOPAGOCUOEXTRA,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.BASETASAINTERES,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.REESTRUCTURADO,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;    
                UTL_FILE.PUT(w_file,w_columna);                                     
		w_columna := PKG_LEGALREPORT3.to_format(to_char(x.RENDIXCOBRAR,'99999999999999.99'),'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.DEUDORESBENEFI,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.TIPOCLIENTE,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.FACILIDADCREDIT,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna); 
        w_columna:= pkg_legalreport3.obt_formatvalue('C',x.CTACONTABLECONTIG,'XXX.XX.X.XX.XX.XX.XX.XX.XX.XX.XX.XX');                                                                                            
		w_columna := PKG_LEGALREPORT3.to_format(w_columna,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;                                         
                UTL_FILE.PUT(w_file,w_columna);
		w_columna := PKG_LEGALREPORT3.to_format(x.SALDOCONTING,'C',17,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                                   
		w_columna := PKG_LEGALREPORT3.to_format(x.ORIGENCREDITO,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;
                  UTL_FILE.PUT(w_file,w_columna);        

        --abril 2018
		w_columna := PKG_LEGALREPORT3.to_format(x.FECHAREFINANCIACION,'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;
                  UTL_FILE.PUT(w_file,w_columna);                          

		w_columna := PKG_LEGALREPORT3.to_format(x.RAZONRIEGODEUDOR,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;
                  UTL_FILE.PUT(w_file,w_columna);                          
                  
		w_columna := PKG_LEGALREPORT3.to_format(x.FECHAINICOBRAJUDI,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;
                  UTL_FILE.PUT(w_file,w_columna);                          

		w_columna := PKG_LEGALREPORT3.to_format(x.TIPOCANCELACION,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;
                  UTL_FILE.PUT(w_file,w_columna);                          

		w_columna := PKG_LEGALREPORT3.to_format(x.TIPOFLEXIBINORMA,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;
                  UTL_FILE.PUT(w_file,w_columna);                          

		w_columna := PKG_LEGALREPORT3.to_format(x.DIASMORACAP,'C',5,'B',v_concaten);        
        w_line    := w_line || w_columna;
                  UTL_FILE.PUT(w_file,w_columna);                          

		w_columna := PKG_LEGALREPORT3.to_format(x.DIASMORAINT,'C',5,'B',null);        
        w_line    := w_line || w_columna;
                  UTL_FILE.PUT(w_file,w_columna);                          

                  
        --UTL_FILE.PUT(w_file, w_line);
        UTL_FILE.NEW_LINE(w_file, 1);		             
		end loop;   
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
end;
------------------------------------------------------------------------------file codeudores----------------------------------------------------------------------
procedure file_INTCODEUDOR(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2) is
cursor c is
select * 
from rsk_cabecera
 where FECHA=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;

cursor p is
select *
  from rsk_INTCODEUDOR
 where FECHACORTE=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;
           
    w_line      varchar2(8000);
    w_columna   varchar2(4000);
    w_file      UTL_FILE.FILE_TYPE;
    w_obtfile   boolean;
    w_directory varchar2(100);
    lnNot_open EXCEPTION; 
    v_concaten varchar2(1):=',';
begin   
    --w_directory := PKG_LEGALREPORT3.to_directory();
    w_file      := UTL_FILE.FOPEN('DIR_ASM', p_codfile||'.txt', 'w',32760);
    w_obtfile   := UTL_FILE.IS_OPEN(w_file);
    IF w_obtfile = TRUE THEN  
   		w_line := null;
		for y in c loop
        w_columna := PKG_LEGALREPORT3.to_format(y.empresa,'C',20,'B',null);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		     
		w_columna := PKG_LEGALREPORT3.to_format(y.CODFILE,'C',11,'B',null);        
        w_line    := w_line || w_columna;                                        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		             
		w_columna := PKG_LEGALREPORT3.to_format(y.FECHAENV,'C',10,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.NUMRECORDS,'C',12,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.STATUS,'C',5,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.STRECORD,'C',12,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
		end loop;  
        --UTL_FILE.PUT(w_file, w_line);
        UTL_FILE.NEW_LINE(w_file, 1);		     
        w_line:=null;
		for x in p loop 
		w_line:=null;
        w_columna := PKG_LEGALREPORT3.to_format(to_char(x.FECHACORTE,'ddmmyyyy'),'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);                         
        w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOCREDITO,'C',27,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
        
        w_columna := PKG_LEGALREPORT3.to_format(x.IDENTIFICADEUDOR,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.TIPODEUDOR,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        
        --w_columna := PKG_LEGALREPORT3.to_format(x.NOMBRECODEUDOR,'C',2,'B',v_concaten);        
        --w_line    := w_line || w_columna;      
          --      UTL_FILE.PUT(w_file,w_columna);                   
        
        w_columna := PKG_LEGALREPORT3.to_format(x.NOMBRECODEUDOR,'C',60,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        w_columna := PKG_LEGALREPORT3.to_format(x.APELLIDOSSIGLAS,'C',60,'B',v_concaten);        
        w_line    := w_line || w_columna;      
                UTL_FILE.PUT(w_file,w_columna);                   
        w_columna := PKG_LEGALREPORT3.to_format(x.TIPOCREDITO,'C',1,'B',null);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);                   
        UTL_FILE.NEW_LINE(w_file, 1);		             
		end loop;   
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
end;
----------------------------------------------------------------------------garantias de03------------------------------------------------------------------------
procedure file_INTGARANTIA(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2) is
cursor c is
select * 
from rsk_cabecera
 where FECHA=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;

cursor p is
select *
  from rsk_INTGARANTIA
 where FECHACORTE=p_fechaCorte
   and EMPRESA=p_ruc        
   and CODFILE = p_codfile;
           
    w_line      varchar2(8000);
    w_columna   varchar2(4000);
    w_file      UTL_FILE.FILE_TYPE;
    w_obtfile   boolean;
    w_directory varchar2(100);
    lnNot_open EXCEPTION; 
    v_concaten varchar2(1):=',';
begin   
    --w_directory := PKG_LEGALREPORT3.to_directory();
    w_file      := UTL_FILE.FOPEN('DIR_ASM', p_codfile||'.txt', 'w',32760);
    w_obtfile   := UTL_FILE.IS_OPEN(w_file);
    IF w_obtfile = TRUE THEN  
   		w_line := null;
		for y in c loop
        w_columna := PKG_LEGALREPORT3.to_format(y.empresa,'C',20,'B',null);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		     
		w_columna := PKG_LEGALREPORT3.to_format(y.CODFILE,'C',11,'B',null);        
        w_line    := w_line || w_columna;                                        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		             
		w_columna := PKG_LEGALREPORT3.to_format(y.FECHAENV,'C',10,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.NUMRECORDS,'C',12,'B',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.STATUS,'C',5,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
        UTL_FILE.NEW_LINE(w_file, 1);		                                                     
		w_columna := PKG_LEGALREPORT3.to_format(y.STRECORD,'C',12,'0',null);        
        w_line    := w_line || w_columna;        
        UTL_FILE.PUT(w_file, w_columna);
		end loop;  
        --UTL_FILE.PUT(w_file, w_line);
        UTL_FILE.NEW_LINE(w_file, 1);		     
        w_line:=null;
		for x in p loop 
		w_line:=null;
        w_columna := PKG_LEGALREPORT3.to_format(to_char(x.FECHACORTE,'ddmmyyyy'),'C',8,'B',v_concaten);        
        w_line    := w_line || w_columna;
        UTL_FILE.PUT(w_file, w_columna);                         
        w_columna := PKG_LEGALREPORT3.to_format(x.CODIGOCREDITO,'C',27,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.IDENTIFICAGARANTIA,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.TIPOGARANTESOLIDARIO,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.TIPOGARANTIA,'C',2,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.DESCRIPGARANTIA,'C',250,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHACONSTIT,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHAFORMA,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHATASACION,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.VALORTASACION,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.RANGOGARANTIA,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.TIPOGARANTIAVAL,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.IDENTENTEMISORA,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.GARASEGURADA,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHAVENPOLIZA,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.NOMBREGARANTE,'C',60,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.APELLIDOGARAN,'C',30,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.TIPOCREDITO,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.PORCTASACIONGAR,'C',6,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.GARANTIAADMI,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.ANIOFABGARANTIA,'C',4,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.MONTOFORMAGAR,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.IDENTTASADOR,'C',13,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECHAVENCGAR,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.NUMEROPOLIZASEG,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.FECEMIPOLSEG,'C',10,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.IDENTCOMASEGURADORA,'C',13,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.VALORENDOSO,'C',15,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.GARANFIDUCIARIA,'C',1,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.CLASIFFIDUCIARIA,'C',3,'B',v_concaten);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);
        w_columna := PKG_LEGALREPORT3.to_format(x.DESCRIPFIDE,'C',250,'B',null);        
        w_line    := w_line || w_columna;      
        UTL_FILE.PUT(w_file,w_columna);

        UTL_FILE.NEW_LINE(w_file, 1);		             
		end loop;   
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
end;

-------------------------------------------------------------------------------Cabecera---------------------------------------------------------------------------- 
--file
  --Modificado el 2004/07/23         
  --Para llenar la cabecera
	procedure risk_cabecera( 
	p_operador in number,
	p_dml in varchar2,
	p_FechaCorte in Date,
	p_codfile in varchar2,
	p_fechaenn in varchar2,
	p_numrecord in varchar2,
	p_status in varchar2,
	p_sterecord in varchar2
	) is         
	v_ruc varchar2(19);
	begin
	select INS_RUC into v_ruc from tgen_instit; 
	if p_dml = 'D' then
	     delete rsk_cabecera
	      where 
	       FECHA = p_FechaCorte
	       and EMPRESA = v_ruc 
	       and CODFILE = p_codfile;
	end if;
	if p_dml = 'I' then
	     insert into rsk_cabecera(FECHA,EMPRESA,CODFILE,FECHAENV,NUMRECORDS,STATUS,STRECORD)
	     values(p_FechaCorte,v_ruc,p_codfile,p_fechaenn,p_numrecord,p_status,p_sterecord);
	end if;
	if p_dml = 'U' then
	     update rsk_cabecera
	       set FECHAENV=p_fechaenn,
	       NUMRECORDS=p_numrecord,
	       STATUS=p_status,
	       STRECORD=p_sterecord
	      where 
	       FECHA = p_FechaCorte
	       and EMPRESA = v_ruc 
	       and CODFILE = p_codfile;
	end if;	
    end;
function Tasamora( modulo in number, 
                   producto in number,
                   tipo in number, 
                   moneda in number,
                   fecha in date) return number is
 v_tasa number;                   
begin
  select tam_puntos 
    into v_tasa 
  from  TGEN_TASAMORA
  where tam_mod = modulo
    and tam_pro = producto
    and tam_tip = tipo
    and tam_moneda = moneda
    and trunc(fecha) between tam_fechvigen and nvl(tam_fechasta,to_date('2199/12/31','yyyy/mm/dd'));
    return v_tasa;
exception 
  when others then     
     return v_tasa;
end;                   
     
function CuentaCon(credito in number,
					modulo in number, 
                    producto in number,
                    tipo in number, 
                    moneda in number,
                    codgru in number,
                    p_fecha in date) return varchar2 is
v_cuenta varchar2(20);
vlv_cuenta varchar2(30);
begin                
  begin
      select gdt_cuenta
        into v_cuenta
        from tpre_funcgroupdet, tpre_funcgroup
       where GDT_MOD = modulo 
         and GDT_PRO = producto 
         and GDT_TIP = tipo 
         and GDT_MON = moneda 
         and fng_modrpt = 20 
         and fng_trarpt = 450 
         and fng_entrpt = 0 
         and fng_secrpt = 1 
         and gdt_mod = 6 
         and fng_cuenta = gdt_cuenta 
         and GDT_MODRPT = fng_modrpt
         and GDT_TRARPT = fng_trarpt 
         and fng_secrpt = gdt_secrpt
         and GDT_CODGRU = codgru;
  exception
    when others then
       v_cuenta:=null;
  end;
  vlv_cuenta := pkg_legal_dom_de.fun_gen_cuenta(credito,
                               v_cuenta,
                                         4,
                                         6,
                                         '2045001',
                                         p_fecha);
     return vlv_cuenta;
end;   

function TIP_GARANTIA(tabgar   IN NUMBER,
	                  gar   IN NUMBER) return varchar2 is
tipo_gar varchar2(2);	                  
begin                        
BEGIN
      SELECT substr(codeiso, 1, 2)
        INTO tipo_gar
        FROM tgen_desctabla
       WHERE des_codtab = nvl(tabgar,33) 
         and des_codigo = gar;
         
    EXCEPTION
      WHEN no_data_found THEN
        tipo_gar := NULL;
    END;	         
    return tipo_gar;
end; 
function Val_Relacion_bco(p_codigocli in number) return VARCHAR2 is
relacion_banco varchar2(8);
begin        
begin
          select CODEISO
            into relacion_banco
            from TGEN_DESCTABLA, TCLI_PERSONA
           where DES_CODTAB = cli_tabrel 
             AND DES_CODIGO = cli_relacionban 
             AND cli_codigo = p_codigocli;
        EXCEPTION
          when others then
            relacion_banco := null;
        End;
        return relacion_banco;
end;    
function Val_rowid(p_report    IN varchar2,
                      p_sessionid   IN NUMBER,
                      p_columnid in number,
                      p_credito in number) return NUMBER is
v_row number;
begin
select rowsid
into v_row
from tvaluelevelcustom
where REPORT=p_report
  AND SESSIONID = p_sessionid
  AND columsid = p_columnid--6
  and valuechar = p_credito;
  return v_row;
exception
  when others then
     return 0;             
end;
function Val_cuentacon( p_mod in number,
						p_operacion in number,
                        p_tra in number,
                        p_rubro in number) return VARCHAR2 is
 v_cta tgen_cuenta%rowtype;
 v_con varchar2(20);
begin                                                        
      select *
         into v_cta 
       from tgen_cuenta
       where pro_mod = p_mod
         and pro_cue = p_operacion;
         
       select con_ctadeb 
         into v_con
       from tgen_trancont
       where con_mod = v_cta.pro_mod
        and con_pro = v_cta.pro_pro 
        and con_tip = v_cta.pro_tip
        and con_mon = v_cta.pro_mon
        and con_fechahasta is null
        and con_tra =p_tra
        and con_rubro = p_rubro;
        return v_con;
exception 
  when others then
       return null;       
end; 

function Val_desctabla_n(p_codtab in number,
					   p_codeiso in varchar2) return NUMBER is
 v_val number;					   
begin
  select des_codigo
     into v_val 
   from tgen_desctabla 
   where des_codtab = p_codtab
     and codeiso = p_codeiso;
     return v_val;
exception 
  when others then
   return 0;       
end;   
function Val_desctabla_c(p_codtab in number,
					   p_codeiso in varchar2) return VARCHAR2 is
 v_val varchar2(100);					   
begin
  select des_descripcion
     into v_val 
   from tgen_desctabla 
   where des_codtab = p_codtab
     and codeiso = p_codeiso;
     return v_val;
exception 
  when others then
   return 0;       
end;   
function Val_cuota(p_credito in number,
                   p_moneda in number,
                   p_fecha in date,
                   p_tipocuota in number) return NUMBER is
fecha_primer_pago date;
 valor_primer_pago number;
 w_diasper number;
 numero_cuotas number; 
 total number;
 primera_cuota number;
begin
          If p_tipocuota in (1,3,5,7) then
            BEGIN

              select cuo_fecha,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       cuo_valor,
                                                       p_moneda,
                                                       1),
                           2),
                     cuo_dias
                into fecha_primer_pago, valor_primer_pago,w_diasper
                from tpre_cuotas
               where cuo_credito = p_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                fecha_primer_pago := null;
                valor_primer_pago := null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        p_credito || ' ' || sqlerrm);
            END;
          Elsif p_tipocuota = 6 then
            BEGIN

              select let_fecven,
                     round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                       let_valor,
                                                       p_moneda,
                                                       1),
                           2)
                into fecha_primer_pago, valor_primer_pago
                from tpre_letras
               where let_credito = p_credito and let_sec = 1;

            EXCEPTION
              when no_data_found then
                fecha_primer_pago := null;
                valor_primer_pago := null;
              when others then
                raise_application_error(-20201,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        p_credito || ' ' || sqlerrm);
            END;
            --CUOTA FIJA DE CAPITAL
          Elsif p_tipocuota = 8 then
            BEGIN
              Select count(*), sum(cuo_valor)--cuo_capital
                Into numero_cuotas, total
                From tpre_cuotas
               Where cuo_credito = p_credito;

            EXCEPTION
              when no_data_found then
                numero_cuotas := 0;
                total         := 0;
              When others then
                raise_application_error(-20203,
                                        'Error al obtener el capital del crédito ' ||
                                        p_credito || ' ' || sqlerrm);
            END;
            BEGIN
              Select cuo_valor,cuo_dias --cuo_capital
                Into primera_cuota,w_diasper
                From tpre_cuotas
               Where cuo_credito = p_credito and cuo_numcuo = 1;

            EXCEPTION
              when no_data_found then
                primera_cuota := null;
              when others then
                raise_application_error(-20204,
                                        'Error al obtener cuota 1 del crédito ' ||
                                        p_credito || ' ' || sqlerrm);
            END;
            If numero_cuotas != 0 then
              If total / numero_cuotas = primera_cuota then

                BEGIN
                  Select cuo_fecha,
                         round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                           cuo_valor, --cuo_capital,
                                                           p_moneda,
                                                           1),
                               2)
                    Into fecha_primer_pago, valor_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = p_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                    valor_primer_pago := null;
                  when others then
                    raise_application_error(-20205,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            p_credito || ' ' || sqlerrm);
                END;
              Else
                BEGIN
                  Select cuo_fecha
                    Into fecha_primer_pago
                    From tpre_cuotas
                   Where cuo_credito = p_credito and cuo_numcuo = 1;
                EXCEPTION
                  when no_data_found then
                    fecha_primer_pago := null;
                  when others then
                    raise_application_error(-20206,
                                            'Error al obtener fecha de la cuota 1 del crédito ' ||
                                            p_credito || ' ' || sqlerrm);
                END;
                begin
                  -- HD4324_2 (SE HIZO UN SUM PORQUE PUEDE HABER UN ARREGLO DE PAGOS Y TENDREMOS DOS VALORES PARA CUO_CAPITAL)
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital,
                                                               p_moneda,
                                                               1),
                                   2)
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_fecha =
                         (Select max(cuo_fecha)
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = p_credito) and
                         cuo_credito = p_credito and
                         cuo_numcuo = (Select max(cuo_numcuo) - 1
                            From tpre_cuotas
                           Where cuo_fecha <= p_fecha and
                                 cuo_credito = p_credito);
                Exception
                  when no_data_found then
                  Select round(pkg_legalreport3.cotiza_mon(p_fecha,
                                                               cuo_valor,--cuo_capital
                                                               p_moneda,
                                                               1),
                                   2) --HD4324_2
                    into valor_primer_pago
                    From tpre_cuotas
                   Where cuo_NUMCUO = 1 and
                         cuo_credito = p_credito;
                    -- insert into log_batch values (to_char(p_fecha,'yyyy/mm/dd')||' - '||to_char(i.Pre_credito)||' 1 ');
                  WHEN OTHERS THEN
                    raise_application_error(-20207,
                                            'Error al obtener cuota 1 del crédito ' ||
                                            p_credito || ' ' || sqlerrm);
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
          return  valor_primer_pago;                   
end;                   

function Nombre_prd(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number   ) return varchar2 is
p_nombre varchar2(30);
begin
    select substr(pso_nombre,1,30)
     into p_nombre
    from tsol_producto
  where  pso_mod = p_mod
    and pso_pro = p_pro
    and pso_tip = p_tip
    and pso_moneda = p_mon ;
  return p_nombre;
exception
   when others then
       return null;  					
end;       
function TIPOCREDITO( modulo in number, 
                   producto in number,
                   tipo in number, 
                   moneda in number) return VARCHAR2 IS
  V_CODEISO VARCHAR2(8);                   
BEGIN
    SELECT CODEISO
       INTO v_codeiso
     from TPRE_PROTIPOCREDITO,TGEN_DESCTABLA
     WHERE PTC_TABTIPOCRERISK = DES_CODTAB
       AND PTC_TIPOCRERISK = DES_CODIGO
       AND PTC_MOD = modulo
		AND PTC_PRO = producto
		AND PTC_TIP = tipo
		AND PTC_MON = moneda;
		return v_codeiso;
exception
   when others then
      return null;		
END;
function Val_campo_NUM(p_report    IN varchar2,
                      p_sessionid   IN NUMBER,
                      p_rowid in number,
                      p_columnid in number) return NUMBER is
v_valor number;
begin
select valuenum
into v_valor
from tvaluelevelcustom
where REPORT=p_report
  AND SESSIONID = p_sessionid
  AND rowsid = p_rowid
  and columsid = p_columnid;
 return  v_valor;
exception 
  when others then
    return 0;                       
end;
function Val_desctabla_iso(p_codtab in number,
					   p_codigo in number) return VARCHAR2 is
   v_val varchar2(8);					   
begin
  select codeiso
     into v_val 
   from tgen_desctabla 
   where des_codtab = p_codtab
     and des_codigo = p_codigo;
     return v_val;
exception 
  when others then
   return 0;       
end;					   
function Val_penalcance(p_fecha in date,
						p_tipocartera in varchar2,
                        p_moneda in number,
                        p_plazocance in number) return number is
v_penal number;
begin
  select rsk_penalidad
    into v_penal
    from rsk_tasaspenalidad
    where rsk_tipocredito=decode(p_tipocartera,'M','C','R','C',p_tipocartera)
      and rsk_moneda = p_moneda
      and p_plazocance>= rsk_antesplazo 
      and p_fecha between rks_desde and nvl(rls_hasta,to_date('2199/12/31','yyyy/mm/dd'));
      return v_penal;
exception 
  when others then
      return 0;      
end;
function Val_campo_char(p_report    IN varchar2,
                      p_sessionid   IN NUMBER,
                      p_rowid in number,
                      p_columnid in number) return NUMBER is
v_valor varchar2(1000);
begin
select valuechar
into v_valor
from tvaluelevelcustom
where REPORT=p_report
  AND SESSIONID = p_sessionid
  AND rowsid = p_rowid
  and columsid = p_columnid;
 return  v_valor;
exception 
  when others then
    return 0;                       
end;
          
procedure rsk_credact(
p_operador in number,
p_dml in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2) is
	v_ruc varchar2(19);
--variables tabla
v_FECHAAPROB                               VARCHAR2(8);
v_FECHADESEMBOLSO                          VARCHAR2(8);
v_FECHAPRIMERPAGO                          VARCHAR2(8);
v_FECHAVENCIMIENTO                         VARCHAR2(8);
v_FECHARESTRUCTURA                         VARCHAR2(8);
v_FECHARENOV                               VARCHAR2(8);
v_FECHACANCELADO                           VARCHAR2(8);
v_PLAZOORIGINAL                            NUMBER(6);
v_PLAZOAJUSTADO                            NUMBER(6);
v_CODIGODEUDOR                             VARCHAR2(30);
v_TIPODEUDOR                               VARCHAR2(2);
v_NUMEROOPER                      	     VARCHAR2(30);
v_CODIGOREVOL                              VARCHAR2(30);
v_CUENTACONTABLE                           VARCHAR2(30);
v_TIPOGARANTIA                             VARCHAR2(2) ;
v_GARANTIAADMISIBLE                        NUMBER(15,2);
v_NOMBRECLIENTE                            VARCHAR2(60);
v_CODIGOCRITERIO                           VARCHAR2(2) ;
v_CODIGOCATEGORIA                          VARCHAR2(2) ;
v_DIASMORA                                 NUMBER(5)   ;
v_MONEDA                                   NUMBER(2)   ;
v_TASAINTERES                              NUMBER(7,2) ;
v_TASAMORA                                 NUMBER(7,2) ;
v_FREREPRECIOTASA                          NUMBER(3)   ;
v_MONTOVIGENTE                             NUMBER(17,2);
v_MONTOAPROBADO                            NUMBER(17,2);
v_SALDOINSOLUTO                            NUMBER(17,2);
v_CUOTA                                    NUMBER(17,2);
v_PERIODOCAPITAL                           NUMBER(3)   ;
v_PERIODOINTERES                           NUMBER(3)   ;
v_PRINCIPALMOROSO                          NUMBER(17,2);
v_INTERESMOROSO                            NUMBER(17,2);
v_SECTORECOSUJETO                          NUMBER(6)   ;
v_SECTORECOCREDITO                         NUMBER(6)   ;
v_TIPOCREDITO                              VARCHAR2(3) ;
v_CREDITSCORINGINI                         VARCHAR2(5) ;
v_IRBGENERAL                               VARCHAR2(5) ;
v_IRBMONTOEXPUESTO                         VARCHAR2(5) ;
v_IRBMONTOCUBIERTO                         VARCHAR2(5) ;
v_IRBRIESGOPAIS                            VARCHAR2(5) ;
v_MONTOCOBERTURAFIN                        NUMBER(17,2);
v_ESTADOOPERACION                          NUMBER(2)   ;
v_PROVREQCAPITAL                           NUMBER(17,2);
v_CODIGOPAIS                               VARCHAR2(4) ;
v_LOCALIDAD                                VARCHAR2(6) ;
v_PRODUCTO                                 VARCHAR2(30);
v_ORIGENREC                                VARCHAR2(30);
v_OFICINASUCURSAL                          NUMBER(5)   ;
v_SUBPOBLACIÓN                             VARCHAR2(30);
v_CUOTASEGURO                              NUMBER(15,2);
v_CUOTACOMISION                            NUMBER(15,2);
v_APELLIDOSSIGLAS                          VARCHAR2(30);
v_PERIODOGRACIA                            NUMBER(2)   ;
v_TIPOVINCULACION                          VARCHAR2(2) ;
v_FECHAINICIOADJU                          VARCHAR2(10);
v_IDENTBENECONTING                         VARCHAR2(15);
v_NOMBREBENEFICIARIO                       VARCHAR2(60);
v_OPCIONPAGOCANCE                          VARCHAR2(2) ;
v_PENALPAGOANTICIPA                        NUMBER(6,2) ;
v_PROVIREQGRADUALCAP                       NUMBER(15,2);
v_PROVICAPCONST                            NUMBER(15,2);
v_PROVIREQRENDI                            NUMBER(15,2);
v_PROVIREQGRARENDI                         NUMBER(15,2);
v_PROVIREQCONTINGE                         NUMBER(15,2);
v_PROVIREQGRADCONTINGE                     NUMBER(15,2);
v_FECHAREVITASA                            VARCHAR2(10);
v_FREREPRECIO                              NUMBER(3)   ;
v_FECHAPAGOCUOEXTRA                        VARCHAR2(10);
v_MONTOPAGOCUOEXTRA                        NUMBER(15,2);
v_BASETASAINTERES                          NUMBER(1)   ;
v_REESTRUCTURADO                           VARCHAR2(2) ;
v_RENDIXCOBRAR                             NUMBER(17,2);
v_DEUDORESBENEFI                           VARCHAR2(1) ;
v_TIPOCLIENTE                              NUMBER(3)   ;
v_FACILIDADCREDIT                          NUMBER(3)   ;
v_CTACONTABLECONTIG                        VARCHAR2(30);
v_SALDOCONTING                             NUMBER(17,2) ;
v_ORIGENCREDITO                            VARCHAR2(2)  ;
--abril 2018
v_FECHAREFINANCIACION                      VARCHAR2(10);
v_RAZONRIEGODEUDOR                         VARCHAR2(1);
v_FECHAINICOBRAJUDI                        VARCHAR2(10);
v_TIPOCANCELACION                          VARCHAR2(1);
v_TIPOFLEXIBINORMA                         VARCHAR2(3);
w_tipocan varchar2(1);
v_DIASMORACAP                              NUMBER(5);
v_DIASMORAINT                              NUMBER(5);



v_tipocli number; 
v_PERIODOCAPITALaux varchar2(2);
v_PERIODOINTERESaux varchar2(2);
contador number:=0; 
V_MONTOPAGOCUOEXTRAaux number(15,2);
    r_operacioncalif tleg_operacioncalif%rowtype;
    r_accrual tpre_accrual%rowtype;    

--fin
cursor ca is	
	select *
	  from tleg_operacioncalif , tpre_prestamos,tpre_accrual 
	where lca_fecha = p_FechaCorte
	  and lca_cuenta = pre_credito
	  and lca_cuenta = acr_credito 
	  --and rownum < 6
	  and lca_fecha = acr_fecha;
	  --and pre_credito = 6010004392;
	  ---AND ROWNUM <= 20;              
cursor cancelados is
select * 
   from tpre_prestamos
    where pre_fecance is not null
    	  --and rownum < 4
      and pre_fecance between add_months(p_FechaCorte,-1)+1 and p_FechaCorte;
	  
	nombre       tcli_persona.cli_nombre%type;
    apellido     tcli_persona.cli_nombre%type;
    identifica   varchar2(20);
    tipocli      char(2);	
    v_numcomprom number;
    vln_monextra number;
    v_fechacuoextraaux date;
begin  
	select INS_RUC into v_ruc from tgen_instit; 

if p_dml = 'D' then
	     delete rsk_credact
	      where 
	       FECHACORTE = p_FechaCorte
	       and EMPRESA = v_ruc 
	       and CODFILE = p_codfile;
end if;          
if p_dml = 'I' then
   --PKG_LEGAL_DOM_DE_risk.DE11('DE11',p_operador,1,p_fechacorte,p_fechacorte,20,0,0,5000000,0) ;
   --PKG_LEGAL_DOM_DE_risk.DE13('DE13',p_operador,1,p_fechacorte,22,0,0,0);
   --PKG_LEGAL_DOM_DE_risk.DE15('DE15',p_operador,1,p_fechacorte,21,0,0,0);
   
  contador :=0;
   for x in ca loop    
   v_FECHAAPROB:= to_char(x.pre_fecemi,'ddmmyyyy');         
   v_FECHADESEMBOLSO:= to_char(x.pre_fecontab,'ddmmyyyy');
   v_FECHAPRIMERPAGO:= to_char(x.pre_fecemi + x.pre_diasinicio,'ddmmyyyy');
   v_FECHAVENCIMIENTO:=to_char(x.pre_fecven,'ddmmyyyy');   
   v_FECHARESTRUCTURA:=to_char(x.LCA_FECREESTRUC,'ddmmyyyy');   
   v_FECHARENOV:=to_char(x.LCA_FECRENOV,'ddmmyyyy');   
   v_FECHACANCELADO:=null;--to_char(x.LCA_FECRENOV,'ddmmyyyy');  
   V_PLAZOORIGINAL:=trunc((x.pre_plazoorg/30)); 
   v_PLAZOAJUSTADO:=trunc((x.pre_plazo/30));
   --inicio
        pkg_legalreport3.datos_cliente(x.pre_clientep,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido);   
   --fin
   v_CODIGODEUDOR:= identifica;--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),2);--identifica;            
   
   v_TIPODEUDOR:= tipocli;--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),3); --tipocli;

   v_NUMEROOPER:=x.pre_credito;
   v_CODIGOREVOL:= x.pre_numcomprom;
   v_CUENTACONTABLE:= CuentaCon(x.pre_credito,x.pre_mod,x.pre_pro,x.pre_tip,x.pre_moneda,21,p_fechaCorte);
   v_TIPOGARANTIA := TIP_GARANTIA(x.pre_codtipgar,x.pre_garantia); 
   v_GARANTIAADMISIBLE:=pkg_legal_Dom_de.fun_garantia(X.pre_credito,p_FECHACORTE,null);--x.LCA_MONTOGARADMI;
   v_NOMBRECLIENTE := substr(replace(apellido,',','') ||';'||replace(nombre,',',''),1,60);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),4);--nombre ||', '||apellido;
   select decode(x.LCA_CODTIPOCREDITO,'C','1A','M','1B','O','2A','H','3A','D','1D','1C')--abril 2018 
    into v_CODIGOCRITERIO
   from dual;
   v_CODIGOCATEGORIA:='A';                            
   v_DIASMORA:=nvl(x.LCA_DIASMORAINT,x.LCA_DIASMORACAP);
   select decode(x.pre_moneda,2,1,7,2,0) 
    into v_MONEDA
   from dual;
   v_TASAINTERES:= x.acr_tasa;
   v_TASAMORA:= NVL(Tasamora(x.pre_mod,x.pre_pro,x.pre_tip,x.pre_moneda,p_fechacorte),0);
   v_FREREPRECIOTASA:=12;   --consultar que va en el caso de los creditos que tienen tasa variable y los creditos que tiene tasa fija por 1 ,2 3,0 4 años
   v_MONTOVIGENTE:=round(pkg_legalreport3.cotiza_mon(p_fechaCorte,x.pre_valent,x.pre_moneda,x.pre_sucursal),2)  ;                                                                    
        IF x.pre_numcomprom is null then
          v_MONTOAPROBADO := pkg_legal_Dom_de.monto_aprobado(x.pre_credito,
                                                           p_fechacorte);
        ELSE
          v_MONTOAPROBADO := pkg_legal_Dom_de.monto_aprobado(x.pre_numcomprom,
                                                           p_fechacorte);
        END IF;
   
   --v_MONTOAPROBADO:=Val_campo_NUM('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),9);
   
   v_SALDOINSOLUTO:=saldo_capital_fecha(x.pre_credito,p_FechaCorte);


    v_CUOTA:=Val_cuota(x.pre_credito, x.pre_moneda,p_fechacorte,x.pre_tipocuo);--Val_campo_NUM('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),14);

     
        --PERIODOS(i.pre_tipocuo,i.pre_diaspercap,PERIODOCAP);
     
        --PERGRACIA(i.pre_credito, p_fecha, p_gracia);

      
   pkg_legal_dom_de.PERIODOS_CAP(x.pre_tipocuo, x.pre_diasper,v_PERIODOCAPITALaux);--Val_desctabla_n(701,Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),15));
   v_PERIODOCAPITAL:=Val_desctabla_n(704,replace(v_PERIODOCAPITALaux,' ',''));
   dbms_output.put_line('v_PERIODOCAPITAL:'||v_PERIODOCAPITAL);
   pkg_legal_dom_de.PERIODOS_INT(x.pre_tipocuo, x.pre_diasper, v_PERIODOINTERESaux);--w_diasper Val_desctabla_n(701,Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),16));
      v_PERIODOINTERES:=Val_desctabla_n(704,replace(v_PERIODOCAPITALaux,' ',''));   
   select decode(nvl(X.acr_otrostatus,'-'),'J',0,X.acr_capnogi) + decode(nvl(x.acr_otrostatus,'-'),'-',x.acr_capvencido,0)
     into v_PRINCIPALMOROSO
     from dual;
   select decode(nvl(x.acr_otrostatus,'-'),'J',0,x.acr_intactven) + decode(nvl(x.acr_otrostatus,'-'),'J',0,x.acr_intacteje)
     into V_INTERESMOROSO
     from dual;
   begin

    v_SECTORECOSUJETO:=pkg_legal_dom_de.fun_actividad_cliente(x.pre_clientep);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),31);
   exception 
   when others then 
     v_SECTORECOSUJETO:=null;
   end;
   v_SECTORECOCREDITO:= pkg_legal_dom_de.fun_destino_ciiu(0,x.pre_credito,v_numcomprom);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),32);
   IF v_SECTORECOCREDITO = '9500' THEN
      v_SECTORECOCREDITO := '9500'||'01';
   END IF;
   --V_TIPOCREDITO:= x.lca_codtipocredito;--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),51);   
   V_TIPOCREDITO:= TipoCredito(x.pre_mod,x.pre_pro,x.pre_tip,x.pre_moneda);
   --ABRIL 2018
   IF V_TIPOCREDITO IS NULL THEN
     select decode(x.LCA_CODTIPOCREDITO,'C','020','M','020','O','001','H','003','D','020','001')--abril 2018 
    into V_TIPOCREDITO
   from dual;
   END IF;
   
   V_CREDITSCORINGINI:='A';
   V_IRBGENERAL:='A';
   V_IRBMONTOEXPUESTO:='A';
   V_IRBMONTOCUBIERTO:='A';
   V_IRBRIESGOPAIS:='A';
   V_MONTOCOBERTURAFIN:=0;
   V_ESTADOOPERACION:='00';--PARA PRESTAMOS   --se envia con Cero
   V_PROVREQCAPITAL:=0;
   V_CODIGOPAIS:= PKG_LEGAL_DOM.CODIGO_PAIS(x.pre_clientep,x.pre_numdir);
        --Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),35);
   V_LOCALIDAD:=PKG_LEGAL_DOM_de.fun_localidad(x.pre_sucursal,x.pre_oficina,x.pre_clientep,x.pre_numdir);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),30);
   V_PRODUCTO:=  REPLACE(Nombre_prd(x.pre_mod,x.pre_pro,x.pre_tip,x.pre_moneda),',','');
   v_ORIGENREC:=PKG_LEGAL_DOM_de.RECURSO(x.pre_mod, x.pre_credito);--Val_desctabla_c(702,Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),25));
   v_OFICINASUCURSAL:=x.pre_sucursal;
   v_SUBPOBLACIÓN:=null;
   v_CUOTASEGURO:=0;
   v_CUOTACOMISION:=0;
   v_APELLIDOSSIGLAS:=substr(replace(apellido,',',''),1,30);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),5);    
   PKG_LEGAL_DOM_de.PERGRACIA(x.pre_credito, p_fechaCorte, v_PERIODOGRACIA);
   --v_PERIODOGRACIA:=;--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),17);
   V_TIPOVINCULACION:=Val_Relacion_bco(x.pre_clientep);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),26);
   V_FECHAINICIOADJU :=null;-- en el de11 esta en blanco Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),36);
   V_IDENTBENECONTING:=NULL;--EN EL DE11 Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),37);
   V_NOMBREBENEFICIARIO:= NULL;-- EN EL DE11 NULLVal_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),38);   
   V_OPCIONPAGOCANCE:='S';
   V_PENALPAGOANTICIPA:=Val_penalcance(p_fechaCorte,x.LCA_CODTIPOCREDITO,x.pre_moneda,trunc(x.acr_plazo/30));--
   v_PROVIREQGRADUALCAP:=0;
   v_PROVICAPCONST:=0;
   v_PROVIREQRENDI:=0;
   v_PROVIREQGRARENDI:=0;
   v_PROVIREQCONTINGE:=0;
   v_PROVIREQGRADCONTINGE:=0;
   v_FECHAREVITASA:=to_char(x.pre_plazotasa,'dd/mm/yyyy');
   v_FREREPRECIO:=12;--mensual
   --v_FECHAPAGOCUOEXTRA:= TO_CHAR(Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),45),'DDMMYYYY');   
   PKG_LEGAL_DOM_de.pro_gen_cuoextra(x.pre_credito,p_fechaCorte,V_MONTOPAGOCUOEXTRAaux,v_fechacuoextraaux);
   --V_MONTOPAGOCUOEXTRA:=Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),46); 
   v_FECHAPAGOCUOEXTRA:= to_char(v_fechacuoextraaux,'dd/mm/yyyy');
   V_MONTOPAGOCUOEXTRA:=V_MONTOPAGOCUOEXTRAaux;
   V_BASETASAINTERES:=0;--360
    V_REESTRUCTURADO:= NVL(x.LCA_ESTRUCTURACIONCRE,'NR');--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),56);
    select decode(nvl(x.acr_otrostatus,'-'),'J',0,x.acr_intactven) + decode(nvl(x.acr_otrostatus,'-'),'J',0,x.acr_intacteje)
      into V_RENDIXCOBRAR
      from dual;
    V_DEUDORESBENEFI:='N';               
    begin                               
     select nat_subtipocliente
       into v_tipocli
     from tcli_natural
     where nat_codcli = x.pre_clientep;
    exception
      when no_data_found then
      select jur_subtipocliente
        into v_tipocli
       from tcli_juridica 
       where jur_codcli = x.pre_clientep;
    end;                                      
    --21/9126
         begin  
        select decode(nvl(PTC_ENCAJE,'N'),'N','S'),CODEISO
        into V_DEUDORESBENEFI,v_TIPOFLEXIBINORMA
        from TPRE_PROTIPOCREDITO,tgen_desctabla 
       where PTC_MOD=x.pre_mod
        and PTC_PRO=x.pre_pro
        and PTC_TIP=x.pre_tip
        and PTC_MON= x.pre_moneda
        and nvl(PTC_TABFLEXIBLE,701) = des_codtab(+) 
        and PTC_CODFLEXIBLE = des_codigo(+);
     exception
       when no_data_found then
            V_DEUDORESBENEFI:='N';
            v_TIPOFLEXIBINORMA:=null;               
    end;              

    V_TIPOCLIENTE:=Val_desctabla_iso(305,v_tipocli); 
    v_FACILIDADCREDIT:=PKG_LEGAL_DOM_de.fun_producto_servicio(x.pre_credito,6);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),48);
    IF v_FACILIDADCREDIT IS NULL THEN
    IF  X.LCA_CODTIPOCREDITO IN ('C','D','M') THEN
        v_FACILIDADCREDIT:='112';
    ELSIF X.LCA_CODTIPOCREDITO = 'O' THEN
        v_FACILIDADCREDIT:='161';    
    ELSIF X.LCA_CODTIPOCREDITO = 'H' THEN
        v_FACILIDADCREDIT:='181';            
    END IF;
    END IF;    
    
    V_CTACONTABLECONTIG:=Val_cuentacon(x.pre_mod,x.pre_numcomprom,30,1);--Val_campo_char('DE21',p_operador,Val_rowid('DE21',p_operador,2,x.pre_credito),3);  
    V_SALDOCONTING:= X.lca_contingente;
    v_ORIGENCREDITO:= x.LCA_ORIGENCREDITO;-- Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),57); 
    --cambios abril 2018
	v_FECHAREFINANCIACION:=null;--hasta que Fisa entregue HD de refinanciados D11
	    select max(trunc(ref_fechaini))
      into v_FECHAREFINANCIACION
                               from TPRE_PRESTAMOSCOND 
                               where p_FechaCorte between trunc(ref_fechaini) and nvl(ref_fechafin,to_date('2199/12/31','yyyy/mm/dd'))   
                               and ref_credito = x.pre_credito
                               and ref_tipproc = 1;--1 refinanciado 2 renovado


	v_RAZONRIEGODEUDOR:=null;--usuario indica colocar Null - carlos Almanzar  D11 	v_RAZONRIEGODEUDOR:=null;--a espera de confirmacion correo francis 2018/04/11 2:21
	 SELECT  to_char(MAX(PRL_DESDE),'dd/mm/yyyy') 	--misma formula del D11
			  INTO v_FECHAINICOBRAJUDI
			FROM TPRE_PRESLEG 
			WHERE PRL_CREDITO = x.pre_credito
			  AND PRL_HASTA IS NULL;
    --fin			  
     v_TIPOCANCELACION:=null;
    /*begin
    select cst_tipocan
       into w_tipocan
      from  tpre_castigoshis
     where cst_credito = x.pre_credito
      and  rownum = 1;           
      if  w_tipocan='C' then
             v_TIPOCANCELACION:='C';
      else 
             v_TIPOCANCELACION:='B';
      end if;
                   
    exception
      when no_data_found then
          v_TIPOCANCELACION:='G';
    end;*/  
	--v_TIPOFLEXIBINORMA:=null;--a espera de confirmacion correo francis 2018/04/11 2:21

     v_DIASMORACAP:=x.LCA_DIASMORACAP;
     v_DIASMORAINT:=x.LCA_DIASMORACAP;
    
   insert into rsk_credact(FECHACORTE,EMPRESA,CODFILE,FECHAAPROB,FECHADESEMBOLSO,FECHAPRIMERPAGO,FECHAVENCIMIENTO,FECHARESTRUCTURA,FECHARENOV,FECHACANCELADO,PLAZOORIGINAL,
			                      PLAZOAJUSTADO,CODIGODEUDOR,TIPODEUDOR,NUMEROOPER,CODIGOREVOL,CUENTACONTABLE,TIPOGARANTIA,GARANTIAADMISIBLE,NOMBRECLIENTE,
								  CODIGOCRITERIO,CODIGOCATEGORIA,DIASMORA,MONEDA,TASAINTERES,TASAMORA,FREREPRECIOTASA,MONTOVIGENTE,MONTOAPROBADO,SALDOINSOLUTO,
			                      CUOTA,PERIODOCAPITAL,PERIODOINTERES,PRINCIPALMOROSO,INTERESMOROSO,SECTORECOSUJETO,SECTORECOCREDITO,TIPOCREDITO,CREDITSCORINGINI,
			                      IRBGENERAL,IRBMONTOEXPUESTO,IRBMONTOCUBIERTO,IRBRIESGOPAIS,MONTOCOBERTURAFIN,ESTADOOPERACION,PROVREQCAPITAL,CODIGOPAIS,LOCALIDAD,
								  PRODUCTO,ORIGENREC,OFICINASUCURSAL,SUBPOBLACIoN,CUOTASEGURO,CUOTACOMISION,APELLIDOSSIGLAS,PERIODOGRACIA,TIPOVINCULACION,FECHAINICIOADJU,
								  IDENTBENECONTING,NOMBREBENEFICIARIO,OPCIONPAGOCANCE,PENALPAGOANTICIPA,PROVIREQGRADUALCAP,PROVICAPCONST,PROVIREQRENDI,PROVIREQGRARENDI,
			                      PROVIREQCONTINGE,PROVIREQGRADCONTINGE,FECHAREVITASA,FREREPRECIO,FECHAPAGOCUOEXTRA,MONTOPAGOCUOEXTRA,BASETASAINTERES,REESTRUCTURADO,
			                      RENDIXCOBRAR,DEUDORESBENEFI,TIPOCLIENTE,FACILIDADCREDIT,CTACONTABLECONTIG,SALDOCONTING,ORIGENCREDITO,
			                      FECHAREFINANCIACION,RAZONRIEGODEUDOR,FECHAINICOBRAJUDI,TIPOCANCELACION,TIPOFLEXIBINORMA,DIASMORACAP,DIASMORAINT)
   values(p_FechaCorte,v_ruc,p_CODFILE,v_FECHAAPROB,v_FECHADESEMBOLSO,v_FECHAPRIMERPAGO,v_FECHAVENCIMIENTO,v_FECHARESTRUCTURA,v_FECHARENOV,v_FECHACANCELADO,V_PLAZOORIGINAL,
          v_PLAZOAJUSTADO,v_CODIGODEUDOR,v_TIPODEUDOR,v_NUMEROOPER,v_CODIGOREVOL,v_CUENTACONTABLE,v_TIPOGARANTIA,v_GARANTIAADMISIBLE,v_NOMBRECLIENTE,
          v_CODIGOCRITERIO,v_CODIGOCATEGORIA,v_DIASMORA,v_MONEDA,v_TASAINTERES,v_TASAMORA,v_FREREPRECIOTASA,v_MONTOVIGENTE,v_MONTOAPROBADO,v_SALDOINSOLUTO,
          v_CUOTA,v_PERIODOCAPITAL,v_PERIODOINTERES,v_PRINCIPALMOROSO,V_INTERESMOROSO,v_SECTORECOSUJETO,v_SECTORECOCREDITO,V_TIPOCREDITO,V_CREDITSCORINGINI,
          V_IRBGENERAL,V_IRBMONTOEXPUESTO,V_IRBMONTOCUBIERTO,V_IRBRIESGOPAIS,V_MONTOCOBERTURAFIN,V_ESTADOOPERACION,V_PROVREQCAPITAL,V_CODIGOPAIS,V_LOCALIDAD,
 		  V_PRODUCTO,v_ORIGENREC,v_OFICINASUCURSAL,v_SUBPOBLACIÓN,v_CUOTASEGURO,v_CUOTACOMISION,v_APELLIDOSSIGLAS,v_PERIODOGRACIA,V_TIPOVINCULACION,V_FECHAINICIOADJU,
 		  V_IDENTBENECONTING,V_NOMBREBENEFICIARIO,V_OPCIONPAGOCANCE,V_PENALPAGOANTICIPA,v_PROVIREQGRADUALCAP,v_PROVICAPCONST,v_PROVIREQRENDI,v_PROVIREQGRARENDI,
 		  v_PROVIREQCONTINGE,v_PROVIREQGRADCONTINGE,v_FECHAREVITASA,v_FREREPRECIO,v_FECHAPAGOCUOEXTRA,V_MONTOPAGOCUOEXTRA,V_BASETASAINTERES,V_REESTRUCTURADO,
 		  V_RENDIXCOBRAR,V_DEUDORESBENEFI,V_TIPOCLIENTE,v_FACILIDADCREDIT,V_CTACONTABLECONTIG,V_SALDOCONTING,v_ORIGENCREDITO,v_FECHAREFINANCIACION,v_RAZONRIEGODEUDOR,
		  v_FECHAINICOBRAJUDI,v_TIPOCANCELACION,v_TIPOFLEXIBINORMA,v_DIASMORACAP,v_DIASMORAINT);
 		  contador := contador + 1;
 		  commit;
  end loop;  
-------------------------------------------------------------------------cancelados-------------------------------------------------------  
   for x in cancelados loop   
      begin
      select *
        into r_operacioncalif
        from tleg_operacioncalif 
       where lca_cuenta = x.pre_credito
         and lca_fecha = (select max(lca_fecha) from tleg_operacioncalif where lca_cuenta = x.pre_credito);
      exception
         when others then
            r_operacioncalif:=null;      
      end; 
      begin
      select *
        into r_accrual
        from tpre_accrual
       where acr_credito = x.pre_credito
         and acr_fecha = (select max(acr_fecha) from tpre_accrual where acr_credito = x.pre_credito);
      exception
         when others then
            r_accrual:=null;      
      end;       
   v_FECHAAPROB:= to_char(x.pre_fecemi,'ddmmyyyy');         
   v_FECHADESEMBOLSO:= to_char(x.pre_fecontab,'ddmmyyyy');
   v_FECHAPRIMERPAGO:= to_char(x.pre_fecemi + x.pre_diasinicio,'ddmmyyyy');
   v_FECHAVENCIMIENTO:=to_char(x.pre_fecven,'ddmmyyyy');   
   v_FECHARESTRUCTURA:=to_char(r_operacioncalif.LCA_FECREESTRUC,'ddmmyyyy');   
   v_FECHARENOV:=to_char(r_operacioncalif.LCA_FECRENOV,'ddmmyyyy');   
   v_FECHACANCELADO:=to_char(x.PRE_FECANCE,'ddmmyyyy'); 
   V_PLAZOORIGINAL:=trunc((x.pre_plazoorg/30)); 
   v_PLAZOAJUSTADO:=trunc((x.pre_plazo/30));
   --inicio
        pkg_legalreport3.datos_cliente(x.pre_clientep,
                                       identifica,
                                       tipocli,
                                       nombre,
                                       apellido);   
   --fin
   v_CODIGODEUDOR:= identifica;--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),2);--identifica;            
   
   v_TIPODEUDOR:= tipocli;--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),3); --tipocli;

   v_NUMEROOPER:=x.pre_credito;
   v_CODIGOREVOL:= x.pre_numcomprom;
   v_CUENTACONTABLE:= CuentaCon(x.pre_credito,x.pre_mod,x.pre_pro,x.pre_tip,x.pre_moneda,21,p_fechacorte);
   v_TIPOGARANTIA := TIP_GARANTIA(x.pre_codtipgar,x.pre_garantia); 
   v_GARANTIAADMISIBLE:=pkg_legal_Dom_de.fun_garantia(X.pre_credito,p_FECHACORTE,null);--x.LCA_MONTOGARADMI;
   v_NOMBRECLIENTE := substr(replace(apellido,',','') ||';'||replace(nombre,',',''),1,60);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),4);--nombre ||', '||apellido;
   if r_operacioncalif.LCA_CODTIPOCREDITO is null then
       if x.PRE_TIPOCREDITO  = 20 then
          r_operacioncalif.LCA_CODTIPOCREDITO:='C';
       end if;
   end if;
   select decode(r_operacioncalif.LCA_CODTIPOCREDITO,'C','1A','M','1B','O','2A','H','3A','D','1D','1C')--abril 2018 
    into v_CODIGOCRITERIO
   from dual;
   v_CODIGOCATEGORIA:='A';                            
   v_DIASMORA:=nvl(nvl(r_operacioncalif.LCA_DIASMORAINT,r_operacioncalif.LCA_DIASMORACAP),91);
   select decode(x.pre_moneda,2,1,7,2,0) 
    into v_MONEDA
   from dual;
   v_TASAINTERES:= r_accrual.acr_tasa;
   v_TASAMORA:= NVL(Tasamora(x.pre_mod,x.pre_pro,x.pre_tip,x.pre_moneda,p_fechacorte),0);
   v_FREREPRECIOTASA:=12;   --consultar que va en el caso de los creditos que tienen tasa variable y los creditos que tiene tasa fija por 1 ,2 3,0 4 años
   v_MONTOVIGENTE:=round(pkg_legalreport3.cotiza_mon(p_fechaCorte,x.pre_valent,x.pre_moneda,x.pre_sucursal),2)  ;                                                                    
        IF x.pre_numcomprom is null then
          v_MONTOAPROBADO := pkg_legal_Dom_de.monto_aprobado(x.pre_credito,
                                                           p_fechacorte);
        ELSE
          v_MONTOAPROBADO := pkg_legal_Dom_de.monto_aprobado(x.pre_numcomprom,
                                                           p_fechacorte);
        END IF;
   
   --v_MONTOAPROBADO:=Val_campo_NUM('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),9);
   
   v_SALDOINSOLUTO:=saldo_capital_fecha(x.pre_credito,p_FechaCorte);


    v_CUOTA:=Val_cuota(x.pre_credito, x.pre_moneda,p_fechacorte,x.pre_tipocuo);--Val_campo_NUM('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),14);

     
        --PERIODOS(i.pre_tipocuo,i.pre_diaspercap,PERIODOCAP);
     
        --PERGRACIA(i.pre_credito, p_fecha, p_gracia);

      
   pkg_legal_dom_de.PERIODOS_CAP(x.pre_tipocuo, x.pre_diasper,v_PERIODOCAPITALaux);--Val_desctabla_n(701,Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),15));
   v_PERIODOCAPITAL:=Val_desctabla_n(704,replace(v_PERIODOCAPITALaux,' ',''));
   dbms_output.put_line('v_PERIODOCAPITAL:'||v_PERIODOCAPITAL);
   pkg_legal_dom_de.PERIODOS_INT(x.pre_tipocuo, x.pre_diasper, v_PERIODOINTERESaux);--w_diasper Val_desctabla_n(701,Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),16));
     dbms_output.put_line('v_PERIODOCAPITALAUX:'||v_PERIODOINTERESaux);
      v_PERIODOINTERES:=Val_desctabla_n(704,replace(v_PERIODOCAPITALaux,' ',''));   
     dbms_output.put_line('v_PERIODOINTERES:'||v_PERIODOINTERES);      
   select decode(nvl(r_accrual.acr_otrostatus,'-'),'J',0,r_accrual.acr_capnogi) + decode(nvl(r_accrual.acr_otrostatus,'-'),'-',r_accrual.acr_capvencido,0)
     into v_PRINCIPALMOROSO
     from dual;                                                        
     dbms_output.put_line('v_PRINCIPALMOROSO:'||v_PRINCIPALMOROSO);           
   select decode(nvl(r_accrual.acr_otrostatus,'-'),'J',0,r_accrual.acr_intactven) + decode(nvl(r_accrual.acr_otrostatus,'-'),'J',0,r_accrual.acr_intacteje)
     into V_INTERESMOROSO
     from dual;                                                               
     dbms_output.put_line('V_INTERESMOROSO:'||V_INTERESMOROSO);                
   begin
    v_SECTORECOSUJETO:=pkg_legal_dom_de.fun_actividad_cliente(x.pre_clientep);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),31);
   exception 
   when others then 
     v_SECTORECOSUJETO:=null;
   end;     
   dbms_output.put_line('v_SECTORECOSUJETO:'||v_SECTORECOSUJETO);                
   v_SECTORECOCREDITO:= pkg_legal_dom_de.fun_destino_ciiu(0,x.pre_credito,v_numcomprom);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),32);
   IF v_SECTORECOCREDITO = '9500' THEN
      v_SECTORECOCREDITO := '9500'||'01';
   END IF;
   
   dbms_output.put_line('v_SECTORECOCREDITO:'||v_SECTORECOCREDITO);                   
   --V_TIPOCREDITO:= x.lca_codtipocredito;--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),51);   
   V_TIPOCREDITO:= TipoCredito(x.pre_mod,x.pre_pro,x.pre_tip,x.pre_moneda);
   dbms_output.put_line('V_TIPOCREDITO:'||V_TIPOCREDITO);                   
      
   --ABRIL 2018
   IF V_TIPOCREDITO IS NULL THEN
     select decode(r_operacioncalif.LCA_CODTIPOCREDITO,'C','020','M','020','O','001','H','003','D','020','001')--abril 2018 
    into V_TIPOCREDITO
   from dual;
   END IF;
   V_CREDITSCORINGINI:='A';
   V_IRBGENERAL:='A';
   V_IRBMONTOEXPUESTO:='A';
   V_IRBMONTOCUBIERTO:='A';
   V_IRBRIESGOPAIS:='A';
   V_MONTOCOBERTURAFIN:=0;
   V_ESTADOOPERACION:='00';--PARA PRESTAMOS   
   V_PROVREQCAPITAL:=0;
   V_CODIGOPAIS:= PKG_LEGAL_DOM.CODIGO_PAIS(x.pre_clientep,x.pre_numdir);
        --Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),35);
   V_LOCALIDAD:=PKG_LEGAL_DOM_de.fun_localidad(x.pre_sucursal,x.pre_oficina,x.pre_clientep,x.pre_numdir);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),30);
   V_PRODUCTO:=  REPLACE(Nombre_prd(x.pre_mod,x.pre_pro,x.pre_tip,x.pre_moneda),',','');
   v_ORIGENREC:=PKG_LEGAL_DOM_de.RECURSO(x.pre_mod, x.pre_credito);--Val_desctabla_c(702,Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),25));
   v_OFICINASUCURSAL:=x.pre_sucursal;
   v_SUBPOBLACIÓN:=null;
   v_CUOTASEGURO:=0;
   v_CUOTACOMISION:=0;
   v_APELLIDOSSIGLAS:=substr(replace(apellido,',',''),1,30);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),5);    
   PKG_LEGAL_DOM_de.PERGRACIA(x.pre_credito, p_fechaCorte, v_PERIODOGRACIA);
   --v_PERIODOGRACIA:=;--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),17);
   V_TIPOVINCULACION:=Val_Relacion_bco(x.pre_clientep);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),26);
   V_FECHAINICIOADJU :=null;-- en el de11 esta en blanco Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),36);
   V_IDENTBENECONTING:=NULL;--EN EL DE11 Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),37);
   V_NOMBREBENEFICIARIO:= NULL;-- EN EL DE11 NULLVal_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),38);   
   V_OPCIONPAGOCANCE:='S';
   V_PENALPAGOANTICIPA:=Val_penalcance(p_fechaCorte,r_operacioncalif.LCA_CODTIPOCREDITO,x.pre_moneda,trunc(r_accrual.acr_plazo/30));--
   v_PROVIREQGRADUALCAP:=0;
   v_PROVICAPCONST:=0;
   v_PROVIREQRENDI:=0;
   v_PROVIREQGRARENDI:=0;
   v_PROVIREQCONTINGE:=0;
   v_PROVIREQGRADCONTINGE:=0;
   v_FECHAREVITASA:=to_char(x.pre_plazotasa,'dd/mm/yyyy');
   v_FREREPRECIO:=12;--mensual
   --v_FECHAPAGOCUOEXTRA:= TO_CHAR(Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),45),'DDMMYYYY');   
   PKG_LEGAL_DOM_de.pro_gen_cuoextra(x.pre_credito,p_fechaCorte,V_MONTOPAGOCUOEXTRAaux,v_fechacuoextraaux);
   --V_MONTOPAGOCUOEXTRA:=Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),46); 
   v_FECHAPAGOCUOEXTRA:= to_char(v_fechacuoextraaux,'dd/mm/yyyy');
   V_MONTOPAGOCUOEXTRA:=V_MONTOPAGOCUOEXTRAaux;
   V_BASETASAINTERES:=0;--360
    V_REESTRUCTURADO:= NVL(r_operacioncalif.LCA_ESTRUCTURACIONCRE,'NR');--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),56);
    select decode(nvl(r_accrual.acr_otrostatus,'-'),'J',0,r_accrual.acr_intactven) + decode(nvl(r_accrual.acr_otrostatus,'-'),'J',0,r_accrual.acr_intacteje)
      into V_RENDIXCOBRAR
      from dual; 
      --parametrizar la 21/9126
         begin  
        select decode(nvl(PTC_ENCAJE,'N'),'N','S'),CODEISO
        into V_DEUDORESBENEFI,v_TIPOFLEXIBINORMA
        from TPRE_PROTIPOCREDITO,tgen_desctabla 
       where PTC_MOD=x.pre_mod
        and PTC_PRO=x.pre_pro
        and PTC_TIP=x.pre_tip
        and PTC_MON= x.pre_moneda
        and nvl(PTC_TABFLEXIBLE,701) = des_codtab(+) 
        and PTC_CODFLEXIBLE = des_codigo(+);
     exception
       when no_data_found then
            V_DEUDORESBENEFI:='N';
            v_TIPOFLEXIBINORMA:=null;               
    end;              
    begin                               
     select nat_subtipocliente
       into v_tipocli
     from tcli_natural
     where nat_codcli = x.pre_clientep;
    exception
      when no_data_found then
      select jur_subtipocliente
        into v_tipocli
       from tcli_juridica 
       where jur_codcli = x.pre_clientep;
    end;
    V_TIPOCLIENTE:=Val_desctabla_iso(305,v_tipocli); 
    v_FACILIDADCREDIT:=PKG_LEGAL_DOM_de.fun_producto_servicio(x.pre_credito,6);--Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),48);
    IF v_FACILIDADCREDIT IS NULL THEN
    IF  r_operacioncalif.LCA_CODTIPOCREDITO IN ('C','D','M') THEN
        v_FACILIDADCREDIT:='112';
    ELSIF r_operacioncalif.LCA_CODTIPOCREDITO = 'O' THEN
        v_FACILIDADCREDIT:='161';    
    ELSIF r_operacioncalif.LCA_CODTIPOCREDITO = 'H' THEN
        v_FACILIDADCREDIT:='181';            
    END IF;
    END IF;    
    V_CTACONTABLECONTIG:=Val_cuentacon(x.pre_mod,x.pre_numcomprom,30,1);--Val_campo_char('DE21',p_operador,Val_rowid('DE21',p_operador,2,x.pre_credito),3);  
    V_SALDOCONTING:= r_operacioncalif.lca_contingente;
    v_ORIGENCREDITO:= nvl(r_operacioncalif.LCA_ORIGENCREDITO,'OP');-- Val_campo_char('DE11',p_operador,Val_rowid('DE11',p_operador,6,x.pre_credito),57); 
    --cambios abril 2018
	v_FECHAREFINANCIACION:=null;--hasta que Fisa entregue HD de refinanciados D11
	    select max(trunc(ref_fechaini))
      into v_FECHAREFINANCIACION
                               from TPRE_PRESTAMOSCOND 
                               where p_FechaCorte between trunc(ref_fechaini) and nvl(ref_fechafin,to_date('2199/12/31','yyyy/mm/dd'))   
                               and ref_credito = x.pre_credito
                               and ref_tipproc = 1;--1 refinanciado 2 renovado
	
	v_RAZONRIEGODEUDOR:=null;--usuario indica colocar Null - carlos Almanzar  D11 	v_RAZONRIEGODEUDOR:=null;--a espera de confirmacion correo francis 2018/04/11 2:21
	 SELECT  to_char(MAX(PRL_DESDE),'dd/mm/yyyy') 	--misma formula del D11
			  INTO v_FECHAINICOBRAJUDI
			FROM TPRE_PRESLEG 
			WHERE PRL_CREDITO = x.pre_credito
			  AND PRL_HASTA IS NULL;
    --fin			         
    declare
       v_tipocan varchar2(2);
    begin
    select distinct cst_tipocan 
       into v_tipocan
      from tpre_castigoshis
    where cst_credito = x.pre_credito    
      and rownum = 1;         
      if v_tipocan='C' then     
                 v_TIPOCANCELACION:='C';
      elsif v_tipocan='A' then                      
         v_TIPOCANCELACION:='B';      
      else                                    
        v_TIPOCANCELACION:='G';
      end if;
      exception
   when others then
         v_TIPOCANCELACION:='G';
    end ;
    /*begin
    select cst_tipocan
       into w_tipocan
      from  tpre_castigoshis
     where cst_credito = x.pre_credito
      and  rownum = 1;           
      if  w_tipocan='C' then
             v_TIPOCANCELACION:='C';
      else 
             v_TIPOCANCELACION:='B';
      end if;
                   
    exception
      when no_data_found then
          v_TIPOCANCELACION:='G';
    end;*/  
	--v_TIPOFLEXIBINORMA:=null;--a espera de confirmacion correo francis 2018/04/11 2:21
     v_DIASMORACAP:=0;
     v_DIASMORAINT:=0;

    
   insert into rsk_credact(FECHACORTE,EMPRESA,CODFILE,FECHAAPROB,FECHADESEMBOLSO,FECHAPRIMERPAGO,FECHAVENCIMIENTO,FECHARESTRUCTURA,FECHARENOV,FECHACANCELADO,PLAZOORIGINAL,
			                      PLAZOAJUSTADO,CODIGODEUDOR,TIPODEUDOR,NUMEROOPER,CODIGOREVOL,CUENTACONTABLE,TIPOGARANTIA,GARANTIAADMISIBLE,NOMBRECLIENTE,
								  CODIGOCRITERIO,CODIGOCATEGORIA,DIASMORA,MONEDA,TASAINTERES,TASAMORA,FREREPRECIOTASA,MONTOVIGENTE,MONTOAPROBADO,SALDOINSOLUTO,
			                      CUOTA,PERIODOCAPITAL,PERIODOINTERES,PRINCIPALMOROSO,INTERESMOROSO,SECTORECOSUJETO,SECTORECOCREDITO,TIPOCREDITO,CREDITSCORINGINI,
			                      IRBGENERAL,IRBMONTOEXPUESTO,IRBMONTOCUBIERTO,IRBRIESGOPAIS,MONTOCOBERTURAFIN,ESTADOOPERACION,PROVREQCAPITAL,CODIGOPAIS,LOCALIDAD,
								  PRODUCTO,ORIGENREC,OFICINASUCURSAL,SUBPOBLACIoN,CUOTASEGURO,CUOTACOMISION,APELLIDOSSIGLAS,PERIODOGRACIA,TIPOVINCULACION,FECHAINICIOADJU,
								  IDENTBENECONTING,NOMBREBENEFICIARIO,OPCIONPAGOCANCE,PENALPAGOANTICIPA,PROVIREQGRADUALCAP,PROVICAPCONST,PROVIREQRENDI,PROVIREQGRARENDI,
			                      PROVIREQCONTINGE,PROVIREQGRADCONTINGE,FECHAREVITASA,FREREPRECIO,FECHAPAGOCUOEXTRA,MONTOPAGOCUOEXTRA,BASETASAINTERES,REESTRUCTURADO,
			                      RENDIXCOBRAR,DEUDORESBENEFI,TIPOCLIENTE,FACILIDADCREDIT,CTACONTABLECONTIG,SALDOCONTING,ORIGENCREDITO,FECHAREFINANCIACION,RAZONRIEGODEUDOR,
							      FECHAINICOBRAJUDI,TIPOCANCELACION,TIPOFLEXIBINORMA,DIASMORACAP,DIASMORAINT)
   values(p_FechaCorte,v_ruc,p_CODFILE,v_FECHAAPROB,v_FECHADESEMBOLSO,v_FECHAPRIMERPAGO,v_FECHAVENCIMIENTO,v_FECHARESTRUCTURA,v_FECHARENOV,v_FECHACANCELADO,V_PLAZOORIGINAL,
          v_PLAZOAJUSTADO,v_CODIGODEUDOR,v_TIPODEUDOR,v_NUMEROOPER,v_CODIGOREVOL,v_CUENTACONTABLE,v_TIPOGARANTIA,v_GARANTIAADMISIBLE,v_NOMBRECLIENTE,
          v_CODIGOCRITERIO,v_CODIGOCATEGORIA,v_DIASMORA,v_MONEDA,v_TASAINTERES,v_TASAMORA,v_FREREPRECIOTASA,v_MONTOVIGENTE,v_MONTOAPROBADO,v_SALDOINSOLUTO,
          v_CUOTA,v_PERIODOCAPITAL,v_PERIODOINTERES,v_PRINCIPALMOROSO,V_INTERESMOROSO,v_SECTORECOSUJETO,v_SECTORECOCREDITO,V_TIPOCREDITO,V_CREDITSCORINGINI,
          V_IRBGENERAL,V_IRBMONTOEXPUESTO,V_IRBMONTOCUBIERTO,V_IRBRIESGOPAIS,V_MONTOCOBERTURAFIN,V_ESTADOOPERACION,V_PROVREQCAPITAL,V_CODIGOPAIS,V_LOCALIDAD,
 		  V_PRODUCTO,v_ORIGENREC,v_OFICINASUCURSAL,v_SUBPOBLACIÓN,v_CUOTASEGURO,v_CUOTACOMISION,v_APELLIDOSSIGLAS,v_PERIODOGRACIA,V_TIPOVINCULACION,V_FECHAINICIOADJU,
 		  V_IDENTBENECONTING,V_NOMBREBENEFICIARIO,V_OPCIONPAGOCANCE,V_PENALPAGOANTICIPA,v_PROVIREQGRADUALCAP,v_PROVICAPCONST,v_PROVIREQRENDI,v_PROVIREQGRARENDI,
 		  v_PROVIREQCONTINGE,v_PROVIREQGRADCONTINGE,v_FECHAREVITASA,v_FREREPRECIO,v_FECHAPAGOCUOEXTRA,V_MONTOPAGOCUOEXTRA,V_BASETASAINTERES,V_REESTRUCTURADO,
 		  V_RENDIXCOBRAR,V_DEUDORESBENEFI,V_TIPOCLIENTE,v_FACILIDADCREDIT,V_CTACONTABLECONTIG,V_SALDOCONTING,v_ORIGENCREDITO,v_FECHAREFINANCIACION,v_RAZONRIEGODEUDOR,
		  v_FECHAINICOBRAJUDI,v_TIPOCANCELACION,v_TIPOFLEXIBINORMA,v_DIASMORACAP,v_DIASMORAINT);
 		  contador := contador + 1;
 		  commit;
  end loop;    
    
  risk_cabecera(p_operador,'U',p_FechaCorte,p_CODFILE,to_CHAR(sysdate,'dd/mm/yyyy'),contador,00000,000000000000);
  file_credact(p_operador,v_ruc,p_FechaCorte,p_codfile);
end if;	
end;    
----codeudores
	procedure rsk_INTCODEUDOR(
	p_operador in number,
	p_dml in varchar2,
	p_FechaCorte in Date,
	p_codfile in varchar2,
	p_tiporel  number) is
		v_ruc varchar2(19);
	nombre       tcli_persona.cli_nombre%type;
    apellido     tcli_persona.cli_nombre%type;
    identifica   varchar2(20);
    tipocli      char(2);			
	v_FECHACORTE                               DATE ;
	v_EMPRESA                                  VARCHAR2(20);
	v_CODFILE                                  VARCHAR2(11);
	v_CODIGOCREDITO                            VARCHAR2(27);
	v_IDENTIFICADEUDOR                         VARCHAR2(15);
	v_TIPODEUDOR                               VARCHAR2(2);
	v_NOMBRECODEUDOR                           VARCHAR2(60);
	v_APELLIDOSSIGLAS                          VARCHAR2(60);
	v_TIPOCREDITO                              VARCHAR2(1);
	contador number:=0;
	    CURSOR CODEUDORES IS
      select pre_credito, clc_codcli, codeiso
        from tpre_prestamos, TCLI_CLICTA, tgen_desctabla, tcli_persona
       where trunc(pre_fecontab) <= p_fechacORTE and
             pre_status not in ('0', 'D', 'L', 'F', 'A') and
             nvl(pre_fecance, p_fechacORTE + 1) >= p_fechaCorte and pre_mod = clc_mod and
             pre_credito = clc_cue and clc_tiporel = p_tiporel and
             des_codtab = pre_tabtipocredito and
             des_codigo = pre_tipocredito and cli_codigo = clc_codcli;
	begin
	 
		select INS_RUC into v_ruc from tgen_instit;                                                             
if p_dml = 'D' then
	     delete rsk_INTCODEUDOR
	      where 
	       FECHACORTE = p_FechaCorte
	       and EMPRESA = v_ruc 
	       and CODFILE = p_codfile;
end if;          
if p_dml = 'I' then		
	      FOR I IN CODEUDORES LOOP    
		 --inicio
		        pkg_legalreport3.datos_cliente(I.clc_codcli,
		                                       identifica,
		                                       tipocli,
		                                       nombre,
		                                       apellido);   
		   --fin	      
	      v_CODIGOCREDITO:=I.pre_credito;                                                                     
	      v_IDENTIFICADEUDOR:= identifica; 
	      v_TIPODEUDOR:= tipocli;
	      v_NOMBRECODEUDOR:= REPLACE(nombre,',','');
	      v_APELLIDOSSIGLAS:=REPLACE(apellido,',','');
	      v_TIPOCREDITO:=pkg_legal_dom_de.codtipo_cartera(p_fechaCorte,'P',I.pre_credito);
	               insert into rsk_INTCODEUDOR(FECHACORTE,EMPRESA,CODFILE,CODIGOCREDITO,IDENTIFICADEUDOR,TIPODEUDOR,
											   NOMBRECODEUDOR,APELLIDOSSIGLAS,TIPOCREDITO)
	                                    values(P_FECHACORTE,v_ruc,P_CODFILE,v_CODIGOCREDITO,v_IDENTIFICADEUDOR,v_TIPODEUDOR,
											   v_NOMBRECODEUDOR,v_APELLIDOSSIGLAS,v_TIPOCREDITO); 
 		  contador := contador + 1;
 		  commit;											   
	      END LOOP;	                                                                             
	        risk_cabecera(p_operador,'U',p_FechaCorte,p_CODFILE,to_CHAR(sysdate,'dd/mm/yyyy'),contador,00000,000000000000);											   
            file_INTCODEUDOR(p_operador,v_ruc,p_FechaCorte,p_codfile);
  end if;            
	end;    
/*-----------------------------------------------------------------GARANTIAS-------------------------------------------------------------------------*/	
procedure rsk_INTGARANTIA(
	p_operador in number,
	p_dml in varchar2,
	p_FechaCorte in Date,
	p_codfile in varchar2,
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
                    P_TIPOCREDITO3 NUMBER
	) IS
    p_montoriesgo      number;
    v_codeiso          varchar2(2);
    CODIGO             NUMBER := 1; --contador de numero secuencial (1)
    p_identifica       VARCHAR2(15);
    p_tipoper          CHAR(2);
    p_tipoper_cli      NUMBER;
    p_nombre           VARCHAR2(60);
    p_apellido         VARCHAR2(60);
    p_garante16        VARCHAR2(60);
    p_garante17        VARCHAR2(30);
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
    v_ruc varchar2(19);    
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
             TRUNC(B.PRE_FECONTAB) <= p_FechaCorte AND
             NVL(B.PRE_FECANCE, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) > TO_DATE(p_FechaCorte, 'YYYY/MM/DD')
             AND nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_FechaCorte   --IS NULL
             AND ogr_fecdesde IS  NOT NULL
             AND gar_fecdesde IS NOT NULL
             AND nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_FechaCorte --HD4324_2 
             --AND PRE_CREDITO = 6080004776
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
             I.ACA_FECHA = p_FechaCorte AND
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
             TRUNC(B.VIS_FECHAPE) <= p_FechaCorte AND 
               trunc(crd_fechautor) <= p_FechaCorte
	         AND  crd_fechavenc >= p_FechaCorte
			 AND  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > p_FechaCorte
			 AND  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > p_FechaCorte               
             AND NVL(B.VIS_FECHCIERR, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) > TO_DATE(p_FechaCorte, 'YYYY/MM/DD')
             AND nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_FechaCorte  --IS NULL
             AND ogr_fecdesde IS  NOT NULL
             AND gar_fecdesde IS NOT NULL
             AND nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_FechaCorte; --HD4324_2
    --AND p_dateto BETWEEN A.OGR_FECDESDE AND NVL(A.OGR_FECHASTA, TO_DATE('2199/12/31', 'yyyy/mm/dd')); SE SOLICITA
    --DESPLEGAR LAS GARANTIAS QUE NO ESTEN INSCRITAS O LEGALIZADAS

    /* Selecciona todos los garantes personales a la fecha de créditos
    no vencidos de la tabla tcli_clicta, es decir mod=6 y tiporel=9*/

    CURSOR GARANTES IS
      SELECT CLC_CUE,
             CLC_CODCLI,
             PRE_STATUS,
             C.CODEISO,
             NVL(PRE_FECORIGINAL, PRE_FECEMI) PRE_FECEMI,
             CLC_TIPOREL
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
             TRUNC(B.PRE_FECONTAB) <= p_FechaCorte AND
             NVL(B.PRE_FECANCE, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_FechaCorte, 'YYYY/MM/DD') --HD4324_2
 UNION
       SELECT CLC_CUE,
             CLC_CODCLI,
             VIS_STATUS PRE_STATUS,
             C.CODEISO,
             CRD_FECHAVIG PRE_FECEMI,
             CLC_TIPOREL
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
             I.ACA_FECHA = p_FechaCorte AND
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
             TRUNC(B.VIS_FECHAPE) <= p_FechaCorte AND
                         trunc(crd_fechautor) <= p_FechaCorte
					   and  crd_fechavenc >= p_FechaCorte
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > p_FechaCorte 
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > p_FechaCorte             
             AND NVL(B.VIS_FECHCIERR, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_FechaCorte, 'YYYY/MM/DD'); --HD4324_2

    --UNION
    CURSOR GARANTESG3 IS
      SELECT CLC_CUE,
             CLC_CODCLI,
             PRE_STATUS,
             C.CODEISO,
             NVL(PRE_FECORIGINAL, PRE_FECEMI) PRE_FECEMI,
             CLC_TIPOREL
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
             TRUNC(B.PRE_FECONTAB) <= p_FechaCorte AND
             NVL(B.PRE_FECANCE, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_FechaCorte, 'YYYY/MM/DD') AND
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
                     TRUNC(B.PRE_FECONTAB) <= p_FechaCorte AND
                     NVL(B.PRE_FECANCE, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) >
                     TO_DATE(p_FechaCorte, 'YYYY/MM/DD')) AND
             CLC_CUE IN
             (SELECT CLC_CUE
                FROM TPRE_PRESTAMOS B, TCLI_CLICTA A
               WHERE A.CLC_MOD = B.PRE_MOD AND A.CLC_CUE = B.PRE_CREDITO AND
                     B.PRE_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
                    --A.CLC_TIPOREL  NOT IN (7,8) AND
                     TRUNC(B.PRE_FECONTAB) <= p_FechaCorte AND
                     NVL(B.PRE_FECANCE, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) >
                     TO_DATE(p_FechaCorte, 'YYYY/MM/DD')
               GROUP BY CLC_CUE
              --HAVING COUNT(*) = 1
              MINUS
              SELECT ogr_operacion
                FROM tcli_opergaran A, tpre_prestamos B
               WHERE A.OGR_OPERACION = B.PRE_CREDITO AND
                     B.PRE_STATUS NOT IN ('0', 'D', 'L', 'F', 'A') AND
                     TRUNC(B.PRE_FECONTAB) <= p_FechaCorte AND
                     nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_FechaCorte  AND--IS NULL
                     ogr_fecdesde is  not null AND
                     NVL(B.PRE_FECANCE, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) >
                     TO_DATE(p_FechaCorte, 'YYYY/MM/DD'))
      UNION
      SELECT CLC_CUE,
             CLC_CODCLI,
             PRE_STATUS,
             CODEISO,
             NVL(PRE_FECORIGINAL, PRE_FECEMI) PRE_FECEMI,
             CLC_TIPOREL
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
             TRUNC(B.PRE_FECONTAB) <= p_FechaCorte AND
             NVL(B.PRE_FECANCE, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_FechaCorte, 'YYYY/MM/DD')
      UNION
       SELECT CLC_CUE,
             CLC_CODCLI,
             VIS_STATUS PRE_STATUS,
             C.CODEISO,
             CRD_FECHAVIG PRE_FECEMI,
             CLC_TIPOREL
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
             I.ACA_FECHA = p_FechaCorte AND
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
             TRUNC(B.VIS_FECHAPE) <= p_FechaCorte AND
                        trunc(crd_fechautor) <= p_FechaCorte
					   and  crd_fechavenc >= p_FechaCorte
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > p_FechaCorte
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > p_FechaCorte             
             AND NVL(B.VIS_FECHCIERR, TO_DATE(p_FechaCorte, 'YYYY/MM/DD') + 1) >
             TO_DATE(p_FechaCorte, 'YYYY/MM/DD'); --HD4324_2
V_FECHACORTE                               DATE;
V_EMPRESA                                  VARCHAR2(20);
V_CODFILE                                  VARCHAR2(11);
V_CODIGOCREDITO                            VARCHAR2(27);
V_IDENTIFICAGARANTIA                       VARCHAR2(15);
V_TIPOGARANTESOLIDARIO                     VARCHAR2(2);
V_TIPOGARANTIA                             VARCHAR2(2);
V_DESCRIPGARANTIA                          VARCHAR2(250);
V_FECHACONSTIT                             VARCHAR2(10);
V_FECHAFORMA                               VARCHAR2(10);
V_FECHATASACION                            VARCHAR2(10);
V_VALORTASACION                            NUMBER(15,2);
V_RANGOGARANTIA                            NUMBER(1);
V_TIPOGARANTIAVAL                          NUMBER(3);
V_IDENTENTEMISORA                          VARCHAR2(15);
V_GARASEGURADA                             VARCHAR2(1);
V_FECHAVENPOLIZA                           VARCHAR2(10);
V_NOMBREGARANTE                            VARCHAR2(60);
V_APELLIDOGARAN                            VARCHAR2(30);
V_TIPOCREDITO                              VARCHAR2(1);
V_PORCTASACIONGAR                          NUMBER(6,2);
V_GARANTIAADMI                             NUMBER(15,2);
V_ANIOFABGARANTIA                          VARCHAR2(4);
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
p_tipoper_cli      NUMBER;
TXT_TIPOREL        TGEN_DESCTABLA.DES_DESCRIPCION%TYPE;
TXT_CODEISO        TGEN_DESCTABLA.CODEISO%TYPE;

BEGIN
		select INS_RUC into v_ruc from tgen_instit;                                                             
if p_dml = 'D' then
	     delete RSK_INTGARANTIA
	      where 
	       FECHACORTE = p_FechaCorte
	       and EMPRESA = v_ruc 
	       and CODFILE = p_codfile;
end if;          
if p_dml = 'I' then		
   contador:=0;
   FOR I IN GARANTIAS LOOP   
     pkg_legalreport3.DATOS_CLIENTE(I.ogr_cliente,
                                       p_identifica,
                                       p_tipoper,
                                       p_nombre,
                                       p_apellido);   
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
                                     trunc(p_fechacorte),
                                     gar_id,
                                     p_valores,
                                     p_noformato,
                                     p_descripcion,
                                     p_identificaemisor); 
         DBMS_OUTPUT.PUT_LINE('Paso 0:'||gar_id);                                                                            
        IF p_tipogar in (p_tipogarid1, p_tipogarid2, p_tipogarid3) THEN
          gar_id := p_identifica;
        END IF;  

        IF gar_id IS NULL THEN
           gar_id:=000-0000000-0;
        END IF;
        
        --
        pkg_legal_dom_de.fecha_tasacion(i.ogr_cliente,
                                        i.ogr_numgaran,
                                        p_decimales,
                                        p_fechacorte,
                                        p_fechat,
                                        p_valor);
        pkg_legalreport3.TIENE_PIGNORACION_BLH(i.ogr_cliente,
                                               i.ogr_numgaran,
                                               I.OGR_OPERACION,
                                               trunc(p_fechacorte),
                                               p_esplazo,
                                               p_esplazodelbanco,
                                               p_valorpignorado);
        pkg_legalreport3.ES_GARANTIAREAL(i.ogr_cliente,
                                         i.pre_clientep,
                                         I.OGR_OPERACION,
                                         p_esreal);
        v_anio:=pkg_legal_dom_de.fun_aniogarantia(i.ogr_cliente,i.ogr_numgaran);
        IF p_esplazo = 'S' THEN
          IF p_esplazodelbanco = 'S' THEN
            --
            p_valor := p_valorpignorado;
          END IF;
        END IF;
                                     
DBMS_OUTPUT.PUT_LINE('Paso 1');                                       
V_CODIGOCREDITO:= i.ogr_operacion;                                    
DBMS_OUTPUT.PUT_LINE('Paso 2:'||gar_id);                                       
V_IDENTIFICAGARANTIA:= substr(gar_id,1,15); 
DBMS_OUTPUT.PUT_LINE('Paso 3');                                       
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
          V_TIPOGARANTESOLIDARIO:= p_tipoper;
        ELSE
          V_TIPOGARANTESOLIDARIO:= NULL;
        END IF;          
        IF i.ogr_fecdesde IS NULL THEN
          p_tipogar := NULL;
        END IF;
        V_TIPOGARANTIA:= p_tipogar;
        DBMS_OUTPUT.PUT_LINE('Paso 4');                                       
        V_DESCRIPGARANTIA:= SUBSTR(REPLACE(i.gar_descrip,',',''),1,250);
        DBMS_OUTPUT.PUT_LINE('Paso 5');
		V_FECHACONSTIT:= TO_CHAR(p_fechaconst,'DD/MM/YYYY');          
		DBMS_OUTPUT.PUT_LINE('Paso 6');
         vld_ogr_fecha := I.OGR_FECHA;
         IF p_tipogar NOT IN  ('HI','I1','I2','I3','I4','I5','I6','I7','P2','P3','P4') THEN
              vld_ogr_fecha := p_fechaconst;
         END IF;
         --viene de BDI
         IF p_tipogar  IN  ('P2','P3','P4') THEN
              vld_ogr_fecha := p_FECHACORTE;
         END IF;
         --fin de bdi
		V_FECHAFORMA:= TO_CHAR(vld_ogr_fecha,'DD/MM/YYYY');
		DBMS_OUTPUT.PUT_LINE('Paso 7');
        --EN BLH
        pkg_legalreport3.segurogarantia_BLH(i.ogr_operacion,
                                            i.ogr_cliente,
                                            i.ogr_numgaran,
                                            p_FECHACORTE,
                                            p_seguro,
                                            p_fechavencepoliza);
        IF upper(p_tipogar) IN ('V1', 'V4','V5','V2','V6', 'V3','V7') THEN
          -- ANTES P3
          p_fechat := NULL;
        END IF;
        V_FECHATASACION:= TO_CHAR(p_fechat,'DD/MM/YYYY');
        DBMS_OUTPUT.PUT_LINE('Paso 8');
        -- si no es null
           p_valor:=p_valor*bdi_promedio(I.gar_moneda,1,p_FechaCorte);
        --NUEVO CAMBIO OCTUBRE 2008
        IF p_valor IS NULL THEN
          p_valor := pkg_legal_dom_de.SALDO_CAPITAL_FECHA_DE(I.OGR_OPERACION,p_FechaCorte);
        END IF;
        --
	V_VALORTASACION:=  p_valor;     
	DBMS_OUTPUT.PUT_LINE('Paso 9');   
 V_RANGOGARANTIA:=  I.OGR_GRADO;
 DBMS_OUTPUT.PUT_LINE('Paso 10');        
        -- Si la garantía es en valores, es decir acciones o documentos el tipo de garantia en valores es igual al codeiso de
        -- tabla tgen_desctabla con codtab=22

        p_tipvalor := PKG_LEGAL_DOM_de.fun_producto_servicio(i.ogr_operacion,6);-- TO_NUMBER(substr(pkg_legal_dom.tipo_valores(I.gar_tipbien),1,3));
        --p_tipvalor := TO_NUMBER(substr(pkg_legal_dom.tipo_valores(I.gar_tipbien),1,3));

        IF p_valores = 'S' THEN
			V_TIPOGARANTIAVAL:= p_tipvalor;
        ELSE
			V_TIPOGARANTIAVAL:=NULL;
        END IF;    
        ---CARGAR PARAMETROS            
          BEGIN
           SELECT HOMOLOGADO 
            INTO V_TIPOGARANTIAVAL
           FROM TDOM_9497D 
           WHERE TABLA='GAR_TGV'
             AND CODIGO = p_tipogar;
           EXCEPTION
              WHEN OTHERS THEN
                V_TIPOGARANTIAVAL:=NULL;
           END;
        
        DBMS_OUTPUT.PUT_LINE('Paso 11');
        --si son plazos del banco deben salir el rnc del banco
        -- si son de otra institucion deben salir el rnc de la otra ins. para eso deben llenar en el campo
        --identificación de la pantalla de depósitos a plazo
        --IF p_esplazodelbanco = 'S' THEN
        IF upper(p_tipogar) in ('V2','V6') THEN
          pkg_legalreport3.RUC_INSTITUCION(p_ruc);
          p_identificaemisor := p_ruc;
        END IF;       
V_IDENTENTEMISORA:= p_identificaemisor;
DBMS_OUTPUT.PUT_LINE('Paso 12');
V_GARASEGURADA:= nvl(p_seguro, 'N');
DBMS_OUTPUT.PUT_LINE('Paso 13');
V_FECHAVENPOLIZA:= TO_CHAR(p_fechavencepoliza,'DD/MM/YYYY');
DBMS_OUTPUT.PUT_LINE('Paso 14');
V_NOMBREGARANTE:= REPLACE(p_garante16,',','');         
DBMS_OUTPUT.PUT_LINE('Paso 15');
V_APELLIDOGARAN:= REPLACE(p_garante17,',','');
DBMS_OUTPUT.PUT_LINE('Paso 16');
v_codeiso := pkg_legal_dom_de.codtipo_cartera(p_fechaCorte,'P',I.OGR_OPERACION);
V_TIPOCREDITO:= v_codeiso; 
DBMS_OUTPUT.PUT_LINE('Paso 17');  
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
     w_capred := gar_saldooperaciones(i.ogr_mod,i.ogr_operacion,p_fechaCorte,w_monoper,w_monoper,1);
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
V_PORCTASACIONGAR:= w_porcentaje;--garantia_adm;       
DBMS_OUTPUT.PUT_LINE('Paso 18');
garantia_adm:= pkg_legal_dom_de.fun_garantia(I.OGR_OPERACION,p_FechaCorte,null);          
V_GARANTIAADMI:= garantia_adm;  
DBMS_OUTPUT.PUT_LINE('Paso 19');        
V_ANIOFABGARANTIA:= v_anio;  
DBMS_OUTPUT.PUT_LINE('Paso 20');     
V_MONTOFORMAGAR:= w_totgaran;
DBMS_OUTPUT.PUT_LINE('Paso 21');          
  begin
  select ava_cedula
    into V_IDENTTASADOR
from tgar_avaluos,TGAR_AVALUADORES
where avl_codcli = i.ogr_cliente
  and avl_numgar = i.ogr_numgaran
  and P_FECHACORTE between trunc(avl_fecha) and nvl(avl_fechasta, to_date('2199/12/31','yyyy/mm/dd'))
  and avl_codava = ava_codigo;
exception
  when others then
     V_IDENTTASADOR:=null;
  end;                   
begin
select to_char(max(pre_fecven),'dd/mm/yyyy')
  into V_FECHAVENCGAR
 from tpre_prestamos
  where pre_credito in (select ogr_operacion from tcli_opergaran where ogr_cliente = i.ogr_cliente and ogr_numgaran = i.ogr_numgaran);
exception 
  when others then
DBMS_OUTPUT.PUT_LINE('Paso 22');  
V_FECHAVENCGAR:= null; --PENDIENTE DEFINIR SI VA FECHA DE VENCIMIENTO DEL PRESTAMO.-2018/09/26    
end;      
r_endosobien:=null;          
DBMS_OUTPUT.PUT_LINE('Paso 23');
begin
 select *
   into r_endosobien
  from tpre_endosobien
  where ecb_codcli= i.ogr_cliente
    and ecb_numgar = i.ogr_numgaran
    and P_FECHACORTE between trunc(ecb_desde) 
                                           and nvl(ecb_hasta, to_date('2199/12/31','yyyy/mm/dd'));
exception
  when others then
      r_endosobien:=null;
end;   
v_NUMEROPOLIZASEG:= substr(r_endosobien.ecb_poliza,1,10);           
DBMS_OUTPUT.PUT_LINE('Paso 24');
V_FECEMIPOLSEG:=TO_CHAR(r_endosobien.ecb_emipol,'DD/MM/YYYY');           
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
    V_DESCRIPFIDE:=V_DESCRIPGARANTIA;
    V_CLASIFFIDUCIARIA:= LPAD(V_TIPOGARANTIA,'0',3);    
END IF;

DBMS_OUTPUT.PUT_LINE('Paso 30:'||V_FECHACONSTIT);
DBMS_OUTPUT.PUT_LINE('Paso 30.1:'||V_FECHAFORMA);
DBMS_OUTPUT.PUT_LINE('Paso 30.2:'||V_FECHATASACION);
DBMS_OUTPUT.PUT_LINE('Paso 30.3:'||V_FECHAVENPOLIZA);
DBMS_OUTPUT.PUT_LINE('Paso 30.4:'||V_FECHAVENCGAR);
DBMS_OUTPUT.PUT_LINE('Paso 30.5:'||V_FECEMIPOLSEG);
      INSERT INTO RSK_INTGARANTIA(FECHACORTE,EMPRESA,CODFILE,CODIGOCREDITO,IDENTIFICAGARANTIA,TIPOGARANTESOLIDARIO,TIPOGARANTIA,
								  DESCRIPGARANTIA,FECHACONSTIT,FECHAFORMA,FECHATASACION,VALORTASACION,RANGOGARANTIA,TIPOGARANTIAVAL,
						          IDENTENTEMISORA,GARASEGURADA,FECHAVENPOLIZA,NOMBREGARANTE,APELLIDOGARAN,TIPOCREDITO,PORCTASACIONGAR,
								  GARANTIAADMI,ANIOFABGARANTIA,MONTOFORMAGAR,IDENTTASADOR,FECHAVENCGAR,NUMEROPOLIZASEG,FECEMIPOLSEG,IDENTCOMASEGURADORA,
								  VALORENDOSO,GARANFIDUCIARIA,CLASIFFIDUCIARIA,DESCRIPFIDE)
			      VALUES (P_FECHACORTE,V_RUC,P_CODFILE,V_CODIGOCREDITO,V_IDENTIFICAGARANTIA,
			               V_TIPOGARANTESOLIDARIO,V_TIPOGARANTIA,
								  V_DESCRIPGARANTIA,V_FECHACONSTIT,V_FECHAFORMA,
								  V_FECHATASACION,V_VALORTASACION,V_RANGOGARANTIA,V_TIPOGARANTIAVAL,
						          V_IDENTENTEMISORA,V_GARASEGURADA,
						          V_FECHAVENPOLIZA,V_NOMBREGARANTE,V_APELLIDOGARAN,V_TIPOCREDITO,V_PORCTASACIONGAR,
								  V_GARANTIAADMI,V_ANIOFABGARANTIA,V_MONTOFORMAGAR,V_IDENTTASADOR,
								  V_FECHAVENCGAR,v_NUMEROPOLIZASEG,
								  V_FECEMIPOLSEG,V_IDENTCOMASEGURADORA,
								  V_VALORENDOSO,V_GARANFIDUCIARIA,V_CLASIFFIDUCIARIA,V_DESCRIPFIDE);

 		  contador := contador + 1;
 		  commit;								      
   END LOOP;  
   --Garantes
/*   FOR I IN GARANTES LOOP   
        pkg_legalreport3.DATOS_CLIENTE_BLH(I.clc_codcli,
                                           p_identifica,
                                           p_tipoper,
                                           p_nombre,
                                           p_apellido,
                                           p_tipoper_cli);

		BEGIN
          SELECT DES_DESCRIPCION, CODEISO
            INTO TXT_TIPOREL, TXT_CODEISO
            FROM TCLI_CLICTA, TGEN_RELPROCLI, TGEN_DESCTABLA
           WHERE CLC_MOD = RPC_MOD AND CLC_TIPOREL = RPC_RELACION AND
                 RPC_TABREL = DES_CODTAB AND RPC_RELACION = DES_CODIGO AND
                 CLC_CODCLI = I.clc_codcli AND CLC_CUE = I.CLC_CUE;
        EXCEPTION
          WHEN OTHERS THEN
            TXT_TIPOREL := 'FIRMA SOLIDARIA';
            TXT_CODEISO := 'S4';
        END;                                         
        -- Este procedimiento recibe el modulo, el número de credito, codigo de cliente y tipo de persona;
        --  debe devolver tipo garantia en formato H1, H2, etc.
        pkg_legal_dom.tip_garantia(i.ogr_mod,
                                   i.ogr_operacion,
                                   i.ogr_cliente,
                                   i.OGR_NUMGARAN,
                                   p_tipogar);
         p_tipogar:= TXT_CODEISO;
        --  i.bxg_formato IN, i.ogr_cliente IN , i.ogr_numgaran IN, gar_id OUT(3), p_valores OUT, p_noformato OUT, p_desc OUT
        --  Este procedimiento devuelve la identificación de la garantia
        pkg_legalreport3.garantia_id(i.bxg_formato,
                                     i.ogr_cliente,
                                     i.ogr_numgaran,
                                     trunc(p_fechacorte),
                                     gar_id,
                                     p_valores,
                                     p_noformato,
                                     p_descripcion,
                                     p_identificaemisor); 
         DBMS_OUTPUT.PUT_LINE('Paso 0:'||gar_id);                                                                            
        IF p_tipogar in (p_tipogarid1, p_tipogarid2, p_tipogarid3) THEN
          gar_id := p_identifica;
        END IF;
        --
        pkg_legal_dom_de.fecha_tasacion(i.ogr_cliente,
                                        i.ogr_numgaran,
                                        p_decimales,
                                        p_fechacorte,
                                        p_fechat,
                                        p_valor);
        pkg_legalreport3.TIENE_PIGNORACION_BLH(i.ogr_cliente,
                                               i.ogr_numgaran,
                                               I.OGR_OPERACION,
                                               trunc(p_fechacorte),
                                               p_esplazo,
                                               p_esplazodelbanco,
                                               p_valorpignorado);
        pkg_legalreport3.ES_GARANTIAREAL(i.ogr_cliente,
                                         i.pre_clientep,
                                         I.OGR_OPERACION,
                                         p_esreal);
        v_anio:=pkg_legal_dom_de.fun_aniogarantia(i.ogr_cliente,i.ogr_numgaran);
        IF p_esplazo = 'S' THEN
          IF p_esplazodelbanco = 'S' THEN
            --
            p_valor := p_valorpignorado;
          END IF;
        END IF;
                                     
DBMS_OUTPUT.PUT_LINE('Paso 1');                                       
V_CODIGOCREDITO:= i.ogr_operacion;                                    
DBMS_OUTPUT.PUT_LINE('Paso 2:'||gar_id);                                       
V_IDENTIFICAGARANTIA:= substr(gar_id,1,15); 
DBMS_OUTPUT.PUT_LINE('Paso 3');                                       
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
          V_TIPOGARANTESOLIDARIO:= p_tipoper;
        ELSE
          V_TIPOGARANTESOLIDARIO:= NULL;
        END IF;          
        IF i.ogr_fecdesde IS NULL THEN
          p_tipogar := NULL;
        END IF;
        V_TIPOGARANTIA:= p_tipogar;
        DBMS_OUTPUT.PUT_LINE('Paso 4');                                       
        V_DESCRIPGARANTIA:= SUBSTR(REPLACE(i.gar_descrip,',',''),1,250);
        DBMS_OUTPUT.PUT_LINE('Paso 5');
		V_FECHACONSTIT:= p_fechaconst;          
		DBMS_OUTPUT.PUT_LINE('Paso 6');
         vld_ogr_fecha := I.OGR_FECHA;
         IF p_tipogar NOT IN  ('HI','I1','I2','I3','I4','I5','I6','I7','P2','P3','P4') THEN
              vld_ogr_fecha := p_fechaconst;
         END IF;
         --viene de BDI
         IF p_tipogar  IN  ('P2','P3','P4') THEN
              vld_ogr_fecha := p_FECHACORTE;
         END IF;
         --fin de bdi
		V_FECHAFORMA:= vld_ogr_fecha;            
		DBMS_OUTPUT.PUT_LINE('Paso 7');
        --EN BLH
        pkg_legalreport3.segurogarantia_BLH(i.ogr_operacion,
                                            i.ogr_cliente,
                                            i.ogr_numgaran,
                                            p_FECHACORTE,
                                            p_seguro,
                                            p_fechavencepoliza);
        IF upper(p_tipogar) IN ('V1', 'V4','V5','V2','V6', 'V3','V7') THEN
          -- ANTES P3
          p_fechat := NULL;
        END IF;
        V_FECHATASACION:= p_fechat; 
        DBMS_OUTPUT.PUT_LINE('Paso 8');
        -- si no es null
           p_valor:=p_valor*bdi_promedio(I.gar_moneda,1,p_FechaCorte);
        --NUEVO CAMBIO OCTUBRE 2008
        IF p_valor IS NULL THEN
          p_valor := pkg_legal_dom_de.SALDO_CAPITAL_FECHA_DE(I.OGR_OPERACION,p_FechaCorte);
        END IF;
        --
	V_VALORTASACION:=  p_valor;     
	DBMS_OUTPUT.PUT_LINE('Paso 9');   
 V_RANGOGARANTIA:=  I.OGR_GRADO;
 DBMS_OUTPUT.PUT_LINE('Paso 10');        
        -- Si la garantía es en valores, es decir acciones o documentos el tipo de garantia en valores es igual al codeiso de
        -- tabla tgen_desctabla con codtab=22

        p_tipvalor := TO_NUMBER(substr(pkg_legal_dom.tipo_valores(I.gar_tipbien),
                                       1,
                                       3));

        IF p_valores = 'S' THEN
			V_TIPOGARANTIAVAL:= p_tipvalor;
        ELSE
			V_TIPOGARANTIAVAL:=NULL;
        END IF;               
        DBMS_OUTPUT.PUT_LINE('Paso 11');
        --si son plazos del banco deben salir el rnc del banco
        -- si son de otra institucion deben salir el rnc de la otra ins. para eso deben llenar en el campo
        --identificación de la pantalla de depósitos a plazo
        --IF p_esplazodelbanco = 'S' THEN
        IF upper(p_tipogar) in ('V2','V6') THEN
          pkg_legalreport3.RUC_INSTITUCION(p_ruc);
          p_identificaemisor := p_ruc;
        END IF;       
V_IDENTENTEMISORA:= p_identificaemisor;
DBMS_OUTPUT.PUT_LINE('Paso 12');
V_GARASEGURADA:= nvl(p_seguro, 'N');
DBMS_OUTPUT.PUT_LINE('Paso 13');
V_FECHAVENPOLIZA:= p_fechavencepoliza;
DBMS_OUTPUT.PUT_LINE('Paso 14');
V_NOMBREGARANTE:= REPLACE(p_garante16,',','');         
DBMS_OUTPUT.PUT_LINE('Paso 15');
V_APELLIDOGARAN:= REPLACE(p_garante17,',','');
DBMS_OUTPUT.PUT_LINE('Paso 16');
v_codeiso := pkg_legal_dom_de.codtipo_cartera(p_fechaCorte,'P',I.OGR_OPERACION);
V_TIPOCREDITO:= v_codeiso; 
DBMS_OUTPUT.PUT_LINE('Paso 17');  
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
     w_capred := gar_saldooperaciones(i.ogr_mod,i.ogr_operacion,p_fechaCorte,w_monoper,w_monoper,1);
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
V_PORCTASACIONGAR:= w_porcentaje;--garantia_adm;       
DBMS_OUTPUT.PUT_LINE('Paso 18');
garantia_adm:= pkg_legal_dom_de.fun_garantia(I.OGR_OPERACION,p_FechaCorte);          
V_GARANTIAADMI:= garantia_adm;  
DBMS_OUTPUT.PUT_LINE('Paso 19');        
V_ANIOFABGARANTIA:= v_anio;  
DBMS_OUTPUT.PUT_LINE('Paso 20');     
V_MONTOFORMAGAR:= w_totgaran;
DBMS_OUTPUT.PUT_LINE('Paso 21');          
  begin
  select ava_cedula
    into V_IDENTTASADOR
from tgar_avaluos,TGAR_AVALUADORES
where avl_codcli = i.ogr_cliente
  and avl_numgar = i.ogr_numgaran
  and P_FECHACORTE between trunc(avl_fecha) and nvl(avl_fechasta, to_date('2199/12/31','yyyy/mm/dd'))
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
    and P_FECHACORTE between trunc(ecb_desde) 
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
     V_IDENTCOMASEGURADORA:=null;
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
    V_DESCRIPFIDE:=V_DESCRIPGARANTIA;
    V_CLASIFFIDUCIARIA:= LPAD(V_TIPOGARANTIA,'0',3);    
END IF;

DBMS_OUTPUT.PUT_LINE('Paso 30');
      INSERT INTO RSK_INTGARANTIA(FECHACORTE,EMPRESA,CODFILE,CODIGOCREDITO,IDENTIFICAGARANTIA,TIPOGARANTESOLIDARIO,TIPOGARANTIA,
								  DESCRIPGARANTIA,FECHACONSTIT,FECHAFORMA,FECHATASACION,VALORTASACION,RANGOGARANTIA,TIPOGARANTIAVAL,
						          IDENTENTEMISORA,GARASEGURADA,FECHAVENPOLIZA,NOMBREGARANTE,APELLIDOGARAN,TIPOCREDITO,PORCTASACIONGAR,
								  GARANTIAADMI,ANIOFABGARANTIA,MONTOFORMAGAR,IDENTTASADOR,FECHAVENCGAR,NUMEROPOLIZASEG,FECEMIPOLSEG,IDENTCOMASEGURADORA,
								  VALORENDOSO,GARANFIDUCIARIA,CLASIFFIDUCIARIA,DESCRIPFIDE)
			      VALUES (P_FECHACORTE,V_RUC,P_CODFILE,V_CODIGOCREDITO,V_IDENTIFICAGARANTIA,V_TIPOGARANTESOLIDARIO,V_TIPOGARANTIA,
								  V_DESCRIPGARANTIA,V_FECHACONSTIT,V_FECHAFORMA,V_FECHATASACION,V_VALORTASACION,V_RANGOGARANTIA,V_TIPOGARANTIAVAL,
						          V_IDENTENTEMISORA,V_GARASEGURADA,V_FECHAVENPOLIZA,V_NOMBREGARANTE,V_APELLIDOGARAN,V_TIPOCREDITO,V_PORCTASACIONGAR,
								  V_GARANTIAADMI,V_ANIOFABGARANTIA,V_MONTOFORMAGAR,V_IDENTTASADOR,V_FECHAVENCGAR,v_NUMEROPOLIZASEG,V_FECEMIPOLSEG,V_IDENTCOMASEGURADORA,
								  V_VALORENDOSO,V_GARANFIDUCIARIA,V_CLASIFFIDUCIARIA,V_DESCRIPFIDE);

 		  contador := contador + 1;
 		  commit;								      
   END LOOP;  */   
   --fin de garantes
   
   
   	        risk_cabecera(p_operador,'U',p_FechaCorte,p_CODFILE,to_CHAR(sysdate,'dd/mm/yyyy'),contador,00000,000000000000);											   
            file_INTGARANTIA(p_operador,v_ruc,p_FechaCorte,p_codfile);
END IF;   
EXCEPTION
 WHEN OTHERS THEN 
    RAISE_APPLICATION_ERROR(-20801,'eRROR:'||V_CODIGOCREDITO||' '||SQLERRM);
END;--fin garantias
-----------para cuenta contable------------------

function cuenta_con(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number,
					p_gru in  number   ) return varchar2 is
 V_CUENTA VARCHAR2(20);					
begin                                                      
 SELECT  GDT_CUENTA
   INTO  V_CUENTA
      FROM  TPRE_FUNCGROUPDET
      WHERE GDT_MODRPT  = 21
      AND   GDT_TRARPT  = 4963
      AND   GDT_ENTRPT  = 0
      AND   GDT_SECRPT  = 1
      AND GDT_MOD = P_MOD
      AND GDT_PRO = P_PRO
      AND GDT_TIP = P_TIP
      AND GDT_MON = P_MON
      AND GDT_CODGRU = P_GRU;
      return V_CUENTA;
 exception
 WHEN others then 
       
      return null;
end;	
function cuenta_connos(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number,
					p_tra in  number,
					p_rubro in number   ) return varchar2 is
 V_CUENTA VARCHAR2(20);					
begin                                                      
 SELECT  CON_CTADEB
   INTO  V_CUENTA
      FROM  tgen_trancont
      WHERE CON_MOD = P_MOD
      AND CON_PRO = P_PRO
      AND CON_TIP = P_TIP
      AND CON_MON = P_MON
      AND CON_TRA = P_TRA
      AND CON_RUBRO = P_RUBRO
      AND CON_FECHAHASTA IS NULL;
      return V_CUENTA;
 exception
 WHEN others then 
       
      return null;
end cuenta_connos;					
				
----------------codigo de categoria----------------------------
--90/7002
function codigo_cat(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number) return varchar2    is
   v_tipoprod varchar2(1);					
 begin   
   select PLE_TIPOPROD
     into v_tipoprod 
    from TGEN_PRELOCEXT 
    where PLE_MOD=p_mod
     and PLE_PRO=p_pro        
     and PLE_TIP=p_tip        
     and PLE_MON=p_mon;
     if v_tipoprod = 'C' then
        return 1;
     elsif v_tipoprod = 'A' then
        return 2;
     else
      return 3;--ANTES ESTABA 0
     end if;
    exception
      when others then
         return 0;        
 end;					  
---------------------------------------------status Cuenta -----------------------------------
function status_cta(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number,
					p_numcue in number,
					p_status in varchar2,
					p_fecha in date) return varchar2 is
     V_EXISTE NUMBER:=0;
     v_status varchar2(1);					
	begin           
	BEGIN
	SELECT HOMOLOGADO
	 INTO V_STATUS 
     FROM TDOM_9497D
     WHERE 	 TABLA = 'ASM_E'
       AND CODIGO IN (SELECT BALTYPE
                       FROM TGEN_SALDOMODULOs
                       WHERE DATELOAD = P_FECHA
                         AND ACC=P_NUMCUE)
      AND ROWNUM = 1;
        RETURN V_STATUS;
     EXCEPTION 
      WHEN OTHERS THEN
       --1
         BEGIN 
         SELECT COUNT(*)
           INTO V_EXISTE
           FROM TGEN_SALDOMODULOs
          WHERE DATELOAD = P_FECHA
           AND ACC=P_NUMCUE
           AND SUBSTR(BALTYPE,1,2)= 'CG';
           if v_existe > 0 then
              return 'G';         
           end if;
         SELECT COUNT(*)
           INTO V_EXISTE
           FROM TGEN_SALDOMODULOs
          WHERE DATELOAD = P_FECHA
           AND ACC=P_NUMCUE
           AND SUBSTR(BALTYPE,1,2)= 'CB';
           if v_existe > 0 then
              return 'R';         
           end if;
            SELECT COUNT(*)
              INTO V_EXISTE
           FROM TGEN_SALDOMODULOs
          WHERE DATELOAD = P_FECHA
           AND ACC=P_NUMCUE
           AND APPID IN (SELECT CODIGO FROM tdom_9497d where tabla = 'ASM_B');
           if v_existe > 0 then
              return 'B';         
           end if;
           IF P_STATUS = 6 THEN
              RETURN 'I';
           END IF;       
				RETURN 'A';           
		 EXCEPTION 
		 WHEN OTHERS THEN
		                      return 'A';    
         END ;                                  
         
      --FIN 1                          
      
     END ; 
     RETURN 'A';      
	end;					 
function CuentaCon(credito in number,
					modulo in number, 
                    producto in number,
                    tipo in number, 
                    moneda in number,
                    codgru in number,
                    fecha in date) return varchar2 is
v_cuenta varchar2(20);
vlv_cuenta varchar2(30);
begin                
  begin
      select gdt_cuenta
        into v_cuenta
        from tpre_funcgroupdet, tpre_funcgroup
       where GDT_MOD = modulo 
         and GDT_PRO = producto 
         and GDT_TIP = tipo 
         and GDT_MON = moneda 
         and fng_modrpt = 20 
         and fng_trarpt = 450 
         and fng_entrpt = 0 
         and fng_secrpt = 1 
         and gdt_mod = 6 
         and fng_cuenta = gdt_cuenta 
         and GDT_MODRPT = fng_modrpt
         and GDT_TRARPT = fng_trarpt 
         and fng_secrpt = gdt_secrpt
         and GDT_CODGRU = codgru;
  exception
    when others then
       v_cuenta:=null;
  end;
  vlv_cuenta := pkg_legal_dom_de.fun_gen_cuenta(credito,
                               v_cuenta,
                                         4,
                                         6,
                                         '2045001',
                                         fecha);
     return vlv_cuenta;
end;   
FUNCTION FGETDOMSALDOVISTA(p_ope in number,P_CTA IN NUMBER, P_FECHA IN DATE ,ANIGROUP     VARCHAR2) RETURN NUMBER is
   v_valor number:=0;
  begin
  SELECT sum(nvl(gdt_saldomod,0))
    into v_valor
  FROM TAUX_FUNCGROUPDET_det 
  where GDT_OPERADOR    = p_ope
    and gdt_codgru = ANIGROUP
    and gdt_operacion = p_cta; 
    return v_valor;
  end;						
end;
/
