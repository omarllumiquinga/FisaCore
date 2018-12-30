CREATE OR REPLACE
package body pkg_dom_intersucursal_imp is        
PROCEDURE InsertaLog(Mensaje in LOG_BATCH.TEXTO%TYPE,pFecha date) AS
BEGIN
  rollback;
  insert into log_batch
  values (substr(Mensaje,1,150));
  commit;
END InsertaLog;

PROCEDURE BorrarComprobantes (pFecha date,p_texto in varchar2) IS
 -- Cursor de transacciones existentes en contabilidad en la fecha de contabilizacion
 cursor c2(sucursal_tc in number,oficina_tc in number) is
    select distinct dst_sucursal,dst_numtran
      from tcon_destran,tcon_transa
     where dst_numtran = tsa_numtran
       and dst_sucursal = tsa_sucursal
       and dst_oficina = tsa_oficina
       and dst_fecha between pFecha and pFecha+.99999
       --and dst_liquida is not null
       --and dst_sucursal = sucursal_tc
       --and dst_oficina = oficina_tc
       and tsa_glosa LIKE '%'||upper(p_texto)||'%';--'%!FISA_AJUSTE_INTER_9018!%';
 errtxt varchar2(150);
 sucursal number:=0;
 Oficina number:=0;
 Err number:=1;
BEGIN
   --
  begin
      select bdf_valor
      into   sucursal
      from   tgen_def_batch
      where  bdf_cod='SUCURSAL_TC';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',pFecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,pFecha);
     Err:=1;

  end;
  begin
      select bdf_valor
      into   oficina
      from   tgen_def_batch
      where  bdf_cod='OFICINA_TC';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',pfecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,pFecha);
     Err:=1;

  end;
   -- Eliminar contabilización automática realizada en la misma fecha
   begin
     for regc2 in c2(sucursal, oficina) loop
       delete tcon_transa
        where tsa_sucursal = regc2.dst_sucursal and
              tsa_numtran  = regc2.dst_numtran;
     delete tcon_destran
      where dst_sucursal = regc2.dst_sucursal 
       and dst_numtran  = regc2.dst_numtran 
       and dst_fecha between pFecha and pFecha+.99999
       and dst_liquida is not null;
       commit;
     end loop;
     commit;
   exception when others then
     rollback;
     errtxt:=substr(sqlerrm,1,60);
     insert into log_batch
     values ('ERROR ELIMINANDO TRANSACCIONES '||errtxt);
     commit;
     --err:=1;
   end;
END BorrarComprobantes;
PROCEDURE InsDestran(
 pSucursal    tgen_sucursal.suc_codigo%Type,
 pOficina     tgen_oficina.ofi_codofi%Type,
 pFechcon     tcap_tramon.tmo_fechcon%Type,
 pDescrip     tcon_transa.tsa_glosa%Type,
 pDepto       tgen_desctabla.des_codigo%Type,
 pOperador    tgen_usuario.usr_codigo%Type,
 pSucOrg      tgen_sucursal.suc_codigo%Type,
 pOfiOrg      tgen_oficina.ofi_codofi%Type,
 pDeptOrg     tgen_desctabla.des_codigo%Type,
 pRubro       tgen_tranrubro.rub_rubro%Type,
 pModulo      tgen_tipmon.tpm_mod%Type,
 pProducto    tgen_tipmon.tpm_pro%Type,
 pTipo        tgen_tipmon.tpm_tip%TYpe,
 pMoneda      tgen_tipmon.tpm_mon%Type,
 pTransa      teve_trancont.eco_tra%Type,
 pVarAcr      number,
 pNumtran     in out tcon_destran.dst_numtran%Type,
 pError       in out number,
 pSecuencia   in out number,
 pSucDes      tgen_sucursal.suc_codigo%Type,
 pOfiDes      tgen_oficina.ofi_codofi%Type,
 pOrgTran      tgen_tranrubro.rub_afectacion%Type)
Is
 errtxt       varchar2(150);
BEGIN
  -- Asignar numero de comprobante
  Begin
   pNumtran:=upd_secdestran(pSucursal);
  Exception
    when no_data_found then
    InsertaLog('SECDESTRAN NOT FOUND '||to_char(pSucursal),pFechcon);
  End;
  -- Insertar Cabecera del Comprobante
  begin
    insert into tcon_destran
           (dst_numtran,dst_sucursal,dst_oficina,dst_fecha,
            dst_descripc,dst_depto,dst_mayor,dst_usuario,dst_hora,
            dst_terminal,dst_cuadra,dst_numref,dst_liquida, DST_TABDEPTO )
    values (pNumtran,pSucursal,pOficina,pFechcon,pDescrip,pDepto,'0',
            pOperador,to_char(pFechcon,'hh24:mi'),
            lpad(ltrim(to_char(pSucorg)),3,'0')||'-'||lpad(ltrim(to_char(pOfiorg)),2,'0')|| '-'||lpad(ltrim(to_char(pDepto)),4,'0')||'-'
            ||lpad(ltrim(to_char(pRubro)),2,'0')
            ||lpad(ltrim(to_char(pSucDes)),3,'0')||'-'||lpad(ltrim(to_char(pOfiDes)),2,'0')||nvl(pOrgTran,'D'),
            'S',null,null,40);

            --lpad(to_char(pModulo),2,'0'),
            --lpad(to_char(pOperador),4,'0')||'-'||lpad(to_char(pProducto),2,'0')||
            --lpad(to_char(pTipo),2,'0')||lpad(to_char(pMoneda),2,'0')||lpad(TO_CHAR(pVarAcr),1,'0'),40);
    pSecuencia := 0;       --secuencia:=0;
  exception when others then
    pError:= 1;
    errtxt:=substr(sqlerrm,1,130);
    rollback;
    InsertaLog('ERROR INSERTANDO COMPROBANTE: '||errtxt||' '||
               to_char(pOperador)||' '||to_char(pNumtran),pFechcon);
    commit;
  end;
END InsDestran;
PROCEDURE DefTrancontSuc(
 pModuloSuc      tgen_tipmon.tpm_mod%Type,
 pMoneda         tgen_tipmon.tpm_mon%Type,
 pTransaSuc      tint_trancont.int_tra%Type,
 pRubroSuc       tint_trancont.int_rubro%Type, 
 pCtadebSuc      in out tgen_trancont.con_ctadeb%Type,
 pCtahabSuc      in out tgen_trancont.con_ctahab%Type,
 pRubdesSuc      in out tgen_tranrubro.rub_desc%Type,
 pError          in out number,
 pFechcon        tcap_tramon.tmo_fechcon%Type)
IS
BEGIN
  -- Buscar cuentas contables definidas para transaccion
    select int_ctadeb ,int_ctahab ,cod_destra|| ' AJU INTERSUCURSAL' 
    into   pctadebSuc ,pctahabSuc ,pRubdesSuc
    from   tint_trancont,tgen_codtrans
    where  int_mod   = pModuloSuc
      and  int_tra   = pTransaSuc
      and  int_mon   = pMoneda
      and  int_fechahasta is null      
      and int_mod = cod_mod
      and int_tra = cod_tra
      and int_rubro =pRubroSuc ;
exception
 when  OTHERS then
  pError := 1;
  InsertaLog('CONTABILIZACION NO DEFINIDA  SUCURSAL revise 1/9017:modulo: '||
                to_char(pModuloSuc)||' Moneda:'||to_char(pMoneda)||' Transa:'||
                to_char(pTransasuc)||'^'||sqlerrm,pFechcon);
END DefTrancontSuc;
/*Para comprobantes intersucursales - se quiere que sea parametrico*/
PROCEDURE DefTrancontSucIs(
 pModuloSuc      tgen_tipmon.tpm_mod%Type,
 pMoneda         tgen_tipmon.tpm_mon%Type,
 pTransaSuc      tint_trancontis.int_tra%Type,
 pRubroSuc       tint_trancontis.int_rubro%Type, 
 pSecSuc         tint_trancontis.int_sec%Type,  
 pSucSuc         in out tint_trancontis.int_sucdest%Type,   
 pOfiSuc         in out tint_trancontis.int_Ofidest%Type,    
 pCtadebSuc      in out tgen_trancont.con_ctadeb%Type,
 pCtahabSuc      in out tgen_trancont.con_ctahab%Type,
 pRubdesSuc      in out tgen_tranrubro.rub_desc%Type,
 pError          in out number,
 pFechcon        tcap_tramon.tmo_fechcon%Type)
IS
BEGIN
  -- Buscar cuentas contables definidas para transaccion
    select int_ctadeb ,int_ctahab,int_sucdest,int_ofidest,cod_destra|| ' AJU INTERSUCURSAL' 
    into   pctadebSuc ,pctahabSuc,pSucSuc,pOfiSuc,pRubdesSuc
    from   tint_trancontis,tgen_codtrans
    where  int_mod   = pModuloSuc
      and  int_tra   = pTransaSuc
      and  int_mon   = pMoneda
      and  int_fechahasta is null      
      and  int_mod = cod_mod
      and  int_tra = cod_tra
      and  int_rubro =pRubroSuc 
      and  int_sec = pSecSuc;
exception
 when  OTHERS then
  pError := 1;
  InsertaLog('CONTABILIZACION NO DEFINIDA  SUCURSAL revise 1/9017:modulo: '||
                to_char(pModuloSuc)||' Moneda:'||to_char(pMoneda)||' Transa:'||
                to_char(pTransasuc)||'^'||sqlerrm,pFechcon);
END DefTrancontSucIS;

/*Cración de detalle del comprobante*/
PROCEDURE InsTransa(
 pNuntran     tcon_destran.dst_numtran%Type,
 pSucursal    tgen_sucursal.suc_codigo%Type,
 pOficina     tgen_oficina.ofi_codofi%Type,
 pCtadeb      tgen_trancont.con_ctadeb%Type,
 pCtahab      tgen_trancont.con_ctahab%Type,
 pRubdesc     tgen_tranrubro.rub_desc%Type,
 pValor       tcon_transa.tsa_valor%Type,
 pValorHB     tcon_transa.tsa_valor%Type,
 pValorme     tcon_transa.tsa_valorme%Type,
 pValormeHB   tcon_transa.tsa_valorme%Type,
 pCotizacion  tgen_monfecha.mof_promedio%Type,
 pRubro       tgen_tranrubro.rub_rubro%Type,
 pRubdes      tgen_tranrubro.rub_desc%Type,
 pModulo      tgen_tipmon.tpm_mod%Type,
 pProducto    tgen_tipmon.tpm_pro%Type,
 pTipo        tgen_tipmon.tpm_tip%TYpe,
 pMoneda      tgen_tipmon.tpm_mon%Type,
 pTransa      teve_trancont.eco_tra%Type,
 pError       in out number,
 pFechcon     tcap_tramon.tmo_fechcon%Type,
 pSecuencia   in out number,
 p_texto in varchar2)
IS
 errtxt       varchar2(150);
 v_esext varchar2(1):=1;
 pValormeHB_aux   tcon_transa.tsa_valorme%Type;
 pCotizacion_aux  tgen_monfecha.mof_promedio%Type;

BEGIN
   -- Insertar transacciones contables
   begin
     if pCtadeb is not null then
      pSecuencia:=pSecuencia+1;   --secuencia:=secuencia+1;
      insert into tcon_transa
             (tsa_numtran,tsa_sucursal,tsa_oficina,
              tsa_secuencia,tsa_cuenta,tsa_tipo,tsa_valor,
              tsa_valorme,tsa_cotiza,tsa_tabcosto,tsa_glosa)
      values (pNuntran,pSucursal,pOficina,pSecuencia,
              pCtadeb,'D',pValor,pValorme,pCotizacion,96,lpad(ltrim(to_char(pRubro)),2,'0')||'-'||pRubdesc||upper(p_texto));
     end if;
     pError:=0;
   exception when others then
     pError := 1;
     errtxt:=substr(sqlerrm,1,80);
     InsertaLog('TRANSACCION DEBITO: '||errtxt||' '||
                to_char(pModulo)||' '||to_char(pProducto)||' '||
                to_char(pTipo)||' '||to_char(pMoneda)||' '||to_char(pCotizacion)||' '||
                (pTransa)||' '||to_char(pRubro)||' '||pCtadeb,pFechcon);
--' pValor '||pValor||' VME '||pValorme,pFechcon);
   end;
pValormeHB_aux:=pValormeHB;
pCotizacion_aux:=pCotizacion;

    if pError = 0 and pCtahab is not null then   --if err = 0 then
     begin
       pSecuencia:=pSecuencia+1;   --secuencia:=secuencia+1;
        v_esext :=substr(pCtahab,6,1);
        if v_esext=1 then
			pValormeHB_aux:=null;
			pCotizacion_aux:=null;
        end if;
       insert into tcon_transa
              (tsa_numtran,tsa_sucursal,tsa_oficina,
               tsa_secuencia,tsa_cuenta,tsa_tipo,tsa_valor,
               tsa_valorme,tsa_cotiza,tsa_tabcosto,tsa_glosa)
       values (pNuntran,pSucursal,pOficina,pSecuencia,
               pCtahab,'C',pValorhb,pValormehb_aux,pCotizacion_aux,96,lpad(ltrim(to_char(pRubro)),2,'0')||'-'||pRubdesc||upper(p_texto));

     exception when others then
       pError:=1;
       errtxt:=substr(sqlerrm,1,10);
       errtxt:=substr(sqlerrm,1,80);
       InsertaLog('TRANSACCION CREDITO: '||errtxt||' '||
                  to_char(pModulo)||' '||to_char(pProducto)||' '||
                  to_char(pTipo)||' '||to_char(pMoneda)||' '||
                  (pTransa)||' '||to_char(pRubro)||' '||pCtahab,pFechcon);
     end;
   end if;
END InsTransa;
/*------------------------------------------------Fin Intersucursales mas parametrico ------------------------------------------*/
----------------------------------------------------------Fin intersucursales----------------------------------------------------------
----------------------------------------------------------para intersucursales----------------------------------------------------------
procedure mov_manual(p_operador in number,p_fecha in date) is
cursor c_ctarecibidos is
 select int_ctadeb
   from tint_trancont
  where int_mod = 3
    and int_tra = 15
    and int_rubro = 1
    and 1=2
    and int_fechahasta is null;
--en las transferencias que realiza el fin de dia
--la intersucursal  
cursor c_26_27 is
select
        a.tmo_fechcon,
        a.tmo_codusr codusr,
        a.tmo_numtra numtra,
        a.tmo_sec,
        b.tmo_codsuc sucorigen,
        b.tmo_codofi ofiorigen,
        a.tmo_codsuc succuenta,
        a.tmo_codofi oficuenta,
        b.tmo_numcue cuentaorigen,        
        a.tmo_numcue cuenta,
        a.tmo_codmon codmon,
        a.tmo_codmod codmod,
        a.tmo_codpro codpro,
        a.tmo_codtip codtip,
        b.tmo_codtra codtraorigen,        
        a.tmo_codtra codtra, 
        b.tmo_rubro codrubroorigen,        
        a.tmo_rubro codrubro,
        a.tmo_val valor
from tcap_tramon  a,(select tmo_fechcon, tmo_codusr,tmo_numtra,tmo_codsuc,tmo_codofi,tmo_codmon,tmo_numcue,tmo_codtra,tmo_rubro
                      from tcap_tramon
                     where tmo_fechcon = p_fecha
                       and tmo_modo = 'N'
                       and tmo_codmod = 4
                       and tmo_codtra = 26
                       and tmo_rubro = 1) b
where a.tmo_fechcon = b.tmo_fechcon
  and a.tmo_codusr = b.tmo_codusr
  and a.tmo_numtra = b.tmo_numtra                                                   
  and a.tmo_codmon = b.tmo_codmon
  and a.tmo_modo = 'N'
  and a.tmo_codmod = 4
  and a.tmo_codtra = 27
  and (a.tmo_codsuc <> b.tmo_codsuc
   or  a.tmo_codofi <> b.tmo_codofi)
  and a.tmo_rubro = 1 
  and 1=2
  order by a.tmo_fechcon;  

cursor c_chqrec(p_cuenta in varchar2,p_texto in varchar2) is
select
        1 sucorigen,
        1 ofiorigen,
        tsa_sucursal succuenta,
        tsa_oficina  oficuenta,
        ccb_moneda codmon,
        sum(decode(tsa_tipo,'D',TSA_VALOR,0)) - 
        sum(decode(tsa_tipo,'C',TSA_VALOR,0)) valor,
        sum(decode(tsa_tipo,'D',TSA_VALORME,0)) -
        sum(decode(tsa_tipo,'C',TSA_VALORME,0)) valorME
  from tcon_transa,tcon_destran,tcon_cuentas
 where     tsa_sucursal = dst_sucursal
       and tsa_numtran = dst_numtran
       and tsa_cuenta = ccb_codigo
       and tsa_cuenta =  p_cuenta
       and dst_fecha BETWEEN p_fecha and p_fecha + 0.9999
       and (tsa_sucursal,tsa_oficina) not in (select 1,1 from dual)  
       and 1=2
       --and tsa_glosa NOT LIKE '%'||upper(p_texto)||'%'       
  group by tsa_sucursal,tsa_oficina,ccb_moneda
  having sum(decode(tsa_tipo,'D',TSA_VALOR,0)) - 
         sum(decode(tsa_tipo,'C',TSA_VALOR,0)) > 0  ;
/*para movimientos de tpago*/
cursor c_ctaTpago_180 is
 select int_ctadeb
   from tint_trancont
  where int_mod = 4
    and int_tra = 180
    and int_rubro = 1
    and 1=2
    and int_fechahasta is null;                         
cursor c_tpago_180(p_cuenta in varchar2,p_texto in varchar2) is
select
        1 sucorigen,
        1 ofiorigen,
        tsa_sucursal succuenta,
        tsa_oficina  oficuenta,
        ccb_moneda codmon,
        sum(decode(tsa_tipo,'D',TSA_VALOR,0)) - 
        sum(decode(tsa_tipo,'C',TSA_VALOR,0)) valor,
        sum(decode(tsa_tipo,'D',TSA_VALORME,0)) -
        sum(decode(tsa_tipo,'C',TSA_VALORME,0)) valorME
  from tcon_transa,tcon_destran,tcon_cuentas
 where     tsa_sucursal = dst_sucursal
       and tsa_numtran = dst_numtran
       and tsa_cuenta = ccb_codigo
       and tsa_cuenta =  p_cuenta
       and dst_fecha BETWEEN p_fecha and p_fecha + 0.9999
       and (tsa_sucursal,tsa_oficina) not in (select 1,1 from dual) 
       and 1=2       
       --and tsa_glosa NOT LIKE '%'||upper(p_texto)||'%'       
  group by tsa_sucursal,tsa_oficina,ccb_moneda
  having sum(decode(tsa_tipo,'D',TSA_VALOR,0)) - 
         sum(decode(tsa_tipo,'C',TSA_VALOR,0)) > 0  ; 
         
cursor c_ctaTpago_181 is
 select int_ctahab --int_ctadeb
   from tint_trancont
  where int_mod = 4
    and int_tra = 181
    and int_rubro = 1
    and 1=2
    and int_fechahasta is null;                         
cursor c_tpago_181(p_cuenta in varchar2,p_texto in varchar2) is
select
        1 sucorigen,
        1 ofiorigen,
        tsa_sucursal succuenta,
        tsa_oficina  oficuenta,
        ccb_moneda codmon,
        sum(decode(tsa_tipo,'C',TSA_VALOR,0)) - 
        sum(decode(tsa_tipo,'D',TSA_VALOR,0)) valor,
        sum(decode(tsa_tipo,'C',TSA_VALORME,0)) -
        sum(decode(tsa_tipo,'D',TSA_VALORME,0)) valorME
  from tcon_transa,tcon_destran,tcon_cuentas
 where     tsa_sucursal = dst_sucursal
       and tsa_numtran = dst_numtran
       and tsa_cuenta = ccb_codigo
       and tsa_cuenta =  p_cuenta
       and dst_fecha BETWEEN p_fecha and p_fecha + 0.9999
       and 1=2
       and (tsa_sucursal,tsa_oficina) not in (select 1,1 from dual) 
       --and tsa_glosa NOT LIKE '%'||upper(p_texto)||'%'       
  group by tsa_sucursal,tsa_oficina,ccb_moneda
  having sum(decode(tsa_tipo,'C',TSA_VALOR,0)) - 
         sum(decode(tsa_tipo,'D',TSA_VALOR,0)) > 0  ;
/*La idea es que los balances de las cuentas en el caso del 0.15% yamilka mesa de ayuda 5912*/
cursor c_imp is
select
        a.tmo_fechcon,
        a.tmo_codusr codusr,
        a.tmo_numtra numtra,
        a.tmo_sec,
        b.tmo_codsuc SucTransa,
        b.tmo_codofi OfiTransa,
        a.tmo_succue succuenta,
        a.tmo_oficue oficuenta,
        b.tmo_numcue cuentaorigen,        
        a.tmo_numcue cuenta,
        a.tmo_codmon codmon,
        a.tmo_codmod codmod,
        a.tmo_codpro codpro,
        a.tmo_codtip codtip,
        b.tmo_codtra codtraorigen,        
        a.tmo_codtra codtra, 
        b.tmo_rubro codrubroorigen,        
        a.tmo_rubro codrubro,
        (select decode(RUB_CARGO,2,903,904) TraImp     
														from trep_tranxrep,tgen_tranrubro 
														WHERE TXR_MODREP= 1
														  AND TXR_TRAREP = 52
                              and TXR_CODMOD = rub_mod
                              and TXR_CODTRA = rub_tra
                              and TXR_RUBRO = rub_rubro
                              and rub_mod = a.tmo_codmod
                              and rub_tra = a.tmo_codtra
                              and rub_rubro = a.tmo_rubro
      ) Traimp,
        a.tmo_val valor
from tcap_tramon  a,(select tmo_fechcon, tmo_codusr,tmo_numtra,tmo_codsuc,tmo_codofi,tmo_codmon,tmo_numcue,tmo_codtra,tmo_rubro
                      from tcap_tramon
                     where tmo_fechcon = p_fecha
                       and tmo_modo = 'N'
                       and (tmo_codmod,tmo_codtra,tmo_rubro) IN (select TXR_CODMOD,TXR_CODTRA,TXR_RUBRO      
														from trep_tranxrep 
														WHERE TXR_MODREP= 1
														  AND TXR_TRAREP = 52)) b
where a.tmo_fechcon = b.tmo_fechcon
  and a.tmo_codusr = b.tmo_codusr
  and a.tmo_numtra = b.tmo_numtra                                                   
  and a.tmo_codmon = b.tmo_codmon
  and a.tmo_modo = 'N'
  and (a.tmo_codmod,a.tmo_codtra,a.tmo_rubro) IN (select TXR_CODMOD,TXR_CODTRA,TXR_RUBRO      
														from trep_tranxrep 
														WHERE TXR_MODREP= 1
														  AND TXR_TRAREP = 52)
  and (a.tmo_succue <> b.tmo_codsuc
   or  a.tmo_oficue <> b.tmo_codofi)
   --and 1=2
  order by a.tmo_fechcon;  

cursor c_cta_5912 is
 select int_tra,int_rubro,int_ctahab ,int_descrip--int_ctadeb
   from tint_trancont
  where int_mod = 3
    and int_tra in (900,901,902)
--    and int_rubro = 1
    and int_fechahasta is null
    order by int_tra,int_rubro;                         
cursor c_5912(p_cuenta in varchar2,p_texto in varchar2) is
select
        1 sucorigen,
        1 ofiorigen,
        tsa_sucursal succuenta,
        tsa_oficina  oficuenta,
        ccb_moneda codmon,
        sum(decode(tsa_tipo,'C',TSA_VALOR,0)) - 
        sum(decode(tsa_tipo,'D',TSA_VALOR,0)) valor,
        sum(decode(tsa_tipo,'C',TSA_VALORME,0)) -
        sum(decode(tsa_tipo,'D',TSA_VALORME,0)) valorME
  from tcon_transa,tcon_destran,tcon_cuentas
 where     tsa_sucursal = dst_sucursal
       and tsa_numtran = dst_numtran
       and tsa_cuenta = ccb_codigo
       and tsa_cuenta =  p_cuenta
       and dst_fecha BETWEEN p_fecha and p_fecha + 0.9999
       and (tsa_sucursal,tsa_oficina) not in (select 1,1 from dual) 
       --and tsa_glosa NOT LIKE '%'||upper(p_texto)||'%'       
  group by tsa_sucursal,tsa_oficina,ccb_moneda
  having sum(decode(tsa_tipo,'C',TSA_VALOR,0)) - 
         sum(decode(tsa_tipo,'D',TSA_VALOR,0)) > 0  ;

cursor c_cta_5912_uni is
 select distinct int_tra,int_rubro,int_ctadeb int_ctahab ,int_descrip--int_ctadeb
   from tint_trancont
  where int_mod = 3
    and int_tra in (905)
--    and int_rubro = 1
    and int_fechahasta is null
    order by int_tra,int_rubro;                         

cursor c_5912_uni(p_cuenta in varchar2,p_texto in varchar2) is
select
        1 sucorigen,
        1 ofiorigen,
        tsa_sucursal succuenta,
        tsa_oficina  oficuenta,
        ccb_moneda codmon,
        sum(decode(tsa_tipo,'C',TSA_VALOR,0)) - 
        sum(decode(tsa_tipo,'D',TSA_VALOR,0)) valor,
        sum(decode(tsa_tipo,'C',TSA_VALORME,0)) -
        sum(decode(tsa_tipo,'D',TSA_VALORME,0)) valorME
  from tcon_transa,tcon_destran,tcon_cuentas
 where     tsa_sucursal = dst_sucursal
       and tsa_numtran = dst_numtran
       and tsa_cuenta = ccb_codigo
       and tsa_cuenta =  p_cuenta
       and dst_fecha BETWEEN p_fecha and p_fecha + 0.9999
       and (tsa_sucursal,tsa_oficina)  in (select 1,1 from dual) 
       --and tsa_glosa NOT LIKE '%'||upper(p_texto)||'%'       
  group by tsa_sucursal,tsa_oficina,ccb_moneda
  having sum(decode(tsa_tipo,'C',TSA_VALOR,0)) - 
         sum(decode(tsa_tipo,'D',TSA_VALOR,0)) > 0  ;

/*para movimientos de td*/   

/*nuevo cambio solicitado por por merys 2013/10/22*/
cursor c_ctaTD is
 select int_ctahab
   from tint_trancont
  where int_mod = 4
    and int_tra = 72
    and int_rubro = 1     
      and 1=2
    and int_fechahasta is null;                         
cursor c_td(p_cuenta in varchar2,p_texto in varchar2) is    
select
        1 sucorigen,
        1 ofiorigen,
        tsa_sucursal succuenta,
        tsa_oficina  oficuenta,
        ccb_moneda codmon,
        sum(decode(tsa_tipo,'C',TSA_VALOR,0)) - 
        sum(decode(tsa_tipo,'D',TSA_VALOR,0)) valor,
        sum(decode(tsa_tipo,'D',TSA_VALORME,0)) - 
        sum(decode(tsa_tipo,'C',TSA_VALORME,0)) valorME                                
  from tcon_transa,tcon_destran,tcon_cuentas
 where     tsa_sucursal = dst_sucursal
       and tsa_numtran = dst_numtran
       and tsa_cuenta = ccb_codigo
       and tsa_cuenta =  p_cuenta
       --and dst_fecha BETWEEN p_fecha and p_fecha + 0.9999
       and dst_fecha <= p_fecha--BETWEEN p_fecha and p_fecha + 0.9999
       and (tsa_sucursal,tsa_oficina) not in (select 1,1 from dual) 
         and 1=2
       --and tsa_glosa NOT LIKE '%'||upper(p_texto)||'%'       
 group by tsa_sucursal,tsa_oficina,ccb_moneda
 having sum(decode(tsa_tipo,'C',TSA_VALOR,0)) - 
        sum(decode(tsa_tipo,'D',TSA_VALOR,0)) <> 0;
        
cursor c_tc(p_sucursal_tc in number) is
select
        a.tmo_fechcon,
        a.tmo_codusr codusr,
        a.tmo_numtra numtra,
        a.tmo_sec,
        a.tmo_codsuc succuenta,
        a.tmo_codofi oficuenta,
        a.tmo_numcue cuenta,
        a.tmo_codmon codmon,
        a.tmo_codmod codmod,
        a.tmo_codpro codpro,
        a.tmo_codtip codtip,
        a.tmo_codtra codtra,
        a.tmo_rubro codrubro,
        a.tmo_val valor
from tcap_tramon  a
where a.tmo_fechcon = p_fecha
  and a.tmo_modo = 'N'
  and a.tmo_codmod = 3
  and a.tmo_codtra = 4
  and a.tmo_codsuc <> p_sucursal_tc
    and 1=2
  and a.tmo_rubro in (34,35)
union
select
        a.tmo_fechcon,
        a.tmo_codusr codusr,
        a.tmo_numtra numtra,
        a.tmo_sec,
        a.tmo_codsuc succuenta,
        a.tmo_codofi oficuenta,
        a.tmo_numcue cuenta,
        a.tmo_codmon codmon,
        a.tmo_codmod codmod,
        a.tmo_codpro codpro,
        a.tmo_codtip codtip,
        a.tmo_codtra codtra,
        a.tmo_rubro codrubro,
        a.tmo_val valor
from tcap_tramon  a
where a.tmo_fechcon = p_fecha
  and a.tmo_modo = 'N'
  and a.tmo_codmod = 3
  and a.tmo_codtra = 803
  and a.tmo_codsuc <> p_sucursal_tc
  and a.tmo_rubro  = 1
   and 1=2
  order by tmo_fechcon;  
--Para Incoming Visa sobre la sucursal  
--se realiza un solo comprobante
cursor c_165 is
select distinct int_mod,int_tra,int_rubro,int_mon
from TINT_TRANCONT
where int_mod = 4
  and int_tra=165
    and 1=2
order by 1,2,3  ;
cursor c_165_31(p_mod in number,p_tra in number,p_rubro in number,p_moneda in number) is
select
        a.tmo_fechcon,
        a.tmo_codusr codusr,
        a.tmo_numtra numtra,
        a.tmo_sec,
        b.tmo_codsuc sucorigen,
        b.tmo_codofi ofiorigen,
        a.tmo_codsuc succuenta,
        a.tmo_codofi oficuenta,
        b.tmo_numcue cuentaorigen,        
        a.tmo_numcue cuenta,
        a.tmo_codmon codmon,
        a.tmo_codmod codmod,
        a.tmo_codpro codpro,
        a.tmo_codtip codtip,
        b.tmo_codtra codtraorigen,        
        a.tmo_codtra codtra, 
        b.tmo_rubro codrubroorigen,        
        a.tmo_rubro codrubro,
        a.tmo_val valor
from tcap_tramon  a,(select tmo_fechcon, tmo_codusr,tmo_numtra,tmo_codsuc,tmo_codofi,tmo_codmon,tmo_numcue,tmo_codtra,tmo_rubro
                      from tcap_tramon
                     where tmo_fechcon = p_fecha
                       and tmo_modo = 'N'
                       and tmo_codmod = p_mod
                       and tmo_codtra = p_tra
                       and tmo_rubro = p_rubro
                       and tmo_codmon = p_moneda) b
where a.tmo_fechcon = b.tmo_fechcon
  and a.tmo_codusr = b.tmo_codusr
  and a.tmo_numtra = b.tmo_numtra                                                   
  and a.tmo_codmon = b.tmo_codmon
  and a.tmo_modo = 'N'
  and a.tmo_codmod = p_mod
  and a.tmo_codtra =p_tra
  and a.tmo_rubro = p_rubro
  and a.tmo_codmon = p_moneda  
    and 1=2
  order by a.tmo_fechcon;  
--Pago de Cheque certificado Por Camara
--al contabilizar D, por la liquidacion 
--que se realiza desde la 1 1 se necesitan
--mover los saldos
cursor c_17_1 is
select
        a.tmo_fechcon,
        a.tmo_codusr codusr,
        a.tmo_numtra numtra,
        a.tmo_sec,
        b.tmo_codsuc sucorigen,
        b.tmo_codofi ofiorigen,
        a.tmo_codsuc succuenta,
        a.tmo_codofi oficuenta,
        b.tmo_numcue cuentaorigen,        
        a.tmo_numcue cuenta,
        a.tmo_codmon codmon,
        a.tmo_codmod codmod,
        a.tmo_codpro codpro,
        a.tmo_codtip codtip,
        b.tmo_codtra codtraorigen,        
        a.tmo_codtra codtra, 
        b.tmo_rubro codrubroorigen,        
        a.tmo_rubro codrubro,
        a.tmo_val valor
from tcap_tramon  a,(select tmo_fechcon, tmo_codusr,tmo_numtra,tmo_codsuc,tmo_codofi,tmo_codmon,tmo_numcue,tmo_codtra,tmo_rubro
                      from tcap_tramon
                     where tmo_fechcon = p_fecha
                       and tmo_modo = 'N'
                       and tmo_codmod = 4
                       and tmo_codtra = 17
                       and tmo_rubro = 1) b
where a.tmo_fechcon = b.tmo_fechcon
  and a.tmo_codusr = b.tmo_codusr
  and a.tmo_numtra = b.tmo_numtra                                                   
  and a.tmo_codmon = b.tmo_codmon
  and a.tmo_modo = 'N'
  and a.tmo_codmod = 4
  and a.tmo_codtra =17
  and a.tmo_rubro = 1
  and (a.tmo_succue,a.tmo_oficue) not in (select 1,1 from dual)
    and 1=2
  order by a.tmo_fechcon;  
    
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
  numtran  tcon_destran.dst_numtran%Type;
  /*Definición de variables    */
  sucursal    tgen_sucursal.suc_codigo%Type;
  moneda      tgen_moneda.mon_cod%Type;
  cotizacion  tgen_monfecha.mof_promedio%Type;
  cotiza      tgen_monfecha.mof_promedio%Type;
  oficina     tgen_oficina.ofi_codofi%Type;
  modulo      tgen_tipmon.tpm_mod%Type;
  producto    tgen_tipmon.tpm_pro%Type;
  tipo        tgen_tipmon.tpm_tip%Type;
  transa      teve_trancont.eco_tra%Type;
  rubro       tgen_tranrubro.rub_rubro%Type;
  valor       tcon_transa.tsa_valor%Type;
  valorhb     tcon_transa.tsa_valor%Type;
  valorme     tcon_transa.tsa_valorme%Type;
  valormehb  tcon_transa.tsa_valorme%Type;
  TipoCuenta  tgen_productos.prd_tippro%Type;
  TipoCuentaAux  tgen_productos.prd_tippro%Type;
  Descripc    tcon_transa.tsa_glosa%Type;
  Ctadeb      tgen_trancont.con_ctadeb%Type;
  Ctahab      tgen_trancont.con_ctahab%Type;
  Caja        tgen_tranrubro.rub_caja%Type;
  sAfectacion tgen_tranrubro.rub_afectacion%Type;
  Rubdesc     tgen_tranrubro.rub_desc%Type;
  TipAcr      number:=0;
  errtxt      varchar2(100);
  operador    tgen_usuario.usr_codigo%Type;
  Depto       number;
  /*Intersucursale variables */
  moduloSuc   tgen_tipmon.tpm_mod%Type;
  productoSuc tgen_tipmon.tpm_pro%Type;
  tipoSuc     tgen_tipmon.tpm_tip%Type;
  transaSuc   tgen_tranrubro.rub_tra%Type;
  rubroSuc    tgen_tranrubro.rub_rubro%Type;
  CtadebSuc   tgen_trancont.con_ctadeb%Type;
  CtahabSuc   tgen_trancont.con_ctahab%Type;
  CtadebSucN  tgen_trancont.con_ctadeb%Type;
  CtahabSucN  tgen_trancont.con_ctahab%Type;
  RubdesSuc   tgen_tranrubro.rub_desc%Type;
  cabece      number;
  secuencia   number;
  SucOrg        tgen_sucursal.suc_codigo%Type;
  OfiOrg        tgen_oficina.ofi_codofi%Type;
  SucDes        tgen_sucursal.suc_codigo%Type;
  OfiDes        tgen_oficina.ofi_codofi%Type;
  FlagContMonNac tgen_tranrubro.rub_posicion%Type;
  pMonCtaCon    number;
  sFlagContAltr tgen_tranrubro.rub_contabaltr%Type;
  Err Number;
  pErr Number;

  
 begin
  --BorrarComprobantes (p_Fecha,'!FISA_AJUSTE_INTER_9018!');
  --delete from log_batch;  
  --commit;     
  --viende upd_succue
/*----------------------------------------para intersucursales tc-----------------------------------------------------------------*/
/*Ajustes Manuales TC																																  */
/*--------------------------------------------------------------------------------------------------------------------------------*/
  begin
      select bdf_valor
      into   sucursal
      from   tgen_def_batch
      where  bdf_cod='SUCURSAL_TC';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,p_Fecha);
     Err:=1;

  end;
    begin
      select bdf_valor
      into   oficina
      from   tgen_def_batch
      where  bdf_cod='OFICINA_TC';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,p_Fecha);
     Err:=1;

  end;
  begin
      select bdf_valor
      into   operador
      from   tgen_def_batch
      where  bdf_cod='OPERADOR';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,p_Fecha);
     Err:=1;

  end;

  --BorrarComprobantes(p_Fecha,'!FISA_AJUSTE_INTER_9018!');
  delete from log_batch;       
  begin
      select pcb_depto
      into   depto
      from   tcon_param
      where  pcb_hasta is null;
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS CONTABILIDAD NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:20-9',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS CONTABILIDAD '||errtxt,p_Fecha);
     Err:=1;

  end;
  
  commit;
    moneda  :=0;
    modulo  := 8;
    dbms_output.put_line('************inicio***************');  
    for reg1 in c_tc(sucursal) loop
        cabece   := 1;
        depto    := depto;
        transa   := reg1.codtra;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
     moneda:=reg1.codmon;
        --
        modulo   := reg1.codmod;
        producto:=reg1.codpro;
        tipo:=reg1.codtip;
        rubro:=reg1.codrubro;
        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL TARJETA CREDITO USUARIO:'||reg1.codusr||' Numtra:'||reg1.numtra||' codtra:'||reg1.codtra||' rubro:'||reg1.codrubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,1,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor*cotizacion;
          valorhb:=reg1.valor*cotizacion;
          valorme:=reg1.valor;
          valormehb:=reg1.valor;
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
    end loop; -- Fin loop de transacciones
   --comprobante de la sucursal 4
  begin
      select bdf_valor
      into   sucursal
      from   tgen_def_batch
      where  bdf_cod='SUCURSAL_TC';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,p_Fecha);
     Err:=1;

  end;
    begin
      select bdf_valor
      into   oficina
      from   tgen_def_batch
      where  bdf_cod='OFICINA_TC';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,p_Fecha);
     Err:=1;

  end;
    for reg1 in c_tc(sucursal) loop
        cabece   := 1;
        depto    := depto;
        transa   := reg1.codtra;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
     moneda:=reg1.codmon;
        --
        modulo   := reg1.codmod;
        producto:=reg1.codpro;
        tipo:=reg1.codtip;
        rubro:=reg1.codrubro;
        sucdes:=sucursal;
        ofides:=oficina;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL TARJETA CREDITO USUARIO:'||reg1.codusr||' Numtra:'||reg1.numtra||' codtra:'||reg1.codtra||' rubro:'||reg1.codrubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,2,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor*cotizacion;
          valorhb:=reg1.valor*cotizacion;
          valorme:=reg1.valor;
          valormehb:=reg1.valor;
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
    end loop; -- Fin loop de transacciones
    --fin de comprobante de la sucursal 4      
/*---------------------------------------------Para incommig 4-165-31------------------------------------------------------------*/
/*Ajustes Manuales Por chq recibidos																						     */
/*-------------------------------------------------------------------------------------------------------------------------------*/
    moneda  :=0;
    modulo  := 4;
    dbms_output.put_line('************inicio***************'); 
    for reg0 in c_165 loop 
    for reg1 in c_165_31(reg0.int_mod,reg0.int_tra,reg0.int_rubro,reg0.int_mon) loop
        cabece   := 1;
        depto    := depto;
        transa   := reg1.codtra;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
     moneda:=reg1.codmon;
        --
        modulo   := reg1.codmod;
        producto:=reg1.codpro;
        tipo:=reg1.codtip;
        rubro:=reg1.codrubro;
        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL INCOMMIN165 USUARIO:'||reg1.codusr||' Numtra:'||reg1.numtra||' codtra:'||reg1.codtra||' rubro:'||reg1.codrubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor*cotizacion;
          valorhb:=reg1.valor*cotizacion;
          valorme:=reg1.valor;
          valormehb:=reg1.valor;
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
    end loop; -- Fin loop de transacciones
   end loop;
/*----------------------------------------Para certificacion de Cheque-----------------------------------------------------------*/
/*para cuando se certifica el cheque   																					         */
/*-------------------------------------------------------------------------------------------------------------------------------*/
    moneda  :=0;
    modulo  := 4;
    dbms_output.put_line('************inicio***************');  
    for reg1 in c_17_1 loop
        cabece   := 1;
        depto    := depto;
        transa   := reg1.codtra;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
     moneda:=reg1.codmon;
        --
        modulo   := reg1.codmod;
        producto:=reg1.codpro;
        tipo:=reg1.codtip;
        rubro:=reg1.codrubro;
        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL CHEQCERT17 USUARIO:'||reg1.codusr||' Numtra:'||reg1.numtra||' codtra:'||reg1.codtra||' rubro:'||reg1.codrubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor*cotizacion;
          valorhb:=reg1.valor*cotizacion;
          valorme:=reg1.valor;
          valormehb:=reg1.valor;
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
    end loop; -- Fin loop de transacciones  
  --fin de upd_succue  
/*----------------------------------------Para cheques Recibidos RD y US---------------------------------------------------------*/
/*Ajustes Manuales Por chq recibidos																						     */
/*-------------------------------------------------------------------------------------------------------------------------------*/
--PESOS
  begin
      select bdf_valor
      into   operador
      from   tgen_def_batch
      where  bdf_cod='OPERADOR';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,p_Fecha);
     Err:=1;

  end;

  --BorrarComprobantes (p_Fecha);
  --delete from log_batch;       
  begin
      select pcb_depto
      into   depto
      from   tcon_param
      where  pcb_hasta is null;
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS CONTABILIDAD NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:20-9',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS CONTABILIDAD '||errtxt,p_Fecha);
     Err:=1;

  end;                      
  
  commit;
    moneda  :=0;
    modulo  := 4;
 for x in c_ctarecibidos   loop
    for reg1 in c_chqrec(x.int_ctadeb,'!FISA_AJUSTE_INTER_9018!') loop
        cabece   := 1;
        depto    := depto;
        moneda:=reg1.codmon;
        --
        modulo   := 3; --reg1.codmod;
        transa   := 15;--reg1.codtra;        
        rubro:= 2;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL CHQRECIBIDOS USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha-1);--para el ajuste se toma la cotizacion del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valorme*cotizacion;   --El valor que tomo ya es en pesos
          valorhb:=reg1.valorme*cotizacion; 
          valorme:=reg1.valorme;--El valor que tomo ya es en pesos
          valormehb:=reg1.valorme;--El valor que tomo ya es en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
      --registro de la sucursal uno
        cabece   := 1;
        depto    := depto;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
        moneda:=reg1.codmon;
        --
        modulo := 3;
        transa := 15;        
        rubro  :=1;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.sucorigen;--sucursal;
        ofides:=reg1.ofiorigen; --oficina;
        Sucursal:= reg1.sucorigen;
        Oficina:= reg1.ofiorigen;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL CHQRECIBIDOS USUARIO:'||operador||' Numtra:'||transa||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha-1);--se toma del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valorme*cotizacion;
          valorhb:=reg1.valorme*cotizacion;
          valorme:=reg1.valorme; --Se divide porque se toma el valor en pesos
          valormehb:=reg1.valorme;--Se divide porque se toma el valor en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
      --fin registros de la sucursal uno
      
    end loop; -- Fin loop de transacciones
   --comprobante de la sucursal 4
    end loop;--c_chqrecibidos    
--fin  chequers recibidos    
/*-----------------------------TPAGO-------------------*/
    moneda  :=0;
    modulo  := 4;
 for x in c_ctatpago_180   loop
    for reg1 in c_tpago_180(x.int_ctadeb,'!FISA_AJUSTE_INTER_9018!') loop
        cabece   := 1;
        depto    := depto;
        moneda:=reg1.codmon;
        --
        modulo   := 4; --reg1.codmod;
        transa   := 180;--reg1.codtra;        
        rubro:= 2;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL MANTPAGO USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha-1);--para el ajuste se toma la cotizacion del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valorme*cotizacion;   --El valor que tomo ya es en pesos
          valorhb:=reg1.valorme*cotizacion; 
          valorme:=reg1.valorme;--El valor que tomo ya es en pesos
          valormehb:=reg1.valorme;--El valor que tomo ya es en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
      --registro de la sucursal uno
        cabece   := 1;
        depto    := depto;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
        moneda:=reg1.codmon;
        --
        modulo := 4;
        transa := 180;        
        rubro  :=1;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.sucorigen;--sucursal;
        ofides:=reg1.ofiorigen; --oficina;
        Sucursal:= reg1.sucorigen;
        Oficina:= reg1.ofiorigen;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL MANTPAGO USUARIO:'||operador||' Numtra:'||transa||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha-1);--se toma del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valorme*cotizacion;
          valorhb:=reg1.valorme*cotizacion;
          valorme:=reg1.valorme; --Se divide porque se toma el valor en pesos
          valormehb:=reg1.valorme;--Se divide porque se toma el valor en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
      --fin registros de la sucursal uno
      
    end loop; -- Fin loop de transacciones
   --comprobante de la sucursal 4
    end loop;
    --181    
    moneda  :=0;
    modulo  := 4;
 for x in c_ctatpago_181   loop
    for reg1 in c_tpago_181(x.int_ctahab,'!FISA_AJUSTE_INTER_9018!') loop
        cabece   := 1;
        depto    := depto;
        moneda:=reg1.codmon;
        --
        modulo   := 4; --reg1.codmod;
        transa   := 180;--reg1.codtra;        
        rubro:= 1;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL MANTPAGO USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha-1);--para el ajuste se toma la cotizacion del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valorme*cotizacion;   --El valor que tomo ya es en pesos
          valorhb:=reg1.valorme*cotizacion; 
          valorme:=reg1.valorme;--El valor que tomo ya es en pesos
          valormehb:=reg1.valorme;--El valor que tomo ya es en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
      --registro de la sucursal uno
        cabece   := 1;
        depto    := depto;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
        moneda:=reg1.codmon;
        --
        modulo := 4;
        transa := 180;        
        rubro  :=2;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.sucorigen;--sucursal;
        ofides:=reg1.ofiorigen; --oficina;
        Sucursal:= reg1.sucorigen;
        Oficina:= reg1.ofiorigen;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL MANTPAGO USUARIO:'||operador||' Numtra:'||transa||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha-1);--se toma del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valorme*cotizacion;
          valorhb:=reg1.valorme*cotizacion;
          valorme:=reg1.valorme; --Se divide porque se toma el valor en pesos
          valormehb:=reg1.valorme;--Se divide porque se toma el valor en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
      --fin registros de la sucursal uno
      
    end loop; -- Fin loop de transacciones
   --comprobante de la sucursal 4
    end loop;    
    --FIN 181
/*--------------------FIN TPAGO------------------------*/
/*----------------------------------------Para tarjeta de debito-----------------------------------------------------------------*/
/*Ajustes Manuales Para Tarjeta de Debito																					     */
/*-------------------------------------------------------------------------------------------------------------------------------*/
--PESOS
  begin
      select bdf_valor
      into   operador
      from   tgen_def_batch
      where  bdf_cod='OPERADOR';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,p_Fecha);
     Err:=1;

  end;

  --BorrarComprobantes (p_Fecha);
  --delete from log_batch;       
  begin
      select pcb_depto
      into   depto
      from   tcon_param
      where  pcb_hasta is null;
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS CONTABILIDAD NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:20-9',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS CONTABILIDAD '||errtxt,p_Fecha);
     Err:=1;

  end;                      
  
  commit;
    moneda  :=0;
    modulo  := 4;
 for x in c_ctatd   loop
    for reg1 in c_td(x.int_ctahab,'!FISA_AJUSTE_INTER_9018!') loop
        cabece   := 1;
        depto    := depto;
        moneda:=reg1.codmon;
        --
        modulo   := 4; --reg1.codmod;
        transa   := 72;--reg1.codtra;        
        rubro:= 2;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL TARJETADEBITO USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
      dbms_output.put_line('paso 3');  
      if reg1.valor > 0 then
         rubro:= 2;        
         DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      else                 
        rubro:= 1;              
         DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      end if;
      dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha-1);
        if cotizacion = 1 then
          valor:=ABS(reg1.valor);--ESTABA SOLO reg1.valor
          valorhb:=ABS(reg1.valor);
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=ABS(reg1.valorme)*cotizacion;
          valorhb:=ABS(reg1.valorme)*cotizacion;
          valorme:=reg1.valorme; --el valor que se toma ya es en pesos
          valormehb:=reg1.valorme;--el valor que se toma ya es en pesos
        end if;
      else
        valor:=ABS(reg1.valor);
        valorhb:=ABS(reg1.valor);
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');

      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;             
    --para registro sucursal uno
        cabece   := 1;
        depto    := depto;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
        moneda:=reg1.codmon;
        --
        modulo := 4;
        transa := 72;        
        rubro  :=1;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.sucorigen;--sucursal;
        ofides:=reg1.ofiorigen; --oficina;
        Sucursal:= reg1.sucorigen;
        Oficina:= reg1.ofiorigen;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL TARJETADEBITO USUARIO:'||operador||' Numtra:'||transa||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      if reg1.valor > 0 then
         rubro  :=1;
         DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      else          
        rubro  :=2;
        DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      end if;
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha-1);
        if cotizacion = 1 then
          valor:=ABS(reg1.valor);
          valorhb:=ABS(reg1.valor);
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=ABS(reg1.valorme)*cotizacion;
          valorhb:=ABS(reg1.valorme)*cotizacion;
          valorme:=ABS(reg1.valorme);   --Ya se toma viene el valor en pesos
          valormehb:=ABS(reg1.valorme); --Ya se toma el valor en pesos
        end if;
      else
        valor:=ABS(reg1.valor);
        valorhb:=ABS(reg1.valor);
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');

      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
      
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
    --fin registro sucursal uno      
    end loop; -- Fin loop de transacciones
   --comprobante de la sucursal 4
    end loop;--c_tdctas

--fin de TD
/*----------------------------------------Para Transferencia---------------------------------------------------------------------*/
/*Ajustes Manuales Por 4-26 y 4-27																						         */
/*-------------------------------------------------------------------------------------------------------------------------------*/
  begin
      select bdf_valor
      into   operador
      from   tgen_def_batch
      where  bdf_cod='OPERADOR';
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS BATCH '||errtxt,p_Fecha);
     Err:=1;

  end;

  --BorrarComprobantes (p_Fecha);
  --delete from log_batch;       
  begin
      select pcb_depto
      into   depto
      from   tcon_param
      where  pcb_hasta is null;
      -- dbms_output.put_line('DESPUES DE LEER PARAMETROS');
  exception
    when no_data_found then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS CONTABILIDAD NO DEFINIDOS, POR FAVOR REVISE EN LA TRAN:20-9',p_Fecha);
     Err:=1;
    when others then
     errtxt:=substr(sqlerrm,1,60);
     InsertaLog('PARAMETROS CONTABILIDAD '||errtxt,p_Fecha);
     Err:=1;

  end;
  
  commit;
    moneda  :=0;
    modulo  := 4;
    dbms_output.put_line('************inicio***************');  
    for reg1 in c_26_27 loop
        cabece   := 1;
        depto    := depto;
        transa   := reg1.codtra;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
     moneda:=reg1.codmon;
        --
        modulo   := reg1.codmod;
        producto:=reg1.codpro;
        tipo:=reg1.codtip;
        rubro:=reg1.codrubro;
        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL TRXAAUTBATCH USUARIO:'||reg1.codusr||' Numtra:'||reg1.numtra||' codtra:'||reg1.codtra||' rubro:'||reg1.codrubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,1,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor*cotizacion;
          valorhb:=reg1.valor*cotizacion;
          valorme:=reg1.valor;
          valormehb:=reg1.valor;
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
    end loop; -- Fin loop de transacciones
   --comprobante de la sucursal 4
    for reg1 in c_26_27 loop
        cabece   := 1;
        depto    := depto;
        transa   := reg1.codtraorigen;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
     moneda:=reg1.codmon;
        --
        modulo   := reg1.codmod;
        producto:=reg1.codpro;
        tipo:=reg1.codtip;
        rubro:=reg1.codrubroorigen;
        sucdes:=reg1.sucorigen;--sucursal;
        ofides:=reg1.ofiorigen; --oficina;
        Sucursal:= reg1.sucorigen;
        Oficina:= reg1.ofiorigen;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL TRXAAUTBATCH USUARIO:'||reg1.codusr||' Numtra:'||reg1.numtra||' codtra:'||reg1.codtraorigen||' rubro:'||reg1.codrubroorigen;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,1,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor*cotizacion;
          valorhb:=reg1.valor*cotizacion;
          valorme:=reg1.valor;
          valormehb:=reg1.valor;
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
    end loop; -- Fin loop de transacciones    
--fin trx 4-26-4-27      
--PARA 10% Y 1% CUANDO CANCELAN EN UNA SUCURSAL A LA QUE NO PERTENECE
    moneda  :=0;
    modulo  := 5;
    dbms_output.put_line('************inicio***************');  
    for reg1 in c_imp loop
        cabece   := 1;
        depto    := depto;
        transa   := reg1.codtra;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
     moneda:=reg1.codmon;
        --
        modulo   := reg1.codmod;
        producto:=reg1.codpro;
        tipo:=reg1.codtip;
        rubro:=reg1.codrubro;
        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL COBRO IMPUESTO USUARIO:'||reg1.codusr||' Numtra:'||reg1.numtra||' codtra:'||reg1.codtra||' rubro:'||reg1.codrubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      --DefTrancontsuc(modulo,Moneda,Transa,1,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      DefTrancontsuc(3,Moneda,reg1.TraImp,2,Ctahab,Ctadeb,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor*cotizacion;
          valorhb:=reg1.valor*cotizacion;
          valorme:=reg1.valor;
          valormehb:=reg1.valor;
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
    end loop; -- Fin loop de transacciones
   --comprobante de la sucursal 4
    for reg1 in c_imp loop
        cabece   := 1;
        depto    := depto;
        transa   := reg1.codtraorigen;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
     moneda:=reg1.codmon;
        --
        modulo   := reg1.codmod;
        producto:=reg1.codpro;
        tipo:=reg1.codtip;
        rubro:=reg1.codrubroorigen;
        sucdes:=reg1.sucTransa;--sucursal;
        ofides:=reg1.ofiTransa; --oficina;
        Sucursal:= reg1.sucTransa;
        Oficina:= reg1.ofiTransa;
		numtran:=0;
		Descripc:= 'COMPROBANTE INTERSUCURSAL COBRO IMPUESTO USUARIO:'||reg1.codusr||' Numtra:'||reg1.numtra||' codtra:'||reg1.codtraorigen||' rubro:'||reg1.codrubroorigen;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      --DefTrancontsuc(modulo,Moneda,Transa,1,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      DefTrancontsuc(3,Moneda,reg1.Traimp,2,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor*cotizacion;
          valorhb:=reg1.valor*cotizacion;
          valorme:=reg1.valor;
          valormehb:=reg1.valor;
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
    end loop; -- Fin loop de transacciones    
--fin trx 4-26-4-27                       

--FIN DE 10% Y 1%
/* para mesa de ayuda 5912*/       
    moneda  :=0;
    modulo  := 4;
 for x in c_cta_5912   loop
    for reg1 in c_5912(x.int_ctahab,'!FISA_AJUSTE_INTER_9018!') loop
        cabece   := 1;
        depto    := depto;
        moneda:=reg1.codmon;
        --
        modulo   := 3; --reg1.codmod;
        transa   := x.int_tra;        
        rubro:= x.int_rubro;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= x.int_descrip||' USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro;--'COMPROBANTE INTERSUCURSAL del 0.15%,1%,10% USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro;  
		if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      Rubdesc:= x.int_descrip;
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);-- antes estaba -1 para el ajuste se toma la cotizacion del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor;--reg1.valorme*cotizacion;   --El valor que tomo ya es en pesos
          valorhb:=reg1.valor;--reg1.valorme*cotizacion; para evitar diferencias 
          valorme:=reg1.valorme;--El valor que tomo ya es en pesos
          valormehb:=reg1.valorme;--El valor que tomo ya es en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctahab,Ctadeb,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
      --registro de la sucursal uno
        cabece   := 1;
        depto    := depto;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
        moneda:=reg1.codmon;
        --
        modulo := 3;
        transa := x.int_tra;        
        rubro  :=x.int_rubro;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.sucorigen;--sucursal;
        ofides:=reg1.ofiorigen; --oficina;
        Sucursal:= reg1.sucorigen;
        Oficina:= reg1.ofiorigen;
		numtran:=0;
		Descripc:= x.int_descrip||' USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro; --'COMPROBANTE INTERSUCURSAL del 0.15%,1%,10% USUARIO:'||operador||' Numtra:'||transa||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      Rubdesc:= x.int_descrip;
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);--se toma del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor;--reg1.valorme*cotizacion; para evitar diferencias
          valorhb:=reg1.valor;--reg1.valorme*cotizacion;
          valorme:=reg1.valorme; --Se divide porque se toma el valor en pesos
          valormehb:=reg1.valorme;--Se divide porque se toma el valor en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
      --fin registros de la sucursal uno
      
    end loop; -- Fin loop de transacciones
   --comprobante de la sucursal 4
    end loop;    
/* fin mesa de ayuda 5912*/
  --unif       
    moneda  :=0;
    modulo  := 4;
 for x in c_cta_5912_uni   loop
    for reg1 in c_5912_uni(x.int_ctahab,'!FISA_AJUSTE_INTER_9018!') loop
        cabece   := 1;
        depto    := depto;
        moneda:=reg1.codmon;
        --
        modulo   := 3; --reg1.codmod;
        transa   := x.int_tra;        
        rubro:= x.int_rubro;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.succuenta;--sucursal;
        ofides:=reg1.oficuenta; --oficina;
        Sucursal:= reg1.succuenta;
        Oficina:= reg1.oficuenta;
		numtran:=0;
		Descripc:= x.int_descrip||' USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro;--'COMPROBANTE INTERSUCURSAL del 0.15%,1%,10% USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro;  
		if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      Rubdesc:= x.int_descrip;
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);-- antes estaba -1 para el ajuste se toma la cotizacion del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor;--reg1.valorme*cotizacion;   --El valor que tomo ya es en pesos
          valorhb:=reg1.valor;--reg1.valorme*cotizacion; para evitar diferencias 
          valorme:=reg1.valorme;--El valor que tomo ya es en pesos
          valormehb:=reg1.valorme;--El valor que tomo ya es en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;
      --registro de la sucursal uno
        /*cabece   := 1;
        depto    := depto;
        --PARA MONEDA 1/117
        --moneda   := reg1.EVT_NROORGANIZACION;
     --begin  
        moneda:=reg1.codmon;
        --
        modulo := 3;
        transa := x.int_tra;        
        rubro  :=x.int_rubro;        
        producto:=2;--reg1.codpro;
        tipo:=1;--reg1.codtip;

        sucdes:=reg1.sucorigen;--sucursal;
        ofides:=reg1.ofiorigen; --oficina;
        Sucursal:= reg1.sucorigen;
        Oficina:= reg1.ofiorigen;
		numtran:=0;
		Descripc:= x.int_descrip||' USUARIO:'||operador||' Numtra:'||numtran||' codtra:'||transa||' rubro:'||rubro; --'COMPROBANTE INTERSUCURSAL del 0.15%,1%,10% USUARIO:'||operador||' Numtra:'||transa||' codtra:'||transa||' rubro:'||rubro;
      if cabece = 1 then       --Inserta la cabecera del comprobante
            --VarAcr := 0;  -- Indica que la transacción proviene de Tcap_Tramon
        InsDestran(sucursal,Oficina,p_Fecha,Descripc,Depto,
        operador,Sucursal,Oficina,Depto,rubro,modulo,producto,
        tipo,moneda,transa,0,numtran,pErr,secuencia,SucDes,OfiDes,'O');
       dbms_output.put_line('paso 2:'||pErr);
        if pErr = 1 then
          exit;
        end if;
      end if; -- cabecera
      --Define la cuentas contables
             dbms_output.put_line('paso 3');
      DefTrancontsuc(modulo,Moneda,Transa,rubro,Ctadeb,Ctahab,Rubdesc,pErr,p_Fecha);
      Rubdesc:= x.int_descrip;
             dbms_output.put_line('paso 4');
        if pErr = 1 then
          exit;
        end if;
      --
      if moneda <> 0 then
          cotizacion:=bdi_promedio(moneda,sucursal,p_fecha);--se toma del dia anterior
        if cotizacion = 1 then
          valor:=reg1.valor;
          valorhb:=reg1.valor;
          valorme:=null;
          valormehb:=null;
          --cotizacion:=null;
        else
          valor:=reg1.valor;--reg1.valorme*cotizacion; para evitar diferencias
          valorhb:=reg1.valor;--reg1.valorme*cotizacion;
          valorme:=reg1.valorme; --Se divide porque se toma el valor en pesos
          valormehb:=reg1.valorme;--Se divide porque se toma el valor en pesos
        end if;
      else
        valor:=reg1.valor;
        valorhb:=reg1.valor;
        valorme:=null;
        valormehb:=null;
        cotizacion:=null;
      end if;
      --
             dbms_output.put_line('paso 5');
      InsTransa(numtran,sucursal,oficina,Ctadeb,Ctahab,Rubdesc,Valor,Valorhb,Valorme,ValormeHB,Cotizacion,
      Rubro,Rubdesc,modulo,producto,tipo,moneda,transa,pErr,p_Fecha,secuencia,'!FISA_AJUSTE_INTER_9018!');
             dbms_output.put_line('paso 6');
      if pErr = 1 then
        exit;
      end if;*/
      --fin registros de la sucursal uno
      
    end loop; -- Fin loop de transacciones
   --comprobante de la sucursal 4
    end loop;    
/* fin mesa de ayuda 5912*/  
  --fin de unif

    fechainiciocol := SYSDATE;
    COMMIT; 
end;
----------------------------------------------------------Fin intersucursales----------------------------------------------------------

END;
/
