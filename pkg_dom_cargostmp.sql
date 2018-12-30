CREATE OR REPLACE
PACKAGE BODY pkg_dom_cargostmp AS
  PROCEDURE carga_informadiaria(
   operador      in number,
   p_fecha       in  date ) is
   cursor t is
  select tmo_fechcon,tmo_codusr,tmo_numtra,tmo_sec,tmo_fechproc,tmo_codmod,tmo_codpro,
         tmo_codtip,tmo_codmon,rub_tra tmo_codtra,rub_rubro tmo_rubro,tmo_numcue,tmo_val,
         rub_cargo tmo_cargo,tmo_depart,tmo_ref,tmo_diferido,tmo_cotiza,dcc_natural,dcc_juridico,
         TCC_NUMMAXEXO tmo_nummaxexo
    from tcap_tramon a,(     select rub_mod,rub_tra,rub_rubro,rub_desc,rub_cargo,
                                    rub_rubroref,tcc_modaplica,tcc_traplica,tcc_rubroaplica,
                                    dcc_natural,dcc_juridico,TCC_NUMMAXEXO
                               from tgen_tranrubro,tdom_cargosxcobrarbatch,tdom_tranorgxcargosxcob,tgen_cargos
                              where rub_cargo = dcc_codcargo
                                and dcc_codcargo = tcc_codcargo
                                and crg_codigo =  dcc_codcargo
                                and RUB_TIPVAL = 4
                                and nvl(crg_modulo,0) =  4--solo Para Vista
                                and rub_habilitado is null) b--definir estos cargos no habilitados para que no interfieran con la funcionalidad diaria
    where a.tmo_fechcon = p_fecha
      and a.tmo_codmod = b.tcc_modaplica
      and a.tmo_codtra = b.tcc_traplica
      and a.tmo_rubro = b.tcc_rubroaplica
      and a.tmo_modo = 'N';

  
         
  cargo  NUMBER(18,6);
  sobregiro VARCHAR2(1);
  abierto   VARCHAR2(1);
  v_commit varchar2(1);
  efectivo number;
  secuencia NUMBER(7);
  SEC NUMBER(5);
  errtxt    char(80);
  cargoloquehay NUMBER(18,6);
  w_tipoper number;
  w_fechaDesde date;
  w_numreg number:=0;
  begin
   for x in t loop
    cargo := 0;
    sobregiro := 'N';
    abierto:='N';
    v_commit := 'S';
    efectivo :=0;
    secuencia:=0;
    SEC :=0;
    cargoloquehay:=0; --Para cobrarle lo que tenga
   begin   
   w_fechaDesde:=null;
    select NVL(vis_fechultcor+1,vis_fechape), cli_tipoper
      into w_fechaDesde,w_tipoper
      from tcap_vista,tcli_persona
     where vis_numcue = x.tmo_numcue
       and vis_codcli=cli_codigo;
  exception
    when others then
      v_commit := 'N';
  end;           

  w_numreg :=0;
  
   select count(*)        
      into w_numreg
   from tdom_aplicacargosbatchtmp a
   where a.tmo_codcargo=x.tmo_cargo
     and a.tmo_codmod =x.tmo_codmod
     and a.tmo_codtra =x.tmo_codtra
     and a.tmo_rubro =x.tmo_rubro
     and a.tmo_numcue = x.tmo_numcue
     and a.tmo_fechcon between w_fechaDesde  and p_fecha;
     
--Para determinar si el cargo aplica a personas naturales y personas juridicas
 if w_tipoper = 1 then
   if nvl(x.dcc_natural,0) = '1' then
            v_commit := 'S';
   else
            v_commit := 'N';
   end if;
 elsif w_tipoper = 2 then
   if nvl(x.dcc_juridico,0) = '1' then
            v_commit := 'S';
   else
            v_commit := 'N';
   end if;
 end if;


     if  v_commit = 'S' then
     begin
        val_cargo(x.tmo_numcue,x.tmo_codmod,x.tmo_codpro,x.tmo_codtip,x.tmo_codmon,x.tmo_cargo,x.tmo_val,x.tmo_fechproc,cargo,sobregiro,abierto);
     exception
      when others then
       rollback;
       v_commit := 'N';
        errtxt:=substr(sqlerrm,1,80);
        INSERT INTO LOG_BATCH(TEXTO) VALUES(X.tmo_CODUSR||'-'||X.tmo_NUMTRA||'-'||X.tmo_NUMCUE||'-'||x.tmo_cargo||'-'||errtxt);
         commit;
     end;
    end if;
      if w_numreg+1 <= x.tmo_nummaxexo then
         cargo:=0;-- tmo_nummaxexo es el numero de transacciones gratis en el periodo
      end if;
     if  v_commit = 'S' then
     --if nvl(cargo,0) > 0 then
     begin
     Insert into tdom_aplicacargosbatchtmp(tmo_fechcon, tmo_codusr,tmo_numtra, tmo_sec,tmo_fechproc,tmo_codmod, tmo_codpro,
                                           tmo_codtip,tmo_codmon, tmo_codtra, tmo_rubro, tmo_numcue, tmo_valorg, tmo_codcargo,
                                           tmo_depart,tmo_ref,tmo_diferido,tmo_cotiza,tmo_cargototal,tmo_cargosaldo)
     values(x.tmo_fechcon, x.tmo_codusr,x.tmo_numtra,x.tmo_sec,x.tmo_fechproc,x.tmo_codmod,x.tmo_codpro,
            x.tmo_codtip, x.tmo_codmon,x.tmo_codtra,x.tmo_rubro,x.tmo_numcue,x.tmo_val,x.tmo_cargo,x.tmo_depart,x.tmo_ref,x.tmo_diferido,x.tmo_cotiza,cargo,cargo);
            --la primer carga total y saldo es lo mismo.
       dbms_output.put_line('paso 4');
     commit;
     exception
      when dup_val_on_index then
        null; --si ejecutan el proceso dos veces que se carguen solo los que faltan
      when others then
       rollback;
       v_commit := 'N';
        errtxt:=substr(sqlerrm,1,80);
        INSERT INTO LOG_BATCH(TEXTO) VALUES(X.tmo_CODUSR||'-'||X.tmo_NUMTRA||'-'||X.tmo_NUMCUE||'-'||x.tmo_cargo||'-'||errtxt);
         commit;
     end;
     --end if;
    end if;
   end loop;
  end;

  PROCEDURE aplica_cargostmp(
   operador      in number,
   p_fecha       in  date,
   p_sucursal in number,
   p_oficina in number  ) is
   cursor t is
   select rowid,tmo_fechcon,tmo_fechproc, tmo_codusr,tmo_numtra, tmo_sec,tmo_codmod, tmo_codpro,
          tmo_codtip,tmo_codmon, tmo_codtra, tmo_rubro, tmo_numcue, tmo_valorg, tmo_codcargo,
          tmo_fechapli,tmo_codusrapli,tmo_depart,tmo_ref,tmo_diferido,tmo_cotiza,
          tmo_cargototal,tmo_cargosaldo
     from tdom_aplicacargosbatchtmp
     where  nvl(tmo_cargosaldo,0) > 0
      order by tmo_fechcon;
  cargo  NUMBER(18,6);
  sobregiro VARCHAR2(1);
  abierto   VARCHAR2(1);
  v_commit varchar2(1);
  efectivo number;
  secuencia NUMBER(7);
  SEC NUMBER(5);
  errtxt    char(80);
  cargoloquehay NUMBER(18,6);
  cargosaldo NUMBER(18,6);
   v_descargo tgen_cargos.crg_descripcion%type;
  begin
  delete log_batch;
  commit;
  for x in t loop
    cargo := 0;
    sobregiro := 'N';
    abierto:='N';
    v_commit := 'S';
    efectivo :=0;
   secuencia:=0;
   SEC :=0;
   cargoloquehay:=0; --Para cobrarle lo que tenga
   cargosaldo:=x.tmo_cargosaldo; --Para cobrarle lo que tenga
   v_descargo := null;
   begin
    VAL_CUENTA(x.tmo_numcue, 4, 'D', NULL);
   exception
       when others then
        rollback;
        errtxt:=substr(sqlerrm,1,80);
        V_COMMIT := 'N';
        INSERT INTO LOG_BATCH(TEXTO) VALUES(X.tmo_CODUSR||'-'||X.tmo_NUMTRA||'-'||X.tmo_NUMCUE||'-'||x.tmo_codcargo||'-'||errtxt);
        commit;
    end;
    if v_commit = 'S' then
    if nvl(cargosaldo,0) > 0 then
       --verifico que tenga saldo en efectivo
       begin
       select nvl(vis_salefe,0)
         into efectivo
		 from tcap_vista
        where vis_numcue = x.tmo_numcue;
      if nvl(efectivo,0) > 0 then
       if nvl(efectivo,0) >= nvl(x.tmo_cargosaldo,0) then
       begin
       cargosaldo:= 0;
	   secuencia:=Upd_Sectran(operador);
       sec:= NVL(sec,0)+1;
				   		   INS_TRAMON_BATCH(p_sucursal,p_oficina,operador,X.tmo_depart,X.tmo_ref,
							               X.tmo_diferido,secuencia,SEC,X.tmo_codmod,X.tmo_codpro,X.tmo_codtip,X.tmo_numcue,
							               X.tmo_codmon,X.tmo_codtra,X.tmo_rubro,NULL,x.tmo_cargosaldo,X.tmo_COTIZA,'D',f_fechatrabajo);--X.BMI_FECHPROC);
       exception
       when others then
        rollback;
        errtxt:=substr(sqlerrm,1,80);
        V_COMMIT := 'N';
        INSERT INTO LOG_BATCH(TEXTO) VALUES(X.tmo_CODUSR||'-'||X.tmo_NUMTRA||'-'||X.tmo_NUMCUE||'-'||x.tmo_codcargo||'-'||errtxt);
        commit;
        end;
        else--efectivo es menor <
       begin
       cargosaldo:= nvl(x.tmo_cargosaldo,0) - nvl(efectivo,0);
	   secuencia:=Upd_Sectran(operador);
       sec:= NVL(sec,0)+1;
				   		   INS_TRAMON_BATCH(p_sucursal,p_oficina,operador,X.tmo_depart,X.tmo_ref,
							               X.tmo_diferido,secuencia,SEC,X.tmo_codmod,X.tmo_codpro,X.tmo_codtip,X.tmo_numcue,
							               X.tmo_codmon,X.tmo_codtra,X.tmo_rubro,NULL,efectivo,X.tmo_COTIZA,'D',f_fechatrabajo);--X.BMI_FECHPROC);
       exception
       when others then
        rollback;
        errtxt:=substr(sqlerrm,1,80);
        V_COMMIT := 'N';
        INSERT INTO LOG_BATCH(TEXTO) VALUES(X.tmo_CODUSR||'-'||X.tmo_NUMTRA||'-'||X.tmo_NUMCUE||'-'||x.tmo_codcargo||'-'||errtxt);
        commit;
        end;
        end if;--if nvl(efectivo,0) >= nvl(cargo,0) then
        IF V_COMMIT = 'S' THEN
	      BEGIN
          SELECT crg_descripcion
            into v_descargo
           FROM tgen_cargos
           WHERE crg_codigo = x.tmo_codcargo;

          INSERT INTO TCAP_CONCEPTO(CON_CODUSR,CON_NUMTRAN,CON_SEC,CON_MODULO,CON_CUENTA,
									CON_TABCONCEPTO,CON_CONCEPTO,CON_LIBRE,CON_RUBRO,CON_TRANSA,CON_MODULO1,
									CON_CUENTA1)
				  VALUES(operador,secuencia,SEC,x.tmo_codmod,x.tmo_numcue,
									110,NULL,v_descargo||' del:'||to_char(x.tmo_fechcon,'yyyy/mm/dd')||' Por:'||to_char(x.tmo_valorg)||' usr:'||x.tmo_codusr||' Numtra:'||x.tmo_numtra,x.tmo_rubro,x.tmo_codtra,NULL,
									NULL);
		  EXCEPTION
		  WHEN OTHERS THEN
          ROLLBACK;
          V_COMMIT  := 'N';
          errtxt := SUBSTR(SQLERRM,1,150);
          INSERT INTO LOG_BATCH VALUES('ERROR concepto:'||errtxt);
          COMMIT;
 	      END;
       END IF;
        end if; --      if nvl(efectivo,0) > 0 then
     exception
      when others then
       rollback;
       v_commit := 'N';
        errtxt:=substr(sqlerrm,1,80);
        INSERT INTO LOG_BATCH(TEXTO) VALUES(X.tmo_CODUSR||'-'||X.tmo_NUMTRA||'-'||X.tmo_NUMCUE||'-'||x.tmo_codcargo||'-'||errtxt);
        commit;
       end;
    end if; --if nvl(cargo,0) > 0 then
    end if; --if v_commit = 'S' then
    if v_commit = 'S' then
       update tdom_aplicacargosbatchtmp
          set tmo_cargosaldo = cargosaldo,
              tmo_fechapli = sysdate,
              tmo_codusrapli = operador
        where rowid = x.rowid;
         commit;
    end if;
  end loop;
  end;
END;
/
