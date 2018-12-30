CREATE OR REPLACE
package body pkg_precalifica_do is

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Procedimiento que permite enviar mensajes de error a la Bitácora de la Calificación de Cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
/*  procedure send_logcalifica ( p_fecha date, p_tipo varchar2, p_fechainicio date, p_fechareal date, p_texto varchar2 ) is
  begin
    insert into tpre_logcalifdet ( lga_fecha, lga_tipo, lga_fechaini, lga_fechareal, lga_texto )
    values ( p_fecha, p_tipo, p_fechainicio, p_fechareal, p_texto );
  end send_logcalifica;*/

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Extrae los dias de mora por los valores de capital a la fecha indicada
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

/*  function dias_mora_cap ( p_credito number, p_fecha date ) return number is
  begin
  null;
  end;*/

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Extrae los dias de mora por los valores de interes a la fecha indicada
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

/*  function dias_mora_int ( p_credito number, p_fecha date ) return number is o
  begin
   null;
  end;*/
  --Reestructuraciones
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- me indica si el credito es reestructurado
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  function reestructurado ( p_fecha in date, p_credito number) return date is
   Fdesde Date:=null;
   w_fuerestruct number:=0;
  begin
   select max(prp_fechaarr)
     into Fdesde
     from tpre_arrprestamos
    where prp_credito = p_credito
      and nvl(prp_afectacalif,'N') = 'S'
      and prp_fecarr <= p_fecha;
      if fdesde is null then
         select count(*)
           into w_fuerestruct
          from tleg_calificafija
           where ccf_cuenta = p_credito;
           if  nvl(w_fuerestruct,0) > 0 then
              fdesde:=to_date('2006/06/01','yyyy/mm/dd');--inicia cambio contable la idea es poner una fecha menor a la de ejecucion
           end if;
      end if;
      return Fdesde;
  exception
    when others then
       return null;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Si el credito es reestructurado se determina los dias de atraso con que se reestructuro
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  function dias_mora_en_reest (  p_fechares date, p_credito number ) return number is
    w_diasv number:=0;
    w_res   number:=0;
  begin
  select count(*)
    into w_res
    from tleg_calificafija
    where ccf_cuenta = p_credito;
    if nvl(w_res,0) > 0 then
       w_res:=1;
       return w_res;
    end if;
   select decode(acr_diasv,0,acr_diasint,acr_diasv)
     into w_diasv
    from  tpre_accrual
    where acr_fecha = trunc(p_fechares)- 1
      and acr_credito = p_credito;
      dbms_output.put_line('1 w_diasv:'||w_diasv);
      if nvl(w_diasv,0) = 0 then
		   select decode(acr_diasv,0,acr_diasint,acr_diasv)
		     into w_diasv
		    from  tpre_accrual
		    where acr_credito = p_credito
		      and acr_fecha = (select min(acr_fecha)-1
		                         from tpre_accrual
		                        where acr_credito = p_credito
		                          and nvl(acr_otrostatus,'-')= 'R');         
      dbms_output.put_line('2 w_diasv:'||w_diasv);		                          
      end if;
      return w_diasv;
  exception
    when others then
       return 0;
  end;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Indica si se usan garantias o no
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  function aplica_garantia_prov( p_tabtipocred in number, p_tipocred in number, p_calprev in varchar2) return varchar2 is
  w_usogarantia varchar2(1);
  begin
   	select par_usogarantia
   	  into w_usogarantia
   	  from tpre_paramcaliftipo
   	 where par_tabtipocredito = p_tabtipocred
   	   and par_tipocredito = p_tipocred;
   	 return w_usogarantia;
  end;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Función que devuelve el Monto de Garantías Reales que está asociado a un préstamo, según 2/6004
  --aqui se aumenta las condiciones de admisibilidad
  --garantias debe estar legalizada-tasacion no debe pasar de 18 meses - debe tener endoso vigente
  --las garantias deben reunir condiciones de facil convertibilidad
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  function cta_montogar_real ( p_mod number, p_credito number, p_mon number, p_sucursal number, p_fecha date,
                               po_garadminisble out number,po_garadminisblefisa out number,
                               po_tipogarantia out varchar2 ) return number is
    w_valor_garantizado number(20,3) := 0;
    w_valor_admisiblefisa   number(20,3) := 0;
    w_valor_admisible   number(20,3) := 0;
    w_Pcotiza number(14,7);
    w_Pcotiza1 number(14,7);
    w_Pcotiza2 number(14,7);
    w_monto1 number(20,3);
    w_monto1ad number(20,3);

    w_valor_garantizado_v2 number(20,3) := 0;
    w_valor_admisiblefisa_v2   number(20,3) := 0;
    w_valor_admisible_v2   number(20,3) := 0;
    w_Pcotiza_v2 number(14,7);
    w_Pcotiza1_v2 number(14,7);
    w_Pcotiza2_v2 number(14,7);
    w_monto1_v2 number(20,3);
    w_monto1ad_v2 number(20,3);
    w_metrosconstruct number:=0;
    cursor operaciones_garantia is
      select ogr_cliente,ogr_numgaran,
             gar_moneda, nvl(ogr_valor,0) valor,
             avl_valor,gar_valorcomercial,codeiso
      from tgar_garantias , tcli_opergaran,tgar_avaluos,tgen_desctabla
      where gar_codcli = ogr_cliente
      and gar_numero = ogr_numgaran
      and gar_codcli = avl_codcli
      and gar_numero = avl_numgar
      and gar_tipgar = des_codigo
      and des_codtab = 33
     and codeiso NOT IN ('V2','V1','V3','V4','V5','V6','V7','V8') --certificados propios del banco no espero notificacion
      and p_fecha between trunc(avl_fecha) and nvl(avl_fechasta,to_date('2199/12/31','YYYY/MM/DD'))
      and ogr_mod = p_mod
      and ogr_operacion = p_credito
      and gar_fecdesde IS NOT NULL --<= p_fecha
      --and ogr_fecdesde IS NOT NULL --<= p_fecha
      and nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha  --IS NULL
      and ogr_fecdesde is  not null
      and nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha; --IS  NULL
    cursor operaciones_garantia_v2 is
      select ogr_cliente,ogr_numgaran,
             gar_moneda, nvl(ogr_valor,0) valor,
             avl_valor,gar_valorcomercial,codeiso
      from tgar_garantias , tcli_opergaran,tgar_avaluos,tgen_desctabla
      where gar_codcli = ogr_cliente
      and gar_numero = ogr_numgaran
      and gar_codcli = avl_codcli
      and gar_numero = avl_numgar
      and gar_tipgar = des_codigo
      and des_codtab = 33
      and codeiso  IN ('V2','V1','V3','V4','V5','V6','V7','V8') --certificados propios del banco no espero notificacion
      and p_fecha between trunc(avl_fecha) and nvl(avl_fechasta,to_date('2199/12/31','YYYY/MM/DD'))
      and ogr_mod = p_mod
      and ogr_operacion = p_credito
      and gar_fecdesde IS NOT NULL --<= p_fecha
      --and ogr_fecdesde <= p_fecha
      --and nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha  --IS NULL se comenta ya que si se cacncelan despues del cierre igual deja descubierto el credito
      and ogr_fechasta is null
      and ogr_fecdesde is  not null
      and gar_fechasta is null;
      --and nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha; --IS  NULL
    cursor operaciones_garantia_tipo is
      select ogr_cliente,ogr_numgaran,
             gar_moneda, nvl(ogr_valor,0) valor,
             avl_valor,gar_valorcomercial,codeiso,
             decode(substr(codeiso,1,1),'H',1,'I',1,'P',2,'V',3,'O',4,5) Peso
      from tgar_garantias , tcli_opergaran,tgar_avaluos,tgen_desctabla
      where gar_codcli = ogr_cliente
      and gar_numero = ogr_numgaran
      and gar_codcli = avl_codcli(+)
      and gar_numero = avl_numgar(+)
      and gar_tipgar = des_codigo
      and des_codtab = 33
      --and codeiso = 'V2' --certificados propios del banco no espero notificacion
      --and p_fecha between trunc(avl_fecha) and nvl(avl_fechasta,to_date('2199/12/31','YYYY/MM/DD'))
      and ogr_mod = p_mod
      and ogr_operacion = p_credito
      and gar_fecdesde IS NOT NULL --<= p_fecha
      and nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha  --IS NULL
      and ogr_fecdesde is  not null
      and nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha
      order by decode(substr(codeiso,1,1),'H',1,'I',1,'P',2,'V',3,'O',4,5) ; --IS  NULL

      w_ultimoavaluo date;
      w_finendoso date;
      w_esadmisible varchar2(1):='S';
      w_gar_valorcomercial number:=0;
      w_gar_valorcomercial_v2 number:=0;
      w_aniop2 number;      
      --nOV 2017
      w_vigencia_tas_hipoteca number:=0;
      w_vigencia_tas_prendaria number:=0;      
      
  begin
  --NOV 2017
        w_vigencia_tas_hipoteca :=parametro_calif('T_VIGENCIA_TAS_HIPOTECA');
      w_vigencia_tas_prendaria :=parametro_calif('T_VIGENCIA_TAS_PRENDARIA');      

   --para el valor de admisible segun fisa con validaciones
    for c1 in operaciones_garantia loop
        w_esadmisible :='S';
        w_gar_valorcomercial:=0;
        select max(avl_fecha)
          into w_ultimoavaluo
        from tgar_avaluos
        where avl_codcli = c1.ogr_cliente
          and avl_numgar =  c1.ogr_numgaran
          and avl_fechasta is null;
         if p_fecha > w_ultimoavaluo then
           if substr(c1.codeiso,1,1) in ('H','I') then
            if p_fecha - w_ultimoavaluo > w_vigencia_tas_hipoteca then --549 dias 18 meses , algun rato se debe poner como parametro este tiempo
               w_esadmisible :='N';
            end if;
           else
            if p_fecha - w_ultimoavaluo > w_vigencia_tas_prendaria then --365 dias 12 meses , algun rato se debe poner como parametro este tiempo
               w_esadmisible :='N';
            end if;
           end if;
         end if;
--cuando es solo terreno no exisje poliza
       if substr(c1.codeiso,1,1) IN ('H','I') then
         begin
          select nvl(GHP_MTSCONSTRU,0)
            into w_metrosconstruct
            from tgar_hipoteca
           where ghp_codcli = c1.ogr_cliente
             and ghp_numero = c1.ogr_numgaran
             and GHP_FECHASTA is null;--camiones no entran en este proceso
         exception
          when no_data_found then
              w_metrosconstruct:=0;
         end;
       end if;
--fin
       if w_esadmisible = 'S' then
        select nvl(max(ecb_hastaendoso),to_date('2000/11/01','yyyy/mm/dd'))
          into w_finendoso
         from tpre_endosobien
         where ecb_codcli = c1.ogr_cliente
           and ecb_numgar = c1.ogr_numgaran
           and ecb_hasta is null;
         if  substr(c1.codeiso,1,1) IN ('H','I') and w_metrosconstruct = 0 then
             w_esadmisible :='S';
         else
         if  w_finendoso <= p_fecha then
               w_esadmisible :='N';
         end if;
         end if;
       end if;  --si el endoso esta a favor del banco y vigente
        w_aniop2 := to_number(to_char(f_fechatrabajo,'yyyy')) ;
        po_tipogarantia:=substr(c1.codeiso,1,2);
       if c1.codeiso IN ('P2','P3','P4') then
         begin
          select to_number(to_char(gvh_anio,'yyyy'))
            into w_aniop2
            from tgar_prendveh
           where gvh_codcli = c1.ogr_cliente
             and gvh_numero = c1.ogr_numgaran
             and GVH_FECHASTA is null
             and nvl(gvh_codigo,0) not in (4,6);--camiones no entran en este proceso
         exception
          when no_data_found then
              w_aniop2 := to_number(to_char(f_fechatrabajo,'yyyy')) ;
         end;
         if to_number(to_char(f_fechatrabajo,'yyyy')) -  w_aniop2 > 5 then
            w_esadmisible := 'N';
            po_tipogarantia:='O2';--un p2 mas de 5 años se vuelve O2 sin valor
         end if;
       end if;
   if w_esadmisible = 'S' then
      --extraer el valor prorrateado
      begin
      select prg_valor
        into w_gar_valorcomercial
       from tgar_prorragaran
       where prg_codmod = p_mod
         and prg_numcue = p_credito
         and prg_cliente = c1.ogr_cliente
         and prg_numgaran = c1.ogr_numgaran
         and p_fecha between trunc(prg_desde) and nvl(prg_hasta,to_date('2199/12/31','YYYY/MM/DD'));
      exception
       when others then
        w_gar_valorcomercial:=0;
      end;
      w_gar_valorcomercial:=c1.gar_valorcomercial;
      if c1.gar_moneda = p_mon then
        w_valor_garantizado := w_valor_garantizado + c1.avl_valor;
        w_valor_admisiblefisa := w_valor_admisiblefisa + w_gar_valorcomercial;--c1.gar_valorcomercial;
        --w_valor_admisible := w_valor_admisible + c1.gar_valorcomercial;
      else
        if p_mon = 0 then
          pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza );
          w_valor_garantizado  := w_valor_garantizado + (c1.avl_valor * w_Pcotiza);
          w_valor_admisiblefisa  := w_valor_admisiblefisa + (w_gar_valorcomercial * w_Pcotiza);
          --w_valor_admisible  := w_valor_admisible + (c1.gar_valorcomercial * w_Pcotiza);
        else
          if c1.gar_moneda = 0 then
            pkg_cambios.Pcotpromedio (p_fecha, p_sucursal, p_mon, w_Pcotiza);
            w_valor_garantizado := w_valor_garantizado + (c1.avl_valor / w_Pcotiza);
            w_valor_admisiblefisa := w_valor_admisiblefisa + (w_gar_valorcomercial / w_Pcotiza);
            --w_valor_admisible := w_valor_admisible + (c1.gar_valorcomercial / w_Pcotiza);
          else
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza1);
            w_monto1 := c1.avl_valor * w_Pcotiza1;
            w_monto1ad := w_gar_valorcomercial * w_Pcotiza1;
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, p_mon, w_Pcotiza2 );
            w_monto1 := w_monto1 / w_Pcotiza2;
            w_monto1ad := w_monto1ad / w_Pcotiza2;
            w_valor_garantizado := w_valor_garantizado + w_monto1;
            w_valor_admisiblefisa := w_valor_admisiblefisa + w_monto1ad;
            --w_valor_admisible := w_valor_admisible + w_monto1ad;
          end if;
        end if;
      end if;
   end if;
    end loop;
  --para el admisible como esta actualmente 1
  w_valor_garantizado:=0;
  for c1 in operaciones_garantia loop
        w_esadmisible :='S';
        w_gar_valorcomercial:=c1.gar_valorcomercial;
        w_aniop2 := to_number(to_char(f_fechatrabajo,'yyyy')) ;
        po_tipogarantia:=substr(c1.codeiso,1,2);
       if c1.codeiso in ('P2','P3','P4') then
         begin
          select to_number(to_char(gvh_anio,'yyyy'))
            into w_aniop2
            from tgar_prendveh
           where gvh_codcli = c1.ogr_cliente
             and gvh_numero = c1.ogr_numgaran
             and gvh_fechasta is null
             and nvl(gvh_codigo,0) not in (4,6);--camiones no entran en este proceso
         exception
          when no_data_found then
              w_aniop2 := to_number(to_char(f_fechatrabajo,'yyyy')) ;
         end;
         if to_number(to_char(f_fechatrabajo,'yyyy')) -  w_aniop2 > 5 then
            w_esadmisible := 'N';
            po_tipogarantia:='O2';
         end if;
       end if;
     if w_esadmisible = 'S' then
      --extraer el valor prorrateado
      begin
      select prg_valor
        into w_gar_valorcomercial
       from tgar_prorragaran
       where prg_codmod = p_mod
         and prg_numcue = p_credito
         and prg_cliente = c1.ogr_cliente
         and prg_numgaran = c1.ogr_numgaran
         and p_fecha between trunc(prg_desde) and nvl(prg_hasta,to_date('2199/12/31','YYYY/MM/DD'));
      exception
       when others then
        w_gar_valorcomercial:=0;
      end;
      w_gar_valorcomercial:=c1.gar_valorcomercial;
      if c1.gar_moneda = p_mon then
        --w_valor_garantizado := w_valor_garantizado + c1.avl_valor;
        --w_valor_admisiblefisa := w_valor_admisible + w_gar_valorcomercial;--c1.gar_valorcomercial;
        w_valor_admisible := w_valor_admisible + c1.gar_valorcomercial;
      else
        if p_mon = 0 then
          pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza );
          --w_valor_garantizado  := w_valor_garantizado + (c1.avl_valor * w_Pcotiza);
          --w_valor_admisiblefisa  := w_valor_admisible + (w_gar_valorcomercial * w_Pcotiza);
          w_valor_admisible  := w_valor_admisible + (c1.gar_valorcomercial * w_Pcotiza);
        else
          if c1.gar_moneda = 0 then
            pkg_cambios.Pcotpromedio (p_fecha, p_sucursal, p_mon, w_Pcotiza);
            --w_valor_garantizado := w_valor_garantizado + (c1.avl_valor / w_Pcotiza);
            --w_valor_admisiblefisa := w_valor_admisible + (w_gar_valorcomercial / w_Pcotiza);
            w_valor_admisible := w_valor_admisible + (c1.gar_valorcomercial / w_Pcotiza);
          else
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza1);
            w_monto1 := c1.avl_valor * w_Pcotiza1;
            w_monto1ad := w_gar_valorcomercial * w_Pcotiza1;
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, p_mon, w_Pcotiza2 );
            w_monto1 := w_monto1 / w_Pcotiza2;
            w_monto1ad := w_monto1ad / w_Pcotiza2;
            --w_valor_garantizado := w_valor_garantizado + w_monto1;
            --w_valor_admisiblefisa := w_valor_admisiblefisa + w_monto1ad;
            w_valor_admisible := w_valor_admisible + w_monto1ad;
          end if;
        end if;
      end if;
   end if;
    end loop;
  --fin de dmisib
---ini v2
    for c1 in operaciones_garantia_v2 loop
        w_esadmisible :='S';
        w_gar_valorcomercial_v2:=0;
        po_tipogarantia:=substr(c1.codeiso,1,2);
     if w_esadmisible = 'S' then
      --extraer el valor prorrateado
      begin
      select prg_valor
        into w_gar_valorcomercial_v2
       from tgar_prorragaran
       where prg_codmod = p_mod
         and prg_numcue = p_credito
         and prg_cliente = c1.ogr_cliente
         and prg_numgaran = c1.ogr_numgaran
         and p_fecha between trunc(prg_desde) and nvl(prg_hasta,to_date('2199/12/31','YYYY/MM/DD'));
      exception
       when others then
        w_gar_valorcomercial_v2:=0;
      end;
      w_gar_valorcomercial_v2:=c1.gar_valorcomercial;
      if c1.gar_moneda = p_mon then
        w_valor_garantizado_v2 := w_valor_garantizado_v2 + c1.avl_valor;
        w_valor_admisiblefisa_v2 := w_valor_admisiblefisa_v2 + w_gar_valorcomercial_v2;--c1.gar_valorcomercial;
        w_valor_admisible_v2 := w_valor_admisible_v2 + c1.gar_valorcomercial;
      else
        if p_mon = 0 then
          pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza_v2 );
          w_valor_garantizado_v2  := w_valor_garantizado_v2 + (c1.avl_valor * w_Pcotiza_v2);
          w_valor_admisiblefisa_v2  := w_valor_admisiblefisa_v2 + (w_gar_valorcomercial_v2 * w_Pcotiza_v2);
          w_valor_admisible_v2  := w_valor_admisible_v2 + (c1.gar_valorcomercial * w_Pcotiza_v2);
        else
          if c1.gar_moneda = 0 then
            pkg_cambios.Pcotpromedio (p_fecha, p_sucursal, p_mon, w_Pcotiza_v2);
            w_valor_garantizado_v2 := w_valor_garantizado_v2 + (c1.avl_valor / w_Pcotiza_v2);
            w_valor_admisiblefisa_v2 := w_valor_admisiblefisa_v2 + (w_gar_valorcomercial_v2 / w_Pcotiza_v2);
            w_valor_admisible_v2 := w_valor_admisible_v2 + (c1.gar_valorcomercial / w_Pcotiza_v2);
          else
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza1_v2);
            w_monto1_v2 := c1.avl_valor * w_Pcotiza1_v2;
            w_monto1ad_v2 := w_gar_valorcomercial_v2 * w_Pcotiza1_v2;
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, p_mon, w_Pcotiza2_v2 );
            w_monto1_v2 := w_monto1_v2 / w_Pcotiza2_v2;
            w_monto1ad_v2 := w_monto1ad_v2 / w_Pcotiza2_v2;
            w_valor_garantizado_v2 := w_valor_garantizado_v2 + w_monto1_v2;
            w_valor_admisiblefisa_v2 := w_valor_admisiblefisa_v2 + w_monto1ad_v2;
            w_valor_admisible_v2 := w_valor_admisible_v2 + w_monto1ad_v2;
          end if;
        end if;
      end if;
   end if;
    end loop;
--para ver el tipo de garantia
--  w_pesoaux:=99;
    for c1 in operaciones_garantia_tipo loop
            po_tipogarantia:=substr(c1.codeiso,1,2);
            --w_peso := c1.peso;
       if c1.codeiso in ('P2','P3','P4') then
         begin
          select to_number(to_char(gvh_anio,'yyyy'))
            into w_aniop2
            from tgar_prendveh
           where gvh_codcli = c1.ogr_cliente
             and gvh_numero = c1.ogr_numgaran
             and gvh_fechasta is null
             and nvl(gvh_codigo,0) not in (4,6);--camiones no entran en este proceso
         exception
          when no_data_found then
              w_aniop2 := to_number(to_char(f_fechatrabajo,'yyyy')) ;
         end;
         if to_number(to_char(f_fechatrabajo,'yyyy')) -  w_aniop2 > 5 then
            w_esadmisible := 'N';
            po_tipogarantia:='O2';
            --w_peso := 4;
         end if;
       end if;
      if substr(po_tipogarantia,1,1) ='H' then
         exit;
      end if ;                          
      if substr(po_tipogarantia,1,1) ='I' then
         exit;
      end if ;      
      if substr(po_tipogarantia,1,1) ='P' then
         exit;
      end if ;
      if substr(po_tipogarantia,1,1) ='V' then
         exit;
      end if ;
      if substr(po_tipogarantia,1,1) ='O' then
         exit;
      end if ;

    end loop;
--
---fin v2
    po_garadminisblefisa := nvl(w_valor_admisiblefisa,0) + nvl(w_valor_admisiblefisa_v2,0);
    po_garadminisble := nvl(w_valor_admisible,0) + nvl(w_valor_admisible_v2,0);
    return nvl(w_valor_garantizado,0) + nvl(w_valor_garantizado_v2,0);
  end cta_montogar_real;
--inicio distribucion
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Función que devuelve el Monto de Garantías Reales que está asociado a un préstamo, según 2/6004
  --aqui se aumenta las condiciones de admisibilidad
  --garantias debe estar legalizada-tasacion no debe pasar de 18 meses - debe tener endoso vigente
  --las garantias deben reunir condiciones de facil convertibilidad
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure distribucion_admisible_real ( p_mod number, p_credito number, p_mon number, p_sucursal number, p_fecha date,
                               po_garadminisble out number,po_garadminisblefisa out number,
                               po_tipogarantia out varchar2 ) is
    w_valor_garantizado number(20,3) := 0;
    w_valor_admisiblefisa   number(20,3) := 0;
    w_valor_admisible   number(20,3) := 0;
    w_Pcotiza number(14,7);
    w_Pcotiza1 number(14,7);
    w_Pcotiza2 number(14,7);
    w_monto1 number(20,3);
    w_monto1ad number(20,3);

    w_valor_garantizado_v2 number(20,3) := 0;
    w_valor_admisiblefisa_v2   number(20,3) := 0;
    w_valor_admisible_v2   number(20,3) := 0;
    w_Pcotiza_v2 number(14,7);
    w_Pcotiza1_v2 number(14,7);
    w_Pcotiza2_v2 number(14,7);
    w_monto1_v2 number(20,3);
    w_monto1ad_v2 number(20,3);
    w_metrosconstruct number:=0;
    cursor operaciones_garantia is
      select ogr_cliente,ogr_numgaran,
             gar_moneda, nvl(ogr_valor,0) valor,
             avl_valor,gar_valorcomercial,codeiso,ogr_operacion,
             ogr_mod
      from tgar_garantias , tcli_opergaran,tgar_avaluos,tgen_desctabla
      where gar_codcli = ogr_cliente
      and gar_numero = ogr_numgaran
      and gar_codcli = avl_codcli
      and gar_numero = avl_numgar
      and gar_tipgar = des_codigo
      and des_codtab = 33
      and codeiso not in ('V1','V2','V3','V4','V5','V6','V7','V8') --certificados propios del banco no espero notificacion
      and p_fecha between trunc(avl_fecha) and nvl(avl_fechasta,to_date('2199/12/31','YYYY/MM/DD'))
      and ogr_mod = p_mod
      and ogr_operacion = p_credito
      and gar_fecdesde IS NOT NULL --<= p_fecha
      --and ogr_fecdesde IS NOT NULL --<= p_fecha
      and nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha  --IS NULL
      and ogr_fecdesde is  not null
      and nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha; --IS  NULL
    cursor operaciones_garantia_v2 is
      select ogr_cliente,ogr_numgaran,
             gar_moneda, nvl(ogr_valor,0) valor,
             avl_valor,gar_valorcomercial,codeiso,ogr_operacion,ogr_mod
      from tgar_garantias , tcli_opergaran,tgar_avaluos,tgen_desctabla
      where gar_codcli = ogr_cliente
      and gar_numero = ogr_numgaran
      and gar_codcli = avl_codcli
      and gar_numero = avl_numgar
      and gar_tipgar = des_codigo
      and des_codtab = 33
      and codeiso  in ('V1','V2','V3','V4','V5','V6','V7','V8') --certificados propios del banco no espero notificacion
      and p_fecha between trunc(avl_fecha) and nvl(avl_fechasta,to_date('2199/12/31','YYYY/MM/DD'))
      and ogr_mod = p_mod
      and ogr_operacion = p_credito
      and gar_fecdesde IS NOT NULL --<= p_fecha
      --and ogr_fecdesde <= p_fecha
      --and nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha  --IS NULL se comenta ya que si se cacncelan despues del cierre igual deja descubierto el credito
      and ogr_fechasta is null
      and ogr_fecdesde is  not null
      and gar_fechasta is null;
      --and nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha; --IS  NULL
    cursor operaciones_garantia_tipo is
      select ogr_cliente,ogr_numgaran,
             gar_moneda, nvl(ogr_valor,0) valor,
             avl_valor,gar_valorcomercial,codeiso,
             decode(substr(codeiso,1,1),'H',1,'I',1,'P',2,'V',3,'O',4,5) Peso
      from tgar_garantias , tcli_opergaran,tgar_avaluos,tgen_desctabla
      where gar_codcli = ogr_cliente
      and gar_numero = ogr_numgaran
      and gar_codcli = avl_codcli(+)
      and gar_numero = avl_numgar(+)
      and gar_tipgar = des_codigo
      and des_codtab = 33
      --and codeiso = 'V2' --certificados propios del banco no espero notificacion
      --and p_fecha between trunc(avl_fecha) and nvl(avl_fechasta,to_date('2199/12/31','YYYY/MM/DD'))
      and ogr_mod = p_mod
      and ogr_operacion = p_credito
      and gar_fecdesde IS NOT NULL --<= p_fecha
      and nvl(ogr_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha  --IS NULL
      and ogr_fecdesde is  not null
      and nvl(gar_fechasta,to_date('2199/12/31','YYYY/MM/DD')) > p_fecha
      order by decode(substr(codeiso,1,1),'H',1,'I',1,'P',2,'V',3,'O',4,5) ; --IS  NULL
      w_ultimoavaluo date;
      w_finendoso date;
      w_esadmisible varchar2(1):='S';
      w_gar_valorcomercial number:=0;
      w_gar_valorcomercialope number:=0;
      w_gar_valorcomercial_v2 number:=0;
      w_aniop2 number;
      w_TPROPIEDAD varchar2(16);
      --nOV 2017
      w_vigencia_tas_hipoteca number:=0;
      w_vigencia_tas_prendaria number:=0;      
      
  begin
  --NOV 2017
        w_vigencia_tas_hipoteca :=parametro_calif('T_VIGENCIA_TAS_HIPOTECA');
      w_vigencia_tas_prendaria :=parametro_calif('T_VIGENCIA_TAS_PRENDARIA');      

   --para el valor de admisible segun fisa con validaciones
    for c1 in operaciones_garantia loop
        w_esadmisible :='S';
        w_gar_valorcomercial:=0;
		w_gar_valorcomercialope:=0;
        select max(avl_fecha)
          into w_ultimoavaluo
        from tgar_avaluos
        where avl_codcli = c1.ogr_cliente
          and avl_numgar =  c1.ogr_numgaran
          and avl_fechasta is null;
         if p_fecha > w_ultimoavaluo then
           if substr(c1.codeiso,1,1) IN ('H','I') then
            if p_fecha - w_ultimoavaluo > w_vigencia_tas_hipoteca then --549 dias 18 meses , algun rato se debe poner como parametro este tiempo
               w_esadmisible :='N';
            end if;
           else
            if p_fecha - w_ultimoavaluo > w_vigencia_tas_prendaria then --365 dias 12 meses , algun rato se debe poner como parametro este tiempo
               w_esadmisible :='N';
            end if;
           end if;
         end if;
         dbms_output.put_line(' c1.ogr_cliente:'||c1.ogr_cliente||' c1.ogr_numgaran:'||c1.ogr_numgaran||'1 w_esadmisible:'||w_esadmisible);
--cuando es solo terreno no exisje poliza
       if substr(c1.codeiso,1,1)IN ('H','I') then
         begin
          select nvl(GHP_MTSCONSTRU,0),GHP_TPROPIEDAD
            into w_metrosconstruct,w_TPROPIEDAD
            from tgar_hipoteca
           where ghp_codcli = c1.ogr_cliente
             and ghp_numero = c1.ogr_numgaran
             and GHP_FECHASTA is null;--camiones no entran en este proceso
         exception
          when no_data_found then
              w_metrosconstruct:=0;
         end;
         if w_TPROPIEDAD is null then
           w_esadmisible := 'N';
         end if;
       end if;
         dbms_output.put_line(' 2 w_esadmisible:'||w_esadmisible);
--fin
       if w_esadmisible = 'S' then
        select nvl(max(ecb_hastaendoso),to_date('2000/11/01','yyyy/mm/dd'))
          into w_finendoso
         from tpre_endosobien
         where ecb_codcli = c1.ogr_cliente
           and ecb_numgar = c1.ogr_numgaran
           and ecb_hasta is null;
         if  substr(c1.codeiso,1,1) IN ('H','I') and w_metrosconstruct = 0 then
             w_esadmisible :='S';
         else
         if  w_finendoso <= p_fecha then
               w_esadmisible :='N';
         end if;
         end if;
       end if;  --si el endoso esta a favor del banco y vigente
         dbms_output.put_line(' 3 w_esadmisible:'||w_esadmisible);
        w_aniop2 := to_number(to_char(f_fechatrabajo,'yyyy')) ;
        po_tipogarantia:=substr(c1.codeiso,1,2);
       if c1.codeiso in ('P2','P3','P4') then
         begin
          select to_number(to_char(gvh_anio,'yyyy'))
            into w_aniop2
            from tgar_prendveh
           where gvh_codcli = c1.ogr_cliente
             and gvh_numero = c1.ogr_numgaran
             and GVH_FECHASTA is null
             and nvl(gvh_codigo,0) not in (4,6);--camiones no entran en este proceso
         exception
          when no_data_found then
              w_aniop2 := to_number(to_char(f_fechatrabajo,'yyyy')) ;
         end;
         if to_number(to_char(f_fechatrabajo,'yyyy')) -  w_aniop2 > 5 then
            w_esadmisible := 'N';
            po_tipogarantia:='O2';--un p2 mas de 5 años se vuelve O2 sin valor
         end if;
       end if;
         dbms_output.put_line(' 4 w_esadmisible:'||w_esadmisible);
   if w_esadmisible = 'S' then
      --extraer el valor prorrateado
      begin
      select prg_valor
        into w_gar_valorcomercial
       from tgar_prorragaran
       where prg_codmod = p_mod
         and prg_numcue = p_credito
         and prg_cliente = c1.ogr_cliente
         and prg_numgaran = c1.ogr_numgaran
         and p_fecha between trunc(prg_desde) and nvl(prg_hasta,to_date('2199/12/31','YYYY/MM/DD'));
      exception
       when others then
        w_gar_valorcomercial:=0;
      end;
      w_gar_valorcomercial:=c1.gar_valorcomercial;
      if c1.gar_moneda = p_mon then
        w_valor_garantizado := w_valor_garantizado + c1.avl_valor;
        w_valor_admisiblefisa := w_valor_admisiblefisa + w_gar_valorcomercial;--c1.gar_valorcomercial;
        w_gar_valorcomercialope:=w_gar_valorcomercial;
        --w_valor_admisible := w_valor_admisible + c1.gar_valorcomercial;
      else
        if p_mon = 0 then
          pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza );
          w_valor_garantizado  := w_valor_garantizado + (c1.avl_valor * w_Pcotiza);
          w_valor_admisiblefisa  := w_valor_admisiblefisa + (w_gar_valorcomercial * w_Pcotiza);
          w_gar_valorcomercialope:=(w_gar_valorcomercial * w_Pcotiza);
          --w_valor_admisible  := w_valor_admisible + (c1.gar_valorcomercial * w_Pcotiza);
        else
          if c1.gar_moneda = 0 then
            pkg_cambios.Pcotpromedio (p_fecha, p_sucursal, p_mon, w_Pcotiza);
            w_valor_garantizado := w_valor_garantizado + (c1.avl_valor / w_Pcotiza);
            w_valor_admisiblefisa := w_valor_admisiblefisa + (w_gar_valorcomercial / w_Pcotiza);
            w_gar_valorcomercialope:=(w_gar_valorcomercial / w_Pcotiza);
            --w_valor_admisible := w_valor_admisible + (c1.gar_valorcomercial / w_Pcotiza);
          else
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza1);
            w_monto1 := c1.avl_valor * w_Pcotiza1;
            w_monto1ad := w_gar_valorcomercial * w_Pcotiza1;
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, p_mon, w_Pcotiza2 );
            w_monto1 := w_monto1 / w_Pcotiza2;
            w_monto1ad := w_monto1ad / w_Pcotiza2;
            w_valor_garantizado := w_valor_garantizado + w_monto1;
            w_valor_admisiblefisa := w_valor_admisiblefisa + w_monto1ad;
            w_gar_valorcomercialope:=w_monto1ad;
            --w_valor_admisible := w_valor_admisible + w_monto1ad;
          end if;
        end if;
      end if;
   end if;
            dbms_output.put_line(' 5 w_esadmisible:'||w_esadmisible);
   update tleg_opergaran
      set log_valoradmifisa= w_gar_valorcomercialope
    where log_fecha = p_fecha
      and log_tipo = 'P'
      and log_mod = c1.ogr_mod
      and log_operacion = c1.ogr_operacion
      and log_cliente = c1.ogr_cliente
      and log_numgaran = c1.ogr_numgaran;
     if sql%notfound then
     insert into tleg_opergaran(log_fecha, log_tipo, log_mod, log_operacion, log_cliente, log_numgaran, log_valoradmifisa, log_valoradmireal)
                 values(p_fecha,'P',c1.ogr_mod,c1.ogr_operacion,c1.ogr_cliente,c1.ogr_numgaran,w_gar_valorcomercialope,0);
     end if;
    end loop;
  --para el admisible como esta actualmente 1
  for c1 in operaciones_garantia loop
        w_esadmisible :='S';
        w_gar_valorcomercial:=c1.gar_valorcomercial;
        w_gar_valorcomercialope:=0;
        w_aniop2 := to_number(to_char(f_fechatrabajo,'yyyy')) ;
        po_tipogarantia:=substr(c1.codeiso,1,2);
       if c1.codeiso in ('P2','P3','P4') then
         begin
          select to_number(to_char(gvh_anio,'yyyy'))
            into w_aniop2
            from tgar_prendveh
           where gvh_codcli = c1.ogr_cliente
             and gvh_numero = c1.ogr_numgaran
             and gvh_fechasta is null
             and nvl(gvh_codigo,0) not in (4,6);--camiones no entran en este proceso
         exception
          when no_data_found then
              w_aniop2 := to_number(to_char(f_fechatrabajo,'yyyy')) ;
         end;
         if to_number(to_char(f_fechatrabajo,'yyyy')) -  w_aniop2 > 5 then
            w_esadmisible := 'N';
            po_tipogarantia:='O2';
         end if;
       end if;
     if w_esadmisible = 'S' then
      --extraer el valor prorrateado
      begin
      select prg_valor
        into w_gar_valorcomercial
       from tgar_prorragaran
       where prg_codmod = p_mod
         and prg_numcue = p_credito
         and prg_cliente = c1.ogr_cliente
         and prg_numgaran = c1.ogr_numgaran
         and p_fecha between trunc(prg_desde) and nvl(prg_hasta,to_date('2199/12/31','YYYY/MM/DD'));
      exception
       when others then
        w_gar_valorcomercial:=0;
      end;
      w_gar_valorcomercial:=c1.gar_valorcomercial;
      if c1.gar_moneda = p_mon then
        w_valor_garantizado := w_valor_garantizado + c1.avl_valor;
        --w_valor_admisiblefisa := w_valor_admisible + w_gar_valorcomercial;--c1.gar_valorcomercial;
        w_valor_admisible := w_valor_admisible + c1.gar_valorcomercial;
        w_gar_valorcomercialope:=c1.gar_valorcomercial;
      else
        if p_mon = 0 then
          pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza );
          w_valor_garantizado  := w_valor_garantizado + (c1.avl_valor * w_Pcotiza);
          --w_valor_admisiblefisa  := w_valor_admisible + (w_gar_valorcomercial * w_Pcotiza);
          w_valor_admisible  := w_valor_admisible + (c1.gar_valorcomercial * w_Pcotiza);
          w_gar_valorcomercialope:=(c1.gar_valorcomercial * w_Pcotiza);
        else
          if c1.gar_moneda = 0 then
            pkg_cambios.Pcotpromedio (p_fecha, p_sucursal, p_mon, w_Pcotiza);
            w_valor_garantizado := w_valor_garantizado + (c1.avl_valor / w_Pcotiza);
            --w_valor_admisiblefisa := w_valor_admisible + (w_gar_valorcomercial / w_Pcotiza);
            w_valor_admisible := w_valor_admisible + (c1.gar_valorcomercial / w_Pcotiza);
            w_gar_valorcomercialope:=(c1.gar_valorcomercial / w_Pcotiza);
          else
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza1);
            w_monto1 := c1.avl_valor * w_Pcotiza1;
            w_monto1ad := w_gar_valorcomercial * w_Pcotiza1;
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, p_mon, w_Pcotiza2 );
            w_monto1 := w_monto1 / w_Pcotiza2;
            w_monto1ad := w_monto1ad / w_Pcotiza2;
            w_valor_garantizado := w_valor_garantizado + w_monto1;
            --w_valor_admisiblefisa := w_valor_admisiblefisa + w_monto1ad;
            w_valor_admisible := w_valor_admisible + w_monto1ad;
            w_gar_valorcomercialope:=w_monto1ad;
          end if;
        end if;
      end if;
   end if;
      update tleg_opergaran
      set log_valoradmireal= w_gar_valorcomercialope
    where log_fecha = p_fecha
      and log_tipo = 'P'
      and log_mod = c1.ogr_mod
      and log_operacion = c1.ogr_operacion
      and log_cliente = c1.ogr_cliente
      and log_numgaran = c1.ogr_numgaran;
     if sql%notfound then
     insert into tleg_opergaran(log_fecha, log_tipo, log_mod, log_operacion, log_cliente, log_numgaran, log_valoradmifisa, log_valoradmireal)
                 values(p_fecha,'P',c1.ogr_mod,c1.ogr_operacion,c1.ogr_cliente,c1.ogr_numgaran,0,w_gar_valorcomercialope);
     end if;
    end loop;
  --fin de dmisib
---ini v2
    for c1 in operaciones_garantia_v2 loop
        w_esadmisible :='S';
        w_gar_valorcomercial_v2:=0;
        w_gar_valorcomercialope:=0;
        po_tipogarantia:=substr(c1.codeiso,1,2);
     if w_esadmisible = 'S' then
      --extraer el valor prorrateado
      begin
      select prg_valor
        into w_gar_valorcomercial_v2
       from tgar_prorragaran
       where prg_codmod = p_mod
         and prg_numcue = p_credito
         and prg_cliente = c1.ogr_cliente
         and prg_numgaran = c1.ogr_numgaran
         and p_fecha between trunc(prg_desde) and nvl(prg_hasta,to_date('2199/12/31','YYYY/MM/DD'));
      exception
       when others then
        w_gar_valorcomercial_v2:=0;
      end;
      w_gar_valorcomercial_v2:=c1.gar_valorcomercial;
      if c1.gar_moneda = p_mon then
        w_valor_garantizado_v2 := w_valor_garantizado_v2 + c1.avl_valor;
        w_valor_admisiblefisa_v2 := w_valor_admisible_v2 + w_gar_valorcomercial_v2;--c1.gar_valorcomercial;
        w_valor_admisible_v2 := w_valor_admisible_v2 + c1.gar_valorcomercial;
      else
        if p_mon = 0 then
          pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza_v2 );
          w_valor_garantizado_v2  := w_valor_garantizado_v2 + (c1.avl_valor * w_Pcotiza_v2);
          w_valor_admisiblefisa_v2  := w_valor_admisible_v2 + (w_gar_valorcomercial_v2 * w_Pcotiza_v2);
          w_valor_admisible_v2  := w_valor_admisible_v2 + (c1.gar_valorcomercial * w_Pcotiza_v2);
        else
          if c1.gar_moneda = 0 then
            pkg_cambios.Pcotpromedio (p_fecha, p_sucursal, p_mon, w_Pcotiza_v2);
            w_valor_garantizado_v2 := w_valor_garantizado_v2 + (c1.avl_valor / w_Pcotiza_v2);
            w_valor_admisiblefisa_v2 := w_valor_admisible_v2 + (w_gar_valorcomercial_v2 / w_Pcotiza_v2);
            w_valor_admisible_v2 := w_valor_admisible_v2 + (c1.gar_valorcomercial / w_Pcotiza_v2);
          else
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, c1.gar_moneda, w_Pcotiza1_v2);
            w_monto1_v2 := c1.avl_valor * w_Pcotiza1_v2;
            w_monto1ad_v2 := w_gar_valorcomercial_v2 * w_Pcotiza1_v2;
            pkg_cambios.Pcotpromedio ( p_fecha, p_sucursal, p_mon, w_Pcotiza2_v2 );
            w_monto1_v2 := w_monto1_v2 / w_Pcotiza2_v2;
            w_monto1ad_v2 := w_monto1ad_v2 / w_Pcotiza2_v2;
            w_valor_garantizado_v2 := w_valor_garantizado_v2 + w_monto1_v2;
            w_valor_admisiblefisa_v2 := w_valor_admisiblefisa_v2 + w_monto1ad_v2;
            w_valor_admisible_v2 := w_valor_admisible_v2 + w_monto1ad_v2;
          end if;
        end if;
      end if;
   end if;
   w_gar_valorcomercialope:= (nvl(c1.gar_valorcomercial,0)*bdi_promedio(c1.gar_moneda,p_sucursal,p_fecha));
      update tleg_opergaran
      set log_valoradmifisa= nvl(log_valoradmifisa,0)+ (nvl(c1.gar_valorcomercial,0)*bdi_promedio(c1.gar_moneda,p_sucursal,p_fecha)),
          log_valoradmireal= nvl(log_valoradmireal,0)+ (nvl(c1.gar_valorcomercial,0)*bdi_promedio(c1.gar_moneda,p_sucursal,p_fecha))
    where log_fecha = p_fecha
      and log_tipo = 'P'
      and log_mod = c1.ogr_mod
      and log_operacion = c1.ogr_operacion
      and log_cliente = c1.ogr_cliente
      and log_numgaran = c1.ogr_numgaran;
     if sql%notfound then
     insert into tleg_opergaran(log_fecha, log_tipo, log_mod, log_operacion, log_cliente, log_numgaran, log_valoradmifisa, log_valoradmireal)
                 values(p_fecha,'P',c1.ogr_mod,c1.ogr_operacion,c1.ogr_cliente,c1.ogr_numgaran,w_gar_valorcomercialope,w_gar_valorcomercialope);
     end if;
    end loop;
--para ver el tipo de garantia
--
---fin v2
    po_garadminisblefisa := nvl(w_valor_admisiblefisa,0) + nvl(w_valor_admisiblefisa_v2,0);
    po_garadminisble := nvl(w_valor_admisible,0) + nvl(w_valor_admisible_v2,0);
    --return nvl(w_valor_garantizado,0) + nvl(w_valor_garantizado_v2,0);
  end ;
--fin distribucuin
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Función que devuelve la calificación de mora actual (JAV hd2875)
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure calif_mora(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,p_calif out number) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
    w_calificacionini    tpre_rangodetcalif.rad_tipocalif%type;
    w_howmanycalif NUMBER;
    w_havecomprom NUMBER;
  begin
	select distinct par_codigo
      into w_parcodigo
	 from tpre_paramcaliftipo
    where par_tabtipocredito = 244
      and par_tipocredito = 20;--p_tipocredito; --mora nOV 2017

    select pcp_tiporango
      into w_tiporango
     from tpre_paramcalifprev
	 where pcp_parcodigo = w_parcodigo
  	   and pcp_tabtipocalifprev = 253
  	   and pcp_tipocalifprev = 1 --mora
  	   and pcp_hasta is null;
--
    begin -- Definir la calificación de la operación
      select distinct pct_rancodigo
      	into w_codigorango
        from tpre_parrancaltipo,tpre_rangodetcalif
       where pct_califprev = 'C'
         and pct_mod = p_mod
         and pct_parcodigo = w_parcodigo
         and pct_mod = rad_mod
         and pct_rancodigo = rad_rancodigo
         and pct_califprev = rad_califprev
         and rad_tiporango = w_tiporango;
    exception
      when no_data_found then
        raise_application_error(-20465,'No está parametrizado Grupo '' cód '|| to_char(w_parcodigo)||
        					 ' Mód '|| to_char(p_mod)||' Cta:'||to_char(p_cuenta)||' TipoRa:'||to_char(w_tiporango));
      when too_many_rows then
        raise_application_error(-20465,'Parametrizado más de un Grupo '||' cód '|| w_parcodigo||
        					 ' Mód '||p_mod);
    end;
		begin
           select rad_tipocalif
           into w_calificacion
           from tpre_rangodetcalif
           where rad_califprev = 'C'
           and rad_mod = p_mod
           and rad_rancodigo = w_codigorango
           --and nvl(rad_garhip,'N') = decode(nvl(p_garhip,'N'),'Y','S','N')
           and rad_tiporango = w_tiporango
           and p_diasmora between rad_inicio and rad_final;
        exception
           when no_data_found then
              raise_application_error(-20465,'No está parametrizado la '||' para el código '|| w_parcodigo||
              ' Módulo '||p_mod||' w_codrango:'||w_codigorango||' w_tiporango:'||w_tiporango||' p_diasmora:'||p_diasmora);
           when too_many_rows then
              raise_application_error(-20465,'Parametrizado más de una '||' para el código '|| w_parcodigo||
         					 ' Módulo '||p_mod);
        end;
        /*se agrega esta parte para los prestamos nuevo bajo linea deben heredar la calificacion del anterior
          prestamo*/
          SELECT COUNT(*)
            INTO w_howmanycalif
            FROM TLEG_OPERACIONCALIF
           WHERE lca_cuenta = p_cuenta
             --AND lca_fecha < p_fecha;
			 AND lca_fecha between add_months(p_fecha,-12) and p_fecha-1;
          IF w_howmanycalif = 0 THEN
             BEGIN
             SELECT PRE_NUMCOMPROM
               INTO w_havecomprom
              FROM TPRE_PRESTAMOS
              WHERE PRE_CREDITO = p_cuenta;
              w_calificacionini:=0;
              IF w_havecomprom IS NOT NULL THEN
                 SELECT CCF_CALIFICA
                   INTO w_calificacionini
                   FROM TLEG_CALIFICACOMPROM
				   WHERE CCF_FECHA = (SELECT MAX(CCF_FECHA)
				   					    FROM TLEG_CALIFICACOMPROM
		                               WHERE CCF_FECHA < P_FECHA
		                                 AND CCF_CUENTA  = w_havecomprom
			                             AND CCF_TABTIPOCALIFICA = 253
			                             AND CCF_TIPOCALIFICA = 14)
			         AND CCF_CUENTA  = w_havecomprom
			         AND CCF_TABTIPOCALIFICA = 253
			         AND CCF_TIPOCALIFICA = 14;
			         IF w_calificacionini > w_calificacion THEN
			            w_calificacion:=w_calificacionini;
			         END IF;
              END IF;
             EXCEPTION
               WHEN OTHERS THEN
                 NULL;
             END;
          END IF;
        /*fin*/
        p_calif:= w_calificacion;                              
       update tpre_califdettipos--Nov 2017
          set  cdt_calificacion  = w_calificacion
         where CDT_FECHA = p_fecha
           and CDT_TIPO = p_tipo
           and CDT_CODCLI = p_cliente
           and CDT_MOD = p_mod
           and CDT_CUENTA = p_cuenta
           and CDT_TIPOCALIF  = 1;
         IF SQL%NOTFOUND THEN  
	        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
						           cdt_tabcalificacion, cdt_calificacion)
	        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,1,245,w_calificacion);
		 END IF;        
  exception
  	when no_data_found then
  	  raise_application_error(-20465,'No existe calificación por mora '||p_cuenta||' Tipcredito:'||p_tipocredito||' w_parcodigo:'||w_parcodigo||'^'||sqlerrm);
    when others then
      raise_application_error(-20465,'Calif.Mora '||substr(sqlerrm,1,120));
  end calif_mora;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Actualiza la calificacion de los prestamos por sobregiro con la calif de otro credito comercial
  --que tenga menor calificación
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure upd_calif_sob(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                          p_cuenta in number,p_mod in number,p_tipocredito in number,
                          p_calif out number) is
  	w_calif tpre_califdettipos.cdt_calificacion%type:=0;
  	w_calif_mora tpre_califdettipos.cdt_calificacion%type:=0;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	 number(4);
    w_tiporango      tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion   tpre_rangodetcalif.rad_tipocalif%type;
    w_otroscreditos  number:=0;--ajuste 2008/09/04
    --w_calif_mora number;
    w_statuscre varchar2(1);
  begin
  select max(cdt_calificacion),count(*)
    into w_calif,w_otroscreditos
 from tleg_operacioncalif,tpre_califdettipos
 where cdt_fecha = lca_fecha
   and cdt_tipo = lca_tipo
   and cdt_cuenta = lca_cuenta
   and cdt_tipocalif = 14  --tipo de calificacion final
   and nvl(lca_desobregiro,0) = 'N'
   and lca_codtipocredito in ('C','M','D')--solo se busca la comercial-mayore deudor o menor deudor, nov 2017 I Mediano
   and cdt_fecha = p_fecha
   and cdt_tipo = p_tipo
   and lca_codcli = p_cliente;

  select max(cdt_calificacion)
    into w_calif_mora
 from tleg_operacioncalif,tpre_califdettipos
 where cdt_fecha = lca_fecha
   and cdt_tipo = lca_tipo
   and cdt_cuenta = lca_cuenta
   and cdt_tipocalif = 1  --para encontrar la calif por mora si es peor que al anterior
   --and nvl(lca_desobregiro,0) = 'N'
   and lca_codtipocredito in ('C','M','D')--solo se busca la comercial-mayore deudor o menor deudor nov 2017 I MEDIANO
   and cdt_fecha = p_fecha
   and cdt_tipo = p_tipo
   and lca_codcli = p_cliente
   and lca_cuenta = p_cuenta;--el sobregiro
   --si no existen otros creditos c o m la calificacion es E para sobregiros
   select lca_status
     into w_statuscre
     from tleg_operacioncalif
    where lca_fecha = p_fecha
      and lca_tipo = p_tipo
      and lca_cuenta = p_cuenta;
   if nvl(w_otroscreditos,0) =  0 then
      w_calif:=5;
   end if;

   if nvl(w_calif_mora,0) > nvl(w_calif,0) then
     w_calif:=w_calif_mora;
   end if;
--si tienen un crédito existente heredan la misma calificación final de otrso creditos  siempre y cuando ese sobregiro no este vencido
   if nvl(w_calif,0) <> 0 then
     if w_statuscre not in ('V','N') THEN
      update tpre_califdettipos
         set cdt_calificacion = w_calif
       where cdt_fecha = p_fecha
         and cdt_tipo  = p_tipo
         and cdt_tipocalif = 14
         and cdt_cuenta = p_cuenta;
      end if;
   end if;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- se busca la peor calificacion del cliente y se coloca a todos sus crédito.
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure upd_calif_unica(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                            p_calif out number,p_codtipocartera in varchar2) is
  	w_calif tpre_califdettipos.cdt_calificacion%type:=0;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
  begin
  select max(cdt_calificacion)
    into w_calif
 from tleg_operacioncalif,tpre_califdettipos
 where cdt_fecha = lca_fecha
   and cdt_tipo = lca_tipo
   and cdt_cuenta = lca_cuenta
   and cdt_tipocalif = 14  --tipo de calificacion final
   --and nvl(lca_calificado,'N') ='N'
   --and nvl(lca_desobregiro,0) = 'N'
   --and lca_codtipocredito in ('C','M')--solo se busca la comercial-mayore deudor o menor deudor
   and nvl(lca_capital,0)>=1--los menores a 1 son prestamos con lineas que no se pueden cancelar por ser no revolventes
   and cdt_fecha = p_fecha
   and cdt_tipo = p_tipo
   and decode(lca_codtipocredito,'M','C','D','C',lca_codtipocredito)=p_codtipocartera --mayores y menores dedudores son una sola para este caso--por BDI  comento, unica por cartera Nov 2017
   and lca_codcli = p_cliente;
   if nvl(w_calif,0) <> 0 then
           p_calif:=w_calif;--nuevo
      update tpre_califdettipos
         set cdt_calificacion = w_calif
       where cdt_fecha = p_fecha
         and cdt_tipo  = p_tipo
         and cdt_tipocalif = 14
         and cdt_codcli = p_cliente
         and cdt_cuenta in (select lca_cuenta
                              from tleg_operacioncalif
                            where lca_codcli = p_cliente
                              and lca_fecha = p_fecha
                              and lca_tipo = p_tipo
                              and decode(lca_codtipocredito,'M','C','D','C',lca_codtipocredito)=p_codtipocartera--Nov 2017
                              and nvl(lca_calificado,'N') ='N');
         update tpre_califica
            set cal_califactual = w_calif
          where cal_fecha = p_fecha
            and cal_tipo = p_tipo
            and cal_codcli = p_cliente
            and cal_codcli in (select lca_codcli
                              from tleg_operacioncalif
                            where lca_codcli = p_cliente
                              and lca_fecha = p_fecha
                              and lca_tipo = p_tipo
                              and decode(lca_codtipocredito,'M','C','D','C',lca_codtipocredito)=p_codtipocartera--Nov 2017
                              and nvl(lca_calificado,'N') ='N');
         update tpre_calificadet
            set cad_califcuenta = w_calif
          where cad_fecha = p_fecha
            and cad_tipo =  p_tipo
            and cad_codcli = p_cliente
            and cad_cuenta in (select lca_cuenta
                              from tleg_operacioncalif
                            where lca_codcli = p_cliente
                              and lca_fecha = p_fecha
                              and lca_tipo = p_tipo
                              and decode(lca_codtipocredito,'M','C','D','C',lca_codtipocredito)=p_codtipocartera--Nov 2017
                              and nvl(lca_calificado,'N') ='N');
   end if;
  end;                     
 --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Funcion que devuelve la calificacion por perdidas Riesgo medianos deudores y mayores deudores
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure calif_riesgo_medmay(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                            p_cuenta in number,p_mod in number,p_tipocredito in number, 
                            p_codtipocredito in varchar2,p_califmora in number,p_califcapa in number,
                            p_calif out number) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
    v_continuar varchar2(1):='S';
  begin
   if p_codtipocredito = 'D' then
		begin
           select CCA_CALIFICAPERDIDA
           into w_calificacion
           from TLEG_CLICALIFRIESGO_MEDIANO
           where cca_fecha = p_fecha
           and cca_tipo = p_tipo
           and cca_codcli = p_cliente;
        exception
           when no_data_found then
              raise_application_error(-20465,'Cliente No dispone de Calificacion Por Pérdida de Pago(2/6001) '|| p_cliente||
              ' Modulo '||p_mod);
           when too_many_rows then
              raise_application_error(-20465,'Tiene mas de una deficinion Por Perdida(2/6001) de  '|| p_cliente||
         					 ' Modulo '||p_mod);
        end;
        p_calif:= w_calificacion;
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,16,245,w_calificacion);
   elsif p_codtipocredito = 'C' then
		begin
           
           w_calificacion:=Calcula_CalificaRiesgoMayores(p_califmora,p_califcapa) ;
           
        exception
           when no_data_found then
              raise_application_error(-20465,'Cliente No dispone de Calificacion Por Riesgo mayores '|| p_cliente||
              ' Modulo '||p_mod);
           when too_many_rows then
              raise_application_error(-20465,'Tiene mas de una deficinion Por Riesgo mayores de  '|| p_cliente||
         					 ' Modulo '||p_mod);
        end;
        p_calif:= w_calificacion;
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,17,245,w_calificacion);           
   end if;
  exception
  	when no_data_found then
  	  raise_application_error(-20465,'No existe calificacion por Riesgo perdidas  '||p_cuenta);
    when others then
      raise_application_error(-20465,'Calif.Riesgo Perdidas '||substr(sqlerrm,1,80));
  end calif_riesgo_medmay;  
  
  --   
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Función que devuelve la calificacion por capacidad de pago.
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure calif_capacidad(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                            p_cuenta in number,p_mod in number,p_tipocredito in number, p_codtipocredito in varchar2,
                            p_calif out number) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
    v_continuar varchar2(1):='S';
  begin
	select distinct par_codigo
      into w_parcodigo
	 from tpre_paramcaliftipo
    where par_tabtipocredito = 244
      and par_tipocredito = p_tipocredito; --mora
      DBMS_OUTPUT.PUT_LINE('w_parcodigo:'||w_parcodigo);
  begin
    select pcp_tiporango
      into w_tiporango
     from tpre_paramcalifprev
	 where pcp_parcodigo = w_parcodigo
  	   and pcp_tabtipocalifprev = 253
  	   and pcp_tipocalifprev = 9 --mora
  	   and pcp_hasta is null;
    v_continuar :='S';
          DBMS_OUTPUT.PUT_LINE('w_tiporango:'||w_tiporango);

  exception
    when no_data_found then
    if  p_codtipocredito = 'C' then
              raise_application_error(-20465,'Cliente No dispone de Capacidad de Pago Cli: '|| p_cliente||
              ' Es mayor deudor Módulo '||p_mod);
    else
        v_continuar :='N';
    end if;
  end;

   if  v_continuar = 'S' then
		begin
           select cca_califica
           into w_calificacion
           from tleg_clicalifcapacidad
           where cca_fecha = p_fecha
           and cca_tipo = p_tipo
           and cca_codcli = p_cliente;
           DBMS_OUTPUT.PUT_LINE('w_calificacion:'||w_calificacion||' p_cliente:'||p_cliente);
        exception
           when no_data_found then
              raise_application_error(-20465,'Cliente No dispone de Capacidad de Pago '|| p_cliente||
              ' Módulo '||p_mod);
           when too_many_rows then
              raise_application_error(-20465,'Tiene mas de una deficinion de capacidad de  '|| p_cliente||
         					 ' Módulo '||p_mod);
        end;
        p_calif:= w_calificacion;
        DBMS_OUTPUT.PUT_LINE('p_calif:'||p_calif||' p_cliente:'||p_cliente);
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,9,245,w_calificacion);
   end if;
  exception
  	when no_data_found then
  	  raise_application_error(-20465,'No existe calificación por capacidad  '||p_cuenta);
    when others then
      raise_application_error(-20465,'Calif.capacidad de Pago '||substr(sqlerrm,1,80));
  end calif_capacidad;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Peor Calificacion SiB
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure calif_sib(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,
                       p_califinicial in number,p_calif out number) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
    w_calificacionfija    tpre_rangodetcalif.rad_tipocalif%type;
  begin
--
        ---para encontrar la peor calificacion sib
          begin
            w_calificacion:=0;
           select max(des_codigo)
             into w_calificacion
             from TRIESGO_CLIENTESISTEMA,tgen_desctabla
            where nvl(PEOR_CALIFICA,'A')=des_descripcion
              and des_codtab = 245
              and rnc = (select cli_identifica  from tcli_persona where cli_codigo = p_cliente)
              and fechacarga = (select max(fechacarga)
		                                  from triesgo_clienteSistema
		                                  where replace(RNC,' ','') = (select cli_identifica
		                                           from tcli_persona
		                                           where cli_codigo = p_cliente));--ojo cuando vayan a reprocesar con fechas hacia atras
           IF nvl(w_calificacion,0) = 0 then
             w_calificacion:=1;
           end if;
          exception
             when others then
                w_calificacion:=1;
          end;

        --
        p_calif:= w_calificacion;
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,15,245,w_calificacion);
  exception
  	when no_data_found then
  	  raise_application_error(-20465,'No existe calificación de la SIB  '||p_cuenta);
    when others then
      raise_application_error(-20465,'Calif.Peor SiB '||p_cliente||'^'||substr(sqlerrm,1,80));

  end;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Calificación Por reestructuracion
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure calif_restruc(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,
                       p_califinicial in number,p_calif out number) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
    w_calificacionfija    tpre_rangodetcalif.rad_tipocalif%type;
  begin
	select distinct par_codigo
      into w_parcodigo
	 from tpre_paramcaliftipo
    where par_tabtipocredito = 244
      and par_tipocredito = p_tipocredito; --mora

    select pcp_tiporango
      into w_tiporango
     from tpre_paramcalifprev
	 where pcp_parcodigo = w_parcodigo
  	   and pcp_tabtipocalifprev = 253
  	   and pcp_tipocalifprev = 2 --reestructuracion
  	   and pcp_hasta is null;
--
    begin -- Definir la calificación de la operación
      select distinct pct_rancodigo
      	into w_codigorango
        from tpre_parrancaltipo,tpre_rangodetcalif
       where pct_califprev = 'C'
         and pct_mod = p_mod
         and pct_parcodigo = w_parcodigo
         and pct_mod = rad_mod
         and pct_califprev = rad_califprev
         and pct_rancodigo = rad_rancodigo
         and rad_tiporango = w_tiporango;
    exception
      when no_data_found then
        raise_application_error(-20465,'No está parametrizado Grupo '' cód '|| to_char(w_parcodigo)||
        					 ' Mód '|| to_char(p_mod));
      when too_many_rows then
        raise_application_error(-20465,'Parametrizado más de un Grupo '||' cód '|| w_parcodigo||
        					 ' Mód '||p_mod);
    end;
		begin
           select rad_tipocalif
           into w_calificacion
           from tpre_rangodetcalif
           where rad_califprev = 'C'
           and rad_mod = p_mod
           and rad_rancodigo = w_codigorango
           --and nvl(rad_garhip,'N') = decode(nvl(p_garhip,'N'),'Y','S','N')
           and rad_tiporango = w_tiporango
           and nvl(p_diasmora,0) between rad_inicio and rad_final;
        exception
           when no_data_found then
              raise_application_error(-20465,'No está parametrizado la '||' para el código '|| w_parcodigo||
              ' Módulo '||p_mod);
           when too_many_rows then
              raise_application_error(-20465,'Parametrizado más de una '||' para el código '|| w_parcodigo||
         					 ' Módulo '||p_mod);
        end;
        ---para determinar si tiene calificacion fija
          begin
            w_calificacionfija:=0;
           select ccf_califica
             into w_calificacionfija
             from tleg_calificafija
            where ccf_cuenta = p_cuenta
              and ccf_tipocalifica = 2;
            if w_calificacion < w_calificacionfija then
               w_calificacion:= w_calificacionfija;
            end if;
          exception
             when others then
                null;
          end;

        --
        if w_calificacion < p_califinicial then
           w_calificacion:= p_califinicial;
        end if;
        p_calif:= w_calificacion;
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,2,245,w_calificacion);
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Calificación Por reestructuracion para los que salieron de reestructurado
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure calif_restruc_out(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_codtipocredito in varchar2,
                       p_diasmora in number,
                       p_califinicial in number,p_cuoarrdesd in out number,
                       p_calif out number,
                       p_fechasalree in date) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
    w_calificacionaux    tpre_rangodetcalif.rad_tipocalif%type;
    w_calificacionaux_1    tpre_rangodetcalif.rad_tipocalif%type;
    w_calificacionfija    tpre_rangodetcalif.rad_tipocalif%type;
    w_fechacuoini date;
    w_fechacuoiniaux date;
    w_fechacuofin date;
    w_diasatrasocap number;
    w_diasatrasoint number;
    w_diasatrasototal number;
    w_vencio varchar2(1):='N';
  begin
	select distinct par_codigo
      into w_parcodigo
	 from tpre_paramcaliftipo
    where par_tabtipocredito = 244
      and par_tipocredito = 20;--p_tipocredito; --mora

    select pcp_tiporango
      into w_tiporango
     from tpre_paramcalifprev
	 where pcp_parcodigo = w_parcodigo
  	   and pcp_tabtipocalifprev = 253
  	   and pcp_tipocalifprev = 2 --reestructuracion
  	   and pcp_hasta is null;
--
    begin -- Definir la calificación de la operación
      select distinct pct_rancodigo
      	into w_codigorango
        from tpre_parrancaltipo,tpre_rangodetcalif
       where pct_califprev = 'C'
         and pct_mod = p_mod
         and pct_parcodigo = w_parcodigo
         and pct_mod = rad_mod
         and pct_califprev = rad_califprev
         and pct_rancodigo = rad_rancodigo
         and rad_tiporango = w_tiporango;
    exception
      when no_data_found then
        raise_application_error(-20465,'No está parametrizado Grupo '' cód '|| to_char(w_parcodigo)||
        					 ' Mód '|| to_char(p_mod));
      when too_many_rows then
        raise_application_error(-20465,'Parametrizado más de un Grupo '||' cód '|| w_parcodigo||
        					 ' Mód '||p_mod);
    end;
		begin
           select rad_tipocalif
           into w_calificacion
           from tpre_rangodetcalif
           where rad_califprev = 'C'
           and rad_mod = p_mod
           and rad_rancodigo = w_codigorango
           --and nvl(rad_garhip,'N') = decode(nvl(p_garhip,'N'),'Y','S','N')
           and rad_tiporango = w_tiporango
           and nvl(p_diasmora,0) between rad_inicio and rad_final;
        exception
           when no_data_found then
              raise_application_error(-20465,'No está parametrizado la '||' para el código '|| w_parcodigo||
              ' Módulo '||p_mod);
           when too_many_rows then
              raise_application_error(-20465,'Parametrizado más de una '||' para el código '|| w_parcodigo||
         					 ' Módulo '||p_mod);
        end;
        if w_calificacion < p_califinicial then
           w_calificacion:= p_califinicial;
        end if;
        w_calificacionaux_1 := w_calificacion; --por BLH
        --al salir de rrestructura ya automaticamente disminuye su calificación
	         --w_calificacion:= w_calificacion - 1;
        --esta es la calificacion cuando ingreso a reestructurado

        --esta es la calificacion cuando ingreso a reestructurado
        p_calif:= w_calificacion;
        w_calificacionaux := w_calificacion; --por BLH
        w_vencio :='N';
        --fin
        select cuo_fecha
          into w_fechacuoini
         from tpre_cuotas
         where cuo_credito = p_cuenta
           and cuo_numcuo = p_cuoarrdesd;
         w_fechacuoiniaux:= w_fechacuoini;
     --la tercera cuota si ha pagado mejora la calificacion
       --p_cuoarrdesd:=(p_cuoarrdesd + 3) - 1;--ya disminui al salir

     begin
        select cuo_fecha
          into w_fechacuofin
         from tpre_cuotas
         where cuo_credito = p_cuenta
           and cuo_numcuo = (p_cuoarrdesd + 3) - 1; --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
   exception
     when no_data_found then
        select cuo_fecha
          into w_fechacuofin
         from tpre_cuotas
         where cuo_credito = p_cuenta
           and cuo_numcuo = (select max(cuo_numcuo)
           					   from tpre_cuotas
                               where cuo_credito = p_cuenta
                                 and cuo_numcuo <=(p_cuoarrdesd + 3) - 1); --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
        w_vencio :='S';
   end ;
   --como ya disminuyo una calificacin la fecha de inicio es la fecha de salida
     --w_fechacuoini := p_fechasalree;

         --primera disminucion de calificacon
    if  w_vencio ='N' then
         select nvl(max(acr_diasv),0), nvl(max(acr_diasint),0)
         into w_diasatrasocap,w_diasatrasoint
           from tpre_accrual
          where acr_credito =  p_cuenta
            and acr_fecha between w_fechacuoini and w_fechacuofin;
          if  nvl(w_diasatrasocap,0) >= nvl(w_diasatrasoint,0)   then
              w_diasatrasototal := w_diasatrasocap;
          else
              w_diasatrasototal := w_diasatrasoint;
          end if;
         if nvl(w_diasatrasototal,0) <= 30 then --si es mayor a 30 quiere decir quye paso a vencido entonces no se puede bajar la calificacion
            w_calificacion:=w_calificacion-1;
         else
                   w_vencio :='S';
         end if;
         if w_calificacion<=0 then
            w_calificacion:=1;
         end if ;
         p_calif:= w_calificacion; --le bajo a d
 end if;
         --segunda disminucion de calificacon
       if w_calificacion > 1 and w_vencio ='N' then
       begin
        select cuo_fecha
          into w_fechacuofin
         from tpre_cuotas
         where cuo_credito = p_cuenta
           and cuo_numcuo = (p_cuoarrdesd + 6) - 1; --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
   exception
     when no_data_found then
        select cuo_fecha
          into w_fechacuofin
         from tpre_cuotas
         where cuo_credito = p_cuenta
           and cuo_numcuo = (select max(cuo_numcuo)
           					   from tpre_cuotas
                               where cuo_credito = p_cuenta
                                 and cuo_numcuo <=(p_cuoarrdesd + 6) - 1); --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
        w_vencio :='S';
   end ;
    if  w_vencio ='N' then
         select nvl(max(acr_diasv),0), nvl(max(acr_diasint),0)
         into w_diasatrasocap,w_diasatrasoint
           from tpre_accrual
          where acr_credito =  p_cuenta
            and acr_fecha between w_fechacuoini and w_fechacuofin;
          if  nvl(w_diasatrasocap,0) >= nvl(w_diasatrasoint,0)   then
              w_diasatrasototal := w_diasatrasocap;
          else
              w_diasatrasototal := w_diasatrasoint;
          end if;
         if nvl(w_diasatrasototal,0) <= 30 then --si es mayor a 30 quiere decir quye paso a vencido entonces no se puede bajar la calificacion
            w_calificacion:=w_calificacion-1;
         else
           w_vencio :='S';
         end if;
         if w_calificacion<=0 then
            w_calificacion:=1;
         end if ;
         p_calif:= w_calificacion;
       end if;--w_calificacion > 1
     end if;
      --tercera disminucion de calificacon
       if w_calificacion > 1 and w_vencio ='N' then    --le bajo a c
       begin
        select cuo_fecha
          into w_fechacuofin
         from tpre_cuotas
         where cuo_credito = p_cuenta
           and cuo_numcuo = (p_cuoarrdesd + 9) - 1; --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
	   exception
	     when no_data_found then
	        select cuo_fecha
	          into w_fechacuofin
	         from tpre_cuotas
	         where cuo_credito = p_cuenta
	           and cuo_numcuo = (select max(cuo_numcuo)
	           					   from tpre_cuotas
	                               where cuo_credito = p_cuenta
	                                 and cuo_numcuo <=(p_cuoarrdesd + 9) - 1); --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
	        w_vencio :='S';
	   end ;
    if  w_vencio ='N' then
         select nvl(max(acr_diasv),0), nvl(max(acr_diasint),0)
         into w_diasatrasocap,w_diasatrasoint
           from tpre_accrual
          where acr_credito =  p_cuenta
            and acr_fecha between w_fechacuoini and w_fechacuofin;
          if  nvl(w_diasatrasocap,0) >= nvl(w_diasatrasoint,0)   then
              w_diasatrasototal := w_diasatrasocap;
          else
              w_diasatrasototal := w_diasatrasoint;
          end if;
         if nvl(w_diasatrasototal,0) <= 30 then --si es mayor a 30 quiere decir quye paso a vencido entonces no se puede bajar la calificacion
            w_calificacion:=w_calificacion-1;
         else
            w_vencio :='S';
         end if;
         if w_calificacion<=0 then
            w_calificacion:=1;
         end if ;
         p_calif:= w_calificacion;
       end if;--w_calificacion > 1
     end if;
      --tercera disminucion de calificacon
       if w_calificacion > 1 and w_vencio ='N' then --le bajo a b
       begin
        select cuo_fecha
          into w_fechacuofin
         from tpre_cuotas
         where cuo_credito = p_cuenta
           and cuo_numcuo = (p_cuoarrdesd + 12) - 1; --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
	   exception
	     when no_data_found then
	        select cuo_fecha
	          into w_fechacuofin
	         from tpre_cuotas
	         where cuo_credito = p_cuenta
	           and cuo_numcuo = (select max(cuo_numcuo)
	           					   from tpre_cuotas
	                               where cuo_credito = p_cuenta
	                                 and cuo_numcuo <=(p_cuoarrdesd + 12) - 1); --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
	        w_vencio :='S';
	   end ;
    if  w_vencio ='N' then
         select nvl(max(acr_diasv),0), nvl(max(acr_diasint),0)
         into w_diasatrasocap,w_diasatrasoint
           from tpre_accrual
          where acr_credito =  p_cuenta
            and acr_fecha between w_fechacuoini and w_fechacuofin;
          if  nvl(w_diasatrasocap,0) >= nvl(w_diasatrasoint,0)   then
              w_diasatrasototal := w_diasatrasocap;
          else
              w_diasatrasototal := w_diasatrasoint;
          end if;
         if nvl(w_diasatrasototal,0) <= 30 then --si es mayor a 30 quiere decir quye paso a vencido entonces no se puede bajar la calificacion
            w_calificacion:=w_calificacion-1;
         else
           w_vencio :='S';
         end if;
         if w_calificacion<=0 then
            w_calificacion:=1;
         end if ;
         p_calif:= w_calificacion;
       end if;--w_calificacion > 1
   end if;
       if w_calificacion > 1 and w_vencio ='N' then --le bajo a a
      begin
        select cuo_fecha
          into w_fechacuofin
         from tpre_cuotas
         where cuo_credito = p_cuenta
           and cuo_numcuo = (p_cuoarrdesd + 15) - 1; --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
	   exception
	     when no_data_found then
	        select cuo_fecha
	          into w_fechacuofin
	         from tpre_cuotas
	         where cuo_credito = p_cuenta
	           and cuo_numcuo = (select max(cuo_numcuo)
	           					   from tpre_cuotas
	                               where cuo_credito = p_cuenta
	                                 and cuo_numcuo <=(p_cuoarrdesd + 3) - 1); --se considera de la cuota inicial , la ley dice 3 cuotas inicialmente deberiasmos parametrizar
	        w_vencio :='S';
	   end ;
    if  w_vencio ='N' then
         select nvl(max(acr_diasv),0), nvl(max(acr_diasint),0)
         into w_diasatrasocap,w_diasatrasoint
           from tpre_accrual
          where acr_credito =  p_cuenta
            and acr_fecha between w_fechacuoini and w_fechacuofin;
          if  nvl(w_diasatrasocap,0) >= nvl(w_diasatrasoint,0)   then
              w_diasatrasototal := w_diasatrasocap;
          else
              w_diasatrasototal := w_diasatrasoint;
          end if;
         if nvl(w_diasatrasototal,0) <= 30 then --si es mayor a 30 quiere decir quye paso a vencido entonces no se puede bajar la calificacion
            w_calificacion:=w_calificacion-1;
         end if;
         if w_calificacion<=0 then
            w_calificacion:=1;
         end if ;
         p_calif:= w_calificacion;
       end if;--w_calificacion > 1
     end if;
       if p_codtipocredito in ('C','O','M','H','D') then --NOV 2017
         if p_codtipocredito in ('O','M','H','D') then--NOV 2017
	          if w_calificacion = 1 then
	             w_calificacion:=2; --estos tipos de creditos no pueden llegar a tener una A cuando son reestructurados
	          end if;
          end if;
       else
               w_calificacion := w_calificacionaux; --por BLH
       end if;
       ---para determinar si tiene calificacion fija
       --para verificar si no se vuelve a atrasar
         select nvl(max(acr_diasv),0), nvl(max(acr_diasint),0)
           into w_diasatrasocap,w_diasatrasoint
           from tpre_accrual
          where acr_credito =  p_cuenta
            and acr_fecha between w_fechacuoiniaux and p_fecha;
          if  nvl(w_diasatrasocap,0) >= nvl(w_diasatrasoint,0)   then
              w_diasatrasototal := w_diasatrasocap;
          else
              w_diasatrasototal := w_diasatrasoint;
          end if;
         if nvl(w_diasatrasototal,0) > 31 then --si es mayor a 30 quiere decir quye paso a vencido entonces no se puede bajar la calificacion
            w_calificacion:=w_calificacionaux;
         end if;
        --fin

          begin
            w_calificacionfija:=0;
           select ccf_califica
             into w_calificacionfija
             from tleg_calificafija
            where ccf_cuenta = p_cuenta
              and ccf_tipocalifica = 2;
              --w_calificacion:=w_calificacionfija;
            if w_calificacion < w_calificacionfija then
               w_calificacion:= w_calificacionfija;
            end if;
          exception
             when others then
                null;
          end;
                p_calif:= w_calificacion;
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,2,245,w_calificacion);
  exception
   when others then
          raise_application_error (-20505,'Error en califica reestruc:'||p_cuenta||'-'||sqlerrm);
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calificacion inicial - resultado entre la calificacion de capacidad de pago y el historico de pago
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure calif_inicial(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_califmora in number, p_califcapacidad in number,
                       p_califriesgo in number,p_califinicial out number) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
  begin
       if p_califmora >  p_califcapacidad then
          w_calificacion := p_califmora;
       else
          w_calificacion := p_califcapacidad;
       end if;           
       --añadir la matriz 2, pag 34 rea
        --p_califinicial:= w_calificacion;
        --p_califinicial:= w_calificacion; feb 2018
        p_califinicial:=p_califriesgo;        
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,8,245,w_calificacion);      --8 como calificacion inicial
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calificacion monto cubierto
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  procedure calif_cubierto(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,p_montocub in number,
                       p_califinicial in number,v_codicion in number,p_calif out number) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
    w_diasvencontrato number:=0;
    w_capitalcub number:=0;
    w_demandajundicial varchar2(1);
    w_intacteje number:=0;
    w_codtipocredito varchar2(1);
    w_status  tpre_accrual.acr_status%type;
    w_statusint tpre_accrual.acr_statusint%type;
    w_desobregiro varchar2(1):='N';
 begin
  if nvl(p_montocub,0) > 0 then
      /*feb 2018
      select csc_calificacub
        into w_calificacion
       from tleg_califsaldocub--parametrizar en base a la matriz 6 pag 51 agregar condicion 2, 3 pag 52- para condicion 3 deberian usar la 2/2 estados financieron
       where csc_tabcalifica = 245
         and csc_califica = p_califinicial;*/  

       if v_codicion = 1 then
	      select csc_calificacub
	        into w_calificacion
	       from tleg_califsaldocub
	       where csc_tabcalifica = 245
	         and csc_califica = p_califinicial;
       elsif v_codicion = 2 then
	      select csc_calificacub2
	        into w_calificacion
	       from tleg_califsaldocub
	       where csc_tabcalifica = 245
	         and csc_califica = p_califinicial;
       elsif v_codicion = 3 then	      
	       select csc_calificacub3
		        into w_calificacion
		       from tleg_califsaldocub
		       where csc_tabcalifica = 245
		         and csc_califica = p_califinicial;
       end if;
         
  else
   --   w_calificacion:=p_califinicial; feb 2018
      /*select cse_calificacub
        into w_calificacion
       from tleg_califsaldoexp
       where cse_tabcalifica = 245
         and cse_califica = p_califinicial;*/
   --   w_calificacion:=p_califinicial;
    if v_codicion = 1 then
      select cse_calificacub
        into w_calificacion
       from tleg_califsaldoexp
       where cse_tabcalifica = 245
         and cse_califica = p_califinicial;
    elsif v_codicion = 2 then	      
      select csc_calificacub2
        into w_calificacion
       from tleg_califsaldoexp
       where cse_tabcalifica = 245
         and cse_califica = p_califinicial;
    
    elsif v_codicion = 3 then	      
      select csc_calificacub3
        into w_calificacion
       from tleg_califsaldoexp
       where cse_tabcalifica = 245
         and cse_califica = p_califinicial;
    
     end if;      
         
  end if;
  w_desobregiro:='N';
  -- controla si esta D o e o en demanda judicial o mas de 90 dias no debe ser menor que 20% o C
        begin
         select lca_diasvencontrato,lca_capitalcub,lca_demandajundicial,acr_intacteje,acr_status,acr_statusint,lca_codtipocredito,lca_desobregiro
           into w_diasvencontrato,w_capitalcub,w_demandajundicial,w_intacteje,w_status,w_statusint,w_codtipocredito,w_desobregiro
           from tleg_operacioncalif,tpre_accrual
         where lca_fecha = p_fecha
           and lca_tipo = p_tipo
           and lca_cuenta = p_cuenta
           and acr_credito = lca_cuenta
           and acr_fecha = lca_fecha;
        exception
         when others then
           w_diasvencontrato:=0;
        end;

      if w_codtipocredito in ('C','M','D') then --Nov 2017
           /*
	        if nvl(w_diasvencontrato,0) > 0 and nvl(w_capitalcub,0) > 0 then
	           if w_calificacion < 3 then
	              w_calificacion:=3;--los vencidos por contratos siempre s 20 independiente del los dias de vencido - BDI
	           end if;
	        end if; */
        --creditos con D o E y si tienen vencidos +90 intereses o estan judicial minimo es 20%
        if (w_status = 'N' or w_statusint = 'N') or (w_demandajundicial='S') then
           if w_calificacion < 3 then
              w_calificacion:=3;--los vencidos por contratos siempre s 20 independiente del los dias de vencido - BDI
           end if;
         end if;
  --los casos de sobregiros ocasionales no documentados, si tienen un crédito existente heredan la misma calificación final
  --siempre y cuando ese sobregiro no este vencido y su calificación expuesta y cubierta sería E aprovisionando el 100%.
  --De no tener un crédito existente su calificación Final pasa a ser automáticamente E aprovisionando el 100%
        if w_desobregiro ='S' then
           w_calificacion:=5;
        end if;
        --if nvl(p_califinicial,0) in (4,5) then
        --if (nvl(w_intacteje,0) > 0 and nvl(w_capitalcub,0) > 0) or (w_demandajundicial='S') then
          -- if w_calificacion < 3 then
            --  w_calificacion:=3;--los vencidos por contratos siempre s 20 independiente del los dias de vencido - BDI
           --end if;
         --end if;
        --end if;
        end if;
  --      frin
      p_calif:= w_calificacion;
   update tpre_califdettipos
      set cdt_calificacion = w_calificacion
    where cdt_fecha = p_fecha
      and cdt_tipo = p_tipo
      and cdt_mod = p_mod
      and cdt_cuenta = p_cuenta
      and cdt_tipocalif=12;
      if sql%notfound then
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,12,245,w_calificacion);      --8 como calificacion inicial
      end if;
 end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calificacion monto expuesto
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  procedure calif_expuesto(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,
                       p_califinicial in number,v_codicion in number,p_calif out number) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
    w_desobregiro varchar2(1):='N';
 begin
      /* feb 2018
      select cse_calificacub
        into w_calificacion
       from tleg_califsaldoexp
       where cse_tabcalifica = 245
         and cse_califica = p_califinicial;*/
     if v_codicion = 1 then
      select cse_calificacub
        into w_calificacion
       from tleg_califsaldoexp
       where cse_tabcalifica = 245
         and cse_califica = p_califinicial;
    elsif v_codicion = 2 then	      
      select csc_calificacub2
        into w_calificacion
       from tleg_califsaldoexp
       where cse_tabcalifica = 245
         and cse_califica = p_califinicial;
    
    elsif v_codicion = 3 then	      
      select csc_calificacub3
        into w_calificacion
       from tleg_califsaldoexp
       where cse_tabcalifica = 245
         and cse_califica = p_califinicial;
    
     end if;      
         
  w_desobregiro:='N';
  -- ajuste 2008/09/04
        begin
         select lca_desobregiro
           into w_desobregiro
           from tleg_operacioncalif,tpre_accrual
         where lca_fecha = p_fecha
           and lca_tipo = p_tipo
           and lca_cuenta = p_cuenta
           and acr_credito = lca_cuenta
           and acr_fecha = lca_fecha;
        exception
         when others then
           w_desobregiro:='N';
        end;
  --los casos de sobregiros ocasionales no documentados, si tienen un crédito existente heredan la misma calificación final
  --siempre y cuando ese sobregiro no este vencido y su calificación expuesta y cubierta sería E aprovisionando el 100%.
  --De no tener un crédito existente su calificación Final pasa a ser automáticamente E aprovisionando el 100%
        if w_desobregiro ='S' then
           w_calificacion:=5;
        end if;

        p_calif:= w_calificacion;
   update tpre_califdettipos
      set cdt_calificacion = w_calificacion
    where cdt_fecha = p_fecha
      and cdt_tipo = p_tipo
      and cdt_mod = p_mod
      and cdt_cuenta = p_cuenta
      and cdt_tipocalif=13;

      if sql%notfound then
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,13,245,w_calificacion);      --8 como calificacion inicial
      end if;
 end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calificacion final monto expuesto
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  procedure calif_final(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,
                       p_califinicial in number) is
  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
 begin
        insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
        values(p_fecha,p_tipo,p_cliente,p_mod,p_cuenta,253,14,245,p_califinicial);      --califica final resultado
 end;                           
--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calificacion por mora 
 -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   procedure calificacion_cartera_mora ( p_fecha date, p_tipo varchar2 )   is
        w_paramcalifstatus tpre_paramcalif.pca_status%type;
        w_cuantasporprocesar number(6) := 0;
  cursor p is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado
        lca_montogarreal,lca_suc,lca_ofi,lca_desobregiro,lca_capitalcub,lca_capitalexp,lca_fechasalreest, lca_reescuotadesd,
        lca_diasvencontrato,LCA_CODTIPOCREDITOANTES,LCA_DIASMORACAPCOR,LCA_DIASMORAINTCOR
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and nvl(lca_calificado,'N') = 'N'
    and lca_codtipocredito in ('D') --Nov 2017    	
   Order by lca_codcli;
  cursor p_sob is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado
        lca_montogarreal,lca_suc,lca_ofi,lca_desobregiro,lca_fechasalreest, lca_reescuotadesd,
        LCA_DIASMORACAPCOR,LCA_DIASMORAINTCOR
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and nvl(lca_desobregiro,'N') = 'S'
    and nvl(lca_calificado,'N') = 'N'
   Order by lca_codcli;
  cursor p_unica is
  select distinct lca_fecha, lca_tipo, lca_codcli,decode(lca_codtipocredito,'M','C','D','C',lca_codtipocredito) lca_codtipocredito
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
   Order by lca_codcli;

  cursor p_anterioresnuevos is
  select lca_fecha, lca_tipo, lca_codcli,lca_cuenta,lca_esnuevo
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo  = p_tipo
    and lca_esnuevo = 'S'
    and lca_codtipocredito = 'C'
   Order by lca_codcli;
   w_diasmora number;                              
   w_diasmoracor number;                                 
   w_califmora number;
   w_califcapacidad number;
   w_califrestruc number;
   w_califinicial number;
   w_califcub number;
   w_califexp number;
   w_existeprev number:=0;
   v_nromeses number :=0;
   w_esnuevoanterior number :=0;
      w_califsib number;
  begin

   /* begin
      select pca_status
      into w_paramcalifstatus
      from tpre_paramcalif
      where pca_fecha = p_fecha
      and pca_tipo = p_tipo;
      if nvl(w_paramcalifstatus,'N') = 'P' then
        raise_application_error(-20501,'Ya esta procesada la Calificacion para la fecha ' ||
                                       to_char(p_fecha,'yyyy/mm/dd'));
      end if;
    exception
      when no_data_found then
        raise_application_error(-20502,'No esta parametrizada la Calificacion para la fecha ' ||
                                       to_char(p_fecha,'yyyy/mm/dd'));
    end;
    -- REVISAR SI EXISTEN DATOS PARA PROCESAR LA CALIFICACION
    begin
      w_cuantasporprocesar := pkg_precalifica_do.cuantas_operaciones ( p_fecha, p_tipo );
      if w_cuantasporprocesar = 0 then
        raise_application_error(-20503,'No existen datos a procesar para la fecha ' || to_char(p_fecha,'yyyy/mm/dd'));
      end if;
    exception
      when others then
        raise_application_error(-20503,'No existen datos a procesar para la fecha ' || to_char(p_fecha,'yyyy/mm/dd'));
    end; */
    --calificacion por mora o comporatamiento de pago
    begin
    INSERT INTO TPRE_PARAMCALIF
      (PCA_FECHA, PCA_TIPO, PCA_STATUS, PCA_PREVSTATUS)
    VALUES
      (p_fecha, p_tipo, 'P', 'P');
    exception
    when dup_val_on_index then
      null;
    end;
      v_nromeses :=0;
      begin
      select nvl(pda_nromeses,0)
        into v_nromeses
       from tpre_prevdefantparam
      where pda_anio = to_char(p_fecha,'yyyy')
        and pda_mes = to_char(p_fecha,'mm');
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
      if nvl(v_nromeses,0) = 3 then--periodo trismestral , deberiamos parametrizar el 3
		    select count(*)
		     into w_existeprev
		      from tpre_prevdettipos
		     where pdt_fecha = p_fecha
		       and pdt_tipo = p_tipo;
		    if nvl(w_existeprev,0) > 0 then
		          raise_application_error (-20505,'No Puede regenerar la calificacion  existe valores de Provision para el periodo: ');
		    end if;
		  delete tpre_califdettipos
		   where cdt_fecha = p_fecha
		     and cdt_tipo = p_tipo;

		   update tleg_operacioncalif
		      set lca_calificado = 'N'
		     where lca_fecha = p_fecha
		       and lca_tipo = p_tipo;
      else
	    select count(*)
	     into w_existeprev
	      from tpre_prevdettipos
	     where pdt_fecha = p_fecha
	       and pdt_tipo = p_tipo
	       and pdt_cuenta in (select lca_cuenta
		                          from tleg_operacioncalif
							      where lca_fecha = p_fecha
							        and lca_tipo = p_tipo
							        and lca_esnuevo = 'S');
	    if nvl(w_existeprev,0) > 0 then
	          raise_application_error (-20505,'No Puede regenerar la calificacion  existe valores de Provision para el periodo: ');
	    end if;
		  delete tpre_califdettipos
		   where cdt_fecha = p_fecha
		     and cdt_tipo = p_tipo
		     and cdt_cuenta in (select lca_cuenta
		                          from tleg_operacioncalif
							      where lca_fecha = p_fecha
							        and lca_tipo = p_tipo
							        and lca_esnuevo = 'S');

		   update tleg_operacioncalif
		      set lca_calificado = 'N'
		     where lca_fecha = p_fecha
		       and lca_tipo = p_tipo
		       and lca_esnuevo='S';
      end if;
     commit;
    for x in p loop
   w_califmora :=0;
   w_califcapacidad :=0;
   w_califrestruc :=0;
   w_califinicial :=0;
   w_califcub :=0;
   w_califexp :=0;
    if x.lca_diasmoracap > x.lca_diasmoraint then
        w_diasmora := x.lca_diasmoracap ;
    else
        w_diasmora := x.lca_diasmoraint ;
    end if;
    if x.lca_diasmoracapcor > x.lca_diasmoraintcor then
        w_diasmoracor := x.lca_diasmoracapcor ;
    else
        w_diasmoracor := x.lca_diasmoraintcor ;
    end if;

    if w_diasmoracor > w_diasmora then
        w_diasmora := w_diasmoracor;
    end if;
    
    begin
      insert into tpre_califica ( cal_fecha, cal_tipo, cal_codcli )
         values ( p_fecha, p_tipo, x.lca_codcli );
    exception
     when dup_val_on_index then null;
     end;
    begin
    insert into tpre_calificadet
            (cad_fecha,cad_tipo,cad_codcli,cad_mod,cad_cuenta,cad_prod,cad_tip,cad_califsist,
             cad_diasmora,cad_mon,cad_monto,cad_codsuc,cad_codofi,cad_status,cad_montogar,cad_prevreqdir,
             cad_califcuenta, cad_fechavcto,cad_prevconstitdir)
          values
            (p_fecha,p_tipo,x.lca_codcli,x.lca_mod,x.lca_cuenta, x.lca_prod, x.lca_tip,
             0,w_diasmora,x.lca_mon, nvl(x.lca_capital,0),x.lca_suc,x.lca_ofi,
             decode(x.lca_status,'E',1,'V',3,'N',4,1),1,0,1,sysdate,0);
    exception
     when dup_val_on_index then null;
    end;
  --historico de pagos--para los mayores deudores se analiza el mayor numero de dias
  --de los ultimos 12 fines de dia(meses)
    calif_mora(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, w_diasmora,w_califmora);
    commit;                
  --Para consumo he hipotecario es D directamente, dse debe tomar en cuenta que si el credito
  --fue reestructardo para consumo e hipotecario no puede regresar a A , para comerciales si puede regresar
  --hasta una A , siempre y cuando no hayan pasado a vencido.
  --esta parte es para los que actualmente estan en reestructurados
    end loop;
  end;
 
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Para separar un mayor o menor deudor
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure calificacion_cartera ( p_fecha date, p_tipo varchar2 )   is
        w_paramcalifstatus tpre_paramcalif.pca_status%type;
        w_cuantasporprocesar number(6) := 0;
  cursor p is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado
        lca_montogarreal,lca_suc,lca_ofi,lca_desobregiro,lca_capitalcub,lca_capitalexp,lca_fechasalreest, lca_reescuotadesd
        ,LCA_CODTIPOCREDITOANTES,LCA_DIASMORACAPCOR,LCA_DIASMORAINTCOR
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and nvl(lca_calificado,'N') = 'N'
   Order by lca_codcli;
  cursor p_sob is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado
        lca_montogarreal,lca_suc,lca_ofi,lca_desobregiro,lca_fechasalreest, lca_reescuotadesd,LCA_DIASMORACAPCOR,LCA_DIASMORAINTCOR
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and nvl(lca_desobregiro,'N') = 'S'
    and nvl(lca_calificado,'N') = 'N'
   Order by lca_codcli;
  cursor p_unica is
  select distinct lca_fecha, lca_tipo, lca_codcli,decode(lca_codtipocredito,'M','C','D','C',lca_codtipocredito) lca_codtipocredito
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
   Order by lca_codcli;
  cursor p_anterioresnuevos is
  select lca_fecha, lca_tipo, lca_codcli,lca_cuenta,lca_esnuevo
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo  = p_tipo
    and lca_esnuevo = 'S'
    and lca_codtipocredito = 'C'--solo para mayores deudores
    AND 1=0
   Order by lca_codcli;

   w_diasmora number;   
   w_diasmoracor number;      
   w_califmora number;
   w_califcapacidad number;
   w_califrestruc number;
   w_califsib number;
   w_califinicial number;
   w_califcub number;
   w_califexp number;
   w_existeprev number:=0;
   v_nromeses number :=0;
   w_esnuevoanterior number :=0;              
   
    w_califxriesgo number:=0;--nov 2017   
    v_codicion number :=0;--feb 2018   
  begin
    --calificacion por mora o comporatamiento de pago
    begin
    INSERT INTO TPRE_PARAMCALIF
      (PCA_FECHA, PCA_TIPO, PCA_STATUS, PCA_PREVSTATUS)
    VALUES
      (p_fecha, p_tipo, 'P', 'P');
    exception
    when dup_val_on_index then
      null;
    end;
      v_nromeses :=0;
      begin
      select nvl(pda_nromeses,0)
        into v_nromeses
       from tpre_prevdefantparam
      where pda_anio = to_char(p_fecha,'yyyy')
        and pda_mes = to_char(p_fecha,'mm');
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
      if nvl(v_nromeses,0) = 3 then--periodo trismestral , deberiamos parametrizar el 3
	    select count(*)
	     into w_existeprev
	      from tpre_prevdettipos
	     where pdt_fecha = p_fecha
	       and pdt_tipo = p_tipo;
	    if nvl(w_existeprev,0) > 0 then
	          raise_application_error (-20505,'No Puede regenerar la calificacion  existe valores de Provisión para el periodo: ');
	    end if;
		  delete tpre_califdettipos
		   where cdt_fecha = p_fecha
		     and cdt_tipo = p_tipo;

		   update tleg_operacioncalif
		      set lca_calificado = 'N'
		     where lca_fecha = p_fecha
		       and lca_tipo = p_tipo;
      else
	    select count(*)
	     into w_existeprev
	      from tpre_prevdettipos
	     where pdt_fecha = p_fecha
	       and pdt_tipo = p_tipo
	       and pdt_cuenta in (select lca_cuenta
		                          from tleg_operacioncalif
							      where lca_fecha = p_fecha
							        and lca_tipo = p_tipo
							        and lca_esnuevo = 'S');
	    if nvl(w_existeprev,0) > 0 then
	          raise_application_error (-20505,'No Puede regenerar la calificacion  existe valores de Provisión para el periodo: ');
	    end if;
		  delete tpre_califdettipos
		   where cdt_fecha = p_fecha
		     and cdt_tipo = p_tipo
		     and cdt_cuenta in (select lca_cuenta
		                          from tleg_operacioncalif
							      where lca_fecha = p_fecha
							        and lca_tipo = p_tipo
							        and lca_esnuevo = 'S');

		   update tleg_operacioncalif
		      set lca_calificado = 'N'
		     where lca_fecha = p_fecha
		       and lca_tipo = p_tipo
		       and lca_esnuevo='S';
      end if;
     commit;
    for x in p loop
     dbms_output.put_line('inicio calificacion:');
   w_califmora :=0;
   w_califcapacidad :=0;
   w_califrestruc :=0;
   w_califinicial :=0;
   w_califcub :=0;
   w_califexp :=0;
    if x.lca_diasmoracap > x.lca_diasmoraint then
        w_diasmora := x.lca_diasmoracap ;
    else
        w_diasmora := x.lca_diasmoraint ;
    end if;
    --mora corte
    if x.lca_diasmoracapcor > x.lca_diasmoraintcor then
        w_diasmoracor := x.lca_diasmoracapcor ;
    else
        w_diasmoracor := x.lca_diasmoraintcor ;
    end if;
                     
    if w_diasmoracor > w_diasmora then
        w_diasmora := w_diasmoracor ;
    end if;
    
    begin
      insert into tpre_califica ( cal_fecha, cal_tipo, cal_codcli )
         values ( p_fecha, p_tipo, x.lca_codcli );
    exception
     when dup_val_on_index then null;
     end;
    begin
    insert into tpre_calificadet
            (cad_fecha,cad_tipo,cad_codcli,cad_mod,cad_cuenta,cad_prod,cad_tip,cad_califsist,
             cad_diasmora,cad_mon,cad_monto,cad_codsuc,cad_codofi,cad_status,cad_montogar,cad_prevreqdir,
             cad_califcuenta, cad_fechavcto,cad_prevconstitdir)
          values
            (p_fecha,p_tipo,x.lca_codcli,x.lca_mod,x.lca_cuenta, x.lca_prod, x.lca_tip,
             0,w_diasmora,x.lca_mon, nvl(x.lca_capital,0),x.lca_suc,x.lca_ofi,
             decode(x.lca_status,'E',1,'V',3,'N',4,1),1,0,1,sysdate,0);
    exception
     when dup_val_on_index then null;
    end;
  --historico de pagos--para los mayores deudores se analiza el mayor numero de dias
  --de los ultimos 12 fines de dia(meses)
    calif_mora(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, w_diasmora,w_califmora);
  --Para consumo he hipotecario es D directamente, dse debe tomar en cuenta que si el credito
  --fue reestructardo para consumo e hipotecario no puede regresar a A , para comerciales si puede regresar
  --hasta una A , siempre y cuando no hayan pasado a vencido.
  --esta parte es para los que actualmente estan en reestructurados
  --if x.lca_fechasalreest is null then
  if nvl(x.lca_fechasalreest,to_date('2199/01/01','yyyy/mm/dd')) >= p_fecha then
     if x.lca_reestructurado = 'S' then
       calif_restruc(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,w_califinicial,w_califrestruc);
     end if;
  else
     if x.lca_reestructurado = 'S' then
       calif_restruc_out(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito,x.lca_codtipocredito,
                x.lca_diasreestruc,w_califinicial,
                x.lca_reescuotadesd,w_califrestruc,x.lca_fechasalreest);
     end if;
  end if;
 --para los que estuvieron en reestructurado se debe determinar la colificacion si han pagado o no a tiempo
    --capacidad de pago
    --capacidad de pago  --aqui debo cambiar para provision anterior
    --se quita este punto por e-mail de rosemary - dia 2013/11/13
    /*if x.lca_codtipocredito= 'C' and x.lca_codtipocreditoantes= 'M' then
       x.lca_codtipocredito:= 'M';
    end if;*/
       --nov 2017         
    if x.lca_codtipocreditoantes= 'D' then    
      calif_riesgo_medmay(p_fecha,p_tipo,x.lca_codcli,
                       x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_codtipocredito,
                       w_califmora,w_califcapacidad,
                       w_califxriesgo);
    end if;                       


    if x.lca_codtipocredito= 'C' then--mayores deudores - capacidad de pago
       calif_capacidad(p_fecha,p_tipo,x.lca_codcli,
                       x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_codtipocredito,w_califcapacidad);

                       --nov 2017
        calif_riesgo_medmay(p_fecha,p_tipo,x.lca_codcli,
                       x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_codtipocredito,
                       w_califmora,w_califcapacidad,
                       w_califxriesgo) ;

       calif_inicial(p_fecha,p_tipo,x.lca_codcli,
                     x.lca_cuenta,x.lca_mod,w_califmora, w_califcapacidad,w_califxriesgo,w_califinicial) ;

     if x.lca_reestructurado = 'S' then
       /*calif_restruc(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,w_califinicial,w_califrestruc);*/
      --siempre busco la peor calificación
      if w_califrestruc > w_califinicial then
         w_califinicial := w_califrestruc;
      end if;
     end if;
     --si el credito tiene otro comercial con mejor calificacion el sobregiro adquiere esa calificacion pero en lo expuesto y cubiero si se provisiona
     --el 100%
     if x.lca_desobregiro= 'S' then
        w_califinicial := 5;--calificacion E
     end if;
	    -- para saber calificacion de la SIB
	    w_califsib:=0;
	     --if x.lca_codtipocredito in ('C','M') then--mayores deudores - capacidad de pago
	     if x.lca_codtipocredito in ('C') then--mayores deudores - capacidad de pago	     servicio 263  solo para mayores deudores
	           calif_sib(p_fecha,p_tipo,x.lca_codcli,
	                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,w_califinicial,w_califsib);
	     end if;
	     --calif sib
	     --se quita xrosemary 20151214
        /*if  w_califsib > w_califinicial then
            w_califinicial:= w_califsib-1;
        end if;*/

       calif_final(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,w_califinicial);
    else
        if x.lca_reestructurado = 'S' then
           --siempre busco la peor calificación
           if w_califrestruc > w_califmora then
              w_califinicial := w_califrestruc;
           else
             w_califinicial := w_califmora;
           end if;
        else
             w_califinicial := w_califmora;
        end if;

	     if x.lca_desobregiro= 'S' then
        	w_califinicial := 5;--calificacion E  de alguna forma parametrizar
    	 end if;
    -- para saber calificacion de la SIB
    w_califsib:=0;
     --if x.lca_codtipocredito in ('C') then--mayores deudores - capacidad de pago
     if x.lca_codtipocredito in ('C') then--mayores deudores - capacidad de pago     servicio 263
           calif_sib(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,w_califinicial,w_califsib);
     end if;
    --calif sib
     --se quita x rosemary 20151214
        /*if  w_califsib > w_califinicial then
            w_califinicial:= w_califsib-1;
        end if;*/

       calif_final(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,w_califinicial);
    end if;

    if x.lca_codtipocredito in ('C','M','D','O','H') then--mayores deudores y menore deudore SYSAID 19081
      --if x.lca_capitalcub > 0 then esto aplica para la provison feb 2018
      v_codicion := fdom_condicion(p_fecha,x.lca_cuenta,x.lca_codcli,x.LCA_DIASMORACAP,x.LCA_DIASMORAINT ,x.LCA_DEMANDAJUNDICIAL);
      
       calif_cubierto(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,x.lca_capitalcub,w_califinicial,v_codicion,w_califcub);--feb 2018
      --end if;
      --if x.lca_capitalexp > 0 then
       calif_expuesto(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,w_califinicial,v_codicion,w_califexp); --feb 2018
      --end if;
    end if;
    end loop;
    --en este punto verifico si los nuevos vienen de meses anteriores para copiar la calificación anterior
    w_esnuevoanterior:=0;
    for x in p_anterioresnuevos loop
       select count(*)
         into w_esnuevoanterior
         from tleg_operacioncalif
        where lca_fecha < p_fecha
          and lca_tipo=p_tipo
          and lca_cuenta = x.lca_cuenta;
   if  nvl(w_esnuevoanterior,0) > 1 then
        delete tpre_califdettipos
         where cdt_fecha = p_fecha
           and cdt_tipo = p_tipo
           and cdt_cuenta = x.lca_cuenta;
         insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
       select p_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion
         from  tpre_califdettipos
         where cdt_fecha = (select max(lca_fecha) from tleg_operacioncalif where lca_fecha < p_fecha and lca_tipo=p_tipo and lca_cuenta = x.lca_cuenta)
           and cdt_tipo = p_tipo
           and cdt_cuenta = x.lca_cuenta;
    end if;
    end loop;
    --
    --Para calcular la mejor calificacion en el caso que el cliente tenga otro prestamos comercial
    --que el prestamos por sobregiro.       esto si esta al dia
    --si el el 6/14 esta atrasado y
    for x in p_sob loop
      upd_calif_sob(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito,w_califinicial);
    end loop;
    --para calcular la calificacion unica.
    for x in p_unica loop
      upd_calif_unica(p_fecha,p_tipo,x.lca_codcli,w_califinicial,x.lca_codtipocredito);
    end loop;
    commit;
    for x in p loop
    --Por la unificacion de la calificacion debo calcular nuevamente la calificacion cubierta y expuesta.
    if x.lca_codtipocredito in ('C','M','D','O','H') then--mayores deudores y menore deudore
      --if x.lca_capitalcub > 0 then esto aplica para la provison
      w_califinicial:=0;
      begin
      select cdt_calificacion
        into w_califinicial
        from tpre_califdettipos
       where cdt_fecha = p_fecha
         and cdt_tipo  = p_tipo
         and cdt_mod = x.lca_mod
         and cdt_cuenta = x.lca_cuenta
         and cdt_tipocalif = 14;--la calificacion fina
      exception
        when others then
        raise_application_error(-20502,'No Operacion no dispone de Calificación Final:'||x.lca_cuenta);
      end;
       calif_cubierto(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,x.lca_capitalcub,w_califinicial,v_codicion,w_califcub);--feb 2018
      --end if;
      --if x.lca_capitalexp > 0 then
       calif_expuesto(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_tipocredito, x.lca_diasreestruc,w_califinicial,v_codicion,w_califexp);--feb 2018
      --end if;
    end if;
      update tleg_operacioncalif
        set lca_calificado = 'S'
       where lca_fecha = p_fecha
         and lca_tipo = p_tipo
         and lca_mod = x.lca_mod
         and lca_cuenta = x.lca_cuenta;
        commit;
    end loop;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- copiar la calificacion unicamente
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   procedure copy_calificacion(p_fechafrom in date,p_tipofrom in varchar2,p_fechato in date,p_codtipocredito in varchar2) is
   cursor p00 is
       select p_fechato, cal_tipo, cal_codcli
      from tpre_califica
      where cal_fecha = p_fechafrom
       and cal_tipo = p_tipofrom
           and cal_codcli in (select lca_codcli
           					from tleg_operacioncalif
         					where lca_fecha = p_fechato
         					  and lca_codtipocredito = p_codtipocredito
         					  and lca_codtipocredito in ('C'))
       and cal_codcli in (select lca_codcli
         					from tleg_operacioncalif
         					where lca_fecha = p_fechafrom
         					  and lca_tipo = p_tipofrom
         					  and lca_codtipocredito = p_codtipocredito
                              and lca_codtipocredito in ('C'));

   cursor p0 is
     select  p_fechato,cad_tipo,cad_codcli,cad_mod,cad_cuenta,cad_prod,cad_tip,cad_califsist,
             cad_diasmora,cad_mon,cad_monto,cad_codsuc,cad_codofi,cad_status,cad_montogar,cad_prevreqdir,
             cad_califcuenta, cad_fechavcto,cad_prevconstitdir
       from tpre_calificadet
      where cad_fecha = p_fechafrom
        and cad_tipo  = p_tipofrom
        and cad_cuenta in (select lca_cuenta
           					from tleg_operacioncalif
         					where lca_fecha = p_fechafrom
         					  and lca_tipo = p_tipofrom
         					  and lca_codtipocredito = p_codtipocredito
         					  and lca_codtipocredito in ('C'))
           and cad_cuenta in (select lca_cuenta
           					from tleg_operacioncalif
         					where lca_fecha = p_fechato
         					  and lca_codtipocredito = p_codtipocredito
         					  and lca_codtipocredito in ('C'))
        and cad_cuenta not in (select pre_credito
                              from tpre_prestamos
                              where pre_fecance between p_fechafrom + 1 and p_fechato
                                and pre_fecance is not null);

    cursor p is
           select p_fechato, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion
         from  tpre_califdettipos
         where cdt_fecha = p_fechafrom
           and cdt_tipo = p_tipofrom
           and cdt_cuenta in (select lca_cuenta
           					from tleg_operacioncalif
         					where lca_fecha = p_fechafrom
         					  and lca_tipo = p_tipofrom
         					  and lca_codtipocredito = p_codtipocredito
         					  and lca_codtipocredito in ('C'))
           and cdt_cuenta in (select lca_cuenta
           					from tleg_operacioncalif
         					where lca_fecha = p_fechato
         					  and lca_codtipocredito = p_codtipocredito
         					  and lca_codtipocredito in ('C'))
        and cdt_cuenta not in (select pre_credito
                              from tpre_prestamos
                              where pre_fecance between p_fechafrom + 1 and p_fechato
                                and pre_fecance is not null);
   begin
     for x in p00 loop
          begin
           insert into tpre_califica ( cal_fecha, cal_tipo, cal_codcli )
                 values(p_fechato, x.cal_tipo, x.cal_codcli);
		    exception
		    when dup_val_on_index then
		      null;
		    end;
     end loop;
     for x in p0 loop
       begin
                   insert into tpre_calificadet
            (cad_fecha,cad_tipo,cad_codcli,cad_mod,cad_cuenta,cad_prod,cad_tip,cad_califsist,
             cad_diasmora,cad_mon,cad_monto,cad_codsuc,cad_codofi,cad_status,cad_montogar,cad_prevreqdir,
             cad_califcuenta, cad_fechavcto,cad_prevconstitdir)
        values(p_fechato,x.cad_tipo,x.cad_codcli,x.cad_mod,x.cad_cuenta,x.cad_prod,x.cad_tip,x.cad_califsist,
             x.cad_diasmora,x.cad_mon,x.cad_monto,x.cad_codsuc,x.cad_codofi,x.cad_status,x.cad_montogar,x.cad_prevreqdir,
             x.cad_califcuenta, x.cad_fechavcto,x.cad_prevconstitdir);
	    exception
	    when dup_val_on_index then
	      null;
	    end;
     end loop;
      for x in p loop
         begin
               insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
					           values(p_fechato, x.cdt_tipo, x.cdt_codcli, x.cdt_mod, x.cdt_cuenta, x.cdt_tabtipocalif, x.cdt_tipocalif,
					           x.cdt_tabcalificacion, x.cdt_calificacion);
              update tleg_operacioncalif
                set LCA_CALIFICADO= 'S'
               where lca_fecha = p_fechato
                 and lca_cuenta = x.cdt_cuenta;
     exception
         when others then
                      raise_application_error(-20465,'Error COPY:'||x.cdt_cuenta||' '||sqlerrm);
         end;
      end loop;
      commit;
   end;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- copiar la calificacion por capacidad de pago desde una fecha a otra
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   procedure copy_calificacionprovision(p_fechafrom in date,p_tipofrom in varchar2,p_fechato in date,p_codtipocredito in varchar2) is
   begin
    begin
    INSERT INTO TPRE_PARAMCALIF
      (PCA_FECHA, PCA_TIPO, PCA_STATUS, PCA_PREVSTATUS)
    VALUES
      (p_fechato, p_tipofrom, 'P', 'P');
    exception
    when dup_val_on_index then
      null;
    end;

    begin
    insert into tpre_califica ( cal_fecha, cal_tipo, cal_codcli )
    select p_fechato, cal_tipo, cal_codcli
      from tpre_califica
      where cal_fecha = p_fechafrom
       and cal_tipo = p_tipofrom
       and cal_codcli in (select lca_codcli
         					from tleg_operacioncalif
         					where lca_fecha = p_fechafrom
         					  and lca_tipo = p_tipofrom
         					  and lca_codtipocredito = p_codtipocredito
         					  and lca_codtipocredito in ('C','D'));
    exception
    when dup_val_on_index then
      null;
    end;
--
    begin
    insert into tpre_calificadet
            (cad_fecha,cad_tipo,cad_codcli,cad_mod,cad_cuenta,cad_prod,cad_tip,cad_califsist,
             cad_diasmora,cad_mon,cad_monto,cad_codsuc,cad_codofi,cad_status,cad_montogar,cad_prevreqdir,
             cad_califcuenta, cad_fechavcto,cad_prevconstitdir)
     select  p_fechato,cad_tipo,cad_codcli,cad_mod,cad_cuenta,cad_prod,cad_tip,cad_califsist,
             cad_diasmora,cad_mon,cad_monto,cad_codsuc,cad_codofi,cad_status,cad_montogar,cad_prevreqdir,
             cad_califcuenta, cad_fechavcto,cad_prevconstitdir
       from tpre_calificadet
      where cad_fecha = p_fechafrom
        and cad_tipo  = p_tipofrom
        and cad_cuenta in (select lca_cuenta
           					from tleg_operacioncalif
         					where lca_fecha = p_fechafrom
         					  and lca_tipo = p_tipofrom
         					  and lca_codtipocredito = p_codtipocredito
         					  and lca_codtipocredito in ('C','D'))
       and cad_cuenta not in (select pre_credito
                              from tpre_prestamos
                              where pre_fecance between p_fechafrom + 1 and p_fechato
                                and pre_fecance is not null);
    exception
    when dup_val_on_index then
      null;
    end;
--   
BEGIN
   insert into tleg_operacioncalif(lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_suc, lca_ofi,
                                   lca_gar, lca_riesgopais, lca_reestructurado, lca_desobregiro,lca_demandajundicial, lca_capital,
                                   lca_interes, lca_montogaradmi, lca_capitalcub, lca_capitalexp, lca_fecreestruc, lca_diasreestruc,
                                   lca_fecrenov, lca_codigopais, lca_diasmoracap, lca_diasmoraint, lca_tabtipocredito, lca_tipocredito,
                                   lca_codtipocredito, lca_status, lca_procesado, lca_montogarreal, lca_calificado, lca_provisionado,
                                   lca_montogaradmifisa, lca_esnuevo, lca_montopignorado, lca_nrorestruct, lca_montorestruc,
                                   lca_fecultrenovtasa, lca_nrorenovtasa, lca_fecultrenovplazo, lca_nrorenovplazo, lca_ejecutivo,
                                   lca_grupoeconomico, lca_fecultcal, lca_destinoeconomico, lca_destinofondos, lca_fechainsgar,
                                   lca_fechavenpolizas, lca_fechainglegal, lca_fechasalilegal, lca_numcomprom, lca_ciiu,
                                   lca_tipoamortiza, lca_fechasalreest, lca_reescuotadesd, lca_interescub, lca_interesexp,
                                   lca_tipogarantia, lca_diasvencontrato,lca_contingente,lca_consolidasistema,lca_codtipocreditoantes,lca_montoaprobado,LCA_MONTOAPROBADOMANUAL,
                                   LCA_MONTOCUOTAEXTRAOR,LCA_TIPOTASA,LCA_BALPROMDCAPMES,LCA_INTDEVENCORTE,LCA_COMCARGODEVCORTE,LCA_ESTRUCTURACIONCRE,
                                   LCA_ORIGENCREDITO,LCA_TIPORECURSO,LCA_DIASMORACAPCOR,LCA_DIASMORAINTCOR,LCA_FECREFIN)
    select p_fechato, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_suc, lca_ofi,
                                   lca_gar, lca_riesgopais, lca_reestructurado, lca_desobregiro,lca_demandajundicial, lca_capital,
                                   lca_interes, lca_montogaradmi, lca_capitalcub, lca_capitalexp, lca_fecreestruc, lca_diasreestruc,
                                   lca_fecrenov, lca_codigopais, lca_diasmoracap, lca_diasmoraint, lca_tabtipocredito, lca_tipocredito,
                                   lca_codtipocredito, lca_status, lca_procesado, lca_montogarreal, lca_calificado, 'N',
                                   lca_montogaradmifisa, 'N', lca_montopignorado, lca_nrorestruct, lca_montorestruc,
                                   lca_fecultrenovtasa, lca_nrorenovtasa, lca_fecultrenovplazo, lca_nrorenovplazo, lca_ejecutivo,
                                   lca_grupoeconomico, lca_fecultcal, lca_destinoeconomico, lca_destinofondos, lca_fechainsgar,
                                   lca_fechavenpolizas, lca_fechainglegal, lca_fechasalilegal, lca_numcomprom, lca_ciiu,
                                   lca_tipoamortiza, lca_fechasalreest, lca_reescuotadesd, lca_interescub, lca_interesexp,
                                   lca_tipogarantia, lca_diasvencontrato,lca_contingente,lca_consolidasistema,lca_codtipocreditoantes,lca_montoaprobado,LCA_MONTOAPROBADOMANUAL,
                                   LCA_MONTOCUOTAEXTRAOR,LCA_TIPOTASA,LCA_BALPROMDCAPMES,LCA_INTDEVENCORTE,LCA_COMCARGODEVCORTE,LCA_ESTRUCTURACIONCRE,
                                   LCA_ORIGENCREDITO,LCA_TIPORECURSO,LCA_DIASMORACAPCOR,LCA_DIASMORAINTCOR,LCA_FECREFIN
     from tleg_operacioncalif
     where lca_fecha = p_fechafrom
       and lca_tipo = p_tipofrom
       and lca_codtipocredito = p_codtipocredito
       and lca_codtipocredito in ('C','D')
       and lca_cuenta not in (select pre_credito
                              from tpre_prestamos
                              where pre_fecance between p_fechafrom + 1 and p_fechato
                              and pre_fecance is not null);
    exception
    when dup_val_on_index then
      null;
    end;
                              
--
BEGIN
 insert into tleg_opergaran(log_fecha, log_tipo, log_mod, log_operacion, log_cliente, log_numgaran,
             log_valoradmifisa, log_valoradmireal, log_valorgarantizadofisa, log_valorgarantizadoreal,
             log_saldoadmisiblefisa, log_saldoadmisiblereal)
  select p_fechato, log_tipo, log_mod, log_operacion, log_cliente, log_numgaran,
             log_valoradmifisa, log_valoradmireal, log_valorgarantizadofisa, log_valorgarantizadoreal,
             log_saldoadmisiblefisa, log_saldoadmisiblereal
    from tleg_opergaran
     where log_fecha = p_fechafrom
       and log_tipo = p_tipofrom
       and log_operacion in (select lca_cuenta
           					from tleg_operacioncalif,tleg_opergaran,tgar_garantias
         					where log_fecha = lca_fecha
                              and log_tipo = lca_tipo
						      and log_operacion = lca_cuenta
                              and log_cliente = gar_codcli
                              and log_numgaran = gar_numero
                              and gar_moneda = 0
                              and lca_fecha = p_fechafrom
         					  and lca_tipo = p_tipofrom
         					  and lca_codtipocredito = p_codtipocredito
         					  and lca_codtipocredito in ('C','M','D'))--NOV 2017
       and log_operacion not in (select pre_credito
                              from tpre_prestamos
                              where pre_fecance between p_fechafrom + 1 and p_fechato
                              and pre_fecance is not null);
       exception
    when dup_val_on_index then
      null;
    end;

---

 --
 BEGIN
         insert into tpre_califdettipos(cdt_fecha, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion)
       select p_fechato, cdt_tipo, cdt_codcli, cdt_mod, cdt_cuenta, cdt_tabtipocalif, cdt_tipocalif,
					           cdt_tabcalificacion, cdt_calificacion
         from  tpre_califdettipos
         where cdt_fecha = p_fechafrom
           and cdt_tipo = p_tipofrom
           and cdt_cuenta in (select lca_cuenta
           					from tleg_operacioncalif
         					where lca_fecha = p_fechafrom
         					  and lca_tipo = p_tipofrom
         					  and lca_codtipocredito = p_codtipocredito
         					  and lca_codtipocredito in ('C','D'))
           and cdt_cuenta not in (select pre_credito
                              from tpre_prestamos
                              where pre_fecance between p_fechafrom + 1 and p_fechato
                                 and pre_fecance is not null);
    exception
    when dup_val_on_index then
      null;
    end;

--Por actualizacione en el REA esto ya no se copia
/*        INSERT INTO tpre_prevdettipos(PDT_FECHA,PDT_TIPO,PDT_CODCLI,PDT_MOD,
									  PDT_CUENTA,PDT_TABTIPOPREV,PDT_TIPOPREV,
								      PDT_DESDE,PDT_VALORDIR,PDT_VALORDIRMN,PDT_VALORCONTIN,PDT_VALORCONTINMN,
								      PDT_PORCENTAJE,PDT_VALORGARAN,PDT_HASTA,PDT_TABAGRMONTCLASIF,PDT_AGRMONTCLASIF)
        select  P_FECHAto,PDT_TIPO,PDT_CODCLI,PDT_MOD,
									  PDT_CUENTA,PDT_TABTIPOPREV,PDT_TIPOPREV,
								      PDT_DESDE,PDT_VALORDIR,PDT_VALORDIRMN,PDT_VALORCONTIN,PDT_VALORCONTINMN,
								      PDT_PORCENTAJE,PDT_VALORGARAN,PDT_HASTA,PDT_TABAGRMONTCLASIF,PDT_AGRMONTCLASIF
		  from  tpre_prevdettipos
		  where  PDT_FECHA = p_fechafrom
		    and PDT_TIPO = p_tipofrom
		    and pdt_tipoprev NOT in (10,11,5,12,17)
            and pdt_cuenta in (select lca_cuenta
           					from tleg_operacioncalif
         					where lca_fecha = p_fechafrom
         					  and lca_tipo = p_tipofrom
         					  and lca_codtipocredito = p_codtipocredito) ;*/
    commit;
     upd_saldos_fecha(P_FECHAto,p_tipofrom);

  exception
  when others then
          raise_application_error(-20465,'Error al copiar calificacion:'||sqlerrm);
   end;
  -- 601-5681 EL 606-1413
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- copiar la calificacion por capacidad de pago desde una fecha a otra
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

   procedure copy_calificaxcapacidad(p_fechafrom in date,p_tipofrom in varchar2,p_fechato in date)
   is
   begin
    begin
    INSERT INTO TPRE_PARAMCALIF
      (PCA_FECHA, PCA_TIPO, PCA_STATUS, PCA_PREVSTATUS)
    VALUES
      (p_fechato, p_tipofrom, 'P', 'P');
    exception
    when dup_val_on_index then
      null;
    end;
   insert into tleg_clicalifcapacidad(cca_fecha,cca_tipo,cca_codcli,cca_tabcalifica,cca_califica)
   select p_fechato,cca_tipo,cca_codcli,cca_tabcalifica,cca_califica
     from tleg_clicalifcapacidad
     where cca_fecha = p_fechafrom
       and cca_tipo  = p_tipofrom;
       commit;
   end;                  
--XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- copiar la calificacion copy_calificaxperdida
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

   procedure copy_calificaxperdida(p_fechafrom in date,p_tipofrom in varchar2,p_fechato in date)
   is
   begin
    begin
    INSERT INTO TPRE_PARAMCALIF
      (PCA_FECHA, PCA_TIPO, PCA_STATUS, PCA_PREVSTATUS)
    VALUES
      (p_fechato, p_tipofrom, 'P', 'P');
    exception
    when dup_val_on_index then
      null;
    end;
   insert into TLEG_CLICALIFRIESGO_MEDIANO(CCA_FECHA,CCA_TIPO,CCA_CODCLI,CCA_TABCALIFICA,CCA_CALIFICA,CCA_PORCENTAJEPERD,CCA_CALIFICAPERDIDA)
   select p_fechato,CCA_TIPO,CCA_CODCLI,CCA_TABCALIFICA,CCA_CALIFICA,CCA_PORCENTAJEPERD,CCA_CALIFICAPERDIDA
     from  TLEG_CLICALIFRIESGO_MEDIANO
     where cca_fecha = p_fechafrom
       and cca_tipo  = p_tipofrom;
       commit;
   end;
   

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo de la provisones
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   procedure prov_general(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                         p_cuenta in number,p_mod in number,p_monto in number,p_montogar in number,
                         p_tipocredito in number,p_codtipocredito in varchar2,p_tipoprov in number,p_tipocalif in number,
                         p_provision out number) is

  	w_califmora tpre_califdettipos.cdt_calificacion%type;
    w_parcodigo tpre_paramcaliftipo.par_codigo%type;
    w_codigorango	number(4);
    w_tiporango tpre_paramcalifprev.pcp_tiporango%type:=10;
    w_califrango tpre_paramcalifprev.pcp_califrango%type;
    w_calificacion    tpre_rangodetcalif.rad_tipocalif%type;
    w_calificacionfinal    tpre_rangodetcalif.rad_tipocalif%type;
    w_provision number:=0;
    w_porcentaje number:=0;
    w_diasvencontrato number:=0;
    w_capitalcub number:=0;
    w_demandajundicial varchar2(1):='N';
    w_intacteje number :=0;
    w_desobregiro varchar2(1):='N';
    w_capvencido number:=0;
    begin
	select distinct par_codigo
      into w_parcodigo
	 from tpre_paramcaliftipo
    where par_tabtipocredito = 244
      and par_tipocredito = 20;--p_tipocredito; --mora NOV 2017

--
    begin -- Definir la calificación de la operación
      select pct_rancodigo,rad_califrango
      	into w_codigorango,w_califrango
        from tpre_parrancaltipo,tpre_rangodetcalif
       where pct_califprev = 'P'
         and pct_mod = p_mod
         and pct_parcodigo = w_parcodigo
         and pct_mod = rad_mod
         and pct_rancodigo = rad_rancodigo
         and pct_califprev = rad_califprev
         and rad_tiporango =  w_tiporango
         and rownum = 1;
    exception
      when no_data_found then
        raise_application_error(-20465,'No está parametrizado Grupo '' cód '|| to_char(w_parcodigo)||
        					 ' Mód '|| to_char(p_mod)||'p_tipocredito:'||p_tipocredito||' w_tiporango:'||w_tiporango);
      when too_many_rows then
        raise_application_error(-20465,'Parametrizado más de un Grupo '||' cód '|| w_parcodigo||
        					 ' Mód '||p_mod);
    end;
		begin
           select cdt_calificacion
           into w_calificacion
           from tpre_califdettipos
           where cdt_fecha = p_fecha
           and cdt_tipo= p_tipo
           and cdt_codcli = p_cliente
           and cdt_mod = p_mod
           and cdt_cuenta = p_cuenta
           and cdt_tabtipocalif = 253
	       and cdt_tipocalif = p_tipocalif;--calificacion resultado final;
        exception
           when no_data_found then
              raise_application_error(-20465,'No está existe calificacion '||' para el código '|| p_tipocalif||
              ' Módulo '||p_mod);
           when too_many_rows then
              raise_application_error(-20465,'No está existe calificacion '||' para el código '|| p_tipocalif||
         					 ' Módulo '||p_mod);
        end;
--para saber si minimo es el 20%
		begin
           select cdt_calificacion
           into w_calificacionfinal
           from tpre_califdettipos
           where cdt_fecha = p_fecha
           and cdt_tipo= p_tipo
           and cdt_codcli = p_cliente
           and cdt_mod = p_mod
           and cdt_cuenta = p_cuenta
           and cdt_tabtipocalif = 253
	       and cdt_tipocalif = 14;--p_tipocalif;--calificacion resultado final;
        exception
           when no_data_found then
              raise_application_error(-20465,'No está existe calificacion '||' para el código '|| p_tipocalif||
              ' Módulo '||p_mod);
           when too_many_rows then
              raise_application_error(-20465,'No está existe calificacion '||' para el código '|| p_tipocalif||
         					 ' Módulo '||p_mod);
        end;
--
		begin
           select rad_porcprev
           into w_porcentaje
           from tpre_rangodetcalif
           where rad_califprev = 'P'
           and rad_mod = p_mod
           and rad_rancodigo = w_codigorango
           and rad_califrango= w_califrango
           and rad_tipocalif = w_calificacion;
        exception
           when no_data_found then
              raise_application_error(-20465,'No está parametrizado la '||' para el código '|| w_parcodigo||
              ' Módulo '||p_mod);
           when too_many_rows then
              raise_application_error(-20465,'Parametrizado más de una '||' para el código '|| w_parcodigo||
         					 ' Módulo '||p_mod);
        end;
        w_diasvencontrato:=0;
        w_desobregiro:='N';
        w_capvencido:=0;
       if p_codtipocredito in ('C','M','D') then --NOV 2017
        begin
         select lca_diasvencontrato,lca_capitalcub,lca_demandajundicial,acr_capvencido,lca_desobregiro --AJUSTE 2008/09/04
           into w_diasvencontrato,w_capitalcub,w_demandajundicial,w_capvencido,w_desobregiro
           from tleg_operacioncalif,tpre_accrual
          where lca_fecha = p_fecha
           and lca_tipo = p_tipo
           and lca_cuenta = p_cuenta
           and acr_credito = lca_cuenta
           and acr_fecha = lca_fecha;
        exception
         when others then
           w_diasvencontrato:=0;
        end;
       /* Se comenta esta validacion 2009/04/08
        if nvl(w_diasvencontrato,0) > 0 and nvl(w_capitalcub,0) > 0 then
           if w_porcentaje < 20 then
              w_porcentaje:=20;--los vencidos por contratos siempre s 20 independiente del los dias de vencido - BDI
           end if;
        end if; */
        --creditos con D o E y si tienen vencidos +90 intereses o estan judicial minimo es 20%
        --si capvencido es > 0 siginifica que esta en N
        if nvl(w_calificacionfinal,0) in (4,5) then
        if (nvl(w_capvencido,0) > 0 and nvl(w_capitalcub,0) > 0) or (w_demandajundicial='S') then
           if w_porcentaje < 20 then
              w_porcentaje:=20;--los vencidos por contratos siempre s 20 independiente del los dias de vencido - BDI
           end if;
         end if;
        end if;
        end if;
        --ajuste 2008/09/04 --los creditos por sobregiro se calcula el 100%
        if nvl(w_desobregiro,'N')='S' then
           w_porcentaje:=100;
        end if;
        w_provision := (p_monto*w_porcentaje)/100;
     if p_tipoprov = 9 then
        update tpre_calificadet
          set cad_prevconstitdir= w_provision
         where cad_fecha = p_fecha
           and cad_tipo = p_tipo
           and cad_mod = p_mod
           and cad_cuenta = p_cuenta;
      end if;
      --actualizo las provisiones determinando
        UPDATE tpre_prevdettipos
           SET pdt_valordir = w_provision
         WHERE pdt_fecha = p_fecha
           AND pdt_tipo = 'P'
           AND pdt_mod = p_mod
           AND pdt_cuenta =  p_cuenta
           AND pdt_tipoprev = p_tipoprov;
      --Fin
      IF SQL%NOTFOUND THEN
        INSERT INTO tpre_prevdettipos(PDT_FECHA,PDT_TIPO,PDT_CODCLI,PDT_MOD,
									  PDT_CUENTA,PDT_TABTIPOPREV,PDT_TIPOPREV,
								      PDT_DESDE,PDT_VALORDIR,PDT_VALORDIRMN,PDT_VALORCONTIN,PDT_VALORCONTINMN,
								      PDT_PORCENTAJE,PDT_VALORGARAN,PDT_HASTA,PDT_TABAGRMONTCLASIF,PDT_AGRMONTCLASIF)
        VALUES(p_fecha,'P',p_cliente,p_mod,p_cuenta,254,p_tipoprov,p_fecha,nvl(w_provision, 0),0,0,0,
               w_porcentaje,NVL(p_montogar, 0),NULL,245,nvl(w_calificacion,1));
      END IF;
      p_provision:=w_provision;
     END ;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo total de la provision
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  function total_prov_general(p_fecha in date,p_tipo in varchar2,p_tipoprov in number,p_codtipocredito in varchar2) return number is
    w_total_prov number:=0;
  begin
         select sum(nvl(pdt_valordir,0))
           into w_total_prov
           from tpre_prevdettipos,tleg_operacioncalif
          where pdt_fecha = lca_fecha
          and pdt_tipo = lca_tipo
          and pdt_cuenta = lca_cuenta
          and pdt_fecha =  p_fecha
          and pdt_tipo  = p_tipo
          and lca_codtipocredito=p_codtipocredito
          and pdt_tipoprev = p_tipoprov;
       return w_total_prov;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- conocer la prov de cada credito
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  function oper_prov_general(p_fecha in date,p_tipo in varchar2,p_tipoprov in number, p_cuenta in number) return number is
       w_prov number:=0;
  begin
         select sum(nvl(pdt_valordir,0))
           into w_prov
           from tpre_prevdettipos
          where pdt_fecha =  p_fecha
          and pdt_tipo  = p_tipo
          and pdt_tipoprev = p_tipoprov
          and pdt_cuenta = p_cuenta;
       return w_prov;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- prov-gradual c-capital
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  procedure prov_gradual_cap(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                         p_cuenta in number,p_mod in number,p_tipoprov in number,p_provtotal in number) is
  w_factor number:=0;
  w_prov_oper number:=0;
  w_montodef number:=0;
  w_prov_gradual number:=0;
  w_fecontab date:=null;
  w_esadju number:=0;
  begin
   w_prov_oper:=oper_prov_general(p_fecha,p_tipo,9,p_cuenta);
   begin
   select pre_fecontab
     into w_fecontab
     from tpre_prestamos
    where pre_credito = p_cuenta;
   exception
   	  when others then
   	    w_fecontab:=f_fechatrabajo;
   end;

   /*--revisando a la provision requerida de cada operacion se le aplica la  gradualidad.
   --w_factor := w_prov_oper/p_provtotal;
    select sum(nvl(pdf_montomn,0))
      into w_montodef
      from tpre_prevdefant
     where pdf_tabtipodef=248
       and pdf_tipodef = 2
       and pdf_vigente='S'
       and p_fecha between pdf_fechadesde and nvl(pdf_fechahasta,to_date('2199/12/01','yyyy/mm/dd'));*/
   begin
              select pda_porcentaje
                into w_factor
               from tpre_prevdefantparam
               where pda_anio = to_number(to_char(p_fecha,'yyyy'))
                 and pda_mes = to_number(to_char(p_fecha,'mm'));
   exception
     when no_data_found then
           w_factor:=1;
   end ;
   if trunc(w_fecontab) <= to_date('2004/12/31','yyyy/mm/dd') then--variable a parametrizar
   	  w_factor:=1;--es el 100 de gradualidad
   end if;
    w_prov_gradual:= w_factor*w_prov_oper;

   --Por adjudicados BDI
   select count(*)
    into w_esadju
     from tleg_adjudicados
    where lea_operacion = p_cuenta
      and trunc(lea_fechaadjud) <= p_fecha;
   if nvl(w_esadju,0) > 0 then
       w_prov_gradual:=0;
   end if;
   --fin
      --actualizo las provisiones determinando
        UPDATE tpre_prevdettipos
           SET pdt_valordir = w_prov_gradual
         WHERE pdt_fecha = p_fecha
           AND pdt_tipo = 'P'
           AND pdt_mod = p_mod
           AND pdt_cuenta =  p_cuenta
           AND pdt_tipoprev = p_tipoprov;
      --Fin
     IF SQL%NOTFOUND THEN
        INSERT INTO tpre_prevdettipos(PDT_FECHA,PDT_TIPO,PDT_CODCLI,PDT_MOD,
									  PDT_CUENTA,PDT_TABTIPOPREV,PDT_TIPOPREV,
								      PDT_DESDE,PDT_VALORDIR,PDT_VALORDIRMN,PDT_VALORCONTIN,PDT_VALORCONTINMN,
								      PDT_PORCENTAJE,PDT_VALORGARAN,PDT_HASTA,PDT_TABAGRMONTCLASIF,PDT_AGRMONTCLASIF)
        VALUES(p_fecha,'P',p_cliente,p_mod,p_cuenta,254,p_tipoprov,p_fecha,nvl(w_prov_gradual, 0),0,0,0,
               w_factor,NVL(p_provtotal, 0),NULL,245,1); --le dejo constante uno
    END IF;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo de la prov gradual interes
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  procedure prov_gradual_int(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                         p_cuenta in number,p_mod in number,p_tipoprov in number,p_provtotal in number) is
  w_factor number:=0;
  w_prov_oper number:=0;
  w_montodef number:=0;
  w_prov_gradual number:=0;
  w_fecontab date:=null;
  w_esadju number:=0;
  begin
   w_prov_oper:=oper_prov_general(p_fecha,p_tipo,8,p_cuenta);
   begin
   select pre_fecontab
     into w_fecontab
     from tpre_prestamos
    where pre_credito = p_cuenta;
   exception
   	  when others then
   	    w_fecontab:=f_fechatrabajo;
   end;

   --w_factor := w_prov_oper/p_provtotal;
   begin
              select pda_porcentaje
                into w_factor
               from tpre_prevdefantparam
               where pda_anio = to_number(to_char(p_fecha,'yyyy'))
                 and pda_mes = to_number(to_char(p_fecha,'mm'));
   exception
     when no_data_found then
           w_factor:=1;
   end ;
   /*if trunc(w_fecontab) <= to_date('2004/12/31','yyyy/mm/dd') then--variable a parametrizar
   	  w_factor:=1;--es el 100 de gradualidad
   end if;   */
   w_prov_gradual:= w_factor*w_prov_oper;
   --Por adjudicados BDI
   select count(*)
    into w_esadju
     from tleg_adjudicados
    where lea_operacion = p_cuenta
      and trunc(lea_fechaadjud) <= p_fecha;
   if nvl(w_esadju,0) > 0 then
       w_prov_gradual:=0;
   end if;
   --fin
      --actualizo las provisiones determinando
        UPDATE tpre_prevdettipos
           SET pdt_valordir = w_prov_gradual
         WHERE pdt_fecha = p_fecha
           AND pdt_tipo = 'P'
           AND pdt_mod = p_mod
           AND pdt_cuenta =  p_cuenta
           AND pdt_tipoprev = p_tipoprov;
    IF SQL%NOTFOUND THEN
       INSERT INTO tpre_prevdettipos(PDT_FECHA,PDT_TIPO,PDT_CODCLI,PDT_MOD,
									  PDT_CUENTA,PDT_TABTIPOPREV,PDT_TIPOPREV,
								      PDT_DESDE,PDT_VALORDIR,PDT_VALORDIRMN,PDT_VALORCONTIN,PDT_VALORCONTINMN,
								      PDT_PORCENTAJE,PDT_VALORGARAN,PDT_HASTA,PDT_TABAGRMONTCLASIF,PDT_AGRMONTCLASIF)
       VALUES(p_fecha,'P',p_cliente,p_mod,p_cuenta,254,p_tipoprov,p_fecha,nvl(w_prov_gradual, 0),0,0,0,
               w_factor,NVL(p_provtotal, 0),NULL,245,1); --le dejo constante uno
   END IF;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo de la previson de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure prov_constituida_cap(p_fecha in date,p_tipo in varchar2,p_cliente in number,p_codtipocartera in varchar2,
                         p_cuenta in number,p_mod in number,p_tipoprov in number,p_tipoprovreq in number,p_provtotal in number,
                         p_provresul out number) is
  w_factor number:=0;
  w_prov_oper number:=0;
  w_montodef number:=0;
  w_prov_cons number:=0;
  w_prov_conssistema number:=0;
  w_esadju number:=0;
  w_esadjudicado number:=0;
  begin
   w_prov_oper:=oper_prov_general(p_fecha,p_tipo,p_tipoprovreq,p_cuenta);
   w_factor := w_prov_oper/p_provtotal;
   --esto uso para BDi
    select nvl(PGA_PROVCONSFECHA,0)/decode(nvl(PGA_PROVTOTCOMER,0)-nvl(PGA_PROVCASTIGADOS,0),0,1,nvl(PGA_PROVTOTCOMER,0)-nvl(PGA_PROVCASTIGADOS,0))
      into w_factor
      from tleg_provisiongen
     where pga_fecha = p_fecha
       and pga_tipo = p_tipo--libros
       and pga_codtipocredito=decode(p_codtipocartera,'M','C','D','C',p_codtipocartera);
    --verifico si esta castigado o adjudicado
       w_esadjudicado:=0;
       select count(lea_operacion)
         into w_esadjudicado
		 from tleg_adjudicados
		where trunc(lea_fechaadjud) <= p_fecha
		  and lea_operacion = p_cuenta;
       if nvl(w_esadjudicado,0)> 0 then
          w_prov_oper:=0;
       end if;

   --fin bdi
   --Para BLH
   /*begin
    select nvl(PGA_PROVCONSFECHA,0)
      into w_prov_conssistema
      from tleg_provisiongen
     where pga_fecha = p_fecha
       and pga_tipo = p_tipo--libros
       and pga_codtipocredito=decode(p_codtipocartera,'M','C',p_codtipocartera);
   exception
     when others then
         raise_application_error (-20505,'No existe el monto de constitucuón del sistema ingresado para la fecha revise 2/6028 p_codtipocartera:'||p_codtipocartera);
   end;*/
    w_prov_cons:= w_factor*w_prov_oper; --para BDI
   --Por adjudicados BDI
   /*select count(*)
    into w_esadju
     from tleg_adjudicados
    where lea_operacion = p_cuenta
      and trunc(lea_fechaadjud) <= p_fecha;
   if nvl(w_esadju,0) > 0 then
       w_prov_cons:=0;
   end if;*/
   --fin

    --w_prov_cons:= w_factor*w_prov_conssistema;
    p_provresul:= round(NVL(w_prov_cons,0),3);
    INSERT INTO tpre_prevdettipos(PDT_FECHA,PDT_TIPO,PDT_CODCLI,PDT_MOD,
									  PDT_CUENTA,PDT_TABTIPOPREV,PDT_TIPOPREV,
								      PDT_DESDE,PDT_VALORDIR,PDT_VALORDIRMN,PDT_VALORCONTIN,PDT_VALORCONTINMN,
								      PDT_PORCENTAJE,PDT_VALORGARAN,PDT_HASTA,PDT_TABAGRMONTCLASIF,PDT_AGRMONTCLASIF)
        VALUES(p_fecha,'P',p_cliente,p_mod,p_cuenta,254,p_tipoprov,p_fecha,round(nvl(w_prov_cons,0),3),0,0,0,
               round(w_factor,2),round(NVL(p_provtotal, 0),3),NULL,245,1); --le dejo constante uno
exception
      when others then
         rollback;
          raise_application_error (-20505,'tpre_prevdettipos: ' || w_factor||'^'||p_cuenta||'^'||substr(sqlerrm,1,120));
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo de la previson de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure prov_constituida_total(p_fecha in date,p_tipo in varchar2,p_cliente in number,p_codtipocartera in varchar2,
                         p_cuenta in number,p_mod in number,p_tipoprov in number,p_tipoprovreq in number,p_provtotal in number,p_provresul out number) is
  w_factor number:=0;
  w_prov_oper number:=0;
  w_montodef number:=0;
  w_prov_cons number:=0;
  begin
       p_provresul:= round(NVL(p_provtotal,0),2);
        INSERT INTO tpre_prevdettipos(PDT_FECHA,PDT_TIPO,PDT_CODCLI,PDT_MOD,
									  PDT_CUENTA,PDT_TABTIPOPREV,PDT_TIPOPREV,
								      PDT_DESDE,PDT_VALORDIR,PDT_VALORDIRMN,PDT_VALORCONTIN,PDT_VALORCONTINMN,
								      PDT_PORCENTAJE,PDT_VALORGARAN,PDT_HASTA,PDT_TABAGRMONTCLASIF,PDT_AGRMONTCLASIF)
        VALUES(p_fecha,'P',p_cliente,p_mod,p_cuenta,254,p_tipoprov,p_fecha,round(nvl(p_provtotal, 0),3),0,0,0,
               round(w_factor,2),round(NVL(p_provtotal, 0),2),NULL,245,1); --le dejo constante uno

  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo de la previson de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure upd_prov_req(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                         p_cuenta in number,p_mod in number,p_tipoprov in number,p_provtotal in number) is
  w_factor number:=0;
  w_prov_oper number:=0;
  w_montodef number:=0;
  w_prov_cons number:=0;
  begin
      --actualizo las provisiones determinando
        UPDATE tpre_prevdettipos
           SET pdt_valordir = p_provtotal
         WHERE pdt_fecha = p_fecha
           AND pdt_tipo = 'P'
           AND pdt_mod = p_mod
           AND pdt_cuenta =  p_cuenta
           AND pdt_tipoprev = p_tipoprov;
      --Fin
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo de la previson de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure provision_cartera ( p_fecha in date, p_tipo in varchar2 ) is
  p_fechadesdenuevos date;
  w_esnuevo varchar2(1):=null;
  cursor p is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado,
        lca_montogarreal,lca_suc,lca_ofi,lca_capitalcub,lca_capitalexp,lca_interescub,lca_interesexp,lca_diasvencontrato,
        lca_esnuevo,nvl(lca_desobregiro,'N') lca_desobregiro,lca_contingente
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
--    and lca_cuenta = 1000000471
    and nvl(lca_provisionado,'N') = 'N'
    --and lca_esnuevo = w_esnuevo
   Order by lca_codcli;
   w_diasmora number;
   w_califmora number;
   w_califcapacidad number;
   w_califrestruc number;
   w_califinicial number;
   w_califcub number;
   w_califexp number;
   w_provtotal_comercial number;
   w_provtotal_hipotecario number;
   w_provtotal_consumo number;
   w_provtotal_microempresa number;
   w_provision number;
   w_provision_15 number:=0;
   w_provision_16 number:=0;
   w_provision_18 number:=0;
   w_provision_19 number:=0;
   v_nromeses number:=0;     
      --NOV 2017
    w_provtotal_mediano number:=0;

  begin

      v_nromeses :=0;
      begin
      select nvl(pda_nromeses,0)
        into v_nromeses
       from tpre_prevdefantparam
      where pda_anio = to_char(p_fecha,'yyyy')
        and pda_mes = to_char(p_fecha,'mm');
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;

-- por regerenacion
if nvl(v_nromeses,0) = 3 then
   w_esnuevo := 'N';
   delete tpre_prevdettipos
   where pdt_fecha = p_fecha
    and pdt_tipo = p_tipo;

    update tleg_operacioncalif
      set lca_provisionado = 'N'
      where lca_fecha = p_fecha
        and lca_tipo = p_tipo;
 else
    w_esnuevo := 'N';
   delete tpre_prevdettipos
   where pdt_fecha = p_fecha
    and pdt_tipo = p_tipo
    and pdt_cuenta in (select lca_cuenta from tleg_operacioncalif where lca_fecha =  p_fecha and lca_tipo = p_tipo);

    update tleg_operacioncalif
      set lca_provisionado = 'N'
      where lca_fecha = p_fecha
        and lca_tipo = p_tipo;
        --and lca_esnuevo='S';
 end if;

    commit;
    --and pdt_tipoprev not in (5,12,17
   for x in p loop
   w_provision_15 :=0;
   w_provision_16 :=0;
   w_provision_18 :=0;
   w_provision_19 :=0;
   prov_general(p_fecha,
                p_tipo,
                x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_interes,x.lca_montogarreal,
                         x.lca_tipocredito,x.lca_codtipocredito,8,14,w_provision);--interes
   prov_general(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_capital,x.lca_montogarreal,
                         x.lca_tipocredito,x.lca_codtipocredito,9,14,w_provision);--capital

   prov_general(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_contingente,x.lca_montogarreal,
                         x.lca_tipocredito,x.lca_codtipocredito,13,14,w_provision);--contingencia

  if x.lca_codtipocredito in ('C','M','D','O','H') then--SYSAID 19081
    --capital cubierto
     if nvl(x.lca_capitalcub,0) > 0 then
        prov_general(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_capitalcub,x.lca_montogaradmi,
                         x.lca_tipocredito,x.lca_codtipocredito,15,12,w_provision_15);--cubierto
     end if;
    --interes cubierto
     if nvl(x.lca_interescub,0) > 0 then
        prov_general(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_interescub,x.lca_montogaradmi,
                         x.lca_tipocredito,x.lca_codtipocredito,18,12,w_provision_18);--cubierto
     end if;
    --capital expuesto
     if nvl(x.lca_capitalexp,0) > 0 then
        prov_general(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_capitalexp,x.lca_montogaradmi,
                x.lca_tipocredito,x.lca_codtipocredito,16,13,w_provision_16);--expuesto
     end if;
    --interes expuesto
     if nvl(x.lca_interesexp,0) > 0 then
        prov_general(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_interesexp,x.lca_montogaradmi,
                x.lca_tipocredito,x.lca_codtipocredito,19,13,w_provision_19);--expuesto
     end if;
   --actualizando las provisones requeridas
   IF X.LCA_MOD NOT IN (10) THEN
   upd_prov_req(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,8,nvl(w_provision_18,0)+nvl(w_provision_19,0));

   upd_prov_req(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,9,nvl(w_provision_15,0)+nvl(w_provision_16,0));
   END IF;
  end if;
   end loop;
   w_provtotal_comercial:= total_prov_general(p_fecha,p_tipo,9,'C');--comercial capital
   w_provtotal_hipotecario:= total_prov_general(p_fecha,p_tipo,9,'H');--hipotecarios capital
   w_provtotal_consumo:= total_prov_general(p_fecha,p_tipo,9,'O');--consumo capital
   w_provtotal_microempresa:= total_prov_general(p_fecha,p_tipo,9,'M');--microempresa capital
   w_provtotal_mediano:= total_prov_general(p_fecha,p_tipo,9,'D');--microempresa capital   
    upd_provision_6_15_req( p_fecha,p_tipo);
    commit;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo de la previson de cartera_ gradual
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure provision_cartera_gradual ( p_fecha in date, p_tipo in varchar2 ) is
  p_fechadesdenuevos date;
  cursor p is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado,
        lca_montogarreal,lca_suc,lca_ofi,lca_capitalcub,lca_capitalexp,lca_interescub,lca_interesexp,lca_diasvencontrato
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and nvl(lca_provisionado,'N') = 'N'
    --and lca_esnuevo = 'N'
   Order by lca_codcli;
   w_diasmora number;
   w_califmora number;
   w_califcapacidad number;
   w_califrestruc number;
   w_califinicial number;
   w_califcub number;
   w_califexp number;
   w_provtotal_comercial number;
   w_provtotal_hipotecario number;
   w_provtotal_consumo number;
   w_provtotal_microempresa number;
   w_provision number;
   w_provision_15 number:=0;
   w_provision_16 number:=0;
   w_provision_18 number:=0;
   w_provision_19 number:=0;
   v_nromeses number :=0;
   --NOV 2017
   w_provtotal_mediana NUMBER;
  begin
      v_nromeses :=0;
      begin
      select nvl(pda_nromeses,0)
        into v_nromeses
       from tpre_prevdefantparam
      where pda_anio = to_char(p_fecha,'yyyy')
        and pda_mes = to_char(p_fecha,'mm');
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
 if nvl(v_nromeses,0) = 3 then
   delete tpre_prevdettipos
   where pdt_fecha = p_fecha
    and pdt_tipo = p_tipo
    and pdt_tipoprev in (11,10);

    update tleg_operacioncalif
      set lca_provisionado = 'N'
      where lca_fecha = p_fecha
        and lca_tipo = p_tipo;

 else
   delete tpre_prevdettipos
   where pdt_fecha = p_fecha
    and pdt_tipo = p_tipo
    and pdt_tipoprev in (11,10)
    and pdt_cuenta in (select lca_cuenta from tleg_operacioncalif where lca_fecha =  p_fecha and lca_tipo = p_tipo);

    update tleg_operacioncalif
      set lca_provisionado = 'N'
      where lca_fecha = p_fecha
        and lca_tipo = p_tipo;
        --and lca_esnuevo='S';
 end if;

    commit;
    --and pdt_tipoprev not in (5,12,17
   w_provtotal_comercial:= total_prov_general(p_fecha,p_tipo,9,'C');--comercial capital
   w_provtotal_hipotecario:= total_prov_general(p_fecha,p_tipo,9,'H');--hipotecarios capital
   w_provtotal_consumo:= total_prov_general(p_fecha,p_tipo,9,'O');--consumo capital
   w_provtotal_microempresa:= total_prov_general(p_fecha,p_tipo,9,'M');--microempresa capital 
   w_provtotal_mediana:= total_prov_general(p_fecha,p_tipo,9,'D');--microempresa capital    
   for x in p loop
     if x.lca_codtipocredito = 'C' then
     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,11,w_provtotal_comercial);
     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,w_provtotal_comercial);                         */
     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
                      x.lca_cuenta,x.lca_mod,10,w_provtotal_comercial);
     elsif x.lca_codtipocredito = 'H' then
     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,11,w_provtotal_hipotecario);
     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,w_provtotal_hipotecario);*/
     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
                      x.lca_cuenta,x.lca_mod,10,w_provtotal_hipotecario);
     elsif x.lca_codtipocredito = 'O' then
     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,11,w_provtotal_consumo);
     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,w_provtotal_consumo);*/
     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
                      x.lca_cuenta,x.lca_mod,10,w_provtotal_consumo);
     elsif x.lca_codtipocredito = 'M' then
     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,11,w_provtotal_microempresa);
     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,w_provtotal_microempresa);*/
     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
                      x.lca_cuenta,x.lca_mod,10,w_provtotal_microempresa);
     elsif x.lca_codtipocredito = 'D' then  --Nov 2017
	     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
	                         x.lca_cuenta,x.lca_mod,11,w_provtotal_mediana);
	     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
	                         x.lca_cuenta,x.lca_mod,12,w_provtotal_microempresa);*/
	     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
	                      x.lca_cuenta,x.lca_mod,10,w_provtotal_mediana);

     end if;
     update tleg_operacioncalif
       set lca_provisionado = 'S'
      where lca_fecha = p_fecha
        and lca_tipo = p_tipo
        and lca_cuenta = x.lca_cuenta;
        --para la 6/15
   end loop;
    --upd_provision_6_15_req( p_fecha,p_tipo);
    commit;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Actualiza provision requerida
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure upd_provision_req( p_fecha in date, p_tipo in varchar2 ) is
  p_fechadesdenuevos date;
  cursor p is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado,
        lca_montogarreal,lca_suc,lca_ofi,lca_capitalcub,lca_capitalexp,lca_interescub,lca_interesexp,lca_diasvencontrato
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and lca_cuenta in (select pre_credito
    			         from tpre_prestamos
                        where pre_fecontab between p_fechadesdenuevos and p_fecha)
   Order by lca_codcli;
   w_diasmora number;
   w_califmora number;
   w_califcapacidad number;
   w_califrestruc number;
   w_califinicial number;
   w_califcub number;
   w_califexp number;
   w_provtotal_comercial number;
   w_provtotal_hipotecario number;
   w_provtotal_consumo number;
   w_provtotal_microempresa number;
   w_provision number;
   w_provision_15 number:=0;
   w_provision_16 number:=0;
   w_provision_18 number:=0;
   w_provision_19 number:=0;
  begin
   /*delete tpre_prevdettipos
   where pdt_fecha = p_fecha
    and pdt_tipo = p_tipo
    and pdt_tipoprev not in (5,12,17); */
 begin
  SELECT decode(to_char(p_fecha,'mm'),'01',last_day(add_months(p_fecha,-1)),'02',last_day(add_months(p_fecha,-2)),last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd'))) + 1--last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd')) + 1
    INTO p_fechadesdenuevos
 	  FROM tpre_prevdefantparam
 	 WHERE pda_anio = to_number(to_char(P_FECHA,'yyyy'))
     AND pda_mes <= to_number(to_char(P_FECHA,'mm'))
     AND pda_nromeses = 3 ;
  exception
  	when others then
  	p_fechadesdenuevos:=p_fecha;
  end;
   for x in p loop
   begin
   prov_general(p_fecha,
                p_tipo,
                x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_interes,x.lca_montogarreal,
                         x.lca_tipocredito,x.lca_codtipocredito,8,14,w_provision);--interes
   exception
    when  dup_val_on_index then
          null;
   end ;
   begin
   prov_general(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_capital,x.lca_montogarreal,
                         x.lca_tipocredito,x.lca_codtipocredito,9,14,w_provision);--capital
   exception
    when  dup_val_on_index then
          null;
   end ;
   if x.lca_codtipocredito in ('C','M','D','O','H') then  --Nov 2017 SYS19081
     if nvl(x.lca_capitalcub,0) > 0 then
        prov_general(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_capitalcub,x.lca_montogaradmi,
                         x.lca_tipocredito,x.lca_codtipocredito,15,12,w_provision_15);--cubierto
       /*prov_general(p_fecha,
                p_tipo,
                x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_interes,x.lca_montogarreal,
                         x.lca_tipocredito,8,12);--interes para actualizar
       prov_general(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_capital,x.lca_montogarreal,
                         x.lca_tipocredito,9,12);--capital*/
     end if;
     if nvl(x.lca_capitalexp,0) > 0 then
        prov_general(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_capitalexp,x.lca_montogaradmi,
                x.lca_tipocredito,x.lca_codtipocredito,16,13,w_provision_16);--expuesto
       /*prov_general(p_fecha,
                p_tipo,
                x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_interes,x.lca_montogarreal,
                         x.lca_tipocredito,8,13);--interes para actualizar
       prov_general(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_capital,x.lca_montogarreal,
                         x.lca_tipocredito,9,13);--capital*/
     end if;
     if nvl(x.lca_interescub,0) > 0 then
        prov_general(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,x.lca_capitalcub,x.lca_montogaradmi,
                         x.lca_tipocredito,x.lca_codtipocredito,18,12,w_provision_18);--cubierto
     end if;
     if nvl(x.lca_interesexp,0) > 0 then
        prov_general(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,x.lca_capitalexp,x.lca_montogaradmi,
                x.lca_tipocredito,x.lca_codtipocredito,19,13,w_provision_19);--expuesto
     end if;
   upd_prov_req(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,8,nvl(w_provision_18,0)+nvl(w_provision_19,0));

   upd_prov_req(p_fecha,p_tipo,x.lca_codcli,
                x.lca_cuenta,x.lca_mod,9,nvl(w_provision_15,0)+nvl(w_provision_16,0));
   end if;
   end loop;
    commit;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Actualiza provision requerida
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure upd_provision_6_15_req( p_fecha in date, p_tipo in varchar2) is
  p_fechadesdenuevos date;
  cursor p is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado,
        lca_montogarreal,lca_suc,lca_ofi,lca_capitalcub,lca_capitalexp
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
   Order by lca_codcli;
   w_diasmora number;
   w_califmora number;
   w_califcapacidad number;
   w_califrestruc number;
   w_califinicial number;
   w_califcub number;
   w_califexp number;
   w_provtotal_comercial number;
   w_provtotal_hipotecario number;
   w_provtotal_consumo number;
   w_provtotal_microempresa number;
   w_provision number:=0;
  begin
   for x in p loop
   begin
	select sum(pdt_valordir)
	  into w_provision
	  from tpre_prevdettipos
	 where pdt_fecha = p_fecha
	   and pdt_tipo = 'P'
	   --and pdt_codcli = :tpre_calificadet.cad_codcli
	   and pdt_mod = x.lca_mod
	   and pdt_cuenta = x.lca_cuenta
	   and pdt_tipoprev in (9,8) --Ind.
	   and pdt_hasta is null;
exception
	when others then
	w_provision:=0;
end;
        update tpre_calificadet
          set cad_prevconstitdir= w_provision
         where cad_fecha = p_fecha
           and cad_tipo = p_tipo
           and cad_mod = x.lca_mod
           and cad_cuenta = x.lca_cuenta;
   end loop;
    commit;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo de la previson de cartera
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure upd_provision_grad ( p_fecha in date, p_tipo in varchar2 ) is
  p_fechadesdenuevos date;
  cursor p is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado,
        lca_montogarreal,lca_suc,lca_ofi,lca_capitalcub,lca_capitalexp
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and lca_cuenta in (select pre_credito
    			         from tpre_prestamos
                        where pre_fecontab between p_fechadesdenuevos and p_fecha)
   Order by lca_codcli;
   w_diasmora number;
   w_califmora number;
   w_califcapacidad number;
   w_califrestruc number;
   w_califinicial number;
   w_califcub number;
   w_califexp number;
   w_provtotal_comercial number;
   w_provtotal_hipotecario number;
   w_provtotal_consumo number;
   w_provtotal_microempresa number;
   --NOV
   w_provtotal_medianos NUMBER;
   
  begin
   /*delete tpre_prevdettipos
   where pdt_fecha = p_fecha
    and pdt_tipo = p_tipo
    and pdt_tipoprev not in (5,12,17); */
 begin
  SELECT decode(to_char(p_fecha,'mm'),'01',last_day(add_months(p_fecha,-1)),'02',last_day(add_months(p_fecha,-2)),last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd'))) + 1--last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd')) + 1
    INTO p_fechadesdenuevos
 	  FROM tpre_prevdefantparam
 	 WHERE pda_anio = to_number(to_char(P_FECHA,'yyyy'))
     AND pda_mes <= to_number(to_char(P_FECHA,'mm'))
     AND pda_nromeses = 3 ;
  exception
  	when others then
  	p_fechadesdenuevos:=p_fecha;
  end;
   w_provtotal_comercial:= total_prov_general(p_fecha,p_tipo,9,'C');--comercial capital
   w_provtotal_hipotecario:= total_prov_general(p_fecha,p_tipo,9,'H');--hipotecarios capital
   w_provtotal_consumo:= total_prov_general(p_fecha,p_tipo,9,'O');--consumo capital
   w_provtotal_microempresa:= total_prov_general(p_fecha,p_tipo,9,'M');--microempresa capital
   w_provtotal_medianos:= total_prov_general(p_fecha,p_tipo,9,'D');--microempresa capital  Nov 2017 
   --w_provtotal_comercial := nvl(w_provtotal_comercial,0) + nvl(w_provtotal_microempresa,0);
   for x in p loop
     if x.lca_codtipocredito = 'C' then
     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,11,w_provtotal_comercial);
     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,w_provtotal_comercial);                         */
     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
                      x.lca_cuenta,x.lca_mod,10,w_provtotal_comercial);
     elsif x.lca_codtipocredito = 'H' then
     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,11,w_provtotal_hipotecario);
     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,w_provtotal_hipotecario);*/
     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
                      x.lca_cuenta,x.lca_mod,10,w_provtotal_hipotecario);
     elsif x.lca_codtipocredito = 'O' then
     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,11,w_provtotal_consumo);
     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,w_provtotal_consumo);*/
     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
                      x.lca_cuenta,x.lca_mod,10,w_provtotal_consumo);
     elsif x.lca_codtipocredito = 'M' then
     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,11,w_provtotal_microempresa);
     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,w_provtotal_microempresa);*/
     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
                      x.lca_cuenta,x.lca_mod,10,w_provtotal_microempresa);
     elsif x.lca_codtipocredito = 'D' then
     prov_gradual_cap(p_fecha,p_tipo,x.lca_codcli,
                         x.lca_cuenta,x.lca_mod,11,w_provtotal_medianos);
     /*prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,w_provtotal_microempresa);*/
     prov_gradual_int(p_fecha,p_tipo,x.lca_codcli,
                      x.lca_cuenta,x.lca_mod,10,w_provtotal_medianos);
     end if;
   end loop;
    commit;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- calculo de la previson de cartera constituida
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure provision_cartera_cons ( p_fecha in date, p_tipo in varchar2 ) is
  cursor pc is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado,
        lca_montogarreal,lca_suc,lca_ofi
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and lca_codtipocredito in ('C','M','D')
    --and nvl(lca_provisionado,'N') = 'N'
    --and lca_esnuevo = 'N'
    /*and lca_cuenta not in (select lea_operacion
						     from tleg_adjudicados
						    where trunc(lea_fechaadjud) <= p_fecha)*/
   Order by lca_codcli;
  cursor po is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado,
        lca_montogarreal,lca_suc,lca_ofi
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and lca_codtipocredito = 'O'
    --and nvl(lca_provisionado,'N') = 'N'
    --and lca_esnuevo = 'N'
    /*and lca_cuenta not in (select lea_operacion
						     from tleg_adjudicados
						    where trunc(lea_fechaadjud) <= p_fecha)*/
   Order by lca_codcli;
  cursor ph is
  select lca_fecha, lca_tipo, lca_codcli, lca_mod, lca_cuenta, lca_prod, lca_tip, lca_mon, lca_gar,
 		lca_riesgopais, lca_reestructurado, lca_demandajundicial, lca_capital, lca_interes,
 		lca_montogaradmi, lca_fecreestruc, lca_diasreestruc, lca_fecrenov, lca_codigopais, lca_diasmoracap,
 		lca_diasmoraint, lca_tabtipocredito, lca_tipocredito, lca_codtipocredito, lca_status, lca_procesado,
        lca_montogarreal,lca_suc,lca_ofi
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_tipo = p_tipo
    and lca_codtipocredito = 'H'
    --and lca_esnuevo = 'N'
    /*and lca_cuenta not in (select lea_operacion
						     from tleg_adjudicados
						    where trunc(lea_fechaadjud) <= p_fecha)    */
    --and nvl(lca_provisionado,'N') = 'N'
   Order by lca_codcli;
   w_diasmora number;
   w_califmora number;
   w_califcapacidad number;
   w_califrestruc number;
   w_califinicial number;
   w_califcub number;
   w_califexp number;
   w_provtotal_comercial number;
   w_provtotal_hipotecario number;
   w_provtotal_consumo number;
   w_provtotal_microempresa number;
   p_provresul_12 number;
   p_provresul_17 number;
   p_provresul number;
   w_provtotal_mediano NUMBER;
  begin
   --para BDI
    --w_provtotal_comercial:= total_prov_general(p_fecha,p_tipo,9,20);--comercial capital
    --w_provtotal_hipotecario:= total_prov_general(p_fecha,p_tipo,9,21);--hipotecarios capital
    --w_provtotal_consumo:= total_prov_general(p_fecha,p_tipo,9,22);--consumo capital
    --w_provtotal_microempresa:= total_prov_general(p_fecha,p_tipo,9,23);--microempresa capital
   --para BLH
   w_provtotal_comercial:= total_prov_general(p_fecha,p_tipo,11,'C');--comercial capital
   w_provtotal_hipotecario:= total_prov_general(p_fecha,p_tipo,11,'H');--hipotecarios capital
   w_provtotal_consumo:= total_prov_general(p_fecha,p_tipo,11,'O');--consumo capital
   w_provtotal_microempresa:= total_prov_general(p_fecha,p_tipo,11,'M');--microempresa capital
   w_provtotal_mediano:= total_prov_general(p_fecha,p_tipo,11,'D');--microempresa capital   --NOV 2017
   w_provtotal_comercial := nvl(w_provtotal_comercial,0) + nvl(w_provtotal_microempresa,0) + nvl(w_provtotal_mediano,0);--Nov 2017
   --fin blh
   delete tpre_prevdettipos
   where pdt_fecha = p_fecha
    and pdt_tipo = p_tipo
    and pdt_tipoprev  in (5,12,17);
   for x in pc loop
    p_provresul_12:=0;
    p_provresul_17:=0;
     prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,11,w_provtotal_comercial,p_provresul_12);--15 BDI
     prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,17,10,w_provtotal_comercial,p_provresul_17);--8 bdi
     prov_constituida_total(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,5,11,nvl(p_provresul_12,0)+nvl(p_provresul_17,0),p_provresul);
   end loop;
   for x in po loop
    p_provresul_12:=0;
    p_provresul_17:=0;
     prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,11,w_provtotal_consumo,p_provresul_12);--15 BDI
     prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,17,10,w_provtotal_consumo,p_provresul_17);--8 bdi
     prov_constituida_total(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,5,11,nvl(p_provresul_12,0)+nvl(p_provresul_17,0),p_provresul);
   end loop;
   for x in ph loop
    p_provresul_12:=0;
    p_provresul_17:=0;
     prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,12,11,w_provtotal_hipotecario,p_provresul_12);--15 BDI
     prov_constituida_cap(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,17,10,w_provtotal_hipotecario,p_provresul_17);--8 bdi
     prov_constituida_total(p_fecha,p_tipo,x.lca_codcli,x.lca_codtipocredito,
                         x.lca_cuenta,x.lca_mod,5,11,nvl(p_provresul_12,0)+nvl(p_provresul_17,0),p_provresul);
   end loop;
    commit;
  end;


  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Función que devuelve el número de operaciones a procesar a una Fecha determinada y de un Tipo determinado
  -- de Calificación
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  function cuantas_operaciones ( p_fecha date, p_tipo varchar2 ) return number is

    w_cuantasporprocesar number(6);

  begin

    select count(*) into w_cuantasporprocesar
    from tleg_operacioncalif
    where lca_fecha = p_fecha
    and lca_tipo = p_tipo;

    return w_cuantasporprocesar;

  end cuantas_operaciones;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Para separar un mayor o menor deudor
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  ---monto autorizado para discriminar mayor o menor deudor
  FUNCTION monto_autorizado_leg(p_moneda   IN NUMBER,
                                p_sucursal IN NUMBER,
                                p_credito  in number,
                                p_fecha    in date) RETURN NUMBER IS
    v_Monto_Org NUMBER;
    v_Monto_Inc NUMBER;
    v_Cobros    NUMBER;
    bCompromiso BOOLEAN := FALSE;
    --Colaboracion de p.p PMC
    CURSOR curInc IS
      SELECT *
        FROM tpre_inccapital
       WHERE inc_credito = p_credito
         AND inc_transa = 36
            --AND inc_fechasol = nvl(pApplyForDate_i,inc_fechasol)
          AND TRUNC(inc_fechaaut) <= p_Fecha
          AND inc_numtra IS NULL
          AND inc_codusr IS NOT NULL
          AND inc_status = 2; /*Autorizados*/
    vln_Capitalized NUMBER(18, 6) := 0;
    vln_Aux         NUMBER(18, 6);
    compromiso      number(10) := 0;
    w_tipocuo       number(4);
  BEGIN
    BEGIN
      -- Obtenemos el monto original, que no se modifica
      SELECT pre_valent, pre_numcomprom,pre_tipocuo
        INTO v_Monto_Org, compromiso,w_tipocuo
        FROM tpre_prestamos
       WHERE pre_credito = p_credito;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        bCompromiso := TRUE;
        v_Monto_Org := 0;
        w_tipocuo:=0;
      WHEN OTHERS THEN
        Raise_Application_Error(-20999,
                                'Error en Saldo_Actual_Capital ' ||
                                TO_CHAR(p_credito));
    END;
    --SE COLOCA ESTA VALIDACIN E-MAIL ROSEMARY 2013/10/08 1500 INDICA QUE LOS 604 O DOCUMENTOS DESCONTADOS
    --SE DEBE SACAR EL MONTO ORIGINAL Y NO TOMAR EL MONTO DE LA LINEA O 612
    IF w_tipocuo = 6 THEN
       compromiso:=0;
    END IF;
    IF nvl(compromiso, 0) <> 0 THEN
      /*
      ** En el caso de Compromiso, devuelve el monto pactado hasta un fecha X, pero no su saldo!!
      ** Esto es necesario ya que se la utiliza al reversar el uso de garantías.
      ** Pero hay que considerar que una garantía SIEMPRE cubre el total del monto del Compromiso y no su Saldo!!
      */
      SELECT SUM(NVL(dcm_valor, 0))
        INTO v_Monto_Org
        FROM tpre_detcomprom
       WHERE dcm_numero = compromiso
       AND trunc(dcm_fechaliq) <= p_Fecha;
      IF P_MONEDA = 0 THEN
        RETURN NVL(v_Monto_Org, 0);
      ELSE
        RETURN NVL(BDI_PROMEDIO(P_MONEDA, P_SUCURSAL, P_FECHA) *
                   v_Monto_Org,
                   0);
      END IF;

    END IF;
    -- Retorna la suma de los incrementos
    --   SELECT Nvl(Sum(inc_monto),0)
    SELECT nvl(sum(nvl(inc_monto, 0) - nvl(inc_retenido, 0)), 0)
      INTO v_Monto_Inc
      FROM tpre_inccapital
     WHERE inc_codusr IS NOT NULL
       AND inc_numtra IS NOT NULL
       AND inc_status = 2
       AND inc_transa <> 36
       AND inc_credito = p_credito
       AND TRUNC(inc_fechaaut) <= p_Fecha;
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
    IF P_MONEDA = 0 THEN
      RETURN v_Monto_Org + v_Monto_Inc + vln_Capitalized;
    ELSE
      RETURN NVL(BDI_PROMEDIO(P_MONEDA, P_SUCURSAL, P_FECHA) *
                 (v_Monto_Org + v_Monto_Inc + vln_Capitalized),
                 0);
    END IF;
    --RETURN v_Monto_Org + v_Monto_Inc + vln_Capitalized;
  END;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Para separar un mayor o menor deudor
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  PROCEDURE elimina_calprov(p_fecha in date, p_tipo in varchar2) IS
  BEGIN
    DELETE TPRE_PREVDETTIPOS WHERE pdt_fecha = p_fecha and pdt_tipo = p_tipo;
    DELETE tpre_califdettipos WHERE cdt_fecha = p_fecha and cdt_tipo = p_tipo;
    DELETE tpre_calificaprovdet WHERE cad_fecha = p_fecha and cad_tipo = p_tipo;
    DELETE tpre_calificaprov WHERE cal_fecha = P_fecha and cal_tipo = p_tipo ;
    DELETE TLEG_OPERACIONCALIF WHERE lca_fecha = P_fecha and lca_tipo = p_tipo ;
    DELETE TPRE_CALIFICADET WHERE CAD_FECHA = P_FECHA AND CAD_TIPO = p_tipo;
    DELETE TPRE_CALIFICA WHERE CAL_FECHA = p_FECHA AND CAL_TIPO = p_tipo;
    DELETE tleg_opergaran  WHERE LOG_FECHA = p_FECHA AND LOG_TIPO = p_tipo;
    --DELETE TPRE_PARAMCALIF WHERE PCA_FECHA = P_FECHA AND PCA_TIPO = p_tipo;
    COMMIT;
  END;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Para separar un mayor o menor deudor
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  PROCEDURE elimina_provision_cons(p_fecha in date, p_tipo in varchar2) IS
  BEGIN
     delete tpre_prevdettipos
   where pdt_fecha = p_fecha
    and pdt_tipo = p_tipo
    and  pdt_tipoprev  in (5,12,17);
    COMMIT;
  END;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Para separar un mayor o menor deudor
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  PROCEDURE elimina_provision_gradual(p_fecha in date, p_tipo in varchar2) IS
  w_existeprev number:=0;
  v_nromeses number;
  BEGIN
      v_nromeses :=0;
      begin
      select nvl(pda_nromeses,0)
        into v_nromeses
       from tpre_prevdefantparam
      where pda_anio = to_char(p_fecha,'yyyy')
        and pda_mes = to_char(p_fecha,'mm');
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
 if nvl(v_nromeses,0) = 3 then
    select count(*)
     into w_existeprev
      from tpre_prevdettipos
     where pdt_fecha = p_fecha
       and pdt_tipo = p_tipo
       and  pdt_tipoprev  in (12,17);
    if nvl(w_existeprev,0) > 0 then
          raise_application_error (-20505,'No Puede Eliminar Provision Requerida Y gradual ..Existe Prov. Constituida..revise: ');
    end if;
     delete tpre_prevdettipos
   where pdt_fecha = p_fecha
    and pdt_tipo = p_tipo;

    update tleg_operacioncalif
      set lca_provisionado = 'N'
      where lca_fecha = p_fecha
        and lca_tipo = p_tipo  ;
   else
    select count(*)
     into w_existeprev
      from tpre_prevdettipos
     where pdt_fecha = p_fecha
       and pdt_tipo = p_tipo
       and  pdt_tipoprev  in (12,17)
       and pdt_cuenta in (select lca_cuenta from tleg_operacioncalif where lca_fecha =  p_fecha and lca_tipo = p_tipo and lca_esnuevo='S');
    if nvl(w_existeprev,0) > 0 then
          raise_application_error (-20505,'No Puede Eliminar Provision Requerida Y gradual ..Existe Prov. Constituida..revise: ');
    end if;
     delete tpre_prevdettipos
      where pdt_fecha = p_fecha
       and pdt_tipo = p_tipo
       and  pdt_tipoprev  in (10,11);

     delete tpre_prevdettipos
      where pdt_fecha = p_fecha
       and pdt_tipo = p_tipo
       and  pdt_tipoprev  in (8,9,15,16,18,19)
       and pdt_cuenta in (select lca_cuenta from tleg_operacioncalif where lca_fecha =  p_fecha and lca_tipo = p_tipo and lca_esnuevo='S');

    update tleg_operacioncalif
      set lca_provisionado = 'N'
      where lca_fecha = p_fecha
        and lca_tipo = p_tipo;
        --and lca_esnuevo='S';
 end if;

    --DELETE TPRE_PARAMCALIF WHERE PCA_FECHA = P_FECHA AND PCA_TIPO = p_tipo;
    COMMIT;
  END;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Para actualizar en la 6/15
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

   procedure upd_tipo_credito(p_credito in number,
                              p_fecha in date,
                              p_dml varchar2,
 					    p_tipocredito in number) is
   w_tipocredito number;
   begin
     if p_dml='D' then
        select LCA_TIPOCREDITO
          into w_tipocredito
         from tleg_operacioncalif
         where lca_cuenta = p_credito
           and lca_fecha = (select max(lca_fecha)
                              from tleg_operacioncalif
                             where lca_cuenta = p_credito
                               and lca_fecha <p_fecha);
    elsif p_dml='U' then
       w_tipocredito:=p_tipocredito;
    end if;
     update tpre_prestamos
       set PRE_NOGENHISTORICO='1',
           PRE_TIPOCREDITO=w_tipocredito
     where pre_credito = p_credito;
   end;
  --antes
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Para separar un mayor o menor deudor
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  FUNCTION tipo_credito_antes(p_cliente in number,
  					    p_tipocredito in number,
                        p_fecha    in date,
                        p_saldopre    in number,
                        p_montoconsolidado out number) RETURN VARCHAR2 IS
    v_Monto_Org NUMBER;
    v_Monto_Inc NUMBER;
    v_Cobros    NUMBER;
    bCompromiso BOOLEAN := FALSE;
    vln_Capitalized NUMBER(18, 6) := 0;
    vln_Aux         NUMBER(18, 6);
    compromiso      number(10) := 0;
  w_montocompara number;
  w_tipocredito varchar2(8);
  w_registros  number:=0;
  w_clientep number;
  w_sumatotal number := 0;
cursor p is
		select distinct pre_clientep,nvl(pre_numcomprom,pre_credito) operacion,
		(pkg_precalifica_do.MONTO_AUTORIZADO_LEG(PRE_MONEDA, PRE_SUCURSAL, PRE_CREDITO, P_FECHA)) monto  --se comenta por nuevo cambio 2013/06/06 rafael, sr sunchio , Daneyury
		--p_saldopre monto
        --(nvl(acr_capred,0) + nvl(acr_capnogi,0) + nvl(acr_capvencido,0))*bdi_promedio(pre_moneda,pre_sucursal,p_fecha) +
        --(DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'-',acr_intacum,0))) + decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) + DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'J',0,acr_intactven)))) * bdi_promedio(pre_moneda,pre_sucursal,p_fecha) monto
		from tpre_accrual,
		tpre_prestamos,
		tpre_comprom,
		tpre_protipocredito
		where ptc_mod = pre_mod and
		ptc_pro = pre_pro and
		ptc_tip = pre_tip and
		ptc_mon = pre_moneda and
		acr_fecha = p_fecha and
		pre_credito = acr_credito and
		pre_clientep = p_cliente and
		ptc_tipocredito = p_tipocredito and
		pre_credito not in (select crd_credito from tleg_cambiorangodeuda where crd_tipocredito <> p_tipocredito) and
		cmp_numero(+) = pre_numcomprom and
		cmp_clientep(+) = pre_clientep;
cursor pc is --pra sobregiros contratados
		select distinct vis_codcli pre_clientep,vis_numcue  operacion,
         crd_valor * bdi_promedio(vis_moneda,vis_suc,p_fecha)  monto
         --p_saldopre monto
         --nvl(aca_utilizado,0) * bdi_promedio(vis_moneda,vis_suc,p_fecha) +
         --nvl(aca_interes,0) * bdi_promedio(vis_moneda,vis_suc,p_fecha) monto
		from tcap_acract tpre_accrual,
		tcap_vista tpre_prestamos,
		tcap_credvista tpre_comprom,
		tpre_protipocredito
		where ptc_mod = vis_mod and
		ptc_pro = vis_pro and
		ptc_tip = vis_tip and
		ptc_mon = vis_moneda and
		aca_fecha(+) = p_fecha and
	    aca_numcue(+) = crd_numcue and
	    aca_tipocre(+) = crd_tipocred and
	    aca_secuencia(+) = crd_secuencia and
		aca_numcue(+) = crd_numcue and
	    crd_tipocred = 2 and
        crd_fechavig <= p_fecha and
        trunc(crd_fechautor) <= p_fecha and
		crd_fechavenc >= p_fecha and
        nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > p_fecha and
        nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > p_fecha and
		vis_codcli = p_cliente and
		ptc_tipocredito = p_tipocredito and
		vis_numcue not in (select crd_credito from tleg_cambiorangodeuda where crd_tipocredito <> p_tipocredito) and
		crd_numcue = vis_numcue ;
 cursor pcomex is
      ((SELECT MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter * bdi_promedio(CYLETTER,branch,p_fecha) acr_contingencia,
             dateexpired pre_fecven,
             branch pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter)* bdi_promedio(CYLETTER,branch,p_fecha) acr_capital,
             operator pre_codeje ,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
             nvl(status,'X') pre_status,
             20 ptc_tipocredito,
             typeprod pre_tip,
             prod pre_pro,
             dateissue fecha_Emision
        FROM tlettercredit
       WHERE --NVL(status, '!') = '!'
       --decode(CYLETTER,0,valueletter,bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) >       p_Montocompara AND
       seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
       codeclient = p_cliente  AND
       codeletter not in
       (select codeletter
          FROM tlettercredit A, Tstageprod B, tstage C
         WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
               a.module = c.module AND a.prod = c.prod AND codeclient = p_cliente  AND
               a.typeprod = c.typeprod AND a.seqamen = b.seq and
               c.stage = 'ST_CLOSE' and DATEPROCESS <= p_fecha + 0.99999)
      UNION
      SELECT a.MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter* bdi_promedio(CYLETTER,branch,p_fecha) Montoaprobado,
             dateexpired pre_fecven,
             branch pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter)* bdi_promedio(CYLETTER,branch,p_fecha) Balance,
             operator,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             DATEPROCESS,
             nvl(a.status,'X'),
             20 ptc_tipocredito,
             a.typeprod,
             a.prod,
             dateissue
        FROM tlettercredit A, Tstageprod B, tstage C
       WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
             a.module = c.module AND a.prod = c.prod AND
             a.typeprod = c.typeprod AND a.seqamen = b.seq AND        codeclient = p_cliente  AND
             c.stage = 'ST_CLOSE' AND NVL(a.status, '!') in ('C', 'P') AND
             1 = 0 AND --SE DESABILITA LOS CANCELADOS O CERRADOS NO VAN
             --decode(CYLETTER,0,valueletter,bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) > p_Montocompara AND
             seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
             DATEPROCESS between p_fecha and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codedraft,
                    valuedraft*bdi_promedio(CYDRAFT,branch,p_fecha) valuedraft,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codedraft) * bdi_promedio(CYDRAFT,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    nvl(status,'X'),
                    20 ptc_tipocredito,
                    typeprod,
                    prod,
                    dateissue
               FROM tdraft
              WHERE --NVL(status, '!') = '!'
              --decode(CYDRAFT,0,valuedraft,bdi_promedio(CYDRAFT, branch, p_fecha) * valuedraft) > P_Montocompara AND
              codeclient = p_cliente  AND
              codedraft not in
              (select codedraft
                 FROM tdraft A, Tstageprod B, tstage C
                WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND
                     --a.seqamen = b.seq and
                     codeclient = p_cliente  AND
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
                                                          codedraft) * bdi_promedio(CYDRAFT,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    DATEPROCESS,
                    nvl(a.status,'X'),
                    20 ptc_tipocredito,
                    a.typeprod,
                    a.prod,
                    dateissue
               FROM tdraft A, Tstageprod B, tstage C
              WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND codeclient = p_cliente  AND
                    1 = 0 AND --SE DESABILITA LOS CANCELADOS O CERRADOS NO VAN
                    --decode(CYDRAFT,0,valuedraft,bdi_promedio(CYDRAFT, branch, p_fecha) * valuedraft) > P_Montocompara AND
                    DATEPROCESS between p_fecha and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codeguarantee,
                    valueguarantee*bdi_promedio(CYGUARANTEE,branch,p_fecha) valueguarantee,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codeguarantee) * bdi_promedio(CYGUARANTEE,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    nvl(status,'X'),
                    20 ptc_tipocredito,
                    typeprod,
                    prod,
                    datevalue
               FROM tguarantee
              WHERE --NVL(status, '!') = '!' AND
              --decode(CYGUARANTEE,0,valueguarantee,bdi_promedio(CYGUARANTEE, branch, p_fecha) * valueguarantee) > p_Montocompara AND
              seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
              codeclient = p_cliente  AND
              codeguarantee not in
              (select codeguarantee
                 FROM tguarantee A, Tstageprod B, tstage C
                WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND a.seqamen = b.seq and
                      c.stage = 'ST_CLOSE' and codeclient = p_cliente  AND
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
                                                          codeguarantee) * bdi_promedio(CYGUARANTEE,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    DATEPROCESS,
                    nvl(a.status,'X'),
                    20 ptc_tipocredito,
                    a.typeprod,
                    a.prod,
                    datevalue
               FROM tguarantee A, Tstageprod B, tstage C
              WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND a.seqamen = b.seq AND
                    c.stage = 'ST_CLOSE' AND codeclient = p_cliente  AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    1 = 0 AND --SE DESABILITA LOS CANCELADOS O CERRADOS NO VAN
                    --decode(CYGUARANTEE,0,valueguarantee,bdi_promedio(CYGUARANTEE, branch, p_fecha) * valueguarantee) > p_Montocompara AND
                    seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
                    DATEPROCESS between p_fecha and p_fecha + 0.99999));
  w_sumaconsolidado Number:=0;
  w_esmayorxventas VARCHAR2(1):='N';
  BEGIN
    --RETURN v_Monto_Org + v_Monto_Inc + vln_Capitalized;
    /*select par_valor
      into w_montocompara
      from tpre_paramcaliftipo
     where par_tipocredito = p_tipocredito;*/--tomar siempre de la tabla 244
            w_montocompara:=to_number(parametro_calif('PAR_MONTO_MAYOR_ANTES'));
		  select codeiso
		    into w_tipocredito
		   from tgen_desctabla
		   where des_codtab =  244
		     and des_codigo = p_tipocredito;--si no es comercial mayor deudor , toma el valor de microcrdito + comerciales menors deudores

   dbms_output.put_line('tipocred 1:'||p_tipocredito||'-'||w_tipocredito);
   if p_tipocredito = 20 then
        begin
		 w_sumatotal := 0;
		for x in p loop
		 w_sumatotal := nvl(w_sumatotal,0) + x.monto;
		end loop;
		for x in pc loop
		 w_sumatotal := nvl(w_sumatotal,0) + x.monto;
		end loop;
		for x in pcomex loop
		 --w_sumatotal := nvl(w_sumatotal,0) + x.acr_contingencia;--por el envio de mayo se cambia
		 w_sumatotal := nvl(w_sumatotal,0) + x.acr_capital;
		end loop;
		  if w_sumatotal >= w_montocompara then
			return w_tipocredito;
		  elsif w_esmayorxventas = 'S' then
		      return w_tipocredito;
		  else
				select codeiso
				into w_tipocredito
				from tgen_desctabla
				where des_codtab = 244
				and des_codigo = 23;--si no es comercial mayor deudor , toma el valor de microcrdito + comerciales menors deudores
				return w_tipocredito;
		  end if;
		exception
		when no_data_found then
			select codeiso
	 		  into w_tipocredito
			  from tgen_desctabla
			  where des_codtab = 244
			   and des_codigo = 23;--si no es comercial mayor deudor , toma el valor de microcrdito + comerciales menors deudores
			return w_tipocredito;
		when others then
		return w_tipocredito;
		end;
   else -- p_tipocredito in (21,22,23) then
         dbms_output.put_line('tipocred 3:'||p_tipocredito||'-'||w_tipocredito);
      return w_tipocredito;
   end if;
  exception
   when others then
          raise_application_error (-20505,'Error: ' ||w_tipocredito||' - '|| substr(1,120,sqlerrm));
  END;
  --fin de antes
FUNCTION f_fechamaxincremento(p_operacion in number,p_fecha in date) return date is
w_maxfecha date;
cursor p is
  select PRE_FECONTAB,pre_credito
    from tpre_prestamos,tpre_accrual
    where pre_credito = acr_credito
      and acr_fecha = p_fecha
      and pre_numcomprom = p_operacion;
BEGIN
  for x in p loop
   if x.pre_fecontab >= to_date(parametro_calif('PAR_FECHA_NUEVOS'),'yyyymmdd') then
      return x.pre_fecontab;
    end if;
  end loop;
     select max(dcm_fecha)
      into w_maxfecha
      from tpre_detcomprom
      where dcm_numero = p_operacion
       and dcm_numtra is not null;
       return w_maxfecha;
END;



  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Para separar un mayor o menor deudor
  --2013/10/01 Rosemary, rafael, sr sunchio, daneury
  --cambios: campo de monto autorizado sea modificable, colocar marca que es manual , para tomar del dia anterior
  --buscar creditos nuevos o incrementos nuevos , si son nuevos validar contra el archivo consolidado
  --con los 25 millones
  --el monto para mayores y menores , se toma del monto autorizado por el sistema(aqui deben arreglar manualmente
  --PENDIENTE:  migrar ciiu excel, sacar ciiu del cliente para el de08
  --            sacar ciiu del cliente para el de11 y ciiu destino del prestamo en el de11
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  FUNCTION tipo_credito(p_cliente in number,
  					    p_tipocredito in number,
                        p_fecha    in date,
                        p_saldopre    in number,
                        p_montoconsolidado out number) RETURN VARCHAR2 IS
    v_Monto_Org NUMBER;
    v_Monto_Inc NUMBER;
    v_Cobros    NUMBER;
    bCompromiso BOOLEAN := FALSE;
    vln_Capitalized NUMBER(18, 6) := 0;
    vln_Aux         NUMBER(18, 6);
    compromiso      number(10) := 0;
    w_montocompara number;
    w_tipocredito varchar2(8);
    w_registros  number:=0;
    w_clientep number;
    w_sumatotal number := 0;
cursor p is
		select distinct pre_clientep,nvl(pre_numcomprom,pre_credito) operacion, PRE_FECONTAB,pre_tipocuo,
		(pkg_precalifica_do.MONTO_AUTORIZADO_LEG(PRE_MONEDA, PRE_SUCURSAL, PRE_CREDITO, P_FECHA)) monto  --se comenta por nuevo cambio 2013/06/06 rafael, sr sunchio , Daneyury
		--p_saldopre monto
        --(nvl(acr_capred,0) + nvl(acr_capnogi,0) + nvl(acr_capvencido,0))*bdi_promedio(pre_moneda,pre_sucursal,p_fecha) +
        --(DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'-',acr_intacum,0))) + decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) + DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'J',0,acr_intactven)))) * bdi_promedio(pre_moneda,pre_sucursal,p_fecha) monto
        --se comenta reunion 2013/10/01 rosemary, daneury , sr sunchio, rafael  y el nuevo
		from tpre_accrual,
		tpre_prestamos,
		tpre_comprom,
		tpre_protipocredito
		where ptc_mod = pre_mod and
		ptc_pro = pre_pro and
		ptc_tip = pre_tip and
		ptc_mon = pre_moneda and
		acr_fecha = p_fecha and
		pre_credito = acr_credito and
		pre_clientep = p_cliente and
		ptc_tipocredito = p_tipocredito and
		pre_credito not in (select crd_credito from tleg_cambiorangodeuda where crd_tipocredito <> p_tipocredito) and
		cmp_numero(+) = pre_numcomprom and
		cmp_clientep(+) = pre_clientep;
cursor pc is --pra sobregiros contratados
		select distinct vis_codcli pre_clientep,vis_numcue  operacion,trunc(crd_fechautor) crd_fechautor,
         crd_valor * bdi_promedio(vis_moneda,vis_suc,p_fecha)  monto
         --p_saldopre monto
         --nvl(aca_utilizado,0) * bdi_promedio(vis_moneda,vis_suc,p_fecha) +
         --nvl(aca_interes,0) * bdi_promedio(vis_moneda,vis_suc,p_fecha) monto
         --se comenta reunion 2013/10/01 rosemary, daneury , sr sunchio, rafael  y el nuevo
		from tcap_acract tpre_accrual,
		tcap_vista tpre_prestamos,
		tcap_credvista tpre_comprom,
		tpre_protipocredito
		where ptc_mod = vis_mod and
		ptc_pro = vis_pro and
		ptc_tip = vis_tip and
		ptc_mon = vis_moneda and
		aca_fecha(+) = p_fecha and
	    aca_numcue(+) = crd_numcue and
	    aca_tipocre(+) = crd_tipocred and
	    aca_secuencia(+) = crd_secuencia and
		aca_numcue(+) = crd_numcue and
	    crd_tipocred = 2 and
        crd_fechavig <= p_fecha and
        trunc(crd_fechautor) <= p_fecha and
		crd_fechavenc >= p_fecha and
        nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > p_fecha and
        nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > p_fecha and
		vis_codcli = p_cliente and
		ptc_tipocredito = p_tipocredito and
		vis_numcue not in (select crd_credito from tleg_cambiorangodeuda where crd_tipocredito <> p_tipocredito) and
		crd_numcue = vis_numcue ;
 cursor pcomex is
      ((SELECT MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter * bdi_promedio(CYLETTER,branch,p_fecha) acr_contingencia,
             dateexpired pre_fecven,
             branch pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter)* bdi_promedio(CYLETTER,branch,p_fecha) acr_capital,
             operator pre_codeje ,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
             nvl(status,'X') pre_status,
             20 ptc_tipocredito,
             typeprod pre_tip,
             prod pre_pro,
             dateissue fecha_Emision
        FROM tlettercredit
       WHERE --NVL(status, '!') = '!'
       --decode(CYLETTER,0,valueletter,bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) >       p_Montocompara AND
       seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
       codeclient = p_cliente  AND
       codeletter not in
       (select codeletter
          FROM tlettercredit A, Tstageprod B, tstage C
         WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
               a.module = c.module AND a.prod = c.prod AND codeclient = p_cliente  AND
               a.typeprod = c.typeprod AND a.seqamen = b.seq and
               c.stage = 'ST_CLOSE' and DATEPROCESS <= p_fecha + 0.99999)
      UNION
      SELECT a.MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter* bdi_promedio(CYLETTER,branch,p_fecha) Montoaprobado,
             dateexpired pre_fecven,
             branch pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter)* bdi_promedio(CYLETTER,branch,p_fecha) Balance,
             operator,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             DATEPROCESS,
             nvl(a.status,'X'),
             20 ptc_tipocredito,
             a.typeprod,
             a.prod,
             dateissue
        FROM tlettercredit A, Tstageprod B, tstage C
       WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
             a.module = c.module AND a.prod = c.prod AND
             a.typeprod = c.typeprod AND a.seqamen = b.seq AND        codeclient = p_cliente  AND
             c.stage = 'ST_CLOSE' AND NVL(a.status, '!') in ('C', 'P') AND
             1 = 0 AND --SE DESABILITA LOS CANCELADOS O CERRADOS NO VAN
             --decode(CYLETTER,0,valueletter,bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) > p_Montocompara AND
             seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
             DATEPROCESS between p_fecha and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codedraft,
                    valuedraft*bdi_promedio(CYDRAFT,branch,p_fecha) valuedraft,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codedraft) * bdi_promedio(CYDRAFT,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    nvl(status,'X'),
                    20 ptc_tipocredito,
                    typeprod,
                    prod,
                    dateissue
               FROM tdraft
              WHERE --NVL(status, '!') = '!'
              --decode(CYDRAFT,0,valuedraft,bdi_promedio(CYDRAFT, branch, p_fecha) * valuedraft) > P_Montocompara AND
              codeclient = p_cliente  AND
              codedraft not in
              (select codedraft
                 FROM tdraft A, Tstageprod B, tstage C
                WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND
                     --a.seqamen = b.seq and
                     codeclient = p_cliente  AND
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
                                                          codedraft) * bdi_promedio(CYDRAFT,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    DATEPROCESS,
                    nvl(a.status,'X'),
                    20 ptc_tipocredito,
                    a.typeprod,
                    a.prod,
                    dateissue
               FROM tdraft A, Tstageprod B, tstage C
              WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND codeclient = p_cliente  AND
                    1 = 0 AND --SE DESABILITA LOS CANCELADOS O CERRADOS NO VAN
                    --decode(CYDRAFT,0,valuedraft,bdi_promedio(CYDRAFT, branch, p_fecha) * valuedraft) > P_Montocompara AND
                    DATEPROCESS between p_fecha and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codeguarantee,
                    valueguarantee*bdi_promedio(CYGUARANTEE,branch,p_fecha) valueguarantee,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codeguarantee) * bdi_promedio(CYGUARANTEE,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    nvl(status,'X'),
                    20 ptc_tipocredito,
                    typeprod,
                    prod,
                    datevalue
               FROM tguarantee
              WHERE --NVL(status, '!') = '!' AND
              --decode(CYGUARANTEE,0,valueguarantee,bdi_promedio(CYGUARANTEE, branch, p_fecha) * valueguarantee) > p_Montocompara AND
              seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
              codeclient = p_cliente  AND
              codeguarantee not in
              (select codeguarantee
                 FROM tguarantee A, Tstageprod B, tstage C
                WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
                      a.module = c.module AND a.prod = c.prod AND
                      a.typeprod = c.typeprod AND a.seqamen = b.seq and
                      c.stage = 'ST_CLOSE' and codeclient = p_cliente  AND
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
                                                          codeguarantee) * bdi_promedio(CYGUARANTEE,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    DATEPROCESS,
                    nvl(a.status,'X'),
                    20 ptc_tipocredito,
                    a.typeprod,
                    a.prod,
                    datevalue
               FROM tguarantee A, Tstageprod B, tstage C
              WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND a.seqamen = b.seq AND
                    c.stage = 'ST_CLOSE' AND codeclient = p_cliente  AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    1 = 0 AND --SE DESABILITA LOS CANCELADOS O CERRADOS NO VAN
                    --decode(CYGUARANTEE,0,valueguarantee,bdi_promedio(CYGUARANTEE, branch, p_fecha) * valueguarantee) > p_Montocompara AND
                    seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
                    DATEPROCESS between p_fecha and p_fecha + 0.99999));
  cursor Inc(p_credito in number) is
    SELECT TRUNC(inc_fechaaut) inc_fechaaut
      FROM tpre_inccapital
     WHERE inc_codusr IS NOT NULL
       AND inc_numtra IS NOT NULL
       AND inc_status = 2
       AND inc_transa <> 36
       AND inc_credito = p_credito
       AND TRUNC(inc_fechaaut) <= p_Fecha;

  w_sumaconsolidado Number:=0;
  w_esmayorxventas VARCHAR2(1):='N';
  w_es_nuevo varchar2(1);
  w_es_nuevo_fin varchar2(1);
  w_fechacontab date;
--Nov.2017
    w_montocomparamenor number:=0;
    w_codnummediano number:=0;		
      
  BEGIN
    --RETURN v_Monto_Org + v_Monto_Inc + vln_Capitalized;
    select par_valor
      into w_montocompara
      from tpre_paramcaliftipo
     where par_tipocredito = p_tipocredito;--tomar siempre de la tabla 244
     --nov 2017
     w_montocomparamenor:=  parametro_calif('MONTO_MENOR_DEUDOR');
      w_codnummediano:=  parametro_calif('CODNUM_MEDIANOS_DEUDORES');

		  select codeiso
		    into w_tipocredito
		   from tgen_desctabla
		   where des_codtab =  244
		     and des_codigo = p_tipocredito;--si no es comercial mayor deudor , toma el valor de microcrdito + comerciales menors deudores

   dbms_output.put_line('tipocred 1:'||p_tipocredito||'-'||w_tipocredito);
   --insert into log_batch values('1:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
   if p_tipocredito = 20 then
        begin
		 w_sumatotal := 0;
		for x in p loop
		 w_sumatotal := nvl(w_sumatotal,0) + x.monto;
		end loop;
		   --insert into log_batch values('2:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
		  w_es_nuevo := 'N';
		  w_es_nuevo_fin :='N';
		for x in p loop
           if x.pre_tipocuo = 6 then
		      w_fechacontab:=x.PRE_FECONTAB;
		   else
		      w_fechacontab:=nvl(f_fechamaxincremento(x.operacion,p_fecha),x.PRE_FECONTAB);
		   end if;
          if w_fechacontab >= to_date(parametro_calif('PAR_FECHA_NUEVOS'),'yyyymmdd') then
             w_es_nuevo := 'S';
             exit;
          end if;
          --if
		end loop;
		   --insert into log_batch values('3:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
		if w_es_nuevo = 'N' then
		for x in p loop
		  for y in Inc(x.operacion) loop
		          if y.inc_fechaaut >= to_date(parametro_calif('PAR_FECHA_NUEVOS'),'yyyymmdd') then
		             w_es_nuevo := 'S';
		             exit;
		          end if;
          end loop;
          if w_es_nuevo = 'S' then
	             exit;
	      end if;
		end loop;
		end if;
		   --insert into log_batch values('4:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
		for x in pc loop
		 w_sumatotal := nvl(w_sumatotal,0) + x.monto;
		end loop;
		if w_es_nuevo = 'N' then
		for x in pc loop
          if x.crd_fechautor >= to_date(parametro_calif('PAR_FECHA_NUEVOS'),'yyyymmdd') then
             w_es_nuevo := 'S';
             exit;
          end if;
		end loop;
        end if;
		   --insert into log_batch values('5:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
		for x in pcomex loop
		 --w_sumatotal := nvl(w_sumatotal,0) + x.acr_contingencia;--por el envio de mayo se cambia
		 w_sumatotal := nvl(w_sumatotal,0) + x.acr_capital;
		end loop;
	    if w_es_nuevo = 'N' then
			for x in pcomex loop
	          if x.fecha_emision >= to_date(parametro_calif('PAR_FECHA_NUEVOS'),'yyyymmdd') then
	             w_es_nuevo := 'S';
	             exit;
	          end if;
			end loop;
	    end if;
		   --insert into log_batch values('6:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
        --se agrega lo nuevo de ls super del valor consolidado  de toda la banca
          w_esmayorxventas:='N';
          begin
            select 'S'
             into w_esmayorxventas
             from tcli_juridica,tgen_reprandet
             where jur_clasifpymes between rde_inicio and rde_final
               and rde_modulo = 2
               and rde_codigo = 1
               and jur_codcli = p_cliente
               and parametro_calif('MAYOR_DEUDOR_X_EM')='S'
               and rde_seq in (3,4);

          exception
            when others then
                 w_esmayorxventas:='N';
          end;
             --insert into log_batch values('7:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
          begin
           if parametro_calif('PAR_SOLO_NUEVOS')='S' then
                          --insert into log_batch values('7.1:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
             if w_es_nuevo = 'S' then

                          --insert into log_batch values('7.2:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
           /*
		            select nvl(sum(to_number(MONTO_TOT_APROB,'999999999999999.99')),0)
		              into w_sumaconsolidado
		            from triesgo_clienteSistema
		            where replace(RNC,' ','') = (select cli_identifica
		                                           from tcli_persona
		                                           where cli_codigo = p_cliente)
		               and fechacarga = (select max(fechacarga)
		                                  from triesgo_clienteSistema
		                                  where replace(RNC,' ','') = (select cli_identifica
		                                           from tcli_persona
		                                           where cli_codigo = p_cliente));    */
              w_sumaconsolidado:=0;

                          --insert into log_batch values('7.3:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
             end if;
                                       --insert into log_batch values('7.4:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
            else
                                      --insert into log_batch values('7.5:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
                  w_sumaconsolidado:=0;
		            /*select nvl(sum(to_number(MONTO_TOT_APROB,'999,999,999,999,999.99')),0)
		              into w_sumaconsolidado
		            from triesgo_clienteSistema
		            where replace(RNC,' ','') = (select cli_identifica
		                                           from tcli_persona
		                                           where cli_codigo = p_cliente)
		               and fechacarga = (select max(fechacarga)
		                                  from triesgo_clienteSistema
		                                  where replace(RNC,' ','') = (select cli_identifica
		                                           from tcli_persona
		                                           where cli_codigo = p_cliente));         */
                          --insert into log_batch values('7.6:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito||'^'||w_es_nuevo);
            end if;
   --insert into log_batch values('8:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito);
          if nvl(w_sumaconsolidado,0) > 0 then
             if nvl(w_sumaconsolidado,0) > w_sumatotal then
                w_sumatotal:=w_sumaconsolidado;
             end if;
          end if;
   --insert into log_batch values('9:'||p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas||'^'||p_tipocredito);
          p_montoconsolidado:=w_sumaconsolidado;
          end;
          --insert into log_batch values(p_cliente||'^'||w_montocompara||'^'||w_sumatotal||'^'||p_montoconsolidado||'^'||w_esmayorxventas);
        --fin
		  if w_sumatotal >= w_montocompara then
			return w_tipocredito;
		  elsif w_esmayorxventas = 'S' then
		      return w_tipocredito;
          elsif w_sumatotal < w_montocomparamenor then --Nov 2017
				select codeiso
				into w_tipocredito
				from tgen_desctabla
				where des_codtab = 244
				and des_codigo = 23;--si no es comercial mayor deudor , toma el valor de microcrdito + comerciales menors deudores
				return w_tipocredito;
		  elsif w_sumatotal between w_montocomparamenor and w_montocompara then --Nov 2017
				select codeiso
				into w_tipocredito
				from tgen_desctabla
				where des_codtab = 244
				and des_codigo = w_codnummediano;--si no es comercial mayor deudor , toma el valor de microcrdito + comerciales menors deudores
				return w_tipocredito;		            		      
		  else
				select codeiso
				into w_tipocredito
				from tgen_desctabla
				where des_codtab = 244
				and des_codigo = 23;--si no es comercial mayor deudor , toma el valor de microcrdito + comerciales menors deudores
				return w_tipocredito;
		  end if;
		exception
		when no_data_found then
			select codeiso
	 		  into w_tipocredito
			  from tgen_desctabla
			  where des_codtab = 244
			   and des_codigo = 23;--si no es comercial mayor deudor , toma el valor de microcrdito + comerciales menors deudores
			return w_tipocredito;
		when others then
		return w_tipocredito;
		end;
   else -- p_tipocredito in (21,22,23) then
         dbms_output.put_line('tipocred 3:'||p_tipocredito||'-'||w_tipocredito);
      return w_tipocredito;
   end if;
  exception
   when others then
          raise_application_error (-20505,'Error: ' ||w_tipocredito||' - '|| substr(1,120,sqlerrm));
  END;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- pais del cliente
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  function pais (p_cliente in number, p_numdir in number ) return number is
  v_pais number;
  begin
    select dir_pais
      into v_pais
    from tcli_direccion
    where dir_codcli = p_cliente
      and dir_numero = p_numdir;
    return v_pais;
  exception
    when others then
      return 0;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fecha de renovación
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  FUNCTION FECHA_RENOVA(p_credito in number, p_fecha in Date) RETURN DATE IS
    v_plazo      number := 0;
    p_fecharenov DATE;
  BEGIN
     		select max(trunc(ref_fechaini))
		      into p_fecharenov--v_FECHAREFINANCIACION
		       from TPRE_PRESTAMOSCOND 
		      where p_Fecha between trunc(ref_fechaini) and nvl(ref_fechafin,to_date('2199/12/31','yyyy/mm/dd'))   
		        and ref_credito = p_credito
		        and ref_tipproc = 2;--1 refinanciado 2 renovado
                IF p_fecharenov IS NOT NULL THEN
                    RETURN p_fecharenov;        
                END IF;    
		        
    SELECT ACR_PLAZO
      INTO v_plazo
      FROM TPRE_ACCRUAL
     WHERE ACR_CREDITO = P_CREDITO AND ACR_FECHA = P_FECHA;

    SELECT MAX(ACR_FECHA)
      INTO p_fecharenov
      FROM TPRE_ACCRUAL
     WHERE ACR_CREDITO = P_CREDITO
       AND TRUNC(ACR_FECHA) <= P_FECHA AND
           ACR_PLAZO <> v_plazo;
     
    RETURN p_fecharenov + 1;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- fecha de REFINANCIADO
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  FUNCTION FECHA_REFINANCIADO(p_credito in number, p_fecha in Date) RETURN DATE IS
    v_plazo      number := 0;
    p_fecharenov DATE;
  BEGIN
     		select max(trunc(ref_fechaini))
		      into p_fecharenov--v_FECHAREFINANCIACION
		       from TPRE_PRESTAMOSCOND 
		      where p_Fecha between trunc(ref_fechaini) and nvl(ref_fechafin,to_date('2199/12/31','yyyy/mm/dd'))   
		        and ref_credito = p_credito
		        and ref_tipproc = 1;--1 refinanciado 2 renovado
    
    RETURN p_fecharenov ;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END;
  
-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- UPD fecha de renovación
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  PROCEDURE UPD_FECHA_RENOVA(p_fecha in Date) IS
   cursor p is
    select lca_cuenta
      from tleg_operacioncalif
     where lca_fecha = p_fecha
       and lca_tipo ='P';
       w_numrenov number;
  BEGIN
   for x in p loop
     w_numrenov:=0;
     select count(distinct acr_plazo) - 1
       into w_numrenov
       from tpre_accrual
      where acr_fecha <= p_fecha
        and acr_credito = x.lca_cuenta;

     update tleg_operacioncalif
        set LCA_FECRENOV = FECHA_RENOVA(x.lca_cuenta,p_fecha),
            LCA_FECULTRENOVPLAZO=FECHA_RENOVA(x.lca_cuenta,p_fecha),
            LCA_NRORENOVPLAZO=w_numrenov
     where lca_fecha = p_fecha
       and lca_tipo ='P'
       and lca_cuenta = x.lca_cuenta;
   end loop;
  END;
  procedure upd_saldos_fecha(p_fecha in date,p_tipo in varchar2) is

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Prorrateo de garantias cuando se comparte
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 cursor p is
 select lca_mod pre_mod,lca_cuenta pre_credito,lca_mon pre_moneda,lca_suc pre_sucursal,
       lca_capital acr_capital,lca_codtipocredito,lca_contingente,LCA_MONTOAPROBADOMANUAL,LCA_MONTOAPROBADO
  from tleg_operacioncalif
  where lca_fecha = p_fecha
   and lca_tipo = p_tipo;
   w_montogar_real number:=0;
   w_montogar_admisible number:=0;
   w_montocub number:= 0;
   w_montoexp number:= 0;
   w_interes number:=0;
   w_cancelado VARCHAR2(1) :='N';
   w_diasv number :=0;
   w_diasint number :=0;
   w_demandajundicial varchar2(1):='N';
   w_contingencia number :=0;
   w_montoaprobado number :=0;
   w_limiteactual number:=0;
 begin
  -- MONTO REAL DE GARANTIAS y garantias admisible
    for prestamos in p loop
     w_montogar_real :=0;
     w_montogar_admisible :=0;
     w_montocub:= 0;
     w_montoexp:= 0;
     w_interes:=0;
     w_cancelado :='N';
     w_diasv :=0;
     w_diasint :=0;
     w_demandajundicial :='N';
     w_limiteactual:=0;
     if  prestamos.pre_mod = 6 then
     begin
     select   (DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'-',acr_intacum,0))) + decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) + DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'J',0,acr_intactven)))) * bdi_promedio(pre_moneda,pre_sucursal,p_fecha),
              (nvl(acr_capred,0) + nvl(acr_capnogi,0) + nvl(acr_capvencido,0))*bdi_promedio(prestamos.pre_moneda,prestamos.pre_sucursal,p_fecha),
              nvl(acr_diasv,0), nvl(acr_diasint,0)
       into w_interes,w_montocub,w_diasv,w_diasint
       from tpre_accrual,tpre_prestamos
       where acr_credito = pre_credito
         and acr_credito = prestamos.pre_credito
         and acr_fecha = p_fecha;
     exception
       when no_data_found then
         w_interes := 0;
         w_montocub :=0;
         w_cancelado :='S';
     end;
      begin
      if prestamos.LCA_MONTOAPROBADOMANUAL='N' then
      w_montoaprobado:= pkg_precalifica_do.MONTO_AUTORIZADO_LEG(prestamos.pre_moneda, prestamos.pre_sucursal, prestamos.pre_credito, P_FECHA);
      else
         w_montoaprobado:=  prestamos.LCA_MONTOAPROBADO;
      end if;
      --w_montoaprobado:= nvl(w_montoaprobado,0)*bdi_promedio(prestamos.pre_moneda,prestamos.pre_sucursal,p_fecha);
      exception
         when others then
         w_montoaprobado:=0;
      end;

     w_contingencia:=0;
     elsif prestamos.pre_mod = 4 then
        select sum(crd_valor * bdi_promedio(vis_moneda,vis_suc,p_fecha))
        into w_limiteactual
        from tcap_credvista,tcap_vista
        where crd_numcue =  prestamos.pre_credito
          and crd_numcue = vis_numcue
          and crd_tipocred = 2
		  and crd_fechavig <= P_FECHA
		  and crd_fechavenc >= P_FECHA
		  and nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
		  and nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA;
          w_montoaprobado:=w_limiteactual;
		select nvl(sum((BALTRCY*-1)*bdi_promedio(cy,branch,p_fecha)),0)
		  into w_interes
          from   tgen_saldomodulos
         where   acc = prestamos.pre_credito
          and    dateload = p_fecha
          and    baltype = 'SCON'
          and    accentrytype = 2;
		select nvl(sum((BALTRCY*-1)*bdi_promedio(cy,branch,p_fecha)),0)
		  into w_montocub
          from   tgen_saldomodulos
         where   acc = prestamos.pre_credito
          and    dateload = p_fecha
          and    baltype = 'SCON'
          and    accentrytype = 1;
          w_contingencia := nvl(w_limiteactual,0) - nvl(w_montocub,0);
     end if;
     begin
      select 'S'
        into w_demandajundicial
        from tpre_presleg
       where prl_credito = prestamos.pre_credito
        and p_fecha between trunc(prl_desde) and nvl(trunc(prl_hasta),to_date('2199/12/01','yyyy/mm/dd'));
    exception
      when others then
        w_demandajundicial:='N';
    end;

    IF  w_cancelado =  'S' then
     update tleg_operacioncalif
        set lca_interes = 0,
            lca_contingente = 0
     where lca_fecha = p_fecha
       and lca_tipo = p_tipo
       and lca_mod = prestamos.pre_mod
       and lca_cuenta = prestamos.pre_credito;
    else
     update tleg_operacioncalif
        set lca_interes = w_interes,
            lca_capital = w_montocub,
            lca_demandajundicial = w_demandajundicial,
            lca_contingente = w_contingencia,
            lca_montoaprobado = w_montoaprobado
     where lca_fecha = p_fecha
       and lca_tipo = p_tipo
       and lca_mod = prestamos.pre_mod
       and lca_cuenta = prestamos.pre_credito;
    if prestamos.lca_codtipocredito <> 'C' then
     update tleg_operacioncalif
        set lca_diasmoracap=w_diasv,
            lca_diasmoraint=w_diasint
     where lca_fecha = p_fecha
       and lca_tipo = p_tipo
       and lca_mod = prestamos.pre_mod
       and lca_cuenta = prestamos.pre_credito;
    end if;
    end if;
       commit;
    end loop;
 end;
/***************************************************************************************************/
/*recalcula Tipo Credito																			*/
/***************************************************************************************************/
 procedure recalcula_tipoCredito(p_fecha in date,p_tipo in varchar2) is
 cursor p is
 select --lca_codcli ,sum(nvl(lca_interes,0) + nvl(lca_capital,0)+ nvl(lca_contingente,0)) lca_capital --por nov 2017
 lca_codcli ,sum(nvl(lca_interes,0) + nvl(lca_capital,0)) lca_capital
   from tleg_operacioncalif
  where lca_fecha = p_fecha
    and lca_codtipocredito in ('C','M','D') --Nov 2017
    --and lca_codcli =899
 group by lca_codcli;
 w_montocompara number:=0;
 w_sumaconsolidado number:=0;
 w_tipocredito varchar2(1);
 w_tipocredito_antes varchar2(1);
 w_tipocreditonum number;
 v_todosmenores NUMBER :=0;
--Nov.2017
    w_montocomparamenor number:=0;
    w_codnummediano number:=0;		 
 begin
   begin
     select par_valor
      into w_montocompara
      from tpre_paramcaliftipo
     where par_tipocredito = 20;--tomar siempre de la tabla 244
   exception
       when others then
          w_montocompara:=0;
    end;                            
    --Nov 2017
      w_montocomparamenor:=  parametro_calif('MONTO_MENOR_DEUDOR');
      w_codnummediano:=  parametro_calif('CODNUM_MEDIANOS_DEUDORES');
    
    dbms_output.put_line('w_montocompara:'||w_montocompara);
   for x in p loop
      -- por mesa de ayuda 1721- para conocer
			  select nvl(sum(to_number(MONTO_TOT_APROB)),0)
				into w_sumaconsolidado
			    from triesgo_clienteSistema
			   where 
				 fechacarga = (select max(fechacarga)
				                     from triesgo_clienteSistema
				                    where fechacarga between add_months(p_fecha,-1) and trunc(sysdate)
				                    and replace(RNC,' ','') = (select cli_identifica
				                                                   from tcli_persona
				                                                  where cli_codigo = x.lca_codcli))
			and replace(RNC,' ','') = (select cli_identifica
				                              from tcli_persona
				                             where cli_codigo = x.lca_codcli);
               dbms_output.put_line('w_sumaconsolidado:'||w_sumaconsolidado);
          update tleg_operacioncalif
            set lca_consolidasistema=w_sumaconsolidado
          where lca_fecha = p_fecha
            and lca_codcli = x.lca_codcli
            and lca_codtipocredito in ('C','M','D');--Nov 2017
            commit;
   end loop;
   for x in p loop
    --primero busco localmente
      dbms_output.put_line('x.lca_capital:'||x.lca_capital);
  	  if nvl(x.lca_capital,0)  >  w_montocompara then
	  	 w_tipocredito:='C';
	  	 w_tipocreditonum:=20;
	  elsif  nvl(x.lca_capital,0)  <  w_montocomparamenor then --nov 2017
	     w_tipocredito:='M';
	     w_tipocreditonum:=23;	   
	  elsif  nvl(x.lca_capital,0)  between w_montocomparamenor and w_montocompara then --nov 2017
	     w_tipocredito:='D';
	     w_tipocreditonum:=w_codnummediano;	   --NOV 2017
	  else
	     w_tipocredito:='M';
	     w_tipocreditonum:=23;
	  end if;
      dbms_output.put_line('w_tipocreditonum:'||w_tipocreditonum);
      --if w_tipocredito <> x.lca_codtipocredito then
          update tleg_operacioncalif
            set lca_codtipocredito = w_tipocredito,
                LCA_TIPOCREDITO = w_tipocreditonum
          where lca_fecha = p_fecha
            and lca_codcli = x.lca_codcli
            and lca_codtipocredito in ('C','M','D');--NOV 2017
            --and lca_cuenta = x.lca_cuenta;
       --del consolidado    
          select sum(nvl(lca_consolidasistema,0))
             into w_sumaconsolidado
          from tleg_operacioncalif
          where lca_fecha = p_fecha
            and lca_codcli = x.lca_codcli
            and rownum = 1
            and lca_codtipocredito in ('C','M','D');--Nov 2017

  	  if nvl(w_sumaconsolidado,0)  >  w_montocompara then
	  	  w_tipocredito:='C';
	  	  w_tipocreditonum:=20;
	  elsif  nvl(w_sumaconsolidado,0)  <  w_montocomparamenor then --nov 2017
	      w_tipocredito:='M';
	      w_tipocreditonum:=23;	   
	  elsif  nvl(w_sumaconsolidado,0)  between w_montocomparamenor and w_montocompara then --nov 2017
	      w_tipocredito:='D';
	      w_tipocreditonum:=w_codnummediano;	   --NOV 2017
	  else
	      w_tipocredito:='M';
	      w_tipocreditonum:=23;
	  end if;
                    
       --fin del consolidado            
          update tleg_operacioncalif
            set lca_codtipocredito = w_tipocredito,
                LCA_TIPOCREDITO = w_tipocreditonum
          where lca_fecha = p_fecha
            and lca_codcli = x.lca_codcli
            and lca_codtipocredito in ('C','M','D');--NOV 2017

      --end if;
        begin
          select lca_codtipocredito
                    into w_tipocredito_antes
          from tleg_operacioncalif a
         where lca_fecha < p_fecha
            and lca_fecha = last_day(lca_fecha)
            and lca_codcli = x.lca_codcli
            and lca_codtipocredito in ('C','M','D')--NOV 2017
            and lca_fecha = (select max(lca_fecha)
                               from tleg_operacioncalif b
                               where lca_fecha < p_fecha
                                 and lca_fecha = last_day(lca_fecha)
                                 and lca_codcli = x.lca_codcli
            and lca_codtipocredito in ('C','M','D'));--NOV 2017
         exception
            when others then
               w_tipocredito_antes:='M';
         end;
      if w_tipocredito  IN ('M','D') then   --NOV 2017
          --se comenta por mesa de ayuda 1721 Rosemary- para saber si debe o no bajar a menor deudior despues de 6 meses consecutivos
				select count(*)
				  into v_todosmenores
				from (select lca_fecha,lca_consolidasistema, lca_codcli
				from (select distinct lca_fecha,lca_consolidasistema, lca_codcli
				        from tleg_operacioncalif
				         where lca_codcli = x.lca_codcli
				           and lca_fecha = last_day(lca_fecha)
                            and lca_fecha >= to_date('2014/03/01','yyyy/mm/dd')--fecha en que inicio esta ley , correo Rosemary 2015/05/26
				         order by lca_fecha desc)
				 where lca_codcli = x.lca_codcli
				   and rownum <=NVL(parametro_calif('PAR_PERIODOS_EN_MENOR'),6)) --6 meses atras quisiera poner un parametro pero estamos contra el tiempo
				 where lca_codcli = x.lca_codcli
				   and nvl(lca_consolidasistema,0) < w_montocompara;
      dbms_output.put_line('v_todosmenores:'||v_todosmenores);

			  --if w_sumaconsolidado >  w_montocompara then
			  --IF v_todosmenores > 0 then
      dbms_output.put_line('v_todosmenores:'||v_todosmenores);
      dbms_output.put_line('NVL(parametro_calif:'||NVL(parametro_calif('PAR_PERIODOS_EN_MENOR'),6));
			  IF v_todosmenores < NVL(parametro_calif('PAR_PERIODOS_EN_MENOR'),6) and w_tipocredito_antes = 'C' then
			  	 w_tipocredito:='C';
			  	 w_tipocreditonum:=20;
			  --else nOV 2017
			    -- w_tipocredito:='M';
			     --w_tipocreditonum:=23;
			  end if;
      dbms_output.put_line('w_tipocreditonum:'||w_tipocreditonum);
		      --if w_tipocredito <> x.lca_codtipocredito then
		          update tleg_operacioncalif
		            set lca_codtipocredito = w_tipocredito,
		                LCA_TIPOCREDITO = w_tipocreditonum
		          where lca_fecha = p_fecha
		            and lca_codcli = x.lca_codcli
		            and lca_codtipocredito in ('C','M','D');  --nOV 2017
		            --and lca_cuenta = x.lca_cuenta;
		      --end if;
	  end if;
	  commit;
   end loop;
 end;
 procedure upd_prorrateo_gar(p_fecha in date,p_tipo in varchar2) is
 cursor p is
 select lca_mod pre_mod,lca_cuenta pre_credito,lca_mon pre_moneda,lca_suc pre_sucursal,
        lca_capital acr_capital,lca_interes acr_interes
  from tleg_operacioncalif
  where lca_fecha = p_fecha
   and lca_tipo = p_tipo;
   w_montogar_real number:=0;
   w_montogar_admisible number:=0;
   w_montogar_admisiblefisa number:=0;
   w_montocub number:= 0;
   w_montoexp number:= 0;
   w_montocub_int number:= 0;
   w_montoexp_int number:= 0;
   w_interes number:=0;
   w_tipogarantia varchar2(2);
   w_montogar_admisible_dif number:=0;
   w_valorgarantizadofisa number:=0;
   w_valorgarantizadoreal number:=0;
   w_valor varchar2(30);
   w_valoradistribuir number;

 begin
  -- MONTO REAL DE GARANTIAS y garantias admisible
    for prestamos in p loop
     w_montogar_real :=0;
     w_montogar_admisible :=0;
     w_montogar_admisiblefisa :=0;
     w_montocub:= 0;
     w_montoexp:= 0;
     w_montogar_admisible_dif :=0;
     w_valorgarantizadofisa :=0;
     w_valorgarantizadoreal :=0;
     w_valoradistribuir :=0;
     --PARA CONOCER QUE VALOR DISTRIBUYO, EL ADMISIBLE CALCULADO POR FISA  CON TODAS LAS VALIDACIONES
     --O SOLO EL VALOR ADMISIBLE QUE MUESTRA EN LA 28/1
     w_valor := parametro_calif('VALOR_DISTRIBUCION_GARANTIA');

     begin
     select nvl(sum(log_valoradmifisa),0),nvl(sum(log_valoradmireal),0),nvl(sum(log_valorgarantizadofisa),0),nvl(sum(log_valorgarantizadoreal),0)
       into w_montogar_admisiblefisa,w_montogar_admisible,w_valorgarantizadofisa,w_valorgarantizadoreal
       from tleg_opergaran
      where log_fecha = p_fecha
        and log_tipo = p_tipo
        and log_mod = prestamos.pre_mod
        and log_operacion = prestamos.pre_credito;
     exception
       when others then
         w_montogar_admisiblefisa:=0;
         w_montogar_admisible:=0;
     end;
     --1 distribuyo lo de calculado por Fisa validando
     --por defaul va lo real
     IF nvl(w_valor,0) = '1' then
        w_valoradistribuir:=w_valorgarantizadofisa;
     else
        w_valoradistribuir:=w_valorgarantizadoreal;
     end if ;
     dbms_output.put_line('w_valorgarantizadofisa:'||w_valorgarantizadofisa||' '||prestamos.pre_credito);
     dbms_output.put_line('w_valorgarantizadoreal:'||w_valorgarantizadoreal||' '||prestamos.pre_credito);
      --Para capital
      IF nvl(w_valoradistribuir,0) >= nvl(prestamos.acr_capital,0) then -- + nvl(prestamos.acr_interes,0) then
         w_montocub:= nvl(prestamos.acr_capital,0); -- + nvl(prestamos.acr_interes,0);
         w_montoexp:= 0;
         w_montogar_admisible_dif := nvl(w_valoradistribuir,0) -  nvl(prestamos.acr_capital,0);
      else
         w_montocub:= nvl(w_valoradistribuir,0);
         w_montoexp:= (nvl(prestamos.acr_capital,0)) - nvl(w_valoradistribuir,0);
         w_montogar_admisible_dif := 0;
      end if;
      --para interes
      IF nvl(w_montogar_admisible_dif,0) >= nvl(prestamos.acr_interes,0) then
         w_montocub_int:= nvl(prestamos.acr_interes,0);
         w_montoexp_int:= 0;
      else
         w_montocub_int:= nvl(w_montogar_admisible_dif,0);
         w_montoexp_int:= nvl(prestamos.acr_interes,0) - nvl(w_montogar_admisible_dif,0);
      end if;
   --para saber si cubre o no estoy tomando el adminsible real.
        dbms_output.put_line('w_valorgarantizadofisa:'||w_valorgarantizadofisa);
     dbms_output.put_line('w_valorgarantizadoreal:'||w_valorgarantizadoreal);

        update tleg_operacioncalif
        set lca_montogaradmi = w_montogar_admisible,
            lca_montogaradmifisa = w_montogar_admisiblefisa,
            lca_capitalcub=w_montocub,
            lca_capitalexp=w_montoexp,
            lca_interescub=w_montocub_int,
            lca_interesexp=w_montoexp_int
     where lca_fecha = p_fecha
       and lca_tipo = p_tipo
       and lca_mod = prestamos.pre_mod
       and lca_cuenta = prestamos.pre_credito;
       commit;
    end loop;
 end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Tipo de garantia
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   function tipo_garantia (p_garantia in number ) return varchar2 is
    w_codeiso varchar2(2);
   begin
   select substr(codeiso,1,2)
     into w_codeiso
     from tgen_desctabla
    where des_codtab = 33
      and des_codigo = p_garantia;
      return w_codeiso;
   exception
     when no_data_found then
       w_codeiso:=null;
       return w_codeiso;
   end;
   --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   --CALUCLO DE LA ADMISIBILIDAD DE ACUERDO A LOS FACTORES QUE INCLUYE EL REA
   --XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure admisibilidad_garantias ( p_fecha date, p_tipo varchar2, p_mes in number ) is
    w_diasmora_cap tpre_operacioncalif.oca_diasmora%type;
    w_diasmora_int tpre_operacioncalif.oca_diasmora%type;
    w_diasmora_rees tpre_operacioncalif.oca_diasmora%type;

    w_reprogramaciones tpre_operacioncalif.oca_reprograma%type;
    w_cuotaspag tpre_operacioncalif.oca_cuotaspag%type;	--JAV hd2875
    w_autoliquidable tpre_operacioncalif.oca_autoliquidable%type;
    w_gar tpre_operacioncalif.oca_garhip%type;
    w_montogar_admisible tpre_operacioncalif.oca_montogar%type;
    w_montogar_admisiblefisa tpre_operacioncalif.oca_montogar%type;
    w_montogar_real tpre_operacioncalif.oca_montogarreal%type; --JAV hd2875
    --w_monto tpre_operacioncalif.oca_capdir%type;
    w_diasbase tpre_prestamos.pre_diasbase%type;
    w_basemes tpre_prestamos.pre_basemes%type;  w_fecarr date;
    w_diasreprograma tpre_operacioncalif.oca_prediasreprograma%type;
    w_demandajundicial varchar2(1):= 'N';
    cuantos number(4);
    w_reestructurado varchar2(1);
    w_reestructuradoactual varchar2(1);
    w_fecreestruc date;
    w_codtipocredito VARCHAR2(8);
    w_codigopais number;
    w_fecrenov date;
    w_interes number;
    w_capital number;
    w_riesgopais varchar2(1):='A';
    w_pais number;
    w_montocub number;
    w_montoexp number;
    w_desobregiro varchar2(1):='N';
    w_diasv number;
    w_diasint number;
    v_nromeses number;
    w_esnuevo varchar2(1):='N';
    --w_montogar_admisible number;
	w_montopignorado                       number(18,2);
	w_nrorestruct                          number(18,2);
	w_montorestruc                         number(18,2);
	w_fecultrenovtasa                      date;
	w_nrorenovtasa                         number;
	w_fecultrenovplazo                     date;
	w_nrorenovplazo                        number;
	w_ejecutivo                            varchar2(30);
	w_grupoeconomico                       varchar2(100);
	w_fecultcal                            date;
	w_destinoeconomico                     varchar2(100);
	w_destinofondos                        varchar2(100);
	w_fechainsgar                          date;
	w_fechavenpolizas                      date;
	w_fechainglegal                        date;
	w_fechasalilegal                       date;
	w_numcomprom                           number(10);
	w_ciiu                                 varchar2(6);
	w_tipoamortizacion                     varchar2(100);
	w_existecalif number:=0;
	w_existeprev number:=0;
	w_fechasalreest date;
	w_reescuotadesd number;
    w_montogar_admisible_dif number:=0;
    w_montocub_int number:= 0;
    w_montoexp_int number:= 0;
    w_tipogarantia varchar2(2);
    w_diasvencontrato number;
    w_tipocredito number;
    w_riesgo number;
    w_monto number;
     cursor gar is
       select log_cliente,log_numgaran,log_valoradmifisa,log_valoradmireal
         from tleg_opergaran
        where  log_fecha = p_fecha
          and log_tipo = p_tipo
          --and log_operacion = 6010004816
          group by log_cliente,log_numgaran,log_valoradmifisa,log_valoradmireal
          order by 1,2;
     cursor dis_gar(cli in number, numgar in number) is
       select pre_fecontab,log_cliente,log_numgaran,log_mod,log_operacion,log_tipo,log_fecha,
              log_valoradmifisa,log_valoradmireal,nvl(lca_capital,0) + nvl(lca_interes,0) Riesgo,
              pre_numcomprom
         from tleg_opergaran,tpre_prestamos,tleg_operacioncalif
        where log_fecha = lca_fecha
          and log_tipo = lca_tipo
          and log_mod = lca_mod
          and log_operacion = lca_cuenta
          and log_mod = pre_mod
          and log_operacion = pre_credito
          and log_fecha = p_fecha
          and log_tipo = p_tipo
          and log_cliente = cli
          and log_numgaran = numgar
      union
       select VIS_FECHAPE pre_fecontab,log_cliente,log_numgaran,log_mod,log_operacion,log_tipo,log_fecha,
              log_valoradmifisa,log_valoradmireal,nvl(lca_capital,0) + nvl(lca_interes,0) Riesgo,
              0 pre_numcomprom
         from tleg_opergaran,tcap_vista tpre_prestamos,tleg_operacioncalif
        where log_fecha = lca_fecha
          and log_tipo = lca_tipo
          and log_mod = lca_mod
          and log_operacion = lca_cuenta
          and log_mod = vis_mod
          and log_operacion = vis_numcue --pre_credito
          and log_fecha = p_fecha
          and log_tipo = p_tipo
          and log_cliente = cli
          and log_numgaran = numgar
     Order by pre_fecontab;
        w_valorgarantizadofisa number:= 0;
        w_valorgarantizadoreal number:=0;
        w_saldoadmisiblefisa number:=0;
        w_saldoadmisiblereal number:=0;
        w_valoryagarantizadofisa number:= 0;
        w_valoryagarantizadoreal number:=0;
        w_saldoadmisiblefisaaux number:=0;
        w_saldoadmisiblerealaux number:=0;

  begin
      begin
    INSERT INTO TPRE_PARAMCALIF
      (PCA_FECHA, PCA_TIPO, PCA_STATUS, PCA_PREVSTATUS)
    VALUES
      (p_fecha, p_tipo, 'P', 'P');
    exception
    when dup_val_on_index then
      null;
    end;
    if p_mes = 3 then
    delete tleg_opergaran
    where log_fecha = p_fecha
      and log_tipo = p_tipo;
    else
    delete tleg_opergaran
    where log_fecha = p_fecha
      and log_tipo = p_tipo
      and log_operacion not in (select lca_cuenta
                                    from tleg_operacioncalif
                                   where lca_fecha = p_fecha
                                     and lca_codtipocredito in ('C','M','D'));--NOV 2017
    end if;
     commit;
    for prestamos in ( select pre_clientep, pre_mod, pre_credito, pre_pro, pre_tip, pre_monto, pre_fecven,
                              pre_tabtipocredito, pre_tipocredito, pre_moneda, pre_sucursal, pre_oficina,
                              decode(nvl(acr_otrostatus,'-'),'-',acr_status,acr_otrostatus) pre_status,
                              sta_descripcion, pre_fecemi, pre_tipotasa, pre_diasbase, pre_basemes, des_codigociiu,
                              acr_diasint, acr_diasv, acr_otrostatus,ptc_tipocredito,
                              (nvl(acr_capred,0) + nvl(acr_capnogi,0) + nvl(acr_capvencido,0))*bdi_promedio(pre_moneda,pre_sucursal,p_fecha) acr_capital,
							  --nvl(acr_intacum,0) + nvl(acr_intactven,0) * bdi_promedio(pre_moneda,pre_sucursal,p_fecha) acr_interes,
                              (DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'-',acr_intacum,0))) + decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) + DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'J',0,acr_intactven)))) * bdi_promedio(pre_moneda,pre_sucursal,p_fecha) acr_interes,
							  pre_numdir,pre_modvencorg,pre_ctavencorg,pre_tabtipocuo,pre_tipocuo,pre_destecon,pre_numcomprom,
							  pre_codeje,pre_garantia
                       from tpre_prestamos,tpre_accrual ,tgen_status, tpre_destecon,tpre_protipocredito
                       where pre_credito = acr_credito
                       and acr_fecha = p_fecha
				       and ptc_mod = pre_mod
                       and ptc_pro = pre_pro
                       and ptc_tip = pre_tip
                       and ptc_mon = pre_moneda
                       and pre_mod = sta_mod
                       and pre_status = sta_codigo
                       and pre_destecon = des_codigo
                       --and pre_credito=6010004816
                       and pre_credito not in (select log_Operacion
                                                from tleg_opergaran
                                                where  log_fecha = p_fecha
                                                union
                                                select 1
                                                from dual)
                       union
                       select vis_codcli pre_clientep, vis_mod pre_mod, vis_numcue pre_credito,
                              vis_pro pre_pro,vis_tip pre_tip, crd_valor pre_monto,crd_fechavenc pre_fecven,
                              ptc_tabtipocredito pre_tabtipocredito,ptc_tipocredito pre_tipocredito,
                              vis_moneda pre_moneda,vis_suc pre_sucursal,vis_ofi pre_oficina,
                              vis_status pre_status,
                              sta_descripcion,crd_fechavig pre_fecemi, 13 pre_tipotasa, 361 pre_diasbase, 360 pre_basemes,
                              crd_ciiu des_codigociiu,
                              0 acr_diasint, 0 acr_diasv, vis_status acr_otrostatus,ptc_tipocredito,
                              nvl(aca_utilizado,0)*bdi_promedio(vis_moneda,vis_suc,p_fecha) acr_capital,
                              nvl(aca_interes,0)* bdi_promedio(vis_moneda,vis_suc,p_fecha) acr_interes,
							  vis_numdircor pre_numdir, 0 pre_modvencorg, vis_numcue pre_ctavencorg,68 pre_tabtipocuo,
							  8 pre_tipocuo,1 pre_destecon,crd_compromaso pre_numcomprom,
							  vis_codeje pre_codeje,crd_tipogar pre_garantia
                       from tcap_vista tpre_prestamos,tcap_acract tpre_accrual ,tcap_credvista ,tgen_status, tpre_protipocredito
                       where vis_numcue = aca_numcue
                       and aca_fecha = p_fecha
                       and aca_numcue = crd_numcue
                       and aca_tipocre = crd_tipocred
                       and aca_secuencia = crd_secuencia
                       and aca_tipocre = 2
				       and ptc_mod = vis_mod
                       and ptc_pro = vis_pro
                       and ptc_tip = vis_tip
                       and ptc_mon = vis_moneda
                       and vis_mod = sta_mod
                       and vis_status = sta_codigo
                       and vis_numcue not in (select log_Operacion
                                                from tleg_opergaran
                                                where  log_fecha = p_fecha
                                                union
                                                select 1
                                                from dual))	--ajuste 2008/09/04


    loop
    --solo las garantias de moneda extranjera debo tomar a la nueva tasa de la fexcha de corte
    --si existió un cambio
      w_fecarr := null;
      w_reestructurado := 'N';
      w_reestructuradoactual := 'N';
      w_fecreestruc := null;
      w_diasmora_rees := 0;
      w_gar :='N';
      w_montogar_real:=0;
      w_codtipocredito := NULL;
      w_montocub :=0;
      w_montoexp :=0;
      w_montogar_admisible_dif:=0;
      w_montocub_int:= 0;
      w_montoexp_int:= 0;

      w_desobregiro :='N';
      w_fecrenov:=FECHA_RENOVA(prestamos.pre_credito, p_fecha);
      w_montogar_admisible:=0;
      w_montogar_admisiblefisa :=0;
      w_esnuevo :='N';
	  w_fechasalreest :=null;
	  w_reescuotadesd := 0;
	  w_tipogarantia:=null;
      w_diasvencontrato:=0;
      w_tipocredito := 0;
	  -- MONTO REAL DE GARANTIAS y garantias admisible
      pkg_precalifica_do.distribucion_admisible_real(prestamos.pre_mod, prestamos.pre_credito, 0,--moneda local
                                                   	 prestamos.pre_sucursal, p_fecha, w_montogar_admisible,
                                                   	 w_montogar_admisiblefisa,w_tipogarantia);

      if w_tipogarantia is null then
      w_tipogarantia:= tipo_garantia(prestamos.pre_garantia);
      end if;
      end loop;
      for x in gar loop
        w_valorgarantizadofisa:= 0;
        w_valorgarantizadoreal:=0;
        w_saldoadmisiblefisa:=0;
        w_saldoadmisiblereal:=0;
        w_saldoadmisiblefisaaux:=0;
        w_saldoadmisiblerealaux:=0;
        w_riesgo :=0;
        w_saldoadmisiblefisa:= x.log_valoradmifisa;
        w_saldoadmisiblereal:= x.log_valoradmireal;
        w_valoryagarantizadofisa := 0;
        w_valoryagarantizadoreal :=0;
        for y in dis_gar(x.log_cliente, x.log_numgaran) loop
                w_riesgo :=y.riesgo;
                w_monto:=0;
		        w_valoryagarantizadofisa := 0;
		        w_valoryagarantizadoreal :=0;
		        w_saldoadmisiblefisaaux:=0;
		        w_saldoadmisiblerealaux:=0;
		        --BDI no ha dicho nada de esto.
                if nvl(y.pre_numcomprom,0) <> 0 then
                   select cmp_monto*bdi_promedio(cmp_moneda,cmp_sucursal,p_fecha)
                     into w_monto
                    from tpre_comprom
                    where cmp_numero = y.pre_numcomprom;
                end if;
                if nvl(w_riesgo,0)< nvl(w_monto,0) then
                   w_riesgo := w_monto;
                end if;
          --encuentro valores ya garantizados para el prestamo
               begin
                 select sum(nvl(log_valorgarantizadofisa,0)),sum(nvl(log_valorgarantizadoreal,0))
                   into w_valoryagarantizadofisa,w_valoryagarantizadoreal
                   from tleg_opergaran
                  where log_fecha = y.log_fecha
                    and log_tipo = y.log_tipo
                    and log_mod = y.log_mod
                    and log_operacion = y.log_operacion
                    and (log_cliente,log_numgaran) not in (select x.log_cliente,x.log_numgaran from dual);
               end;
          --lo calculado por Fisa
            if nvl(w_saldoadmisiblefisa,0) > nvl(w_riesgo,0) then
               w_valorgarantizadofisa:= nvl(w_riesgo,0);
               w_saldoadmisiblefisa:= nvl(w_saldoadmisiblefisa,0) - nvl(w_riesgo,0);
            else
               w_valorgarantizadofisa:= nvl(w_saldoadmisiblefisa,0);
               w_saldoadmisiblefisa:=0;
            end if;
            if w_valorgarantizadofisa <=0 then
              w_valorgarantizadofisa:=0;
            end if;
            --resto los valores ya garantizados
              if nvl(w_valorgarantizadofisa,0)+nvl(w_valoryagarantizadofisa,0)>w_riesgo then
                 w_saldoadmisiblefisaaux:= (nvl(w_valorgarantizadofisa,0)+nvl(w_valoryagarantizadofisa,0))-w_riesgo;
                  w_valorgarantizadofisa := w_valorgarantizadofisa-w_saldoadmisiblefisaaux;
              end if;
              w_saldoadmisiblefisa:=nvl(w_saldoadmisiblefisa,0) + nvl(w_saldoadmisiblefisaaux,0);
            --lo rreal sin valida
            if nvl(w_saldoadmisiblereal,0) > nvl(w_riesgo,0) then
               w_valorgarantizadoreal:= nvl(w_riesgo,0);
               w_saldoadmisiblereal:= nvl(w_saldoadmisiblereal,0) - nvl(w_riesgo,0);
            else
               w_valorgarantizadoreal:= nvl(w_saldoadmisiblereal,0);
               w_saldoadmisiblereal:=0;
            end if;
            if w_valorgarantizadoreal <=0 then
              w_valorgarantizadoreal:=0;
            end if;
              if nvl(w_valorgarantizadoreal,0)+nvl(w_valoryagarantizadoreal,0)>w_riesgo then
                 w_saldoadmisiblerealaux := (nvl(w_valorgarantizadoreal,0)+nvl(w_valoryagarantizadoreal,0))-w_riesgo;
                  w_valorgarantizadoreal := w_valorgarantizadoreal-w_saldoadmisiblerealaux;
              end if;
              w_saldoadmisiblereal:=nvl(w_saldoadmisiblereal,0) + nvl(w_saldoadmisiblerealaux,0);
            update tleg_opergaran
              set log_valorgarantizadofisa = w_valorgarantizadofisa,
              log_valorgarantizadoreal=w_valorgarantizadoreal,
              log_saldoadmisiblefisa=w_saldoadmisiblefisa,
              log_saldoadmisiblereal= w_saldoadmisiblereal
             where log_fecha = y.log_fecha
               and log_tipo = y.log_tipo
               and log_mod =y.log_mod
               and log_operacion = y.log_operacion
               and log_cliente = y.log_cliente
               and log_numgaran = y.log_numgaran;
        end loop;
      end loop;
 END;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Carga de datos del módulo de colocaciones
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  procedure datos_ini_mod6 ( p_operador number, p_fecha date, p_tipo varchar2 ) is
    w_diasmora_cap tpre_operacioncalif.oca_diasmora%type;
    w_diasmora_int tpre_operacioncalif.oca_diasmora%type;
    w_diasmora_rees tpre_operacioncalif.oca_diasmora%type;

    w_reprogramaciones tpre_operacioncalif.oca_reprograma%type;
    w_cuotaspag tpre_operacioncalif.oca_cuotaspag%type;	--JAV hd2875
    w_autoliquidable tpre_operacioncalif.oca_autoliquidable%type;
    w_gar tpre_operacioncalif.oca_garhip%type;
    w_montogar_admisible tpre_operacioncalif.oca_montogar%type;
    w_montogar_admisiblefisa tpre_operacioncalif.oca_montogar%type;
    w_montogar_real tpre_operacioncalif.oca_montogarreal%type; --JAV hd2875
    w_monto tpre_operacioncalif.oca_capdir%type;
    w_diasbase tpre_prestamos.pre_diasbase%type;
    w_basemes tpre_prestamos.pre_basemes%type;  w_fecarr date;
    w_diasreprograma tpre_operacioncalif.oca_prediasreprograma%type;
    w_demandajundicial varchar2(1):= 'N';
    cuantos number(4);
    w_reestructurado varchar2(1);
    w_reestructuradoactual varchar2(1);
    w_fecreestruc date;
    w_codtipocredito VARCHAR2(8);
    w_codtipocredito_antes VARCHAR2(8);
    w_codigopais number;
    w_fecrenov date;
	w_fecrefin date;    
    w_interes number;
    w_capital number;
    w_riesgopais varchar2(1):='A';
    w_pais number;
    w_montocub number;
    w_montoexp number;
    w_desobregiro varchar2(1):='N';
    w_diasv number;
    w_diasint number; 
    w_diasvcor number;
    w_diasintcor number;    
    v_nromeses number;
    w_esnuevo varchar2(1):='N';
    --w_montogar_admisible number;
	w_montopignorado                       number(18,2);
	w_nrorestruct                          number(18,2);
	w_montorestruc                         number(18,2);
	w_fecultrenovtasa                      date;
	w_nrorenovtasa                         number;
	w_fecultrenovplazo                     date;
	w_nrorenovplazo                        number;
	w_ejecutivo                            varchar2(30);
	w_grupoeconomico                       varchar2(100);
	w_fecultcal                            date;
	w_destinoeconomico                     varchar2(100);
	w_destinofondos                        varchar2(100);
	w_fechainsgar                          date;
	w_fechavenpolizas                      date;
	w_fechainglegal                        date;
	w_fechasalilegal                       date;
	w_numcomprom                           number(10);
	w_ciiu                                 varchar2(6);
	w_tipoamortizacion                     varchar2(100);
	w_existecalif number:=0;
	w_existeprev number:=0;
	w_fechasalreest date;
	w_reescuotadesd number;
    w_montogar_admisible_dif number:=0;
    w_montocub_int number:= 0;
    w_montoexp_int number:= 0;
    w_tipogarantia varchar2(2);
    w_diasvencontrato number;
    w_tipocredito number;
    w_ultimotrimestre date;--ajuste 2008/09/04
    w_saldoconsolidadosistema number:=0;
    w_lca_montoaprobado number:=0;
    w_lca_aprobadomanual varchar2(1):='N';
    --
	w_MONTOCUOTAEXTRAOR                    NUMBER;
	w_TIPOTASA                             VARCHAR2(1):='F';
	w_BALPROMDCAPMES                       NUMBER;
	w_INTDEVENCORTE                        NUMBER;
	w_COMCARGODEVCORTE                     NUMBER;
	w_ESTRUCTURACIONCRE                    VARCHAR2(3);
	w_ORIGENCREDITO                        VARCHAR2(3);
	w_TIPORECURSO                          VARCHAR2(3);
    w_fcuoextra          date;
    w_represtamo number;  

    w_codnummediano NUMBER; 
     w_codmediano VARCHAR2(1); 
  begin
      begin
    INSERT INTO TPRE_PARAMCALIF
      (PCA_FECHA, PCA_TIPO, PCA_STATUS, PCA_PREVSTATUS)
    VALUES
      (p_fecha, p_tipo, 'P', 'P');
    exception
    when dup_val_on_index then
      null;
    end;
      v_nromeses :=0;
      begin
      select nvl(pda_nromeses,0)
        into v_nromeses
       from tpre_prevdefantparam
      where pda_anio = to_char(p_fecha,'yyyy')
        and pda_mes = to_char(p_fecha,'mm');
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
      if nvl(v_nromeses,0) = 3 then--periodo trismestral , deberiamos parametrizar el 3
		    select count(*)
		     into w_existecalif
		      from tpre_califdettipos
		     where cdt_fecha = p_fecha
		       and cdt_tipo = p_tipo;
		    if nvl(w_existecalif,0) > 0 then
		          raise_application_error (-20505,'No Puede regenerar datos iniciales ya existe calificación realizada para el periodo: ');
		    end if;
		    select count(*)
		     into w_existeprev
		      from tpre_prevdettipos
		     where pdt_fecha = p_fecha
		       and pdt_tipo = p_tipo;
		    if nvl(w_existeprev,0) > 0 then
		          raise_application_error (-20505,'No Puede regenerar la calificacion  existe valores de Provisión para el periodo:');
		    end if;
	    delete tleg_operacioncalif
	    where lca_fecha = p_fecha
	      and lca_tipo = p_tipo;
      else
    select count(*)
     into w_existecalif
      from tpre_califdettipos
     where cdt_fecha = p_fecha
       and cdt_tipo = p_tipo
       and cdt_cuenta in (select lca_cuenta from tleg_operacioncalif where lca_fecha = p_fecha and lca_tipo = p_tipo and lca_esnuevo = 'S');
    if nvl(w_existecalif,0) > 0 then
          raise_application_error (-20505,'No Puede regenerar datos iniciales ya existe calificación realizada para el periodo: ');
    end if;
    select count(*)
     into w_existeprev
      from tpre_prevdettipos
     where pdt_fecha = p_fecha
       and pdt_tipo = p_tipo
       and pdt_cuenta in (select lca_cuenta from tleg_operacioncalif where lca_fecha = p_fecha and lca_tipo = p_tipo and lca_esnuevo = 'S');
    if nvl(w_existeprev,0) > 0 then
          raise_application_error (-20505,'No Puede regenerar la calificacion  existe valores de Provisión para el periodo:');
    end if;
        delete tleg_operacioncalif
         where lca_fecha = p_fecha
          and lca_tipo = p_tipo
          and lca_esnuevo='S';
      end if;
     commit;
     --nov 2017
     w_codnummediano:=  parametro_calif('CODNUM_MEDIANOS_DEUDORES'); 
     w_codmediano :=  parametro_calif('COD_MEDIANOS_DEUDORES'); 

    for prestamos in ( select pre_clientep, pre_mod, pre_credito, pre_pro, pre_tip, pre_monto, pre_fecven,
                              pre_tabtipocredito, pre_tipocredito, pre_moneda, pre_sucursal, pre_oficina,
                              decode(nvl(acr_otrostatus,'-'),'-',acr_status,acr_otrostatus) pre_status,
                              sta_descripcion, pre_fecemi, pre_tipotasa, pre_diasbase, pre_basemes, des_codigociiu,
                              acr_diasint, acr_diasv, acr_otrostatus,ptc_tipocredito,
                              (nvl(acr_capred,0) + nvl(acr_capnogi,0) + nvl(acr_capvencido,0))*bdi_promedio(pre_moneda,pre_sucursal,p_fecha) acr_capital,
							  --nvl(acr_intacum,0) + nvl(acr_intactven,0) * bdi_promedio(pre_moneda,pre_sucursal,p_fecha) acr_interes,
                              (DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'-',acr_intacum,0))) + decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) + DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'J',0,acr_intactven)))) * bdi_promedio(pre_moneda,pre_sucursal,p_fecha) acr_interes,
							  pre_numdir,pre_modvencorg,pre_ctavencorg,pre_tabtipocuo,pre_tipocuo,pre_destecon,pre_numcomprom,
							  pre_codeje,pre_garantia,
							  PTC_TABORGCREDITO,
                              PTC_ORGCREDITO,
                              nvl(PTC_TABORIGENFONDOS,pre_taborigfondos) pre_taborigfondos,
                              nvl(PTC_ORIGENFONDOS,pre_origfondos) pre_origfondos
                       from tpre_prestamos,tpre_accrual ,tgen_status, tpre_destecon,tpre_protipocredito
                       where pre_credito = acr_credito
                       and acr_fecha = p_fecha
				       and ptc_mod = pre_mod
                       and ptc_pro = pre_pro
                       and ptc_tip = pre_tip
                       and ptc_mon = pre_moneda
                       and pre_mod = sta_mod
                       and pre_status = sta_codigo
                       and pre_destecon = des_codigo
                       --and pre_clientep=96227
                       --and pre_clientep=217123
                       --and pre_clientep = 103807 --in (6010004008,6010003785,6010004442,6010004451,6010004711)
                       --and pre_credito in (6010018488)
                       and pre_credito not in (select lca_cuenta
                                                 from tleg_operacioncalif
                                                where lca_fecha= p_fecha
                                                  and lca_tipo = p_tipo
                                                  and nvl(lca_esnuevo,'N') ='N'))
                       --and nvl(pre_tipocredito,0) in (select distinct par_tipocredito from tpre_paramcaliftipo)  --JAV hd2875

    loop
      w_fecarr := null;
      w_reestructurado := 'N';
      w_reestructuradoactual := 'N';
      w_fecreestruc := null;
      w_diasmora_rees := 0;
      w_gar :='N';
      w_montogar_real:=0;
      w_codtipocredito := NULL;
      w_codtipocredito_antes := NULL;
      w_montocub :=0;
      w_montoexp :=0;
      w_montogar_admisible_dif:=0;
      w_montocub_int:= 0;
      w_montoexp_int:= 0;

      w_desobregiro :='N';
      w_fecrenov:=FECHA_RENOVA(prestamos.pre_credito, p_fecha);
      w_fecrefin:=FECHA_REFINANCIADO(prestamos.pre_credito, p_fecha);      
      w_montogar_admisible:=0;
      w_montogar_admisiblefisa :=0;
      w_esnuevo :='N';
	  w_fechasalreest :=null;
	  w_reescuotadesd := 0;
	  w_tipogarantia:=null;
      w_diasvencontrato:=0;
      w_tipocredito := 0;
      -- REESTRUCTURACIONES
      IF nvl(prestamos.acr_otrostatus,'-') = 'R' then
         w_reestructurado := 'S';
         w_fecreestruc:= pkg_precalifica_do.reestructurado(p_fecha , prestamos.pre_credito );
         w_diasmora_rees := pkg_precalifica_do.dias_mora_en_reest(  w_fecreestruc,prestamos.pre_credito );
      else
         w_reestructurado := 'N';
      end if;
      --conocer si para la provision  se usa la garantia
      w_gar := pkg_precalifica_do.aplica_garantia_prov( 244, prestamos.ptc_tipocredito,'P' );
     ---se cambia de sitio ajuste 2008/09/04
      w_codtipocredito_antes := substr(tipo_credito_antes(prestamos.pre_clientep,prestamos.ptc_tipocredito,p_fecha,nvl(prestamos.acr_capital,0)+nvl(prestamos.acr_interes,0),w_saldoconsolidadosistema),1,1);
      w_codtipocredito := substr(tipo_credito(prestamos.pre_clientep,prestamos.ptc_tipocredito,p_fecha,nvl(prestamos.acr_capital,0)+nvl(prestamos.acr_interes,0),w_saldoconsolidadosistema),1,1);
      --oara que tomen los rangos correctos de la 2/6002
      if w_codtipocredito = 'C' then
         w_tipocredito:= 20;
      elsif w_codtipocredito = 'M' then
         w_tipocredito:= 23;
      elsif w_codtipocredito = 'O' then
         w_tipocredito:= 22;
      elsif w_codtipocredito = 'H' then
         w_tipocredito:= 21;
      elsif w_codtipocredito = w_codmediano then  --Nov 2017
                   w_tipocredito:= w_codnummediano;                  
      end if;
     --se actualiza en la 6/15
        --upd_tipo_credito(prestamos.pre_credito,
                      --   p_fecha,
                        -- 'U',
                         --w_tipocredito);
     --se actualiza en la 6/15

    --ajuste 2008/09/04--por menores deudores
   if nvl(v_nromeses,1) = 1 and  (w_codtipocredito in ('C','M') or w_codtipocredito = w_codmediano) then --NOV 2017
      begin
      select last_day(to_date(max(to_number(pda_anio||lpad(pda_mes,2,'0'))),'yyyymm'))
        into w_ultimotrimestre
       from tpre_prevdefantparam
      where to_number(pda_anio||lpad(pda_mes,2,'0')) < to_number(to_char(p_fecha,'yyyy')||to_char(p_fecha,'mm'))
        and nvl(pda_nromeses,0) = 3;
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
    select sum(nvl(log_valoradmireal,0)), sum(nvl(log_valoradmifisa,0))
      into w_montogar_admisible,w_montogar_admisiblefisa
		from tleg_opergaran
		where log_fecha = p_fecha
		and log_operacion=prestamos.pre_credito;
    select max(lca_tipogarantia)
      into w_tipogarantia
      from tleg_operacioncalif
       where lca_fecha = w_ultimotrimestre
         and lca_cuenta = prestamos.pre_credito;
    else
      w_montogar_real := pkg_precalifica_do.cta_montogar_real( prestamos.pre_mod, prestamos.pre_credito, 0,--moneda local
                                                   			prestamos.pre_sucursal, p_fecha, w_montogar_admisible,
                                                   			w_montogar_admisiblefisa,w_tipogarantia);
    end if;
	  -- MONTO REAL DE GARANTIAS y garantias admisible

      if w_tipogarantia is null then
      w_tipogarantia:= tipo_garantia(prestamos.pre_garantia);
      end if;
      --
      w_codigopais:=pais (prestamos.pre_clientep, prestamos.pre_numdir);

      --Para capital 
      dbms_output.put_line(prestamos.pre_credito||' gar:'||w_montogar_admisible);
      IF nvl(w_montogar_admisible,0) >= nvl(prestamos.acr_capital,0) then -- + nvl(prestamos.acr_interes,0) then
         w_montocub:= nvl(prestamos.acr_capital,0); -- + nvl(prestamos.acr_interes,0);
         w_montoexp:= 0;
         w_montogar_admisible_dif := nvl(w_montogar_admisible,0) -  nvl(prestamos.acr_capital,0);
      else
         w_montocub:= nvl(w_montogar_admisible,0);
         w_montoexp:= (nvl(prestamos.acr_capital,0)) - nvl(w_montogar_admisible,0);
         w_montogar_admisible_dif := 0;
      end if;
      --para interes
      IF nvl(w_montogar_admisible_dif,0) >= nvl(prestamos.acr_interes,0) then
         w_montocub_int:= nvl(prestamos.acr_interes,0);
         w_montoexp_int:= 0;
      else
         w_montocub_int:= nvl(w_montogar_admisible_dif,0);
         w_montoexp_int:= nvl(prestamos.acr_interes,0) - nvl(w_montogar_admisible_dif,0);
      end if;
      --fin
      if nvl(prestamos.pre_modvencorg,0) = 4 then
      w_desobregiro :='S';
      end if ;
      w_diasv:= nvl(prestamos.acr_diasv,0);
      w_diasint:=nvl(prestamos.acr_diasint,0);
      w_diasvcor:= nvl(prestamos.acr_diasv,0);
      w_diasintcor:=nvl(prestamos.acr_diasint,0);
      
      w_diasvencontrato:= prestamos.pre_fecven-p_fecha;
      if w_diasvencontrato < 0 then
         w_diasvencontrato:=0;
      end if;
      --para los mayores deudores saco el atraso de los 12 ultimos meses
      if w_codtipocredito in('C','D') then
       --select max(nvl(acr_diasv,0)),max(nvl(acr_diasint,0))  feb 2018
        select avg(nvl(acr_diasv,0)),avg(nvl(acr_diasint,0))
         into w_diasv,w_diasint
         from tpre_accrual
        where acr_credito = prestamos.pre_credito
          and acr_fecha in (
         select last_day(to_date(to_char(acr_fecha,'yyyymm')||'01','yyyymmdd'))
           from tpre_accrual
           where acr_credito = prestamos.pre_credito
             and acr_fecha between add_months(p_fecha,-12) and p_fecha
           group by last_day(to_date(to_char(acr_fecha,'yyyymm')||'01','yyyymmdd')));
       
       /*select avg(nvl(acr_diasv,0)),avg(nvl(acr_diasint,0))       
          into w_diasv,w_diasint
         from tpre_accrual
        where acr_credito = prestamos.pre_credito
          and acr_fecha between add_months(p_fecha,-12) and p_fecha;*/
          /* es el mayor en el dia
          in (
         select last_day(to_date(to_char(acr_fecha,'yyyymm')||'01','yyyymmdd'))
           from tpre_accrual
           where acr_fecha between add_months(p_fecha,-12) and p_fecha
           group by last_day(to_date(to_char(acr_fecha,'yyyymm')||'01','yyyymmdd')));*/
       end if;
      --fin
      --
      --esto realizo para saber si es el trimestre y saber si es mes de calificaciones y provision
      /* ya esta al inicio de este proceso
      v_nromeses :=0;
      begin
      select nvl(pda_nromeses,0)
        into v_nromeses
       from tpre_prevdefantparam
      where pda_anio = to_char(p_fecha,'yyyy')
        and pda_mes = to_char(p_fecha,'mm');
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || prestamos.pre_mod || ', Cuenta: ' ||
                                   prestamos.pre_credito || ' ' || substr(1,120,sqlerrm));
      end;*/
      if nvl(v_nromeses,0) = 3 then--periodo trismestral , deberiamos parametrizar el 3
          w_esnuevo:='N';
      else
          w_esnuevo:='S'; --Pongo S porque los meses que no se califica se deben identificar los nuevos los datos del trimestre se copian en los siguientes meses.
      end if;
      --
	--valor pignorado
           select sum(pig_valor)
             into w_montopignorado
             from tcli_pignora
            where pig_ctaorg = prestamos.pre_credito
              and pig_fechalev is null;
	--nro de reestructuraciones
	--monto restructurado
	reestruturacion (prestamos.pre_credito,p_fecha,w_montorestruc,w_nrorestruct, w_fechasalreest, w_reescuotadesd);
	if  w_reestructurado = 'N' then
	    if nvl(w_nrorestruct,0)  > 0 then
	             w_reestructurado := 'S';
	             w_fecreestruc:= pkg_precalifica_do.reestructurado(p_fecha , prestamos.pre_credito );
                 w_diasmora_rees := pkg_precalifica_do.dias_mora_en_reest( w_fecreestruc,prestamos.pre_credito );
	    end if;
    end if;
	--ultima renovacion de tasa
	--numero de renovacion de tasa
	--ultima renovacion de tasa
    renovacion_tasa (prestamos.pre_credito,p_fecha,w_fecultrenovtasa,w_nrorenovtasa);
    --renovacion_plazo(prestamos.pre_credito,p_fecha,w_fecultrenovplazo,w_nrorenovplazo);
    select count(distinct acr_plazo) - 1
      into w_nrorenovplazo
      from tpre_accrual
     where acr_credito = prestamos.pre_credito
       and acr_fecha <= p_fecha;

    w_fecultrenovplazo:=w_fecrenov;

	--numero de renovacion de tasa

	--ejecutivo
	select emp_nombre
  	  into w_ejecutivo
      from tgen_usuario,tgen_empleado
      where usr_codemp = emp_codigo
        and usr_codigo = prestamos.pre_codeje;

	--grupo economico
   begin
	select des_descripcion
   	 into w_grupoeconomico
   	  from tgen_desctabla,tcli_persona
   	  where des_codtab = cli_tabgrupo
   	    and des_codigo = cli_grupoeco
   	    and cli_codigo = prestamos.pre_clientep;
   exception
    when others then
      w_grupoeconomico:=0;
   end;
	--ultima calificacion
	begin
     select decode(to_char(p_fecha,'mm'),'01',last_day(add_months(p_fecha,-1)),'02',last_day(add_months(p_fecha,-2)),last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd')))--last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd'))
       into w_fecultcal
      from tpre_prevdefantparam
     where pda_anio = to_number(to_char(P_FECHA,'yyyy'))
       and pda_mes <= to_number(to_char(P_FECHA,'mm'))
  	   and pda_nromeses = 3;
   exception
    when others then
      w_fecultcal:=null;
   end;
	--destino economico,ciiu
        BEGIN
          select des_nombre,substr(DES_CODIGOCIIU, 1, 4)
            Into w_destinoeconomico,w_ciiu
            from tpre_destecon
           where des_codigo = prestamos.pre_destecon;
        Exception
          When others then
            w_ciiu := null;
       end;

	--destino de fondos
	begin
	 select fpa_cuentades||' '||fpa_beneficia
	   into w_destinofondos
	 from tcaj_forpago
	  where fpa_cuentaorg = prestamos.pre_credito
       and fpa_numtran is not null
       and rownum = 1;
        Exception
          When others then
            w_destinofondos := null;
       end;


	--fecha de incripcion de garantia
	--fecha de vencimiento poliza
	--se cambia la busqueda por reunion de amalfi y maria jose 2014/02/05
        select max(ecb_vencepol),max(ogr_fecprot)
          into w_fechavenpolizas,w_fechainsgar
         from tpre_endosobien,tcli_opergaran
         where ecb_codcli(+) = ogr_cliente
           and ecb_numgar(+) = ogr_numgaran
           and ogr_operacion= prestamos.pre_credito
           and OGR_FECHASTA is null
           and ecb_hasta is null;

	--fecha de ingreso a legal
	select max(prl_desde),max(prl_hasta)
	  into w_fechainglegal,w_fechasalilegal
	 from tpre_presleg
	 where prl_credito = prestamos.pre_credito
	   and prl_desde = (select max(prl_desde)
				          from tpre_presleg
				         where prl_credito = prestamos.pre_credito
				           and prl_desde <= p_fecha);
	--fecha de salida de legal
	--numero de compromiso
	w_numcomprom :=prestamos.pre_numcomprom;
	--tipo de amortizacion
	select des_descripcion
   	  into w_tipoamortizacion
     from tgen_desctabla
     where des_codtab = prestamos.pre_tabtipocuo
       and des_codigo = prestamos.pre_tipocuo;
    --para verificar demanda judicial
     begin
      select 'S'
        into w_demandajundicial
        from tpre_presleg
       where prl_credito = prestamos.pre_credito
        and p_fecha between trunc(prl_desde) and nvl(trunc(prl_hasta),to_date('2199/12/01','yyyy/mm/dd'));
    exception
      when others then
        w_demandajundicial:='N';
    end;
     --si modificado el monto aprobado manualmente
       begin
        select nvl(LCA_MONTOAPROBADOMANUAL,'N'),LCA_MONTOAPROBADO
           into w_lca_aprobadomanual,w_lca_montoaprobado
          from tleg_operacioncalif
          where lca_cuenta = prestamos.pre_credito
            and lca_fecha = (select max(lca_fecha)
                               from tleg_operacioncalif
                               where lca_cuenta = prestamos.pre_credito
                                 and lca_fecha = last_day(add_months(p_fecha,-1)) );
       exception
         when others then
          w_lca_aprobadomanual:='N';
       end;
     --monto probado
      begin
      if w_lca_aprobadomanual='N' then
         w_lca_montoaprobado:= pkg_precalifica_do.MONTO_AUTORIZADO_LEG(prestamos.pre_moneda, prestamos.pre_sucursal, prestamos.pre_credito, P_FECHA);
      end if;
      --w_lca_montoaprobado:= nvl(w_lca_montoaprobado,0)*bdi_promedio(prestamos.pre_moneda,prestamos.pre_sucursal,p_fecha);
      exception
         when others then
         w_lca_montoaprobado:=0;
      end;
    --fin
    --cambios junio 2015
    pkg_legal_dom_de.pro_gen_cuoextra(prestamos.pre_credito,p_fecha,w_MONTOCUOTAEXTRAOR,w_fcuoextra);
    --w_TIPOTASA:=
    w_TIPOTASA:=pkg_legal_Dom_de.fun_tipo_tasa(P_FECHA,prestamos.pre_credito);
    -- w_ESTRUCTURACIONCRE
       if w_reestructurado  = 'S' then
          w_ESTRUCTURACIONCRE:= 'RN';
       else
          w_ESTRUCTURACIONCRE:= 'NR';
        end if;                                      
        IF nvl(prestamos.acr_otrostatus,'-') = 'R' then
                 w_ESTRUCTURACIONCRE:= 'RT';
        end if;                     
        if w_ESTRUCTURACIONCRE <> 'RT' then       
		select count(*)
		   into w_represtamo
		from tpre_inccapital
		where inc_status = 2
		  and inc_concepto= 6
		  and inc_credito = prestamos.pre_credito
		  and trunc(inc_fechaaut) <= p_fecha;
          if  w_represtamo > 0 then
             w_ESTRUCTURACIONCRE:= 'RP';
             
          end if;
         end if;
      --w_ORIGENCREDITO
        w_ORIGENCREDITO:=pkg_legal_Dom_de.fun_iso_desctabla(prestamos.PTC_TABORGCREDITO,prestamos.PTC_ORGCREDITO);
      --w_TIPORECURSO
         w_TIPORECURSO:=pkg_legal_Dom_de.fun_iso_desctabla(prestamos.pre_TABORIGFONDOS,prestamos.PRE_ORIGFONDOS);
    --fin cambios junio 2015
      begin
        insert into tleg_operacioncalif ( lca_fecha,lca_tipo,lca_codcli,lca_mod,lca_cuenta,lca_prod,lca_tip,
										  lca_mon,lca_gar,lca_riesgopais,lca_reestructurado,lca_demandajundicial,
										  lca_capital,lca_interes,lca_montogaradmi,lca_fecreestruc,lca_diasreestruc,
										  lca_fecrenov,lca_codigopais,lca_diasmoracap,lca_diasmoraint,lca_tabtipocredito,
								          lca_tipocredito,lca_codtipocredito,lca_status,lca_procesado,lca_montogarreal,
								          lca_suc,lca_ofi,lca_capitalcub,lca_capitalexp,lca_desobregiro,lca_montogaradmifisa,lca_esnuevo,
								          lca_montopignorado, lca_nrorestruct, lca_montorestruc, lca_fecultrenovtasa,lca_nrorenovtasa,
								          lca_fecultrenovplazo, lca_nrorenovplazo,lca_ejecutivo, lca_grupoeconomico, lca_fecultcal, lca_destinoeconomico,
								          lca_destinofondos, lca_fechainsgar,
								          lca_fechavenpolizas, lca_fechainglegal, lca_fechasalilegal,lca_numcomprom,lca_ciiu,lca_tipoamortiza,
								          lca_fechasalreest, lca_reescuotadesd,lca_interescub,lca_interesexp,lca_tipogarantia,lca_diasvencontrato,
								          lca_consolidaSistema,LCA_CODTIPOCREDITOANTES,LCA_MONTOAPROBADO,LCA_MONTOAPROBADOMANUAL,
								          LCA_MONTOCUOTAEXTRAOR,LCA_TIPOTASA,LCA_BALPROMDCAPMES,LCA_INTDEVENCORTE,LCA_COMCARGODEVCORTE,LCA_ESTRUCTURACIONCRE,
								          LCA_ORIGENCREDITO,LCA_TIPORECURSO,LCA_DIASMORACAPCOR,LCA_DIASMORAINTCOR,LCA_FECREFIN)
        values (p_fecha,p_tipo,prestamos.pre_clientep, prestamos.pre_mod,prestamos.pre_credito,prestamos.pre_pro,prestamos.pre_tip,
										  prestamos.pre_moneda,w_gar,w_riesgopais,w_reestructurado,w_demandajundicial,
										  prestamos.acr_capital,prestamos.acr_interes,w_montogar_admisible,w_fecreestruc,nvl(w_diasmora_rees,0),
										  w_fecrenov,w_codigopais,nvl(w_diasv,0),nvl(w_diasint,0),244,
								          w_tipocredito,w_codtipocredito,prestamos.pre_status,'N',w_montogar_real,
								          prestamos.pre_sucursal,prestamos.pre_oficina,w_montocub,w_montoexp,w_desobregiro,
								          w_montogar_admisiblefisa,
								          w_esnuevo,
								          w_montopignorado, w_nrorestruct, w_montorestruc, w_fecultrenovtasa,w_nrorenovtasa,
								          w_fecultrenovplazo,w_nrorenovplazo,w_ejecutivo,w_grupoeconomico,w_fecultcal,w_destinoeconomico,
								          w_destinofondos,w_fechainsgar,
								          w_fechavenpolizas,w_fechainglegal,w_fechasalilegal,w_numcomprom,w_ciiu,w_tipoamortizacion,
								          w_fechasalreest,w_reescuotadesd,w_montocub_int,w_montoexp_int,w_tipogarantia,w_diasvencontrato,
								          w_saldoconsolidadosistema,w_codtipocredito_antes,w_lca_montoaprobado,w_lca_aprobadomanual,
								          w_MONTOCUOTAEXTRAOR,w_TIPOTASA,w_BALPROMDCAPMES,w_INTDEVENCORTE,w_COMCARGODEVCORTE,w_ESTRUCTURACIONCRE,
								          w_ORIGENCREDITO,w_TIPORECURSO,nvl(w_diasvcor,0),nvl(w_diasintcor,0),w_fecrefin) ;
      exception
        when others then
         rollback;
          raise_application_error (-20505,'Error al insertar datos de Mód: ' || prestamos.pre_mod || ', Cuenta: ' ||
                                   prestamos.pre_credito || ' ' || substr(1,120,sqlerrm));
      end;
      commit;
    end loop;
    admisibilidad_garantias (p_fecha,p_tipo,v_nromeses);
    upd_prorrateo_gar(p_fecha,p_tipo);
    --recalcula_tipoCredito(p_fecha,p_tipo);
    commit;
  end;
  --cuentas a la Vista sobregiros contratados
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Carga de datos del módulo de capataciones a la vista
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  procedure datos_ini_mod4 ( p_operador number, p_fecha date, p_tipo varchar2 ) is
    w_diasmora_cap tpre_operacioncalif.oca_diasmora%type;
    w_diasmora_int tpre_operacioncalif.oca_diasmora%type;
    w_diasmora_rees tpre_operacioncalif.oca_diasmora%type;

    w_reprogramaciones tpre_operacioncalif.oca_reprograma%type;
    w_cuotaspag tpre_operacioncalif.oca_cuotaspag%type;	--JAV hd2875
    w_autoliquidable tpre_operacioncalif.oca_autoliquidable%type;
    w_gar tpre_operacioncalif.oca_garhip%type;
    w_montogar_admisible tpre_operacioncalif.oca_montogar%type;
    w_montogar_admisiblefisa tpre_operacioncalif.oca_montogar%type;
    w_montogar_real tpre_operacioncalif.oca_montogarreal%type; --JAV hd2875
    w_monto tpre_operacioncalif.oca_capdir%type;
    w_diasbase tpre_prestamos.pre_diasbase%type;
    w_basemes tpre_prestamos.pre_basemes%type;  w_fecarr date;
    w_diasreprograma tpre_operacioncalif.oca_prediasreprograma%type;
    w_demandajundicial varchar2(1):= 'N';
    cuantos number(4);
    w_reestructurado varchar2(1);
    w_reestructuradoactual varchar2(1);
    w_fecreestruc date;
    w_codtipocredito VARCHAR2(8);
    w_codtipocredito_antes varchar2(8);
    w_codigopais number;
    w_fecrenov date;
    w_interes number;
    w_capital number;
    w_riesgopais varchar2(1):='A';
    w_pais number;
    w_montocub number;
    w_montoexp number;
    w_desobregiro varchar2(1):='N';
    w_diasv number;
    w_diasint number;
    v_nromeses number;
    w_esnuevo varchar2(1):='N';
    --w_montogar_admisible number;
	w_montopignorado                       number(18,2);
	w_nrorestruct                          number(18,2);
	w_montorestruc                         number(18,2);
	w_fecultrenovtasa                      date;
	w_nrorenovtasa                         number;
	w_fecultrenovplazo                     date;
	w_nrorenovplazo                        number;
	w_ejecutivo                            varchar2(30);
	w_grupoeconomico                       varchar2(100);
	w_fecultcal                            date;
	w_destinoeconomico                     varchar2(100);
	w_destinofondos                        varchar2(100);
	w_fechainsgar                          date;
	w_fechavenpolizas                      date;
	w_fechainglegal                        date;
	w_fechasalilegal                       date;
	w_numcomprom                           number(10);
	w_ciiu                                 varchar2(6);
	w_tipoamortizacion                     varchar2(100);
	w_existecalif number:=0;
	w_existeprev number:=0;
	w_fechasalreest date;
	w_reescuotadesd number;
    w_montogar_admisible_dif number:=0;
    w_montocub_int number:= 0;
    w_montoexp_int number:= 0;
    w_tipogarantia varchar2(2);
    w_diasvencontrato number;
    w_tipocredito number;
    w_ultimotrimestre date;--ajuste 2008/09/04
    w_interessob number:=0;
    w_CAPITALsob number:=0;
    w_contingencia number:=0;
    w_saldoconsolidado number:=0;
    w_lca_montoaprobado number:=0;
    w_guarda varchar2(1):='S';
    --
	  w_MONTOCUOTAEXTRAOR                    NUMBER;
	  w_TIPOTASA                             VARCHAR2(1):='F';
	  w_BALPROMDCAPMES                       NUMBER;
	  w_INTDEVENCORTE                        NUMBER;
	  w_COMCARGODEVCORTE                     NUMBER;
	  w_ESTRUCTURACIONCRE                    VARCHAR2(3);
	  w_ORIGENCREDITO                        VARCHAR2(3);
	  w_TIPORECURSO                          VARCHAR2(3);
	    w_fcuoextra          date;
	    w_represtamo number;
  --Nov.2017
    w_montocomparamenor number:=0;
    w_codnummediano number:=0;		
     w_codmediano VARCHAR2(1); 

  begin
      begin
    INSERT INTO TPRE_PARAMCALIF
      (PCA_FECHA, PCA_TIPO, PCA_STATUS, PCA_PREVSTATUS)
    VALUES
      (p_fecha, p_tipo, 'P', 'P');
    exception
    when dup_val_on_index then
      null;
    end;
      v_nromeses :=0;
      begin
      select nvl(pda_nromeses,0)
        into v_nromeses
       from tpre_prevdefantparam
      where pda_anio = to_char(p_fecha,'yyyy')
        and pda_mes = to_char(p_fecha,'mm');
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
     commit; 
     --NOV 2017 
      w_montocomparamenor:=  parametro_calif('MONTO_MENOR_DEUDOR');
      w_codnummediano:=  parametro_calif('CODNUM_MEDIANOS_DEUDORES');
     
    for prestamos in ( select vis_codcli pre_clientep, vis_mod pre_mod, vis_numcue pre_credito,
                              vis_pro pre_pro,vis_tip  pre_tip, crd_valor pre_monto, crd_fechavenc pre_fecven,
                              ptc_tabtipocredito pre_tabtipocredito,ptc_tipocredito pre_tipocredito,
                              vis_moneda pre_moneda, vis_suc pre_sucursal, vis_ofi pre_oficina,
                              vis_status pre_status,
                              sta_descripcion,crd_fechavig  pre_fecemi,13  pre_tipotasa, 361 pre_diasbase, 360 pre_basemes,
                              CRD_CIIU des_codigociiu,
                              1 acr_diasint, 0 acr_diasv, vis_status acr_otrostatus,ptc_tipocredito,
                              nvl(aca_utilizado,0) * bdi_promedio(vis_moneda,vis_suc,p_fecha) acr_capital,
                              nvl(aca_interes,0) * bdi_promedio(vis_moneda,vis_suc,p_fecha) acr_interes,
                              nvl(crd_valor,0) * bdi_promedio(vis_moneda,vis_suc,p_fecha) acr_contingencia,
							  vis_numdircor pre_numdir,0 pre_modvencorg,vis_numcue pre_ctavencorg,
							  68 pre_tabtipocuo, 8 pre_tipocuo,
							  CRD_CIIU pre_destecon,
							  crd_compromaso pre_numcomprom,
							  vis_codeje pre_codeje,
							  crd_tipogar pre_garantia,
							  crd_valor,
							  700 PTC_TABORGCREDITO,
							  3 PTC_ORGCREDITO
                       from tcap_vista,tcap_acract ,tcap_credvista,tgen_status,tpre_protipocredito
                       where vis_numcue = crd_numcue
                       and aca_fecha(+) = p_fecha
                       and crd_numcue = aca_numcue(+)
                       and crd_tipocred = aca_tipocre(+)
                       and crd_secuencia =  aca_secuencia(+)
				       and ptc_mod = vis_mod
                       and ptc_pro = vis_pro
                       and ptc_tip = vis_tip
                       and ptc_mon = vis_moneda
                       and vis_mod = sta_mod
                       and vis_status = sta_codigo
                       and crd_tipocred = 2
					   and  crd_fechavig <= P_FECHA
                       and  trunc(crd_fechautor) <= P_FECHA
					   --and  crd_fechavenc >= P_FECHA DEBEN SALIR LOS VENCIDOS SI ES QUE TIENEN INTERESES PENDIENTE
	                   and crd_fechavenc = (select max(crd_fechavenc)
	                                          from tcap_credvista
	                                          where crd_numcue = vis_numcue
	                                          and crd_tipocred = 2
	                                          and  crd_fechavig <= P_FECHA
                                              and  trunc(crd_fechautor) <= P_FECHA)
					   and  nvl(trunc(crd_fechanula),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
					   and  nvl(trunc(vis_fechcierr),to_date('2199/02/20','yyyy/mm/dd')) > P_FECHA
                       --and pre_clientep = 11684 --in (6010004008,6010003785,6010004442,6010004451,6010004711)
                       --and pre_credito in (6010006105)
                       and vis_numcue not in (select lca_cuenta
                                                 from tleg_operacioncalif
                                                where lca_fecha= p_fecha
                                                  and lca_tipo = p_tipo
                                                  and nvl(lca_esnuevo,'N') ='N'))
                       --and nvl(pre_tipocredito,0) in (select distinct par_tipocredito from tpre_paramcaliftipo)  --JAV hd2875

    loop
      w_fecarr := null;
      w_reestructurado := 'N';
      w_reestructuradoactual := 'N';
      w_fecreestruc := null;
      w_diasmora_rees := 0;
      w_gar :='N';
      w_montogar_real:=0;
      w_codtipocredito := NULL;
      w_codtipocredito_antes:=null;
      w_montocub :=0;
      w_montoexp :=0;
      w_montogar_admisible_dif:=0;
      w_montocub_int:= 0;
      w_montoexp_int:= 0;

      w_desobregiro :='N';
      --w_fecrenov:=FECHA_RENOVA(prestamos.pre_credito, p_fecha); no tiene fecha de renovacion
      w_montogar_admisible:=0;
      w_montogar_admisiblefisa :=0;
      w_esnuevo :='N';
	  w_fechasalreest :=null;
	  w_reescuotadesd := 0;
	  w_tipogarantia:=null;
      w_diasvencontrato:=0;
      w_tipocredito := 0;
      w_interessob :=0;
      w_capitalsob :=0;
      -- REESTRUCTURACIONES
      IF nvl(prestamos.acr_otrostatus,'-') = 'R' then
         w_reestructurado := 'S';
         w_fecreestruc:= pkg_precalifica_do.reestructurado(p_fecha , prestamos.pre_credito );
         w_diasmora_rees := pkg_precalifica_do.dias_mora_en_reest(  w_fecreestruc,prestamos.pre_credito );
      else
         w_reestructurado := 'N';
      end if;
      --conocer si para la provision  se usa la garantia
      w_gar := pkg_precalifica_do.aplica_garantia_prov( 244, prestamos.ptc_tipocredito,'P' );
     ---se cambia de sitio ajuste 2008/09/04
      w_codtipocredito_antes := substr(tipo_credito_antes(prestamos.pre_clientep,prestamos.ptc_tipocredito,p_fecha,nvl(prestamos.acr_capital,0) + nvl(prestamos.acr_interes,0),w_saldoconsolidado),1,1);
      w_codtipocredito := substr(tipo_credito(prestamos.pre_clientep,prestamos.ptc_tipocredito,p_fecha,nvl(prestamos.acr_capital,0) + nvl(prestamos.acr_interes,0),w_saldoconsolidado),1,1);
      --oara que tomen los rangos correctos de la 2/6002
      w_codmediano:='D';
      if w_codtipocredito = 'C' then
         w_tipocredito:= 20;
      elsif w_codtipocredito = 'M' then
         w_tipocredito:= 23;
      elsif w_codtipocredito = 'O' then
         w_tipocredito:= 22;
      elsif w_codtipocredito = 'H' then
         w_tipocredito:= 21;  
      elsif w_codtipocredito = 'D' then --w_codmediano then  --Nov 2017
                   w_tipocredito:= w_codnummediano;         
      end if;

    --ajuste 2008/09/04--por menores deudores
   if nvl(v_nromeses,1) = 1 and  (w_codtipocredito in ('C','M') OR  w_codtipocredito = w_codmediano) then
      begin
      select last_day(to_date(max(to_number(pda_anio||lpad(pda_mes,2,'0'))),'yyyymm'))
        into w_ultimotrimestre
       from tpre_prevdefantparam
      where to_number(pda_anio||lpad(pda_mes,2,'0')) < to_number(to_char(p_fecha,'yyyy')||to_char(p_fecha,'mm'))
        and nvl(pda_nromeses,0) = 3;
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
    select sum(nvl(log_valoradmireal,0)), sum(nvl(log_valoradmifisa,0))
      into w_montogar_admisible,w_montogar_admisiblefisa
		from tleg_opergaran
		where log_fecha = p_fecha
		and log_operacion=prestamos.pre_credito;
    select max(lca_tipogarantia)
      into w_tipogarantia
      from tleg_operacioncalif
       where lca_fecha = w_ultimotrimestre
         and lca_cuenta = prestamos.pre_credito;
    else
      w_montogar_real := pkg_precalifica_do.cta_montogar_real( prestamos.pre_mod, prestamos.pre_credito, 0,--moneda local
                                                   			prestamos.pre_sucursal, p_fecha, w_montogar_admisible,
                                                   			w_montogar_admisiblefisa,w_tipogarantia);
    end if;
	  -- MONTO REAL DE GARANTIAS y garantias admisible

      if w_tipogarantia is null then
      w_tipogarantia:= tipo_garantia(prestamos.pre_garantia);
      end if;
      --
      w_codigopais:=pais (prestamos.pre_clientep, prestamos.pre_numdir);

      --Para capital
      IF nvl(w_montogar_admisible,0) >= nvl(prestamos.acr_capital,0) then -- + nvl(prestamos.acr_interes,0) then
         w_montocub:= nvl(prestamos.acr_capital,0); -- + nvl(prestamos.acr_interes,0);
         w_montoexp:= 0;
         w_montogar_admisible_dif := nvl(w_montogar_admisible,0) -  nvl(prestamos.acr_capital,0);
      else
         w_montocub:= nvl(w_montogar_admisible,0);
         w_montoexp:= (nvl(prestamos.acr_capital,0)) - nvl(w_montogar_admisible,0);
         w_montogar_admisible_dif := 0;
      end if;
      --
       --w_interes
		select nvl(sum((BALTRCY*-1)*bdi_promedio(cy,branch,p_fecha)),0)
		  into w_interessob
          from   tgen_saldomodulos
         where   acc = prestamos.pre_credito
          and    dateload = p_fecha
          and    baltype = 'SCON'
          and    accentrytype = 2;

       select nvl(sum((BALTRCY*-1)*bdi_promedio(cy,branch,p_fecha)),0)
		  into w_capitalsob
          from   tgen_saldomodulos
         where   acc = prestamos.pre_credito
          and    dateload = p_fecha
          and    baltype = 'SCON'
          and    accentrytype = 1;

      --para interes
      --IF nvl(w_montogar_admisible_dif,0) >= nvl(prestamos.acr_interes,0) then
      IF nvl(w_montogar_admisible_dif,0) >= nvl(w_interessob,0) then
         w_montocub_int:= w_interessob;--nvl(prestamos.acr_interes,0);
         w_montoexp_int:= 0;
      else
         w_montocub_int:= nvl(w_montogar_admisible_dif,0);
         --w_montoexp_int:= nvl(prestamos.acr_interes,0) - nvl(w_montogar_admisible_dif,0);
         w_montoexp_int:= nvl(w_interessob,0) - nvl(w_montogar_admisible_dif,0);
      end if;
      --fin
      if nvl(prestamos.pre_modvencorg,0) = 4 then
      w_desobregiro :='S';
      end if ;
      w_diasv:= nvl(prestamos.acr_diasv,0);
      w_diasint:=nvl(prestamos.acr_diasint,0);
      w_diasvencontrato:= prestamos.pre_fecven - p_fecha ;
      if w_diasvencontrato < 0 then
         w_diasvencontrato:=0;
      end if;
      --para los mayores deudores saco el atraso de los 12 ultimos meses
      if w_codtipocredito = 'C' then
         w_diasv:=0;
         w_diasint:=0;
       /*select max(nvl(acr_diasv,0)),max(nvl(acr_diasint,0))
          into w_diasv,w_diasint
         from tpre_accrual
        where acr_credito = prestamos.pre_credito
          and acr_fecha between add_months(p_fecha,-12) and p_fecha;*/
       end if;
      --fin
      if nvl(v_nromeses,0) = 3 then--periodo trismestral , deberiamos parametrizar el 3
          w_esnuevo:='N';
      else
          w_esnuevo:='S'; --Pongo S porque los meses que no se califica se deben identificar los nuevos los datos del trimestre se copian en los siguientes meses.
      end if;
      --
	--valor pignorado
           select sum(pig_valor)
             into w_montopignorado
             from tcli_pignora
            where pig_ctaorg = prestamos.pre_credito
              and pig_fechalev is null;
	--nro de reestructuraciones
	--monto restructurado
	reestruturacion (prestamos.pre_credito,p_fecha,w_montorestruc,w_nrorestruct, w_fechasalreest, w_reescuotadesd);
	if  w_reestructurado = 'N' then
	    if nvl(w_nrorestruct,0)  > 0 then
	             w_reestructurado := 'S';
	             w_fecreestruc:= pkg_precalifica_do.reestructurado(p_fecha , prestamos.pre_credito );
                 w_diasmora_rees := pkg_precalifica_do.dias_mora_en_reest( w_fecreestruc,prestamos.pre_credito );
	    end if;
    end if;
	--ultima renovacion de tasa
	--numero de renovacion de tasa
	--ultima renovacion de tasa - vista
    renovacion_tasa (prestamos.pre_credito,p_fecha,w_fecultrenovtasa,w_nrorenovtasa);
    --renovacion_plazo(prestamos.pre_credito,p_fecha,w_fecultrenovplazo,w_nrorenovplazo);
      w_nrorenovplazo:=0;
    /*select count(distinct acr_plazo) - 1
      into w_nrorenovplazo
      from tpre_accrual
     where acr_credito = prestamos.pre_credito
       and acr_fecha <= p_fecha;*/

    w_fecultrenovplazo:=w_fecrenov;

	--numero de renovacion de tasa

	--ejecutivo
	select emp_nombre
  	  into w_ejecutivo
      from tgen_usuario,tgen_empleado
      where usr_codemp = emp_codigo
        and usr_codigo = prestamos.pre_codeje;

	--grupo economico
   begin
	select des_descripcion
   	 into w_grupoeconomico
   	  from tgen_desctabla,tcli_persona
   	  where des_codtab = cli_tabgrupo
   	    and des_codigo = cli_grupoeco
   	    and cli_codigo = prestamos.pre_clientep;
   exception
    when others then
      w_grupoeconomico:=0;
   end;
	--ultima calificacion
	begin
     select decode(to_char(p_fecha,'mm'),'01',last_day(add_months(p_fecha,-1)),'02',last_day(add_months(p_fecha,-2)),last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd')))--last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd'))
       into w_fecultcal
      from tpre_prevdefantparam
     where pda_anio = to_number(to_char(P_FECHA,'yyyy'))
       and pda_mes <= to_number(to_char(P_FECHA,'mm'))
  	   and pda_nromeses = 3;
   exception
    when others then
      w_fecultcal:=null;
   end;
	--destino economico,ciiu
        BEGIN
          select des_nombre,substr(DES_CODIGOCIIU, 1, 4)
            Into w_destinoeconomico,w_ciiu
            from tpre_destecon
           where --des_codigo = prestamos.pre_destecon;
              des_codigociiu = prestamos.des_codigociiu;
        Exception
          When others then
            w_ciiu := null;
       end;

	--destino de fondos
	begin
	 select fpa_cuentades||' '||fpa_beneficia
	   into w_destinofondos
	 from tcaj_forpago
	  where fpa_cuentaorg = prestamos.pre_credito
       and fpa_numtran is not null
       and rownum = 1;
        Exception
          When others then
            w_destinofondos := null;
       end;


	--fecha de incripcion de garantia
	--fecha de vencimiento poliza
        select max(ecb_vencepol),max(ogr_fecprot)
          into w_fechavenpolizas,w_fechainsgar
         from tpre_endosobien,tcli_opergaran
         where ecb_codcli(+) = ogr_cliente
           and ecb_numgar(+) = ogr_numgaran
           and ogr_operacion= prestamos.pre_credito;

	--fecha de ingreso a legal
	select max(prl_desde),max(prl_hasta)
	  into w_fechainglegal,w_fechasalilegal
	 from tpre_presleg
	 where prl_credito = prestamos.pre_credito
	   and prl_desde = (select max(prl_desde)
				          from tpre_presleg
				         where prl_credito = prestamos.pre_credito
				           and prl_desde <= p_fecha);
	--fecha de salida de legal
	--numero de compromiso
	w_numcomprom :=prestamos.pre_numcomprom;
	--tipo de amortizacion
	begin
	select des_descripcion
   	  into w_tipoamortizacion
     from tgen_desctabla
     where des_codtab = prestamos.pre_tabtipocuo
       and des_codigo = prestamos.pre_tipocuo;
   exception
      when others then
         w_tipoamortizacion:=null;
     end;
    --para verificar demanda judicial
     begin
      select 'S'
        into w_demandajundicial
        from tpre_presleg
       where prl_credito = prestamos.pre_credito
        and p_fecha between trunc(prl_desde) and nvl(trunc(prl_hasta),to_date('2199/12/01','yyyy/mm/dd'));
    exception
      when others then
        w_demandajundicial:='N';
    end;
    w_guarda := 'S';
    --contingencia
    if  prestamos.pre_fecven >= p_fecha then
        w_contingencia := nvl(prestamos.acr_contingencia,0) - nvl(prestamos.acr_capital,0);
        w_lca_montoaprobado:= prestamos.crd_valor*bdi_promedio(prestamos.pre_moneda,prestamos.pre_sucursal,p_fecha);
    else
       if nvl(w_interessob,0) <> 0 then
        w_contingencia := 0;
        w_lca_montoaprobado:= 0;
        else
                w_guarda := 'N';
       end if;
    end if;

    --cambios junio 2015
    pkg_legal_dom_de.pro_gen_cuoextra(prestamos.pre_credito,p_fecha,w_MONTOCUOTAEXTRAOR,w_fcuoextra);
    --w_TIPOTASA:=
    w_TIPOTASA:='V';--sobregiros contratados
    -- w_ESTRUCTURACIONCRE
       if w_reestructurado  = 'S' then
          w_ESTRUCTURACIONCRE:= 'RN';
       else
          w_ESTRUCTURACIONCRE:= 'NR';
        end if;
		select count(*)
		   into w_represtamo
		from tpre_inccapital
		where inc_status = 2
		  and inc_concepto= 6
		  and inc_credito = prestamos.pre_credito
		  and trunc(inc_fechaaut) <= p_fecha;
          if  w_represtamo > 0 then
             w_ESTRUCTURACIONCRE:= 'RP';
          end if;
      --w_ORIGENCREDITO
        w_ORIGENCREDITO:=pkg_legal_Dom_de.fun_iso_desctabla(prestamos.PTC_TABORGCREDITO,prestamos.PTC_ORGCREDITO);
      --w_TIPORECURSO
         w_TIPORECURSO:=pkg_legal_Dom_de.fun_iso_desctabla(69,1);
    --fin cambios junio 2015

    --fin
      begin
--      if nvl(w_capitalsob,0) <> 0 or nvl(w_interessob,0) <> 0 then
       if w_guarda = 'S' then
        insert into tleg_operacioncalif ( lca_fecha,lca_tipo,lca_codcli,lca_mod,lca_cuenta,lca_prod,lca_tip,
										  lca_mon,lca_gar,lca_riesgopais,lca_reestructurado,lca_demandajundicial,
										  lca_capital,lca_interes,lca_montogaradmi,lca_fecreestruc,lca_diasreestruc,
										  lca_fecrenov,lca_codigopais,lca_diasmoracap,lca_diasmoraint,lca_tabtipocredito,
								          lca_tipocredito,lca_codtipocredito,lca_status,lca_procesado,lca_montogarreal,
								          lca_suc,lca_ofi,lca_capitalcub,lca_capitalexp,lca_desobregiro,lca_montogaradmifisa,lca_esnuevo,
								          lca_montopignorado, lca_nrorestruct, lca_montorestruc, lca_fecultrenovtasa,lca_nrorenovtasa,
								          lca_fecultrenovplazo, lca_nrorenovplazo,lca_ejecutivo, lca_grupoeconomico, lca_fecultcal, lca_destinoeconomico,
								          lca_destinofondos, lca_fechainsgar,
								          lca_fechavenpolizas, lca_fechainglegal, lca_fechasalilegal,lca_numcomprom,lca_ciiu,lca_tipoamortiza,
								          lca_fechasalreest, lca_reescuotadesd,lca_interescub,lca_interesexp,lca_tipogarantia,lca_diasvencontrato,
								          lca_contingente,lca_consolidaSistema,lca_montoaprobado,
								          LCA_MONTOCUOTAEXTRAOR,LCA_TIPOTASA,LCA_BALPROMDCAPMES,LCA_INTDEVENCORTE,LCA_COMCARGODEVCORTE,LCA_ESTRUCTURACIONCRE,
                                          LCA_ORIGENCREDITO,LCA_TIPORECURSO)
        values (p_fecha,p_tipo,prestamos.pre_clientep, prestamos.pre_mod,prestamos.pre_credito,prestamos.pre_pro,prestamos.pre_tip,
										  prestamos.pre_moneda,w_gar,w_riesgopais,w_reestructurado,w_demandajundicial,
										  prestamos.acr_capital,w_interessob,w_montogar_admisible,w_fecreestruc,nvl(w_diasmora_rees,0),
										  w_fecrenov,w_codigopais,nvl(w_diasv,0),nvl(w_diasint,0),244,
								          w_tipocredito,w_codtipocredito,prestamos.pre_status,'N',w_montogar_real,
								          prestamos.pre_sucursal,prestamos.pre_oficina,w_montocub,w_montoexp,w_desobregiro,w_montogar_admisiblefisa,
								          w_esnuevo,
								          w_montopignorado, w_nrorestruct, w_montorestruc, w_fecultrenovtasa,w_nrorenovtasa,
								          w_fecultrenovplazo,w_nrorenovplazo,w_ejecutivo,w_grupoeconomico,w_fecultcal,w_destinoeconomico,
								          w_destinofondos,w_fechainsgar,
								          w_fechavenpolizas,w_fechainglegal,w_fechasalilegal,w_numcomprom,w_ciiu,w_tipoamortizacion,
								          w_fechasalreest,w_reescuotadesd,w_montocub_int,w_montoexp_int,w_tipogarantia,w_diasvencontrato,
								          w_contingencia,w_saldoconsolidado,prestamos.crd_valor,
								          w_MONTOCUOTAEXTRAOR,w_TIPOTASA,w_BALPROMDCAPMES,w_INTDEVENCORTE,w_COMCARGODEVCORTE,w_ESTRUCTURACIONCRE,
								          w_ORIGENCREDITO,w_TIPORECURSO) ;
     end if;
      exception
        when others then
         rollback;
          raise_application_error (-20505,'Error al insertar datos de Mód: ' || prestamos.pre_mod || ', Cuenta: ' ||
                                   prestamos.pre_credito || ' ' || substr(1,120,sqlerrm));
      end;
    end loop;
     -- para vista sobregiros contratados
    admisibilidad_garantias (p_fecha,p_tipo,v_nromeses);
    upd_prorrateo_gar(p_fecha,p_tipo);
    commit;
  end;
--10
  --cuentas a la Vista sobregiros contratados
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Carga de datos del módulo de capataciones a la vista
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  procedure datos_ini_mod10 ( p_operador number, p_fecha date, p_tipo varchar2 ) is
    w_diasmora_cap tpre_operacioncalif.oca_diasmora%type;
    w_diasmora_int tpre_operacioncalif.oca_diasmora%type;
    w_diasmora_rees tpre_operacioncalif.oca_diasmora%type;

    w_reprogramaciones tpre_operacioncalif.oca_reprograma%type;
    w_cuotaspag tpre_operacioncalif.oca_cuotaspag%type;	--JAV hd2875
    w_autoliquidable tpre_operacioncalif.oca_autoliquidable%type;
    w_gar tpre_operacioncalif.oca_garhip%type;
    w_montogar_admisible tpre_operacioncalif.oca_montogar%type;
    w_montogar_admisiblefisa tpre_operacioncalif.oca_montogar%type;
    w_montogar_real tpre_operacioncalif.oca_montogarreal%type; --JAV hd2875
    w_monto tpre_operacioncalif.oca_capdir%type;
    w_diasbase tpre_prestamos.pre_diasbase%type;
    w_basemes tpre_prestamos.pre_basemes%type;  w_fecarr date;
    w_diasreprograma tpre_operacioncalif.oca_prediasreprograma%type;
    w_demandajundicial varchar2(1):= 'N';
    cuantos number(4);
    w_reestructurado varchar2(1);
    w_reestructuradoactual varchar2(1);
    w_fecreestruc date;
    w_codtipocredito VARCHAR2(8);
    w_codtipocredito_antes VARCHAR2(8);
    w_codigopais number;
    w_fecrenov date;
    w_interes number;
    w_capital number;
    w_riesgopais varchar2(1):='A';
    w_pais number;
    w_montocub number;
    w_montoexp number;
    w_desobregiro varchar2(1):='N';
    w_diasv number;
    w_diasint number;
    v_nromeses number;
    w_esnuevo varchar2(1):='N';
    --w_montogar_admisible number;
	w_montopignorado                       number(18,2);
	w_nrorestruct                          number(18,2);
	w_montorestruc                         number(18,2);
	w_fecultrenovtasa                      date;
	w_nrorenovtasa                         number;
	w_fecultrenovplazo                     date;
	w_nrorenovplazo                        number;
	w_ejecutivo                            varchar2(30);
	w_grupoeconomico                       varchar2(100);
	w_fecultcal                            date;
	w_destinoeconomico                     varchar2(100);
	w_destinofondos                        varchar2(100);
	w_fechainsgar                          date;
	w_fechavenpolizas                      date;
	w_fechainglegal                        date;
	w_fechasalilegal                       date;
	w_numcomprom                           number(10);
	w_ciiu                                 varchar2(6);
	w_tipoamortizacion                     varchar2(100);
	w_existecalif number:=0;
	w_existeprev number:=0;
	w_fechasalreest date;
	w_reescuotadesd number;
    w_montogar_admisible_dif number:=0;
    w_montocub_int number:= 0;
    w_montoexp_int number:= 0;
    w_tipogarantia varchar2(2);
    w_diasvencontrato number;
    w_tipocredito number;
    w_ultimotrimestre date;--ajuste 2008/09/04
    w_interessob number:=0;
    w_contingencia number:=0;
    p_Montocompara number:=0;
    w_pre_numdir number;
    w_estacopiado varchar2(1);
    w_diasvencontratodos number;
    w_saldoconsolidado number:=0;
    w_MONTOCUOTAEXTRAOR                    NUMBER;
	w_TIPOTASA                             VARCHAR2(1):='F';
	w_BALPROMDCAPMES                       NUMBER;
	w_INTDEVENCORTE                        NUMBER;
	w_COMCARGODEVCORTE                     NUMBER;
	w_ESTRUCTURACIONCRE                    VARCHAR2(3);
	w_ORIGENCREDITO                        VARCHAR2(3);
	w_TIPORECURSO                          VARCHAR2(3);
	w_fcuoextra          date;
	w_represtamo number;
    --Nov 2017
    w_codnummediano number:=0;  
    w_codmediano varchar2(1);

  begin
      begin
    INSERT INTO TPRE_PARAMCALIF
      (PCA_FECHA, PCA_TIPO, PCA_STATUS, PCA_PREVSTATUS)
    VALUES
      (p_fecha, p_tipo, 'P', 'P');
    exception
    when dup_val_on_index then
      null;
    end;
      v_nromeses :=0;
      begin
      select nvl(pda_nromeses,0)
        into v_nromeses
       from tpre_prevdefantparam
      where pda_anio = to_char(p_fecha,'yyyy')
        and pda_mes = to_char(p_fecha,'mm');
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
          --p_Montocompara
          select par_valor
            into p_Montocompara
          from tpre_paramcaliftipo
          where par_codigo = 1;
     commit;
     w_codnummediano:=  parametro_calif('CODNUM_MEDIANOS_DEUDORES'); 
     w_codmediano :=  parametro_calif('COD_MEDIANOS_DEUDORES'); 
     
    for prestamos in ((SELECT MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter * bdi_promedio(CYLETTER,branch,p_fecha) acr_contingencia,
             dateexpired pre_fecven,
             branch pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter)* bdi_promedio(CYLETTER,branch,p_fecha) acr_capital,
             operator pre_codeje ,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
             nvl(status,'X') pre_status,
             20 ptc_tipocredito,
             typeprod pre_tip,
             prod pre_pro,
             dateissue fecha_Emision
        FROM tlettercredit
       WHERE --NVL(status, '!') = '!'
       --decode(CYLETTER,0,valueletter,bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) >       p_Montocompara AND
       seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
       codeletter not in
       (select codeletter
          FROM tlettercredit A, Tstageprod B, tstage C
         WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
               a.module = c.module AND a.prod = c.prod AND
               a.typeprod = c.typeprod AND a.seqamen = b.seq and
               c.stage = 'ST_CLOSE' and DATEPROCESS <= p_fecha + 0.99999)
      UNION
      SELECT a.MODULE pre_mod,
             codeclient pre_clientep,
             codeletter pre_credito,
             valueletter* bdi_promedio(CYLETTER,branch,p_fecha) Montoaprobado,
             dateexpired pre_fecven,
             branch pre_sucursal,
             office pre_oficina,
             pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                   codeletter)* bdi_promedio(CYLETTER,branch,p_fecha) Balance,
             operator,
             seqrequest,
             CYLETTER pre_moneda,
             CODEAPPLI,
             DATEPROCESS,
             nvl(a.status,'X'),
             20 ptc_tipocredito,
             a.typeprod,
             a.prod,
             dateissue
        FROM tlettercredit A, Tstageprod B, tstage C
       WHERE a.codeletter = b.code AND b.seqstage = c.seqstage AND
             a.module = c.module AND a.prod = c.prod AND
             a.typeprod = c.typeprod AND a.seqamen = b.seq AND
             c.stage = 'ST_CLOSE' AND NVL(a.status, '!') in ('C', 'P') AND
             1 = 0 AND --SE DESABILITA LOS CANCELADOS O CERRADOS NO VAN
             --decode(CYLETTER,0,valueletter,bdi_promedio(CYLETTER, branch, p_fecha) * valueletter) > p_Montocompara AND
             seqamen = pkg_discrep.ultima_lettercredit(codeletter) AND
             DATEPROCESS between p_fecha and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codedraft,
                    valuedraft*bdi_promedio(CYDRAFT,branch,p_fecha) valuedraft,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codedraft) * bdi_promedio(CYDRAFT,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    nvl(status,'X'),
                    20 ptc_tipocredito,
                    typeprod,
                    prod,
                    dateissue
               FROM tdraft
              WHERE --NVL(status, '!') = '!'
              --decode(CYDRAFT,0,valuedraft,bdi_promedio(CYDRAFT, branch, p_fecha) * valuedraft) > P_Montocompara AND
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
                                                          codedraft) * bdi_promedio(CYDRAFT,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYDRAFT,
                    CODEAPPLI,
                    DATEPROCESS,
                    nvl(a.status,'X'),
                    20 ptc_tipocredito,
                    a.typeprod,
                    a.prod,
                    dateissue
               FROM tdraft A, Tstageprod B, tstage C
              WHERE a.codedraft = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    1 = 0 AND --SE DESABILITA LOS CANCELADOS O CERRADOS NO VAN
                    --decode(CYDRAFT,0,valuedraft,bdi_promedio(CYDRAFT, branch, p_fecha) * valuedraft) > P_Montocompara AND
                    DATEPROCESS between p_fecha and p_fecha + 0.99999)
      UNION (SELECT MODULE,
                    codeclient,
                    codeguarantee,
                    valueguarantee*bdi_promedio(CYGUARANTEE,branch,p_fecha) valueguarantee,
                    dateexpired,
                    branch,
                    office,
                    pkg_functcollateral.balance_operation(f_fechatrabajo,
                                                          codeguarantee) * bdi_promedio(CYGUARANTEE,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    to_date('2199/01/01', 'yyyy/mm/dd') pre_fecance,
                    nvl(status,'X'),
                    20 ptc_tipocredito,
                    typeprod,
                    prod,
                    dateinput datevalue
               FROM tguarantee
              WHERE --NVL(status, '!') = '!' AND
              --decode(CYGUARANTEE,0,valueguarantee,bdi_promedio(CYGUARANTEE, branch, p_fecha) * valueguarantee) > p_Montocompara AND
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
                                                          codeguarantee) * bdi_promedio(CYGUARANTEE,branch,p_fecha) Balance,
                    operator,
                    seqrequest,
                    CYGUARANTEE,
                    CODEAPPLI,
                    DATEPROCESS,
                    nvl(a.status,'X'),
                    20 ptc_tipocredito,
                    a.typeprod,
                    a.prod,
                    dateinput datevalue
               FROM tguarantee A, Tstageprod B, tstage C
              WHERE a.codeguarantee = b.code AND b.seqstage = c.seqstage AND
                    a.module = c.module AND a.prod = c.prod AND
                    a.typeprod = c.typeprod AND a.seqamen = b.seq AND
                    c.stage = 'ST_CLOSE' AND
                    NVL(a.status, '!') in ('C', 'P') AND
                    1 = 0 AND --SE DESABILITA LOS CANCELADOS O CERRADOS NO VAN
                    --decode(CYGUARANTEE,0,valueguarantee,bdi_promedio(CYGUARANTEE, branch, p_fecha) * valueguarantee) > p_Montocompara AND
                    seqamen = PKG_GUAR_FUNCTS.last_seqamen(codeguarantee) AND
                    DATEPROCESS between p_fecha and p_fecha + 0.99999))	    loop
      w_fecarr := null;
      w_reestructurado := 'N';
      w_reestructuradoactual := 'N';
      w_fecreestruc := null;
      w_diasmora_rees := 0;
      w_gar :='N';
      w_montogar_real:=0;
      w_codtipocredito := NULL;
      w_codtipocredito_antes := NULL;
      w_montocub :=0;
      w_montoexp :=0;
      w_montogar_admisible_dif:=0;
      w_montocub_int:= 0;
      w_montoexp_int:= 0;

      w_desobregiro :='N';
      --w_fecrenov:=FECHA_RENOVA(prestamos.pre_credito, p_fecha); no tiene fecha de renovacion
      w_montogar_admisible:=0;
      w_montogar_admisiblefisa :=0;
      w_esnuevo :='N';
	  w_fechasalreest :=null;
	  w_reescuotadesd := 0;
	  w_tipogarantia:=null;
      w_diasvencontrato:=0;
      w_tipocredito := 0;
      w_interessob :=0;
      -- REESTRUCTURACIONES
         w_reestructurado := 'N';

      --conocer si para la provision  se usa la garantia
      w_gar := pkg_precalifica_do.aplica_garantia_prov( 244, prestamos.ptc_tipocredito,'P' );
     ---se cambia de sitio ajuste 2008/09/04
      w_codtipocredito_antes := substr(tipo_credito_antes(prestamos.pre_clientep,prestamos.ptc_tipocredito,p_fecha,prestamos.acr_capital,w_saldoconsolidado),1,1);
      w_codtipocredito := substr(tipo_credito(prestamos.pre_clientep,prestamos.ptc_tipocredito,p_fecha,prestamos.acr_capital,w_saldoconsolidado),1,1);
      /*if nvl(prestamos.acr_contingencia,0) >= p_montocompara then
         w_codtipocredito:='C';
      else
          w_codtipocredito:='M';
      end if;*/
      --oara que tomen los rangos correctos de la 2/6002
      if w_codtipocredito = 'C' then
         w_tipocredito:= 20;
      elsif w_codtipocredito = 'M' then
         w_tipocredito:= 23;
      elsif w_codtipocredito = 'O' then
         w_tipocredito:= 22;
      elsif w_codtipocredito = 'H' then
         w_tipocredito:= 21;
      elsif w_codtipocredito = w_codmediano then  --Nov 2017
                   w_tipocredito:= w_codnummediano;         
      end if;

    --ajuste 2008/09/04--por menores deudores
   if nvl(v_nromeses,1) = 1 and  (w_codtipocredito in ('C','M') or w_codtipocredito = w_codmediano) then
      begin
      select last_day(to_date(max(to_number(pda_anio||lpad(pda_mes,2,'0'))),'yyyymm'))
        into w_ultimotrimestre
       from tpre_prevdefantparam
      where to_number(pda_anio||lpad(pda_mes,2,'0')) < to_number(to_char(p_fecha,'yyyy')||to_char(p_fecha,'mm'))
        and nvl(pda_nromeses,0) = 3;
      exception
      when others then
         rollback;
          raise_application_error (-20505,'revise parametros de la 2/6009: ' || substr(1,120,sqlerrm));
      end;
    select sum(nvl(log_valoradmireal,0)), sum(nvl(log_valoradmifisa,0))
      into w_montogar_admisible,w_montogar_admisiblefisa
		from tleg_opergaran
		where log_fecha = p_fecha
		and log_operacion=prestamos.pre_credito;
    select max(lca_tipogarantia)
      into w_tipogarantia
      from tleg_operacioncalif
       where lca_fecha = w_ultimotrimestre
         and lca_cuenta = prestamos.pre_credito;
    else
      w_montogar_real := pkg_precalifica_do.cta_montogar_real( prestamos.pre_mod, prestamos.pre_credito, 0,--moneda local
                                                   			prestamos.pre_sucursal, p_fecha, w_montogar_admisible,
                                                   			w_montogar_admisiblefisa,w_tipogarantia);
    end if;
	  -- MONTO REAL DE GARANTIAS y garantias admisible

      --if w_tipogarantia is null then
      --w_tipogarantia:= tipo_garantia(prestamos.pre_garantia);
      --end if;
      --
      select min(dir_numero)
      into w_pre_numdir
      from tcli_direccion
      where dir_codcli = prestamos.pre_clientep;
      w_codigopais:=pais (prestamos.pre_clientep, w_pre_numdir);

      --Para capital
      IF nvl(w_montogar_admisible,0) >= nvl(prestamos.acr_capital,0) then -- + nvl(prestamos.acr_interes,0) then
         w_montocub:= nvl(prestamos.acr_capital,0); -- + nvl(prestamos.acr_interes,0);
         w_montoexp:= 0;
         w_montogar_admisible_dif := nvl(w_montogar_admisible,0) -  nvl(prestamos.acr_capital,0);
      else
         w_montocub:= nvl(w_montogar_admisible,0);
         w_montoexp:= (nvl(prestamos.acr_capital,0)) - nvl(w_montogar_admisible,0);
         w_montogar_admisible_dif := 0;
      end if;
      --
       --w_interes
		select nvl(sum((BALTRCY*-1)*bdi_promedio(cy,branch,p_fecha)),0)
		  into w_interessob
          from   tgen_saldomodulos
         where   acc = prestamos.pre_credito
          and    dateload = p_fecha
          and    baltype = 'SCON'
          and    accentrytype = 2;

      --para interes
      --IF nvl(w_montogar_admisible_dif,0) >= nvl(prestamos.acr_interes,0) then
      IF nvl(w_montogar_admisible_dif,0) >= nvl(w_interessob,0) then
         w_montocub_int:= w_interessob;--nvl(prestamos.acr_interes,0);
         w_montoexp_int:= 0;
      else
         w_montocub_int:= nvl(w_montogar_admisible_dif,0);
         --w_montoexp_int:= nvl(prestamos.acr_interes,0) - nvl(w_montogar_admisible_dif,0);
         w_montoexp_int:= nvl(w_interessob,0) - nvl(w_montogar_admisible_dif,0);
      end if;
      --fin
      w_diasv:= 0;-- nvl(prestamos.acr_diasv,0);
      w_diasint:=0;--nvl(prestamos.acr_diasint,0);
      w_diasvencontrato:=   p_fecha - prestamos.pre_fecven;
      w_diasvencontratodos:=   prestamos.pre_fecven-p_fecha;
      if w_diasvencontrato < 0 then
         w_diasvencontrato:=0;
      end if;
      --para los mayores deudores saco el atraso de los 12 ultimos meses
      if w_codtipocredito = 'C' then
         w_diasv:=0;
         w_diasint:=0;
       /*select max(nvl(acr_diasv,0)),max(nvl(acr_diasint,0))
          into w_diasv,w_diasint
         from tpre_accrual
        where acr_credito = prestamos.pre_credito
          and acr_fecha between add_months(p_fecha,-12) and p_fecha;*/
       end if;
      --fin
      if nvl(v_nromeses,0) = 3 then--periodo trismestral , deberiamos parametrizar el 3
          w_esnuevo:='N';
      else
          w_esnuevo:='S'; --Pongo S porque los meses que no se califica se deben identificar los nuevos los datos del trimestre se copian en los siguientes meses.
      end if;
      --
	--valor pignorado
           select sum(pig_valor)
             into w_montopignorado
             from tcli_pignora
            where pig_ctaorg = prestamos.pre_credito
              and pig_fechalev is null;
	--nro de reestructuraciones
	--monto restructurado
	reestruturacion (prestamos.pre_credito,p_fecha,w_montorestruc,w_nrorestruct, w_fechasalreest, w_reescuotadesd);
	if  w_reestructurado = 'N' then
	    if nvl(w_nrorestruct,0)  > 0 then
	             w_reestructurado := 'S';
	             w_fecreestruc:= pkg_precalifica_do.reestructurado(p_fecha , prestamos.pre_credito );
                 w_diasmora_rees := pkg_precalifica_do.dias_mora_en_reest( w_fecreestruc,prestamos.pre_credito );
	    end if;
    end if;
	--ultima renovacion de tasa
	--numero de renovacion de tasa
	--ultima renovacion de tasa - vista
    renovacion_tasa (prestamos.pre_credito,p_fecha,w_fecultrenovtasa,w_nrorenovtasa);
    --renovacion_plazo(prestamos.pre_credito,p_fecha,w_fecultrenovplazo,w_nrorenovplazo);
      w_nrorenovplazo:=0;
    /*select count(distinct acr_plazo) - 1
      into w_nrorenovplazo
      from tpre_accrual
     where acr_credito = prestamos.pre_credito
       and acr_fecha <= p_fecha;*/

    w_fecultrenovplazo:=w_fecrenov;

	--numero de renovacion de tasa

	--ejecutivo
	select emp_nombre
  	  into w_ejecutivo
      from tgen_usuario,tgen_empleado
      where usr_codemp = emp_codigo
        and usr_codigo = prestamos.pre_codeje;

	--grupo economico
   begin
	select des_descripcion
   	 into w_grupoeconomico
   	  from tgen_desctabla,tcli_persona
   	  where des_codtab = cli_tabgrupo
   	    and des_codigo = cli_grupoeco
   	    and cli_codigo = prestamos.pre_clientep;
   exception
    when others then
      w_grupoeconomico:=0;
   end;
	--ultima calificacion
	begin
     select decode(to_char(p_fecha,'mm'),'01',last_day(add_months(p_fecha,-1)),'02',last_day(add_months(p_fecha,-2)),last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd')))--last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd'))
       into w_fecultcal
      from tpre_prevdefantparam
     where pda_anio = to_number(to_char(P_FECHA,'yyyy'))
       and pda_mes <= to_number(to_char(P_FECHA,'mm'))
  	   and pda_nromeses = 3;
   exception
    when others then
      w_fecultcal:=null;
   end;
	--destino economico,ciiu
        BEGIN
          Select substr(CII_DESCRIPCION,1,100),substr(CII_CODIGO, 1, 2)
            Into w_destinoeconomico,w_ciiu
            From tcli_natural, tgen_ciiu
           Where nat_codcli = prestamos.pre_clientep
           and nat_actprin = cii_tipoact;
        Exception
          When no_data_found then
            w_ciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Natural' ||
                                    sqlerrm);
        END;
        BEGIN
          Select substr(CII_DESCRIPCION,1,100),substr(CII_CODIGO, 1, 2) --HD4324_2
            Into w_destinoeconomico,w_ciiu
            From tcli_JURIDICA, tgen_ciiu
           Where JUR_codcli = prestamos.pre_clientep and
                 JUR_actividad = cii_tipoact;
        Exception
          When no_data_found then
            w_ciiu := null;
          when others then
            raise_application_error(-20212,
                                    'Error al obtener Actividad principal del Deudor Jurídico' ||
                                    sqlerrm);
        END;

       -- BEGIN
          --select des_nombre,substr(DES_CODIGOCIIU, 1, 4)
            --Into w_destinoeconomico,w_ciiu
            --from tpre_destecon
           --where --des_codigo = prestamos.pre_destecon;
          --    des_codigociiu = prestamos.des_codigociiu;
        --Exception
         -- When others then
            w_ciiu := null;
       --end;

	--destino de fondos
	begin
	 select fpa_cuentades||' '||fpa_beneficia
	   into w_destinofondos
	 from tcaj_forpago
	  where fpa_cuentaorg = prestamos.pre_credito
       and fpa_numtran is not null
       and rownum = 1;
        Exception
          When others then
            w_destinofondos := null;
       end;


	--fecha de incripcion de garantia
	--fecha de vencimiento poliza
        select max(ecb_vencepol),max(ogr_fecprot)
          into w_fechavenpolizas,w_fechainsgar
         from tpre_endosobien,tcli_opergaran
         where ecb_codcli(+) = ogr_cliente
           and ecb_numgar(+) = ogr_numgaran
           and ogr_operacion= prestamos.pre_credito;

	--fecha de ingreso a legal
	select max(prl_desde),max(prl_hasta)
	  into w_fechainglegal,w_fechasalilegal
	 from tpre_presleg
	 where prl_credito = prestamos.pre_credito
	   and prl_desde = (select max(prl_desde)
				          from tpre_presleg
				         where prl_credito = prestamos.pre_credito
				           and prl_desde <= p_fecha);
	--fecha de salida de legal
	--numero de compromiso
	begin
	select max(codeline)
	 into w_numcomprom --:=prestamos.pre_numcomprom;
	 from TCLIENTLINE
	 where CODEOPERATION = prestamos.pre_credito;
  exception
    when others then
      w_numcomprom:=0;
   end;
	--tipo de amortizacion
	--begin
	 --select des_descripcion
   	 --into w_tipoamortizacion
     --from tgen_desctabla
     --where des_codtab = prestamos.pre_tabtipocuo
     --and des_codigo = prestamos.pre_tipocuo;
   --exception
      --when others then
         w_tipoamortizacion:=null;
     --end;
    --para verificar demanda judicial
     begin
      select 'S'
        into w_demandajundicial
        from tpre_presleg
       where prl_credito = prestamos.pre_credito
        and p_fecha between trunc(prl_desde) and nvl(trunc(prl_hasta),to_date('2199/12/01','yyyy/mm/dd'));
    exception
      when others then
        w_demandajundicial:='N';
    end;
    --contingencia
    w_contingencia := nvl(prestamos.acr_contingencia,0);-- - nvl(prestamos.acr_capital,0);
    --fin
    --Para conocer si le copio antes
   /* begin
    		 w_estacopiado:='N';
		select 'S'
		into w_estacopiado
		from tleg_operacioncalif
		where lca_fecha= p_fecha
		and lca_tipo = p_tipo
		and nvl(lca_esnuevo,'N') ='N'
		and lca_cuenta = prestamos.pre_credito;
		 w_estacopiado:='S';
	exception
	   when no_data_found then
	       w_estacopiado:='N';
	   when others then
	         w_estacopiado:='S';
	end;	*/
	    pkg_legal_dom_de.pro_gen_cuoextra(prestamos.pre_credito,p_fecha,w_MONTOCUOTAEXTRAOR,w_fcuoextra);
    --w_TIPOTASA:=
    w_TIPOTASA:='V';--rosemary 22-mayo 2015

    -- w_ESTRUCTURACIONCRE
       if w_reestructurado  = 'S' then
          w_ESTRUCTURACIONCRE:= 'RN';
       else
          w_ESTRUCTURACIONCRE:= 'NR';
        end if;
		select count(*)
		   into w_represtamo
		from tpre_inccapital
		where inc_status = 2
		  and inc_concepto= 6
		  and inc_credito = prestamos.pre_credito
		  and trunc(inc_fechaaut) <= p_fecha;
          if  w_represtamo > 0 then
             w_ESTRUCTURACIONCRE:= 'RP';
          end if;
      --w_ORIGENCREDITO
        w_ORIGENCREDITO:=pkg_legal_Dom_de.fun_iso_desctabla(700,3);
      --w_TIPORECURSO
         w_TIPORECURSO:=pkg_legal_Dom_de.fun_iso_desctabla(69,1);
    --fin cambios junio 2015
    --fin para saber si ya lo copio
      if PRESTAMOS.fecha_Emision<=p_fecha then
      begin
     -- IF w_diasvencontrato = 0 THEN      --se elimina Rosemary indica que los comercio exterior vencidos deben aparecer ya que se estan reportando en los analiticos
         --IF w_estacopiado='N' THEN
         UPDATE  tleg_operacioncalif
           SET lca_interes = w_interessob,
               lca_montogaradmi = w_montogar_admisible,
               lca_montogarreal = w_montogar_real,
               lca_montogaradmifisa = w_montogar_admisiblefisa,
               lca_montopignorado = w_montopignorado,
               lca_contingente = w_contingencia,
               LCA_TIPOTASA=w_TIPOTASA,
               LCA_ESTRUCTURACIONCRE=w_ESTRUCTURACIONCRE,
               LCA_ORIGENCREDITO=w_ORIGENCREDITO,
               LCA_TIPORECURSO=w_TIPORECURSO
           WHERE  lca_fecha= p_fecha
		and lca_tipo = p_tipo
		and lca_cuenta = prestamos.pre_credito;
		dbms_output.put_line(prestamos.pre_credito);
		IF SQL%NOTFOUND THEN
		 --if prestamos.pre_credito <> 1000000543  then
        insert into tleg_operacioncalif ( lca_fecha,lca_tipo,lca_codcli,lca_mod,lca_cuenta,lca_prod,lca_tip,
										  lca_mon,lca_gar,lca_riesgopais,lca_reestructurado,lca_demandajundicial,
										  lca_capital,lca_interes,lca_montogaradmi,lca_fecreestruc,lca_diasreestruc,
										  lca_fecrenov,lca_codigopais,lca_diasmoracap,lca_diasmoraint,lca_tabtipocredito,
								          lca_tipocredito,lca_codtipocredito,lca_status,lca_procesado,lca_montogarreal,
								          lca_suc,lca_ofi,lca_capitalcub,lca_capitalexp,lca_desobregiro,lca_montogaradmifisa,lca_esnuevo,
								          lca_montopignorado, lca_nrorestruct, lca_montorestruc, lca_fecultrenovtasa,lca_nrorenovtasa,
								          lca_fecultrenovplazo, lca_nrorenovplazo,lca_ejecutivo, lca_grupoeconomico, lca_fecultcal, lca_destinoeconomico,
								          lca_destinofondos, lca_fechainsgar,
								          lca_fechavenpolizas, lca_fechainglegal, lca_fechasalilegal,lca_numcomprom,lca_ciiu,lca_tipoamortiza,
								          lca_fechasalreest, lca_reescuotadesd,lca_interescub,lca_interesexp,lca_tipogarantia,lca_diasvencontrato,
								          lca_contingente,lca_consolidasistema,
								          LCA_MONTOCUOTAEXTRAOR,LCA_TIPOTASA,LCA_BALPROMDCAPMES,LCA_INTDEVENCORTE,LCA_COMCARGODEVCORTE,LCA_ESTRUCTURACIONCRE,
                          				  LCA_ORIGENCREDITO,LCA_TIPORECURSO)
        values (p_fecha,p_tipo,prestamos.pre_clientep, prestamos.pre_mod,prestamos.pre_credito,prestamos.pre_pro,prestamos.pre_tip,
										  prestamos.pre_moneda,w_gar,w_riesgopais,w_reestructurado,w_demandajundicial,
										  0,w_interessob,w_montogar_admisible,w_fecreestruc,nvl(w_diasmora_rees,0),
										  w_fecrenov,w_codigopais,nvl(w_diasv,0),nvl(w_diasint,0),244,
								          w_tipocredito,w_codtipocredito,prestamos.pre_status,'N',w_montogar_real,
								          prestamos.pre_sucursal,prestamos.pre_oficina,w_montocub,w_montoexp,w_desobregiro,w_montogar_admisiblefisa,
								          w_esnuevo,
								          w_montopignorado, w_nrorestruct, w_montorestruc, w_fecultrenovtasa,w_nrorenovtasa,
								          w_fecultrenovplazo,w_nrorenovplazo,w_ejecutivo,w_grupoeconomico,w_fecultcal,w_destinoeconomico,
								          w_destinofondos,w_fechainsgar,
								          w_fechavenpolizas,w_fechainglegal,w_fechasalilegal,w_numcomprom,w_ciiu,w_tipoamortizacion,
								          w_fechasalreest,w_reescuotadesd,w_montocub_int,w_montoexp_int,w_tipogarantia,w_diasvencontratodos,
								          w_contingencia,w_saldoconsolidado,
								          w_MONTOCUOTAEXTRAOR,w_TIPOTASA,w_BALPROMDCAPMES,w_INTDEVENCORTE,w_COMCARGODEVCORTE,w_ESTRUCTURACIONCRE,
								          w_ORIGENCREDITO,w_TIPORECURSO) ;
       --end if;
        END IF;
     --END IF;
      exception
        when others then
         rollback;
          raise_application_error (-20505,'Error al insertar datos de Mód: ' || prestamos.pre_mod || ', Cuenta: ' ||
                                   prestamos.pre_credito || ' ' || substr(1,120,sqlerrm));
      end;
      end if;
      commit;
    end loop;
     -- para vista sobregiros contratados
    --admisibilidad_garantias (p_fecha,p_tipo,v_nromeses);
    --upd_prorrateo_gar(p_fecha,p_tipo);
    commit;
  end;
  --procedure datos_ini_mod4 ( p_fecha date, p_tipo varchar2 );
--fin 10
  --procedure datos_ini_mod4 ( p_fecha date, p_tipo varchar2 );
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- upd datos Iniciales
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure upd_datos_ini_mod6 ( p_operador number, p_fecha date, p_tipo varchar2 ) is
 cursor p is
 select lca_mod,lca_cuenta,lca_mon pre_moneda,lca_suc pre_sucursal,
        lca_capital acr_capital
  from tleg_operacioncalif
  where lca_fecha = p_fecha
   and lca_tipo = p_tipo;
    w_montopignorado                       number(18,2);
	w_nrorestruct                          number(18,2);
	w_montorestruc                         number(18,2);
	w_fecultrenovtasa                      date;
	w_nrorenovtasa                         number;
	w_fecultrenovplazo                     date;
	w_nrorenovplazo                        number;
	w_ejecutivo                            varchar2(30);
	w_grupoeconomico                       varchar2(100);
	w_fecultcal                            date;
	w_destinoeconomico                     varchar2(100);
	w_destinofondos                        varchar2(100);
	w_fechainsgar                          date;
	w_fechavenpolizas                      date;
	w_fechainglegal                        date;
	w_fechasalilegal                       date;
	w_numcomprom                           number(10);
	w_ciiu                                 varchar2(6);
	w_tipoamortizacion                     varchar2(100);
	w_prestamos 						   tpre_prestamos%rowtype;
	w_fechasalreest  date;
	w_reescuotadesd  number;
    w_montogar_admisible_dif number:=0;
    w_montocub_int number:= 0;
   w_montoexp_int number:= 0;
  begin
   for x in p loop
      select *
        into w_prestamos
        from tpre_prestamos
       where pre_credito =  x.lca_cuenta;
	--valor pignorado
           select sum(pig_valor)
             into w_montopignorado
             from tcli_pignora
            where pig_ctaorg = w_prestamos.pre_credito
              and pig_fechalev is null;
	--nro de reestructuraciones
	--monto restructurado
	reestruturacion (w_prestamos.pre_credito,p_fecha,w_montorestruc,w_nrorestruct,w_fechasalreest,w_reescuotadesd);

	--ultima renovacion de tasa
	--numero de renovacion de tasa
	--ultima renovacion de tasa
    renovacion_tasa (w_prestamos.pre_credito,p_fecha,w_fecultrenovtasa,w_nrorenovtasa);
    --renovacion_plazo(w_prestamos.pre_credito,p_fecha,w_fecultrenovplazo,w_nrorenovplazo);
	--numero de renovacion de tasa

	--ejecutivo
	select emp_nombre
  	  into w_ejecutivo
      from tgen_usuario,tgen_empleado
      where usr_codemp = emp_codigo
        and usr_codigo = w_prestamos.pre_codeje;

	--grupo economico
   begin
	select des_descripcion
   	 into w_grupoeconomico
   	  from tgen_desctabla,tcli_persona
   	  where des_codtab = cli_tabgrupo
   	    and des_codigo = cli_grupoeco
   	    and cli_codigo = w_prestamos.pre_clientep;
   exception
    when others then
      w_grupoeconomico:=0;
   end;
	--ultima calificacion
	begin
     select decode(to_char(p_fecha,'mm'),'01',last_day(add_months(p_fecha,-1)),'02',last_day(add_months(p_fecha,-2)),last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd')))--last_day(to_date(MAX(pda_anio)||lpad(MAX(pda_mes),2,'0')||'01','yyyymmdd'))
       into w_fecultcal
      from tpre_prevdefantparam
     where pda_anio = to_number(to_char(P_FECHA,'yyyy'))
       and pda_mes <= to_number(to_char(P_FECHA,'mm'))
  	   and pda_nromeses = 3;
   exception
    when others then
      w_fecultcal:=null;
   end;
	--destino economico,ciiu
        BEGIN
          select des_nombre,substr(DES_CODIGOCIIU, 1, 4)
            Into w_destinoeconomico,w_ciiu
            from tpre_destecon
           where des_codigo = w_prestamos.pre_destecon;
        Exception
          When others then
            w_ciiu := null;
       end;

	--destino de fondos
	begin
	 select fpa_cuentades||' '||fpa_beneficia
	   into w_destinofondos
	 from tcaj_forpago
	  where fpa_cuentaorg = w_prestamos.pre_credito
       and fpa_numtran is not null
       and rownum = 1;
        Exception
          When others then
            w_destinofondos := null;
       end;


	--fecha de incripcion de garantia
	--fecha de vencimiento poliza
        select max(ecb_vencepol),max(ogr_fecprot)
          into w_fechavenpolizas,w_fechainsgar
         from tpre_endosobien,tcli_opergaran
         where ecb_codcli(+) = ogr_cliente
           and ecb_numgar(+) = ogr_numgaran
           and ogr_operacion= w_prestamos.pre_credito;

	--fecha de ingreso a legal
    begin
	select prl_desde,prl_hasta
	  into w_fechainglegal,w_fechasalilegal
	 from tpre_presleg
	 where prl_credito = w_prestamos.pre_credito
	   and prl_desde = (select max(prl_desde)
				          from tpre_presleg
				         where prl_credito = w_prestamos.pre_credito
				           and prl_desde <= p_fecha);
	exception
	 when others then
     w_fechainglegal := null;
     w_fechasalilegal:= null;
	end;
	--fecha de salida de legal
	--numero de compromiso
	w_numcomprom :=w_prestamos.pre_numcomprom;
	--tipo de amortizacion
	select des_descripcion
   	  into w_tipoamortizacion
     from tgen_desctabla
     where des_codtab = w_prestamos.pre_tabtipocuo
       and des_codigo = w_prestamos.pre_tipocuo;
   update tleg_operacioncalif
      set 	lca_montopignorado =w_montopignorado,
			lca_nrorestruct = w_nrorestruct,
			lca_montorestruc = w_montorestruc,
			lca_fecultrenovtasa  = w_fecultrenovtasa,
			lca_nrorenovtasa  = w_nrorenovtasa,
			lca_nrorenovplazo = w_nrorenovplazo,
			lca_ejecutivo = w_ejecutivo,
			lca_grupoeconomico  = w_grupoeconomico,
			lca_fecultcal = w_fecultcal,
			lca_destinoeconomico = w_destinoeconomico,
			lca_destinofondos = w_destinofondos,
			lca_fechainsgar = w_fechainsgar,
			lca_fechavenpolizas = w_fechavenpolizas,
			lca_fechainglegal = w_fechainglegal,
			lca_fechasalilegal = w_fechasalilegal,
			lca_numcomprom  = w_numcomprom,
			lca_ciiu  = w_ciiu,
			lca_tipoamortiza = w_tipoamortizacion,
			lca_fecultrenovplazo = w_fecultrenovplazo
   where  lca_fecha = p_fecha
     and lca_tipo = p_tipo
     and lca_mod = x.lca_mod
     and lca_cuenta =  x.lca_cuenta;
     commit;
   end loop;
  end;

  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- datos Iniciales
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  procedure obtener_datos ( p_operador number, p_fecha date, p_tipo varchar2 ) is

  w_msg varchar2(200);
  w_msgadic varchar2(200);
  w_comex_vale boolean;
  begin
  	for rg in (select mcl_mod
               from tpre_modcalif
               where mcl_hasta is null)
  	loop
			if rg.mcl_mod = 6 then
    		 -- COLOCACIONES
    		 pkg_precalifica_do.datos_ini_mod6 ( p_operador, p_fecha, p_tipo ); ----DESCOMENTAR OLL
    		 --null;
			elsif rg.mcl_mod = 4 then
    		 -- SOBREGIROS
    		  pkg_precalifica_do.datos_ini_mod4 ( p_operador, p_fecha, p_tipo );----DESCOMENTAR OLL
    		 null;
			elsif rg.mcl_mod = 8 then
    		 -- TARJETAS
    		 --PckCcardCalif.PgCalifica ( p_fecha, p_tipo );
    		 null;
			elsif rg.mcl_mod = 10 then
    		 -- COMEX
    		 --w_comex_vale := pkg_scoreloan.exec_loan ( p_fecha, p_tipo, w_msg, w_msgadic );
    		 --if not w_comex_vale then
      			--raise_application_error (-20511,'Error carga Comex: ' || pkg_process.get_message( p_operador, w_msg ) || ' ' ||w_msgadic);
    		 --end if;
    		 pkg_precalifica_do.datos_ini_mod10 ( p_operador, p_fecha, p_tipo );
    		 null;
    	end if;
    end loop;
    recalcula_tipoCredito(p_fecha,p_tipo);
    if p_fecha = last_day(p_fecha) then--los demas dias no debe calcular esto solos los fines de mes
      calificacion_cartera_mora(p_fecha,p_tipo);--nov 2017
    busca_nuevos_medianos_deudores(p_fecha,p_tipo);--nov 2017
    end if;

    
    commit;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Función para devolver el monto total de los créditos por tipo de crédito y por cliente
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  function monto_total_original_tipoc ( p_tipocredito number, p_cliente number, p_fecha date, p_tipo varchar2,
                                        p_error in out varchar2 ) return number is
  begin
   null;
  end;
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  -- Calificacion de la cartera por moa
  -- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  function calif_mora(p_fecha date,p_tipo varchar2,p_codcli number,p_mod number,p_cuenta number,
  										p_error in out varchar2) return number is
   begin
    null;
   end;

 procedure busca_nuevos_mayores_deudores(p_fecha in date,p_tipo in varchar2) is
    w_codtipocredito VARCHAR2(1):= NULL;
    w_saldoconsolidado number:=0;
 begin
     for prestamos in ( select pre_clientep, pre_mod, pre_credito, pre_pro, pre_tip, pre_monto, pre_fecven,
                              pre_tabtipocredito, pre_tipocredito, pre_moneda, pre_sucursal, pre_oficina,
                              decode(nvl(acr_otrostatus,'-'),'-',acr_status,acr_otrostatus) pre_status,
                              sta_descripcion, pre_fecemi, pre_tipotasa, pre_diasbase, pre_basemes, des_codigociiu,
                              acr_diasint, acr_diasv, acr_otrostatus,ptc_tipocredito,
                              (nvl(acr_capred,0) + nvl(acr_capnogi,0) + nvl(acr_capvencido,0))*bdi_promedio(pre_moneda,pre_sucursal,p_fecha) acr_capital,
							  DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'-',acr_intacum,0))) + decode(nvl(acr_otrostatus,'-'),'R',acr_intacum,0) + DECODE(NVL(PRE_MODVENCORG,0),4,0,(decode(nvl(acr_otrostatus,'-'),'J',0,acr_intactven)))* bdi_promedio(pre_moneda,pre_sucursal,p_fecha) acr_interes,
							  pre_numdir,pre_modvencorg,pre_ctavencorg
                       from tpre_prestamos,tpre_accrual ,tgen_status, tpre_destecon,tpre_protipocredito
                       where pre_credito = acr_credito
                       and acr_fecha = p_fecha
				       and ptc_mod = pre_mod
                       and ptc_pro = pre_pro
                       and ptc_tip = pre_tip
                       and ptc_mon = pre_moneda
                       and pre_mod = sta_mod
                       and pre_status = sta_codigo
                       and pre_destecon = des_codigo
                       and pre_clientep not in (select lca_codcli
                                                 from tleg_operacioncalif
                                                where lca_fecha= (select max(lca_fecha)
                                                			        from tleg_operacioncalif
                                                			        where lca_fecha<=p_fecha
                                                			          and lca_tipo = p_tipo
                                                			          and lca_codtipocredito = 'M' )
                                                  and lca_tipo = p_tipo
                                                  and lca_codtipocredito = 'M' ))
                       --and nvl(pre_tipocredito,0) in (select distinct par_tipocredito from tpre_paramcaliftipo)  --JAV hd2875
    loop
     w_codtipocredito := NULL;
      w_codtipocredito := substr(tipo_credito(prestamos.pre_clientep,prestamos.ptc_tipocredito,p_fecha,prestamos.acr_capital + prestamos.acr_interes,w_saldoconsolidado),1,1);
      if w_codtipocredito = 'M' then
        begin
        insert into tleg_clicalifcapacidad(cca_fecha,cca_tipo,cca_codcli,CCA_TABCALIFICA,CCA_CALIFICA)
         values(p_fecha,p_tipo,prestamos.pre_clientep,245,1);
        exception
          when dup_val_on_index then
            null;
          when others then
         rollback;
          raise_application_error (-20505,'Error al insertar Capacidad de Pago cli: ' || prestamos.pre_clientep || ', Cuenta: ' ||
                                   prestamos.pre_credito || ' ' || substr(1,120,sqlerrm));
        end;
      end if;
   end loop;
    commit;
 end;                     
---------------medianos deudores-------------------------
procedure busca_nuevos_medianos_deudores(p_fecha in date,p_tipo in varchar2) is
    w_codtipocredito VARCHAR2(1):= NULL;
    w_montoconsolida number;
  cursor p is
select cdt_codcli lca_codcli ,max(cdt_calificacion) calificacion
from tpre_califdettipos,tleg_operacioncalif 
where LCA_FECHA=cdt_fecha
  and LCA_TIPO= cdt_tipo
  and LCA_CODCLI = cdt_codcli
  and LCA_MOD = cdt_mod 
  and LCA_CUENTA = cdt_cuenta
  and cdt_tipocalif = 1
  and cdt_fecha = p_fecha
  and cdt_tipo = 'P'
  and lca_codtipocredito = 'D'
group by cdt_codcli ;
   w_diasmora number:=0; 
   w_califmora number:=0; 
 begin
     for prestamos in p loop
     w_codtipocredito := NULL;
     w_montoconsolida:=0;
      --w_codtipocredito := substr(tipo_credito(prestamos.pre_clientep,prestamos.ptc_tipocredito,p_fecha,w_montoconsolida),1,1);           
        begin
        insert into TLEG_CLICALIFRIESGO_MEDIANO(CCA_FECHA,CCA_TIPO,CCA_CODCLI,CCA_TABCALIFICA,CCA_CALIFICA,CCA_PORCENTAJEPERD,CCA_CALIFICAPERDIDA)
         values(p_fecha,p_tipo,prestamos.lca_codcli,245,prestamos.calificacion,null,null);
        exception
          when dup_val_on_index then
            null;
          when others then
         rollback;
          raise_application_error (-20505,'Error al insertar Capacidad de Pago cli: ' || prestamos.lca_codcli || ', Cuenta: ' ||
                                   prestamos.lca_codcli || ' ' || substr(1,120,sqlerrm));
        end;
   end loop;
    commit;
 end;

 
procedure califica_ope_new(p_fecha in date) is
begin
 null;
end;
procedure renovacion_tasa (p_credito in number,p_fecha in date,p_fechaArr out date,p_NroArreglos out number) is
--
v_contatasa         number;
v_maxfechatasa  	  date;
--
begin
  select count(*) into v_contatasa
  from tpre_tasaantreaj
  where rea_credito = p_credito
    and rea_desde  <= p_fecha;
--
  select max(rea_desde) into v_maxfechatasa
  from tpre_tasaantreaj
  where rea_credito = p_credito
    and rea_desde  <= p_fecha;
--
    p_nroarreglos   := nvl(v_contatasa,0);
  	p_fechaarr      := v_maxfechatasa;
--
exception
	when others then
	     p_nroarreglos   := 0;
  	   p_fechaarr      := null;
end;
procedure reestruturacion (p_credito in number,p_fecha in date,p_montoArr out number,p_NroArreglos out number,p_fechahasta out date,p_cuotadesde out number) is
  w_calificacionfija number:=0;
begin
           select count(ccf_califica)
             into w_calificacionfija
             from tleg_calificafija
            where ccf_cuenta = p_credito
              and ccf_tipocalifica = 2;
if nvl(w_calificacionfija,0) > 0 then
   p_NroArreglos:=1;
   p_montoArr:=0;
   p_fechahasta:=to_date('2006/06/01','yyyy/mm/dd'); --empezo el cambio contable
   p_cuotadesde:=1;
else
    select count(*)
      into p_NroArreglos
      from tpre_arrprestamos a
     where a.prp_credito = p_credito
       and a.prp_afectacalif = 'S'
       and nvl(a.prp_fechaarr,p_fecha+1) <= p_Fecha ;

    select sum(prp_monto)
      into p_montoArr
      from tpre_arrprestamos a
     where a.prp_credito = p_credito
       and a.prp_afectacalif = 'S'
       and nvl(a.prp_fechaarr,p_fecha+1) <= p_Fecha;

   select max(prp_fechaarrhasta),max(prp_arrcuodesde)
     into p_fechahasta,p_cuotadesde
     from tpre_arrprestamos
    where prp_credito = p_credito
      and nvl(prp_afectacalif,'N') = 'S'
      and prp_fechaarr = (   select max(prp_fechaarr)
						     from tpre_arrprestamos
						    where prp_credito = p_credito
						      and nvl(prp_afectacalif,'N') = 'S'
						      and prp_fecarr <= p_fecha)
      and prp_fecarr <= p_fecha;
end if;
end;
--parametros de calificacion , para quietar varios codigos hardcode
function parametro_calif( p_variable in varchar2 ) RETURN VARCHAR2 is
w_valor varchar2(30):=null;
begin
  select dpi_valor
    into w_valor
   from tdom_parinst_calif
   where dpi_codvar = p_variable;
   return w_valor;
exception
 when others then
 return null;
end; 
--
--Calcula_CalificaPerdida Medianos Deudores
--
function Calcula_CalificaPerdida(p_califmora in number, p_perdida in number) return number is--nov 2017
v_rango number:=0;
v_calif number:=0;
begin         
begin
SELECT rde_seq
  into v_rango
FROM TGEN_REPRANDET 
WHERE RDE_CODIGO = 13
 and p_perdida between rde_inicio and rde_final ;
 exception 
 when others then
    v_rango:=0;
 end;
 if v_rango = 1 then
    begin
	 select csc_calificaran1
	   into v_calif
	 from TLEG_CALIFRIEMEDIANOS 
	 where csc_calificamora = p_califmora;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif;
 elsif v_rango = 2 then
    begin
	 select csc_calificaran2
	   into v_calif
	 from TLEG_CALIFRIEMEDIANOS 
	 where csc_calificamora = p_califmora;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif;
 elsif v_rango = 3 then
    begin
	 select csc_calificaran3
	   into v_calif
	 from TLEG_CALIFRIEMEDIANOS 
	 where csc_calificamora = p_califmora;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif;
 elsif v_rango = 4 then
    begin
	 select csc_calificaran4
	   into v_calif
	 from TLEG_CALIFRIEMEDIANOS 
	 where csc_calificamora = p_califmora;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif; 
  elsif v_rango = 0 then
   return  p_califmora;  
  end if;     
end;
--
--Calcula_CalificaRiesgoMayores Mayores Deudores
--
function Calcula_CalificaRiesgoMayores( p_califmora in number, p_califcapacidad in number) return number is--nov 2017
v_rango number:=0;
v_calif number:=0;
begin         
 if p_califmora = 1 then
    begin
	SELECT csc_calmora_a
      INTO v_calif
      FROM TLEG_CALIFRIEMAYORES 
     WHERE csc_calificacappag = p_califcapacidad;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif;
 elsif p_califmora = 2 then
    begin
	SELECT csc_calmora_b
      INTO v_calif
      FROM TLEG_CALIFRIEMAYORES 
     WHERE csc_calificacappag = p_califcapacidad;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif;
 elsif p_califmora = 3 then
    begin
	SELECT csc_calmora_c
      INTO v_calif
      FROM TLEG_CALIFRIEMAYORES 
     WHERE csc_calificacappag = p_califcapacidad;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif;
 elsif p_califmora = 4 then
    begin
	SELECT csc_calmora_d1
      INTO v_calif
      FROM TLEG_CALIFRIEMAYORES 
     WHERE csc_calificacappag = p_califcapacidad;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif;
  elsif p_califmora = 6 then
    begin
	SELECT csc_calmora_d2
      INTO v_calif
      FROM TLEG_CALIFRIEMAYORES 
     WHERE csc_calificacappag = p_califcapacidad;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif;
  elsif p_califmora = 5 then
    begin
	SELECT csc_calmora_e
      INTO v_calif
      FROM TLEG_CALIFRIEMAYORES 
     WHERE csc_calificacappag = p_califcapacidad;
	 exception
	   when others then
	    v_calif:=0;
     end;          
     return v_calif;
  end if;     
end;
--feb 2018
function fdom_condicion(p_fecha in date,p_credito in number, p_cliente in number, p_moracap in number ,p_moraint in number,p_endemanda in varchar2) return number IS
  V_RESCOND2 NUMBER:=0;
  V_ESTADO NUMBER:=0;
  p_moramayor number:=0;
  v_enejecuta number:=0;
begin                               
   --Para ver reestructurados
      select count(*)
        into V_RESCOND2
        FROM TLEG_OPERACIONCALIF
        WHERE LCA_FECHA = P_FECHA
          AND LCA_CODCLI = P_CLIENTE
          AND LCA_REESTRUCTURADO = 'S'
          AND (LCA_DIASMORACAP < 365  OR LCA_DIASMORAINT < 365);
   -- USAR ESTADOS FINANCIEROS DEL CLIENTE 2/2
      SELECT COUNT(*)            
        INTO V_ESTADO
        FROM TCLI_ESTADOSFINAN_YEAR
        WHERE ETY_CODCLI = P_CLIENTE
          AND ETY_AUDITADO = 'Q';
    if p_endemanda = 'S' then
		 SELECT count(*)
		   INTO v_enejecuta        
		FROM TPRE_PRESLEG  
		  where prl_credito = p_credito
		     and prl_hasta is null
		     and prl_status = 27;
    end if;
   if p_moracap >= p_moraint then
      p_moramayor:= p_moracap;
   else   
         p_moramayor:= p_moraint;
   end if;   
                  
  if (p_moramayor between 0 and 180) or (v_enejecuta > 0) then
      return 1;
  elsif (p_moramayor between 181 and 364) OR (V_RESCOND2 > 0) THEN
       RETURN 2;
  elsif (p_moramayor  >= 365 ) OR (V_ESTADO >= 0) THEN
       RETURN 3;  
  ELSE
    RETURN 1;
  END IF;         
end; 

end pkg_precalifica_do;
/
