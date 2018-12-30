CREATE OR REPLACE
package body pkg_capdom_cuadraturaDET is

--Reporte  modificado en hd8164

PROCEDURE Pro_totales
 (P_FEC DATE
 ,P_INI VARCHAR2
 ,P_FIN VARCHAR2
 ,P_SUCINI NUMBER
 ,P_SUCFIN NUMBER
 ,P_OPERADOR NUMBER
 ,P_MODRPT NUMBER
 ,P_TRARPT NUMBER
 ,P_SECRPT NUMBER
 ,P_ENTRPT NUMBER
 ) IS

CURSOR C1 IS
  SELECT  GDT_CUENTA,  GDT_MOD,  GDT_PRO,
            GDT_TIP,     GDT_MON,  GDT_CODGRU,
            GDT_PACKAGE
      FROM  TPRE_FUNCGROUPDET
      WHERE GDT_CUENTA >= P_INI
      AND   GDT_CUENTA <= P_FIN
      AND   GDT_MODRPT  = P_MODRPT
      AND   GDT_TRARPT  = P_TRARPT
      AND   GDT_ENTRPT  = P_ENTRPT
      AND   GDT_SECRPT  = P_SECRPT;

  -- Cursos monedas para módulo de Plazo
  CURSOR cMONEDA IS
    SELECT DISTINCT TRT_MON
    FROM   tgen_trantipo
    WHERE  TRT_MOD=4;

  W_SQL               VARCHAR2(2500);
  W_COL               TPRE_COLGROUPS.CGP_COLUMNA%type;
  W_PASO              NUMBER:=0;
  vActivaHitsAccrual  BOOLEAN := false;  -- Indica sí ya se verificó que Hits está activo para p_fec pasado como parámetro
  val_intreinv        NUMBER := 0;       -- interes vigente para reinvertidos ALL HD3155_6
  int_venreinv        NUMBER := 0;       -- interes vencido para reinvertido ALL HD3155_6
  vln_numdec          NUMBER;            --ALL HD3155_6 2004/05/20
  vlc_negociable      VARCHAR2(1);       --ALL HD3155_6 2004/05/20

   P_MOD number;
   P_PRO number;
   P_TIP number;
   P_MON number;
   P_GRU number;
   P_OPE number;
   P_CTA varchar2(25);
   w_bloqueadoembargo number:=0;
   w_bloqueadoembargoaux number:=0;
BEGIN
W_PASO := 1;
    --Borra tabla temporal por operador
    DELETE  TAUX_FUNCGROUPDET_DET
    WHERE   GDT_OPERADOR = P_OPERADOR;
    commit;         

W_PASO := 2;
    -- CARGA TABLA AUXILIAR PARA CALCULO INTERES ACUMULADO PARA PLAZO
    --Obtiene las cuentas parametrizadas para el reporte
    --Inserta la tabla auxiliar
    FOR R1 IN C1 loop

         P_MOD := R1.GDT_MOD;
         P_PRO := R1.GDT_PRO;
         P_TIP := R1.GDT_TIP;
         P_MON := R1.GDT_MON;
         P_GRU := R1.GDT_CODGRU;
         P_OPE := P_OPERADOR;
         P_CTA := R1.GDT_CUENTA;

        BEGIN
        	SELECT a.MON_NEGOCIABLE,a.MON_NUMDEC
          INTO   vlc_negociable,vln_numdec
          FROM   TGEN_MONEDA a
          WHERE  a.MON_COD  = P_MON;
        EXCEPTION
          WHEN no_data_found THEN
            raise_application_error(-20100,'Moneda no definida. mon:'||p_mon);
        END;

        IF vlc_negociable = 'N' THEN
          BEGIN
          	SELECT a.MON_NUMDEC
            INTO   vln_numdec
            FROM   TGEN_MONEDA a
            WHERE  a.MON_COD  = 0;
          EXCEPTION
            WHEN no_data_found THEN
              raise_application_error(-20100,'Moneda no definida. mon:0');
          END;
        END IF;
		W_PASO := 3;
          --Obtiene el nomnre de la columan parametrizado para la cuenta y el prodcuto
          --del reporte que se se solicita.
          BEGIN
            SELECT    CGP_COLUMNA
              INTO    W_COL
              FROM    TPRE_FUNCGROUPDET, TPRE_COLGROUPS
              WHERE   CGP_CODIGO  = GDT_CODGRU
                AND   GDT_CUENTA  = R1.GDT_CUENTA
                AND   GDT_MOD     = R1.GDT_MOD
                AND   GDT_PRO     = R1.GDT_PRO
                AND   GDT_TIP     = R1.GDT_TIP
                AND   GDT_MON     = R1.GDT_MON
                AND   GDT_CODGRU  = R1.GDT_CODGRU
                AND   GDT_MODRPT  = P_MODRPT
        	      AND   GDT_TRARPT  = P_TRARPT
        	      AND   GDT_ENTRPT  = P_ENTRPT
        	      AND   GDT_SECRPT  = P_SECRPT
        	      AND CGP_MODULO  = 4;
          EXCEPTION
            when no_data_found then
          	     null;
            WHEN OTHERS THEN
                 RAISE_APPLICATION_ERROR (-20101,'Col.'  || R1.GDT_CODGRU ||
                                                 ' Cta.' || R1.GDT_CUENTA || ' - ' || sqlerrm);
          END;

		W_PASO := 4;
          -- Está verificación se debe hacer una sola vez.
          IF not vActivaHitsAccrual THEN
              declare
                v_desde	date;
                v_hasta	date;
                v_error	varchar2(200);
                v_ok		boolean;
              begin
                v_ok := pkghits.range('TCAP_ACRPAS',v_desde,v_hasta,v_error,p_fec);
                vActivaHitsAccrual := true; -- verificar sólo una vez sí hits está activo para la fecha pasada como parámetro
                if not v_ok then
                 	   raise_application_error(-20455,v_error);
                end if;
              end;
          END IF;

		W_PASO := 5;


          IF P_MOD = 4 and w_col is not null THEN  --VISTA
           w_bloqueadoembargo:=0;
            IF P_GRU IN (79) THEN
              --PARA RESTAR LOS VALORES BLOQUEADOS QUE VAN A OTRA CUENTA
            declare
                   causal varchar2(500);
                   causal1 varchar2(500);
                   posicion1 number;
                   posicion2 number;
                   posicion number;
                   numero number;

              begin

               select CGP_COLUMNA
               into causal
               from TPRE_COLGROUPS
               where CGP_MODULO = 4 and
                     CGP_CODIGO = 90;
               W_PASO:= 14.5;
               posicion1 := instr(causal,'(',1,2)+1;
               posicion2 :=  instr(causal,')',-1,1);
               causal1 := substr(causal,posicion1, posicion2-posicion1)|| ',';
               posicion := 0;


                while posicion <length(causal1) loop
                 posicion1 := posicion+1;


                 posicion := instr(causal1,',',posicion1,1);
                 numero := substr(causal1,posicion1,posicion-posicion1);



                   for cuenta in  (select distinct blq_numcue
                                     from tcli_bloqueos
                                     where blq_modulo = 4 and --blq_fechalev is null and
                                          to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))   and
                                           trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))  and
                                           ( to_date( to_char(P_FEC,'yyyy/mm/dd')) < nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)) and
                                     blq_causa = numero) loop

           --   insert into log values('numero ' || numero || ' cuenta.blq_numcue ' || cuenta.blq_numcue);
             --  commit;

                         begin
                         insert into TAUX_FUNCGROUPDET_DET
                                (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                                 GCT_TABCAT,    GCT_CATEGORIA,
                                 GCT_TABSUBTIP, GCT_SUBTIPO,
                                 GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                                 GDT_CODGRU,  GDT_OFICINA,  GDT_SALDOMOD,
                                 GDT_OPERACION )
                        select   P_OPE   ,    P_CTA      cuenta , acp_suc ,
                                950, cst_categoria,
                                168, pro_sector,
                                vis_mod , acp_pro , acp_tip ip, acp_mon ,
                                P_GRU   grupo, acp_ofi,
                                sum(blq_valorblq*-1),
                                acp_numcue
                         FROM   Tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo, tcli_bloqueos
                         WHERE  acp_numcue = vis_numcue
                         AND    acp_fecha = p_fec
                         AND    acp_suc between   P_SUCINI   and   P_SUCFIN
                         AND    acp_pro    =   P_PRO
                         AND    acp_tip    =   P_TIP
                         AND    acp_mon =   P_MON
                         AND    vis_numcue = pro_cue
                         AND    pro_sector  = cst_sector
                         and    vis_numcue   = cuenta.blq_numcue
                         and    acp_numcue   = blq_numcue
                         and    to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    (  to_date( to_char(P_FEC,'yyyy/mm/dd')) <  nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)  )
                         and    blq_modulo = 4
                         and    blq_causa = numero
                        group by  acp_suc, cst_categoria , pro_sector ,
                        vis_mod, acp_pro, acp_tip, acp_mon,acp_ofi,acp_numcue;
                        EXCEPTION
                          WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital bloqueado - ' || sqlerrm);
                        END;
                  end loop;
              end loop;
              end;
              --FIN DE OBTENER EL VALOR
              --muerte
            declare
                   causal varchar2(500);
                   causal1 varchar2(500);
                   posicion1 number;
                   posicion2 number;
                   posicion number;
                   numero number;

              begin

               select CGP_COLUMNA
               into causal
               from TPRE_COLGROUPS
               where CGP_MODULO = 4 and
                     CGP_CODIGO = 91;
               W_PASO:= 14.5;
               posicion1 := instr(causal,'(',1,2)+1;
               --posicion1 := instr(causal,'(',1)+1;
               W_PASO:= 14.51;
               posicion2 :=  instr(causal,')',-1,1);
               W_PASO:= 14.52;
               causal1 := substr(causal,posicion1, posicion2-posicion1)|| ',';
               W_PASO:= 14.53;
               posicion := 0;


                while posicion <length(causal1) loop
                 posicion1 := posicion+1;


                 posicion := instr(causal1,',',posicion1,1);
                                W_PASO:= 14.54;
                 numero := substr(causal1,posicion1,posicion-posicion1);
                                W_PASO:= 14.55;


                   for cuenta in  (select distinct blq_numcue
                                     from tcli_bloqueos
                                     where blq_modulo = 4 and --blq_fechalev is null and
                                          to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))   and
                                           trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))  and
                                           ( to_date( to_char(P_FEC,'yyyy/mm/dd')) < nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)) and
                                     blq_causa = numero) loop

           --   insert into log values('numero ' || numero || ' cuenta.blq_numcue ' || cuenta.blq_numcue);
             --  commit;

                         begin
                         insert into TAUX_FUNCGROUPDET_det
                                (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                                 GCT_TABCAT,    GCT_CATEGORIA,
                                 GCT_TABSUBTIP, GCT_SUBTIPO,
                                 GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                                 GDT_CODGRU,  GDT_OFICINA,  GDT_SALDOMOD,GDT_OPERACION )
                        select   P_OPE   ,    P_CTA      cuenta , acp_suc ,
                                950, cst_categoria,
                                168, pro_sector,
                                vis_mod , acp_pro , acp_tip ip, acp_mon ,
                                P_GRU   grupo, acp_ofi,
                                sum(blq_valorblq*-1),
                                acp_numcue
                         FROM   Tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo, tcli_bloqueos
                         WHERE  acp_numcue = vis_numcue
                         AND    acp_fecha = p_fec
                         AND    acp_suc between   P_SUCINI   and   P_SUCFIN
                         AND    acp_pro    =   P_PRO
                         AND    acp_tip    =   P_TIP
                         AND    acp_mon =   P_MON
                         AND    vis_numcue = pro_cue
                         AND    pro_sector  = cst_sector
                         and    vis_numcue   = cuenta.blq_numcue
                         and    acp_numcue   = blq_numcue
                         and    to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    (  to_date( to_char(P_FEC,'yyyy/mm/dd')) <  nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)  )
                         and    blq_modulo = 4
                         and    blq_causa = numero
                        group by  acp_suc, cst_categoria , pro_sector ,
                        vis_mod, acp_pro, acp_tip, acp_mon,acp_ofi,
                        acp_numcue;
                        EXCEPTION
                          WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital bloqueado - ' || sqlerrm);
                        END;
                  end loop;
              end loop;
              end;
              --FIN DE OBTENER EL VALOR              
              --fin 
              --restando los pingonaros
              declare
                   causal varchar2(500);
                   causal1 varchar2(500);
                   posicion1 number;
                   posicion2 number;
                   posicion number;
                   numero number;

              begin

W_PASO:= 16;
               select CGP_COLUMNA
               into causal
               from TPRE_COLGROUPS
               where CGP_MODULO = 4 and
                     CGP_CODIGO = 83;

               posicion1 := instr(causal,'(',1,2)+1;

               posicion2 :=  instr(causal,')',-1,1);

              causal1 := substr(causal,posicion1, posicion2-posicion1)|| ',';
              posicion := 0;

                while posicion <length(causal1) loop
                 posicion1 := posicion+1;

                 posicion := instr(causal1,',',posicion1,1);
                 numero := substr(causal1,posicion1,posicion-posicion1);

                   for cuenta in  (select distinct pig_ctapig
                                     from tcli_pignora
                                     where pig_modpig  = 4 and  --pig_fechalev  is null and
                                           to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(pig_fechapig) and nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))   and
                                           trunc(pig_fechapig) <> nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))  and
                                           to_date( to_char(P_FEC,'yyyy/mm/dd')) < (nvl(trunc(pig_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1))
                                          and pig_causa = numero ) loop
                         begin
                                 insert into TAUX_FUNCGROUPDET_det
                                          (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                                           GCT_TABCAT,    GCT_CATEGORIA,
                                           GCT_TABSUBTIP, GCT_SUBTIPO,
                                           GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                                           GDT_CODGRU,    GDT_OFICINA,GDT_SALDOMOD,GDT_OPERACION )
                                  select   P_OPE   ,    P_CTA      cuenta , acp_suc ,
                                           950, cst_categoria,
                                           168, pro_sector,
                                           vis_mod , acp_pro , acp_tip ip, acp_mon,
                                           P_GRU   grupo,acp_ofi,
                                           SUM(pig_valor*-1),
                                           acp_numcue
                                   FROM   Tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo,tcli_pignora
                                   WHERE  acp_numcue = vis_numcue
                                   AND     acp_fecha = p_fec
                                   AND    acp_suc between   P_SUCINI   and   P_SUCFIN
                                   AND    acp_pro      = P_PRO
                                   AND    acp_tip      = P_TIP
                                   AND    acp_mon   = P_MON
                                   AND    vis_numcue   = pro_cue
                                   AND    pro_sector   = cst_sector
                                   and    acp_numcue   = cuenta.pig_ctapig
                                   and    acp_numcue   = pig_ctapig
                                   and    pig_modpig = 4
                                   and    to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(pig_fechapig) and nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))
                                   and    trunc(pig_fechapig) <> nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))
                                   and    ( to_date( to_char(P_FEC,'yyyy/mm/dd')) < nvl(trunc(pig_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)  )
                                   and    pig_causa = numero
                                  group by  acp_suc, cst_categoria , pro_sector ,
                                  vis_mod, acp_pro, acp_tip, acp_mon,acp_ofi,
                                  acp_numcue;
                    EXCEPTION
                      WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital pignorado - '||sqlerrm);
                    END;
              end loop;
           end loop;
         end;
              ---fin
              BEGIN           -- selecciono el capital
              W_PASO := 6;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU, GDT_OFICINA,   GDT_SALDOMOD,GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo,acp_ofi,
                             nvl(SUM((decode(sign(NVL(ACP_SALEFE,0)),-1,F_ACUMSOB_VAR_2(ACP_NUMCUE,ACP_FECHA,(nvl(acp_salgar,0) +nvl(acp_salblq,0) +
                             nvl(acp_salrem,0) +NVL(ACP_SAL72H,0) + NVL(ACP_SAL60H,0) +NVL(ACP_SAL48H,0) + NVL(ACP_SAL36H,0) +
                             NVL(ACP_SAL24H,0) +NVL(ACP_SAL12H,0)),'C'),
                             nvl(acp_salgar,0) +nvl(acp_salblq,0) +nvl(acp_salrem,0) +NVL(ACP_SAL72H,0) + NVL(ACP_SAL60H,0) +
                             NVL(ACP_SAL48H,0) + NVL(ACP_SAL36H,0) + NVL(ACP_SAL24H,0) +
                            NVL(ACP_SAL12H,0) +NVL(ACP_SALEFE,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable))),0)  monto,
                            acp_numcue
                     FROM   tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                     AND decode(sign(NVL(ACP_SALEFE,0)),-1,F_ACUMSOB_VAR_2(ACP_NUMCUE,ACP_FECHA,(nvl(acp_salgar,0) +nvl(acp_salblq,0) +nvl(acp_salrem,0) +NVL(ACP_SAL72H,0) + NVL(ACP_SAL60H,0) +NVL(ACP_SAL48H,0) + NVL(ACP_SAL36H,0) + NVL(ACP_SAL24H,0) +NVL(ACP_SAL12H,0)),'C'),
                            nvl(acp_salgar,0) +nvl(acp_salblq,0) +nvl(acp_salrem,0) +NVL(ACP_SAL72H,0) + NVL(ACP_SAL60H,0) +
                            NVL(ACP_SAL48H,0) + NVL(ACP_SAL36H,0) + NVL(ACP_SAL24H,0) +
                            NVL(ACP_SAL12H,0) +NVL(ACP_SALEFE,0)) > 0
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon,acp_ofi,acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital total - ' ||sqlerrm);
              END;

            ELSIF P_GRU IN (81) THEN -- CAPITAL VIGENTE
              BEGIN           -- selecciono el capital
W_PASO := 7;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU, GDT_OFICINA,   GDT_SALDOMOD,GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo,acp_ofi,
                             nvl(SUM(acp_salefe*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                     --AND 	nvl(acp_salefe,0) > 0
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon,acp_ofi,
                    acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital efectivo - ' ||sqlerrm);
              END;

            ELSIF P_GRU IN (82) THEN -- BLOQUEADO por caulsal
            declare
                   causal varchar2(500);
                   causal1 varchar2(500);
                   posicion1 number;
                   posicion2 number;
                   posicion number;
                   numero number;

              begin

               select CGP_COLUMNA
               into causal
               from TPRE_COLGROUPS
               where CGP_MODULO = 4 and
                     CGP_CODIGO = 82;
               W_PASO:= 14.82;
               posicion1 := instr(causal,'(',1,2)+1;
               posicion2 :=  instr(causal,')',-1,1);
               causal1 := substr(causal,posicion1, posicion2-posicion1)|| ',';
               posicion := 0;


                while posicion <length(causal1) loop
                 posicion1 := posicion+1;


                 posicion := instr(causal1,',',posicion1,1);
                 numero := substr(causal1,posicion1,posicion-posicion1);



                   for cuenta in  (select distinct blq_numcue
                                     from tcli_bloqueos
                                     where blq_modulo = 4 and --blq_fechalev is null and
                                          to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))   and
                                           trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))  and
                                           ( to_date( to_char(P_FEC,'yyyy/mm/dd')) < nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)) and
                                     blq_causa = numero) loop

           --   insert into log values('numero ' || numero || ' cuenta.blq_numcue ' || cuenta.blq_numcue);
             --  commit;

                         begin
                         insert into TAUX_FUNCGROUPDET_det
                                (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                                 GCT_TABCAT,    GCT_CATEGORIA,
                                 GCT_TABSUBTIP, GCT_SUBTIPO,
                                 GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                                 GDT_CODGRU,    GDT_OFICINA,GDT_SALDOMOD,GDT_OPERACION )
                        select   P_OPE   ,    P_CTA      cuenta , acp_suc ,
                                950, cst_categoria,
                                168, pro_sector,
                                vis_mod , acp_pro , acp_tip ip, acp_mon ,
                                P_GRU   grupo,acp_ofi,
                                sum(blq_valorblq),
                                acp_numcue
                         FROM   Tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo, tcli_bloqueos
                         WHERE  acp_numcue = vis_numcue
                         AND    acp_fecha = p_fec
                         AND    acp_suc between   P_SUCINI   and   P_SUCFIN
                         AND    acp_pro    =   P_PRO
                         AND    acp_tip    =   P_TIP
                         AND    acp_mon =   P_MON
                         AND    vis_numcue = pro_cue
                         AND    pro_sector  = cst_sector
                         and    vis_numcue   = cuenta.blq_numcue
                         and    acp_numcue   = blq_numcue
                         and    to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    (  to_date( to_char(P_FEC,'yyyy/mm/dd')) <  nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)  )
                         and    blq_modulo = 4
                         and    blq_causa = numero
                        group by  acp_suc, cst_categoria , pro_sector ,
                        vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,
                        acp_numcue;
                        EXCEPTION
                          WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital bloqueado - ' || sqlerrm);
                        END;
                  end loop;
              end loop;
              end;
        		ELSIF P_GRU IN (83) THEN  --CAPITAL PIGNORADO causal

              declare
                   causal varchar2(500);
                   causal1 varchar2(500);
                   posicion1 number;
                   posicion2 number;
                   posicion number;
                   numero number;

              begin

W_PASO:= 8;
               select CGP_COLUMNA
               into causal
               from TPRE_COLGROUPS
               where CGP_MODULO = 4 and
                     CGP_CODIGO = 83;

               posicion1 := instr(causal,'(',1,2)+1;

               posicion2 :=  instr(causal,')',-1,1);

              causal1 := substr(causal,posicion1, posicion2-posicion1)|| ',';
              posicion := 0;

                while posicion <length(causal1) loop
                 posicion1 := posicion+1;

                 posicion := instr(causal1,',',posicion1,1);
                 numero := substr(causal1,posicion1,posicion-posicion1);

                   for cuenta in  (select distinct pig_ctapig
                                     from tcli_pignora
                                     where pig_modpig  = 4 and  --pig_fechalev  is null and
                                           to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(pig_fechapig) and nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))   and
                                           trunc(pig_fechapig) <> nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))  and
                                           to_date( to_char(P_FEC,'yyyy/mm/dd')) < (nvl(trunc(pig_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1))
                                          and pig_causa = numero ) loop
                         begin
                                 insert into TAUX_FUNCGROUPDET_det
                                          (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                                           GCT_TABCAT,    GCT_CATEGORIA,
                                           GCT_TABSUBTIP, GCT_SUBTIPO,
                                           GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                                           GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,GDT_OPERACION )
                                  select   P_OPE   ,    P_CTA      cuenta , acp_suc ,
                                           950, cst_categoria,
                                           168, pro_sector,
                                           vis_mod , acp_pro , acp_tip ip, acp_mon,
                                           P_GRU   grupo,acp_ofi,
                                           SUM(pig_valor),
                                           acp_numcue
                                   FROM   Tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo,tcli_pignora
                                   WHERE  acp_numcue = vis_numcue
                                   AND     acp_fecha = p_fec
                                   AND    acp_suc between   P_SUCINI   and   P_SUCFIN
                                   AND    acp_pro      = P_PRO
                                   AND    acp_tip      = P_TIP
                                   AND    acp_mon   = P_MON
                                   AND    vis_numcue   = pro_cue
                                   AND    pro_sector   = cst_sector
	                                   and    acp_numcue   = cuenta.pig_ctapig
                                   and    acp_numcue   = pig_ctapig
                                   and    pig_modpig = 4
                                   and    to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(pig_fechapig) and nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))
                                   and    trunc(pig_fechapig) <> nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))
                                   and    ( to_date( to_char(P_FEC,'yyyy/mm/dd')) < nvl(trunc(pig_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)  )
                                   and    pig_causa = numero
                                  group by  acp_suc, cst_categoria , pro_sector ,
                                  vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,acp_numcue;
                    EXCEPTION
                      WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital pignorado - '||sqlerrm);
                    END;
              end loop;
           end loop;
         end;
            ELSIF P_GRU IN (84) THEN  --CAPITAL LOCALES
              BEGIN           -- selecciono el capital
W_PASO := 9;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo, acp_ofi,
                             nvl(SUM((NVL(ACP_SAL12h,0)+NVL(ACP_SAL24h,0)+NVL(ACP_SAL36h,0)+NVL(ACP_SAL48h,0)+NVL(ACP_SAL60h,0)+NVL(ACP_SAL72h,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital retencion local - ' ||sqlerrm);
              END;

            ELSIF P_GRU IN (85) THEN --CAPITAL REMESAS
              BEGIN           -- selecciono el capital
W_PASO := 10;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo,acp_ofi,
                             nvl(SUM((NVL(ACP_SALREM,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,
                    acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital remesas - ' ||sqlerrm);
              END;
            ELSIF P_GRU IN (86) THEN -- CAPITAL SOBREGIRO UTILIZADO OCASIONAL
              BEGIN           -- selecciono el capital
W_PASO := 11;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo,acp_ofi,
                             nvl(SUM((NVL(ACA_UTILIZADO,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 1
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,acp_numcue ;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital sobregiro ocasional - ' ||sqlerrm);
              END;
            ELSIF P_GRU IN (87) THEN -- INTERES normal

              BEGIN           -- selecciono el capital
W_PASO := 12;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo,acp_ofi,
                             nvl(SUM(((NVL(trunc(ACP_INTACUMCONT,2),0)+NVL(trunc(ACP_AJUSTECAP,2),0))+0.00-nvl(int_capitalizado(p_fec,acp_numcue),0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,--ANTES ESTABA 0.01 PORQUE ? NI IDEA NO RECUERDO
                             acp_numcue
                     FROM   tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha between p_fec and p_fec + 0.99999
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo interes acumulado- ' ||sqlerrm);
              END;

            ELSIF P_GRU IN (88) THEN -- INTERESES OCASIONAL
              BEGIN           -- selecciono el capital
W_PASO := 13;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA,GDT_SALDOMOD,GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo,acp_ofi,
                             nvl(SUM((NVL(ACA_INTERES,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 1
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,
                    acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo interes sobregiro ocsasional - ' ||sqlerrm);
              END;
        ELSIF P_GRU IN (89) THEN  --interes contratado
              BEGIN           -- selecciono el capital
W_PASO := 14;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA,GDT_SALDOMOD,GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo, acp_ofi,
                             nvl(SUM((NVL(ACA_INTERES,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 2
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon,acp_ofi,
                    acp_numcue;
W_PASO := 14.0;                    
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo interes contratado - ' ||sqlerrm);
              END;
     ELSIF P_GRU IN (90) THEN  --CAPITAL BLOQUEADO por embargo
W_PASO := 14.1;     
            declare
                   causal varchar2(500);
                   causal1 varchar2(500);
                   posicion1 number;
                   posicion2 number;
                   posicion number;
                   numero number;

              begin

               select CGP_COLUMNA
               into causal
               from TPRE_COLGROUPS
               where CGP_MODULO = 4 and
                     CGP_CODIGO = 90;
               W_PASO:= 14.2;
               posicion1 := instr(causal,'(',1,2)+1;
               --posicion1 := instr(causal,'(',1)+1;
               posicion2 :=  instr(causal,')',-1,1);
               causal1 := substr(causal,posicion1, posicion2-posicion1)|| ',';
               posicion := 0;


                while posicion <length(causal1) loop
                 posicion1 := posicion+1;


                 posicion := instr(causal1,',',posicion1,1);
                 numero := substr(causal1,posicion1,posicion-posicion1);



                   for cuenta in  (select distinct blq_numcue
                                     from tcli_bloqueos
                                     where blq_modulo = 4 and --blq_fechalev is null and
                                          to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))   and
                                           trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))  and
                                           ( to_date( to_char(P_FEC,'yyyy/mm/dd')) < nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)) and
                                     blq_causa = numero) loop

           --   insert into log values('numero ' || numero || ' cuenta.blq_numcue ' || cuenta.blq_numcue);
             --  commit;

                         begin
                         insert into TAUX_FUNCGROUPDET_det
                                (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                                 GCT_TABCAT,    GCT_CATEGORIA,
                                 GCT_TABSUBTIP, GCT_SUBTIPO,
                                 GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                                 GDT_CODGRU,    GDT_OFICINA,GDT_SALDOMOD,GDT_OPERACION )
                        select   P_OPE   ,    P_CTA      cuenta , acp_suc ,
                                950, cst_categoria,
                                168, pro_sector,
                                vis_mod , acp_pro , acp_tip ip, acp_mon ,
                                P_GRU   grupo, acp_ofi,
                                sum(blq_valorblq),
                                acp_numcue
                         FROM   Tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo, tcli_bloqueos
                         WHERE  acp_numcue = vis_numcue
                         AND    acp_fecha = p_fec
                         AND    acp_suc between   P_SUCINI   and   P_SUCFIN
                         AND    acp_pro    =   P_PRO
                         AND    acp_tip    =   P_TIP
                         AND    acp_mon =   P_MON
                         AND    vis_numcue = pro_cue
                         AND    pro_sector  = cst_sector
                         and    vis_numcue   = cuenta.blq_numcue
                         and    acp_numcue   = blq_numcue
                         and    to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    (  to_date( to_char(P_FEC,'yyyy/mm/dd')) <  nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)  )
                         and    blq_modulo = 4
                         and    blq_causa = numero
                        group by  acp_suc, cst_categoria , pro_sector ,
                        vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,
                        acp_numcue;
                        EXCEPTION
                          WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital bloqueado - ' || sqlerrm);
                        END;
                  end loop;
              end loop;
              end;
       ELSIF P_GRU IN (91) THEN  --CAPITAL bloqueado por muerte del titula
       W_PASO := 14.3;
            declare
                   causal varchar2(500);
                   causal1 varchar2(500);
                   posicion1 number;
                   posicion2 number;
                   posicion number;
                   numero number;

              begin

               select CGP_COLUMNA
               into causal
               from TPRE_COLGROUPS
               where CGP_MODULO = 4 and
                     CGP_CODIGO = 91;
               W_PASO:= 14.91;
               posicion1 := instr(causal,'(',1,2)+1; --esto es cuando tiene nvl 21/9040 
               --posicion1 := instr(causal,'(',1)+1; --esto es sin nvl
               posicion2 :=  instr(causal,')',-1,1);
               causal1 := substr(causal,posicion1, posicion2-posicion1)|| ',';
               posicion := 0;


                while posicion <length(causal1) loop
                 posicion1 := posicion+1;


                 posicion := instr(causal1,',',posicion1,1);
                 numero := substr(causal1,posicion1,posicion-posicion1);



                   for cuenta in  (select distinct blq_numcue
                                     from tcli_bloqueos
                                     where blq_modulo = 4 and --blq_fechalev is null and
                                          to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))   and
                                           trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))  and
                                           ( to_date( to_char(P_FEC,'yyyy/mm/dd')) < nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)) and
                                     blq_causa = numero) loop

           --   insert into log values('numero ' || numero || ' cuenta.blq_numcue ' || cuenta.blq_numcue);
             --  commit;

                         begin
                         insert into TAUX_FUNCGROUPDET_det
                                (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                                 GCT_TABCAT,    GCT_CATEGORIA,
                                 GCT_TABSUBTIP, GCT_SUBTIPO,
                                 GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                                 GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,
                                 GDT_OPERACION )
                        select   P_OPE   ,    P_CTA      cuenta , acp_suc ,
                                950, cst_categoria,
                                168, pro_sector,
                                vis_mod , acp_pro , acp_tip ip, acp_mon ,
                                P_GRU   grupo, acp_ofi,
                                sum(blq_valorblq),
                                acp_numcue
                         FROM   Tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo, tcli_bloqueos
                         WHERE  acp_numcue = vis_numcue
                         AND    acp_fecha = p_fec
                         AND    acp_suc between   P_SUCINI   and   P_SUCFIN
                         AND    acp_pro    =   P_PRO
                         AND    acp_tip    =   P_TIP
                         AND    acp_mon =   P_MON
                         AND    vis_numcue = pro_cue
                         AND    pro_sector  = cst_sector
                         and    vis_numcue   = cuenta.blq_numcue
                         and    acp_numcue   = blq_numcue
                         and    to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(blq_fechblq) and nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    trunc(blq_fechblq) <> nvl(blq_fechalev ,to_date('21991231','yyyymmdd'))
                         and    (  to_date( to_char(P_FEC,'yyyy/mm/dd')) <  nvl(trunc(blq_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)  )
                         and    blq_modulo = 4
                         and    blq_causa = numero
                        group by  acp_suc, cst_categoria , pro_sector ,
                        vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,acp_numcue;
                        EXCEPTION
                          WHEN OTHERS THEN
                            RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital bloqueado muerte titular- ' || sqlerrm);
                        END;
                  end loop;
              end loop;
              end;
        ELSIF P_GRU IN (92) THEN  --CAPITAL PIGNORADO EMBARGO


              declare
                   causal varchar2(500);
                   causal1 varchar2(500);
                   posicion1 number;
                   posicion2 number;
                   posicion number;
                   numero number;

              begin

W_PASO:= 16;
               select CGP_COLUMNA
               into causal
               from TPRE_COLGROUPS
               where CGP_MODULO = 4 and
                     CGP_CODIGO = 92;

               posicion1 := instr(causal,'(',1,2)+1;
               --posicion1 := instr(causal,'(',1)+1;

               posicion2 :=  instr(causal,')',-1,1);

              causal1 := substr(causal,posicion1, posicion2-posicion1)|| ',';
              posicion := 0;

                while posicion <length(causal1) loop
                 posicion1 := posicion+1;

                 posicion := instr(causal1,',',posicion1,1);
                 numero := substr(causal1,posicion1,posicion-posicion1);

                   for cuenta in  (select distinct pig_ctapig
                                     from tcli_pignora
                                     where pig_modpig  = 4 and  --pig_fechalev  is null and
                                           to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(pig_fechapig) and nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))   and
                                           trunc(pig_fechapig) <> nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))  and
                                           to_date( to_char(P_FEC,'yyyy/mm/dd')) < (nvl(trunc(pig_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1))
                                          and pig_causa = numero ) loop
                         begin
                                 insert into TAUX_FUNCGROUPDET_det
                                          (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                                           GCT_TABCAT,    GCT_CATEGORIA,
                                           GCT_TABSUBTIP, GCT_SUBTIPO,
                                           GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                                           GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,GDT_OPERACION )
                                  select   P_OPE   ,    P_CTA      cuenta , acp_suc ,
                                           950, cst_categoria,
                                           168, pro_sector,
                                           vis_mod , acp_pro , acp_tip ip, acp_mon,
                                           P_GRU   grupo, acp_ofi,
                                           SUM(pig_valor),
                                           acp_numcue
                                   FROM   Tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo,tcli_pignora
                                   WHERE  acp_numcue = vis_numcue
                                   AND     acp_fecha = p_fec
                                   AND    acp_suc between   P_SUCINI   and   P_SUCFIN
                                   AND    acp_pro      = P_PRO
                                   AND    acp_tip      = P_TIP
                                   AND    acp_mon   = P_MON
                                   AND    vis_numcue   = pro_cue
                                   AND    pro_sector   = cst_sector
                                   and    acp_numcue   = cuenta.pig_ctapig
                                   and    acp_numcue   = pig_ctapig
                                   and    pig_modpig = 4
                                   and    to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(pig_fechapig) and nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))
                                   and    trunc(pig_fechapig) <> nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))
                                   and    ( to_date( to_char(P_FEC,'yyyy/mm/dd')) < nvl(trunc(pig_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)  )
                                   and    pig_causa = numero
                                  group by  acp_suc, cst_categoria , pro_sector ,
                                  vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,acp_numcue;
                    EXCEPTION
                      WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital pignorado - '||sqlerrm);
                    END;
              end loop;
           end loop;
         end;
        ELSIF P_GRU IN (93) THEN  --CAPITAL PIGNORADO CLIENTE FALLECIDO

              declare
                   causal varchar2(500);
                   causal1 varchar2(500);
                   posicion1 number;
                   posicion2 number;
                   posicion number;
                   numero number;

              begin

W_PASO:= 17;
               select CGP_COLUMNA
               into causal
               from TPRE_COLGROUPS
               where CGP_MODULO = 4 and
                     CGP_CODIGO = 93;

               posicion1 := instr(causal,'(',1,2)+1;
               --posicion1 := instr(causal,'(',1)+1;

               posicion2 :=  instr(causal,')',-1,1);

              causal1 := substr(causal,posicion1, posicion2-posicion1)|| ',';
              posicion := 0;

                while posicion <length(causal1) loop
                 posicion1 := posicion+1;

                 posicion := instr(causal1,',',posicion1,1);
                 numero := substr(causal1,posicion1,posicion-posicion1);

                   for cuenta in  (select distinct pig_ctapig
                                     from tcli_pignora
                                     where pig_modpig  = 4 and  --pig_fechalev  is null and
                                           to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(pig_fechapig) and nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))   and
                                           trunc(pig_fechapig) <> nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))  and
                                           to_date( to_char(P_FEC,'yyyy/mm/dd')) < (nvl(trunc(pig_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1))
                                          and pig_causa = numero ) loop
                         begin
                                 insert into TAUX_FUNCGROUPDET_det
                                          (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                                           GCT_TABCAT,    GCT_CATEGORIA,
                                           GCT_TABSUBTIP, GCT_SUBTIPO,
                                           GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                                           GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,GDT_OPERACION )
                                  select   P_OPE   ,    P_CTA      cuenta , acp_suc ,
                                           950, cst_categoria,
                                           168, pro_sector,
                                           vis_mod , acp_pro , acp_tip ip, acp_mon,
                                           P_GRU   grupo, acp_ofi,
                                           SUM(pig_valor),
                                           acp_numcue
                                   FROM   Tcap_acrpas, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo,tcli_pignora
                                   WHERE  acp_numcue = vis_numcue
                                   AND     acp_fecha = p_fec
                                   AND    acp_suc between   P_SUCINI   and   P_SUCFIN
                                   AND    acp_pro      = P_PRO
                                   AND    acp_tip      = P_TIP
                                   AND    acp_mon   = P_MON
                                   AND    vis_numcue   = pro_cue
                                   AND    pro_sector   = cst_sector
                                   and    acp_numcue   = cuenta.pig_ctapig
                                   and    acp_numcue   = pig_ctapig
                                   and    pig_modpig = 4
                                   and    to_date(to_char(P_FEC,'yyyy/mm/dd'))  between trunc(pig_fechapig) and nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))
                                   and    trunc(pig_fechapig) <> nvl(pig_fechalev ,to_date('21991231','yyyymmdd'))
                                   and    ( to_date( to_char(P_FEC,'yyyy/mm/dd')) < nvl(trunc(pig_fechalev),to_date( to_char(P_FEC,'yyyy/mm/dd'))+1)  )
                                   and    pig_causa = numero
                                  group by  acp_suc, cst_categoria , pro_sector ,
                                  vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,acp_numcue;
                    EXCEPTION
                      WHEN OTHERS THEN
                        RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital pignorado - '||sqlerrm);
                    END;
              end loop;
           end loop;
         end;
         ELSIF P_GRU IN (96) THEN  --CAPITAL UTILIZADO SOBREGIRO CONTRATADO
              BEGIN           -- selecciono el capital
W_PASO := 18;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA,GDT_SALDOMOD,GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo, acp_ofi,
                             nvl(SUM((NVL(ACA_UTILIZADO,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 2
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,
                    acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital sob. contratado - ' ||sqlerrm);
              END;
         ELSIF P_GRU IN (97) THEN  --CAPITAL UTILIZADO GIRADO LOCALES
              BEGIN           -- selecciono el capital
W_PASO := 19;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,
                             GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo, acp_ofi,
                             nvl(SUM((NVL(ACA_UTILIZADO,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 4
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,
                    acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo interes girado locales - ' ||sqlerrm);
              END;
         ELSIF P_GRU IN (98) THEN  --CAPITAL UTILIZADO GIRADO REMESAS
              BEGIN           -- selecciono el capital
W_PASO := 20;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,
                             GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo, acp_ofi,
                             nvl(SUM((NVL(ACA_UTILIZADO,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 3
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,
                    acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo int. utilizado remesas - ' ||sqlerrm);
              END;
         ELSIF P_GRU IN (99) THEN  --INTERES SOB. GIRADO LOCALES
              BEGIN           -- selecciono el capital
W_PASO := 21;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,
                             GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo, acp_ofi,
                             nvl(SUM((NVL(ACA_INTERES,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 4
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,
                    acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo interes girado locales - ' ||sqlerrm);
              END;
         ELSIF P_GRU IN (78) THEN  --INTERES SOB. GIRADO REMESAS
              BEGIN           -- selecciono el capital
W_PASO := 22;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,
                             GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo, acp_ofi,
                             nvl(SUM((NVL(ACA_INTERES,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 3
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon,acp_ofi,
                    acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo int. girado remsas - ' ||sqlerrm);
              END;
         ELSIF P_GRU IN (77) THEN  --INTERES SOBREGIRO INDIRECTO
              BEGIN           -- selecciono el capital
W_PASO :=23;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA, GDT_SALDOMOD,
                             GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo,acp_ofi,
                             nvl(SUM((NVL(ACA_INTERES,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 5
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,
                    acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo int. sob. indirecto - ' ||sqlerrm);
              END;
         ELSIF P_GRU IN (76) THEN  --CAPITAL UTILIZADO SOB. INDIRECTO
              BEGIN           -- selecciono el capital
W_PASO := 24;

                    insert into TAUX_FUNCGROUPDET_det
                            (GDT_OPERADOR,  GDT_CUENTA, GDT_SUCURSAL,
                             GCT_TABCAT,    GCT_CATEGORIA,
                             GCT_TABSUBTIP, GCT_SUBTIPO,
                             GDT_MOD,       GDT_PRO, GDT_TIP, GDT_MON,
                             GDT_CODGRU,    GDT_OFICINA,GDT_SALDOMOD,
                             GDT_OPERACION )
                    select   P_OPE, P_CTA cuenta , acp_suc ,
                             950, cst_categoria,
                             168, pro_sector,
                             vis_mod , acp_pro , acp_tip ip, acp_mon,
                             P_GRU grupo, acp_ofi,
                             nvl(SUM((NVL(ACA_UTILIZADO,0))*pkg_capdom_cuadratura.fun_cotizacion(p_fec,acp_suc,acp_mon,vlc_negociable)),0)  monto,
                             acp_numcue
                     FROM   tcap_acrpas,tcap_acract, tcap_vista, tgen_cuentasubtipo, tgen_categoriasubtipo
                     WHERE  acp_numcue = vis_numcue
                     AND    acp_fecha = p_fec
					 AND    aca_numcue = vis_numcue
					 AND    aca_fecha = acp_fecha
					 AND    aca_tipocre = 5
                     AND    acp_suc between  P_SUCINI  and  P_SUCFIN
                     AND    acp_pro     = P_PRO
                     AND    acp_tip     = P_TIP
                     AND    acp_mon  = P_MON
                     AND    vis_numcue  = pro_cue
                     AND    pro_sector  = cst_sector
                    group by  acp_suc, cst_categoria , pro_sector ,
                    vis_mod, acp_pro, acp_tip, acp_mon, acp_ofi,acp_numcue;
              EXCEPTION
                WHEN OTHERS THEN
                  RAISE_APPLICATION_ERROR (-20134,'Error obteniendo capital utilizado indirecto - ' ||sqlerrm);
              END;
         END IF;--FIN P_GRU
          END IF;
END LOOP; --C1
W_PASO := 98;
    --Elimina registros con saldo 0
    --DELETE  TAUX_FUNCGROUPDET_det
    --WHERE   GDT_OPERADOR = P_OPERADOR
    --AND     NVL(gdt_saldomod,0) = 0;
    commit;

W_PASO := 99;
EXCEPTION
  WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR (-20063,'RptCua.' || W_PASO  || ' ' ||sqlerrm);
END Pro_Totales;


FUNCTION fun_cotizacion(
  pdi_fecha           IN    DATE,
  pni_codsuc          IN   NUMBER,
  pni_moneda          IN   NUMBER,
  pci_negociable      IN   VARCHAR2
)RETURN NUMBER IS
  var1 VARCHAR2(5);
  cot NUMBER:=1;
  dFechaProContReser date;
  tipopdf number:=0;   --0 Vigente, 1 vencido, -1 vigente
  dFechaCot   date;
BEGIN
	dFechaCot  :=  pdi_fecha;
  if nvl(pci_negociable,'S')='N'  then
   cot:=pkg_pla_batch.fRetornaCotProm(pni_codsuc,pni_moneda, dFechaCot ,dFechaProContReser);
  end if;

  return cot;
end fun_cotizacion;
  FUNCTION int_capitalizado(
    pdi_fecha           IN    DATE,
    pni_numcue          IN   NUMBER
  ) RETURN NUMBER is
  p_val number:=0;
  begin
    select sum(nvl(tmo_val,0))
      into p_val
    from tcap_tramon
    where tmo_fechcon = pdi_fecha
     and tmo_numcue = pni_numcue
     and tmo_codmod = 4
     and tmo_codtra = 205
     and tmo_rubro = 1
     and tmo_modo = 'N';
     return p_val;
  end;
------------------------------
-- Valor de en contabilidad --
------------------------------
FUNCTION ValorContable(nFecha Date, nCuenta Varchar2) RETURN Number IS
vValor Number(18,2) := 0;
BEGIN
  SELECT Sum(decode(tsa_tipo,'D',TSA_VALOR,0) - decode(tsa_tipo,'C',TSA_VALOR,0)) DIF
    Into vValor
    FROM TCON_TRANSA,
         TCON_DESTRAN,
         tcon_cuentas
	 WHERE TSA_SUCURSAL = DST_SUCURSAL
	   AND TSA_NUMTRAN  = DST_NUMTRAN
	   AND tsa_cuenta   = ccb_codigo
	   AND TSA_CUENTA   = nCuenta --con_ctadeb
	   AND trunc(DST_FECHA)  <= To_date(nFecha,'yyyy/mm/dd');
	   --
	   Return vValor;
Exception When No_data_Found Then
	Return vValor;
END ValorContable;
------------------------------
-- Valor de en contabilidad --
------------------------------
FUNCTION ValorContableDia(nFecha Date, nCuenta Varchar2) RETURN Number IS
vValor Number(18,2) := 0;
BEGIN
  SELECT Sum(decode(tsa_tipo,'C',TSA_VALOR,0)) DIF
    Into vValor
    FROM TCON_TRANSA,
         TCON_DESTRAN,
         tcon_cuentas
	 WHERE TSA_SUCURSAL = DST_SUCURSAL
	   AND TSA_NUMTRAN  = DST_NUMTRAN
	   AND tsa_cuenta   = ccb_codigo
	   AND TSA_CUENTA   = nCuenta --con_ctadeb
	   AND trunc(DST_FECHA)  = To_date(nFecha,'yyyy/mm/dd');
	   --
	   Return vValor;
Exception When No_data_Found Then
	Return vValor;
END ValorContableDia;
------------------------------
-- Valor de en contabilidad --
------------------------------
FUNCTION ValorContableDiaDeb(nFecha Date, nCuenta Varchar2) RETURN Number IS
vValor Number(18,2) := 0;
BEGIN
  SELECT Sum(decode(tsa_tipo,'D',TSA_VALOR,0)) DIF
    Into vValor
    FROM TCON_TRANSA,
         TCON_DESTRAN,
         tcon_cuentas
	 WHERE TSA_SUCURSAL = DST_SUCURSAL
	   AND TSA_NUMTRAN  = DST_NUMTRAN
	   AND tsa_cuenta   = ccb_codigo
	   AND TSA_CUENTA   = nCuenta --con_ctadeb
	   AND trunc(DST_FECHA)  = To_date(nFecha,'yyyy/mm/dd');
	   --
	   Return vValor;
Exception When No_data_Found Then
	Return vValor;
END ValorContableDiaDeb;
end pkg_capdom_cuadraturaDET;
/
