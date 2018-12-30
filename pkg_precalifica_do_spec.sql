CREATE OR REPLACE
package pkg_precalifica_do is
  --procedure send_logcalifica ( p_fecha date, p_tipo varchar2, p_fechainicio date, p_fechareal date, p_texto varchar2 );
  --Reestructuraciones
  function fdom_condicion(p_fecha in date,p_credito in number, p_cliente in number, p_moracap in number ,p_moraint in number,p_endemanda in varchar2) return number; --feb 2018
  function reestructurado ( p_fecha in date, p_credito number)    return date;
  procedure calificacion_cartera ( p_fecha date, p_tipo varchar2 );
  procedure calificacion_cartera_mora ( p_fecha date, p_tipo varchar2 ); --nov 2017

  procedure provision_cartera ( p_fecha in date, p_tipo in varchar2 );
  procedure provision_cartera_gradual ( p_fecha in date, p_tipo in varchar2 );
  procedure provision_cartera_cons ( p_fecha in date, p_tipo in varchar2 );
  PROCEDURE elimina_calprov(p_fecha in date, p_tipo in varchar2);

  PROCEDURE elimina_provision_cons(p_fecha in date, p_tipo in varchar2);
  PROCEDURE elimina_provision_gradual (p_fecha in date, p_tipo in varchar2);

  function cuantas_operaciones ( p_fecha date, p_tipo varchar2 ) return number;
  function dias_mora_en_reest (  p_fechares date, p_credito number ) return number;
  function pais (p_cliente in number, p_numdir in number ) return number;
  function tipo_garantia (p_garantia in number ) return varchar2;

  procedure datos_ini_mod10 ( p_operador number, p_fecha date, p_tipo varchar2 );
  procedure datos_ini_mod6 ( p_operador number, p_fecha date, p_tipo varchar2 );
  procedure datos_ini_mod4 ( p_operador number, p_fecha date, p_tipo varchar2 );
  --ACTUALIZA DATOS
  procedure upd_datos_ini_mod6 ( p_operador number, p_fecha date, p_tipo varchar2 );
  FUNCTION FECHA_RENOVA(p_credito in number, p_fecha in Date) RETURN DATE;
  FUNCTION FECHA_REFINANCIADO(p_credito in number, p_fecha in Date) RETURN DATE;  
  PROCEDURE UPD_FECHA_RENOVA(p_fecha in Date);
  --procedure datos_ini_mod4 ( p_fecha date, p_tipo varchar2 );

  procedure obtener_datos ( p_operador number, p_fecha date, p_tipo varchar2 );

  FUNCTION monto_autorizado_leg(p_moneda   IN NUMBER,
                                p_sucursal IN NUMBER,
                                p_credito  in number,
                                p_fecha    in date) RETURN NUMBER;

  -- Función para devolver el monto total de los créditos por tipo de crédito y por cliente
  --function monto_total_original_tipoc ( p_tipocredito number, p_cliente number, p_fecha date, p_tipo varchar2,
                                        --p_error in out varchar2 ) return number;

  procedure calif_mora(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,p_calif out number);

  procedure calif_inicial(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_califmora in number, p_califcapacidad in number,
                       p_califriesgo in number,p_califinicial out number);

  procedure calif_restruc(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,
                       p_califinicial in number,p_calif out number);

  procedure calif_final(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,
                       p_califinicial in number);

  procedure calif_cubierto(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,p_montocub in number,
                       p_califinicial in number,v_codicion in number,p_calif out number);

  procedure calif_expuesto(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_diasmora in number,
                       p_califinicial in number,v_codicion in number,p_calif out number);

  procedure calif_capacidad(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                       p_cuenta in number,p_mod in number,p_tipocredito in number, p_codtipocredito in varchar2,p_calif out number);


  procedure prov_general(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                         p_cuenta in number,p_mod in number,p_monto in number,p_montogar in number,
                         p_tipocredito in number,p_codtipocredito in varchar2,p_tipoprov in number,p_tipocalif in number,
                         p_provision out number);

  procedure prov_gradual_cap(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                         p_cuenta in number,p_mod in number,p_tipoprov in number,p_provtotal in number);

  procedure prov_gradual_int(p_fecha in date,p_tipo in varchar2,p_cliente in number,
                         p_cuenta in number,p_mod in number,p_tipoprov in number,p_provtotal in number);


  procedure prov_constituida_cap(p_fecha in date,p_tipo in varchar2,p_cliente in number,p_codtipocartera in varchar2,
                         p_cuenta in number,p_mod in number,p_tipoprov in number,p_tipoprovreq in number,p_provtotal in number,p_provresul out number);

  procedure prov_constituida_total(p_fecha in date,p_tipo in varchar2,p_cliente in number,p_codtipocartera in varchar2,
                         p_cuenta in number,p_mod in number,p_tipoprov in number,p_tipoprovreq in number,p_provtotal in number,p_provresul out number);

  function total_prov_general(p_fecha in date,p_tipo in varchar2,p_tipoprov in number,p_codtipocredito in varchar2) return number;
  function oper_prov_general(p_fecha in date,p_tipo in varchar2,p_tipoprov in number, p_cuenta in number) return number;

 function cta_montogar_real (p_mod in number,p_credito in number,p_mon in number,
                             p_sucursal in number, p_fecha in date,po_garadminisble out number,
                             po_garadminisblefisa out number,po_tipogarantia out varchar2 ) return number;
 procedure admisibilidad_garantias (p_fecha date, p_tipo varchar2, p_mes in number );
 procedure distribucion_admisible_real (p_mod in number,p_credito in number,p_mon in number,
                             p_sucursal in number, p_fecha in date,po_garadminisble out number,
                             po_garadminisblefisa out number,po_tipogarantia out varchar2 );

 function aplica_garantia_prov( p_tabtipocred in number, p_tipocred in number, p_calprev in varchar2 ) return varchar2;
 function tipo_credito( p_cliente     in number,
 					    p_tipocredito in number,
                        p_fecha       in date,
                        p_saldopre    in number,
                        p_montoconsolidado out number) RETURN VARCHAR2;
 procedure califica_ope_new(p_fecha in date);
 procedure busca_nuevos_mayores_deudores(p_fecha in date,p_tipo in varchar2);
 procedure busca_nuevos_medianos_deudores(p_fecha in date,p_tipo in varchar2);--nov 2017
 procedure upd_prorrateo_gar(p_fecha in date,p_tipo in varchar2);
 procedure upd_provision_req(p_fecha in date,p_tipo in varchar2);
 procedure upd_provision_grad(p_fecha in date,p_tipo in varchar2);
 procedure upd_provision_6_15_req(p_fecha in date,p_tipo in varchar2);
-- procedure upd_provision_6_15_cons(p_fecha in date,p_tipo in varchar2);

 procedure renovacion_tasa (p_credito in number,p_fecha in date,p_fechaArr out date,p_NroArreglos out number);
 procedure reestruturacion (p_credito in number,p_fecha in date,p_montoArr out number,p_NroArreglos out number,p_fechahasta out date,p_cuotadesde out number);

 procedure upd_saldos_fecha(p_fecha in date,p_tipo in varchar2);
 procedure copy_calificaxcapacidad(p_fechafrom in date,p_tipofrom in varchar2,p_fechato in date);
 procedure copy_calificacionprovision(p_fechafrom in date,p_tipofrom in varchar2,p_fechato in date,p_codtipocredito in varchar2);
 function parametro_calif( p_variable in varchar2 ) RETURN VARCHAR2;
 procedure copy_calificaxperdida(p_fechafrom in date,p_tipofrom in varchar2,p_fechato in date);--nov 2017
 function Calcula_CalificaPerdida(p_califmora in number, p_perdida in number) return number;--nov 2017
function Calcula_CalificaRiesgoMayores(p_califmora in number, p_califcapacidad in number) return number;--nov 2017

end pkg_precalifica_do;
/
