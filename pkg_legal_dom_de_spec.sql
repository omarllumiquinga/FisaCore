CREATE OR REPLACE
PACKAGE pkg_legal_Dom_de AS
/*--------------------------------------------------------------------*/
/* Objetivo: Estructura el reporte legal REPLEG13                     */
/* Autor: Fisa-Limited						Fecha:09/04/2002 */
/*--------------------------------------------------------------------*/
w_valorparam tdom_parinst_calif.dpi_valor%type:=pkg_precalifica_do.parametro_calif('VALOR_GARANTIZADO_DE');
PROCEDURE FECHA_TASACION(
   p_cliente IN NUMBER,
   p_numgaran IN NUMBER,
   p_decimales IN NUMBER,
   p_dateto IN DATE,
   p_fechat OUT DATE,
   p_valor OUT NUMBER);

/* Objetivo: Estructura el reporte legal REPLEG11   "CODEUDORES DE CREDITO"               */
    PROCEDURE DE04(
       p_report VARCHAR2,
       p_session NUMBER,
       p_sucursal NUMBER,  -- Mando sucursal 0 para que me haga de todas
       p_moneda   NUMBER,
       p_datefrom DATE,
       p_fecha DATE,
       p_tiporel number);

    PROCEDURE EXLIM(
       p_report VARCHAR2,
       p_session NUMBER,
       p_sucursal NUMBER,  -- Mando sucursal 0 para que me haga de todas
       p_moneda   NUMBER,
       p_datefrom DATE,
       p_fecha DATE,
       p_decimales      in    number);

    --2006/08/03
  --Procedimientos para generar Reporte DE03

 PROCEDURE DE03 (
       p_report  VARCHAR2,
       p_session NUMBER,
       p_datefrom DATE,
       p_dateto  DATE,
       p_tiporel number,
       p_tiporel1 number,
       p_tiporel2 number,
       p_tiporel3 number,
       p_tiporel4 number,
	   p_tipogarid1 VARCHAR2,
	   p_tipogarid2 VARCHAR2,
	   p_tipogarid3 VARCHAR2,
       p_decimales NUMBER,
       p_montocompara NUMBER,
       p_tipocredito1 NUMBER,
       p_tipocredito2 NUMBER,
       p_tipocredito3 NUMBER       ) ;

PROCEDURE de11 (
       p_report         in    varchar2,
       p_session        in    number,
       p_sucursal       in    number,
       p_datefrom          in    date,
       p_fecha          in    date,
       p_tipocredito1    in    number,
       p_tipocredito2    in    number,
       p_tipocredito3    in    number,
       p_montocompara    in    number,
       p_decimales      in    number);
PROCEDURE de12(
       p_report         in    varchar2,
       p_session        in    number,
       p_sucursal       in    number,
       p_fecha          in    date,
       p_tipocredito1    in    number,
       p_tipocredito2    in    number,
       p_tipocredito3    in    number,
       p_montocompara    in    number,
       p_decimales      in    number);

PROCEDURE de13(
       p_report         in    varchar2,
       p_session        in    number,
       p_sucursal       in    number,
       p_fecha          in    date,
       p_tipocredito1    in    number,
       p_tipocredito2    in    number,
       p_tipocredito3    in    number,
       p_decimales      in    number);

PROCEDURE de15(
       p_report         in    varchar2,
       p_session        in    number,
       p_sucursal       in    number,
       p_fecha          in    date,
       p_tipocredito1    in    number,
       p_tipocredito2    in    number,
       p_tipocredito3    in    number,
       p_decimales      in    number);

PROCEDURE de21 (
       p_report         in    varchar2,
       p_session        in    number,
       p_sucursal       in    number,
       p_datefrom       in    date,
       p_fecha          in    date,
       p_tipocredito1    in    number,
       p_tipocredito2    in    number,
       p_tipocredito3    in    number,
       p_montocompara    in    number,
       p_decimales      in    number);

FUNCTION RECURSO (
        p_mod    in number,
		p_cuenta in number
		)
        RETURN  varchar2;

PROCEDURE PERIODOS_CAP (
        p_tipocuo in number,
		p_diasper in number,
		p_PERIODO OUT CHAR);
/*PROCEDURE PERIODOS (
        p_tipocuo in number,
		p_diasper in number,
		p_PERIODO OUT CHAR);  */

PROCEDURE PERIODOS_INT (
        p_tipocuo in number,
		p_diasper in number,
		p_PERIODO OUT CHAR);

PROCEDURE PERGRACIA(p_credito IN NUMBER,
                    p_fecha IN DATE,
                    p_gracia OUT NUMBER);

FUNCTION FECHA_RENOVA(
        p_credito in number,
        p_fecha  in Date)
        RETURN DATE;

FUNCTION CODIGO_PAIS(
        p_clientep in number,
        p_numdir in number)
        RETURN VARCHAR2;


FUNCTION CUENTA_CONTABLE(
		pModulo in number,
		pProducto in number,
		pTipo in number,
		pMoneda in number,
		pTipo_cartera in number)
        RETURN varchar2;

FUNCTION CUE_CONTABLE(
		pModulo in number,
		pProducto in number,
		pTipo in number,
		pMoneda in number,
		pTipo_cartera in number)
        RETURN varchar2;


FUNCTION garante_solidario
     (p_cliente IN NUMBER,
      p_tiporel IN NUMBER)

   RETURN CHAR;

FUNCTION saldo_capital_fecha_de
     (p_operacion IN NUMBER,
      p_fecha IN DATE) RETURN NUMBER;

FUNCTION tipo_valores (p_tipbien IN NUMBER)
RETURN VARCHAR2 ;

PROCEDURE tip_garantia
 (MODulo     in number,
  credito in number,
  cliente in number,
  numerogar in number,
  tipo_gar out varchar2);


PROCEDURE de22 (
       p_report         in    varchar2,
       p_session        in    number,
       p_sucursal       in    number,
       p_datefrom       in    date,
       p_fecha          in    date,
       p_tipocredito1    in    number,
       p_tipocredito2    in    number,
       p_tipocredito3    in    number,
       p_montocompara    in    number,
       p_decimales      in    number);

PROCEDURE de23 (
       p_report         in    varchar2,
       p_session        in    number,
       p_sucursal       in    number,
       p_datefrom       in    date,
       p_fecha          in    date,
       p_tipocredito1    in    number,
       p_tipocredito2    in    number,
       p_tipocredito3    in    number,
       p_decimales      in    number);

PROCEDURE de25 (
       p_report         in    varchar2,
       p_session        in    number,
       p_sucursal       in    number,
       p_datefrom          in    date,
       p_FECHA       in    date,
       p_tipocredito1    in    number,
       p_tipocredito2   in    number,
       p_tipocredito3    in    number,
       p_decimales      in    number);

FUNCTION monto_aprobado(
          p_credito in number,
          p_fecha in date)
        RETURN NUMBER;

FUNCTION fecha_aprobado(
          p_numcomprom in number,
          p_credito in number,
          p_solpv in varchar2)
        RETURN DATE;

PROCEDURE arreglo_pagos (  p_credito IN NUMBER,
       p_fecha          IN DATE,
       p_fechaArr       OUT DATE,
       p_NroArreglos OUT NUMBER,
       p_plazoAnterior OUT NUMBER,
       p_montoAnterior OUT NUMBER) ;

FUNCTION numero_dias(
          p_fechaFinal in date,
          p_fechaInicial in date,
          p_basemes in number)
        RETURN NUMBER;

FUNCTION codtipo_cartera(
          p_fecha in date,
          p_tipo in varchar2,
          p_cuenta in number)
        RETURN varchar2;


function fun_gen_cuenta(pni_operacion in varchar2,
                        pni_cuenta    in number,
                        pni_criterio  in number,
                        pni_modulos   in varchar2,
                        pni_reporte   in varchar2,
                        pni_fecha in date) return varchar2;

--15648
function fun_tipo_cliente(pni_cliente in number) return varchar2;
function fun_producto_servicio(pni_operacion in number,p_modulo in number) return varchar2;
function fun_garantia(pni_operacion in number,p_fecha in date,p_rep in varchar2) return number;
function fun_aniogarantia(pni_cliente in number,p_numgaran in number) return number;
function fun_actividad_cliente(pni_cliente in number) return varchar;
function fun_es_peps(pni_tipoid in varchar2,pni_identificacion in varchar2, p_tipopep out varchar2) return varchar;
function fun_localidad(p_sucursal in number,p_oficina in number,pni_codcli in number,pni_numdir in number) return varchar2;
function fun_destino_ciiu(pni_codcli in number,pni_operacion in number,pni_compromiso out varchar2) return varchar2;

--15648
--junio 2015
function fun_tipo_tasa(pni_fecha in date,pni_operacion in number) return varchar2;
--function fun_ORG_CREDITO(pni_codmod in number,pni_codpro in number,pni_codtip in number,pni_codmon in number) return varchar2;
function fun_iso_desctabla(pni_codtab in number,pni_codigo in number) return varchar2;
--fin junio 2015

function fun_cuenta_del_2031(pni_cuenta    in varchar2) return varchar2;

function fun_gen_ReeLeg(pvi_cuenta  in varchar2,
                        pni_credito in number,
                        pdi_fecha   in date) return varchar2;

  procedure pro_gen_cuoextra(pni_credito in number,
                             pdi_fecha in date,
                             pnb_cuota out number,
                             pdb_fecha out date);

  function fun_gen_revtasa (pni_credito in number,
                            pdi_fecha in date) return date;
function calificacion(P_FECHA IN DATE,P_TIPO IN VARCHAR2,P_CUENTA IN NUMBER, P_TIPOCALIF IN NUMBER) RETURN VARCHAR2;
FUNCTION provision(P_FECHA IN DATE,P_TIPO IN VARCHAR2,P_CUENTA IN NUMBER, P_TIPOCALIF IN NUMBER) RETURN NUMBER;
PROCEDURE de_resumen_saldos_anual(
       p_operador        in    number,
       p_fecha          in    date);

end;

 
/
