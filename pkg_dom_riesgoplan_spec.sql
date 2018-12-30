CREATE OR REPLACE
PACKAGE pkg_dom_riesgoplan AS
/*--------------------------------------------------------------------*/
/* Objetivo: Estructura el reporte legal REPLEG13                     */
/* Autor: Fisa-Limited						Fecha:09/04/2002 */
/*--------------------------------------------------------------------*/                    
--w_valorparam tdom_parinst_calif.dpi_valor%type:=pkg_precalifica_do.parametro_calif('VALOR_GARANTIZADO_DE');
procedure risk_cabecera( p_operador in number,
p_dml in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2,
p_fechaenn in VARCHAR2,
p_numrecord in VARCHAR2,
p_status in VARCHAR2,
p_sterecord iN VARCHAR2
);                         
--Disponible Cuentas Corrientes, Ahorros, Depósitos a la Vista Otras Entidades
procedure rsk_displiq(
p_operador in number,
p_dml in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2);

procedure file_displiq(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2);

procedure exec_displiq(
p_operador in number,
p_FechaCorte in Date,
p_codfile in varchar2);   

--Cuentas Corrientes, Ahorros, Depósitos a la Vista
procedure rsk_captliq(
p_operador in number,
p_dml in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2);

procedure file_captliq(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2);

procedure exec_captliq(
p_operador in number,
p_FechaCorte in Date,
p_codfile in varchar2);   

--cartera de credito activa
procedure exec_credact(
p_operador in number,
p_FechaCorte in Date,
p_codfile in varchar2);   

procedure file_credact(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2);

procedure rsk_credact(
p_operador in number,
p_dml in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2);
--codeudores
procedure exec_INTCODEUDOR(
p_operador in number,
p_FechaCorte in Date,
p_codfile in varchar2,
p_tiporel  number);   
procedure file_INTCODEUDOR(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2);

procedure rsk_INTCODEUDOR(
p_operador in number,
p_dml in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2,
p_tiporel  number);
--Garantias
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
                    P_TIPOCREDITO3 NUMBER);



procedure file_INTGARANTIA(
p_operador in number,
p_ruc in varchar2,
p_FechaCorte in Date,
p_codfile in varchar2);

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
                    P_TIPOCREDITO3 NUMBER);

--Para Cuenta Contable
function CuentaCon(credito in number,
					modulo in number, 
                    producto in number,
                    tipo in number, 
                    moneda in number,
                    codgru in number,
                    p_fecha in Date) return varchar2;
function Tasamora( modulo in number, 
                   producto in number,
                   tipo in number, 
                   moneda in number,
                   fecha in date) return number;
function TIPOCREDITO( modulo in number, 
                   producto in number,
                   tipo in number, 
                   moneda in number) return VARCHAR2;
                    
function TIP_GARANTIA(tabgar    IN NUMBER,
                       gar   IN NUMBER) return varchar2;                    

function Val_desctabla_n(p_codtab in number,
					   p_codeiso in varchar2) return NUMBER;                    

function Val_cuota(p_credito in number,
                   p_moneda in number,
                   p_fecha in date,
                   p_tipocuota in number) return NUMBER;                    

function Val_desctabla_c(p_codtab in number,
					   p_codeiso in varchar2) return VARCHAR2;

function Val_cuentacon( p_mod in number,
						p_operacion in number,
                        p_tra in number,
                        p_rubro in number) return VARCHAR2;

function Val_penalcance(p_fecha in date,
                        p_tipocartera in varchar2,
                        p_moneda in number,
                        p_plazocance in number) return number;

function Val_desctabla_iso(p_codtab in number,
					   p_codigo in number) return VARCHAR2;

function Val_Relacion_bco(p_codigocli in number) return VARCHAR2;


function Nombre_prd(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number   ) return varchar2;


function Val_rowid(p_report    IN varchar2,
                      p_sessionid   IN NUMBER,
                      p_columnid in number,
                      p_credito in number) return NUMBER;                    

function Val_campo_NUM(p_report    IN varchar2,
                      p_sessionid   IN NUMBER,
                      p_rowid in number,
                      p_columnid in number) return NUMBER;                    

function Val_campo_char(p_report    IN varchar2,
                      p_sessionid   IN NUMBER,
                      p_rowid in number,
                      p_columnid in number) return NUMBER;                    
 --
 function cuenta_con(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number,
					p_gru in  number   ) return varchar2;

function codigo_cat(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number) return varchar2;

function status_cta(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number,
					p_numcue in number,
					p_status in varchar2,
					p_fecha in date) return varchar2;
                      
--
function cuenta_connos(p_mod in number,
                    p_pro in number,
					p_tip in number,
					p_mon in number,
					p_tra in  number,
					p_rubro in number   ) return varchar2;

FUNCTION FGETDOMSALDOVISTA(p_ope in number,P_CTA IN NUMBER, P_FECHA IN DATE ,ANIGROUP     VARCHAR2) RETURN NUMBER;
end;
/