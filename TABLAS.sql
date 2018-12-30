drop table rsk_cabecera;
create table rsk_cabecera(fecha date,
empresa varchar2(20),
codfile varchar2(11),
FECHAENV varchar2(10),
NUMRECORDS varchar2(12),
STATUS varchar(5) default 0,
STRECORD varchar2(12));
alter table rsk_cabecera
  add constraint CP01rsk_cabecera primary key (fecha,empresa,codfile);

--
drop table rsk_credact;
create table rsk_credact(
fechaCorte date,
empresa varchar2(20),
codfile varchar2(11),
fechaAprob varchar2(8),
fechaDesembolso varchar2(8),
fechaPrimerPago varchar2(8),
fechaVencimiento varchar2(8),
fechaRestructura varchar2(8),
fechaRenov varchar2(8),
fechaCancelado varchar2(8),
PlazoOriginal number(6),
PlazoAjustado number(6),
CodigoDeudor varchar2(30),
TipoDeudor varchar2(2),
NumeroOper varchar2(30),
CodigoRevol varchar2(30),
CuentaContable varchar2(30),
TipoGarantia varchar2(2),
GarantiaAdmisible number(15,2),
NombreCliente varchar2(60),
CodigoCriterio varchar2(2),
CodigoCategoria varchar2(2),
DiasMora number(5),
moneda number(2),
TasaInteres number(7,2),
TasaMora number(7,2),
FreReprecioTasa number(3),
MontoVigente number(17,2),
MontoAprobado number(17,2),
saldoInsoluto number(17,2),
cuota number(17,2),
PeriodoCapital number(3),
PeriodoInteres number(3),
PrincipalMoroso number(17,2),
InteresMoroso number(17,2),
SectorEcoSujeto number(6),
SectorEcoCredito number(6),
TipoCredito varchar2(3),
CreditScoringIni varchar2(5),
IRBGeneral varchar2(5),
IRBMontoExpuesto varchar2(5),
IRBMontoCubierto varchar2(5),
IRBRiesgoPais varchar2(5),
MontoCoberturaFin number(17,2),
EstadoOperacion number(2),
ProvReqCapital number(17,2),
CodigoPais varchar2(4),
localidad varchar2(6),
producto varchar2(30),
OrigenRec  varchar2(30),
OficinaSucursal number(5),
Subpoblacion varchar2(30),
CuotaSeguro number(15,2),
CuotaComision number(15,2),
ApellidosSiglas varchar2(30),
PeriodoGracia number(2),
TipoVinculacion varchar2(2),
fechaInicioAdju varchar2(10),
IdentBeneConting varchar2(15),
NombreBeneficiario varchar2(60),
OpcionPagoCance varchar2(2),
PenalPagoAnticipa number(6,2),
ProviReqGradualCap number(15,2),
ProviCapConst number(15,2),
ProviReqRendi number(15,2),
ProviReqGraRendi number(15,2),
ProviReqContinge number(15,2),
ProviReqGradContinge number(15,2),
FechaReviTasa varchar2(10),
FreReprecio number(3),
FechaPagoCuoextra varchar2(10),
MontoPagoCuoExtra number(15,2),
BasetasaInteres number(1),
Reestructurado varchar2(2),
RendixCobrar Number(17,2),
DeudoresBenefi varchar2(1),
TipoCliente number(3),
FacilidadCredit number(3),
CtaContableContig varchar2(30),
SaldoConting number(17,2),
OrigenCredito varchar2(2),
fechaRefinanciacion varchar2(10),
RazonRiegoDeudor varchar2(1),
FechaIniCobraJudi varchar2(10),
Tipocancelacion varchar2(1),
TipoFlexibiNorma varchar2(3));

alter table rsk_credact
  add constraint CP01rsk_credact primary key (fechaCorte,empresa,NumeroOper);
  
alter table rsk_credact
  add constraint CF01rsk_credact foreign key (fechaCorte,empresa,codfile)
  references rsk_cabecera(fecha,empresa,codfile);
-----------------
drop table rsk_INTCODEUDOR;
create table rsk_INTCODEUDOR(
fechaCorte date,
empresa varchar2(20),
codfile varchar2(11),
CodigoCredito varchar2(27),
IdentificaDeudor varchar2(15),
TipoDeudor varchar2(2),
NombreCodeudor varchar2(60),
ApellidosSiglas varchar2(60),
TipoCredito Varchar2(1));

alter table rsk_INTCODEUDOR
  add constraint CP01rsk_INTCODEUDOR primary key (fechaCorte,empresa,CodigoCredito,IdentificaDeudor);
  
alter table rsk_INTCODEUDOR
  add constraint CF01rsk_INTCODEUDOR foreign key (fechaCorte,empresa,codfile)
  references rsk_cabecera(fecha,empresa,codfile);

 ---------------------
 drop table rsk_INTGARANTIA;
 create table rsk_INTGARANTIA(
fechaCorte date,
empresa varchar2(20),
codfile varchar2(11),
CodigoCredito varchar2(27),
IdentificaGarantia varchar2(15),
TipoGaranteSolidario varchar2(2),
TipoGarantia varchar2(2),
DescripGarantia varchar2(250),
FechaConstit varchar2(10),
fechaForma varchar2(10),
FechaTasacion varchar2(10),
ValorTasacion number(15,2),
RangoGarantia number(1),
TipoGarantiaVal number(3),
IdentEntEmisora varchar2(15),
GarAsegurada varchar2(1),
fechaVenPoliza varchar2(10),
NombreGarante varchar2(60), 
ApellidoGaran varchar2(30),
TipoCredito varchar2(1),
PorcTasacionGar number(6,2),
GarantiaAdmi number(15,2),
AnioFabGarantia varchar2(4),
MontoFormaGar number(15,2),
IdentTasador varchar2(13),
fechaVencGar varchar2(10),
NumeroPolizaSeg varchar2(10),
FecEmiPolSeg varchar2(10),
IdentComAseguradora varchar2(13),
ValorEndoso number(15,2),
GaranFiduciaria varchar2(1),
ClasifFiduciaria varchar2(3),
DescripFide varchar2(250));

--alter table rsk_INTGARANTIA
  --add constraint CP01rsk_INTGARANTIA primary key (fechaCorte,empresa,CodigoCredito,IdentificaGarantia);
  
alter table rsk_INTGARANTIA
  add constraint CF01rsk_INTGARANTIA foreign key (fechaCorte,empresa,codfile)
  references rsk_cabecera(fecha,empresa,codfile);
  
  drop table rsk_tasaspenalidad;
create table rsk_tasaspenalidad(
rks_desde date ,
rsk_tipocredito varchar2(1),
rsk_moneda number,
rls_hasta date,
rsk_antesplazo number,
rsk_penalidad number);

--frep9126 21/9126
alter table TPRE_PROTIPOCREDITO add PTC_TABTIPOCRErisk NUMBER(3);
alter table TPRE_PROTIPOCREDITO add PTC_TIPOCRErisk NUMBER(4);

--drop table Tdom_aseguradoras;
create table Tdom_aseguradoras
(
  ASE_CODTAB      NUMBER(3) not null,
  ASE_CODIGO      NUMBER(4) not null,
  ASE_IDENTIFICA  VARCHAR2(19));
alter table Tdom_aseguradoras
  add constraint CP01ASE_ID primary key (ASE_CODTAB, ASE_CODIGO);
--Plantilla Cuentas Corrientes, Ahorros, Depósitos a la Vista
   drop table rsk_CAPTLIQ;   
create table rsk_CAPTLIQ(
fechaCorte date,
empresa varchar2(20),
codfile varchar2(11),
codigoCliente varchar2(30),
NumeroCuenta varchar2(30),
CuentaContableCap varchar2(30),
NombreCliente varchar2(60),
CodigoCategoria number(2),
FechaApertura varchar2(10),
Moneda number(2),
tasaInteres number(7,3),
FrecuenciaRepre number(3),
SaldoFinal number(17,2),
Rendimientos number(17,2),
SectorEconomico Number(6),
Subpoblacion varchar2(40),
producto varchar2(30),
TipoSujeto varchar2(2),
TipoVinculacion varchar2(2),
EstatusCuenta varchar2(1),
Localidad Number(6),
TipoCliente Number(3),
CuentaContableInt varchar2(30),
Tipodepositante varchar2(1));

alter table rsk_CAPTLIQ
  add constraint CF01rsk_CAPTLIQ foreign key (fechaCorte,empresa,codfile)
  references rsk_cabecera(fecha,empresa,codfile);
---------ahorros y corrientes en otras entidades 
--Plantilla Cuentas Corrientes, Ahorros, Depósitos a la Vista
drop table rsk_displiq;   
create table rsk_displiq(
fechaCorte date,
empresa varchar2(20),
codfile varchar2(11),
codigoBIc varchar2(8),
NumeroCuenta varchar2(30),
CuentaContableCap varchar2(30),
NombreEntidad varchar2(60),
CodigoCategoria number(2),
FechaApertura varchar2(10),
Moneda number(2),
tasaInteres number(7,3),
FrecuenciaRepre number(3),
SaldoFinal number(17,2),
SaldoPromedio number(17,2),
SectorEconomico number(6),
Subpoblacion varchar2(40),
producto varchar2(30),
TipoSujeto varchar2(2),
Subpoblacion1 varchar2(40),
Subpoblacion2 varchar2(40),
Subpoblacion3 varchar2(40),
BaseTasaInteres Number(1),
TipoVinculacion varchar2(2),
EstatusCuenta varchar2(1),
Localidad Number(6));

alter table rsk_displiq
  add constraint CF01rsk_displiq foreign key (fechaCorte,empresa,codfile)
  references rsk_cabecera(fecha,empresa,codfile);

  alter table TPRE_PROTIPOCREDITO add PTC_ENCAJE VARCHAR2(2) default 'N';
  
create table TDOM_BALTYPE_NO_SALDO
(
  C_BALTYPE VARCHAR2(5)
);

DROP TABLE TAUX_FUNCGROUPDET_DET;
create table TAUX_FUNCGROUPDET_DET
(
  GDT_OPERADOR   NUMBER(4) not null,
  GDT_CUENTA     VARCHAR2(14) not null,
  GDT_SUCURSAL   NUMBER(3) not null,
  GCT_TABCAT     NUMBER(3) default 950 not null,
  GCT_CATEGORIA  NUMBER(4) not null,
  GCT_TABSUBTIP  NUMBER(3) default 168 not null,
  GCT_SUBTIPO    NUMBER(4) not null,
  GDT_MOD        NUMBER(2) not null,
  GDT_PRO        NUMBER(2) not null,
  GDT_TIP        NUMBER(2) not null,
  GDT_MON        NUMBER(2) not null,
  GDT_CODGRU     NUMBER(3) not null,
  GDT_SALDOMOD   NUMBER(18,6),
  GDT_SALDOCTA   NUMBER(18,6),
  GDT_SALDOCTAME NUMBER(18,6),
  GDT_OFICINA    NUMBER(2),
  GDT_OPERACION  NUMBER
);
-- Create table
create table TREPORTLEGAL_ASM
(
  CODE        VARCHAR2(6) not null,
  DESCRIPTION VARCHAR2(60),
  FRECUENCIA  VARCHAR2(1),
  FUNCDIN     VARCHAR2(200),
  RANGE       NUMBER(4),
  TYPEOUT     VARCHAR2(1),
  REPORTNAME  VARCHAR2(30),
  FILLER      NUMBER(4),
  TYPERISK    VARCHAR2(1)
);

-- Add comments to the columns 
comment on column TREPORTLEGAL_ASM.FUNCDIN
  is 'Funcion Dinamica';
comment on column TREPORTLEGAL_ASM.RANGE
  is 'Rangos';
comment on column TREPORTLEGAL_ASM.TYPEOUT
  is 'Tipo de salida';
comment on column TREPORTLEGAL_ASM.REPORTNAME
  is 'Nombre del Reporte';
comment on column TREPORTLEGAL_ASM.FILLER
  is 'Tama?o del relleno';
-- Create/Recreate primary, unique and foreign key constraints 
alter table TREPORTLEGAL_ASM
  add constraint CP01LEGASM_RPL primary key (CODE);
-- Create/Recreate check constraints 
alter table TREPORTLEGAL_ASM
  add constraint CC01LEGASM_RPL
  check (FRECUENCIA IN ('D','N','M','T','S','A'));
alter table TREPORTLEGAL_ASM
  add constraint CC02LEGASM_RPL
  check (TYPERISK IN ('M','L'));
