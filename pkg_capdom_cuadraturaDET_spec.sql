CREATE OR REPLACE
package pkg_capdom_cuadraturaDET is

  -- Author  : LLUMIO
  -- Created : 12/03/2008 8:49:15
  -- Purpose : Agrupar las funcionalidades de cuadratura de vista
  -- Public type declarations


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
   );

  FUNCTION fun_cotizacion(
    pdi_fecha           IN    DATE,
    pni_codsuc          IN   NUMBER,
    pni_moneda          IN   NUMBER,
    pci_negociable      IN   VARCHAR2
  ) RETURN NUMBER;

  FUNCTION int_capitalizado(
    pdi_fecha           IN    DATE,
    pni_numcue          IN   NUMBER
  ) RETURN NUMBER;

Function ValorContable(nFecha Date, nCuenta Varchar2) Return Number;
Function ValorContableDia(nFecha Date, nCuenta Varchar2) Return Number;
Function ValorContableDiaDeb(nFecha Date, nCuenta Varchar2) Return Number;
end pkg_capdom_cuadraturaDET;
 
/
