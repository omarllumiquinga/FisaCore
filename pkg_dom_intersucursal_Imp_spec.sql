CREATE OR REPLACE
package pkg_dom_intersucursal_imp is
   procedure mov_manual(p_operador in number,p_fecha in date);
   --procedure mov_manual_is(p_operador in number,p_fecha in date);
   PROCEDURE BorrarComprobantes(pFecha date,p_texto varchar2);
end pkg_dom_intersucursal_imp;
/
