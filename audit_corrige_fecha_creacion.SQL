declare
cursor p is 
select * 
from tcli_persona  
where cli_fechaing is null
order by cli_codigo ;
V_FECHAPE date;
begin
begin
insert into tgen_sesion
    (ses_sesion, ses_sucursal, ses_oficina, ses_operador)
values
(userenv('sessionid'), 1, 1, 1);
exception
   when others then
     null;
end;
for x in p loop
  begin
    select VIS_FECHAPE
	  into V_FECHAPE
	  from vdom_cuentasclimin
	  where CLC_CODCLI =x.cli_codigo;
  exception
     when no_data_found then
	     V_FECHAPE:=x.cli_fecver;
		 --esto en caso de actualizar con la transaccion mas actigua en caso de clientes de divisas
		 /*
		   begin
		   select min(tmo_fechor)
		      into V_FECHAPE
			from tcap_tramon ,(select inf_codusr,inf_numtra
					  from tcaj_infadicional_cpf 
					  where inf_identifica = '1-01-16205-8')
			where  mo_codusr = inf_codusr
			  and tmo_numtra = inf_numtra;
		    exception
			   when others then
			     V_FECHAPE:=x.cli_fecver;
		   end;
		 */
  end ;
  update tcli_persona
     set cli_fechaing = V_FECHAPE
	 where cli_codigo = x.cli_codigo;
	 commit;
end loop;
end ;
/