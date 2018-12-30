prompt PL/SQL Developer import file
prompt Created on Friday, December 04, 2015 by OLlumiquinga
set feedback off
set define off
prompt Loading TGEN_CODTRANS...
    insert into tgen_sesion
      (ses_sesion, ses_sucursal, ses_oficina, ses_operador)
    values
      (userenv('sessionid'), 1, 1, 1);
insert into TGEN_CODTRANS (COD_MOD, COD_TRA, COD_DESTRA, COD_NOMFORMA, COD_TIPOTRA, COD_CHQOLIB, COD_AUTO, COD_TIPOCRE, COD_CODTABCRE, COD_APARECE, COD_GRABACONCEPTO, COD_PAPELETA, COD_CONORIGEN, COD_OPTIMIZER, COD_VALIDA, COD_PARAMETRIZA)
values (3, 901, 'POR IMPUESTO 1%', null, 'D', null, 'S', null, 32, 'N', null, null, null, null, null, null);
insert into TGEN_CODTRANS (COD_MOD, COD_TRA, COD_DESTRA, COD_NOMFORMA, COD_TIPOTRA, COD_CHQOLIB, COD_AUTO, COD_TIPOCRE, COD_CODTABCRE, COD_APARECE, COD_GRABACONCEPTO, COD_PAPELETA, COD_CONORIGEN, COD_OPTIMIZER, COD_VALIDA, COD_PARAMETRIZA)
values (3, 902, 'POR IMPUESTO 10%', null, 'D', null, 'S', null, 32, 'N', null, null, null, null, null, null);
insert into TGEN_CODTRANS (COD_MOD, COD_TRA, COD_DESTRA, COD_NOMFORMA, COD_TIPOTRA, COD_CHQOLIB, COD_AUTO, COD_TIPOCRE, COD_CODTABCRE, COD_APARECE, COD_GRABACONCEPTO, COD_PAPELETA, COD_CONORIGEN, COD_OPTIMIZER, COD_VALIDA, COD_PARAMETRIZA)
values (3, 903, 'POR IMPUESTO 1%', null, 'D', null, 'S', null, 32, 'N', null, null, null, null, null, null);
insert into TGEN_CODTRANS (COD_MOD, COD_TRA, COD_DESTRA, COD_NOMFORMA, COD_TIPOTRA, COD_CHQOLIB, COD_AUTO, COD_TIPOCRE, COD_CODTABCRE, COD_APARECE, COD_GRABACONCEPTO, COD_PAPELETA, COD_CONORIGEN, COD_OPTIMIZER, COD_VALIDA, COD_PARAMETRIZA)
values (3, 904, 'POR IMPUESTO 10%', null, 'D', null, 'S', null, 32, 'N', null, null, null, null, null, null);

insert into TGEN_CODTRANS (COD_MOD, COD_TRA, COD_DESTRA, COD_NOMFORMA, COD_TIPOTRA, COD_CHQOLIB, COD_AUTO, COD_TIPOCRE, COD_CODTABCRE, COD_APARECE, COD_GRABACONCEPTO, COD_PAPELETA, COD_CONORIGEN, COD_OPTIMIZER, COD_VALIDA, COD_PARAMETRIZA)
values (3, 905, 'Unifica POR IMPUESTO ', null, 'D', null, 'S', null, 32, 'N', null, null, null, null, null, null);

commit;
prompt 4 records loaded
set feedback on
set define on
prompt Done.
