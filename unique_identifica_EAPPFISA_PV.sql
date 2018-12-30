/* se aplicaria una vez que se ha depurado las ID que estan duplicados
AMBIENTE DE PLATAFORMA DE VENTAS*/
alter table TPER_PERSONA
  add constraint I03_PER_PERSON unique (PERSON_LEGAL_ID);