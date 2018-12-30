/* se aplicaria una vez que se ha depurado las ID que estan duplicados*/
alter table TCLI_PERSONA
  add constraint CU02CLI_CLI unique (CLI_IDENTIFICA);