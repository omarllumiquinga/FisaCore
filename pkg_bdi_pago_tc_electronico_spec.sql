CREATE OR REPLACE
PACKAGE pkg_bdi_pago_tc_electronico IS
PROCEDURE PAGO_TARJETA_TC (P_CUENTA   IN NUMBER,   --ahorros
                           P_TARJETA  IN VARCHAR2, --NUMBER,   --tarjeta
						   P_MONEDAPAGAR IN NUMBER,--moneda de TC que voy a pagar                           
                           P_TIPOPAGO IN VARCHAR2, --N
                           P_VALOR    IN  NUMBER,--valor a debitar - pagar
                           P_CANAL IN NUMBER,
                           P_MENSAJE   OUT VARCHAR2, --mensaje a transmitir
                           P_CODUSRP OUT NUMBER,--codigo que ejecuto la trans
                           P_NUMTRAP OUT NUMBER,--numero de transaccion unica
                           P_ServRegistroOK  OUT VARCHAR2,--o no exiete error 1 existe un error
                           P_ServMsgError OUT VARCHAR2);
PROCEDURE PAGO_TARJETA_TC_MAS (P_CUENTA   IN NUMBER,   --ahorros
                           P_TARJETA  IN VARCHAR2, --NUMBER,   --tarjeta
						   P_MONEDAPAGAR IN NUMBER,--moneda de TC que voy a pagar                           
                           P_TIPOPAGO IN VARCHAR2, --N
                           P_VALOR    IN  NUMBER,--valor a debitar - pagar
                           P_CANAL IN NUMBER,  
                           PI_OPERADOR IN NUMBER,--operador que envia los pagos
                           PI_SUCURSAL IN NUMBER,
                           PI_OFICINA IN NUMBER,
                           PI_DEPTO IN NUMBER,
                           PI_TERMINAL IN VARCHAR2,                           
                           P_MENSAJE   OUT VARCHAR2, --mensaje a transmitir
                           P_CODUSRP OUT NUMBER,--codigo que ejecuto la trans
                           P_NUMTRAP OUT NUMBER,--numero de transaccion unica
                           P_ServRegistroOK  OUT VARCHAR2,--o no exiete error 1 existe un error
                           P_ServMsgError OUT VARCHAR2);
PROCEDURE PAGO_TARJETA_TC_MASDEBCTA (P_CUENTA   IN NUMBER,   --ahorros
                           P_TARJETA  IN VARCHAR2, --NUMBER,   --tarjeta
						   P_MONEDAPAGAR IN NUMBER,--moneda de TC que voy a pagar                           
                           P_TIPOPAGO IN VARCHAR2, --N
                           P_VALOR    IN  NUMBER,--valor a debitar - pagar
                           P_CANAL IN NUMBER,  
                           PI_OPERADOR IN NUMBER,--operador que envia los pagos
                           PI_SUCURSAL IN NUMBER,
                           PI_OFICINA IN NUMBER,
                           PI_DEPTO IN NUMBER,
                           PI_TERMINAL IN VARCHAR2,                           
                           P_MENSAJE   OUT VARCHAR2, --mensaje a transmitir
                           P_CODUSRP OUT NUMBER,--codigo que ejecuto la trans
                           P_NUMTRAP OUT NUMBER,--numero de transaccion unica
                           P_ServRegistroOK  OUT VARCHAR2,--o no exiete error 1 existe un error
                           P_ServMsgError OUT VARCHAR2);
PROCEDURE PAG_TC_ANTONHY (P_FECHA IN DATE, --FECHA QUE PAGO EN LA TIENDA
                          P_CUENTA   IN NUMBER,--Cuenta de ahorros o corriente
                          P_TARJETA  IN VARCHAR2, --IN NUMBER
                          P_TIPOPAGO IN VARCHAR2,--N normal
                          P_VALOR    IN  NUMBER,--valor del pago
                          P_CANAL IN NUMBER,--2 caja bdi
                          P_OPERADOR IN NUMBER,--operador que envia los pagos
                          P_SUCURSAL IN NUMBER,
                          P_OFICINA IN NUMBER,
                          P_DEPTO IN NUMBER,
                          P_TERMINAL IN VARCHAR2,
                          P_FORMAPAGO IN NUMBER, --FORMA DE PAGO EFECTIVO O CHEQUE                          
                          P_MENSAJE   OUT VARCHAR2,--mensaje de salida
                          P_CODUSRP OUT NUMBER,--usuario que realizo el pago
                          P_NUMTRAP OUT NUMBER,--numtra
                          P_ServRegistroOK  OUT VARCHAR2,
                          P_ServMsgError OUT VARCHAR2);
PROCEDURE PAG_TC_ACH (P_FECHA IN DATE, --FECHA QUE PAGO EN LA TIENDA
                          P_CUENTA   IN NUMBER,--Cuenta de ahorros o corriente
                          P_TARJETA  IN VARCHAR2, --IN NUMBER
                          P_TIPOPAGO IN VARCHAR2,--N normal
                          P_VALOR    IN  NUMBER,--valor del pago
                          P_CANAL IN NUMBER,--2 caja bdi
                          P_OPERADOR IN NUMBER,--operador que envia los pagos
                          P_SUCURSAL IN NUMBER,
                          P_OFICINA IN NUMBER,
                          P_DEPTO IN NUMBER,
                          P_TERMINAL IN VARCHAR2,
                          P_MENSAJE   OUT VARCHAR2,--mensaje de salida
                          P_CODUSRP OUT NUMBER,--usuario que realizo el pago
                          P_NUMTRAP OUT NUMBER,--numtra
                          P_ServRegistroOK  OUT VARCHAR2,
                          P_ServMsgError OUT VARCHAR2); --si es 1 se despliega el mensaje de error
PROCEDURE INS_TCAJ_FORPAGO(P_CUENTA IN NUMBER,
                           P_MODORG IN NUMBER,
                           P_PROORG IN NUMBER,
                           P_TIPORG IN NUMBER,
                           P_MONORG IN NUMBER,
                           P_DEPTO  IN NUMBER,
						   P_SUC IN NUMBER,
						   P_OFI IN NUMBER,
						   P_CLIENTE NUMBER,
						   P_VALOR IN OUT NUMBER,
                           P_MODDES IN NUMBER,
                           P_PRODES IN NUMBER,
                           P_TIPDES IN NUMBER,
                           P_MONDES IN NUMBER,
                           P_OPERADOR IN  NUMBER,
                           P_DISPONIBLE IN NUMBER,
                           P_NUMPAGO OUT NUMBER);
PROCEDURE INS_TCAJ_FORPAGO_MULTI(P_CUENTA IN NUMBER,
                           P_MODORG IN NUMBER,
                           P_PROORG IN NUMBER,
                           P_TIPORG IN NUMBER,
                           P_MONORG IN NUMBER,
                           P_DEPTO  IN NUMBER,
						   P_SUC IN NUMBER,
						   P_OFI IN NUMBER,
						   P_CLIENTE NUMBER,
						   P_VALOR IN OUT NUMBER,
                           P_MODDES IN NUMBER,
                           P_PRODES IN NUMBER,
                           P_TIPDES IN NUMBER,
                           P_MONDES IN NUMBER,
                           P_OPERADOR IN  NUMBER,
                           P_TASACOMPRA IN NUMBER,
                           P_TASAVENTA IN NUMBER, 
                           P_DISPONIBLE IN NUMBER,                                                     
                           P_NUMPAGO OUT NUMBER);                                                     
PROCEDURE realiza_credito(P_CUENTA IN NUMBER,
                          P_SUCURSAL IN NUMBER,
                          P_OFICINA IN NUMBER,
                          P_DEPTO IN NUMBER,
                          P_REF  IN NUMBER,
                          P_DIFERIDO IN VARCHAR2,
                          P_OPERADOR IN NUMBER,
                          p_usrsecuencia IN OUT  NUMBER,
                          P_NUMPAGO IN NUMBER,
                          P_SEC IN OUT  NUMBER,
                          P_MOD IN NUMBER,
                          P_PRO IN NUMBER,
                          P_TIP IN NUMBER,
                          P_MON IN NUMBER,
                          P_TRA IN NUMBER,
                          P_RUBRO IN NUMBER,
                          P_VAL IN NUMBER);
PROCEDURE realiza_debitos(P_CUENTA IN NUMBER,
                                             P_SUCURSAL IN NUMBER,
                                             P_OFICINA IN NUMBER,
                                             P_DEPTO IN NUMBER,
                                             P_REF  IN NUMBER,
                                             P_DIFERIDO IN VARCHAR2,
                                             P_OPERADOR IN NUMBER,
                                             p_usrsecuencia IN OUT  NUMBER,
                                             P_NUMPAGO IN NUMBER,
                                             P_SEC IN OUT  NUMBER);
PROCEDURE inserta_tcap_tramon(P_CUENTA IN NUMBER,
                              P_OPERADOR IN NUMBER,
                              P_SECUENCIA IN NUMBER,
                              P_TRANSACCION IN OUT NUMBER,
                              P_DIFERIDO IN VARCHAR2,
                              P_VALOR IN NUMBER,
                              P_CLIENTE IN NUMBER);
PROCEDURE INSERTA_TCAP_TRAMONAUX(p_operador IN NUMBER,
							     p_secuencia IN NUMBER,
								 p_sucursal IN NUMBER,
                                 p_oficina IN NUMBER,
                                 p_diferido  IN VARCHAR2,
                                 p_modulo IN NUMBER,
                                 p_producto IN NUMBER,
                                 p_tipo IN NUMBER,
                                 p_mon IN NUMBER,
                                 p_transaccion  IN NUMBER,
                                 p_rubro IN NUMBER,
                                 p_depto IN NUMBER,
                                 P_REF IN VARCHAR2,
                                 P_TIPOTRA IN VARCHAR2,
                                 P_VALOR IN NUMBER,
                                 P_tiporub IN VARCHAR2,
                                 P_CUENTA IN NUMBER,
                                 p_autorizador IN NUMBER,
                                 p_terminal IN VARCHAR2);
PROCEDURE INSERTA_TCAP_PAGOSERVICIO( p_tarjeta IN VARCHAR2,
									         p_tipopago  IN VARCHAR2,
									         p_valor IN NUMBER,
									         p_codigoserv IN NUMBER,
                                             p_sucursal IN NUMBER,
                                             p_oficina IN NUMBER,
                                             p_operador IN NUMBER,
                                             p_secuencia IN OUT NUMBER,
                                             p_diferido IN VARCHAR2,
                                             P_DEPTO IN NUMBER,
                                             P_TERMINAL IN VARCHAR2,
                                             P_SEC IN OUT NUMBER,
                                             p_parametro3 IN VARCHAR2,
                                             p_parametro4 IN VARCHAR2);
PROCEDURE ENVIA_MENSAJE(P_mensaje IN VARCHAR2,
						P_ServRegistroOK IN OUT VARCHAR2,
						P_ServMsgError IN OUT VARCHAR2);
PROCEDURE RECIBE_MENSAJE(p_operador IN NUMBER,
                         p_numtra IN NUMBER,
                         p_sec IN NUMBER,
                         p_mensajes IN OUT NUMBER,
                         p_error OUT VARCHAR,
                         p_descerror OUT VARCHAR,
                         P_ServRegistroOK IN OUT VARCHAR2,
						 P_ServMsgError IN OUT VARCHAR2 );
PROCEDURE REVERSO(P_OPERADOR IN NUMBER,
                  P_NUMTRA IN NUMBER,
                  P_SEC IN NUMBER,
                  MENSAJE OUT VARCHAR2);
PROCEDURE VALIDA_TRANSACCION(P_OPERADOR       IN NUMBER,
						     P_NUMTRA         IN NUMBER,
						     P_MENSAJE OUT VARCHAR2,
						     P_ServRegistroOK IN OUT VARCHAR2,
						     P_ServMsgError   IN OUT VARCHAR2,
						     P_canal IN NUMBER,
						     P_CURRENCY IN NUMBER);
PROCEDURE VALIDA_TRANSACCION_MAS(P_OPERADOR       IN NUMBER,
						     P_NUMTRA         IN NUMBER,
						     P_MENSAJE OUT VARCHAR2,
						     P_ServRegistroOK IN OUT VARCHAR2,
						     P_ServMsgError   IN OUT VARCHAR2,
						     P_canal IN NUMBER,
						     P_CURRENCY IN NUMBER);
 PROCEDURE VALIDA_TRANSACCION_ANT(P_OPERADOR       IN NUMBER,
						     P_NUMTRA         IN NUMBER,
						     P_FORMAPAGO IN NUMBER, --EFECTIVO O CHEQUE
						     P_MENSAJE OUT VARCHAR2,
						     P_ServRegistroOK IN OUT VARCHAR2,
						     P_ServMsgError   IN OUT VARCHAR2,
						     P_canal IN NUMBER,
						     P_CURRENCY IN NUMBER);
PROCEDURE VALIDA_TRANSACCION_ACH(P_OPERADOR       IN NUMBER,
						     P_NUMTRA         IN NUMBER,
						     P_MENSAJE OUT VARCHAR2,
						     P_ServRegistroOK IN OUT VARCHAR2,
						     P_ServMsgError   IN OUT VARCHAR2,
						     P_canal IN NUMBER);
 PROCEDURE VALIDA_TRANSACCION_RESP(P_OPERADOR IN NUMBER,--Operador
						     P_NUMTRA IN NUMBER,--numero de transaccion
						     P_MENSAJEREC IN VARCHAR2,--dmensaje de respuesta en este caso desde procecard
						     P_MENSAJESEN OUT VARCHAR2,--mensaje de envio
						     P_ServRegistroOK  OUT VARCHAR2,--1 error 0 todo OK
						     P_ServMsgError  OUT VARCHAR2);
 PROCEDURE VALIDA_TRANSACCION_RESP_ANTO(P_OPERADOR IN NUMBER,--Operador
						     P_NUMTRA IN NUMBER,--numero de transaccion
						     P_MENSAJEREC IN VARCHAR2,--dmensaje de respuesta en este caso desde procecard
						     P_SECTRA OUT NUMBER,--SECUENCIA DE LA TRANSACCION EN TRAMON
						     P_MENSAJESEN OUT VARCHAR2,--mensaje de envio
						     P_ServRegistroOK  OUT VARCHAR2,--1 error 0 todo OK
						     P_ServMsgError  OUT VARCHAR2);
  PROCEDURE CMS_FINTRANSACTION(p_codusr  IN NUMBER,
                         p_numtra  IN NUMBER,
                         p_sec     IN NUMBER,
                         p_fechaproc in date,
                         p_error   IN OUT VARCHAR2,
  						 pcms_undoresponses OUT VARCHAR2);
 PROCEDURE ENVIO_ANTONY_TRAMA(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_USUARIO IN NUMBER,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMTRAP IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TARJETA IN VARCHAR2,
				  P_MSG IN VARCHAR2);
PROCEDURE ENVIO_ACH_TRAMA(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_FECHA IN DATE,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMERO_LOTE IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TIPO_LOTE IN VARCHAR2,
				  P_CANAL IN VARCHAR2,
				  P_IN_OUT IN VARCHAR2,
				  P_TARJETA IN VARCHAR2,
				  P_STATUS IN VARCHAR2,
				  P_MONEDA IN NUMBER);
PROCEDURE ENVIO_ACH_TRAMA_CAB(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_FECHA IN DATE,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMERO_LOTE IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TIPO_LOTE IN VARCHAR2,
				  P_CANAL IN VARCHAR2,
				  P_IN_OUT IN VARCHAR2,
				  P_TARJETA IN VARCHAR2,
				  P_STATUS IN VARCHAR2,
				  P_MONEDA IN NUMBER );
 PROCEDURE REENVIO_ANTONY_TRAMA(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_USUARIO IN NUMBER,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMTRAP IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TARJETA IN VARCHAR2,
				  P_MSG IN VARCHAR2);
PROCEDURE REVERSA_ANTONY_TRAMA(P_OPERADOR IN NUMBER,--USUARIO QUE GRABO LA TRX EN FISA
                  P_NUMTRA IN NUMBER,--NUMERO DE TRX GENERADO EN FISA
				  P_USUARIO IN NUMBER,--USUARIO DE ANTONHY QUE RECIBIO EL PAGO
				  P_NUMTRAP IN NUMBER, --NUMTRA DE LA TRX GENERADA EN ANTONHY
				  P_TARJETA IN VARCHAR2,
				  P_MSG IN VARCHAR2);
PROCEDURE LOG_BITACORA(P_MSG IN VARCHAR2);
PROCEDURE DTE_BITACORA;
PROCEDURE P_TASADIVISACAJA(P_SUC IN NUMBER,
                           P_MONEDA IN NUMBER,
                           P_FECHA IN DATE,
                           P_TASACOMPRA OUT NUMBER,
                           P_TASAVENTA OUT NUMBER);
TYPE moneda_rec IS RECORD(P_CODMON  NUMBER(2),
                          P_NOMBRE VARCHAR2(40));
TYPE REFCURSOR IS REF CURSOR RETURN moneda_rec;
PROCEDURE P_CODMONEDAS(P_MONEDAS OUT REFCURSOR);
 PROCEDURE PCMS_RESPONSE_MM(p_codusr  IN NUMBER,
                         p_numtra  IN NUMBER,
                         p_sec IN NUMBER,
                         p_linea in VARCHAR2,
                         p_error   IN OUT VARCHAR2,
                         p_msgerror IN OUT VARCHAR2,
                         mensaje OUT VARCHAR2,
                         p_canal IN NUMBER,
                         P_CURRENCY IN NUMBER );
 PROCEDURE PCMS_RESPONSE_MM_MAS(p_codusr  IN NUMBER,
                         p_numtra  IN NUMBER,
                         p_sec IN NUMBER,
                         p_linea in VARCHAR2,
                         p_error   IN OUT VARCHAR2,
                         p_msgerror IN OUT VARCHAR2,
                         mensaje OUT VARCHAR2,
                         p_canal IN NUMBER,
                         P_CURRENCY IN NUMBER );
 PROCEDURE PCMS_RESPONSE_MM_ANT(p_codusr  IN NUMBER,
                         p_numtra  IN NUMBER,
                         p_sec IN NUMBER,
                         p_linea in VARCHAR2,
                         p_error   IN OUT VARCHAR2,
                         p_msgerror IN OUT VARCHAR2,
                         P_FORMAPAGO IN NUMBER, --EFECTIVO O CHEQUE
                         mensaje OUT VARCHAR2,
                         p_canal IN NUMBER,
                         P_CURRENCY IN NUMBER );
                                                                                                      				  				  				  				  				    						 						     						     						     						     						     						                       						 						                                                                                                                                                                                                                                                                                              
END; 