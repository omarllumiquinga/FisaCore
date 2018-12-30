CREATE OR REPLACE
FUNCTION FGETDOMSALDOVISTA(P_CTA IN NUMBER, P_FECHA IN DATE , p_accentry in number ,ANIGROUP     VARCHAR2) RETURN NUMBER IS
   -- Tipos de saldo
   GCCAPPRIN VARCHAR2(10) := 'CP';
   GCCAPVENC VARCHAR2(10) := 'CV';
   GCCAPNONA VARCHAR2(10) := 'CN';
   GCCAPBLOQ VARCHAR2(10) := 'CB';
   GCCAPPIGN VARCHAR2(10) := 'CG';
   GCCAPRETR VARCHAR2(10) := 'CR';
   GCCAPRETL VARCHAR2(10) := 'CL';
   GCSOBOCAC VARCHAR2(10) := 'SOCA';
   GCSOBCONT VARCHAR2(10) := 'SCON';
   GCSOBREME VARCHAR2(10) := 'SREM';
   GCSOBLOCA VARCHAR2(10) := 'SLOC';
      LNACCENTRYTYPE       NUMBER;
      LNBALTYPEEQUIV       NUMBER;
      LCPLIDEQUIV          VARCHAR2(10);
      LCNEWLEGACYACC       TLEG_AUXACCOUNT.ACCOUNT%TYPE;
      LCNEWPARENTLEGACYACC TLEG_AUXACCOUNT.ACCOUNT%TYPE;
      LNVALUE              NUMBER;
      LNLEVEL              NUMBER := 0;
      LNREESTRUCTUR        NUMBER := 2;
      LNINLEGAL            NUMBER := 2;
      LNCOLGROUP           NUMBER;
      LCBALTYPE            VARCHAR2(500);
      V_baltrcy number;
BEGIN
select 
       --nvl(sum(baltrcy *bdi_promedio(cy,BRANCH,trunc(DATEload))),0)
       nvl(sum(nvl(baltrcy,0)),0)
       into V_baltrcy
       from tgen_saldomodulos A
  where dateload = P_FECHA                  
    and acc = p_cta
    and  accentrytype=p_accentry
    and  BALTYPE = decode(p_accentry,2,'CP',BALTYPE)
    and  BALTYPE not in (select  C_BALTYPE FROM TDOM_CAUSALFALLECIDO
							union
							SELECT C_BALTYPE FROM TDOM_CAUSALEMBARGO
							union
							SELECT C_BALTYPE FROM TDOM_CAUSALOPOSICION
							union
							SELECT C_BALTYPE FROM TDOM_CAUSALPIGNORA
							union
							SELECT C_BALTYPE FROM TDOM_BALTYPE_NO_SALDO );
  return nvl(V_baltrcy,0);
END;                                                               

 
/
