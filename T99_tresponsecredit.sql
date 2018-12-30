CREATE OR REPLACE
TRIGGER cca_aurtcredit3_idw
  BEFORE INSERT OR UPDATE OF managebranch, manageoffice, creditstatus, canceldate,
                             opendate on tcredit
  FOR EACH ROW
DECLARE
  lvtoken     VARCHAR2(3000);
  lvtoken2    VARCHAR2(3000);
  lnobject    NUMBER := 1;

  lcname       VARCHAR2(40);
  lnproduct    NUMBER;
  lntype       NUMBER;
  lnrespnameid NUMBER;
  lcbalance    VARCHAR2(50);
  lnbalance    NUMBER;
  lnMon        tgen_cuenta.pro_mon%type;

BEGIN
  IF (updating AND (:new.managebranch != :old.managebranch OR :new.manageoffice != :old.manageoffice OR
      :new.creditstatus != :old.creditstatus     OR :new.canceldate!= :old.canceldate      )) OR
      inserting THEN
      ---
   		SELECT pro_nomcta, pro_pro, pro_tip, pro_codeje, pro_mon
  			INTO lcname, lnproduct, lntype, lnrespnameid, lnMon
  			FROM tgen_cuenta
  		 WHERE pro_mod = :new.module AND pro_cue = :new.credit;
      IF :new.purchaselimit IS NULL THEN
        BEGIN
          SELECT assignlimit
            INTO lcbalance
            FROM tcreditlimit
           WHERE client          = :new.client
             AND creditlimittype = 1
             AND datesince       = (SELECT MIN(datesince)
                                      FROM tcreditlimit
                                     WHERE client          = :new.client
                                       AND creditlimittype = 1);
        EXCEPTION
          WHEN OTHERS THEN
            lcbalance := 0;
        END;
      ELSE
        lcbalance := :new.purchaselimit;
      END IF;
      --- HD7213 se incrementa replace para el lcname
      lvtoken := 'fpopulatemetadata(aniseq => 1, acientity => whcg_global.gcentity, '||
                 'acientrycode     => whcg_global.entrycode, acistatus        => whcg_global.statusnew,'||
    						 'acisystemsource  => whcg_global.gcsystemsource, acinameinput     => whcg_global.gcnameinput'||
                 ', adidateinput     => SYSDATE, adidatetimeinput => SYSDATE, aciforminput     => whcg_global.gcforminput'||
                 ', acitableid => ''TACC'', aciacc => '''||lpad(:new.credit, 10, '0')||''', aciclient => '||:new.client||
                 ', acialid        => fgetalid('||:new.module||','||lnproduct||','||lntype||'), acibranch => '''||lpad(:new.managebranch,3,'0')||
                 ''', acioffice        => '''||lpad(:new.managebranch,3,'0')||lpad(:new.manageoffice,2,'0')||

                 ''', acistatusacc   => '''||lpad(:new.module,2,'0')||:new.creditstatus||''', acirespnameid  => fgetrespnameid('||lnrespnameid||')'||
                 ', adiaccclose      => '''||to_char(:new.canceldate,'yyyy/mm/dd')||''', adidateeff       => null '||
                 ', adimat           => null, adiaccopen       => '''||to_char(:new.opendate,'yyyy/mm/dd')||
                 ''', aciaccname     => '''||nvl(REPLACE(lcname,'''',''''''), lpad(:new.credit, 10, '0'))||''', acimnem    => '''||
                 nvl(REPLACE(substr(lcname,1,32),'''',''''''), lpad(:new.credit, 10, '0'))||''', acifundssource   =>  1'||
    						 ', acifundsuse      =>   null, aciguartype      => '''||:new.gartype||
                 ''', acirating        => null'||
                 --
                 ', acisalechannel   => NULL'||
                 ', aciseller        => NULL'||
                 ', aniquotanumber   => NULL'||
                 ', anipaidquota     => NULL'||
                 ', aciassignor      => NULL'||
                 ', aniquotavalue    => NULL'||
                 ', aciaccclass      => fgetaccclass('||:new.module||','||lnproduct||','||lntype||','||lnmon||')';

    IF inserting THEN
      lvtoken := lvtoken||', acisec           => NULL,	aciportf         => NULL, adirenewaldate   => NULL, anicurrnum       => 1'||
                 ', aninoticedays    => NULL, acitimeorig      => NULL, acitimeremainc   => NULL,'||
  							 'acitimeremainp   => NULL, acicollattype    => NULL,	aniacctype       => 1, anicyclausetype  => NULL,'||
                 ', aciappid         => '''||lpad(:new.module,2,'0')||lpad(lnproduct,2,'0')||lpad(lntype,2,'0')||''', acidept          => NULL,'||
                 'aciratingexternal=> NULL, aniduration => NULL, acipayagree => NULL, aciaccorig  => NULL';

    END IF;
    lvtoken := lvtoken||');';

    BEGIN
      INSERT INTO tidw_trail(tra_dateinput,
                             tra_seq,
                             tra_priority,
                             tra_object,
                             tra_trail,
                             tra_process)
                      VALUES(f_fechatrabajo,
                             sidw_trail.NEXTVAL,
                             13,
                             lnobject,
                             lvtoken,
                             2);
      IF inserting THEN
         FOR racccy IN (SELECT tm.tpm_mon
                   FROM tgen_tipmon tm
                   WHERE tm.tpm_mod = :new.module
                    AND tm.tpm_pro = lnproduct
                    AND tm.tpm_tip = lntype ) LOOP
            IF lnmon = racccy.tpm_mon THEN
              lnbalance := lcbalance;
            ELSE
              lnbalance := 0;
            END IF;
            lvtoken2 := 'fpopulatemetadata(aniseq           => 1,	acientity        => whcg_global.gcentity,	acitableid       => ''TACCCY'','||
        								'acientrycode     => whcg_global.entrycode,	acistatus        => whcg_global.statusnew, acisystemsource  => whcg_global.gcsystemsource,'||
        								'acinameinput     => whcg_global.gcnameinput,	adidateinput     => SYSDATE, adidatetimeinput => SYSDATE,'||
        								'aciforminput     => whcg_global.gcforminput,	aciacc           => '''||lpad(:new.credit, 10, '0')||', anibalanceopen   => '||lnbalance||
                        ''', anicyacc         => '||to_char(racccy.tpm_mon)||', aniamtreceive => 0);';

            INSERT INTO tidw_trail(tra_dateinput,
                                   tra_seq,
                                   tra_priority,
                                   tra_object,
                                   tra_trail,
                                   tra_process)
                            VALUES(f_fechatrabajo,
                                   sidw_trail.NEXTVAL,
                                   14,
                                   lnobject,
                                   lvtoken2,
                                   2);
         END LOOP ;
      END IF;
    END;
  END IF;
  IF updating AND :new.creditstatus = '6' THEN    --si se actualizo el status a cancelada
   		INSERT INTO tidw_trail
  			(tra_dateinput, tra_seq, tra_priority, tra_object, tra_trail,
  			 tra_process, tra_mess)
  		VALUES
  			(f_fechatrabajo, sidw_trail.NEXTVAL, 21, 3, lpad(:new.credit, 10, '0')||','||lnMon, 2, NULL);
  END IF;
END cca_aurtcredit3_idw;
/
