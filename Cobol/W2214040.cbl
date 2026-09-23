000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W2214040.                                            
000400 AUTHOR.             IDK, GÖTEBORG.                                       
000500 DATE-WRITTEN.       DEC  1978.                                           
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*            PROGRAMMET ÄR ETT SUBPROGRAM                                 
001100*            SOM KONTROLLERAR OM BESTÄLLNING/ANNULLATION BEHÖVS.          
001200*            I SÅ FALL SKAPAS EN FIL (W22141) SOM UNDERLAG FÖR            
001300*            FRAMSTÄLLNING AV BESTÄLLNINGSRAPPORT.(ALTERNATIVT            
001400*            BEGÄRS OMSPEC AV LEVERANSPLAN FÖR AVTALSARTIKLAR).           
001500*                                                                         
001600*                                                                         
001700*    SUBPROGRAM.                                                          
001800*            POSTSUM                                                      
001900*            W009VADD    ADD AV VECKOR TILL DATUM                         
002000*            W221LPAD    UPPDATERING AV TABELL KDLPORS-TAB                
002100*            DATKORT     DATUMKORT                                        
002200*                                                                         
002300* ---ÄNDRINGAR.                                                           
002400*    2014-03-31  E'TRACKER 8403120  BLOCKADE AVROP.BYT KOD 6 =            
002500*                ANN.HF MOT KOD 6 = LEV.BLOC - CPY W221W005               
002600*                KDLPORS-TABELL.                                          
002700*                                                                         
002710*    2015-04-22  E'TRACKER 10130993                                       
002720*                REDUCE NUMBER OF DELIVERY SCHEDULES                      
002800*                                                                         
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*                            *** UNDERLAG BESTÄLLNINGSRAPPORT  ***        
003500*                            *** UTFIL                         ***        
003600     SELECT  W22141  ASSIGN  UT-S-W22140D9.                               
003700*                            ***                               ***        
003800*                            *** UTFIL                         ***        
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W22141                                                               
004400     RECORDING F                                                          
004500     BLOCK 0                                                              
004600     LABEL RECORD STANDARD.                                               
004700*01  POST   -COPY W221LI41   -PRE U41BEST- -L.                            
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000     SKIP2                                                                
005100*    -COPY WY2000W3                                                       
005200     SKIP3                                                                
005300 01  W-PROGNAMN              PIC X(8)    VALUE 'W2214010'.                
005400 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
005500     SKIP3                                                                
005600 01  KONSTANTER.                                                          
005700     03  JA                  PIC X       VALUE 'J'.                       
005800     03  NEJ                 PIC X       VALUE 'N'.                       
005900     SKIP1                                                                
006000 01  SWICHAR.                                                             
006100     03 SW-W-KVANTAL-KOEP-BERAKN                                          
006200                             PIC X       VALUE 'N'.                       
006300     SKIP3                                                                
006310*    --- VARIABLER TILL SUBPROGRAM W221LPAD                               
006320 01  W-W221LP-CTX            PIC X(08) VALUE 'W221LP01'.                  
006330 01  W-KDLPORS-GRP.                                                       
006340     03 W-KDLPORS-TAB  OCCURS 4 PIC 9(3).                                 
006341     SKIP3                                                                
006400 01  ARBETSAREOR.                                                         
006500     03  W-TIOMSPEC          PIC S9(5)               COMP-3.              
006600     03  W-KVANTAL-TILLG     PIC S9(7)               COMP-3.              
006700     03  W-KVKP-EXTRA        PIC S9(7)               COMP-3.              
006800     03  W-KVPB              PIC S9(6)V9(3)          COMP-3.              
006900     03  W-ANTAL             PIC S9(3)               COMP-3.              
007000     03  W-N                 PIC S9(7)               COMP-3.              
007100     03  W-KVANTAL-KOEP      PIC S9(7)               COMP-3.              
007200 01  W-IDAVTAL-RED       PIC 9(13).                                       
007300 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
007400     03  FILLER          PIC X(1).                                        
007500     03  W-PREFIX        PIC X(3).                                        
007600     03  W-AVTALNR       PIC X(6).                                        
007700     03  W-SUFFIX        PIC X(3).                                        
007800 01  W-IDARTNR-8         PIC 9(8)        VALUE ZERO.                      
007900 01  DAGENS-DATUM        PIC 9(6).                                        
008000 01  DAGENS-DATUM-RED REDEFINES DAGENS-DATUM.                             
008100     03  W-AAR           PIC 9(2).                                        
008200     03  W-MAANAD        PIC 9(2).                                        
008300     03  W-DAG           PIC 9(2).                                        
008400     EJECT                                                                
008500*- - - - - - - - - - - - BYTES-ARTIKEL                                    
008600 01  FILLER                  PIC X(16)   VALUE 'BYTES-ARTIKEL'.           
008700 01  TEST-IDARTNR            PIC 9(9)    COMP-3.                          
008800*01  FILLER  -COPY WWBYT02 -RED TEST-IDARTNR.                             
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
009200     03  ABEND               PIC X(8)    VALUE 'ABEND  '.                 
009300     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
009400     03  W221LPAD            PIC X(8)    VALUE 'W221LPAD'.                
009500     03  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
009600     EJECT                                                                
009700*- - - - - - - - - - - - PARAMETRAR TILL DATUMKORT                        
009800 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
009900*01  -COPY WDATKORT                                                       
010000     EJECT                                                                
010100*    -COPY W0005      -PRE POSTSUM-                                       
010200     EJECT                                                                
010300*                            *************************************        
010400*                            ** POST FÖR FRAMSTÄLLNING AV       **        
010500*                            ** BESTÄLLNINGSRAPPORT             **        
010600*                            *************************************        
010700*01  AREA  -COPY W221LI41   -PRE U41BEST-.                                
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000     SKIP3                                                                
011100*                            *************************************        
011200*                            ** LÄNKAREA TILL HUVUDPROGRAM      **        
011300*                            **                                 **        
011400*                            *************************************        
011500*01  AREA  -COPY W221L401   -PRE LINK-.                                   
011600     EJECT                                                                
011700 PROCEDURE DIVISION  USING LINK-AREA.                                     
011800*                                                                         
011900     IF  LINK-OPPNA                                                       
012000     PERFORM A-INITIERA                                                   
012100     ELSE                                                                 
012200     IF  LINK-BEARBETA                                                    
012300     MOVE NEJ   TO SW-W-KVANTAL-KOEP-BERAKN                               
012400*                                                                         
012500     IF LINK-KDKSP = 2 AND LINK-KDAVT > ZERO                              
012600        MOVE ZERO TO LINK-KDKSP                                           
012700     END-IF                                                               
012800*                                                                         
012900     IF  LINK-KDKSP = ZERO                                                
013000       IF LINK-KDERS (1) > ZERO                                           
013100         PERFORM B-KONTROLL-ERSATTNING                                    
013200       ELSE                                                               
013300         PERFORM C-KONTROLL-ANNPKT-KOEPPKT                                
013400       END-IF                                                             
013500     END-IF                                                               
013600*                                                                         
013700     IF  LINK-KDKSP > 1                                                   
013800         PERFORM D-KONTROLL-PAMINNELSE                                    
013900     END-IF                                                               
014000*                                                                         
014100     ELSE                                                                 
014200     IF  LINK-AVSLUTA                                                     
014300     PERFORM E-AVSLUTA                                                    
014400     END-IF                                                               
014500     END-IF                                                               
014600     END-IF                                                               
014700*                                                                         
014800     MOVE ZERO TO RETURN-CODE                                             
014900     GOBACK                                                               
015000     .                                                                    
015100     EJECT                                                                
015200 A-INITIERA SECTION.                                                      
015300******************************************************************        
015400*                                                                *        
015500*                                                                *        
015600*                                                                *        
015700*                                                                *        
015800******************************************************************        
015900     SKIP1                                                                
016000     OPEN OUTPUT W22141                                                   
016100     MOVE 'W22140' TO POSTSUM-PROGNAMN                                    
016200     CALL DATKORT USING W-PROGNAMN DATUMKORT-ID DATUMKORT                 
016300     MOVE D-AAR TO W-AAR                                                  
016400     MOVE D-MAANAD TO W-MAANAD                                            
016500     MOVE D-DAG TO W-DAG                                                  
016600     .                                                                    
016700     EJECT                                                                
016800 B-KONTROLL-ERSATTNING SECTION.                                           
016900******************************************************************        
017000*                                                                *        
017100*                                                                *        
017200*                                                                *        
017300*                                                                *        
017400*                                                                *        
017500******************************************************************        
017600*                                                                         
017700     IF  (LINK-KDERS (1) > ZERO                                           
017800      AND LINK-KDERS (1) NOT > 29)                                        
017900         PERFORM BA-ERSATTNING-01-29                                      
018000     END-IF                                                               
018100*                                                                         
018200     IF  LINK-KDERS (1) = 52                                              
018300         PERFORM BB-ERSATTNING-52                                         
018400     END-IF                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 BA-ERSATTNING-01-29 SECTION.                                             
018800******************************************************************        
018900*                                                                *        
019000*                                                                *        
019100*                                                                *        
019200******************************************************************        
019300     SKIP1                                                                
019400     IF  LINK-KDERS (1) > ZERO AND < 09                                   
019500*                                                                         
019600       MOVE 'ERS '          TO POSTSUM-TRANSTYP                           
019700       MOVE 4               TO U41BEST-KDBEH-ORSAK                        
019800       MOVE NEJ             TO U41BEST-FLAGGA-PAAM                        
019900       MOVE ZERO            TO U41BEST-KVBEST-BER                         
020000       MOVE LINK-TIBESRPT   TO U41BEST-TIBESRPT-FOREG                     
020100       PERFORM S01-SKRIV-BESTALLNINGSPOST                                 
020200*                                                                         
020300         MOVE  4 TO LINK-KDKSP                                            
020400         MOVE LINK-TIAAVV-AKT TO LINK-TIBESRPT                            
020500                                 LINK-TIBESRPT-PAAM                       
020600         MOVE 10 TO W-ANTAL                                               
020700         CALL W009VADD USING LINK-TIBESRPT-PAAM  W-ANTAL                  
020800     ELSE                                                                 
020900         IF  LINK-KDERS (1) = 9                                           
021000     SKIP1                                                                
021100             IF  LINK-KVBR-TOT > ZERO                                     
021200*                                                                         
021300               MOVE 'ERS9'          TO POSTSUM-TRANSTYP                   
021400               MOVE 4               TO U41BEST-KDBEH-ORSAK                
021500               MOVE NEJ             TO U41BEST-FLAGGA-PAAM                
021600               MOVE ZERO            TO U41BEST-KVBEST-BER                 
021700               MOVE LINK-TIBESRPT   TO U41BEST-TIBESRPT-FOREG             
021800               PERFORM S01-SKRIV-BESTALLNINGSPOST                         
021900*                                                                         
022000             END-IF                                                       
022100             MOVE 1 TO LINK-KDKSP                                         
022200             MOVE LINK-TIAAVV-AKT TO LINK-TIBESRPT                        
022300         ELSE                                                             
022400             IF  LINK-KDERS (1) > 10                                      
022500                 MOVE 1    TO LINK-KDKSP                                  
022600             END-IF                                                       
022700         END-IF                                                           
022800     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 BB-ERSATTNING-52  SECTION.                                               
023200******************************************************************        
023300*                                                                *        
023400*                                                                *        
023500* SKAPA ANNULLATIONSRAPPORT VID 52-MÄRKNING (OM BEST.REST FINNS) *        
023600*                                                                *        
023700*                                                                *        
023800******************************************************************        
023900     SKIP1                                                                
024000     IF  LINK-KVBR-TOT > ZERO                                             
024100*                                                                         
024200       MOVE 'ANN '          TO POSTSUM-TRANSTYP                           
024300       MOVE 3               TO U41BEST-KDBEH-ORSAK                        
024400       MOVE NEJ             TO U41BEST-FLAGGA-PAAM                        
024500       MOVE ZERO            TO U41BEST-KVBEST-BER                         
024600       MOVE LINK-TIBESRPT   TO U41BEST-TIBESRPT-FOREG                     
024700       PERFORM S01-SKRIV-BESTALLNINGSPOST                                 
024800*                                                                         
024900       MOVE LINK-TIAAVV-AKT TO LINK-TIBESRPT                              
025000     END-IF                                                               
025100     MOVE 1 TO LINK-KDKSP                                                 
025200     .                                                                    
025300     EJECT                                                                
025400 C-KONTROLL-ANNPKT-KOEPPKT  SECTION.                                      
025500******************************************************************        
025600*                                                                *        
025700*                                                                *        
025800*                                                                *        
025900*                                                                *        
026000*                                                                *        
026100*                                                                *        
026200******************************************************************        
026300     SKIP1                                                                
026400     PERFORM S03-BERAKNA-TILLG                                            
026500     IF  LINK-KVBR-TOT > ZERO                                             
026600     AND (W-KVANTAL-TILLG - LINK-KVBEST-PL) > LINK-KVAP                   
026700         MOVE 'ANN '          TO POSTSUM-TRANSTYP                         
026800         MOVE 3               TO U41BEST-KDBEH-ORSAK                      
026900         MOVE NEJ             TO U41BEST-FLAGGA-PAAM                      
027000         MOVE ZERO            TO U41BEST-KVBEST-BER                       
027100         MOVE LINK-TIBESRPT   TO U41BEST-TIBESRPT-FOREG                   
027200         PERFORM S01-SKRIV-BESTALLNINGSPOST                               
027300                                                                          
027400         MOVE 3  TO LINK-KDKSP                                            
027500         MOVE LINK-TIAAVV-AKT TO LINK-TIBESRPT                            
027600                               LINK-TIBESRPT-PAAM                         
027700         MOVE 25 TO W-ANTAL                                               
027800         CALL W009VADD USING LINK-TIBESRPT-PAAM W-ANTAL                   
027900     ELSE                                                                 
028000         IF  LINK-KDAVT > ZERO                                            
028100         AND LINK-KDLPORS-TAB (1) NOT < 7                                 
028200         AND LINK-KDLPORS-TAB (1) NOT > 49                                
028300             COMPUTE W-KVKP-EXTRA =                                       
028400                             (LINK-KVPB-SEP (1))                          
028500                         /    2                                           
028600         ELSE                                                             
028700             MOVE ZERO TO W-KVKP-EXTRA                                    
028800         END-IF                                                           
028900*                                                                         
029000     IF  W-KVANTAL-TILLG < LINK-KVKP + W-KVKP-EXTRA                       
029100         IF  SW-W-KVANTAL-KOEP-BERAKN = NEJ                               
029200             PERFORM S02-BERAKN-FORSLAG-TILL-KOEP                         
029300         END-IF                                                           
029400         IF  LINK-KDAVT = 0                                               
029500*                                                                         
029600           MOVE 'BES '          TO POSTSUM-TRANSTYP                       
029700           MOVE 2               TO U41BEST-KDBEH-ORSAK                    
029800           MOVE NEJ             TO U41BEST-FLAGGA-PAAM                    
029900           MOVE W-KVANTAL-KOEP    TO U41BEST-KVBEST-BER                   
030000           MOVE LINK-TIBESRPT   TO U41BEST-TIBESRPT-FOREG                 
030100           PERFORM S01-SKRIV-BESTALLNINGSPOST                             
030200*                                                                         
030300           MOVE 2 TO LINK-KDKSP                                           
030400           MOVE LINK-TIAAVV-AKT TO LINK-TIBESRPT                          
030500                                   LINK-TIBESRPT-PAAM                     
030600           MOVE 10 TO W-ANTAL                                             
030700           CALL W009VADD USING LINK-TIBESRPT-PAAM W-ANTAL                 
030800         ELSE                                                             
030900           ADD  W-KVANTAL-KOEP       TO LINK-KVBEST-PL                    
031000           MOVE LINK-KDLPORS-TAB(01) TO W-KDLPORS-TAB(01)                 
031100           MOVE LINK-KDLPORS-TAB(02) TO W-KDLPORS-TAB(02)                 
031101           MOVE LINK-KDLPORS-TAB(03) TO W-KDLPORS-TAB(03)                 
031110           MOVE 01                   TO W-KDLPORS-TAB(04)                 
031116                                                                          
031120           CALL W221LPAD USING W-W221LP-CTX W-KDLPORS-GRP                 
031121                                                                          
031130           MOVE W-KDLPORS-TAB(01)    TO LINK-KDLPORS-TAB(01)              
031140           MOVE W-KDLPORS-TAB(02)    TO LINK-KDLPORS-TAB(02)              
031150           MOVE W-KDLPORS-TAB(03)    TO LINK-KDLPORS-TAB(03)              
031151                                                                          
031200         END-IF                                                           
031300     END-IF                                                               
031400     END-IF                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 D-KONTROLL-PAMINNELSE SECTION.                                           
031800******************************************************************        
031900*                                                                *        
032000*                                                                *        
032100*                                                                *        
032200*                                                                *        
032300*                                                                *        
032400******************************************************************        
032500     SKIP1                                                                
032600     MOVE LINK-TIBESRPT-PAAM   TO TMP1-YYWW                               
032700     MOVE LINK-TIAAVV-AKT      TO TMP2-YYWW                               
032800     PERFORM WY2000P3                                                     
032900     IF  TMP1-YYWW <= TMP2-YYWW                                           
033000     AND LINK-KDERS (1) < 7                                               
033100     OR LINK-KDKSP > 4                                                    
033200     OR LINK-KDKSP = +3                                                   
033300*                                                                         
033400     PERFORM S03-BERAKNA-TILLG                                            
033500     IF LINK-KDKSP = 2 AND W-KVANTAL-TILLG NOT < LINK-KVKP                
033600     AND LINK-KVKP > 0                                                    
033700     OR LINK-KDKSP = 3 AND                                                
033800              (W-KVANTAL-TILLG - LINK-KVBEST-PL NOT > LINK-KVAP           
033900               OR LINK-KVBR-TOT = 0)                                      
034000         MOVE ZERO TO LINK-KDKSP                                          
034100         MOVE ZERO TO LINK-TIBESRPT-PAAM                                  
034200     ELSE                                                                 
034300     MOVE LINK-TIBESRPT-PAAM   TO TMP1-YYWW                               
034400     MOVE LINK-TIAAVV-AKT      TO TMP2-YYWW                               
034500     PERFORM WY2000P3                                                     
034600     IF  TMP1-YYWW <= TMP2-YYWW                                           
034700     AND LINK-KDERS (1) < 7                                               
034800     OR LINK-KDKSP > 4                                                    
034900       IF  LINK-KDKSP = 2                                                 
035000          IF  SW-W-KVANTAL-KOEP-BERAKN = NEJ                              
035100              PERFORM S02-BERAKN-FORSLAG-TILL-KOEP                        
035200          END-IF                                                          
035300       ELSE                                                               
035400          MOVE ZERO TO W-KVANTAL-KOEP                                     
035500       END-IF                                                             
035600*                                                                         
035700       MOVE 'PAAM'          TO POSTSUM-TRANSTYP                           
035800       MOVE LINK-KDKSP      TO U41BEST-KDBEH-ORSAK                        
035900       MOVE JA              TO U41BEST-FLAGGA-PAAM                        
036000       MOVE W-KVANTAL-KOEP  TO U41BEST-KVBEST-BER                         
036100       MOVE LINK-TIBESRPT   TO U41BEST-TIBESRPT-FOREG                     
036200* (MANUELLT BEGÄRD RAPPORT)                                               
036300       IF LINK-KDKSP = 5 OR 6                                             
036400         MOVE 'BEG' TO POSTSUM-TRANSTYP                                   
036500         MOVE NEJ TO U41BEST-FLAGGA-PAAM                                  
036600         IF LINK-KDERS (1) = ZERO                                         
036700           SUBTRACT 3 FROM LINK-KDKSP                                     
036800         ELSE                                                             
036900           IF LINK-KDERS (1) < 9                                          
037000             MOVE 4 TO LINK-KDKSP                                         
037100           ELSE                                                           
037200             MOVE 1 TO LINK-KDKSP                                         
037300           END-IF                                                         
037400          END-IF                                                          
037500       PERFORM S01-SKRIV-BESTALLNINGSPOST                                 
037600        ELSE                                                              
037700*                                                                         
037800       IF (LINK-KVPB-SEP (1) > +0 )                                       
037900       IF  LINK-KDKSP NOT = +4                                            
038000       PERFORM S01-SKRIV-BESTALLNINGSPOST                                 
038100       END-IF                                                             
038200       END-IF                                                             
038300      END-IF                                                              
038400*                                                                         
038500       MOVE LINK-TIAAVV-AKT TO LINK-TIBESRPT                              
038600                                 LINK-TIBESRPT-PAAM                       
038700       IF LINK-KDKSP = +3                                                 
038800          MOVE +25 TO W-ANTAL                                             
038900       ELSE                                                               
039000         IF LINK-KDKSP = +2                                               
039100            MOVE +6 TO W-ANTAL                                            
039200         ELSE                                                             
039300       MOVE +5 TO W-ANTAL                                                 
039400       END-IF                                                             
039500       END-IF                                                             
039600       CALL W009VADD USING LINK-TIBESRPT-PAAM W-ANTAL                     
039700     END-IF                                                               
039800     END-IF                                                               
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200 E-AVSLUTA SECTION.                                                       
040300******************************************************************        
040400*                                                                *        
040500*                                                                *        
040600*                                                                *        
040700******************************************************************        
040800     SKIP1                                                                
040900     CLOSE W22141                                                         
041000     .                                                                    
041100     EJECT                                                                
041200 S01-SKRIV-BESTALLNINGSPOST SECTION.                                      
041300     SKIP3                                                                
041400     MOVE LINK-IDARTNR       TO U41BEST-IDARTNR                           
041500     MOVE LINK-IDANSK        TO U41BEST-IDANSK                            
041600     MOVE LINK-IDLEVNR       TO U41BEST-IDLEVNR                           
041700     MOVE LINK-KVBR-TOT      TO U41BEST-KVBR-TOT                          
041800     WRITE U41BEST-POST FROM U41BEST-AREA                                 
041900     SKIP1                                                                
042000     MOVE  'W22141'           TO POSTSUM-FDNAMN                           
042100     MOVE  'W22140D9'         TO POSTSUM-DDNAMN2                          
042200     CALL POSTSUM USING POSTSUM-PARM                                      
042300     .                                                                    
042400     EJECT                                                                
042500 S02-BERAKN-FORSLAG-TILL-KOEP SECTION.                                    
042600     SKIP1                                                                
042700     COMPUTE W-KVPB = LINK-KVPB-SEP (1)                                   
042800                   +  LINK-KVPB-SATS (1)                                  
042900     SKIP1                                                                
043000     IF  W-KVPB = ZERO                                                    
043100         COMPUTE W-KVANTAL-KOEP = LINK-KVKP - W-KVANTAL-TILLG             
043200     ELSE                                                                 
043300         IF  LINK-KVQ  > ZERO                                             
043400             COMPUTE W-N = 0.99 + (LINK-KVKP - LINK-KVBK / 2              
043500                                  -  W-KVANTAL-TILLG)                     
043600                                  /  LINK-KVQ                             
043700             COMPUTE W-KVANTAL-KOEP = LINK-KVBK + W-N * LINK-KVQ          
043800         ELSE                                                             
043900             COMPUTE W-KVANTAL-KOEP = LINK-KVBK / 2 + LINK-KVKP           
044000                                     - W-KVANTAL-TILLG                    
044100         END-IF                                                           
044200     END-IF                                                               
044300     IF W-KVANTAL-KOEP < LINK-KVBK                                        
044400        MOVE LINK-KVBK TO W-KVANTAL-KOEP                                  
044500     END-IF                                                               
044600     SKIP1                                                                
044700     MOVE JA TO SW-W-KVANTAL-KOEP-BERAKN                                  
044800     .                                                                    
044900     EJECT                                                                
045000 S03-BERAKNA-TILLG SECTION.                                               
045100     SKIP3                                                                
045200     COMPUTE W-KVANTAL-TILLG = LINK-KVLS (1)                              
045300             -   LINK-KVRESS (1)                                          
045400             +   LINK-KVAKS  (1)                                          
045500             -   LINK-KVROS  (1)                                          
045600             -   (LINK-KVOKS-BULK (1) + LINK-KVOKS-DAG (1) +              
045700                                        LINK-KVOKS-VOR (1))               
045800             +   LINK-KVLAAN                                              
045900             +   LINK-KVBR-TOT                                            
046000             +   LINK-KVBEST-PL                                           
046100             +   LINK-KVLS-SDC-OVER                                       
046200     SKIP1                                                                
046300     MOVE LINK-IDARTNR TO TEST-IDARTNR                                    
046400     .                                                                    
046500     EJECT                                                                
046600     EJECT                                                                
046700*    -COPY WY2000P3                                                       
