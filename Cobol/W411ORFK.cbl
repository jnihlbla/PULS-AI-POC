000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W411ORFK.                                                
000400 AUTHOR.         LARS THELL CAP GEMINI LOCIC.                             
000500 DATE-WRITTEN.   MAJ   -90.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*        PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                       
000900*                                                                         
001000*    FUNKTION.                                                            
001100*      - PROGRAMMET GÖR EN FORMELL KONTROLL AV                            
001200*        SAMTLIGA FÄLT PÅ ORDERRADERNA (14 ST) 4212                       
001300*        OM NÅGOT ELLER NÅGRA FEL UPPTÄCKS, FELMÄRKS                      
001400*        DE AKTUELLA TERMERNA.                                            
001500*                                                                         
001600*        OM ALLT ÄR OK HÄMTAS ARTIKELINFORMATION FÖR GIVNA                
001700*        ARTIKELNUMMER W411AREG                                           
001800*                                                                         
001900*        LÄNKAREA: W411ORFKC0.                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP3                                                                
002400                                                                          
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700*    -COPY WY2000W1                                                       
002800     SKIP3                                                                
002900 77  IDPGM                       PIC X(08)   VALUE 'W411ORFK'.            
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
003300                                                                          
003400*   ----- ARBETSFÄLT                                                      
003500 77  FILLER                      PIC X(08)   VALUE 'AAAAAAAA'.            
003600 01  W-IDARTNR.                                                           
003700      05 W-IDARTNR-1-9           PIC X(9).                                
003800      05 W-IDARTNR-10            PIC X(1).                                
003900      05 W-IDARTNR-11            PIC X(1).                                
004000                                                                          
004100                                                                          
004200 77  W-DATE                      PIC X(6).                                
004300 77  W-IDARTNR-NUM               PIC 9(9).                                
004400 77  W-KDORDKL-NUM               PIC 9(1).                                
004500 77  W-KDTPOTYP-NUM              PIC 9(1).                                
004600 77  W-TITPO-NUM                 PIC 9(7).                                
004700                                                                          
004800 77  MAX-IX                      PIC S9(3)   COMP-3.                      
004900                                                                          
005000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005100 77  FILLER                      PIC X(08)   VALUE 'FELTEXT:'.            
005200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005300                                                                          
005400 01  WS-SOEK-IDDISTR             PIC 9(5).                                
005500                                                                          
005600*      --- VALID IDDC CODES                                               
005700*                                                                         
005800 77  FILLER                      PIC X(08)   VALUE 'BBBBBBBB'.            
005900*01    -COPY WWDC99                                                       
006000       EJECT                                                              
006100*                                                                         
006200*01    -COPY WWDC03                                                       
006300       EJECT                                                              
006400 77  FILLER                      PIC X(08)   VALUE 'TESTDIST'.            
006500 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
006600*01 FILLER    -COPY WWDIST23      -RED  TEST-IDDISTR.                     
006700     EJECT                                                                
006800*01 FILLER    -COPY WWDIST35      -RED  TEST-IDDISTR.                     
006900     EJECT                                                                
007000*01 FILLER    -COPY WWDIST40      -RED  TEST-IDDISTR.                     
007100     EJECT                                                                
007200*01 FILLER    -COPY WWDIST88      -RED  TEST-IDDISTR.                     
007300     EJECT                                                                
007400*   ----- SUBPROGRAM                                                      
007500                                                                          
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03 W009KSIF                 PIC X(8)     VALUE 'W009KSIF'.           
007800     03 WSECURIT                 PIC X(8)     VALUE 'WSECURIT'.           
007900     03 WDATKONV                 PIC X(8)     VALUE 'WDATKONV'.           
008000     03 WDECEDIT                 PIC X(8)     VALUE 'WDECEDIT'.           
008100     03 W411AREG                 PIC X(8)     VALUE 'W411AREG'.           
008200     03 FELLOG                   PIC X(8)     VALUE 'FELLOG  '.           
008300                                                                          
008400*   ----- PARAMETRAR TILL SUBPROGRAM                                      
008500                                                                          
008600 01  W009KSIF-PARM.                                                       
008700     03 FLT                      PIC 9(9).                                
008800     03 LGD                      PIC 9(1).                                
008900     03 KSIFF                    PIC 9(1).                                
009000                                                                          
009100*                                                                         
009200 77  FILLER                      PIC X(08)   VALUE 'WSECAREA'.            
009300*   -COPY WSECAREA                                                        
009400*                                                                         
009500     EJECT                                                                
009600*                                                                         
009700 77  FILLER                      PIC X(08)   VALUE 'WDATAREA'.            
009800*   -COPY WDATAREA                                                        
009900*                                                                         
010000     EJECT                                                                
010100*                                                                         
010200 77  FILLER                      PIC X(08)   VALUE 'WDECAREA'.            
010300*   -COPY WDECAREA                                                        
010400*                                                                         
010500     EJECT                                                                
010600*                                                                         
010700 77  FILLER                      PIC X(08)   VALUE 'AREGAREA'.            
010800*   -COPY W411AREG                                                        
010900*                                                                         
011000     EJECT                                                                
011100*   ----- SWITCHAR                                                        
011200 77  FILLER                      PIC X(08)   VALUE 'SWITCHAR'.            
011300 77  FORMELLT-FEL-SW             PIC X       VALUE 'N'.                   
011400     88 FORMELLT-FEL                         VALUE 'J'.                   
011500     EJECT                                                                
011600                                                                          
011700 77  FILLER                      PIC X(08)   VALUE 'ORFKAREA'.            
011800*   ----- INDEXFÄLT                                                       
011900 77  IX                          PIC S9(9)   VALUE +0 COMP SYNC.          
012000     EJECT                                                                
012100                                                                          
012200 LINKAGE SECTION.                                                         
012300*                                                                         
012400*   -COPY W411ORFK                                                        
012500*                                                                         
012600     EJECT                                                                
012700*                                                                         
012800 01  AREG-WDK6-PCB          PIC X.                                        
012900 01  AREG-WDK7-PCB          PIC X.                                        
013000*                                                                         
013100     EJECT                                                                
013200                                                                          
013300 PROCEDURE DIVISION  USING ORFK-W411ORFK                                  
013400                           AREG-WDK6-PCB                                  
013500                           AREG-WDK7-PCB.                                 
013600                                                                          
013700                                                                          
013800 STYR SECTION.                                                            
013900                                                                          
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     PERFORM B-FORMELL-KONTROLL                                           
014300                                                                          
014400     IF FORMELLT-FEL AND (ORFK-IDSYSTEM = 'IMS ' OR                       
014500                                          'PROF' OR                       
014600                                          'OREL')                         
014700         CONTINUE                                                         
014800      ELSE                                                                
014900         PERFORM C-ARTIKEL-KONTROLL                                       
015000     END-IF                                                               
015100                                                                          
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500                                                                          
015600 A-INIT              SECTION.                                             
015700                                                                          
015800     MOVE ORFK-IDDISTR         TO TEST-IDDISTR                            
015900     MOVE ORFK-IDDC            TO WS-IDDC                                 
016000     MOVE NEJ                  TO FORMELLT-FEL-SW                         
016100                                                                          
016200     IF ORFK-IDSYSTEM = 'WEB'                                             
016300       MOVE 'IMS'              TO ORFK-IDSYSTEM                           
016400       MOVE +100               TO MAX-IX                                  
016500     ELSE                                                                 
016600       MOVE +14                TO MAX-IX                                  
016700     END-IF                                                               
016800                                                                          
016900     MOVE +1                   TO IX                                      
017000     PERFORM UNTIL IX          > MAX-IX                                   
017100         MOVE JA               TO  ORFK-FLINVEST-OK(IX)                   
017200                                   ORFK-FLRESTN-OK(IX)                    
017300                                   ORFK-FLSLATT-OK(IX)                    
017400                                   ORFK-IDARTNR-OK(IX)                    
017500                                   ORFK-IDKONTO-OK(IX)                    
017600                                   ORFK-IDKST-OK(IX)                      
017700                                   ORFK-KDKVBRYT-OK(IX)                   
017800                                   ORFK-KDVRINFO-OK(IX)                   
017900                                   ORFK-KVBEART-OK(IX)                    
018000                                   ORFK-PRARTNTO-OK(IX)                   
018100                                   ORFK-TITPO-OK(IX)                      
018200                                   ORFK-PRARTNTO-LOC-OK(IX)               
018300                                   ORFK-PRARTNTO-LOCPREL-OK(IX)           
018400                                   ORFK-PRARTBTO-LOC-OK(IX)               
018500         ADD +1                TO IX                                      
018600     END-PERFORM                                                          
018700     .                                                                    
018800     EJECT                                                                
018900 B-FORMELL-KONTROLL  SECTION.                                             
019000                                                                          
019100     MOVE +1                   TO IX                                      
019200     PERFORM UNTIL IX          > MAX-IX                                   
019300         IF ORFK-IDARTNR-IN(IX) = ALL '+'                                 
019400             PERFORM BA-KONTROLLERA-IDARTNR-KVBEART                       
019500          ELSE                                                            
019600             PERFORM BB-KONTROLLERA-FLINVEST                              
019700                                                                          
019800             PERFORM BC-KONTROLLERA-FLRESTN                               
019900                                                                          
020000             PERFORM BD-KONTROLLERA-IDARTNR                               
020100                                                                          
020200             IF ORFK-KDKVBRYT(IX) = ALL '+'                               
020300                 CONTINUE                                                 
020400              ELSE                                                        
020500                 PERFORM BG-KONTROLLERA-KDKVBRYT                          
020600             END-IF                                                       
020700                                                                          
020800             PERFORM BH-KONTROLLERA-KDVRINFO                              
020900                                                                          
021000             PERFORM BI-KONTROLLERA-KVBEART                               
021100                                                                          
021200             IF ORFK-PRARTNTO(IX) = ALL '+'                               
021300                 CONTINUE                                                 
021400              ELSE                                                        
021500                 PERFORM BJ-KONTROLLERA-PRARTNTO                          
021600             END-IF                                                       
021700                                                                          
021800             PERFORM BK-KONTROLLERA-TITPO                                 
021900                                                                          
022000             PERFORM BL-KONTROLLERA-FLSLATT                               
022100                                                                          
022200             IF ORFK-PRARTNTO-LOC(IX) = ALL '+'                           
022300                 CONTINUE                                                 
022400             ELSE                                                         
022500                 PERFORM BM-KONTROLLERA-PRARTNTO-LOC                      
022600             END-IF                                                       
022700                                                                          
022800             IF ORFK-PRARTNTO-LOCPREL(IX) = ALL '+'                       
022900                 CONTINUE                                                 
023000             ELSE                                                         
023100                 PERFORM BN-KONTR-PRARTNTO-LOCPREL                        
023200             END-IF                                                       
023300                                                                          
023400             IF ORFK-PRARTBTO-LOC(IX) = ALL '+'                           
023500                 CONTINUE                                                 
023600             ELSE                                                         
023700                 PERFORM BO-KONTROLLERA-PRARTBTO-LOC                      
023800             END-IF                                                       
023900                                                                          
024000         END-IF                                                           
024100         ADD +1                TO IX                                      
024200     END-PERFORM                                                          
024300     .                                                                    
024400     EJECT                                                                
024500                                                                          
024600 BA-KONTROLLERA-IDARTNR-KVBEART  SECTION.                                 
024700                                                                          
024800     IF ORFK-KVBEART(IX)       NOT NUMERIC                                
024900         CONTINUE                                                         
025000      ELSE                                                                
025100         MOVE JA               TO FORMELLT-FEL-SW                         
025200         MOVE NEJ              TO ORFK-IDARTNR-OK(IX)                     
025300     END-IF                                                               
025400     .                                                                    
025500     EJECT                                                                
025600 BB-KONTROLLERA-FLINVEST  SECTION.                                        
025700                                                                          
025800     IF ORFK-FLINVEST(IX)      = JA OR NEJ                                
025900         CONTINUE                                                         
026000      ELSE                                                                
026100         MOVE JA               TO FORMELLT-FEL-SW                         
026200         MOVE NEJ              TO ORFK-FLINVEST-OK(IX)                    
026300     END-IF                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 BC-KONTROLLERA-FLRESTN   SECTION.                                        
026700                                                                          
026800     IF ORFK-FLRESTN(IX)       = JA OR NEJ                                
026900         CONTINUE                                                         
027000      ELSE                                                                
027100         MOVE JA               TO FORMELLT-FEL-SW                         
027200         MOVE NEJ              TO ORFK-FLRESTN-OK(IX)                     
027300     END-IF                                                               
027400                                                                          
027500     IF ORFK-FLRESTN(IX) = JA AND ORFK-KDORDKL = +0                       
027600         MOVE JA               TO FORMELLT-FEL-SW                         
027700         MOVE NEJ              TO ORFK-FLRESTN-OK(IX)                     
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 BD-KONTROLLERA-IDARTNR   SECTION.                                        
028200                                                                          
028300     MOVE ZERO                 TO ORFK-KDORDBEK(IX)                       
028400     MOVE ORFK-IDARTNR-IN(IX)  TO W-IDARTNR                               
028500     IF ORFK-IDARTNR-IN(IX)    NOT NUMERIC                                
028600         IF W-IDARTNR-1-9      NUMERIC   AND                              
028700            W-IDARTNR-10       = '-'     AND                              
028800            W-IDARTNR-11       NUMERIC                                    
028900             MOVE W-IDARTNR-1-9    TO  FLT                                
029000             MOVE +9               TO  LGD                                
029100             PERFORM S01-CALL-W009KSIF                                    
029200             IF KSIFF              NOT = W-IDARTNR-11                     
029300                 MOVE 59           TO ORFK-KDORDBEK(IX)                   
029400             END-IF                                                       
029500             MOVE W-IDARTNR-1-9 TO W-IDARTNR-NUM                          
029600             MOVE W-IDARTNR-NUM TO ORFK-IDARTNR(IX)                       
029700         ELSE                                                             
029800             MOVE JA               TO FORMELLT-FEL-SW                     
029900             MOVE NEJ              TO ORFK-IDARTNR-OK(IX)                 
030000         END-IF                                                           
030100     ELSE                                                                 
030200         MOVE W-IDARTNR (3:9)      TO W-IDARTNR-NUM                       
030300         MOVE W-IDARTNR-NUM        TO ORFK-IDARTNR(IX)                    
030400     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 BG-KONTROLLERA-KDKVBRYT  SECTION.                                        
030800                                                                          
030900     IF ORFK-IDSYSTEM = 'PROF'                                            
031000        IF ORFK-KDKVBRYT(IX) NUMERIC AND                                  
031100           ORFK-KDKVBRYT(IX) < '2'                                        
031200           CONTINUE                                                       
031300        ELSE                                                              
031400           MOVE JA             TO FORMELLT-FEL-SW                         
031500           MOVE NEJ            TO ORFK-KDKVBRYT-OK(IX)                    
031600        END-IF                                                            
031700     ELSE                                                                 
031800        IF ORFK-KDKVBRYT(IX) NUMERIC AND                                  
031900           ORFK-KDKVBRYT(IX) < '3'                                        
032000           CONTINUE                                                       
032100        ELSE                                                              
032200           MOVE JA             TO FORMELLT-FEL-SW                         
032300           MOVE NEJ            TO ORFK-KDKVBRYT-OK(IX)                    
032400        END-IF                                                            
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 BH-KONTROLLERA-KDVRINFO  SECTION.                                        
032900                                                                          
033000     IF ORFK-KDVRINFO(IX)      NUMERIC AND                                
033100        ORFK-KDVRINFO(IX)      < '3'                                      
033200         CONTINUE                                                         
033300      ELSE                                                                
033400         MOVE JA               TO FORMELLT-FEL-SW                         
033500         MOVE NEJ              TO ORFK-KDVRINFO-OK(IX)                    
033600     END-IF                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 BI-KONTROLLERA-KVBEART   SECTION.                                        
034000                                                                          
034100     IF ORFK-KVBEART(IX)       = ALL '+'                                  
034200         MOVE JA               TO FORMELLT-FEL-SW                         
034300         MOVE NEJ              TO ORFK-KVBEART-OK(IX)                     
034400     ELSE                                                                 
034500        IF ORFK-KVBEART(IX)   NUMERIC                                     
034600           IF ORFK-KVBEART(IX) = ZERO                                     
034700              IF (ORFK-IDSYSTEM = 'VR  ' AND ORFK-KDTPOTYP = '1')         
034800                 OR (DIST23-TPO1 AND                                      
034900                  ORFK-IDSYSTEM = 'OVR ' AND ORFK-KDTPOTYP = '1')         
035000                 CONTINUE                                                 
035100              ELSE                                                        
035200                 MOVE JA       TO FORMELLT-FEL-SW                         
035300                 MOVE NEJ      TO ORFK-KVBEART-OK(IX)                     
035400              END-IF                                                      
035500           END-IF                                                         
035600        ELSE                                                              
035700           MOVE JA             TO FORMELLT-FEL-SW                         
035800           MOVE NEJ            TO ORFK-KVBEART-OK(IX)                     
035900        END-IF                                                            
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 BJ-KONTROLLERA-PRARTNTO  SECTION.                                        
036400                                                                          
036500     IF ORFK-IDSYSTEM          = 'IMS ' OR                                
036600                               'PROF'                                     
036700         MOVE ORFK-IDUSER      TO SEC-IDUSER                              
036800         MOVE '4212'           TO SEC-IDTRANS                             
036900         MOVE ORFK-IDDISTR         TO SEC-IDKEY                           
037000         PERFORM S02-CALL-WSECURIT                                        
037100         IF SEC-KDSVAR         NOT = SPACE                                
037200             MOVE JA           TO FORMELLT-FEL-SW                         
037300             MOVE NEJ          TO ORFK-PRARTNTO-OK(IX)                    
037400          ELSE                                                            
037500             MOVE ORFK-PRARTNTO(IX) TO DEC-IDFRIDATA                      
037600             MOVE +7           TO DEC-KVHELTAL                            
037700             MOVE +2           TO DEC-KVDECIMAL                           
037800             PERFORM S05-CALL-WDECEDIT                                    
037900             IF DEC-KDSVAR-FEL                                            
038000                 MOVE JA       TO FORMELLT-FEL-SW                         
038100                 MOVE NEJ      TO ORFK-PRARTNTO-OK(IX)                    
038200              ELSE                                                        
038300                 MOVE DEC-IDEDITDATA TO ORFK-PRARTNTO-UT(IX)              
038400             END-IF                                                       
038500         END-IF                                                           
038600      ELSE                                                                
038700         MOVE ORFK-PRARTNTO(IX) TO DEC-IDFRIDATA                          
038800         MOVE +7               TO DEC-KVHELTAL                            
038900         MOVE +2               TO DEC-KVDECIMAL                           
039000         PERFORM S05-CALL-WDECEDIT                                        
039100         IF DEC-KDSVAR-FEL                                                
039200             MOVE JA           TO FORMELLT-FEL-SW                         
039300             MOVE NEJ          TO ORFK-PRARTNTO-OK(IX)                    
039400          ELSE                                                            
039500             MOVE DEC-IDEDITDATA TO ORFK-PRARTNTO-UT(IX)                  
039600         END-IF                                                           
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000 BK-KONTROLLERA-TITPO     SECTION.                                        
040100                                                                          
040200     IF ORFK-TITPO-RAD(IX) NOT = ALL '+'                                  
040300        IF ORFK-KDORDKL = +0                                              
040400           MOVE JA   TO FORMELLT-FEL-SW                                   
040500           MOVE NEJ  TO ORFK-TITPO-OK(IX)                                 
040600        ELSE                                                              
040700          IF ORFK-TITPO-RAD(IX)     NOT = SPACE                           
040800              IF ORFK-TITPO-RAD(IX) NUMERIC                               
040900                  MOVE 'AAMMDD'     TO DAT-KDDATFORM                      
041000                  MOVE ORFK-TITPO-RAD(IX) TO DAT-I-TIDATUM                
041100                  PERFORM S03-CALL-WDATKONV                               
041200                  IF DAT-KDSVAR-FEL                                       
041300                      MOVE JA       TO FORMELLT-FEL-SW                    
041400                      MOVE NEJ      TO ORFK-TITPO-OK(IX)                  
041500                  ELSE                                                    
041600                    ACCEPT W-DATE FROM DATE                               
041700                    MOVE W-DATE                 TO TMP1-YYMMDD            
041800                    MOVE ORFK-TITPO-RAD(IX)     TO TMP2-YYMMDD            
041900                    PERFORM WY2000P1                                      
042000                    IF ORFK-KDTPOTYP = 1 AND ORFK-IDSYSTEM = 'VR'         
042100                      IF TMP1-YYMMDD > TMP2-YYMMDD                        
042200                        MOVE JA   TO FORMELLT-FEL-SW                      
042300                        MOVE NEJ  TO ORFK-TITPO-OK(IX)                    
042400                      END-IF                                              
042500                    ELSE                                                  
042600                      IF TMP1-YYMMDD >= TMP2-YYMMDD                       
042700                        MOVE JA   TO FORMELLT-FEL-SW                      
042800                        MOVE NEJ  TO ORFK-TITPO-OK(IX)                    
042900                      END-IF                                              
043000                    END-IF                                                
043100                  END-IF                                                  
043200               ELSE                                                       
043300                  MOVE JA           TO FORMELLT-FEL-SW                    
043400                  MOVE NEJ          TO ORFK-TITPO-OK(IX)                  
043500              END-IF                                                      
043600          ELSE                                                            
043700              MOVE JA           TO FORMELLT-FEL-SW                        
043800              MOVE NEJ          TO ORFK-TITPO-OK(IX)                      
043900          END-IF                                                          
044000        END-IF                                                            
044100     ELSE                                                                 
044200        IF (ORFK-KDTPOTYP = +1 OR +3)  OR                                 
044300           (ORFK-KDTPOTYP = +2 AND ORFK-IDSYSTEM = 'VIPS') OR             
044400           (ORFK-KDTPOTYP = +2 AND ORFK-IDSYSTEM = 'OVR ')                
044500                                                                          
044600           MOVE JA           TO FORMELLT-FEL-SW                           
044700           MOVE NEJ          TO ORFK-TITPO-OK(IX)                         
044800        END-IF                                                            
044900     END-IF                                                               
045000     .                                                                    
045100     EJECT                                                                
045200 BL-KONTROLLERA-FLSLATT   SECTION.                                        
045300                                                                          
045400     IF ORFK-FLRESTN(IX)       = JA     AND                               
045500        (ORFK-FLSLATT(IX)  NOT = JA AND NEJ)                              
045600         MOVE JA               TO FORMELLT-FEL-SW                         
045700         MOVE NEJ              TO ORFK-FLSLATT-OK(IX)                     
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 BM-KONTROLLERA-PRARTNTO-LOC SECTION.                                     
046200                                                                          
046300     IF ORFK-IDSYSTEM          = 'IMS ' OR                                
046400                               'PROF'                                     
046500         MOVE ORFK-IDUSER      TO SEC-IDUSER                              
046600         MOVE '4212'           TO SEC-IDTRANS                             
046700         MOVE ORFK-IDDISTR         TO SEC-IDKEY                           
046800         PERFORM S02-CALL-WSECURIT                                        
046900         IF SEC-KDSVAR         NOT = SPACE                                
047000             MOVE JA           TO FORMELLT-FEL-SW                         
047100             MOVE NEJ          TO ORFK-PRARTNTO-LOC-OK(IX)                
047200          ELSE                                                            
047300             MOVE ORFK-PRARTNTO-LOC(IX) TO DEC-IDFRIDATA                  
047400             MOVE +7           TO DEC-KVHELTAL                            
047500             MOVE +2           TO DEC-KVDECIMAL                           
047600             PERFORM S05-CALL-WDECEDIT                                    
047700             IF DEC-KDSVAR-FEL                                            
047800                 MOVE JA       TO FORMELLT-FEL-SW                         
047900                 MOVE NEJ      TO ORFK-PRARTNTO-LOC-OK(IX)                
048000              ELSE                                                        
048100                 MOVE DEC-IDEDITDATA TO ORFK-PRARTNTO-LOC-UT(IX)          
048200             END-IF                                                       
048300         END-IF                                                           
048400      ELSE                                                                
048500         MOVE ORFK-PRARTNTO-LOC(IX) TO DEC-IDFRIDATA                      
048600         MOVE +7               TO DEC-KVHELTAL                            
048700         MOVE +2               TO DEC-KVDECIMAL                           
048800         PERFORM S05-CALL-WDECEDIT                                        
048900         IF DEC-KDSVAR-FEL                                                
049000             MOVE JA           TO FORMELLT-FEL-SW                         
049100             MOVE NEJ          TO ORFK-PRARTNTO-LOC-OK(IX)                
049200          ELSE                                                            
049300             MOVE DEC-IDEDITDATA TO ORFK-PRARTNTO-LOC-UT(IX)              
049400         END-IF                                                           
049500     END-IF                                                               
049600     .                                                                    
049700     EJECT                                                                
049800 BN-KONTR-PRARTNTO-LOCPREL SECTION.                                       
049900                                                                          
050000     IF ORFK-IDSYSTEM          = 'IMS ' OR                                
050100                               'PROF'                                     
050200        MOVE ORFK-IDUSER      TO SEC-IDUSER                               
050300        MOVE '4212'           TO SEC-IDTRANS                              
050400        MOVE ORFK-IDDISTR         TO SEC-IDKEY                            
050500        PERFORM S02-CALL-WSECURIT                                         
050600        IF SEC-KDSVAR         NOT = SPACE                                 
050700           MOVE JA           TO FORMELLT-FEL-SW                           
050800           MOVE NEJ          TO ORFK-PRARTNTO-LOCPREL-OK(IX)              
050900        ELSE                                                              
051000           MOVE ORFK-PRARTNTO-LOCPREL(IX) TO DEC-IDFRIDATA                
051100           MOVE +7           TO DEC-KVHELTAL                              
051200           MOVE +2           TO DEC-KVDECIMAL                             
051300           PERFORM S05-CALL-WDECEDIT                                      
051400           IF DEC-KDSVAR-FEL                                              
051500              MOVE JA       TO FORMELLT-FEL-SW                            
051600              MOVE NEJ      TO ORFK-PRARTNTO-LOCPREL-OK(IX)               
051700           ELSE                                                           
051800              MOVE DEC-IDEDITDATA TO ORFK-PRARTNTO-LOCPREL-UT(IX)         
051900           END-IF                                                         
052000        END-IF                                                            
052100      ELSE                                                                
052200         MOVE ORFK-PRARTNTO-LOCPREL(IX) TO DEC-IDFRIDATA                  
052300         MOVE +7               TO DEC-KVHELTAL                            
052400         MOVE +2               TO DEC-KVDECIMAL                           
052500         PERFORM S05-CALL-WDECEDIT                                        
052600         IF DEC-KDSVAR-FEL                                                
052700            MOVE JA           TO FORMELLT-FEL-SW                          
052800            MOVE NEJ          TO ORFK-PRARTNTO-LOCPREL-OK(IX)             
052900          ELSE                                                            
053000            MOVE DEC-IDEDITDATA TO ORFK-PRARTNTO-LOCPREL-UT(IX)           
053100         END-IF                                                           
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 BO-KONTROLLERA-PRARTBTO-LOC SECTION.                                     
053600                                                                          
053700     IF ORFK-IDSYSTEM          = 'IMS ' OR                                
053800                               'PROF'                                     
053900        MOVE ORFK-IDUSER      TO SEC-IDUSER                               
054000        MOVE '4212'           TO SEC-IDTRANS                              
054100        MOVE ORFK-IDDISTR         TO SEC-IDKEY                            
054200        PERFORM S02-CALL-WSECURIT                                         
054300        IF SEC-KDSVAR         NOT = SPACE                                 
054400           MOVE JA           TO FORMELLT-FEL-SW                           
054500           MOVE NEJ          TO ORFK-PRARTBTO-LOC-OK(IX)                  
054600        ELSE                                                              
054700           MOVE ORFK-PRARTBTO-LOC(IX) TO DEC-IDFRIDATA                    
054800           MOVE +7           TO DEC-KVHELTAL                              
054900           MOVE +2           TO DEC-KVDECIMAL                             
055000           PERFORM S05-CALL-WDECEDIT                                      
055100           IF DEC-KDSVAR-FEL                                              
055200              MOVE JA       TO FORMELLT-FEL-SW                            
055300              MOVE NEJ      TO ORFK-PRARTBTO-LOC-OK(IX)                   
055400           ELSE                                                           
055500              MOVE DEC-IDEDITDATA TO ORFK-PRARTBTO-LOC-UT(IX)             
055600           END-IF                                                         
055700        END-IF                                                            
055800      ELSE                                                                
055900         MOVE ORFK-PRARTBTO-LOC(IX) TO DEC-IDFRIDATA                      
056000         MOVE +7               TO DEC-KVHELTAL                            
056100         MOVE +2               TO DEC-KVDECIMAL                           
056200         PERFORM S05-CALL-WDECEDIT                                        
056300         IF DEC-KDSVAR-FEL                                                
056400            MOVE JA           TO FORMELLT-FEL-SW                          
056500            MOVE NEJ          TO ORFK-PRARTBTO-LOC-OK(IX)                 
056600          ELSE                                                            
056700            MOVE DEC-IDEDITDATA TO ORFK-PRARTBTO-LOC-UT(IX)               
056800         END-IF                                                           
056900     END-IF                                                               
057000     .                                                                    
057100     EJECT                                                                
057200 C-ARTIKEL-KONTROLL   SECTION.                                            
057300                                                                          
057400     MOVE +1                   TO IX                                      
057500     MOVE ORFK-IDDC            TO AREG-IDDC                               
057600                                                                          
057700     IF DIST35-REFILL                                                     
057800     OR DIST35-REFILL-INOM-NDC                                            
057900     OR DIST35-NONVCC-REFILL                                              
058000     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
058100     OR DIST35-NONVCC-VCC-TRANSFER                                        
058200                                                                          
058300       MOVE ORFK-IDDISTR         TO WS-SOEK-IDDISTR                       
058400                                                                          
058500       SEARCH ALL WWDC03-IDDC                                             
058600         AT END                                                           
058700           MOVE 'EJ TRÄFF I TABELL TEXTXX' TO FELTEXT                     
058800           DISPLAY FELTEXT                                                
058900           CALL FELLOG                                                    
059000         WHEN WWDC03-SOK-IDDISTR-TAB2(WWDC03-IX2)                         
059100           = WS-SOEK-IDDISTR                                              
059200             MOVE WWDC03-SOK-IDDC-REC(WWDC03-IX2) TO AREG-IDDC-REC        
059300       END-SEARCH                                                         
059400     ELSE                                                                 
059500       MOVE SPACE TO AREG-IDDC-REC                                        
059600     END-IF                                                               
059700                                                                          
059800     PERFORM UNTIL IX > MAX-IX                                            
059900         IF ORFK-IDARTNR-IN(IX)  NOT = ALL '+'                            
060000           MOVE ORFK-IDARTNR(IX)     TO AREG-IDARTNR                      
060100                                                                          
060200           CALL W411AREG USING AREG-W411AREG                              
060300                               AREG-WDK6-PCB                              
060400                               AREG-WDK7-PCB                              
060500                                                                          
060600           MOVE AREG-W411AREG-001    TO ORFK-W411AREG-001(IX)             
060700                                                                          
060800           IF AREG-KDORDBEK NOT = ZERO                                    
060900             MOVE AREG-KDORDBEK      TO ORFK-KDORDBEK(IX)                 
061000             IF AREG-KDORDBEK = 56 AND (ORFK-KDORDKL = 0     OR           
061100                                        ORFK-FLFORBI = 'J'   OR           
061200                                  ORFK-FLFORBI = SPEC-FORBI  OR           
061300                                        ORFK-FLORDSPE = 'J'  OR           
061400                                        DIST40-NDC-NA        OR           
061500                                        DIST40-NDC-PACIFIC   OR           
061600                                        DIST40-NDC-CN        OR           
061700                                        DIST35-RETUR         OR           
061800                                       DIST35-NA-CDC-RETURN  OR           
061900                                       DIST35-IN-CDC-RETURNS OR           
062000                                       DIST35-AE-CDC-RETURNS OR           
062100                                       DIST35-CN-CDC-RETURNS OR           
062200                                       DIST35-CN-NDC-RETURNS OR           
062300                                    DIST35-NA-CDC-BB-RETURN  OR           
062400                                  DIST35-NA-CDC-QUAL-RETURN  OR           
062500                                        DIST35-NA-TRANSFER   OR           
062600                                    DIST35-NA-NDC-RETURNS    OR           
062700                                    DIST35-REFILL-INOM-JP    OR           
062800                                    DIST35-PACIFIC-TRANSFER  OR           
062900                                         DIST35-CN-TRANSFER  OR           
063000                                        DIST35-ST-CDC        OR           
063100                                      DIST35-NL-SITTARD-OBJEKT)           
063200               MOVE ZERO TO ORFK-KDORDBEK(IX)                             
063300             END-IF                                                       
063400           END-IF                                                         
063500           IF DIST88-VADIS-NDC AND                                        
063600              ORFK-KDORDKL = 1 AND                                        
063700              ORFK-IDSYSTEM = 'VDI ' AND                                  
063800              CDC-SE           AND                                        
063900              AREG-KDSORT NOT = 'SW'                                      
064000             MOVE JA           TO FORMELLT-FEL-SW                         
064100             MOVE NEJ          TO ORFK-IDARTNR-OK(IX)                     
064200           END-IF                                                         
064300         END-IF                                                           
064400         ADD +1                TO IX                                      
064500     END-PERFORM                                                          
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 S01-CALL-W009KSIF    SECTION.                                            
065000                                                                          
065100     CALL W009KSIF USING FLT LGD KSIFF                                    
065200     .                                                                    
065300     EJECT                                                                
065400 S02-CALL-WSECURIT    SECTION.                                            
065500                                                                          
065600     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
065700                         SEC-IDKEY  SEC-KDSVAR                            
065800     .                                                                    
065900     EJECT                                                                
066000 S03-CALL-WDATKONV     SECTION.                                           
066100                                                                          
066200     CALL WDATKONV  USING DAT-KDDATFORM                                   
066300                          DAT-I-TIDATUM                                   
066400                          DAT-O-TIDATUM                                   
066500                          DAT-KDSVAR                                      
066600     .                                                                    
066700     EJECT                                                                
066800 S05-CALL-WDECEDIT    SECTION.                                            
066900                                                                          
067000     CALL WDECEDIT USING DEC-WDECAREA                                     
067100     .                                                                    
067200     EJECT                                                                
067300*    -COPY WY2000P1                                                       
