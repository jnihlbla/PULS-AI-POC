000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411SPAR.                                                
000500 AUTHOR.         LASSI OLGRENER.                                          
000600 DATE-WRITTEN.   APRIL -90.                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL ORDERRAD-PGM                      
001100*                                                                         
001200*    FUNKTION.                                                            
001300*      - RADER GENOMGÅR KONTROLL AV SPÄRRAR FÖR ATT                       
001400*        KONTROLLERA OM RADEN SKA VIDARE BEHANDLAS ELLER EJ.              
001500*                                                                         
001600*        PROGRAMMET LÄSER   WDF8              LEVERANSSPÄRR               
001700*        PROGRAMMET LÄSER   WDK6              ARTIKELREGISTER             
001800*                                                                         
001900*    LÄNKAREA: W411SPAR                                                   
002000*    CHANGE LOG:                                                          
002100*    STORY 2373879 / REPLACE DIST35- 88 LEVELS OF IN,KR,                  
002200*     AE,CN CDC RETURNS TO DIST35-CDC-RETURNS-NON-VCC                     
002300                                                                          
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700                                                                          
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -COPY WY2000W1                                                       
003200*    -COPY WY2000W2                                                       
003300                                                                          
003400 77  IDPGM                       PIC X(08)   VALUE 'W411SPAR'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  YES                         PIC X       VALUE 'Y'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004000 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004100                                                                          
004200 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
004300 77  IDKUNDNR-57561              PIC X       VALUE 'J'.                   
004400                                                                          
004500 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
004600     88  TRAEFF-OK                           VALUE 'J'.                   
004700     88  EJ-TRAEFF                           VALUE 'N'.                   
004800                                                                          
004900 77  AKTUELL-KUND-SW             PIC X       VALUE 'N'.                   
005000     88  AKTUELL-KUND                        VALUE 'J'.                   
005100     88  EJ-AKTUELL-KUND                     VALUE 'N'.                   
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005600                                                                          
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006000                                                                          
006100 01  DAGENS-DATUM                PIC 9(8).                                
006200 01  DAGENS-DAT                  PIC 9(6).                                
006300                                                                          
006400 01  WS-TISTADAT                 PIC 9(6).                                
006500 01  WS-TIREPDAT-6               PIC 9(6).                                
006600 01  WS-TIREPDAT-5               PIC 9(5).                                
006700                                                                          
006800                                                                          
006900*      --- VALID IDDC CODES                                               
007000                                                                          
007100*01    -COPY WWDC99                                                       
007200                                                                          
007300*01    -COPY WWPRODSL                                                     
007400                                                                          
007500                                                                          
007600 01  GENERELLA-SUBPROGRAM.                                                
007700   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
007800   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
007900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008100   03  WZ20DAYS                  PIC X(8)    VALUE 'WZ20DAYS'.            
008200                                                                          
008300 01  RKOD-16                     PIC S9(4)   VALUE +16  COMP-3.           
008400                                                                          
008500 01  AKT-DATUM-PLUS-2-VECKOR     PIC 9(5).                                
008600 01  FILLER REDEFINES AKT-DATUM-PLUS-2-VECKOR.                            
008700   03  AKT-AAR                   PIC 9(2).                                
008800   03  AKT-VECKA                 PIC 9(2).                                
008900   03  AKT-DAG                   PIC 9(1).                                
009000                                                                          
009100 01  AAVVD-TITPO-NUM             PIC 9(5).                                
009200 01  FILLER  REDEFINES AAVVD-TITPO-NUM.                                   
009300   03  AAR                       PIC 9(2).                                
009400   03  VECKA                     PIC 9(2).                                
009500   03  DAG                       PIC 9(1).                                
009600     EJECT                                                                
009700                                                                          
009800*   -COPY WDATAREA.                                                       
009900*                                                                         
010000*    --- PARAMETRAR TILL WZ20DAYS                                         
010100*    -COPY WZ20DAYS                                                       
010200     EJECT                                                                
010300                                                                          
010400                                                                          
010500 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
010600*01 FILLER  -COPY WWDIST03     -RED  TEST-IDDISTR.                        
010700*01 FILLER  -COPY WWDIST18     -RED  TEST-IDDISTR.                        
010800*01 FILLER  -COPY WWDIST35     -RED  TEST-IDDISTR.                        
010900*01 FILLER  -COPY WWDIST47     -RED  TEST-IDDISTR.                        
011000*01 FILLER  -COPY WWDIST65     -RED  TEST-IDDISTR.                        
011100                                                                          
011200                                                                          
011300 01  TEST-OBJEKT                 PIC 9(9)    COMP-3.                      
011400*01 FILLER  -COPY WWBYT03      -RED  TEST-OBJEKT.                         
011500                                                                          
011600                                                                          
011700 01  TEST-ARTIKEL                PIC 9(9)    COMP-3.                      
011800*01 FILLER  -COPY WWART01       -RED  TEST-ARTIKEL.                       
011900*01 FILLER  -COPY WWART04       -RED  TEST-ARTIKEL.                       
012000                                                                          
012100                                                                          
012200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012300                                                                          
012400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012500                                                                          
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FINNS                       VALUE '  '.                  
012900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013000     88  BAS-SLUT                            VALUE 'GB'.                  
013100                                                                          
013200 01  GODK-STATUSKODER.                                                    
013300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400                                                                          
013500 01  ALL-SSA.                                                             
013600     03 SSA1                     PIC X(180).                              
013700     03 SSA2                     PIC X(96).                               
013800                                                                          
013900                                                                          
014000*    --- IMS FUNKTIONSKODER                                               
014100*01  -COPY W0003                                                          
014200                                                                          
014300 01  NYCKLAR-TILL-DLI.                                                    
014400     03  W-WDF8ASEQ-X.                                                    
014500         05  W-IDDISTRF-F8A1     PIC S9(5) VALUE ZERO COMP-3.             
014600         05  W-IDDISTRT-F8A1     PIC S9(5) VALUE ZERO COMP-3.             
014700         05  W-IDKUNDNRF-F8A1    PIC S9(7) VALUE ZERO COMP-3.             
014800         05  W-IDKUNDNRT-F8A1    PIC S9(7) VALUE ZERO COMP-3.             
014900         05  W-IDSPRGRP-F8A1     PIC X(10) VALUE SPACE.                   
015000                                                                          
015100     03  W-IDSPRGRP-X.                                                    
015200         05  W-IDSPRGRP          PIC X(10)   VALUE SPACE.                 
015300                                                                          
015400     03  W-IDDISTR-X.                                                     
015500         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
015600                                                                          
015700     03  W-IDKUNDNR-X.                                                    
015800         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
015900                                                                          
016000     03  W-IDARTNR-X.                                                     
016100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016200                                                                          
016300     03  W-KDARTURS-X.                                                    
016400         05  W-KDARTURS           PIC  X(2) VALUE SPACE.                  
016500                                                                          
016600     03  W-KDPRODSL-X.                                                    
016700         05  W-KDPRODSL           PIC S9(3) VALUE ZERO COMP-3.            
016800                                                                          
016900     03  W-IDFKNGRP-X.                                                    
017000         05  W-IDFKNGRP           PIC S9(5) VALUE ZERO COMP-3.            
017100                                                                          
017200                                                                          
017300                                                                          
017400*    ---  DLI INPUT-OUTPUT AREA                                           
017500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF8A1  '.                    
017600 01  DLI-IO-WDF8A1.                                                       
017700*    03  -COPY WDF8A1                                                     
017800                                                                          
017900                                                                          
018000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF801  '.                    
018100 01  DLI-IO-WDF801.                                                       
018200*    03  -COPY WDF801                                                     
018300                                                                          
018400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF812  '.                    
018500 01  DLI-IO-WDF811.                                                       
018600*    03  -COPY WDF811                                                     
018700                                                                          
018800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF812  '.                    
018900 01  DLI-IO-WDF812.                                                       
019000*    03  -COPY WDF812                                                     
019100                                                                          
019200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF813  '.                    
019300 01  DLI-IO-WDF813.                                                       
019400*    03  -COPY WDF813                                                     
019500                                                                          
019600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF814  '.                    
019700 01  DLI-IO-WDF814.                                                       
019800*    03  -COPY WDF814                                                     
019900                                                                          
020000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF815  '.                    
020100 01  DLI-IO-WDF815.                                                       
020200*    03  -COPY WDF815                                                     
020300                                                                          
020400                                                                          
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601  '.                    
020600 01  DLI-IO-WDK601.                                                       
020700*    03  -COPY WDK601                                                     
020800                                                                          
020900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611  '.                    
021000 01  DLI-IO-WDK611.                                                       
021100*    03  -COPY WDK611                                                     
021200                                                                          
021300                                                                          
021400                                                                          
021500 LINKAGE SECTION.                                                         
021600                                                                          
021700*   -COPY W411SPAR                                                        
021800                                                                          
021900                                                                          
022000*01  -COPY W0008      -PRE WDF8-                                          
022100     05  FILLER                  PIC X.                                   
022200                                                                          
022300*01  -COPY W0008      -PRE WDF8A-                                         
022400     05  FILLER                  PIC X.                                   
022500                                                                          
022600*01  -COPY W0008      -PRE WDK6-                                          
022700     05  FILLER                  PIC X.                                   
022800                                                                          
022900                                                                          
023000 PROCEDURE DIVISION  USING SPAR-W411SPAR WDF8-PCB WDF8A-PCB               
023100                                         WDK6-PCB.                        
023200                                                                          
023300     MOVE FUNCTION CURRENT-DATE(1:8) TO   DAGENS-DATUM                    
023400     ACCEPT DAGENS-DAT               FROM DATE                            
023500                                                                          
023600     MOVE  ZERO                      TO   SPAR-KDORDBEK                   
023700                                                                          
023800     IF SPAR-FLOVRLEV NOT = JA                                            
023900                                                                          
024000        MOVE SPAR-IDDISTR    TO TEST-IDDISTR                              
024100                                                                          
024200        MOVE SPAR-IDARTNR    TO TEST-OBJEKT                               
024300                                TEST-ARTIKEL                              
024400        MOVE SPAR-IDDC       TO WS-IDDC                                   
024500                                                                          
024600        PERFORM A-KOLLA-PROVDETALJ                                        
024700        PERFORM C-KOLLA-UTGONGEN-ARTIKEL                                  
024800        PERFORM D-KOLLA-OM-KDORDBEK-BLIR-55                               
024900        PERFORM E-KOLLA-SATSSPAERR                                        
025000        PERFORM F-KOLLA-OM-KDORDBEK-BLIR-58                               
025100        PERFORM G-KOLLA-MILITAERSPAERR                                    
025200        PERFORM H-KOLLA-OM-KDORDBEK-BLIR-67                               
025300                                                                          
025400        IF SPAR-KDORDBEH = +0 OR +7                                       
025500          PERFORM I-KOLL-VID-FRAGA-OCH-UTSKRIFT                           
025600        END-IF                                                            
025700        IF SPAR-KDORDBEH = +7                                             
025800           IF DIST18-SCRAP-NDC-SC  OR                                     
025900              DIST18-SCRAP-NDC-QUAL                                       
026000              CONTINUE                                                    
026100           ELSE                                                           
026200              PERFORM J-KOLLA-ERSATTNING                                  
026300           END-IF                                                         
026400        END-IF                                                            
026500* FIX DUE TO ABEND U0240 FROM WL013410 SKIP K- SECTION FOR NOW            
026600        IF SPAR-KDORDBEH = +7 AND SPAR-IDDC NOT = '11'                    
026700          CONTINUE                                                        
026800        ELSE                                                              
027100          PERFORM K-KOLLA-LEVERANSSPARR                                   
027200        END-IF                                                            
027300     END-IF                                                               
027400                                                                          
027500     GOBACK                                                               
027600     .                                                                    
027700                                                                          
027800                                                                          
027900                                                                          
028000 A-KOLLA-PROVDETALJ SECTION.                                              
028100     MOVE 'A-KOLLA          ' TO CURRENT-SECTION                          
028200                                                                          
028300     IF SPAR-FLORDSPE = NEJ                                               
028400       IF SPAR-FLFORBI = NEJ                                              
028500                                                                          
028600          IF SPAR-KDERS = +52 OR SPAR-KDERS-UTG = +52                     
028700            IF SPAR-KDERS = +52                                           
028800            AND (DIST18-SKROT                                             
028900            OR   DIST18-SCRAP-NDC-DAM                                     
029000            OR   DIST18-SCRAP-NDC-QUAL                                    
029100            OR   DIST18-SCRAP-NDC-SC)                                     
029200*CDC-DIST:82, 70.  SDC:81, 90.  NDC:8481, 8490.                           
029300*(E-TRACKER 10161892 NDC-SC)    NDC:8480.                                 
029400              CONTINUE                                                    
029500            ELSE                                                          
029600              MOVE 52 TO SPAR-KDORDBEK                                    
029700            END-IF                                                        
029800          END-IF                                                          
029900                                                                          
030000       END-IF                                                             
030100     END-IF                                                               
030200     .                                                                    
030300                                                                          
030400                                                                          
030500 C-KOLLA-UTGONGEN-ARTIKEL SECTION.                                        
030600     MOVE 'C-KOLLA-UTGONGEN ' TO CURRENT-SECTION                          
030700                                                                          
031100     IF SPAR-FLORDSPE = NEJ                                               
031200       IF SPAR-FLFORBI = NEJ                                              
031300          IF (CDC OR SPAR-KDORDBEH = +1) AND                              
031400             ((SPAR-KDERS = +19 OR +29)                                   
031500                               OR SPAR-KDERS-UTG = +29)                   
031700            IF DIST18-SCRAP-NDC-SC  OR                                    
031800               DIST18-SCRAP-NDC-QUAL                                      
032000              CONTINUE                                                    
032100            ELSE                                                          
032200              MOVE 54 TO SPAR-KDORDBEK                                    
032400            END-IF                                                        
032500          END-IF                                                          
032600       END-IF                                                             
032700     END-IF                                                               
032800     .                                                                    
032900                                                                          
033000                                                                          
033100 D-KOLLA-OM-KDORDBEK-BLIR-55 SECTION.                                     
033200     MOVE 'D-KOLLA-OBEK-55  ' TO CURRENT-SECTION                          
033300                                                                          
033400     IF BYT03-OBJEKT                                                      
033500******* KONTROLL AV BYTES-SPÄRR ****************                          
033600       IF SPAR-FLORDSPE = NEJ                                             
033700         IF (CDC AND SPAR-KDFAKTYP NOT = 'N') OR                          
033800            (SDC AND SPAR-KDFAKTYP NOT = 'G') OR                          
033900            (NDC AND SPAR-KDFAKTYP NOT = 'G' AND 'K' AND 'N')             
034000                                                                          
034100           MOVE 55 TO SPAR-KDORDBEK                                       
034200         END-IF                                                           
034300         IF DIST18-SCRAP-NDC-SC                                           
034400           MOVE 55 TO SPAR-KDORDBEK                                       
034500         END-IF                                                           
034600       END-IF                                                             
034700     ELSE                                                                 
034800       IF SPAR-FLORDSPE = JA                                              
034900         IF DIST18-SCRAP-NDC-SC                                           
035000           MOVE 55 TO SPAR-KDORDBEK                                       
035100         END-IF                                                           
035200       END-IF                                                             
035300     END-IF                                                               
035400                                                                          
035500     IF SPAR-FLORDSPE = NEJ                                               
035600       IF SPAR-FLFORBI = NEJ                                              
035700********* KONTROLL AV ASBESTSPÄRR ****************                        
035800          IF  DIST03-SVERIGE     AND                                      
035900             (ART01-ASBEST-BROMS  OR  ART01-ASBEST-OVRIGT)                
036000                                                                          
036100              MOVE  55 TO SPAR-KDORDBEK                                   
036200          END-IF                                                          
036300                                                                          
036400********* KONTROLL AV ASBESTSPÄRR - NORGE***********                      
036500          IF  DIST03-NORGE       AND                                      
036600             (ART01-ASBEST-BROMS)                                         
036700                                                                          
036800              MOVE  55 TO SPAR-KDORDBEK                                   
036900          END-IF                                                          
037000                                                                          
037100******* KONTROLL AV ASBESTSPÄRR - BAHREIN  *******                        
037200       IF SPAR-FLFORBI = NEJ                                              
037300          IF  DIST65-BAHREIN     AND                                      
037400             (ART01-ASBEST-BAHREIN)                                       
037500                                                                          
037600              MOVE  55 TO SPAR-KDORDBEK                                   
037700          END-IF                                                          
037800       END-IF                                                             
037900                                                                          
038000********* KONTROLL AV SPÄRR SVERIGE ****************                      
038100          IF  DIST03-SVERIGE     AND                                      
038200             (ART01-SPARR-SVERIGE)                                        
038300                                                                          
038400              MOVE  55 TO SPAR-KDORDBEK                                   
038500          END-IF                                                          
038600       END-IF                                                             
038700     END-IF                                                               
038800     .                                                                    
038900                                                                          
039000                                                                          
039100 E-KOLLA-SATSSPAERR SECTION.                                              
039200     MOVE 'E-KOLLA-SATSSPARR' TO CURRENT-SECTION                          
039300                                                                          
039400     IF SPAR-FLORDSPE = NEJ                                               
039500       IF SPAR-FLEMBORD NOT = JA                                          
039600         IF SPAR-KDORDKL > +0    AND  SPAR-FLFORBI = NEJ  AND             
039700            SPAR-FLLSRDEL = NEJ  AND  SPAR-FLIART = JA                    
039800                                 AND (NOT DIST35-REFILL)                  
039900                                 AND (NOT DIST35-NONVCC-REFILL)           
040000                                 AND (NOT DIST35-REFILL-INOM-NDC)         
040100                                 AND (NOT DIST35-NA-CDC-RETURN)           
040200                            AND (NOT DIST35-CDC-RETURNS-NON-VCC)          
040300                                 AND (NOT DIST35-CN-NDC-RETURNS)          
040400                                 AND (NOT DIST35-RETUR)                   
040500                                 AND (NOT DIST18-SCRAP-NDC)               
040600                                 AND (SPAR-IDDISTR NOT = 36)              
040700                                 AND (SPAR-IDDISTR NOT = 1479)            
040800                                                                          
040900             MOVE  57 TO SPAR-KDORDBEK                                    
041000         END-IF                                                           
041100       END-IF                                                             
041200     END-IF                                                               
041300     .                                                                    
041400                                                                          
041500                                                                          
041600 F-KOLLA-OM-KDORDBEK-BLIR-58 SECTION.                                     
041700     MOVE 'F-KOLLA-OBEK-58  ' TO CURRENT-SECTION                          
041800                                                                          
041900****** KOLLA STANDARDPRIS-SAKNAS *********                                
042000     IF SPAR-IDSYSTEM = 'W216' OR 'W371' OR '37A'                         
042100       CONTINUE                                                           
042200     ELSE                                                                 
042300       IF SPAR-PRARTSTD = +0                                              
042400                                                                          
042500           MOVE 58 TO SPAR-KDORDBEK                                       
042600       END-IF                                                             
042700     END-IF                                                               
042800                                                                          
042900****** KOLLA-LEVERERAS-EJ-SOM-RESERVDEL *************                     
043000     IF SPAR-FLORDSPE = NEJ                                               
043100       IF SPAR-FLEMBORD NOT = JA                                          
043200         IF SPAR-KDORDKL > +0    AND  SPAR-FLFORBI = NEJ  AND             
043300            SPAR-FLLSRDEL = NEJ  AND  SPAR-FLIART = NEJ                   
043400                                 AND (NOT DIST35-REFILL)                  
043500                                 AND (NOT DIST35-NONVCC-REFILL)           
043600                                 AND (NOT DIST35-REFILL-INOM-NDC)         
043700                                 AND (NOT DIST35-NA-CDC-RETURN)           
043800                            AND (NOT DIST35-CDC-RETURNS-NON-VCC)          
043900                                 AND (NOT DIST35-CN-NDC-RETURNS)          
044000                                 AND (NOT DIST35-RETUR)                   
044100                                 AND (NOT DIST18-SCRAP-NDC)               
044200                                 AND (SPAR-IDDISTR NOT = 36)              
044300                                 AND (SPAR-IDDISTR NOT = 1479)            
044400                                                                          
044500             MOVE  58 TO SPAR-KDORDBEK                                    
044600         END-IF                                                           
044700       END-IF                                                             
044800     END-IF                                                               
044900                                                                          
045000****** LOKAL-ARTIKEL SPÄRR SDC-BULK OCH NDC TILL CDC *******              
045100     IF SPAR-IDSYSTEM = 'W216' OR 'W371' OR 'W37A'                        
045200       CONTINUE                                                           
045300     ELSE                                                                 
045400       MOVE SPAR-KDPRODSL        TO TEST-KDPRODSL                         
045500       IF KDPRODSL-LOCAL           AND                                    
045600          CDC                                                             
045700         MOVE 58 TO SPAR-KDORDBEK                                         
045800       END-IF                                                             
045900     END-IF                                                               
046000     .                                                                    
046100                                                                          
046200                                                                          
046300 G-KOLLA-MILITAERSPAERR SECTION.                                          
046400     MOVE 'G-KOLLA-MILITAER ' TO CURRENT-SECTION                          
046500                                                                          
046600     IF SPAR-FLORDSPE = NEJ              AND                              
046700        SPAR-FLFORBI = NEJ               AND                              
046800        SPAR-KDUART = 'M'                AND                              
046900        SPAR-BERADREF NOT = 'W480      ' AND                              
047000        SPAR-KDPRTYP NOT = 'P'                                            
047100                                                                          
047200          MOVE  66 TO SPAR-KDORDBEK                                       
047300     END-IF                                                               
047400     .                                                                    
047500                                                                          
047600                                                                          
047700 H-KOLLA-OM-KDORDBEK-BLIR-67 SECTION.                                     
047800     MOVE 'H-KOLLA-OBEK-67  ' TO CURRENT-SECTION                          
047900                                                                          
048000     MOVE  NEJ                       TO   SPAR-FLPUBCDC                   
048100                                                                          
048200     IF SPAR-FLORDSPE = NEJ                                               
048300****** KOLLA PUBLIKATIONSVECKA ***********                                
048400       IF SPAR-TITPO > +0   AND (SPAR-KDTPOTYP NOT = 3)                   
048500       OR SPAR-TIREPDAT > +0                                              
048600                                                                          
048700         IF SPAR-TITPO > +0                                               
048800            MOVE SPAR-TITPO TO DAT-I-TIDATUM                              
048900            MOVE 'AAMMDD'   TO DAT-KDDATFORM                              
049000            CALL WDATKONV USING DAT-KDDATFORM   DAT-I-TIDATUM             
049100                                DAT-O-TIDATUM   DAT-KDSVAR                
049200            IF DAT-KDSVAR-OK                                              
049300               MOVE DAT-TIAA-VECKA TO AAR                                 
049400               MOVE DAT-TIVV       TO VECKA                               
049500               MOVE DAT-TID        TO DAG                                 
049600            ELSE                                                          
049700               CALL ABEND USING RKOD-16                                   
049800            END-IF                                                        
049900         END-IF                                                           
050000                                                                          
050100         MOVE AAVVD-TITPO-NUM   TO TMP1-YYWWD                             
050200         MOVE SPAR-TIFINLV      TO TMP2-YYWWD                             
050300                                                                          
050400         IF SPAR-TIREPDAT > ZERO                                          
050500           MOVE SPAR-TIREPDAT TO WS-TIREPDAT-6                            
050600           MOVE WS-TIREPDAT-6 TO DAYS-TIDATE2                             
050700           MOVE 'YYMMDD'      TO DAYS-KDDATFMT2                           
050800           MOVE 0             TO DAYS-KVDAYS                              
050900           MOVE SPACE         TO DAYS-TIDATE1                             
051000           MOVE 'YYWWD'       TO DAYS-KDDATFMT1                           
051100           CALL WZ20DAYS USING DAYS-WZ20DAYS                              
051200            IF DAYS-KDRC NOT = ZERO                                       
051300            MOVE 'WZ20DAYS ERROR CONV. TIREPDAT' TO FELTEXT-STR           
051400               CALL ABEND USING RKOD-ABEND-MED-DUMP                       
051500            END-IF                                                        
051600              MOVE 'YYWWD'    TO DAYS-KDDATFMT2                           
051700              MOVE SPACE      TO DAYS-TIDATE2                             
051800              MOVE 14         TO DAYS-KVDAYS                              
051900              CALL WZ20DAYS USING DAYS-WZ20DAYS                           
052000            IF DAYS-KDRC NOT = ZERO                                       
052100            MOVE 'WZ20DAYS ERROR ADDING 14 DAYS' TO FELTEXT-STR           
052200               CALL ABEND USING RKOD-ABEND-MED-DUMP                       
052300            END-IF                                                        
052400            MOVE DAYS-TIDATE2(1:5) TO WS-TIREPDAT-5                       
052500            MOVE WS-TIREPDAT-5     TO TMP1-YYWWD                          
052600         END-IF                                                           
052700                                                                          
052800         PERFORM WY2000P2                                                 
052900         IF TMP1-YYWWD < TMP2-YYWWD                                       
053000            MOVE  67 TO SPAR-KDORDBEK                                     
053100            IF NDC                                                        
053200               MOVE YES TO SPAR-FLPUBCDC                                  
053300            END-IF                                                        
053400         END-IF                                                           
053500       ELSE                                                               
053600         MOVE DAGENS-DAT TO DAT-I-TIDATUM                                 
053700         MOVE 'AAMMDD'   TO DAT-KDDATFORM                                 
053800         CALL WDATKONV USING DAT-KDDATFORM   DAT-I-TIDATUM                
053900                             DAT-O-TIDATUM   DAT-KDSVAR                   
054000         IF DAT-KDSVAR-OK                                                 
054100           MOVE DAT-TIAA-VECKA TO AKT-AAR                                 
054200           MOVE DAT-TIVV       TO AKT-VECKA                               
054300           MOVE DAT-TID        TO AKT-DAG                                 
054400           ADD 2 TO AKT-VECKA                                             
054500           IF AKT-VECKA > 52                                              
054600             SUBTRACT 52 FROM AKT-VECKA                                   
054700             ADD 1 TO AKT-AAR                                             
054800           END-IF                                                         
054900         ELSE                                                             
055000           CALL ABEND USING RKOD-16                                       
055100         END-IF                                                           
055200                                                                          
055300         MOVE AKT-DATUM-PLUS-2-VECKOR   TO TMP1-YYWWD                     
055400         MOVE SPAR-TIFINLV              TO TMP2-YYWWD                     
055500         PERFORM WY2000P2                                                 
055600         IF TMP1-YYWWD < TMP2-YYWWD                                       
055700         AND NOT DIST35-REFILL                                            
055800         AND NOT DIST35-NONVCC-REFILL                                     
055900         AND NOT DIST35-REFILL-NA-JAP                                     
056000         AND NOT DIST35-REFILL-INOM-NDC                                   
056100             MOVE  67 TO SPAR-KDORDBEK                                    
056200                            AKT-DATUM-PLUS-2-VECKOR                       
056300             IF NDC                                                       
056400                MOVE YES                TO SPAR-FLPUBCDC                  
056500             END-IF                                                       
056600         END-IF                                                           
056700       END-IF                                                             
056800                                                                          
056900********* KOLLA OBLIGATORISK-RADREFERENS **********                       
057000       IF SPAR-FLRADREF = JA  AND  SPAR-BERADREF = SPACE                  
057100                                                                          
057200           MOVE  67 TO SPAR-KDORDBEK                                      
057300       END-IF                                                             
057400                                                                          
057500********* KOLLA AVROPSSPÄRR ***********************                       
057600       IF SPAR-FLFORBI = NEJ AND SPAR-FLAVRART = JA                       
057700                                                                          
057800           MOVE  67 TO SPAR-KDORDBEK                                      
057900       END-IF                                                             
058000     ELSE                                                                 
058100********* KOLLA SOFTWARE ORDER/RAD*****************                       
058200                                                                          
058300       IF SPAR-IDSYSTEM = 'SOFT'                                          
058400         IF SPAR-KDSORT NOT = 'SW'                                        
058500           MOVE 67 TO SPAR-KDORDBEK                                       
058600         END-IF                                                           
058700       ELSE                                                               
058800*SOFT - ENDAST PIN KODER FRÅN 4231, EJ PIE ARTIKLAR                       
058900         IF SPAR-BEKUNDRF = 'SOFTWARE'                                    
059000           IF SPAR-IDSYSTEM = '4231'                                      
059100             IF SPAR-KDSORT = 'SW'                                        
059200               IF ART04-SOFTWARE                                          
059300                 CONTINUE                                                 
059400               ELSE                                                       
059500                 MOVE 67 TO SPAR-KDORDBEK                                 
059600               END-IF                                                     
059700             ELSE                                                         
059800               MOVE 67 TO SPAR-KDORDBEK                                   
059900             END-IF                                                       
060000           ELSE                                                           
060100             IF SPAR-KDSORT NOT = 'SW'                                    
060200               MOVE 67 TO SPAR-KDORDBEK                                   
060300             END-IF                                                       
060400           END-IF                                                         
060500         ELSE                                                             
060600           IF SPAR-KDSORT = 'SW'                                          
060700             MOVE 67 TO SPAR-KDORDBEK                                     
060800           END-IF                                                         
060900         END-IF                                                           
061000       END-IF                                                             
061100     END-IF                                                               
061200     .                                                                    
061300                                                                          
061400                                                                          
061500 I-KOLL-VID-FRAGA-OCH-UTSKRIFT SECTION.                                   
061600     MOVE 'I-KOLL-FRAGA-UTSK' TO CURRENT-SECTION                          
061700                                                                          
061800     MOVE SPAR-IDDISTR         TO TEST-IDDISTR                            
061900*OBS! KUNDNR 57561 FINNS HÅRDKODAT I PGM W40737 PÅ 4 STÄLLEN.             
062000*OM KUNDNR ÄNDRAS HÄR MÅSTE DET ÄNDRAS I 4737 OXÅ.                        
062100     MOVE NEJ                  TO IDKUNDNR-57561                          
062200     IF SPAR-IDKUNDNR = 57561                                             
062300       MOVE JA                 TO IDKUNDNR-57561                          
062400     END-IF                                                               
062500*-----------------------------------------                                
062600     IF SPAR-KDLEVSP = +20                                                
062700     OR (CDC AND SPAR-KDLEVSP = +21)                                      
062800       IF DIST18-SCRAP-NDC-QUAL                                           
062900       OR DIST18-SKROT-KVAL-SDC                                           
063000       OR DIST18-SCRAP-NDC-SC                                             
063100       OR DIST35-NA-CDC-QUAL-RETURN                                       
063200       OR DIST35-RETUR-Q                                                  
063300       OR (DIST18-SKROT-KVAL-CDC AND IDKUNDNR-57561 = JA)                 
063400       OR (DIST18-SKROT-KVAL-CDC AND SPAR-IDKUNDNR = 0)                   
063500       OR (DIST18-SKROT-KVAL-CDC AND SPAR-IDKUNDNR = 71)                  
063600         CONTINUE                                                         
063700       ELSE                                                               
063800         IF SPAR-IDKUNDRF-RO = '0000000   '                               
063900           IF  SPAR-KDORDKL = +0                                          
064000           AND SPAR-FLSDCLEV NOT = JA                                     
064100              MOVE     92 TO SPAR-KDORDBEK                                
064200           ELSE                                                           
064300              IF SPAR-FLRESTN = JA OR YES                                 
064400                MOVE     90 TO SPAR-KDORDBEK                              
064500              ELSE                                                        
064600                MOVE     80 TO SPAR-KDORDBEK                              
064700              END-IF                                                      
064800           END-IF                                                         
064900         ELSE                                                             
065000           IF SPAR-KDTPOTYP = +0 OR +6                                    
065100             MOVE     91 TO SPAR-KDORDBEK                                 
065200           ELSE                                                           
065300             IF SPAR-KDTPOTYP > +0 AND SPAR-TIRODAT > +0                  
065400               MOVE     91 TO SPAR-KDORDBEK                               
065500             ELSE                                                         
065600               MOVE     90 TO SPAR-KDORDBEK                               
065700             END-IF                                                       
065800           END-IF                                                         
065900         END-IF                                                           
066000       END-IF                                                             
066100     END-IF                                                               
066200     .                                                                    
066300                                                                          
066400                                                                          
066500 J-KOLLA-ERSATTNING SECTION.                                              
066600     MOVE 'J-KOLLA-ERSATTN  ' TO CURRENT-SECTION                          
066700                                                                          
066800     IF SPAR-FLORDSPE = NEJ                                               
066900       IF SPAR-FLFORBI = NEJ                                              
067000          IF SPAR-KDERS = +26                                             
067100            IF SPAR-IDKUNDRF-RO = '0000000   '                            
067200              IF  SPAR-KDORDKL = +0                                       
067300              AND SPAR-FLSDCLEV NOT = JA                                  
067400                 MOVE  92 TO SPAR-KDORDBEK                                
067500              ELSE                                                        
067600                 IF SPAR-FLRESTN = JA OR YES                              
067700                   MOVE  90 TO SPAR-KDORDBEK                              
067800                 ELSE                                                     
067900                   MOVE  80 TO SPAR-KDORDBEK                              
068000                 END-IF                                                   
068100              END-IF                                                      
068200            ELSE                                                          
068300              IF SPAR-KDTPOTYP = +0 OR +6                                 
068400                MOVE  91 TO SPAR-KDORDBEK                                 
068500              ELSE                                                        
068600                IF SPAR-KDTPOTYP > +0 AND SPAR-TIRODAT > +0               
068700                  MOVE  91 TO SPAR-KDORDBEK                               
068800                ELSE                                                      
068900                  MOVE  90 TO SPAR-KDORDBEK                               
069000                END-IF                                                    
069100              END-IF                                                      
069200            END-IF                                                        
069300          END-IF                                                          
069400       END-IF                                                             
069500     END-IF                                                               
069600     .                                                                    
069700                                                                          
069800                                                                          
069900 K-KOLLA-LEVERANSSPARR SECTION.                                           
070000     MOVE 'K-KOLLA-LEVSPARR ' TO CURRENT-SECTION                          
070100                                                                          
070200*    MED UTGÅNGSPUNKT FRÅN DISTRIKT/KUND/ORDERKLASS/ARTIKEL               
070300*    SÖKER VI SPÄRRGRUPPER.                                               
070400*    FLAUTUPD AVGÖR OM VI SKALL SÖKA SPÄRRADE ARTIKLAR I 12-SEG.          
070500*    ELLER OM VI SKALL KOLLA REGLER I 13-, 14- OCH 15-SEGMENTEN           
070600*    FLAUTUPD = N BETYDER ATT VI SÖKER ARTIKLAR I 12-SEGMENTET            
070700*    FLAUTUPD = J BETYDER ATT VI TAR REDA PÅ URSPRUNG, PRODUKTSLAG        
070800*    OCH FUNKTIONSGRUPP FÖR ARTIKEL OCH KOLLAR SEDAN OM NÅGOT AV          
070900*    FINNS SPÄRRAT I 13-, 14- ELLER 15-SEGMENTEN                          
071000                                                                          
071100     MOVE NEJ            TO TRAEFF-SW                                     
071200                                                                          
071300     MOVE HIGH-VALUE    TO W-WDF8ASEQ-X                                   
071400     MOVE SPAR-IDDISTR  TO W-IDDISTR                                      
071500                           W-IDDISTRF-F8A1                                
071600     MOVE SPAR-IDKUNDNR TO W-IDKUNDNR                                     
071900                                                                          
072000     PERFORM IMS-GU-WDF8A1                                                
073200     MOVE NEJ         TO TRAEFF-SW                                        
073300                                                                          
073400     PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT OR TRAEFF-OK                
073500        PERFORM KA-KOLLA-DIST-KND-KLASS                                   
073700        IF AKTUELL-KUND                                                   
073800          MOVE SEQA-IDSPRGRP TO W-IDSPRGRP                                
074000          PERFORM IMS-GU-WDF801                                           
074300          IF GSPR-TISTADAT NOT > DAGENS-DAT                               
074400            MOVE SPAR-IDARTNR TO W-IDARTNR                                
074600            IF GSPR-FLAUTUPD = NEJ                                        
074700              PERFORM KB-KOLLA-ARTIKLAR                                   
074800            ELSE                                                          
075000              IF SPAR-KDORDBEK = ZERO                                     
075100                PERFORM IMS-GU-WDK601                                     
075200                IF SEGMENT-FINNS                                          
075300                  PERFORM IMS-GNP-WDK611                                  
075400                  PERFORM KC-KOLLA-URSPRUNG                               
075500                  PERFORM KD-KOLLA-PRODUKTSLAG                            
075600                  PERFORM KE-KOLLA-FUNKTIONSGRUPP                         
075700                END-IF                                                    
075800              END-IF                                                      
075900            END-IF                                                        
076000          END-IF                                                          
076100                                                                          
076300          IF TRAEFF-OK                                                    
076400             PERFORM KF-SAETT-KDORDBEK                                    
076500          ELSE                                                            
076600            PERFORM IMS-GN-WDF8A1                                         
076900            PERFORM UNTIL SEQA-IDSPRGRP NOT = W-IDSPRGRP OR               
077000                          SEGMENT-SAKNAS OR BAS-SLUT                      
077100               PERFORM IMS-GN-WDF8A1                                      
077400            END-PERFORM                                                   
077500          END-IF                                                          
077600        ELSE                                                              
077700           PERFORM IMS-GN-WDF8A1                                          
077900        END-IF                                                            
078000     END-PERFORM                                                          
078100     .                                                                    
078200                                                                          
078300                                                                          
078400 KA-KOLLA-DIST-KND-KLASS SECTION.                                         
078500     MOVE 'KA-KOLLA-D-K-K   ' TO CURRENT-SECTION                          
078600                                                                          
078700     MOVE NEJ           TO AKTUELL-KUND-SW                                
078800     MOVE SPAR-IDKUNDNR TO W-IDKUNDNR                                     
078900     IF  SPAR-IDDISTR  NOT < SEQA-IDDISTR-FOM                             
079000     AND SPAR-IDDISTR  NOT > SEQA-IDDISTR-TOM                             
079100     AND SPAR-IDKUNDNR NOT < SEQA-IDKUNDNR-FOM                            
079200     AND SPAR-IDKUNDNR NOT > SEQA-IDKUNDNR-TOM                            
079300                                                                          
079400        EVALUATE SPAR-KDORDKL                                             
079500          WHEN 0                                                          
079600             IF  SEQA-TISTADAT-KL0 NOT > DAGENS-DAT                       
079700             AND SEQA-TISTADAT-KL0 NOT = ZERO                             
079800                MOVE JA  TO AKTUELL-KUND-SW                               
079900             END-IF                                                       
080000          WHEN 1                                                          
080100             IF  SEQA-TISTADAT-KL1 NOT > DAGENS-DAT                       
080200             AND SEQA-TISTADAT-KL1 NOT = ZERO                             
080300                MOVE JA  TO AKTUELL-KUND-SW                               
080400             END-IF                                                       
080500          WHEN 2                                                          
080600             IF  SEQA-TISTADAT-KL2 NOT > DAGENS-DAT                       
080700             AND SEQA-TISTADAT-KL2 NOT = ZERO                             
080800                MOVE JA  TO AKTUELL-KUND-SW                               
080900             END-IF                                                       
081000          WHEN 3                                                          
081100             IF  SEQA-TISTADAT-KL3 NOT > DAGENS-DAT                       
081200             AND SEQA-TISTADAT-KL3 NOT = ZERO                             
081300                MOVE JA  TO AKTUELL-KUND-SW                               
081400             END-IF                                                       
081500          WHEN 4                                                          
081600             IF  SEQA-TISTADAT-KL4 NOT > DAGENS-DAT                       
081700             AND SEQA-TISTADAT-KL4 NOT = ZERO                             
081800                MOVE JA  TO AKTUELL-KUND-SW                               
081900             END-IF                                                       
082000        END-EVALUATE                                                      
082100     END-IF                                                               
082200     .                                                                    
082300                                                                          
082400                                                                          
082500 KB-KOLLA-ARTIKLAR SECTION.                                               
082600     MOVE 'KB-KOLLA-ARTIKEL ' TO CURRENT-SECTION                          
082700                                                                          
082800     PERFORM IMS-GNP-WDF812                                               
082900     IF SEGMENT-FINNS                                                     
083000     AND ASPR-TISTADAT NOT > DAGENS-DAT                                   
083100        MOVE JA  TO TRAEFF-SW                                             
083200     END-IF                                                               
083300     .                                                                    
083400                                                                          
083500                                                                          
083600 KC-KOLLA-URSPRUNG SECTION.                                               
083700     MOVE 'KC-KOLLA-URSPRUNG' TO CURRENT-SECTION                          
083800                                                                          
083900     IF EJ-TRAEFF                                                         
084200        IF CLAG-KDARTURS NOT = SPACE                                      
084300           MOVE CLAG-KDARTURS  TO W-KDARTURS                              
084400           PERFORM IMS-GNP-WDF813                                         
084500           IF SEGMENT-FINNS                                               
084600           AND USPR-TISTADAT NOT > DAGENS-DAT                             
084700              MOVE JA TO TRAEFF-SW                                        
084800           END-IF                                                         
084900        END-IF                                                            
085000     END-IF                                                               
085100     .                                                                    
085200                                                                          
085300                                                                          
085400 KD-KOLLA-PRODUKTSLAG SECTION.                                            
085500     MOVE 'KD-KOLLA-PRODSLAG' TO CURRENT-SECTION                          
085600                                                                          
085700     IF EJ-TRAEFF                                                         
085800        MOVE ART-KDPRODSL   TO W-KDPRODSL                                 
085900        PERFORM IMS-GNP-WDF814                                            
086000                                                                          
086100        IF SEGMENT-FINNS                                                  
086200        AND PSPR-TISTADAT NOT > DAGENS-DAT                                
086300           MOVE JA TO TRAEFF-SW                                           
086400        END-IF                                                            
086500     END-IF                                                               
086600     .                                                                    
086700                                                                          
086800                                                                          
086900 KE-KOLLA-FUNKTIONSGRUPP SECTION.                                         
087000     MOVE 'KE-KOLLA-FKNGRUP ' TO CURRENT-SECTION                          
087100                                                                          
087200     IF EJ-TRAEFF                                                         
087300        MOVE ART-IDFKNGRP   TO W-IDFKNGRP                                 
087400        PERFORM IMS-GNP-WDF815                                            
087500        IF SEGMENT-FINNS                                                  
087600        AND FSPR-TISTADAT NOT > DAGENS-DAT                                
087700           MOVE JA TO TRAEFF-SW                                           
087800        END-IF                                                            
087900     END-IF                                                               
088000     .                                                                    
088100                                                                          
088200                                                                          
088300 KF-SAETT-KDORDBEK SECTION.                                               
088400     MOVE 'KF-SAETT-KDORDBEK' TO CURRENT-SECTION                          
088500                                                                          
088600     EVALUATE SPAR-KDORDKL                                                
088700     WHEN 0                                                               
088800          MOVE SEQA-TISTADAT-KL0 TO WS-TISTADAT                           
088900     WHEN 1                                                               
089000          MOVE SEQA-TISTADAT-KL1 TO WS-TISTADAT                           
089100     WHEN 2                                                               
089200          MOVE SEQA-TISTADAT-KL2 TO WS-TISTADAT                           
089300     WHEN 3                                                               
089400          MOVE SEQA-TISTADAT-KL3 TO WS-TISTADAT                           
089500     WHEN OTHER                                                           
089600          MOVE SEQA-TISTADAT-KL4 TO WS-TISTADAT                           
089700     END-EVALUATE                                                         
089800                                                                          
089900     IF DAGENS-DAT >= WS-TISTADAT                                         
090000*----FIX FÖR KARINA JANSENIUS 21/6 -02, BS.                               
090100*------- VÄCKT IGEN FOR BUNNING BOARDS 10/4 -03 BS                        
090200        IF  SPAR-IDARTNR  = 8633464                                       
090300        OR  SPAR-IDARTNR  = 8698005                                       
090400         MOVE 67  TO SPAR-KDORDBEK                                        
090500*----END-FIX                                                              
090600        ELSE                                                              
090700          IF GSPR-KDMARKBLK = ZERO                                        
090900            MOVE 55 TO SPAR-KDORDBEK                                      
091000          ELSE                                                            
091100             IF GSPR-KDMARKBLK = 51                                       
091200                MOVE GSPR-KDMARKBLK                                       
091300                                TO SPAR-KDORDBEK                          
091500             ELSE                                                         
091600               IF (SPAR-IDSYSTEM = 'IMS '                                 
091700               OR SPAR-IDSYSTEM = 'REFB'                                  
091800               OR SPAR-IDSYSTEM = 'REPB')                                 
091900              AND (SPAR-FLFORBI = JA OR SPAR-FLFORBI = SPEC-FORBI)        
092000               AND SPAR-KDORDKL = 1                                       
092100                 CONTINUE                                                 
092200               ELSE                                                       
092300                 MOVE GSPR-KDMARKBLK                                      
092400                                TO SPAR-KDORDBEK                          
092600               END-IF                                                     
092700             END-IF                                                       
092800          END-IF                                                          
092900        END-IF                                                            
093000     END-IF                                                               
093100     .                                                                    
093200                                                                          
093300                                                                          
093400 IMS-GU-WDF8A1   SECTION.                                                 
093500     MOVE 'IMS-GU-WDF8A1   ' TO CURRENT-IMS-SECTION                       
093600                                                                          
093700     MOVE SPACE              TO ALL-SSA                                   
093800                                                                          
093900     STRING 'WDF8A1  (WDF8A1KY=<' W-WDF8ASEQ-X                            
094000                    '&IDDISTRF=<' W-IDDISTR-X                             
094100                    '&IDDISTRT=>' W-IDDISTR-X                             
094200                    '&IDKUNDNF=<' W-IDKUNDNR-X                            
094300                    '&IDKUNDNT=>' W-IDKUNDNR-X ')'                        
094400          DELIMITED BY SIZE INTO SSA1                                     
094500     MOVE '  GE'              TO GODK-STATUSKODER                         
094600     CALL CBLTDLI USING GU WDF8A-PCB DLI-IO-WDF8A1 SSA1                   
094700     MOVE WDF8A-STATUS-CODE   TO STATUS-WS                                
094800     PERFORM IMS-STATUSKONTROLL                                           
094900     .                                                                    
095000                                                                          
095100                                                                          
095200 IMS-GN-WDF8A1   SECTION.                                                 
095300     MOVE 'IMS-GN-WDF8A1   ' TO CURRENT-IMS-SECTION                       
095400                                                                          
095500     MOVE SPACE              TO ALL-SSA                                   
095600                                                                          
095700     STRING 'WDF8A1  (WDF8A1KY=<' W-WDF8ASEQ-X                            
095800                    '&IDDISTRF=<' W-IDDISTR-X                             
095900                    '&IDDISTRT=>' W-IDDISTR-X                             
096000                    '&IDKUNDNF=<' W-IDKUNDNR-X                            
096100                    '&IDKUNDNT=>' W-IDKUNDNR-X ')'                        
096200          DELIMITED BY SIZE INTO SSA1                                     
096300     MOVE '  GEGB'            TO GODK-STATUSKODER                         
096400     CALL CBLTDLI USING GN WDF8A-PCB DLI-IO-WDF8A1 SSA1                   
096500     MOVE WDF8A-STATUS-CODE   TO STATUS-WS                                
096600     PERFORM IMS-STATUSKONTROLL                                           
096700     .                                                                    
096800                                                                          
096900                                                                          
097000 IMS-GU-WDF801   SECTION.                                                 
097100     MOVE 'IMS-GU-WDF801   ' TO CURRENT-IMS-SECTION                       
097200                                                                          
097300     MOVE SPACE              TO ALL-SSA                                   
097400                                                                          
097500     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
097600          DELIMITED BY SIZE INTO SSA1                                     
097700     MOVE '    '              TO GODK-STATUSKODER                         
097800     CALL CBLTDLI USING GU WDF8-PCB DLI-IO-WDF801 SSA1                    
097900     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
098000     PERFORM IMS-STATUSKONTROLL                                           
098100     .                                                                    
098200                                                                          
098300                                                                          
098400 IMS-GNP-WDF811  SECTION.                                                 
098500     MOVE 'IMS-GNP-WDF811  ' TO CURRENT-IMS-SECTION                       
098600                                                                          
098700     MOVE SPACE              TO ALL-SSA                                   
098800                                                                          
098900     STRING 'WDF811  (IDDISTRF<=' W-IDDISTR-X                             
099000                    '&IDDISTRT>=' W-IDDISTR-X                             
099100                    '&IDKUNDNF<=' W-IDKUNDNR-X                            
099200                    '&IDKUNDNT>=' W-IDKUNDNR-X ')'                        
099300          DELIMITED BY SIZE INTO SSA1                                     
099400     MOVE '  GE'              TO GODK-STATUSKODER                         
099500     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF811 SSA1                   
099600     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
099700     PERFORM IMS-STATUSKONTROLL                                           
099800     .                                                                    
099900                                                                          
100000                                                                          
100100 IMS-GNP-WDF812  SECTION.                                                 
100200     MOVE 'IMS-GNP-WDF812  ' TO CURRENT-IMS-SECTION                       
100300                                                                          
100400     MOVE SPACE              TO ALL-SSA                                   
100500                                                                          
100600     STRING 'WDF812  (IDARTNR  =' W-IDARTNR-X ')'                         
100700          DELIMITED BY SIZE INTO SSA1                                     
100800     MOVE '  GE'              TO GODK-STATUSKODER                         
100900     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF812 SSA1                   
101000     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
101100     PERFORM IMS-STATUSKONTROLL                                           
101200     .                                                                    
101300                                                                          
101400                                                                          
101500 IMS-GNP-WDF813  SECTION.                                                 
101600     MOVE 'IMS-GNP-WDF813  ' TO CURRENT-IMS-SECTION                       
101700                                                                          
101800     MOVE SPACE              TO ALL-SSA                                   
101900                                                                          
102000     STRING 'WDF813  (KDARTURS =' W-KDARTURS-X ')'                        
102100          DELIMITED BY SIZE INTO SSA1                                     
102200     MOVE '  GE'              TO GODK-STATUSKODER                         
102300     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF813 SSA1                   
102400     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
102700                                                                          
102800                                                                          
102900 IMS-GNP-WDF814  SECTION.                                                 
103000     MOVE 'IMS-GNP-WDF814  ' TO CURRENT-IMS-SECTION                       
103100                                                                          
103200     MOVE SPACE              TO ALL-SSA                                   
103300                                                                          
103400     STRING 'WDF814  (KDPRODSF<=' W-KDPRODSL-X                            
103500                    '&KDPRODST>=' W-KDPRODSL-X ')'                        
103600          DELIMITED BY SIZE INTO SSA1                                     
103700     MOVE '  GE'              TO GODK-STATUSKODER                         
103800     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF814 SSA1                   
103900     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
104000     PERFORM IMS-STATUSKONTROLL                                           
104100     .                                                                    
104200                                                                          
104300                                                                          
104400 IMS-GNP-WDF815  SECTION.                                                 
104500     MOVE 'IMS-GNP-WDF815  ' TO CURRENT-IMS-SECTION                       
104600                                                                          
104700     MOVE SPACE              TO ALL-SSA                                   
104800                                                                          
104900     STRING 'WDF815  (IDFKNGRF<=' W-IDFKNGRP-X                            
105000                    '&IDFKNGRT>=' W-IDFKNGRP-X ')'                        
105100          DELIMITED BY SIZE INTO SSA1                                     
105200     MOVE '  GE'              TO GODK-STATUSKODER                         
105300     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF815 SSA1                   
105400     MOVE WDF8-STATUS-CODE   TO STATUS-WS                                 
105500     PERFORM IMS-STATUSKONTROLL                                           
105600     .                                                                    
105700                                                                          
105800                                                                          
105900 IMS-GU-WDK601 SECTION.                                                   
106000     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
106100                                                                          
106200     MOVE SPACE              TO ALL-SSA                                   
106300                                                                          
106400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
106500         DELIMITED BY SIZE INTO SSA1                                      
106600     MOVE '  GE'             TO GODK-STATUSKODER                          
106700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
106800     MOVE WDK6-STATUS-CODE   TO STATUS-WS                                 
106900     PERFORM IMS-STATUSKONTROLL                                           
107000     .                                                                    
107100                                                                          
107200                                                                          
107300 IMS-GNP-WDK611 SECTION.                                                  
107400     MOVE 'IMS-GNP-WDK611  ' TO CURRENT-IMS-SECTION                       
107500                                                                          
107600     MOVE SPACE              TO ALL-SSA                                   
107700                                                                          
107800     MOVE 'WDK611  '         TO SSA1                                      
107900     MOVE '    '             TO GODK-STATUSKODER                          
108000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
108100     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
108200     PERFORM IMS-STATUSKONTROLL                                           
108300     .                                                                    
108400                                                                          
108500                                                                          
108600 IMS-STATUSKONTROLL SECTION.                                              
108700                                                                          
108800     SET STATUS-IX TO 1                                                   
108900     SEARCH GODK-STATUS                                                   
109000       AT END                                                             
109100         CALL FELLOG                                                      
109200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
109300         CONTINUE                                                         
109400     END-SEARCH                                                           
109500     .                                                                    
109600*    -COPY WY2000P1                                                       
109700*    -COPY WY2000P2                                                       
