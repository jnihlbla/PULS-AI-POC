000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411TPO2.                                                
000500 AUTHOR.         ANNELIE ENGLUND                                          
000600 DATE-WRITTEN.   APRIL 1990                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET TAR EMOT TPO2-MÄRKTA RADER FRÅN RADBE-                
001200*        HANDLINGSPROGRAMMET W40212 OCH SVARSPROGRAMMET W40213.           
001300*        BEROENDE PÅ OM PROGRAMMET KALLAS IN FRÅN W40212 ELLER            
001400*        W40213 BLIR BEHANDLINGEN OLIKA.                                  
001500*        OM KONTROLLEN LÄMNAS FRÅN W40212 KONTROLLERAS RADENS             
001600*        FRYSTID OCH -OM DEN ÄR GODKÄND- LÄGGS RADEN UPP PÅ DEN           
001700*        PASSIVA ORDERRADSKÖN, WLORDP.                                    
001800*        OM KONTROLLEN LÄMNAS FRÅN W40213, LÄGGS RADEN UPP PÅ             
001900*        BÅDE WLORDP OCH LARMKÖN WLXXBU                                   
002000*                                                                         
002100*        PROGRAMMET LÄSER OCH                                             
002200*                   UPPDATERAR WLORDP (WDA5)  ORDERRADREGISTER            
002300*        PROGRAMMET LÄSER OCH                                             
002400*                   UPPDATERAR WLARTM (WDK9)  ARTIKELREGISTER             
002500*        PROGRAMMET LÄSER OCH                                             
002600*                   UPPDATERAR WLXXBU (WDR5)  LARMKÖ                      
002700*        PROGRAMMET UPPDATERAR WLFILA (WDR6)  TRANSAKTIONSBAS             
002800*        PROGRAMMET LÄSER      WLXXBV (WDR2)  TIDTABELL                   
002900*        PROGRAMMET LÄSER      WLXXBX (WDR2)  ÖVERSÄTTN ANSK-LARM         
003000*                                                                         
003100*    LÄNKAREA: W411TPO2                                                   
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800*    -COPY WY2000W1                                                       
003900     SKIP3                                                                
004000 77  IDPGM                       PIC X(08)   VALUE 'W411TPO2'.            
004100 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 77  W-TIREGDAT                  PIC 9(6).                                
004700                                                                          
004800 77  W-KVART                     PIC 9(7).                                
004900                                                                          
005000 77  EXEKV-TID                   PIC X(6).                                
005100                                                                          
005200 77  W-IDLOPNR                   PIC S9(3)    COMP-3.                     
005300                                                                          
005400 01  DAGENS-DAT-TIAAVV           PIC 9(4).                                
005500 01  DAGENS-DAT REDEFINES DAGENS-DAT-TIAAVV.                              
005600     03  DAGENS-DAT-AA           PIC 9(2).                                
005700     03  DAGENS-DAT-VV           PIC 9(2).                                
005800                                                                          
005900 01  TITPO-TIAAVV                PIC 9(4).                                
006000 01  W-TIAAVV REDEFINES TITPO-TIAAVV.                                     
006100     03  W-TIAAVV-AA             PIC 9(2).                                
006200     03  W-TIAAVV-VV             PIC 9(2).                                
006300 01  W-TIAVV REDEFINES TITPO-TIAAVV.                                      
006400     03  W-TIAVV-A               PIC 9(1).                                
006500     03  W-TIAVV-AVV             PIC 9(3).                                
006600                                                                          
006700 01  W-TISENBEK.                                                          
006800     03  W-TISENBEK-DAG          PIC 9(6).                                
006900     03  W-TISENBEK-KL           PIC 9(6).                                
007000                                                                          
007100 01  W-TISENBEK-KL-UPPD.                                                  
007200     03  W-TISENBEK-KL-HH        PIC 9(2)    VALUE ZERO.                  
007300     03  W-TISENBEK-KL-MM        PIC 9(2)    VALUE ZERO.                  
007400     03  W-TISENBEK-KL-SS        PIC 9(2)    VALUE ZERO.                  
007500     EJECT                                                                
007600*      --- VALID IDDC CODES                                               
007700*                                                                         
007800*01    -COPY WWDCKONS                                                     
007900       EJECT                                                              
008000                                                                          
008100 01  GENERELLA-SUBPROGRAM.                                                
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008600     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
008700     03  W411TIME                PIC X(8)    VALUE 'W411TIME'.            
008800                                                                          
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL ABEND                                            
009100                                                                          
009200 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
009300                                                                          
009400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
009500                                                                          
009600*01  -COPY WDATAREA                                                       
009700                                                                          
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL W009VADD                                         
010000                                                                          
010100 01 W009VADD-AREA.                                                        
010200    03 VECKO-DATUM-AAVV          PIC S9(5)   VALUE ZERO COMP-3.           
010300    03 VECKO-ANTAL               PIC S9(3)   VALUE ZERO COMP-3.           
010400                                                                          
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL SUBPROGRAM W411TIME                              
010700                                                                          
010800*01  -COPY W411TIME                                                       
010900                                                                          
011000     EJECT                                                                
011100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011400     SKIP3                                                                
011500 01  NYCKLAR-TILL-DLI.                                                    
011600                                                                          
011700     03  W-IDARTNR-X.                                                     
011800         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
011900                                                                          
012000     03  W-KDSEGKEY-X.                                                    
012100         05  W-KDSEGKEY          PIC X.                                   
012200                                                                          
012300     03  W-WDGX2225-X.                                                    
012400         05  W-IDHTYP-2225       PIC X(4)     VALUE '2225'.               
012500         05  W-VALFRI-2225       PIC X(26)    VALUE LOW-VALUE.            
012600                                                                          
012700     03  W-WDGX2223-X.                                                    
012800         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
012900         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
013000         05  W-VALFRI-2223       PIC X(24)    VALUE LOW-VALUE.            
013100                                                                          
013200     03  W-WDGX2224-X.                                                    
013300         05  W-TISENBEK-DAG-2224 PIC S9(7)    VALUE ZERO COMP-3.          
013400         05  W-TISENBEK-KL-2224  PIC S9(7)    VALUE ZERO COMP-3.          
013500         05  W-KDLARM-2224       PIC S9(3)    VALUE ZERO COMP-3.          
013600                                                                          
013700     03  W-WDGX2231-X.                                                    
013800         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
013900         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
014000                                                                          
014100     03  W-WDGX2232-X.                                                    
014200         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
014300         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
014400                                                                          
014500     03  W-DABEHOV-X.                                                     
014600         05  W-DABEHOV           PIC  9(6)    VALUE ZERO.                 
014700     EJECT                                                                
014800*    --- STATUS-KOD FRÅN IMS                                              
014900 01  STATUS-WS                   PIC XX.                                  
015000     88  SEGMENT-FINNS                       VALUE '  '.                  
015100     88  SEGMENT-HAR-LAGTS-TILL              VALUE '  '.                  
015200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015400     SKIP2                                                                
015500 01  GODK-STATUSKODER.                                                    
015600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015700     SKIP3                                                                
015800 01  SSA1                        PIC X(64).                               
015900 01  SSA2                        PIC X(64).                               
016000     EJECT                                                                
016100*    --- IMS FUNKTIONSKODER                                               
016200*01  -COPY W0003                                                          
016300     EJECT                                                                
016400*    ---  DLI INPUT-OUTPUT AREA                                           
016500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016600     SKIP3                                                                
016700 01  DLI-IO-AREA.                                                         
016800     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
016900     SKIP3                                                                
017000     03  WLORDP01 REDEFINES IO-AREA.                                      
017100*        05  -COPY WDA501                                                 
017200     EJECT                                                                
017300     03  WLXXBU01 REDEFINES IO-AREA.                                      
017400*        05  -COPY WDGX2223                                               
017500     EJECT                                                                
017600     03  WLXXBU11 REDEFINES IO-AREA.                                      
017700*        05  -COPY WDGX2224                                               
017800     EJECT                                                                
017900     03  WLXXBV11 REDEFINES IO-AREA.                                      
018000*        05  -COPY WDGX2226                                               
018100     EJECT                                                                
018200     03  WLARTM01 REDEFINES IO-AREA.                                      
018300*        05  -COPY WDK901                                                 
018400     EJECT                                                                
018500     03  WLARTM11 REDEFINES IO-AREA.                                      
018600*        05  -COPY WDK911                                                 
018700     EJECT                                                                
018800     03  WLXXBX11 REDEFINES IO-AREA.                                      
018900*        05  -COPY WDGX2232                                               
019000     EJECT                                                                
019100 01  DLI-IO-AREA2.                                                        
019200     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
019300     SKIP3                                                                
019400*    03  WLFILA01     -COPY WDR601           -RED IO-AREA2.               
019500*    07  W414203A     -COPY W414203A        -RED FIL-WDR601-DATA.         
019600*    07  W414204A     -COPY W414204A        -RED FIL-WDR601-DATA.         
019700     EJECT                                                                
019800 LINKAGE SECTION.                                                         
019900*                                                                         
020000*   -COPY W411TPO2                                                        
020100*                                                                         
020200     EJECT                                                                
020300*01  -COPY W0008      -PRE ORDP-                                          
020400     05  FILLER                  PIC X.                                   
020500     EJECT                                                                
020600*01  -COPY W0008      -PRE XXBU-                                          
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
020900*01  -COPY W0008      -PRE XXBV-                                          
021000     05  FILLER                  PIC X.                                   
021100     EJECT                                                                
021200*01  -COPY W0008      -PRE ARTM-                                          
021300     05  FILLER                  PIC X.                                   
021400     EJECT                                                                
021500*01  -COPY W0008      -PRE FILA-                                          
021600     05  FILLER                  PIC X.                                   
021700     EJECT                                                                
021800*01  -COPY W0008      -PRE XXBX-                                          
021900     05  FILLER                  PIC X.                                   
022000     EJECT                                                                
022100 01  TIME-4437-PCB               PIC X.                                   
022200     EJECT                                                                
022300 PROCEDURE DIVISION  USING TPO2-W411TPO2 ORDP-PCB                         
022400                           XXBU-PCB                                       
022500                           XXBV-PCB ARTM-PCB FILA-PCB                     
022600                           XXBX-PCB TIME-4437-PCB.                        
022700                                                                          
022800     MOVE ZERO TO TPO2-KDORDBEK                                           
022900     MOVE NEJ TO TPO2-FLKLAR                                              
023000                                                                          
023100     IF TPO2-IDSYSTEM NOT = 'OREL'                                        
023200        IF TPO2-FLORDSPE NOT = JA AND TPO2-FLOVRLEV NOT = JA              
023300          AND TPO2-FLFORBI = NEJ                                          
023400           IF TPO2-KDUART = 'S' OR 'P' OR 'M' OR 'L'                      
023500              IF TPO2-KDORDKL NOT = 0                                     
023600                 MOVE 70 TO TPO2-KDORDBEK                                 
023700                 MOVE 6 TO TPO2-KDTPOTYP                                  
023800              END-IF                                                      
023900           END-IF                                                         
024000           IF TPO2-KDTPOTYP = 2                                           
024100              PERFORM A-INIT                                              
024200              IF TPO2-KDORDBEH = 3                                        
024300                 PERFORM I-TILLAGG-TPO                                    
024400              END-IF                                                      
024500              IF (TPO2-KDORDBEH = 1 OR 8) AND TPO2-KDORDBEK = ZERO        
024600                 PERFORM B-KONTROLL                                       
024700                 IF TPO2-KDORDBEK = ZERO                                  
024800                    PERFORM D-RAD-MED-KDORDBEH1                           
024900                    PERFORM E-SKAPA-TRANS                                 
025000                    MOVE JA TO TPO2-FLKLAR                                
025100                 ELSE                                                     
025200                    MOVE NEJ TO TPO2-FLKLAR                               
025300                 END-IF                                                   
025400              END-IF                                                      
025500              IF TPO2-KDORDBEH = 2 OR (TPO2-KDORDBEH = 8 AND              
025600                                       TPO2-KDORDBEK = 70)                
025700                 PERFORM G-BERAKNA-BEKRTIDPKT                             
025800                 PERFORM H-RAD-MED-KDORDBEH2-OCH-LARMKO                   
025900                 PERFORM E-SKAPA-TRANS                                    
026000                 MOVE JA TO TPO2-FLKLAR                                   
026100                 IF TPO2-KDORDBEH = 2                                     
026200                    MOVE ZERO TO TPO2-KDORDBEK                            
026300                 END-IF                                                   
026400              END-IF                                                      
026500           END-IF                                                         
026600        END-IF                                                            
026700     END-IF                                                               
026800                                                                          
026900     GOBACK                                                               
027000     .                                                                    
027100     EJECT                                                                
027200 A-INIT SECTION.                                                          
027300                                                                          
027400     ACCEPT EXEKV-TID FROM TIME                                           
027500                                                                          
027600     MOVE 'IDAG' TO DAT-KDDATFORM                                         
027700     MOVE ZERO   TO DAT-I-TIDATUM                                         
027800     CALL WDATKONV USING DAT-KDDATFORM,                                   
027900                         DAT-I-TIDATUM,                                   
028000                         DAT-O-TIDATUM,                                   
028100                         DAT-KDSVAR                                       
028200     IF DAT-KDSVAR-OK                                                     
028300       MOVE DAT-TIAAVV-GRP TO DAGENS-DAT                                  
028400       MOVE DAT-TIAAMMDD   TO W-TIREGDAT                                  
028500     ELSE                                                                 
028600       MOVE 'FEL FRÅN SUBPROGRAM W411TPO2 I SECTION A' TO FELTEXT         
028700       CALL ABEND USING RKOD-ABEND                                        
028800     END-IF                                                               
028900                                                                          
029000     MOVE IDPGM          TO   FIL-IDPGM                                   
029100     ACCEPT FIL-TIREGDAT FROM  DATE                                       
029200     ACCEPT FIL-TIKLOCK  FROM  TIME                                       
029300     MOVE ZERO           TO   FIL-IDSEKVNR                                
029400     MOVE 'W414'         TO   FIL-CT-IDSYSTEM                             
029500     MOVE 'A'            TO   FIL-CT-IDVTYP                               
029600                                                                          
029700     .                                                                    
029800     EJECT                                                                
029900 B-KONTROLL SECTION.                                                      
030000                                                                          
030100     MOVE 'AAMMDD'   TO DAT-KDDATFORM                                     
030200     MOVE TPO2-TITPO TO DAT-I-TIDATUM                                     
030300     CALL WDATKONV USING DAT-KDDATFORM,                                   
030400                         DAT-I-TIDATUM,                                   
030500                         DAT-O-TIDATUM,                                   
030600                         DAT-KDSVAR                                       
030700                                                                          
030800     IF DAT-KDSVAR-OK                                                     
030900       MOVE DAT-TIAAVV-GRP TO TITPO-TIAAVV                                
031000     ELSE                                                                 
031100       MOVE 'FEL FRÅN SUBPROGRAM W411TPO2 I SECTION B' TO FELTEXT         
031200       CALL ABEND USING RKOD-ABEND                                        
031300     END-IF                                                               
031400                                                                          
031500     MOVE TPO2-KVFRYSTI TO VECKO-ANTAL                                    
031600     MOVE DAGENS-DAT-TIAAVV TO VECKO-DATUM-AAVV                           
031700                                                                          
031800     CALL W009VADD USING VECKO-DATUM-AAVV VECKO-ANTAL                     
031900                                                                          
032000     MOVE TITPO-TIAAVV       TO TMP1-YYMMDD                               
032100     MOVE VECKO-DATUM-AAVV   TO TMP2-YYMMDD                               
032200     PERFORM WY2000P1                                                     
032300     IF TMP1-YYMMDD < TMP2-YYMMDD                                         
032400       IF W-TIAAVV-AA = 00 AND DAGENS-DAT-AA = 99                         
032500         MOVE ZERO TO TPO2-KDORDBEK                                       
032600       ELSE                                                               
032700         MOVE 70 TO TPO2-KDORDBEK                                         
032800       END-IF                                                             
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 D-RAD-MED-KDORDBEH1 SECTION.                                             
033300                                                                          
033400     MOVE JA  TO RAD-FLTPOBEK                                             
033500     MOVE ZERO TO RAD-DASENDAT                                            
033600                  RAD-TISENBEK-KL                                         
033700     PERFORM S01-SKAPA-RADKO                                              
033800     PERFORM IMS-ISRT-ORDP-WDA501                                         
033900     IF SEGMENT-FINNS-REDAN                                               
034000       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
034100         ADD 1 TO RAD-IDLOPNR                                             
034200         PERFORM IMS-ISRT-ORDP-WDA501                                     
034300       END-PERFORM                                                        
034400     END-IF                                                               
034500     PERFORM DA-UPPDATERA-ARTREG                                          
034600                                                                          
034700     .                                                                    
034800     EJECT                                                                
034900 DA-UPPDATERA-ARTREG SECTION.                                             
035000                                                                          
035100     MOVE TPO2-IDARTNR TO W-IDARTNR                                       
035200     PERFORM IMS-GHU-ARTM-WDK901                                          
035300     ADD TPO2-KVBEART-Q TO ART-SUTPO-TOT                                  
035400     PERFORM IMS-REPL-ARTM-WDK9                                           
035500     MOVE TITPO-TIAAVV  TO W-DABEHOV                                      
035600     IF TITPO-TIAAVV NOT = ZERO                                           
035700       IF TITPO-TIAAVV < 5000                                             
035800         MOVE 20        TO W-DABEHOV (1:2)                                
035900       ELSE                                                               
036000         IF TITPO-TIAAVV < 9999                                           
036100           MOVE 19      TO W-DABEHOV (1:2)                                
036200         ELSE                                                             
036300           MOVE 999999  TO W-DABEHOV                                      
036400         END-IF                                                           
036500       END-IF                                                             
036600     END-IF                                                               
036700     PERFORM IMS-GHNP-ARTM-WDK911                                         
036800     IF SEGMENT-SAKNAS                                                    
036900       MOVE TITPO-TIAAVV   TO ANT-DABEHOV                                 
037000       IF TITPO-TIAAVV NOT = ZERO                                         
037100         IF TITPO-TIAAVV < 5000                                           
037200           MOVE 20        TO ANT-DABEHOV (1:2)                            
037300         ELSE                                                             
037400           IF TITPO-TIAAVV < 9999                                         
037500             MOVE 19      TO ANT-DABEHOV (1:2)                            
037600           ELSE                                                           
037700             MOVE 999999  TO ANT-DABEHOV                                  
037800           END-IF                                                         
037900         END-IF                                                           
038000       END-IF                                                             
038100       MOVE TPO2-KVBEART-Q TO ANT-SUTPO-PB                                
038200       MOVE ZERO           TO ANT-SUTPO-EJPB                              
038300       PERFORM IMS-ISRT-ARTM-WDK911                                       
038400     ELSE                                                                 
038500       ADD TPO2-KVBEART-Q TO ANT-SUTPO-PB                                 
038600       PERFORM IMS-REPL-ARTM-WDK9                                         
038700     END-IF                                                               
038800                                                                          
038900     .                                                                    
039000     EJECT                                                                
039100 E-SKAPA-TRANS SECTION.                                                   
039200                                                                          
039300     MOVE SPACE                        TO FIL-WDR601-DATA                 
039400     MOVE TPO2-IDARTNR                 TO 204-IDARTNR                     
039500     MOVE TPO2-BERADREF                TO 204-BERADREF                    
039600     MOVE TPO2-IDDISTR                 TO 204-IDDISTR                     
039700     MOVE TPO2-IDKONTO                 TO 204-IDKONTO                     
039800     MOVE TPO2-IDKST                   TO 204-IDKST                       
039900     MOVE TPO2-IDKUNDNR                TO 204-IDKUNDNR                    
040000     MOVE TPO2-IDKUNDRF                TO 204-IDKUNDRF                    
040100     MOVE TPO2-IDSYSTEM                TO 204-IDSYSTEM                    
040200     MOVE TPO2-KDFRAKT                 TO 204-KDFRAKT                     
040300     MOVE TPO2-KDKVBRYT                TO 204-KDKVBRYT                    
040400     MOVE TPO2-KDORDKL                 TO 204-KDORDKL                     
040500     MOVE TPO2-KDTPOTYP                TO 204-KDTPOTYP                    
040600     MOVE TPO2-KDUART                  TO 204-KDUART                      
040700     MOVE TPO2-KDVRINFO                TO 204-KDVRINFO                    
040800     MOVE TPO2-KVBEART-Q               TO 204-KVBEART-Q                   
040900     MOVE TPO2-PRARTNTO                TO 204-PRARTNTO                    
041000     MOVE TPO2-REKSIFFR                TO 204-REKSIFFR                    
041100     MOVE W-TIREGDAT                   TO 204-TIREGDAT                    
041200     MOVE TPO2-TITPO                   TO 204-TITPO                       
041300                                                                          
041400     ADD +1                            TO FIL-IDSEKVNR                    
041500     MOVE '204'                        TO FIL-CT-IDPTYP                   
041600     PERFORM IMS-ISRT-FILA-WDR6                                           
041700     PERFORM UNTIL SEGMENT-FINNS                                          
041800        ADD +1 TO FIL-IDSEKVNR                                            
041900        PERFORM IMS-ISRT-FILA-WDR6                                        
042000     END-PERFORM                                                          
042100     .                                                                    
042200     EJECT                                                                
042300 G-BERAKNA-BEKRTIDPKT SECTION.                                            
042400                                                                          
042500     MOVE 1 TO W-KDSEGKEY                                                 
042600     PERFORM IMS-GU-XXBV-WDR210                                           
042700     MOVE 2226-KVARBTIM(TPO2-KDORDKL , TPO2-KDTPOTYP)                     
042800                        TO TIME-TIARB                                     
042900     MOVE WC-CDC-SE     TO TIME-IDDC                                      
043000     MOVE W-TIREGDAT    TO TIME-START-TIAAMMDD                            
043100     MOVE EXEKV-TID     TO TIME-START-TIHHMMSS                            
043200     MOVE 022           TO TIME-KDCALL                                    
043300     MOVE ZERO          TO TIME-STOPDAT                                   
043400                                                                          
043500     CALL W411TIME USING TIME-W411TIME TIME-4437-PCB.                     
043600                                                                          
043700     IF TIME-KDSVAR-OK                                                    
043800       MOVE TIME-STOP-TIAAMMDD TO W-TISENBEK-DAG                          
043900       MOVE TIME-STOP-TIHHMMSS TO W-TISENBEK-KL                           
044000     ELSE                                                                 
044100       MOVE 'FEL FRÅN SUBPROGRAM W411TPO2 I SECTION G' TO FELTEXT         
044200       CALL ABEND USING RKOD-ABEND                                        
044300     END-IF                                                               
044400                                                                          
044500     .                                                                    
044600     EJECT                                                                
044700 H-RAD-MED-KDORDBEH2-OCH-LARMKO SECTION.                                  
044800                                                                          
044900     MOVE TPO2-IDANSK  TO W-IDANSK-2232                                   
045000     PERFORM IMS-GU-XXBX-WDR220                                           
045100     IF SEGMENT-FINNS                                                     
045200       MOVE 2232-IDANSK-LARM TO W-IDANSK-2223                             
045300     ELSE                                                                 
045400       MOVE ZERO TO W-IDANSK-2223                                         
045500     END-IF                                                               
045600     MOVE '2223'           TO 2223-IDHTYP                                 
045700     MOVE LOW-VALUE        TO 2223-LOW-VALUE                              
045800     MOVE W-IDANSK-2223    TO 2223-IDANSK                                 
045900     PERFORM IMS-ISRT-XXBU-WDR501                                         
046000     PERFORM IMS-GHU-XXBU-WDR501                                          
046100     MOVE W-TISENBEK-DAG  TO W-TISENBEK-DAG-2224                          
046200     MOVE W-TISENBEK-KL   TO W-TISENBEK-KL-2224                           
046300     MOVE 110             TO W-KDLARM-2224                                
046400     PERFORM IMS-GNP-XXBU-WDR550                                          
046500     IF SEGMENT-FINNS                                                     
046600       PERFORM UNTIL SEGMENT-SAKNAS                                       
046700         ADD 1 TO W-TISENBEK-KL-2224                                      
046800         PERFORM HB-KOLLA-TIDEN                                           
046900         PERFORM IMS-GNP-XXBU-WDR550                                      
047000       END-PERFORM                                                        
047100     END-IF                                                               
047200     PERFORM HA-LAGG-UPP-RAD                                              
047300     MOVE W-TISENBEK-DAG-2224 TO 2224-TISENBEK-DAG                        
047400     MOVE W-TISENBEK-KL-2224  TO 2224-TISENBEK-KL                         
047500     MOVE 110                 TO 2224-KDLARM                              
047600     MOVE TPO2-IDARTNR        TO 2224-IDARTNR                             
047610     MOVE WC-CDC-SE           TO 2224-IDDC                                
047700     MOVE JA                  TO 2224-FLNYLARM                            
047800     MOVE TPO2-IDDISTR        TO 2224-IDDISTR                             
047900     MOVE TPO2-IDKUNDNR       TO 2224-IDKUNDNR                            
048000     MOVE TPO2-IDKUNDRF       TO 2224-IDKUNDRF                            
048100     MOVE W-IDLOPNR           TO 2224-IDLOPNR                             
048200     MOVE W-TIREGDAT          TO 2224-TIREGDAT                            
048400     MOVE SPACE               TO 2224-IDTRANS                             
048500                                 2224-KDMFSFOR                            
048600     MOVE ZERO                TO 2224-IDKR                                
048610     MOVE SPACE               TO 2224-IDLEVNR                             
048700     PERFORM IMS-ISRT-XXBU-WDR550                                         
048800                                                                          
048900     .                                                                    
049000     EJECT                                                                
049100 HA-LAGG-UPP-RAD SECTION.                                                 
049200                                                                          
049300     MOVE W-TISENBEK-DAG-2224 TO RAD-DASENDAT                             
049400     IF W-TISENBEK-DAG-2224 NOT = ZERO                                    
049500       IF W-TISENBEK-DAG-2224 < 500000                                    
049600         MOVE 20              TO RAD-DASENDAT (1:2)                       
049700       ELSE                                                               
049800         IF W-TISENBEK-DAG-2224 < 999999                                  
049900           MOVE 19            TO RAD-DASENDAT (1:2)                       
050000         ELSE                                                             
050100           MOVE 99999999      TO RAD-DASENDAT                             
050200         END-IF                                                           
050300       END-IF                                                             
050400     END-IF                                                               
050500     MOVE W-TISENBEK-KL-2224  TO RAD-TISENBEK-KL                          
050600     MOVE NEJ TO RAD-FLTPOBEK                                             
050700     PERFORM S01-SKAPA-RADKO                                              
050800     PERFORM IMS-ISRT-ORDP-WDA501                                         
050900     IF SEGMENT-FINNS-REDAN                                               
051000       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
051100         ADD 1 TO RAD-IDLOPNR                                             
051200         PERFORM IMS-ISRT-ORDP-WDA501                                     
051300       END-PERFORM                                                        
051400     END-IF                                                               
051500     MOVE RAD-IDLOPNR TO W-IDLOPNR                                        
051600                                                                          
051700     .                                                                    
051800     EJECT                                                                
051900 HB-KOLLA-TIDEN SECTION.                                                  
052000                                                                          
052100     MOVE W-TISENBEK-KL-2224 TO W-TISENBEK-KL-UPPD                        
052200     IF W-TISENBEK-KL-SS > 59                                             
052300       ADD 1 TO W-TISENBEK-KL-MM                                          
052400       MOVE ZERO TO W-TISENBEK-KL-SS                                      
052500       MOVE W-TISENBEK-KL-UPPD TO W-TISENBEK-KL-2224                      
052600       IF W-TISENBEK-KL-MM > 59                                           
052700         ADD 1 TO W-TISENBEK-KL-HH                                        
052800         MOVE ZERO TO W-TISENBEK-KL-MM                                    
052900         MOVE W-TISENBEK-KL-UPPD TO W-TISENBEK-KL-2224                    
053000       END-IF                                                             
053100     END-IF                                                               
053200                                                                          
053300     .                                                                    
053400     EJECT                                                                
053500 I-TILLAGG-TPO SECTION.                                                   
053600                                                                          
053700     PERFORM S01-SKAPA-RADKO                                              
053800     PERFORM IA-KOMPLETTERA-RADEN                                         
053900     PERFORM IB-INSERT-RAD                                                
054000     PERFORM IC-UPPDATERA-WDK9                                            
054100     PERFORM ID-SKAPA-TRANS                                               
054200     MOVE JA                  TO TPO2-FLKLAR                              
054300     MOVE 71                  TO TPO2-KDORDBEK                            
054400     MOVE W-TIREGDAT          TO TPO2-TITPO                               
054500     .                                                                    
054600     EJECT                                                                
054700 IA-KOMPLETTERA-RADEN SECTION.                                            
054800                                                                          
054900     MOVE 3                   TO RAD-KDSTARAD                             
055000     MOVE W-TIREGDAT          TO RAD-TIRES                                
055100                                 RAD-TITPO                                
055200     MOVE JA                  TO RAD-FLTPOBEK                             
055300     MOVE ZERO                TO RAD-DASENDAT                             
055400                                 RAD-TISENBEK-KL                          
055500     .                                                                    
055600     EJECT                                                                
055700 IB-INSERT-RAD SECTION.                                                   
055800                                                                          
055900     PERFORM IMS-ISRT-ORDP-WDA501                                         
056000     IF SEGMENT-FINNS-REDAN                                               
056100       PERFORM UNTIL SEGMENT-HAR-LAGTS-TILL                               
056200         ADD 1 TO RAD-IDLOPNR                                             
056300         PERFORM IMS-ISRT-ORDP-WDA501                                     
056400       END-PERFORM                                                        
056500     END-IF                                                               
056600                                                                          
056700     .                                                                    
056800     EJECT                                                                
056900 IC-UPPDATERA-WDK9 SECTION.                                               
057000                                                                          
057100     MOVE TPO2-IDARTNR TO W-IDARTNR                                       
057200     PERFORM IMS-GHU-ARTM-WDK901                                          
057300     IF TPO2-KDORDKL = 1                                                  
057400        ADD TPO2-KVBEART-Q   TO ART-KVOKS-DAG                             
057500     ELSE                                                                 
057600        IF TPO2-KDORDKL = 2 OR 3 OR 4                                     
057700          ADD TPO2-KVBEART-Q TO ART-KVOKS-BULK                            
057800        ELSE                                                              
057900          ADD TPO2-KVBEART-Q TO ART-KVOKS-VOR                             
058000        END-IF                                                            
058100     END-IF                                                               
058200     PERFORM IMS-REPL-ARTM-WDK9                                           
058300     .                                                                    
058400     EJECT                                                                
058500                                                                          
058600 ID-SKAPA-TRANS SECTION.                                                  
058700                                                                          
058800     MOVE SPACE                        TO FIL-WDR601-DATA                 
058900     MOVE TPO2-IDARTNR                 TO 203-IDARTNR                     
059000     MOVE TPO2-BERADREF                TO 203-BERADREF                    
059100     MOVE TPO2-BEVOLREF                TO 203-BEVOLREF                    
059200     MOVE TPO2-FLTILLK                 TO 203-FLTILLK                     
059300     MOVE TPO2-IDDISTR                 TO 203-IDDISTR                     
059400     MOVE TPO2-IDKONTO                 TO 203-IDKONTO                     
059500     MOVE TPO2-IDKST                   TO 203-IDKST                       
059600     MOVE TPO2-IDKUNDNR                TO 203-IDKUNDNR                    
059700     MOVE TPO2-IDKUNDRF                TO 203-IDKUNDRF                    
059800     MOVE TPO2-IDSYSTEM                TO 203-IDSYSTEM                    
059900     MOVE TPO2-KDDSP                   TO 203-KDDSP                       
060000     MOVE TPO2-KDFAKTYP                TO 203-KDFAKTYP                    
060100     MOVE TPO2-KDFRAKT                 TO 203-KDFRAKT                     
060200     MOVE TPO2-KDKVBRYT                TO 203-KDKVBRYT                    
060300     MOVE 71                           TO 203-KDORDBEK                    
060400     MOVE TPO2-KDORDING                TO 203-KDORDING                    
060500     IF 203-KDORDING = +1                                                 
060600        MOVE +2                        TO 203-KDORDING                    
060700     END-IF                                                               
060800     MOVE TPO2-KDORDKL                 TO 203-KDORDKL                     
060900     MOVE TPO2-KDTPOTYP                TO 203-KDTPOTYP                    
061000     MOVE TPO2-KDVRINFO                TO 203-KDVRINFO                    
061100     MOVE TPO2-KVBEART-Q               TO 203-KVBEART-Q                   
061200     MOVE TPO2-PRARTNTO                TO 203-PRARTNTO                    
061300     MOVE TPO2-REKSIFFR                TO 203-REKSIFFR                    
061400     MOVE TPO2-TIDISPIN                TO 203-TIDISPIN                    
061500     MOVE TPO2-KVBEART                 TO 203-KVBEART                     
061600     MOVE TPO2-KVQPACK-1               TO 203-KVQPACK-1                   
061700     MOVE TPO2-BEVARREF                TO 203-BEVARREF                    
061800     MOVE TPO2-KDPRTYP                 TO 203-KDPRTYP                     
061900     MOVE TPO2-FLPRTILL                TO 203-FLPRTILL                    
062000     MOVE TPO2-FLINVEST                TO 203-FLINVEST                    
062100     MOVE TPO2-KDPRODSL                TO 203-KDPRODSL                    
062200     MOVE W-TIREGDAT                   TO 203-TIREGDAT                    
062300                                          203-TITPO                       
062400                                                                          
062500     ADD +1                            TO FIL-IDSEKVNR                    
062600     MOVE '203'                        TO FIL-CT-IDPTYP                   
062700     PERFORM IMS-ISRT-FILA-WDR6                                           
062800     PERFORM UNTIL SEGMENT-FINNS                                          
062900        ADD +1 TO FIL-IDSEKVNR                                            
063000        PERFORM IMS-ISRT-FILA-WDR6                                        
063100     END-PERFORM                                                          
063200     .                                                                    
063300     EJECT                                                                
063400 S01-SKAPA-RADKO SECTION.                                                 
063500                                                                          
063600     MOVE TPO2-IDDISTR        TO RAD-IDDISTR                              
063700     MOVE TPO2-IDKUNDNR       TO RAD-IDKUNDNR                             
063800     MOVE SPACE               TO RAD-IDKUNDRF                             
063900     MOVE TPO2-IDKUNDRF (3:5) TO RAD-IDORDNR5                             
064000     MOVE TPO2-IDARTNR        TO RAD-IDARTNR                              
064100     MOVE +1                  TO RAD-IDLOPNR                              
064200     MOVE TPO2-BERADREF       TO RAD-BERADREF                             
064300     MOVE NEJ                 TO RAD-FLERS                                
064400     MOVE TPO2-IDANSK         TO RAD-IDANSK                               
064500     MOVE TPO2-IDKONTO        TO RAD-IDKONTO                              
064600     MOVE TPO2-IDKST          TO RAD-IDKST                                
064700     MOVE TPO2-IDANALYS       TO RAD-IDANALYS                             
064800     MOVE '00000     '        TO RAD-IDKUNDRF-LEV                         
064900     MOVE WC-CDC-SE           TO RAD-IDDC                                 
065000                                 RAD-IDDC-RO                              
065100     MOVE 'DT'                TO RAD-KDOI                                 
065200     MOVE SPACE               TO RAD-CLEARGROUP                           
065300     MOVE TPO2-KDDSP          TO RAD-KDDSP                                
065400     MOVE TPO2-KDFAKTYP       TO RAD-KDFAKTYP                             
065500     MOVE TPO2-KDFRAKT        TO RAD-KDFRAKT                              
065600     MOVE TPO2-KDKVBRYT       TO RAD-KDKVBRYT                             
065700     MOVE TPO2-KDORDING       TO RAD-KDORDING                             
065800     IF RAD-KDORDING = +1                                                 
065900        MOVE +2               TO RAD-KDORDING                             
066000     END-IF                                                               
066100     MOVE TPO2-KDORDKL        TO RAD-KDORDKL                              
066200     MOVE TPO2-KDPRODSL       TO RAD-KDPRODSL                             
066300     MOVE 20                  TO RAD-KDRAPRIO                             
066400     MOVE ZERO                TO RAD-KDROO                                
066500     MOVE 1                   TO RAD-KDSTARAD                             
066600     MOVE TPO2-KDTPOTYP       TO RAD-KDTPOTYP                             
066700     MOVE TPO2-KDVRINFO       TO RAD-KDVRINFO                             
066800     MOVE TPO2-KVBEART-Q      TO RAD-KVART                                
066900                                 RAD-KVBEART-Q                            
067000     MOVE ZERO                TO RAD-KVRO                                 
067100     MOVE TPO2-PRARTNTO       TO RAD-PRARTNTO                             
067200     MOVE TPO2-DEAL-PR-LINE   TO RAD-DEAL-PR-LINE                         
067300     MOVE TPO2-REKSIFFR       TO RAD-REKSIFFR                             
067400     MOVE ZERO                TO RAD-TIAVBOKN                             
067500     MOVE W-TIREGDAT          TO RAD-TIREGDAT                             
067600     MOVE ZERO                TO RAD-TIRES                                
067700     MOVE ZERO                TO RAD-DARODAT                              
067800     MOVE TPO2-TITPO          TO RAD-TITPO                                
067900     MOVE TPO2-KDPRTYP        TO RAD-KDPRTYP                              
068000     MOVE TPO2-BEVOLREF       TO RAD-BEVOLREF                             
068100     MOVE TPO2-FLINVEST       TO RAD-FLINVEST                             
068200     MOVE TPO2-FLPRTILL       TO RAD-FLPRTILL                             
068300     MOVE TPO2-BEKUNDRF       TO RAD-BEKUNDRF                             
068400     MOVE TPO2-IDKAMPRF       TO RAD-IDKAMPRF                             
068500     MOVE TPO2-IDLEVNR        TO RAD-IDLEVNR                              
068600     MOVE TPO2-IDSYSTEM       TO RAD-IDSYSTEM                             
068700     MOVE EXEKV-TID           TO RAD-TIREGTID                             
068800                                                                          
068900     MOVE TPO2-KDORDTYP-LDC   TO RAD-KDORDTYP-LDC                         
069000     MOVE TPO2-TIREPDAT       TO RAD-TIREPDAT                             
069100     MOVE TPO2-IDKUNDRF-WIP   TO RAD-IDKUNDRF-WIP                         
069300     MOVE +0                  TO RAD-PRAVCOST                             
069310                                 RAD-KDROPACK                             
069320                                 RAD-IDARBREF                             
069400     .                                                                    
069500                                                                          
069600* --- IMS SEKTIONER ---                                                   
069700 IMS-GHU-ARTM-WDK901 SECTION.                                             
069800                                                                          
069900     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
070000          DELIMITED BY SIZE INTO SSA1                                     
070100     MOVE '  ' TO GODK-STATUSKODER                                        
070200     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA SSA1                     
070300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
070400     PERFORM IMS-STATUSKONTROLL                                           
070500     .                                                                    
070600     EJECT                                                                
070700 IMS-GHNP-ARTM-WDK911 SECTION.                                            
070800                                                                          
070900     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
071000          DELIMITED BY SIZE INTO SSA1                                     
071100     MOVE '  GE' TO GODK-STATUSKODER                                      
071200     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-AREA SSA1                    
071300     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
071400     PERFORM IMS-STATUSKONTROLL                                           
071500     .                                                                    
071600     EJECT                                                                
071700 IMS-GU-XXBV-WDR210 SECTION.                                              
071800                                                                          
071900     STRING 'WLXXBV01(WDGXKEY  =' W-WDGX2225-X ')'                        
072000          DELIMITED BY SIZE INTO SSA1                                     
072100     STRING 'WLXXBV11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
072200          DELIMITED BY SIZE INTO SSA2                                     
072300     MOVE '  ' TO GODK-STATUSKODER                                        
072400     CALL CBLTDLI USING GU XXBV-PCB DLI-IO-AREA SSA1 SSA2                 
072500     MOVE XXBV-STATUS-CODE TO STATUS-WS                                   
072600     PERFORM IMS-STATUSKONTROLL                                           
072700     .                                                                    
072800     EJECT                                                                
072900 IMS-GU-XXBX-WDR220 SECTION.                                              
073000                                                                          
073100     STRING 'WLXXBX01(WDGXKEY  =' W-WDGX2231-X ')'                        
073200          DELIMITED BY SIZE INTO SSA1                                     
073300     STRING 'WLXXBX11(WDGXKEY  =' W-WDGX2232-X ')'                        
073400          DELIMITED BY SIZE INTO SSA2                                     
073500     MOVE '  GE' TO GODK-STATUSKODER                                      
073600     CALL CBLTDLI USING GU XXBX-PCB DLI-IO-AREA SSA1 SSA2                 
073700     MOVE XXBX-STATUS-CODE TO STATUS-WS                                   
073800     PERFORM IMS-STATUSKONTROLL                                           
073900     .                                                                    
074000     EJECT                                                                
074100 IMS-ISRT-ORDP-WDA501 SECTION.                                            
074200                                                                          
074300     MOVE   'WLORDP01 ' TO SSA1                                           
074400     MOVE '  II' TO GODK-STATUSKODER                                      
074500     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA SSA1                    
074600     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
074700     PERFORM IMS-STATUSKONTROLL                                           
074800     .                                                                    
074900     EJECT                                                                
075000 IMS-REPL-ARTM-WDK9 SECTION.                                              
075100                                                                          
075200     MOVE '  ' TO GODK-STATUSKODER                                        
075300     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA                         
075400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
075500     PERFORM IMS-STATUSKONTROLL                                           
075600     .                                                                    
075700     EJECT                                                                
075800 IMS-ISRT-XXBU-WDR501 SECTION.                                            
075900                                                                          
076000     MOVE 'WLXXBU01 ' TO SSA1                                             
076100     MOVE '  II' TO GODK-STATUSKODER                                      
076200     CALL CBLTDLI USING ISRT XXBU-PCB DLI-IO-AREA SSA1                    
076300     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
076400     PERFORM IMS-STATUSKONTROLL                                           
076500     .                                                                    
076600     EJECT                                                                
076700 IMS-GHU-XXBU-WDR501 SECTION.                                             
076800                                                                          
076900     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
077000          DELIMITED BY SIZE INTO SSA1                                     
077100     MOVE '  ' TO GODK-STATUSKODER                                        
077200     CALL CBLTDLI USING GHU XXBU-PCB DLI-IO-AREA SSA1                     
077300     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
077400     PERFORM IMS-STATUSKONTROLL                                           
077500     .                                                                    
077600     EJECT                                                                
077700 IMS-GNP-XXBU-WDR550 SECTION.                                             
077800                                                                          
077900     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
078000          DELIMITED BY SIZE INTO SSA1                                     
078100     STRING 'WLXXBU11(WDGXKEY  =' W-WDGX2224-X ')'                        
078200          DELIMITED BY SIZE INTO SSA2                                     
078300     MOVE '  GE' TO GODK-STATUSKODER                                      
078400     CALL CBLTDLI USING GNP XXBU-PCB DLI-IO-AREA SSA1 SSA2                
078500     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
078600     PERFORM IMS-STATUSKONTROLL                                           
078700     .                                                                    
078800     EJECT                                                                
078900 IMS-ISRT-XXBU-WDR550 SECTION.                                            
079000                                                                          
079100     STRING 'WLXXBU01(WDGXKEY  =' W-WDGX2223-X ')'                        
079200          DELIMITED BY SIZE INTO SSA1                                     
079300     MOVE 'WLXXBU11 ' TO SSA2                                             
079400     MOVE '  II' TO GODK-STATUSKODER                                      
079500     CALL CBLTDLI USING ISRT XXBU-PCB DLI-IO-AREA SSA1 SSA2               
079600     MOVE XXBU-STATUS-CODE TO STATUS-WS                                   
079700     PERFORM IMS-STATUSKONTROLL                                           
079800     .                                                                    
079900     EJECT                                                                
080000 IMS-ISRT-FILA-WDR6 SECTION.                                              
080100                                                                          
080200     MOVE   'WLFILA01 ' TO SSA1                                           
080300     MOVE '  II' TO GODK-STATUSKODER                                      
080400     CALL CBLTDLI USING ISRT FILA-PCB DLI-IO-AREA2 SSA1                   
080500     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
080600     PERFORM IMS-STATUSKONTROLL                                           
080700     .                                                                    
080800     EJECT                                                                
080900 IMS-ISRT-ARTM-WDK911 SECTION.                                            
081000                                                                          
081100     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
081200          DELIMITED BY SIZE INTO SSA1                                     
081300     MOVE 'WLARTM11 ' TO SSA2                                             
081400     MOVE '  ' TO GODK-STATUSKODER                                        
081500     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-AREA SSA1 SSA2               
081600     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
081700     PERFORM IMS-STATUSKONTROLL                                           
081800     .                                                                    
081900     EJECT                                                                
082000 IMS-STATUSKONTROLL SECTION.                                              
082100                                                                          
082200     SET STATUS-IX TO 1                                                   
082300     SEARCH GODK-STATUS                                                   
082400       AT END                                                             
082500       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
082600       DELIMITED BY SIZE INTO FELTEXT                                     
082700       CALL FELLOG                                                        
082800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
082900     END-SEARCH                                                           
083000     .                                                                    
083100     EJECT                                                                
083200*    -COPY WY2000P1                                                       
