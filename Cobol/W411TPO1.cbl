000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411TPO1.                                                
000500 AUTHOR.         ANNELIE ENGLUND                                          
000600 DATE-WRITTEN.   APRIL 1990                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET TAR EMOT TPO1-MÄRKTA RADER FRÅN HUVUD-                
001200*        PROGRAMMET. LOGISKA KONTROLLER AV TPO-MÄRKNING,                  
001300*        FRYSTID,TPO-DATUM, OCH RADSTATUS  GÖRS. RADEN                    
001400*        LÄGGS SEDAN UPP PÅ DEN PASSIVA KÖN, WLORDP. OM EN                
001500*        RAD TILLHÖR EN VIPS/VR-ORDER, KAN DEN REDAN FINNAS               
001600*        PÅ WLORDP,OCH ERSÄTTS DÅ BARA.                                   
001700*        ON-LINE RADER LÄGGS UPP PÅ WLORDP DIREKT.                        
001800*                                                                         
001900*                                                                         
002000*        PROGRAMMET LÄSER OCH                                             
002100*                   UPPDATERAR WLORDP (WDA5)  ORDERRADREGISTER            
002200*        PROGRAMMET LÄSER OCH                                             
002300*                   UPPDATERAR WLARTM (WDK9)  ARTIKELREGISTER             
002400*                                                                         
002500*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)  TRANSAKTIONSBAS             
002600*                                                                         
002700*    LÄNKAREA: W411TPO1                                                   
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400*    -COPY  WY2000W1                                                      
003500     SKIP3                                                                
003600 77  IDPGM                       PIC X(08)   VALUE 'W411TPO1'.            
003700 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200 77  EXEKV-TID                   PIC X(6).                                
004300                                                                          
004400 77  W-TIREGDAT                  PIC 9(6).                                
004500                                                                          
004600 77  W-KVART                     PIC 9(7).                                
004700                                                                          
004800 01  DAGENS-DAT-TIAAVV           PIC 9(4)    VALUE ZERO.                  
004900 01  DAGENS-DAT REDEFINES DAGENS-DAT-TIAAVV.                              
005000     03  DAGENS-DAT-AA           PIC 9(2).                                
005100     03  DAGENS-DAT-VV           PIC 9(2).                                
005200                                                                          
005300 01  TITPO-TIAAVV                PIC 9(4)    VALUE ZERO.                  
005400 01  W-TIAAVV REDEFINES TITPO-TIAAVV.                                     
005500     03  W-TIAAVV-AA             PIC 9(2).                                
005600     03  W-TIAAVV-VV             PIC 9(2).                                
005700 01  W-TIAVV REDEFINES TITPO-TIAAVV.                                      
005800     03  W-TIAVV-A               PIC 9(1).                                
005900     03  W-TIAVV-AVV             PIC 9(3).                                
006000                                                                          
006100 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
006200                                                                          
006300 01  GENERELLA-SUBPROGRAM.                                                
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006800     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
006900                                                                          
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
007200                                                                          
007300*01  -COPY WDATAREA                                                       
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL SUBPROGRAM W009VADD                              
007600                                                                          
007700 01 W009VADD-AREA.                                                        
007800    03  VECKO-DATUM-AAVV         PIC S9(5)   VALUE ZERO COMP-3.           
007900    03  VECKO-ANTAL              PIC S9(3)   VALUE ZERO COMP-3.           
008000                                                                          
008100     EJECT                                                                
008200 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
008300                                                                          
008400 01  FILLER REDEFINES TEST-IDDISTR.                                       
008500*    03 -COPY WWDIST23                                                    
008600     EJECT                                                                
008700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009000 01  NYCKLAR-TILL-DLI.                                                    
009100                                                                          
009200     03  W-WDA501KY-MIN.                                                  
009300         05  W-IDDISTR-MIN       PIC S9(5) COMP-3   VALUE ZERO.           
009400         05  W-IDKUNDNR-MIN      PIC S9(7) COMP-3   VALUE ZERO.           
009500         05  W-IDKUNDRF-MIN      PIC X(10)          VALUE SPACE.          
009600         05  W-IDARTNR-MIN       PIC S9(9) COMP-3   VALUE ZERO.           
009700         05  W-IDLOPNR-MIN       PIC S9(3) COMP-3   VALUE ZERO.           
009800                                                                          
009900     03  W-WDA501KY-MAX.                                                  
010000         05  W-IDDISTR-MAX       PIC S9(5) COMP-3   VALUE ZERO.           
010100         05  W-IDKUNDNR-MAX      PIC S9(7) COMP-3   VALUE ZERO.           
010200         05  W-IDKUNDRF-MAX      PIC X(10)          VALUE ZERO.           
010300         05  W-IDARTNR-MAX       PIC S9(9) COMP-3   VALUE ZERO.           
010400         05  W-IDLOPNR-MAX       PIC S9(3) COMP-3   VALUE ZERO.           
010500                                                                          
010600     03  W-IDARTNR-X.                                                     
010700         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
010800                                                                          
010900     03  W-KDTPOTYP-X.                                                    
011000         05  W-KDTPOTYP          PIC S9       VALUE ZERO COMP-3.          
011100                                                                          
011200     03  W-KDSTARAD-X.                                                    
011300         05  W-KDSTARAD          PIC X        VALUE SPACE.                
011400                                                                          
011500     03  W-TITPO-X.                                                       
011600         05  W-TITPO             PIC S9(7)    VALUE ZERO COMP-3.          
011700                                                                          
011800     03  W-DABEHOV-X.                                                     
011900         05  W-DABEHOV           PIC  9(6)    VALUE ZERO.                 
012000                                                                          
012100     EJECT                                                                
012200*    --- AREOR TILL TRANSAR                                               
012300                                                                          
012400*01  -COPY WDGZRYA                                                        
012500     EJECT                                                                
012600*      --- VALID IDDC CODES                                               
012700*                                                                         
012800*01    -COPY WWDCKONS                                                     
012900       EJECT                                                              
013000*                                                                         
013100*    --- STATUS-KOD FRÅN IMS                                              
013200 01  STATUS-WS                   PIC XX.                                  
013300     88  SEGMENT-FINNS                       VALUE '  '.                  
013400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013600     SKIP2                                                                
013700 01  GODK-STATUSKODER.                                                    
013800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01  SSA1                        PIC X(150).                              
014100 01  SSA2                        PIC X(32).                               
014200     EJECT                                                                
014300*    --- IMS FUNKTIONSKODER                                               
014400*01  -COPY W0003                                                          
014500     EJECT                                                                
014600*    ---  DLI INPUT-OUTPUT AREA                                           
014700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014800     SKIP3                                                                
014900 01  DLI-IO-AREA.                                                         
015000     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
015100     SKIP3                                                                
015200     03  WLORDP01 REDEFINES IO-AREA.                                      
015300*        05  -COPY WDA501                                                 
015400     EJECT                                                                
015500     03  WLARTM01 REDEFINES IO-AREA.                                      
015600*        05  -COPY WDK901                                                 
015700     EJECT                                                                
015800     03  WLARTM11 REDEFINES IO-AREA.                                      
015900*        05  -COPY WDK911                                                 
016000     EJECT                                                                
016100     03  WLZZAC01 REDEFINES IO-AREA.                                      
016200*        05  -COPY WDGZ01     -PRE ZZAC-                                  
016300     EJECT                                                                
016400 LINKAGE SECTION.                                                         
016500                                                                          
016600*                                                                         
016700*   -COPY W411TPO1                                                        
016800*                                                                         
016900     EJECT                                                                
017000*01  -COPY W0008      -PRE ORDP-                                          
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300*01  -COPY W0008      -PRE ARTM-                                          
017400     05  FILLER                  PIC X.                                   
017500     EJECT                                                                
017600*01  -COPY W0008      -PRE ZZAC-                                          
017700     05  FILLER                  PIC X.                                   
017800     EJECT                                                                
017900 PROCEDURE DIVISION  USING TPO1-W411TPO1 ORDP-PCB                         
018000                           ARTM-PCB ZZAC-PCB.                             
018100                                                                          
018200     MOVE ZERO TO TPO1-KDORDBEK                                           
018300     MOVE ZERO TO TPO1-KVANNANT                                           
018400     MOVE NEJ  TO TPO1-FLKLAR                                             
018500     MOVE TPO1-IDDISTR TO TEST-IDDISTR                                    
018600                                                                          
018700     IF TPO1-IDSYSTEM NOT = 'OREL'                                        
018800        IF TPO1-FLORDSPE NOT = JA AND TPO1-FLOVRLEV NOT = JA              
018900           IF TPO1-KDTPOTYP = 1 AND TPO1-FLFORBI = NEJ                    
019000              IF TPO1-FLTPO1 = JA OR                                      
019100                 ((DIST23-TPO1) AND TPO1-FLTPO1 = NEJ)                    
019200                 PERFORM A-INIT                                           
019300                 PERFORM B-KONTROLL                                       
019400                 IF TPO1-KDORDBEK = ZERO                                  
019500                    IF TPO1-IDSYSTEM = 'IMS'                              
019600                      PERFORM D-ORDERRAD-FRAN-SKARM                       
019700                    ELSE                                                  
019800                      PERFORM E-ORDERRAD-FRAN-SYSTEM                      
019900                    END-IF                                                
020000                 END-IF                                                   
020100                 IF TPO1-KDORDBEK = ZERO OR 85                            
020200                     IF TPO1-KDORDBEK = ZERO AND                          
020300                                        TPO1-KVBEART-Q = +0               
020400                         CONTINUE                                         
020500                     ELSE                                                 
020600                         PERFORM F-SKAPA-TRANS-TILL-VR                    
020700                     END-IF                                               
020800                     MOVE JA TO TPO1-FLKLAR                               
020900                 ELSE                                                     
021000                     MOVE NEJ TO TPO1-FLKLAR                              
021100                 END-IF                                                   
021200              ELSE                                                        
021300                 MOVE 73 TO TPO1-KDORDBEK                                 
021400                 MOVE NEJ TO TPO1-FLKLAR                                  
021500              END-IF                                                      
021600           END-IF                                                         
021700        END-IF                                                            
021800     END-IF                                                               
021900     GOBACK                                                               
022000     .                                                                    
022100     EJECT                                                                
022200 A-INIT SECTION.                                                          
022300                                                                          
022400     ACCEPT EXEKV-TID FROM TIME                                           
022500                                                                          
022600     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
022700     MOVE ZERO     TO DAT-I-TIDATUM                                       
022800     CALL WDATKONV USING DAT-KDDATFORM,                                   
022900                         DAT-I-TIDATUM,                                   
023000                         DAT-O-TIDATUM,                                   
023100                         DAT-KDSVAR                                       
023200     IF DAT-KDSVAR-OK                                                     
023300       MOVE DAT-TIAAVV-GRP TO DAGENS-DAT                                  
023400       MOVE DAT-TIAAMMDD   TO W-TIREGDAT                                  
023500     ELSE                                                                 
023600       MOVE 'FEL FRÅN SUBPROGRAM W411TPO1 I SECTION A' TO FELTEXT         
023700       CALL ABEND USING RKOD-ABEND                                        
023800     END-IF                                                               
023900                                                                          
024000     MOVE 'AAMMDD'   TO DAT-KDDATFORM                                     
024100     MOVE TPO1-TITPO TO DAT-I-TIDATUM                                     
024200     CALL WDATKONV USING DAT-KDDATFORM,                                   
024300                         DAT-I-TIDATUM,                                   
024400                         DAT-O-TIDATUM,                                   
024500                         DAT-KDSVAR                                       
024600                                                                          
024700     IF DAT-KDSVAR-OK                                                     
024800       MOVE DAT-TIAAVV-GRP TO TITPO-TIAAVV                                
024900     ELSE                                                                 
025000       MOVE 'FEL FRÅN SUBPROGRAM W411TPO1 I SECTION A' TO FELTEXT         
025100       CALL ABEND USING RKOD-ABEND                                        
025200     END-IF                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 B-KONTROLL SECTION.                                                      
025600                                                                          
025700     IF DIST23-TPO1 OR TPO1-IDSYSTEM = 'VR'                               
025800        CONTINUE                                                          
025900     ELSE                                                                 
026000       MOVE TPO1-KVFRYSTI TO VECKO-ANTAL                                  
026100       MOVE DAGENS-DAT-TIAAVV TO VECKO-DATUM-AAVV                         
026200                                                                          
026300       CALL W009VADD USING VECKO-DATUM-AAVV VECKO-ANTAL                   
026400                                                                          
026500       MOVE TITPO-TIAAVV       TO TMP1-YYMMDD                             
026600       MOVE VECKO-DATUM-AAVV   TO TMP2-YYMMDD                             
026700       PERFORM WY2000P1                                                   
026800       IF TMP1-YYMMDD < TMP2-YYMMDD                                       
026900         IF W-TIAAVV-AA = 00 AND DAGENS-DAT-AA = 99                       
027000           MOVE ZERO TO TPO1-KDORDBEK                                     
027100         ELSE                                                             
027200           MOVE 74   TO TPO1-KDORDBEK                                     
027300         END-IF                                                           
027400       END-IF                                                             
027500     END-IF                                                               
027600     .                                                                    
027700     EJECT                                                                
027800 D-ORDERRAD-FRAN-SKARM SECTION.                                           
027900                                                                          
028000     MOVE TPO1-IDDISTR       TO W-IDDISTR-MIN                             
028100                                W-IDDISTR-MAX                             
028200     MOVE TPO1-IDKUNDNR      TO W-IDKUNDNR-MIN                            
028300                                W-IDKUNDNR-MAX                            
028400     MOVE SPACE              TO W-IDKUNDRF-MIN                            
028500                                W-IDKUNDRF-MAX                            
028600     MOVE TPO1-IDKUNDRF(3:5) TO W-IDKUNDRF-MIN                            
028700                                W-IDKUNDRF-MAX                            
028800     MOVE TPO1-IDARTNR       TO W-IDARTNR-MIN                             
028900                                W-IDARTNR-MAX                             
029000     MOVE ZERO               TO W-IDLOPNR-MIN                             
029100     MOVE 999                TO W-IDLOPNR-MAX                             
029200     MOVE 1                  TO W-KDTPOTYP                                
029300     MOVE 1                  TO W-KDSTARAD                                
029400     MOVE TPO1-TITPO         TO W-TITPO                                   
029500     PERFORM IMS-GHU-ORDP-WDA501                                          
029600     IF SEGMENT-FINNS                                                     
029700       MOVE 72 TO TPO1-KDORDBEK                                           
029800     ELSE                                                                 
029900       PERFORM S01A-SKAPA-RADKO                                           
030000       PERFORM IMS-ISRT-ORDP-WDA501                                       
030100       PERFORM UNTIL SEGMENT-FINNS                                        
030200         ADD 1 TO RAD-IDLOPNR                                             
030300         PERFORM IMS-ISRT-ORDP-WDA501                                     
030400       END-PERFORM                                                        
030500       PERFORM S02-UPPDATERA-ARTREG                                       
030600     END-IF                                                               
030700     .                                                                    
030800     EJECT                                                                
030900 E-ORDERRAD-FRAN-SYSTEM SECTION.                                          
031000                                                                          
031100     MOVE TPO1-IDDISTR       TO W-IDDISTR-MIN                             
031200                                W-IDDISTR-MAX                             
031300     MOVE TPO1-IDKUNDNR      TO W-IDKUNDNR-MIN                            
031400                                W-IDKUNDNR-MAX                            
031500     MOVE SPACE              TO W-IDKUNDRF-MIN                            
031600                                W-IDKUNDRF-MAX                            
031700     MOVE TPO1-IDKUNDRF(3:5) TO W-IDKUNDRF-MIN                            
031800                                W-IDKUNDRF-MAX                            
031900     MOVE TPO1-IDARTNR       TO W-IDARTNR-MIN                             
032000                                W-IDARTNR-MAX                             
032100     MOVE ZERO               TO W-IDLOPNR-MIN                             
032200     MOVE 999                TO W-IDLOPNR-MAX                             
032300     MOVE 1                  TO W-KDTPOTYP                                
032400     MOVE 1                  TO W-KDSTARAD                                
032500     MOVE TPO1-TITPO         TO W-TITPO                                   
032600     PERFORM IMS-GHU-ORDP-WDA501                                          
032700                                                                          
032800     IF SEGMENT-FINNS                                                     
032900       MOVE RAD-KVART TO W-KVART                                          
033000       IF TPO1-KVBEART-Q = 0                                              
033100         PERFORM IMS-DLET-ORDP-WDA501                                     
033200         MOVE 85 TO TPO1-KDORDBEK                                         
033300         MOVE W-KVART TO TPO1-KVANNANT                                    
033400       ELSE                                                               
033500         PERFORM S01B-SKAPA-RADKO                                         
033600         PERFORM IMS-REPL-ORDP-WDA501                                     
033700         IF RAD-KVART < W-KVART                                           
033800           MOVE 85 TO TPO1-KDORDBEK                                       
033900           COMPUTE TPO1-KVANNANT = W-KVART - RAD-KVART                    
034000         END-IF                                                           
034100       END-IF                                                             
034200       MOVE TPO1-IDARTNR TO W-IDARTNR                                     
034300       PERFORM IMS-GHU-ARTM-WDK901                                        
034400       COMPUTE ART-SUTPO-TOT =                                            
034500               ART-SUTPO-TOT - (W-KVART - TPO1-KVBEART-Q)                 
034600       PERFORM IMS-REPL-ARTM-WDK9                                         
034700       MOVE TITPO-TIAAVV   TO W-DABEHOV                                   
034800       IF TITPO-TIAAVV NOT = ZERO                                         
034900         IF TITPO-TIAAVV < 5000                                           
035000           MOVE 20         TO W-DABEHOV (1:2)                             
035100         ELSE                                                             
035200           IF TITPO-TIAAVV < 9999                                         
035300             MOVE 19       TO W-DABEHOV (1:2)                             
035400           ELSE                                                           
035500             MOVE 999999   TO W-DABEHOV                                   
035600           END-IF                                                         
035700         END-IF                                                           
035800       END-IF                                                             
035900       PERFORM IMS-GHNP-ARTM-WDK911                                       
036000       COMPUTE ANT-SUTPO-PB = ANT-SUTPO-PB -                              
036100               (W-KVART - TPO1-KVBEART-Q)                                 
036200       PERFORM S03-UPPDATERA-WDK911                                       
036300     ELSE                                                                 
036400         IF TPO1-KVBEART-Q > +0                                           
036500             PERFORM S01A-SKAPA-RADKO                                     
036600             PERFORM IMS-ISRT-ORDP-WDA501                                 
036700                                                                          
036800             PERFORM UNTIL SEGMENT-FINNS                                  
036900               ADD 1 TO RAD-IDLOPNR                                       
037000               PERFORM IMS-ISRT-ORDP-WDA501                               
037100             END-PERFORM                                                  
037200                                                                          
037300             PERFORM S02-UPPDATERA-ARTREG                                 
037400         END-IF                                                           
037500     END-IF                                                               
037600     .                                                                    
037700     EJECT                                                                
037800 F-SKAPA-TRANS-TILL-VR SECTION.                                           
037900                                                                          
038000     IF TPO1-IDSYSTEM NOT = 'VR'                                          
038100                                                                          
038200       MOVE +1             TO ZZAC-IDLOGLOP                               
038300       MOVE 'RYA'          TO RYA-IDPTYP                                  
038400                              ZZAC-IDPTYP                                 
038500       ACCEPT ZZAC-TIAAMMDD FROM DATE                                     
038600       ACCEPT ZZAC-TIKLOCK  FROM TIME                                     
038700       MOVE TPO1-IDDISTR   TO RYA-IDDISTR                                 
038800       MOVE TPO1-IDKUNDNR  TO RYA-IDKUNDNR                                
038900       MOVE TPO1-IDKUNDRF  TO RYA-IDKUNDRF                                
039000       MOVE TPO1-IDARTNR   TO RYA-IDARTNR                                 
039100       MOVE TPO1-REKSIFFR  TO RYA-REKSIFFR                                
039200       MOVE TPO1-KVBEART-Q TO RYA-KVBEART                                 
039300       MOVE TPO1-TITPO     TO RYA-TITPO                                   
039400       MOVE TPO1-KDTPOTYP  TO RYA-KDTPOTYP                                
039500       MOVE 2              TO RYA-KDVRTPO                                 
039600       MOVE TPO1-KDVRINFO  TO RYA-KDVRINFO                                
039700                                                                          
039800       MOVE RYA-WDGZRYA    TO ZZAC-LOGGPOST                               
039900       MOVE SPACE          TO ZZAC-SORTPOST                               
040000       PERFORM IMS-ISRT-ZZAC-WDG6                                         
040100       PERFORM UNTIL SEGMENT-FINNS                                        
040200         ADD +1 TO ZZAC-IDLOGLOP                                          
040300         PERFORM IMS-ISRT-ZZAC-WDG6                                       
040400       END-PERFORM                                                        
040500     ELSE                                                                 
040600       MOVE ZERO TO TPO1-KDORDBEK                                         
040700     END-IF                                                               
040800     .                                                                    
040900     EJECT                                                                
041000 S01A-SKAPA-RADKO SECTION.                                                
041100                                                                          
041200     MOVE TPO1-IDDISTR        TO RAD-IDDISTR                              
041300     MOVE TPO1-IDKUNDNR       TO RAD-IDKUNDNR                             
041400     MOVE SPACE               TO RAD-IDKUNDRF                             
041500     MOVE TPO1-IDKUNDRF (3:5) TO RAD-IDORDNR5                             
041600     MOVE TPO1-IDARTNR        TO RAD-IDARTNR                              
041700     MOVE 1                   TO RAD-IDLOPNR                              
041800     MOVE TPO1-BERADREF       TO RAD-BERADREF                             
041900     MOVE NEJ                 TO RAD-FLERS                                
042000     MOVE TPO1-IDANSK         TO RAD-IDANSK                               
042100     MOVE TPO1-IDANSK         TO RAD-IDANSK                               
042200     MOVE TPO1-IDKONTO        TO RAD-IDKONTO                              
042300     MOVE TPO1-IDKST          TO RAD-IDKST                                
042400     MOVE TPO1-IDANALYS       TO RAD-IDANALYS                             
042500     MOVE '00000     '        TO RAD-IDKUNDRF-LEV                         
042600     MOVE WC-CDC-SE           TO RAD-IDDC                                 
042700                                 RAD-IDDC-RO                              
042800     MOVE 'DT'                TO RAD-KDOI                                 
042900     MOVE SPACE               TO RAD-CLEARGROUP                           
043000     MOVE TPO1-KDDSP          TO RAD-KDDSP                                
043100     MOVE TPO1-KDFAKTYP       TO RAD-KDFAKTYP                             
043200     MOVE TPO1-KDFRAKT        TO RAD-KDFRAKT                              
043300     MOVE TPO1-KDKVBRYT       TO RAD-KDKVBRYT                             
043400     MOVE TPO1-KDORDING       TO RAD-KDORDING                             
043500     MOVE TPO1-KDORDKL        TO RAD-KDORDKL                              
043600     MOVE TPO1-KDPRODSL       TO RAD-KDPRODSL                             
043700     MOVE 20                  TO RAD-KDRAPRIO                             
043800     MOVE ZERO                TO RAD-KDROO                                
043900     MOVE 1                   TO RAD-KDSTARAD                             
044000     MOVE TPO1-KDTPOTYP       TO RAD-KDTPOTYP                             
044100     MOVE TPO1-KDVRINFO       TO RAD-KDVRINFO                             
044200     MOVE TPO1-KVBEART-Q      TO RAD-KVART                                
044300                                 RAD-KVBEART-Q                            
044400     MOVE ZERO                TO RAD-KVRO                                 
044500     MOVE TPO1-PRARTNTO       TO RAD-PRARTNTO                             
044600     MOVE TPO1-DEAL-PR-LINE   TO RAD-DEAL-PR-LINE                         
044700     MOVE TPO1-REKSIFFR       TO RAD-REKSIFFR                             
044800     MOVE ZERO                TO RAD-TIAVBOKN                             
044900     MOVE W-TIREGDAT          TO RAD-TIREGDAT                             
045000     MOVE ZERO                TO RAD-TIRES                                
045100     MOVE ZERO                TO RAD-DARODAT                              
045200     MOVE TPO1-TITPO          TO RAD-TITPO                                
045300     MOVE TPO1-KDPRTYP        TO RAD-KDPRTYP                              
045400     MOVE TPO1-BEVOLREF       TO RAD-BEVOLREF                             
045500     MOVE TPO1-FLINVEST       TO RAD-FLINVEST                             
045600     MOVE TPO1-FLPRTILL       TO RAD-FLPRTILL                             
045700     MOVE JA                  TO RAD-FLTPOBEK                             
045800     MOVE TPO1-BEKUNDRF       TO RAD-BEKUNDRF                             
045900     MOVE TPO1-IDKAMPRF       TO RAD-IDKAMPRF                             
046000     MOVE TPO1-IDLEVNR        TO RAD-IDLEVNR                              
046100     MOVE TPO1-IDSYSTEM       TO RAD-IDSYSTEM                             
046200     MOVE EXEKV-TID           TO RAD-TIREGTID                             
046300     MOVE ZERO                TO RAD-DASENDAT                             
046400                                 RAD-TISENBEK-KL                          
046410     MOVE SPACE               TO RAD-KDROPACK                             
046420     MOVE SPACE               TO RAD-IDARBREF                             
046500                                                                          
046600     MOVE TPO1-KDORDTYP-LDC   TO RAD-KDORDTYP-LDC                         
046700     MOVE TPO1-TIREPDAT       TO RAD-TIREPDAT                             
046800     MOVE TPO1-IDKUNDRF-WIP   TO RAD-IDKUNDRF-WIP                         
047000     MOVE +0                  TO RAD-PRAVCOST                             
047100     .                                                                    
047200                                                                          
047300 S01B-SKAPA-RADKO SECTION.                                                
047400                                                                          
047500     MOVE TPO1-BERADREF       TO RAD-BERADREF                             
047600     MOVE TPO1-IDKONTO        TO RAD-IDKONTO                              
047700     MOVE TPO1-IDKST          TO RAD-IDKST                                
047800     MOVE TPO1-IDANSK         TO RAD-IDANSK                               
047900     MOVE TPO1-KDDSP          TO RAD-KDDSP                                
048000     MOVE TPO1-KDFAKTYP       TO RAD-KDFAKTYP                             
048100     MOVE TPO1-KDFRAKT        TO RAD-KDFRAKT                              
048200     MOVE TPO1-KDKVBRYT       TO RAD-KDKVBRYT                             
048300     MOVE TPO1-KDORDING       TO RAD-KDORDING                             
048400     MOVE TPO1-KDORDKL        TO RAD-KDORDKL                              
048500     MOVE TPO1-KDPRODSL       TO RAD-KDPRODSL                             
048600     MOVE TPO1-KDTPOTYP       TO RAD-KDTPOTYP                             
048700     MOVE TPO1-KDVRINFO       TO RAD-KDVRINFO                             
048800     MOVE TPO1-KVBEART-Q      TO RAD-KVART                                
048900                                 RAD-KVBEART-Q                            
049000     MOVE TPO1-PRARTNTO       TO RAD-PRARTNTO                             
049100     MOVE TPO1-DEAL-PR-LINE   TO RAD-DEAL-PR-LINE                         
049200     MOVE TPO1-REKSIFFR       TO RAD-REKSIFFR                             
049300     MOVE TPO1-TITPO          TO RAD-TITPO                                
049400     MOVE TPO1-KDPRTYP        TO RAD-KDPRTYP                              
049500     MOVE TPO1-BEVOLREF       TO RAD-BEVOLREF                             
049600     MOVE TPO1-FLINVEST       TO RAD-FLINVEST                             
049700     MOVE TPO1-FLPRTILL       TO RAD-FLPRTILL                             
049800     MOVE TPO1-BEKUNDRF       TO RAD-BEKUNDRF                             
049900     MOVE TPO1-IDKAMPRF       TO RAD-IDKAMPRF                             
050000     MOVE TPO1-IDLEVNR        TO RAD-IDLEVNR                              
050100     MOVE TPO1-IDSYSTEM       TO RAD-IDSYSTEM                             
050200     .                                                                    
050300     EJECT                                                                
050400 S02-UPPDATERA-ARTREG SECTION.                                            
050500                                                                          
050600     MOVE TPO1-IDARTNR TO W-IDARTNR                                       
050700     PERFORM IMS-GHU-ARTM-WDK901                                          
050800     ADD TPO1-KVBEART-Q TO ART-SUTPO-TOT                                  
050900     PERFORM IMS-REPL-ARTM-WDK9                                           
051000     MOVE TITPO-TIAAVV  TO W-DABEHOV                                      
051100     IF TITPO-TIAAVV NOT = ZERO                                           
051200       IF TITPO-TIAAVV < 5000                                             
051300         MOVE 20        TO W-DABEHOV (1:2)                                
051400       ELSE                                                               
051500         IF TITPO-TIAAVV < 9999                                           
051600           MOVE 19      TO W-DABEHOV (1:2)                                
051700         ELSE                                                             
051800           MOVE 999999  TO W-DABEHOV                                      
051900         END-IF                                                           
052000       END-IF                                                             
052100     END-IF                                                               
052200     PERFORM IMS-GHNP-ARTM-WDK911                                         
052300     IF SEGMENT-FINNS                                                     
052400       ADD TPO1-KVBEART-Q TO ANT-SUTPO-PB                                 
052500       PERFORM S03-UPPDATERA-WDK911                                       
052600     ELSE                                                                 
052700       MOVE TITPO-TIAAVV   TO ANT-DABEHOV                                 
052800       IF TITPO-TIAAVV NOT = ZERO                                         
052900         IF TITPO-TIAAVV < 5000                                           
053000           MOVE 20        TO ANT-DABEHOV (1:2)                            
053100         ELSE                                                             
053200           IF TITPO-TIAAVV < 9999                                         
053300             MOVE 19      TO ANT-DABEHOV (1:2)                            
053400           ELSE                                                           
053500             MOVE 999999  TO ANT-DABEHOV                                  
053600           END-IF                                                         
053700         END-IF                                                           
053800       END-IF                                                             
053900       MOVE TPO1-KVBEART-Q TO ANT-SUTPO-PB                                
054000       MOVE ZERO           TO ANT-SUTPO-EJPB                              
054100       PERFORM IMS-ISRT-ARTM-WDK9                                         
054200     END-IF                                                               
054300     .                                                                    
054400     EJECT                                                                
054500 S03-UPPDATERA-WDK911                    SECTION.                         
054600                                                                          
054700     IF ANT-SUTPO-PB        = 0   AND                                     
054800        ANT-SUTPO-EJPB = 0                                                
054900       PERFORM IMS-DLET-ARTM-WDK9                                         
055000     ELSE                                                                 
055100       PERFORM IMS-REPL-ARTM-WDK9                                         
055200     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500* --- IMS SEKTIONER ---                                                   
055600     SKIP3                                                                
055700 IMS-GHU-ORDP-WDA501 SECTION.                                             
055800                                                                          
055900     STRING 'WLORDP01(WDA501KY>=' W-WDA501KY-MIN                          
056000                    '&WDA501KY<=' W-WDA501KY-MAX                          
056100                    '&KDSTARAD =' W-KDSTARAD-X                            
056200                    '&KDTPOTYP =' W-KDTPOTYP-X                            
056300                    '&TITPO    =' W-TITPO-X ')'                           
056400          DELIMITED BY SIZE INTO SSA1                                     
056500     MOVE '  GE' TO GODK-STATUSKODER                                      
056600     CALL CBLTDLI USING GHU ORDP-PCB DLI-IO-AREA SSA1                     
056700     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
056800     PERFORM IMS-STATUSKONTROLL                                           
056900     .                                                                    
057000     SKIP2                                                                
057100 IMS-ISRT-ORDP-WDA501 SECTION.                                            
057200                                                                          
057300     MOVE   'WLORDP01 ' TO SSA1                                           
057400     MOVE '  II' TO GODK-STATUSKODER                                      
057500     CALL CBLTDLI USING ISRT ORDP-PCB DLI-IO-AREA SSA1                    
057600     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
057700     PERFORM IMS-STATUSKONTROLL                                           
057800     .                                                                    
057900     EJECT                                                                
058000 IMS-DLET-ORDP-WDA501 SECTION.                                            
058100                                                                          
058200     MOVE '  ' TO GODK-STATUSKODER                                        
058300     CALL CBLTDLI USING DLET ORDP-PCB DLI-IO-AREA                         
058400     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
058500     PERFORM IMS-STATUSKONTROLL                                           
058600     .                                                                    
058700     SKIP2                                                                
058800 IMS-REPL-ORDP-WDA501 SECTION.                                            
058900                                                                          
059000     MOVE '  ' TO GODK-STATUSKODER                                        
059100     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA                         
059200     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
059300     PERFORM IMS-STATUSKONTROLL                                           
059400     .                                                                    
059500     EJECT                                                                
059600 IMS-GHU-ARTM-WDK901 SECTION.                                             
059700                                                                          
059800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
059900          DELIMITED BY SIZE INTO SSA1                                     
060000     MOVE '  ' TO GODK-STATUSKODER                                        
060100     CALL CBLTDLI USING GHU ARTM-PCB DLI-IO-AREA SSA1                     
060200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
060300     PERFORM IMS-STATUSKONTROLL                                           
060400     .                                                                    
060500     SKIP2                                                                
060600 IMS-GHNP-ARTM-WDK911 SECTION.                                            
060700                                                                          
060800     STRING 'WLARTM11(DABEHOV  =' W-DABEHOV-X ')'                         
060900          DELIMITED BY SIZE INTO SSA1                                     
061000     MOVE '  GE' TO GODK-STATUSKODER                                      
061100     CALL CBLTDLI USING GHNP ARTM-PCB DLI-IO-AREA SSA1                    
061200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
061300     PERFORM IMS-STATUSKONTROLL                                           
061400     .                                                                    
061500     SKIP2                                                                
061600 IMS-REPL-ARTM-WDK9 SECTION.                                              
061700                                                                          
061800     MOVE '  ' TO GODK-STATUSKODER                                        
061900     CALL CBLTDLI USING REPL ARTM-PCB DLI-IO-AREA                         
062000     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
062100     PERFORM IMS-STATUSKONTROLL                                           
062200     .                                                                    
062300     EJECT                                                                
062400 IMS-ISRT-ARTM-WDK9 SECTION.                                              
062500                                                                          
062600     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
062700          DELIMITED BY SIZE INTO SSA1                                     
062800     MOVE 'WLARTM11 ' TO SSA2                                             
062900     MOVE '  ' TO GODK-STATUSKODER                                        
063000     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-AREA SSA1 SSA2               
063100     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
063200     PERFORM IMS-STATUSKONTROLL                                           
063300     .                                                                    
063400     SKIP3                                                                
063500 IMS-DLET-ARTM-WDK9 SECTION.                                              
063600                                                                          
063700     MOVE '  ' TO GODK-STATUSKODER                                        
063800     CALL CBLTDLI USING DLET ARTM-PCB DLI-IO-AREA                         
063900     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
064000     PERFORM IMS-STATUSKONTROLL                                           
064100     .                                                                    
064200     EJECT                                                                
064300 IMS-ISRT-ZZAC-WDG6 SECTION.                                              
064400                                                                          
064500     MOVE   'WLZZAC01 ' TO SSA1                                           
064600     MOVE '  II' TO GODK-STATUSKODER                                      
064700     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
064800     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
064900     PERFORM IMS-STATUSKONTROLL                                           
065000     .                                                                    
065100     SKIP3                                                                
065200 IMS-STATUSKONTROLL SECTION.                                              
065300                                                                          
065400     SET STATUS-IX TO 1                                                   
065500     SEARCH GODK-STATUS                                                   
065600       AT END                                                             
065700       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
065800       DELIMITED BY SIZE INTO FELTEXT                                     
065900       CALL FELLOG                                                        
066000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
066100     END-SEARCH                                                           
066200     .                                                                    
066300     EJECT                                                                
066400*    -COPY WY2000P1                                                       
