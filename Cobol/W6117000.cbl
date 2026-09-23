000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6117000.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   94/08/24.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER WDL2 MED SB.                                               
001000*        LÄSER WDK6 MED DLI.                                              
001100*                                                                         
001200*                                                                         
001300*        BERÄKNAR SAMMANLAGD INLEVERANSVOLYM OCH SAMMANLAGT               
001400*        INLEVERANSVÄRDE PER ARTIKEL TILL HOLLÄNDSKA TULLEN               
001500*        SAMT SVENSKA MYNDIGHETER                                         
001600*                                                                         
001700*    ÄNDRINGAR:                                                           
001800*        05-08-23  E-TRACKER: 2076532                                     
001900*                  SPÄRR BORTAGEN FÖR NEGATIVA INLEVERANSER//L.A.         
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- LISTPOSTER INLEVERANSVÄRDE OCH INLEVERANSVOLYM             
003400*          PER ARTIKELNUMMER                                              
003500     SELECT W61168                     ASSIGN TO W61170D1.                
003600*          --- POSTER TILL INTRASTATEN = SVENSK TULL                      
003700     SELECT W61169                     ASSIGN TO W61170D2.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W61168                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W61168 -PRE  UT-  -L.                                     
004800     SKIP3                                                                
004900 FD  W61169                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W61169 -PRE  C1-  -L.                                     
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700*    -COPY WY2000W1                                                       
005800     SKIP3                                                                
005900 77  IDPGM                       PIC X(8)    VALUE 'W6117000'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  SW-FIRST-ARTIKEL           PIC X       VALUE 'J'.                    
006300     88  FIRST-ARTIKEL                      VALUE 'J'.                    
006400                                                                          
006500 77  SW-R32-2-FINNS             PIC X       VALUE 'N'.                    
006600     88 R32-2-FINNS                         VALUE 'J'.                    
006700     EJECT                                                                
006800 01  DAGENS-AAAAMMDD             PIC 9(8)    VALUE ZERO.                  
006810 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006900 01  FILLER REDEFINES DAGENS-DATUM.                                       
007000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007300     EJECT                                                                
007400 01  ARBETSAREOR.                                                         
007500     03  WS-SPAR-IDARTNR         PIC S9(9)   VALUE ZERO COMP-3.           
007600     03  WS-SPAR-IDLEVNR         PIC  X(5)   VALUE SPACE.                 
007700     03  WS-SUM-KVANTMOT         PIC S9(7)   VALUE ZERO COMP-3.           
007800     03  WS-SUM-KVANTMOT-CDC     PIC S9(7)   VALUE ZERO COMP-3.           
007900     03  WS-PRARTBES          PIC S9(7)V9(2) VALUE ZERO COMP-3.           
007910     03  FL-PRARTBES             PIC X      VALUE 'N'.                    
008000     03  WS-VKART                PIC S9(7)   VALUE ZERO COMP-3.           
008100     03  WS-IDSTATNR             PIC S9(9)   VALUE ZERO COMP-3.           
008200     03  WS-KDARTURS             PIC X(2).                                
008300     03  WS-KDSORT               PIC X(2).                                
008400     03  WS-SKOTTAR-TEST         PIC 9(2)V9(2).                           
008500     03  FILLER REDEFINES WS-SKOTTAR-TEST.                                
008600         05  WS-SKOTTAR-TEST-HEL PIC 9(2).                                
008700         05  WS-SKOTTAR-TEST-DEC PIC 9(2).                                
008800                                                                          
008900 01  WS-MAN-START                PIC 9(6)    VALUE ZERO.                  
009000 01  FILLER REDEFINES WS-MAN-START.                                       
009100     03  WS-MAN-START-AAR        PIC 9(2).                                
009200     03  WS-MAN-START-MAANAD     PIC 9(2).                                
009300     03  WS-MAN-START-DAG        PIC 9(2).                                
009400     EJECT                                                                
009500 01  WS-MAN-SLUT                 PIC 9(6)    VALUE ZERO.                  
009600 01  FILLER REDEFINES WS-MAN-SLUT.                                        
009700     03  WS-MAN-SLUT-AAR         PIC 9(2).                                
009800     03  WS-MAN-SLUT-MAANAD      PIC 9(2).                                
009900     03  WS-MAN-SLUT-DAG         PIC 9(2).                                
010000     EJECT                                                                
010100                                                                          
010200 01  DYNAMISKA-SUBPROGRAM.                                                
010300*                                                                         
010400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011000     SKIP2                                                                
011100*    --- PARAMETRAR TILL ABEND                                            
011200                                                                          
011300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011500     SKIP2                                                                
011600 01  FELTEXT.                                                             
011700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011900     EJECT                                                                
012000*      --- VALID IDDC CODES                                               
012100*                                                                         
012200*01    -COPY WWDC99                                                       
012300     EJECT                                                                
012400 01  LANDKODER.                                                           
012500     03  OSTERRIKE               PIC X(2)    VALUE 'AT'.                  
012600     03  BELGIEN                 PIC X(2)    VALUE 'BE'.                  
012700     03  TYSKLAND                PIC X(2)    VALUE 'DE'.                  
012800     03  DANMARK                 PIC X(2)    VALUE 'DK'.                  
012900     03  SPANIEN                 PIC X(2)    VALUE 'ES'.                  
013000     03  FINLAND                 PIC X(2)    VALUE 'FI'.                  
013100     03  FRANKRIKE               PIC X(2)    VALUE 'FR'.                  
013200     03  ENGLAND                 PIC X(2)    VALUE 'GB'.                  
013300     03  IRLAND                  PIC X(2)    VALUE 'IE'.                  
013400     03  ITALIEN                 PIC X(2)    VALUE 'IT'.                  
013500     03  LUXEMBURG               PIC X(2)    VALUE 'LU'.                  
013600     03  HOLLAND                 PIC X(2)    VALUE 'NL'.                  
013700     03  PORTUGAL                PIC X(2)    VALUE 'PT'.                  
013800     EJECT                                                                
013900*    --- PARAMETRAR TILL DATKORT                                          
014000*                                                                         
014100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61170'.              
014200     SKIP2                                                                
014300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
014400     SKIP2                                                                
014500*01  -COPY WDATKORT                                                       
014600     EJECT                                                                
014700*    --- PARAMETRAR TILL POSTSUM                                          
014800*                                                                         
014900*01  -COPY W0005   -PRE  POSTSUM-                                         
015000     EJECT                                                                
015100 01  UT-AREA-START               PIC X(24)   VALUE                        
015200                                 'UT-AREA-START  '.                       
015300     SKIP2                                                                
015400                                                                          
015500*01  AREA -COPY W61168     -PRE UT-                                       
015600     EJECT                                                                
015700 01  FILLER                      PIC X(24)   VALUE                        
015800                                 'UT-CDC AREA START'.                     
015900     SKIP2                                                                
016000                                                                          
016100*01  AREA -COPY W61169     -PRE CDC-                                      
016200     EJECT                                                                
016300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016400*                                                                         
016500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016600     SKIP3                                                                
016700 01  NYCKLAR-TILL-DLI.                                                    
016800     03  W-IDARTNR-X.                                                     
016900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017000     03  W-KDERS-0-X.                                                     
017100         05  W-KDERS-0           PIC S9(3)   VALUE ZERO COMP-3.           
017200     03  W-DAPRLIST-X.                                                    
017300         05  W-DAPRLIST          PIC 9(8)    VALUE ZERO.                  
017310     03  W-DAINLEV-X.                                                     
017320         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
017400     03  W-IDPTYP-X.                                                      
017500         05  W-IDPTYP            PIC X(11)    VALUE SPACE.                
017600     03  W-KDSEGKEY-X.                                                    
017700         05  W-KDSEGKEY          PIC X(1)     VALUE SPACE.                
017800     SKIP2                                                                
017900*    --- STATUS-KOD FRÅN IMS                                              
018000 01  STATUS-WS                   PIC XX.                                  
018100     88  SEGMENT-FINNS                       VALUE '  '.                  
018200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018400     SKIP2                                                                
018500 01  GODK-STATUSKODER.                                                    
018600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018700     SKIP3                                                                
018800 01  SSA1                        PIC X(64).                               
018900 01  SSA2                        PIC X(64).                               
019000     EJECT                                                                
019100*    --- IMS FUNKTIONSKODER                                               
019200*01  -COPY W0003                                                          
019300     EJECT                                                                
019400*    ---  DLI INPUT-OUTPUT AREA                                           
019500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019600     SKIP3                                                                
019700 01  DLI-IO-AREA.                                                         
019800     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
019900     SKIP3                                                                
020000     03  WDL201 REDEFINES IO-AREA.                                        
020100*        05  -COPY WDL201  -PRE WDL201-                                   
020200     SKIP3                                                                
020300     03  WDL221 REDEFINES IO-AREA.                                        
020400*        05  -COPY WDL221  -PRE WDL221-                                   
020500     EJECT                                                                
020600 01  FILLER                  PIC X(16) VALUE  'DLI-IO-K601'.              
020800 01  DLI-IO-K601.                                                         
021200*    03  -COPY WDK601                                                     
021600     EJECT                                                                
021610 01  FILLER                  PIC X(16) VALUE  'DLI-IO-K611'.              
021620 01  DLI-IO-K611.                                                         
021630*    03  -COPY WDK611                                                     
021640     EJECT                                                                
021650 01  FILLER                  PIC X(16) VALUE  'DLI-IO-K621'.              
021660 01  DLI-IO-K621.                                                         
021670*    03  -COPY WDK621                                                     
021680     EJECT                                                                
021700 LINKAGE SECTION.                                                         
021800                                                                          
021900*01  -COPY W0008  -PRE WDL2-                                              
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008  -PRE ARTC-                                              
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500 PROCEDURE DIVISION  USING WDL2-PCB ARTC-PCB.                             
022600     ENTRY 'DLITCBL' USING WDL2-PCB ARTC-PCB.                             
022700                                                                          
022800     PERFORM A-INIT                                                       
022900     PERFORM IMS-GET-WDL2                                                 
023000     PERFORM UNTIL SEGMENT-SLUT                                           
023100        EVALUATE WDL2-SEG-NAME-FB                                         
023200           WHEN 'WDL201  '                                                
023300              IF FIRST-ARTIKEL                                            
023400                 MOVE NEJ       TO SW-FIRST-ARTIKEL                       
023500              ELSE                                                        
023600                 IF R32-2-FINNS                                           
023700                     PERFORM B-SKAPA-C2-POST                              
023800                 END-IF                                                   
023900                 PERFORM C-NOLLSTALL                                      
024000              END-IF                                                      
024100              MOVE WDL201-ART-IDARTNR TO WS-SPAR-IDARTNR                  
024200           WHEN 'WDL221  '                                                
024300                                                                          
024400             MOVE WDL221-MOT-TIUPPDAT   TO TMP1-YYMMDD                    
024500             MOVE WS-MAN-SLUT           TO TMP2-YYMMDD                    
024600             MOVE WS-MAN-START          TO TMP3-YYMMDD                    
024700             PERFORM WY2000Q1                                             
024800             IF (WDL221-MOT-IDPTYP   = 'R32')             AND             
024900                (TMP1-YYMMDD <= TMP2-YYMMDD)    AND                       
025000                (TMP1-YYMMDD >= TMP3-YYMMDD)                              
025100                 MOVE WDL221-MOT-IDDC TO WS-IDDC                          
025200                 IF CDC-TR     AND                                        
025300                    WDL221-MOT-KDRT = +00                                 
025400                     MOVE JA       TO SW-R32-2-FINNS                      
025500                     MOVE WDL221-MOT-IDLEVNR TO WS-SPAR-IDLEVNR           
025600                     COMPUTE WS-SUM-KVANTMOT =                            
025700                           WS-SUM-KVANTMOT + WDL221-MOT-KVANTMOT          
025800                 ELSE                                                     
025900                     IF CDC-SE                                            
026000                         IF (WDL221-MOT-KDRT = +00) OR                    
026100                            (WDL221-MOT-KDRT = +08)                       
026200                             PERFORM D-SKAPA-CDC-POST                     
026300                         END-IF                                           
026400                     END-IF                                               
026500                 END-IF                                                   
026600             END-IF                                                       
026700             IF (WDL221-MOT-IDPTYP   = 'R33')             AND             
026800                (WDL221-MOT-IDDC NOT NUMERIC)                             
026900                         IF (WDL221-MOT-KDRT = +00) OR                    
027000                            (WDL221-MOT-KDRT = +08)                       
027100                             PERFORM D-SKAPA-CDC-POST                     
027200                         END-IF                                           
027300             END-IF                                                       
027400         END-EVALUATE                                                     
027500         PERFORM IMS-GET-WDL2                                             
027600     END-PERFORM                                                          
027700     IF R32-2-FINNS                                                       
027800         PERFORM B-SKAPA-C2-POST                                          
027900     END-IF                                                               
028000     PERFORM Z-FINIT                                                      
028100                                                                          
028200     MOVE ZERO TO RETURN-CODE                                             
028300     GOBACK                                                               
028400     .                                                                    
028500     EJECT                                                                
028600                                                                          
028700                                                                          
028800 A-INIT SECTION.                                                          
028900                                                                          
029000     OPEN OUTPUT W61168 W61169                                            
029100                                                                          
029200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
029300     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
029400     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
029500     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
029600                                                                          
029700     MOVE DAGENS-DATUM   TO WS-MAN-START                                  
029800                            WS-MAN-SLUT                                   
029900                                                                          
030000     MOVE 01       TO WS-MAN-START-DAG                                    
030100                                                                          
030200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
030300     .                                                                    
030400     EJECT                                                                
030500                                                                          
030600                                                                          
030700 B-SKAPA-C2-POST SECTION.                                                 
030800                                                                          
030900     IF WS-SPAR-IDARTNR NOT = W-IDARTNR                                   
031000         PERFORM S01-SAMLA-DATA                                           
031100     END-IF                                                               
031200     IF WS-SUM-KVANTMOT > +0                                              
031300                                                                          
031400         MOVE WS-SPAR-IDARTNR     TO UT-IDARTNR                           
031500         MOVE WS-SPAR-IDLEVNR     TO UT-IDLEVNR                           
031600         MOVE WS-MAN-SLUT         TO UT-TIAAMMDD-GAELL                    
031700         MOVE WS-IDSTATNR         TO UT-IDSTATNR                          
031800                                                                          
031900         MOVE WS-KDARTURS         TO UT-KDARTURS                          
032000                                                                          
032100         MOVE WS-SUM-KVANTMOT     TO UT-KVANTMOT                          
032200                                                                          
032300         COMPUTE UT-VKARTTOT =                                            
032400             WS-SUM-KVANTMOT * WS-VKART                                   
032500                                                                          
032600         COMPUTE UT-SUARTBES =                                            
032700             WS-SUM-KVANTMOT * WS-PRARTBES                                
032800                                                                          
032900           PERFORM S11-SKRIV-W61168                                       
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300                                                                          
033400                                                                          
033500 C-NOLLSTALL SECTION.                                                     
033600                                                                          
033700     MOVE NEJ                    TO SW-R32-2-FINNS                        
033800     MOVE ZERO                   TO WS-SPAR-IDARTNR                       
033900                                    WS-SUM-KVANTMOT                       
034000                                    WS-PRARTBES                           
034100                                    WS-IDSTATNR                           
034200                                    WS-SUM-KVANTMOT-CDC                   
034300                                    WS-VKART                              
034400                                    WS-IDSTATNR                           
034500     MOVE SPACE                  TO WS-KDARTURS                           
034600                                    WS-SPAR-IDLEVNR                       
034700     .                                                                    
034800     EJECT                                                                
034900                                                                          
035000                                                                          
035100 D-SKAPA-CDC-POST SECTION.                                                
035200                                                                          
035300     IF WS-SPAR-IDARTNR NOT = W-IDARTNR                                   
035400         PERFORM S01-SAMLA-DATA                                           
035500     END-IF                                                               
035600     MOVE WDL221-MOT-IDLEVNR TO CDC-IDLEVNR                               
035700     MOVE WDL221-MOT-KDRT    TO CDC-KDRT                                  
035800     MOVE WS-SPAR-IDARTNR    TO CDC-IDARTNR                               
035900     MOVE WS-KDSORT          TO CDC-KDSORT                                
036000     MOVE WDL221-MOT-KVANTMOT TO CDC-KVANTMOT                             
036100     COMPUTE  CDC-SUARTBES =                                              
036200          WS-PRARTBES * WDL221-MOT-KVANTMOT                               
036300     MOVE WS-MAN-SLUT        TO CDC-TIAAMMDD-GAELL                        
036400                                                                          
036500     PERFORM S12-SKRIV-W61169                                             
036600     .                                                                    
036700     EJECT                                                                
036800                                                                          
036900 Z-FINIT SECTION.                                                         
037000     CLOSE W61168  W61169                                                 
037100     SKIP2                                                                
037200     MOVE 'S' TO POSTSUM-OPKOD                                            
037300     CALL POSTSUM USING POSTSUM-PARM                                      
037400     .                                                                    
037500     EJECT                                                                
037600                                                                          
037700 S01-SAMLA-DATA SECTION.                                                  
037800                                                                          
037900     MOVE WS-SPAR-IDARTNR TO W-IDARTNR                                    
038000     PERFORM IMS-GET-WDK601                                               
038100     IF SEGMENT-FINNS                                                     
038200        MOVE ART-KDSORT TO WS-KDSORT                                      
038300        PERFORM IMS-GET-WDK611                                            
038400        IF SEGMENT-FINNS                                                  
038600           MOVE CLAG-VKART              TO WS-VKART                       
038700           MOVE CLAG-IDSTATNR(3)        TO WS-IDSTATNR                    
038800           MOVE CLAG-KDARTURS           TO WS-KDARTURS                    
038801* PRARTBES HÄMTAS FRÅN K621 EFTER CN-PROJEKTET                            
038802           MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD             
038803           COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                
038804           PERFORM IMS-GNP-WDK621                                         
038805           IF SEGMENT-SAKNAS                                              
038806             MOVE CLAG-PRARTSTD       TO WS-PRARTBES                      
038807           ELSE                                                           
038808             MOVE NEJ                 TO FL-PRARTBES                      
038809             PERFORM UNTIL  SEGMENT-SAKNAS                                
038810               IF PRL-SUINLEV-PR > ZERO                                   
038811                 MOVE PRL-PRARTBES-PR  TO WS-PRARTBES                     
038812                 SET SEGMENT-SAKNAS TO TRUE                               
038813               ELSE                                                       
038814                 IF FL-PRARTBES = NEJ                                     
038815                   MOVE PRL-PRARTBES-PR TO WS-PRARTBES                    
038816                   MOVE JA              TO FL-PRARTBES                    
038817                 END-IF                                                   
038818                 PERFORM IMS-GNP-WDK621                                   
038819               END-IF                                                     
038820             END-PERFORM                                                  
038821           END-IF                                                         
038900        END-IF                                                            
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400 S11-SKRIV-W61168 SECTION.                                                
039500                                                                          
039600     WRITE UT-POST FROM UT-AREA                                           
039700                                                                          
039800     MOVE 'W61168' TO POSTSUM-FDNAMN                                      
039900     MOVE 'W61170D1' TO POSTSUM-DDNAMN2                                   
040000     CALL POSTSUM USING POSTSUM-PARM                                      
040100     .                                                                    
040200     EJECT                                                                
040300 S12-SKRIV-W61169 SECTION.                                                
040400                                                                          
040500     WRITE C1-POST FROM CDC-AREA                                          
040600                                                                          
040700     MOVE 'W61169' TO POSTSUM-FDNAMN                                      
040800     MOVE 'W61170D2' TO POSTSUM-DDNAMN2                                   
040900     CALL POSTSUM USING POSTSUM-PARM                                      
041000     .                                                                    
041100     EJECT                                                                
041200* --- IMS SEKTIONER ---                                                   
041300     SKIP3                                                                
041400 IMS-GET-WDL2   SECTION.                                                  
041500                                                                          
041600     CALL CBLTDLI USING GN WDL2-PCB DLI-IO-AREA                           
041700     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
041800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
041900     PERFORM IMS-STATUSKONTROLL                                           
042000     .                                                                    
042100     EJECT                                                                
042200                                                                          
042300                                                                          
042400 IMS-GET-WDK601 SECTION.                                                  
042500                                                                          
042600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X '&KDERS    ='               
042700             W-KDERS-0-X ')'                                              
042800          DELIMITED BY SIZE INTO SSA1                                     
042900     MOVE '  GE' TO GODK-STATUSKODER                                      
043000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-K601 SSA1                      
043100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
043200     PERFORM IMS-STATUSKONTROLL                                           
043300     .                                                                    
043400     EJECT                                                                
043500                                                                          
043600                                                                          
043700 IMS-GET-WDK611 SECTION.                                                  
043800                                                                          
043900     MOVE 'WLARTC11 ' TO SSA1                                             
044000     MOVE '  GE' TO GODK-STATUSKODER                                      
044100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-K611 SSA1                     
044200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
044300     PERFORM IMS-STATUSKONTROLL                                           
044400     .                                                                    
044500 IMS-GNP-WDK621 SECTION.                                                  
044510                                                                          
044511     STRING 'WLARTC21(DAPRLIST>=' W-DAPRLIST-X ')'                        
044513          DELIMITED BY SIZE INTO SSA1                                     
044530     MOVE '  GE' TO GODK-STATUSKODER                                      
044540     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-K621 SSA1                     
044550     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
044560     PERFORM IMS-STATUSKONTROLL                                           
044570     .                                                                    
044600     EJECT                                                                
044700                                                                          
044800 IMS-STATUSKONTROLL SECTION.                                              
044900                                                                          
045000     SET STATUS-IX TO 1                                                   
045100     SEARCH GODK-STATUS                                                   
045200       AT END                                                             
045300         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
045400         DISPLAY FELTEXT                                                  
045500         CALL FELLOG                                                      
045600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
045700         CONTINUE                                                         
045800     END-SEARCH                                                           
045900     .                                                                    
046000     EJECT                                                                
046100*    -COPY WY2000Q1                                                       
