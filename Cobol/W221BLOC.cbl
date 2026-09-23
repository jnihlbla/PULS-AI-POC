000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W221BLOC.                                                
000300 AUTHOR.         OLSSON SUSANNE.                                          
000400 DATE-WRITTEN.   14/03/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET FLYTTAR AVROP I EN LEVERANSPLAN SOM HAMNAT            
000900*        UNDER EN BLOCKERAD LEVERANSPERIOD FÖR AKTUELL                    
001000*        SHIP-LEVERANTÖR.SE TABELL PÅ BILD 2149.                          
001100*                                                                         
001200*        WDG3 / H-TYP 2257 BLOCKADE AVROPSVECKOR.                         
001300*                                                                         
001400*        SAMTLIGA AVROP UNDER BLOCKADE PERIODEN SKALL SUMMERAS            
001500*        OCH FÖRDELAS JÄMNT,UNDER DEN PERIOD SOM STRÄCKER SIG             
001600*        FRÅN ANGIVET DATUM I "RESCEDULE CALL-OFF",FRAM T.O.M.            
001700*        VECKAN FÖRE "CALL-OFF FROM".                                     
001800*                                                                         
001900*        FINNS DET INGEN VECKA ATT FLYTTA TILL,SÅ LÄGG BLOCKADE           
002000*        AVROPEN PÅ "VECKAN SOM ÄR BLOCKAD" OCH KDSVAR = N.               
002100*        LEVERANSPLANEN SKALL EJ KUNNA AUTOMATGODKÄNNAS + LARM.           
002200*                                                                         
002300*                                                                         
002400*        PROGRAMMET LÄSER      WDF3                                       
002500*                              WDR2 (WDGX2232) H-TYP 2231                 
002600*        PROGRAMMET UPPDATERAR WDD9                                       
002700*                              WDR5 (WDGX2224) H-TYP 2223                 
002800*                                                                         
002900*    ABENDKODER:                                                          
003000*        U0016 -  . . . .                                                 
003100*        U1000 -  . . . .                                                 
003200*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP2                                                                
003700 INPUT-OUTPUT SECTION.                                                    
003800                                                                          
003900 FILE-CONTROL.                                                            
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 FILE SECTION.                                                            
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600*    -COPY WY2000W3                                                       
004700     SKIP3                                                                
004800 77  IDPGM                       PIC X(8)    VALUE 'W221BLOC'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  CURRENT-SECTION             PIC X(30) VALUE SPACE.                   
005200 77  DBS-SECTION                 PIC X(30) VALUE SPACE.                   
005300                                                                          
005400 77  IX                          PIC S9(3)   VALUE +0   COMP-3.           
005500 77  MAX-IX                      PIC S9(3)   VALUE +60 COMP-3.            
005600                                                                          
005700*-- MAX ANTAL VECKOR ATT SMETA UT AVROPEN PÅ ÄR 30 VECKOR/5 DGR           
005800 77  TAB-IX                      PIC S9(3)   VALUE +0   COMP-3.           
005900 77  TAB-IX-MAX                  PIC S9(3)   VALUE +150 COMP-3.           
006000 77  TAB2-IX                     PIC S9(3)   VALUE +0   COMP-3.           
006100 77  TAB2-IX-MAX                 PIC S9(3)   VALUE +5   COMP-3.           
006200 77  IX-DAG                      PIC S9(2)   VALUE +0   COMP-3.           
006300                                                                          
006400 77  SPAR-DAAVROP-AVS-BLOC       PIC 9(6)    VALUE ZERO.                  
006500 77  SPAR-DAAVROP-FOM            PIC 9(6)    VALUE ZERO.                  
006600 77  SPAR-DAAVROP-TOM            PIC 9(6)    VALUE ZERO.                  
006700 77  SPAR-DAAVROP-TFOM           PIC 9(6)    VALUE ZERO.                  
006800 77  SPAR-TILEVDAG               PIC 9(1)    VALUE ZERO.                  
006900 77  SPAR-DAAVROP-AVS            PIC 9(6)    VALUE ZERO.                  
007000 77  WS-TILEVDAG                 PIC 9(1)    VALUE ZERO.                  
007100 77  WS-KVAVROP-TOT              PIC S9(7)   VALUE ZERO COMP-3.           
007200 77  WS-KVAVROP-SUM              PIC S9(7)   VALUE ZERO COMP-3.           
007300 77  WS-KVAVROP-TOT-VV           PIC S9(7)   VALUE ZERO COMP-3.           
007400 77  WS-KVPALL                   PIC S9(7)   VALUE ZERO COMP-3.           
007500 77  WS-KVULOAD                  PIC S9(7)   VALUE ZERO COMP-3.           
007600 77  WS-CALL-DATKONV             PIC 9(1)    VALUE ZERO.                  
007700                                                                          
007800 77  SW-SAMMA-VECKOR             PIC X       VALUE 'J'.                   
007900     88  SAMMA-VECKOR                        VALUE 'J'.                   
008000     88  EJ-SAMMA-VECKOR                     VALUE 'N'.                   
008100                                                                          
008200 77  SW-LAES-WDD905-AVROP        PIC X       VALUE 'N'.                   
008300     88  LAES-WDD905-AVROP                   VALUE 'J'.                   
008400                                                                          
008500     SKIP3                                                                
008600 01  WS-TISENBEK-KL-TEST         PIC 9(6)    VALUE ZERO.                  
008700 01  FILLER REDEFINES WS-TISENBEK-KL-TEST.                                
008800     03  WS-TISENBEK-KL-HH       PIC 9(2).                                
008900     03  WS-TISENBEK-KL-MM       PIC 9(2).                                
009000     03  WS-TISENBEK-KL-SS       PIC 9(2).                                
009100                                                                          
009200     EJECT                                                                
009300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009400 01  FILLER REDEFINES DAGENS-DATUM.                                       
009500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009800     EJECT                                                                
009900 01 WS-DAAVROP-AVS               PIC 9(6)    VALUE ZERO.                  
010000 01  FILLER REDEFINES WS-DAAVROP-AVS.                                     
010100     03  WS-DAAVROP-SS           PIC 9(2).                                
010200     03  WS-DAAVROP-AAVV         PIC 9(4).                                
010300     SKIP2                                                                
010400 01  WS-DAAVROP-MAX              PIC 9(6)    VALUE ZERO.                  
010500 01  FILLER REDEFINES WS-DAAVROP-MAX.                                     
010600     03  WS-DAAVROP-MAX-SS       PIC 9(2).                                
010700     03  WS-DAAVROP-MAX-AAVV     PIC 9(4).                                
010800     SKIP2                                                                
010900 01  ARB-TIAAMMDD-AVS          PIC 9(6)  VALUE ZERO.                      
011004 01  WS-DAYS-TIDATE1-AAVVD     PIC 9(5)  VALUE ZERO.                      
011102     EJECT                                                                
011202*----                                                                     
011302*---- TABELL MED GODKÄNDA VECKOR ATT FLYTTA AVROPEN TILL.                 
011402*----                                                                     
011502 01  AVROP-TABELL.                                                        
011602     03  FILLER OCCURS 150 TIMES.                                         
011702         05  TAB-DAAVROP-AVS      PIC 9(6).                               
011802         05  TAB-TIAAMMDD-AVS     PIC 9(6).                               
011902         05  TAB-TILEVDAG         PIC S9    COMP-3.                       
012002         05  TAB-KVAVROP          PIC S9(7) COMP-3.                       
012102         05  TAB-KVAVROP-VV       PIC S9(7) COMP-3.                       
012202                                                                          
012302     EJECT                                                                
012402*----                                                                     
012502*---- TABELL MED GODKÄNDA DAGAR I VECKAN,ATT FLYTTA AVROPEN TILL.         
012602*----                                                                     
012702 01  AVROP-TABELL-2.                                                      
012802     03  FILLER OCCURS 5 TIMES.                                           
012902         05  TAB2-DAAVROP-AVS     PIC 9(6).                               
013002         05  TAB2-TIAAMMDD-AVS    PIC 9(6).                               
013102         05  TAB2-TILEVDAG        PIC S9    COMP-3.                       
013202         05  TAB2-KVAVROP         PIC S9(7) COMP-3.                       
013302                                                                          
013402     EJECT                                                                
013502 01  DYNAMISKA-SUBPROGRAM.                                                
013602*                                                                         
013702     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013802     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013902     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014002     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014102     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
014202     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
014300     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
014400     SKIP2                                                                
014500*    --- PARAMETRAR TILL ABEND                                            
014600                                                                          
014700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
015000     SKIP2                                                                
015100 01  FELTEXT.                                                             
015200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
015300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
015400     EJECT                                                                
015500*01  -COPY WDATAREA                                                       
015600     EJECT                                                                
015700*    --- VARIABLER TILL W009VADD                                          
015800 01  WS-DATUM-AAVV-HELP          PIC S9(5)    COMP-3.                     
015900 01  WS-ANTAL-VECKOR             PIC S9(3)    COMP-3.                     
016000     EJECT                                                                
016100*01  -COPY WORKAREA                                                       
016200                                                                          
016400 01  FILLER                  PIC X(16)   VALUE 'WZ20DAYS   '.             
016500*   -COPY WZ20DAYS                                                        
016600     EJECT                                                                
016700                                                                          
016800     EJECT                                                                
016900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017000*                                                                         
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400 01  NYCKLAR-TILL-DLI.                                                    
017500     03  W-WDD901KY-X.                                                    
017600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017800                                                                          
017900     03  W-IDLEVNR-X.                                                     
018000         05  W-IDLEVNR-D902      PIC X(5)    VALUE SPACE.                 
018100                                                                          
018200     03  W-WDD905KY-MIN-X.                                                
018300         05  W-DAAVROP-AVS-MIN   PIC 9(6)    VALUE ZERO.                  
018400         05  W-TILEVDAG-MIN      PIC S9      VALUE ZERO COMP-3.           
018500                                                                          
018600     03  W-WDD905KY-MAX-X.                                                
018700         05  W-DAAVROP-AVS-MAX   PIC 9(6)    VALUE ZERO.                  
018800         05  W-TILEVDAG-MAX      PIC S9      VALUE 5    COMP-3.           
018900                                                                          
019000     03  W-WDD905KY-X.                                                    
019100         05  W-DAAVROP-AVS       PIC 9(6)    VALUE ZERO.                  
019200         05  W-TILEVDAG          PIC S9      VALUE ZERO COMP-3.           
019300                                                                          
019400     03  W-KDAVROP-X.                                                     
019500         05  W-KDAVROP           PIC S9(1)   VALUE ZERO COMP-3.           
019600                                                                          
019700     03  W-WDF301KY-X.                                                    
019800         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
019900         05  W-DADATUM-HELG      PIC 9(8)    VALUE ZERO.                  
020000         05  FILLER  REDEFINES W-DADATUM-HELG.                            
020100             07  W-DADATUM-HELG-SS      PIC 9(2).                         
020200             07  W-DADATUM-HELG-AAMMDD  PIC 9(6).                         
020300                                                                          
020400*--- LARM                                                                 
020500     03  W-WDGX2231-X.                                                    
020600         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
020700         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
020800                                                                          
020900     03  W-WDGX2232-X.                                                    
021000         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
021100         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
021200                                                                          
021300     03  W-WDGX2223-X.                                                    
021400         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
021500         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
021600         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
021700*---                                                                      
021800                                                                          
021900     SKIP2                                                                
022000*    --- STATUS-KOD FRÅN IMS                                              
022100 01  STATUS-WS                   PIC XX.                                  
022200     88  SEGMENT-FINNS                       VALUE '  '.                  
022300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022500     SKIP2                                                                
022600 01  GODK-STATUSKODER.                                                    
022700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022800     SKIP3                                                                
022900 01  SSA1                        PIC X(64).                               
023000 01  SSA2                        PIC X(96).                               
023100 01  SSA3                        PIC X(64).                               
023200     EJECT                                                                
023300*    --- IMS FUNKTIONSKODER                                               
023400*01  -COPY W0003                                                          
023500     EJECT                                                                
023600*    ---  DLI INPUT-OUTPUT AREA                                           
023700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
023800 01  DLI-IO-WDD905.                                                       
023900*    03  -COPY WDD905  -PRE D905-                                         
024000     EJECT                                                                
024100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF301'.                      
024200 01  DLI-IO-WDF301.                                                       
024300*    03  -COPY WDF301                                                     
024400     EJECT                                                                
024500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2232'.                    
024600 01  DLI-IO-WDGX2232.                                                     
024700*    03  -COPY WDGX2232                                                   
024800     EJECT                                                                
024900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2223'.                    
025000 01  DLI-IO-WDGX2223.                                                     
025100*    03  -COPY WDGX2223                                                   
025200     EJECT                                                                
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2224'.                    
025400 01  DLI-IO-WDGX2224.                                                     
025500*    03  -COPY WDGX2224                                                   
025600     EJECT                                                                
025700     EJECT                                                                
025800 LINKAGE SECTION.                                                         
025900                                                                          
026000*   -COPY W221BLOC                                                        
026100                                                                          
026200     EJECT                                                                
026300*01  -COPY W0008  -PRE WDD9-                                              
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600*01  -COPY W0008  -PRE WDF3-                                              
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008  -PRE WDR2-                                              
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200*01  -COPY W0008  -PRE WDR5-                                              
027300     05  FILLER                  PIC X.                                   
027400     EJECT                                                                
027500 PROCEDURE DIVISION  USING BLOC-W221BLOC WDD9-PCB WDF3-PCB                
027600                                         WDR2-PCB WDR5-PCB.               
027700 MAIN SECTION.                                                            
027800     ENTRY 'DLITCBL' USING BLOC-W221BLOC WDD9-PCB WDF3-PCB                
027900                                         WDR2-PCB WDR5-PCB.               
028000                                                                          
028100                                                                          
028200     PERFORM A-INIT                                                       
028300                                                                          
028400     MOVE +1  TO IX                                                       
028500     PERFORM UNTIL IX > MAX-IX                                            
028600       MOVE BLOC-DAAVROP-FOM(IX)  TO SPAR-DAAVROP-FOM                     
028700       MOVE BLOC-DAAVROP-TOM(IX)  TO SPAR-DAAVROP-TOM                     
028800                                                                          
028900       MOVE BLOC-DAAVROP-TFOM(IX) TO SPAR-DAAVROP-TFOM                    
029000       MOVE BLOC-DAAVROP-AVS(IX)  TO SPAR-DAAVROP-AVS-BLOC                
029100       MOVE BLOC-TILEVDAG(IX)     TO SPAR-TILEVDAG                        
029200       MOVE ZERO                  TO WS-KVAVROP-TOT                       
029300                                                                          
029400       MOVE JA TO SW-SAMMA-VECKOR                                         
029500       PERFORM UNTIL (IX > MAX-IX) OR EJ-SAMMA-VECKOR                     
029600                                                                          
029700         ADD BLOC-KVAVROP(IX)     TO WS-KVAVROP-TOT                       
029800                                                                          
029900         ADD +1  TO IX                                                    
030000         IF IX > MAX-IX                                                   
030100           CONTINUE                                                       
030200         ELSE                                                             
030300           IF BLOC-DAAVROP-FOM(IX) = SPAR-DAAVROP-FOM AND                 
030400              BLOC-DAAVROP-TOM(IX) = SPAR-DAAVROP-TOM                     
030500                                                                          
030600             CONTINUE                                                     
030700           ELSE                                                           
030800             MOVE NEJ TO SW-SAMMA-VECKOR                                  
030900           END-IF                                                         
031000         END-IF                                                           
031100       END-PERFORM                                                        
031200                                                                          
031300       PERFORM B-BEHANDLA-AVROP                                           
031400                                                                          
031500       IF BLOC-FLAGGA-DAGL-AVROP = NEJ                                    
031600         PERFORM C-FLYTTA-AVROP                                           
031700       ELSE                                                               
031800         PERFORM D-FLYTTA-AVROP-DAGL                                      
031900       END-IF                                                             
032000                                                                          
032100       IF IX > MAX-IX                                                     
032200         CONTINUE                                                         
032300       ELSE                                                               
032400         IF BLOC-DAAVROP-FOM(IX) = ZERO                                   
032500           MOVE MAX-IX  TO IX                                             
032600           ADD +1       TO IX                                             
032700         END-IF                                                           
032800       END-IF                                                             
032900     END-PERFORM                                                          
033000                                                                          
033100                                                                          
033200     MOVE ZERO TO RETURN-CODE                                             
033300     GOBACK                                                               
033400     .                                                                    
033500     EJECT                                                                
033600 A-INIT SECTION.                                                          
033700                                                                          
033800     ACCEPT DAGENS-DATUM  FROM DATE                                       
033900                                                                          
034000                                                                          
034100     MOVE BLOC-IDARTNR       TO W-IDARTNR                                 
034200     MOVE BLOC-IDDC          TO W-IDDC                                    
034300     MOVE BLOC-IDLEVNR       TO W-IDLEVNR-D902                            
034400     MOVE BLOC-IDLANDX2-SHIP TO W-IDLANDX2                                
034500     MOVE BLOC-KDAVROP       TO W-KDAVROP                                 
034600                                                                          
034700     MOVE ZERO               TO SPAR-DAAVROP-FOM                          
034800                                SPAR-DAAVROP-TOM                          
034900                                                                          
035000     .                                                                    
035100     EJECT                                                                
035200 B-BEHANDLA-AVROP   SECTION.                                              
035300     MOVE 'B-BEHANDLA-AVROP '   TO CURRENT-SECTION                        
035400                                                                          
035500     PERFORM S01-RENSA-TABELL                                             
035600     IF BLOC-FLAGGA-DAGL-AVROP = JA                                       
035700       PERFORM BA-BEHANDLA-DAGL-AVROP                                     
035800     ELSE                                                                 
035900       MOVE SPAR-DAAVROP-TFOM    TO WS-DAAVROP-AVS                        
036000       MOVE SPAR-DAAVROP-FOM     TO WS-DAAVROP-MAX                        
036100       MOVE SPAR-TILEVDAG        TO WS-TILEVDAG                           
036200                                                                          
036300       MOVE +1  TO TAB-IX                                                 
036400       MOVE 1   TO WS-CALL-DATKONV                                        
036500       PERFORM S02-CALL-DATKONV                                           
036600       IF ARB-TIAAMMDD-AVS > BLOC-TIAAMMDD-FT AND                         
036700          ARB-TIAAMMDD-AVS >= BLOC-TIAAMMDD-SPECST                        
036800                                                                          
036900         PERFORM BB-KOLLA-HELGDAG                                         
037000       END-IF                                                             
037100                                                                          
037200*---                                                                      
037300*--- KOLLA OM NÄSTA AVS-VECKA ÄR OK.                                      
037400*---                                                                      
037500       PERFORM UNTIL  TAB-IX > TAB-IX-MAX                                 
037600         MOVE WS-DAAVROP-AAVV      TO WS-DATUM-AAVV-HELP                  
037700         MOVE +1 TO WS-ANTAL-VECKOR                                       
037800         CALL W009VADD USING WS-DATUM-AAVV-HELP WS-ANTAL-VECKOR           
037900         MOVE WS-DATUM-AAVV-HELP    TO TMP1-YYWW                          
038000         MOVE WS-DAAVROP-MAX-AAVV   TO TMP2-YYWW                          
038100         PERFORM WY2000P3                                                 
038200         IF TMP1-YYWW < TMP2-YYWW                                         
038300           MOVE WS-DATUM-AAVV-HELP  TO WS-DAAVROP-AAVV                    
038400                                                                          
038500           MOVE 2   TO WS-CALL-DATKONV                                    
038600           PERFORM S02-CALL-DATKONV                                       
038700           IF ARB-TIAAMMDD-AVS > BLOC-TIAAMMDD-FT AND                     
038800              ARB-TIAAMMDD-AVS >= BLOC-TIAAMMDD-SPECST                    
038900                                                                          
039000             PERFORM BB-KOLLA-HELGDAG                                     
039100           END-IF                                                         
039200         ELSE                                                             
039300           MOVE TAB-IX-MAX  TO TAB-IX                                     
039400           ADD +1           TO TAB-IX                                     
039500         END-IF                                                           
039600       END-PERFORM                                                        
039700     END-IF                                                               
039800                                                                          
039900     IF TAB-DAAVROP-AVS(1) = ZERO                                         
040000       PERFORM BC-BEHANDLA-FLYTT-EJ-OK                                    
040100       MOVE NEJ  TO BLOC-KDSVAR                                           
040200       PERFORM BD-HAEMTA-IDANSK-LARM                                      
040300       PERFORM BE-SKAPA-LARM-PA-2171                                      
040400     END-IF                                                               
040500                                                                          
040600     .                                                                    
040700     EJECT                                                                
040800 BA-BEHANDLA-DAGL-AVROP  SECTION.                                         
040900     MOVE 'BA-BEHANDLA-DAGL-AVROP '  TO CURRENT-SECTION                   
041000                                                                          
041100     MOVE SPAR-DAAVROP-TFOM      TO WS-DAAVROP-AVS                        
041200     MOVE SPAR-DAAVROP-FOM       TO WS-DAAVROP-MAX                        
041300                                                                          
041400     MOVE +1  TO TAB-IX                                                   
041500     MOVE +1  TO IX-DAG                                                   
041600     PERFORM UNTIL IX-DAG > 5                                             
041700       IF BLOC-TILEVDAG-DAGL(IX-DAG) > ZERO                               
041800         MOVE BLOC-TILEVDAG-DAGL(IX-DAG) TO WS-TILEVDAG                   
041900         MOVE 3  TO WS-CALL-DATKONV                                       
042000         PERFORM S02-CALL-DATKONV                                         
042100         IF ARB-TIAAMMDD-AVS > BLOC-TIAAMMDD-FT AND                       
042200            ARB-TIAAMMDD-AVS >= BLOC-TIAAMMDD-SPECST                      
042300                                                                          
042400           PERFORM BB-KOLLA-HELGDAG                                       
042500         END-IF                                                           
042600       END-IF                                                             
042700       ADD +1               TO IX-DAG                                     
042800     END-PERFORM                                                          
042900                                                                          
043000*---                                                                      
043100*--- KOLLA OM NÄSTA AVS-VECKA ÄR OK.                                      
043200*---                                                                      
043300     PERFORM UNTIL  TAB-IX > TAB-IX-MAX                                   
043400       MOVE WS-DAAVROP-AAVV      TO WS-DATUM-AAVV-HELP                    
043500       MOVE +1 TO WS-ANTAL-VECKOR                                         
043600       CALL W009VADD USING WS-DATUM-AAVV-HELP WS-ANTAL-VECKOR             
043700       MOVE WS-DATUM-AAVV-HELP   TO TMP1-YYWW                             
043800       MOVE WS-DAAVROP-MAX-AAVV  TO TMP2-YYWW                             
043900       PERFORM WY2000P3                                                   
044000       IF TMP1-YYWW < TMP2-YYWW                                           
044100         MOVE WS-DATUM-AAVV-HELP  TO WS-DAAVROP-AAVV                      
044200                                                                          
044300         MOVE +1  TO IX-DAG                                               
044400         PERFORM UNTIL IX-DAG > 5                                         
044500           IF BLOC-TILEVDAG-DAGL(IX-DAG) > ZERO                           
044600             MOVE BLOC-TILEVDAG-DAGL(IX-DAG) TO WS-TILEVDAG               
044700             MOVE 4  TO WS-CALL-DATKONV                                   
044800             PERFORM S02-CALL-DATKONV                                     
044900             IF ARB-TIAAMMDD-AVS > BLOC-TIAAMMDD-FT AND                   
045000                ARB-TIAAMMDD-AVS >= BLOC-TIAAMMDD-SPECST                  
045100                                                                          
045200               PERFORM BB-KOLLA-HELGDAG                                   
045300             END-IF                                                       
045400           END-IF                                                         
045500           ADD +1               TO IX-DAG                                 
045600         END-PERFORM                                                      
045700       ELSE                                                               
045800         MOVE TAB-IX-MAX  TO TAB-IX                                       
045900         ADD +1           TO TAB-IX                                       
046000       END-IF                                                             
046100     END-PERFORM                                                          
046200     .                                                                    
046300     EJECT                                                                
046400 BB-KOLLA-HELGDAG   SECTION.                                              
046500     MOVE 'BB-KOLLA-HELGDAG '  TO CURRENT-SECTION                         
046600                                                                          
046700     MOVE 20                   TO W-DADATUM-HELG-SS                       
046800     MOVE ARB-TIAAMMDD-AVS     TO W-DADATUM-HELG-AAMMDD                   
046900                                                                          
047000     PERFORM IMS-GU-WDF301                                                
047100     IF SEGMENT-FINNS                                                     
047200       CONTINUE                                                           
047300     ELSE                                                                 
047400       MOVE ARB-TIAAMMDD-AVS   TO TAB-TIAAMMDD-AVS(TAB-IX)                
047500       MOVE WS-DAAVROP-AVS     TO TAB-DAAVROP-AVS (TAB-IX)                
047600       MOVE WS-TILEVDAG        TO TAB-TILEVDAG    (TAB-IX)                
047700                                                                          
047800       ADD +1   TO TAB-IX                                                 
047900     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200 BC-BEHANDLA-FLYTT-EJ-OK   SECTION.                                       
048300     MOVE 'BC-BEHANDLA-FLYTT-EJ-OK '  TO CURRENT-SECTION                  
048400*---------------------------------------------------------------          
048500*---- KOLLA OM DET INTE FANNS NÅGON GILTIG VECKA ATT FLYTTA               
048600*---- TILL.LÅT BLOCKADE AVROPSKVANTEN "LIGGA KVAR" PÅ DEN BLOCKADE        
048700*---- VECKAN I BLOCK-PERIODEN OCH LARMA ANSKAFFAREN PÅ BILD 2171.         
048800*---------------------------------------------------------------          
048900                                                                          
049000     MOVE SPAR-DAAVROP-AVS-BLOC  TO WS-DAAVROP-AVS                        
049100                                                                          
049200     IF BLOC-FLAGGA-DAGL-AVROP = JA                                       
049300                                                                          
049400       MOVE +1  TO TAB-IX                                                 
049500       MOVE +1  TO IX-DAG                                                 
049600       PERFORM UNTIL IX-DAG > 5                                           
049700         IF BLOC-TILEVDAG-DAGL(IX-DAG) > ZERO                             
049800           MOVE BLOC-TILEVDAG-DAGL(IX-DAG) TO WS-TILEVDAG                 
049900           MOVE 5  TO WS-CALL-DATKONV                                     
050000           PERFORM S02-CALL-DATKONV                                       
050100                                                                          
050200           MOVE ARB-TIAAMMDD-AVS   TO TAB-TIAAMMDD-AVS(TAB-IX)            
050300           MOVE WS-DAAVROP-AVS     TO TAB-DAAVROP-AVS (TAB-IX)            
050400           MOVE WS-TILEVDAG        TO TAB-TILEVDAG    (TAB-IX)            
050500           ADD +1  TO TAB-IX                                              
050600         END-IF                                                           
050700         ADD +1    TO IX-DAG                                              
050800       END-PERFORM                                                        
050900     ELSE                                                                 
051000       MOVE SPAR-TILEVDAG   TO WS-TILEVDAG                                
051100                                                                          
051200       MOVE 6  TO WS-CALL-DATKONV                                         
051300       PERFORM S02-CALL-DATKONV                                           
051400                                                                          
051500       MOVE +1   TO TAB-IX                                                
051600       MOVE ARB-TIAAMMDD-AVS   TO TAB-TIAAMMDD-AVS(TAB-IX)                
051700       MOVE WS-DAAVROP-AVS     TO TAB-DAAVROP-AVS (TAB-IX)                
051800       MOVE WS-TILEVDAG        TO TAB-TILEVDAG    (TAB-IX)                
051900     END-IF                                                               
052000                                                                          
052100     .                                                                    
052200     EJECT                                                                
052300 BD-HAEMTA-IDANSK-LARM  SECTION.                                          
052400     MOVE 'BD-HAEMTA-IDANSK-LARM '  TO CURRENT-SECTION                    
052500                                                                          
052600     MOVE BLOC-IDANSK              TO W-IDANSK-2232                       
052700     PERFORM IMS-GU-WDR220                                                
052800     IF SEGMENT-FINNS                                                     
052900        MOVE 2232-IDANSK-LARM      TO W-IDANSK-2223                       
053000     ELSE                                                                 
053100        MOVE ZERO                  TO W-IDANSK-2223                       
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 BE-SKAPA-LARM-PA-2171  SECTION.                                          
053600     MOVE 'BE-SKAPA-LARM-PA-2171 '  TO CURRENT-SECTION                    
053700                                                                          
053800     MOVE '2223'                  TO 2223-IDHTYP                          
053900     MOVE W-IDANSK-2223           TO 2223-IDANSK                          
054000     MOVE LOW-VALUE               TO 2223-LOW-VALUE                       
054100     PERFORM IMS-ISRT-WDR501-WDGX2223                                     
054200                                                                          
054300* INSERT WDR550/WDGX2224                                                  
054400     PERFORM IMS-GHU-WDR501                                               
054500                                                                          
054600     MOVE FUNCTION CURRENT-DATE(3:6)                                      
054700                                  TO 2224-TISENBEK-DAG                    
054800     MOVE FUNCTION CURRENT-DATE(9:6)                                      
054900                                  TO 2224-TISENBEK-KL                     
055000*-2014-04 PATRIK ÅTERKOMMER MED RÄTT LARMNUMMER.                          
055100     MOVE 999                     TO 2224-KDLARM                          
055200     MOVE BLOC-IDARTNR            TO 2224-IDARTNR                         
055300     MOVE BLOC-IDDC               TO 2224-IDDC                            
055400     MOVE JA                      TO 2224-FLNYLARM                        
055500     MOVE ZERO                    TO 2224-IDDISTR                         
055600                                     2224-IDKUNDNR                        
055700     MOVE '0000000   '            TO 2224-IDKUNDRF                        
055800     MOVE 1                       TO 2224-IDLOPNR                         
055900     MOVE DAGENS-DATUM            TO 2224-TIREGDAT                        
056000     MOVE 'BLOC'                  TO 2224-IDTRANS                         
056100     MOVE SPACE                   TO 2224-KDMFSFOR                        
056200     MOVE ZERO                    TO 2224-IDKR                            
056300     MOVE BLOC-IDLEVNR-SHIP       TO 2224-IDLEVNR                         
056400                                                                          
056500     PERFORM IMS-ISRT-WDR550-WDGX2224                                     
056600                                                                          
056700     IF SEGMENT-FINNS-REDAN                                               
056800       PERFORM UNTIL SEGMENT-FINNS                                        
056900         MOVE 2224-TISENBEK-KL      TO WS-TISENBEK-KL-TEST                
057000         PERFORM BEA-NAESTA-SEKUND                                        
057100         MOVE WS-TISENBEK-KL-TEST   TO 2224-TISENBEK-KL                   
057200         PERFORM IMS-ISRT-WDR550-WDGX2224                                 
057300       END-PERFORM                                                        
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 BEA-NAESTA-SEKUND  SECTION.                                              
057800     MOVE 'BEA-NAESTA-SEKUND '  TO CURRENT-SECTION                        
057900*--------------------------------------------------------------           
058000*    RÄKNAR UPP TILL NÄSTA SEKUND.                                        
058100*    GÅR ALDRIG ÖVER DYGNS-GRÄNS.                                         
058200*    NÄSTA SEKUND EFTER 23.59.59 GER 00.00.00 INOM SAMMA DYGN.            
058300*--------------------------------------------------------------           
058400                                                                          
058500     ADD 1                           TO WS-TISENBEK-KL-SS                 
058600     IF WS-TISENBEK-KL-SS > 59                                            
058700       ADD 1                         TO WS-TISENBEK-KL-MM                 
058800       MOVE ZERO                     TO WS-TISENBEK-KL-SS                 
058900                                                                          
059000       IF WS-TISENBEK-KL-MM > 59                                          
059100         ADD 1                       TO WS-TISENBEK-KL-HH                 
059200         MOVE ZERO                   TO WS-TISENBEK-KL-MM                 
059300                                                                          
059400         IF WS-TISENBEK-KL-HH > 23                                        
059500           MOVE ZERO                 TO WS-TISENBEK-KL-HH                 
059600         END-IF                                                           
059700       END-IF                                                             
059800     END-IF                                                               
059900                                                                          
060000     .                                                                    
060100     EJECT                                                                
060200 C-FLYTTA-AVROP   SECTION.                                                
060300     MOVE 'C-FLYTTA-AVROP '   TO CURRENT-SECTION                          
060400*----------------------------------------------------------------         
060500*---- I FÖRSTA HAND ÄR DET MULTIPLAR AV VÄRDET I FÄLTET KVULOAD           
060600*---- (ENH.LAST BILD 2131) SOM ANVÄNDS NÄR MAN FÖRDELAR BLOCKADE          
060700*---- AVROP UNDER MÖJLIGA SMETA-VECKOR. MEN FÖR EN SPECIFIK               
060800*---- AVROPSDAG SÅ MÅSTE SUMMAN ÖVERSTIGA FÄLTET KVPALL                   
060900*---- (MIN HEMT BILD 2131).                                               
061000*---- ENLIGT PATRIK L. 2014-02-12                                         
061100*---- FÖRDELNINGEN SKALL FÖRST GÖRAS VECKOVIS.OM EN MÖJLIG VECKA          
061200*---- REDAN HAR ETT AVROP SKALL KVULOAD (ENH.LAST BILD 2131) FÖR-         
061300*---- DELAS UNDER AKTUELL VECKA.                                          
061400*---- OM DET INTE FINNS AVROP REDAN I AKTUELL VECKA, SKALL EN             
061500*---- MULTIPEL AV ENHETSLASTEN ÖVER MINHEMTAGNINGSKVANTEN:KVPALL          
061600*---- (MIN HEMT BILD 2131)FÖRDELAS PÅ VECKAN.                             
061700*---- ENLIGT PATRIK L. 2014-06-26                                         
061800*----------------------------------------------------------------         
061900                                                                          
062000     MOVE JA              TO SW-LAES-WDD905-AVROP                         
062100                                                                          
062200     MOVE ZERO            TO WS-KVPALL                                    
062300     IF BLOC-KVPALL < +1                                                  
062400       MOVE +1            TO WS-KVPALL                                    
062500     ELSE                                                                 
062600       MOVE BLOC-KVPALL   TO WS-KVPALL                                    
062700     END-IF                                                               
062800                                                                          
062900     IF BLOC-KVQ > ZERO                                                   
063000       MOVE BLOC-KVQ      TO WS-KVPALL                                    
063100     END-IF                                                               
063200                                                                          
063300     MOVE ZERO            TO WS-KVULOAD                                   
063400     IF BLOC-KVULOAD > ZERO                                               
063500        MOVE BLOC-KVULOAD TO WS-KVULOAD                                   
063600     ELSE                                                                 
063700        MOVE BLOC-KVPALL  TO WS-KVULOAD                                   
063800     END-IF                                                               
063900     IF WS-KVULOAD > ZERO                                                 
064000       CONTINUE                                                           
064100     ELSE                                                                 
064200        MOVE WS-KVPALL    TO WS-KVULOAD                                   
064300     END-IF                                                               
064400                                                                          
064500*----                                                                     
064600                                                                          
064700     MOVE ZERO   TO WS-KVAVROP-SUM                                        
064800                                                                          
064900     MOVE +1  TO TAB-IX                                                   
065000     PERFORM UNTIL WS-KVAVROP-SUM NOT < WS-KVAVROP-TOT                    
065100                                                                          
065200       IF TAB-DAAVROP-AVS(TAB-IX) > ZERO                                  
065300         IF LAES-WDD905-AVROP                                             
065400           MOVE TAB-DAAVROP-AVS(TAB-IX)  TO W-DAAVROP-AVS-MIN             
065500                                            W-DAAVROP-AVS-MAX             
065600           MOVE ZERO                     TO W-TILEVDAG-MIN                
065700           MOVE +5                       TO W-TILEVDAG-MAX                
065800                                                                          
065900           PERFORM IMS-GU-WDD905-AVROP                                    
066000           IF SEGMENT-FINNS                                               
066100             ADD WS-KVULOAD       TO TAB-KVAVROP(TAB-IX)                  
066200             ADD WS-KVULOAD       TO WS-KVAVROP-SUM                       
066300           ELSE                                                           
066400             ADD WS-KVPALL        TO TAB-KVAVROP(TAB-IX)                  
066500             ADD WS-KVPALL        TO WS-KVAVROP-SUM                       
066600           END-IF                                                         
066700         ELSE                                                             
066800           ADD WS-KVULOAD       TO TAB-KVAVROP(TAB-IX)                    
066900           ADD WS-KVULOAD       TO WS-KVAVROP-SUM                         
067000         END-IF                                                           
067100       END-IF                                                             
067200       ADD +1  TO TAB-IX                                                  
067300                                                                          
067400       IF TAB-IX > TAB-IX-MAX                                             
067500       OR TAB-DAAVROP-AVS(TAB-IX) = ZERO                                  
067600         MOVE +1          TO TAB-IX                                       
067700                                                                          
067800         MOVE NEJ  TO SW-LAES-WDD905-AVROP                                
067900       END-IF                                                             
068000     END-PERFORM                                                          
068100                                                                          
068200     PERFORM CA-UPPDATERA-AVROP-WDD905                                    
068300                                                                          
068400     .                                                                    
068500     EJECT                                                                
068600 CA-UPPDATERA-AVROP-WDD905  SECTION.                                      
068700     MOVE 'CA-UPPDATERA-AVROP-WDD905 '   TO CURRENT-SECTION               
068800                                                                          
068900     MOVE +1  TO TAB-IX                                                   
069000     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
069100       IF TAB-DAAVROP-AVS(TAB-IX) = ZERO                                  
069200         MOVE TAB-IX-MAX  TO TAB-IX                                       
069300       ELSE                                                               
069400         IF TAB-KVAVROP(TAB-IX) > ZERO                                    
069500           MOVE TAB-DAAVROP-AVS(TAB-IX)   TO W-DAAVROP-AVS                
069600           MOVE TAB-TILEVDAG(TAB-IX)      TO W-TILEVDAG                   
069700                                                                          
069800           PERFORM IMS-GHU-WDD905                                         
069900                                                                          
070000           IF SEGMENT-FINNS                                               
070100             ADD TAB-KVAVROP(TAB-IX)      TO D905-KVAVROP                 
070200             PERFORM IMS-REPL-WDD905                                      
070300           ELSE                                                           
070400             MOVE BLOC-KDAVROP            TO D905-KDAVROP                 
070500             MOVE TAB-DAAVROP-AVS(TAB-IX) TO D905-DAAVROP-AVS             
070600             MOVE TAB-TILEVDAG(TAB-IX)    TO D905-TILEVDAG                
070700                                                                          
070800             PERFORM CAA-BERAKNA-INL-DISP-AAMMDD                          
070900                                                                          
071000             MOVE TAB-KVAVROP(TAB-IX)     TO D905-KVAVROP                 
071100                                                                          
071200             PERFORM IMS-ISRT-WDD905                                      
071300           END-IF                                                         
071400         END-IF                                                           
071500       END-IF                                                             
071600       ADD +1  TO TAB-IX                                                  
071700     END-PERFORM                                                          
071800     .                                                                    
071900     EJECT                                                                
072000 CAA-BERAKNA-INL-DISP-AAMMDD  SECTION.                                    
072100     MOVE 'CAA-BERAKNA-INL-DISP-AAMMDD '  TO CURRENT-SECTION              
072200                                                                          
072300*INL                                                                      
072400      MOVE 2                   TO WORK-KDCALL                             
072500      MOVE BLOC-IDDC           TO WORK-IDDC                               
072600      MOVE TAB-TIAAMMDD-AVS(TAB-IX)  TO WORK-TIAAMMDD-FOM                 
072700      MOVE BLOC-KVDAGAR-TT     TO WORK-KVWORKD                            
072800      ADD +1                   TO WORK-KVWORKD                            
072900      CALL WORKDAY USING  WORK-KDCALL                                     
073000           WORK-DATE-AREA WORK-KDSVAR                                     
073100      MOVE WORK-TIAAMMDD-TOM   TO D905-TIAVRDAT-INL                       
073200                                                                          
073300*DISP                                                                     
073400      MOVE 2                   TO WORK-KDCALL                             
073500      MOVE BLOC-IDDC           TO WORK-IDDC                               
073600      MOVE D905-TIAVRDAT-INL   TO WORK-TIAAMMDD-FOM                       
073700      MOVE BLOC-KVDAGAR-INLEV  TO WORK-KVWORKD                            
073800      ADD +1                   TO WORK-KVWORKD                            
073900      CALL WORKDAY USING  WORK-KDCALL                                     
074000           WORK-DATE-AREA WORK-KDSVAR                                     
074100      MOVE WORK-TIAAMMDD-TOM   TO D905-TIAVRDAT-DISP                      
074200                                                                          
074300     .                                                                    
074400     EJECT                                                                
074500 D-FLYTTA-AVROP-DAGL   SECTION.                                           
074600     MOVE 'D-FLYTTA-AVROP-DAGL '   TO CURRENT-SECTION                     
074700*----------------------------------------------------------------         
074800*---- FÖRDELNINGEN SKALL FÖRST GÖRAS VECKOVIS.OM EN MÖJLIG VECKA          
074900*---- REDAN HAR ETT AVROP SKALL KVULOAD (ENH.LAST BILD 2131) FÖR-         
075000*---- DELAS UNDER AKTUELL VECKA.                                          
075100*---- OM DET INTE FINNS AVROP REDAN I AKTUELL VECKA, SKALL EN             
075200*---- MULTIPEL AV ENHETSLASTEN ÖVER MINHEMTAGNINGSKVANTEN:KVPALL          
075300*---- (MIN HEMT BILD 2131)FÖRDELAS PÅ VECKAN.                             
075400*----                                                                     
075500*---- OM ARTIKELN HAR DAGSAVROP SKALL SUMMAN AV AVROP/VECKA               
075600*---- ENLIGT OVAN,FÖRDELAS OM ENLIGT NORMALA REGLER PER VECKA.            
075700*----                                                                     
075800*---- ENLIGT PATRIK L. 2014-06-26                                         
075900*----------------------------------------------------------------         
076000                                                                          
076100     MOVE JA              TO SW-LAES-WDD905-AVROP                         
076200                                                                          
076300     MOVE ZERO            TO WS-KVPALL                                    
076400     IF BLOC-KVPALL < +1                                                  
076500       MOVE +1            TO WS-KVPALL                                    
076600     ELSE                                                                 
076700       MOVE BLOC-KVPALL   TO WS-KVPALL                                    
076800     END-IF                                                               
076900                                                                          
077000     IF BLOC-KVQ > ZERO                                                   
077100       MOVE BLOC-KVQ      TO WS-KVPALL                                    
077200     END-IF                                                               
077300                                                                          
077400     MOVE ZERO            TO WS-KVULOAD                                   
077500     IF BLOC-KVULOAD > ZERO                                               
077600        MOVE BLOC-KVULOAD TO WS-KVULOAD                                   
077700     ELSE                                                                 
077800        MOVE BLOC-KVPALL  TO WS-KVULOAD                                   
077900     END-IF                                                               
078000     IF WS-KVULOAD > ZERO                                                 
078100       CONTINUE                                                           
078200     ELSE                                                                 
078300        MOVE WS-KVPALL    TO WS-KVULOAD                                   
078400     END-IF                                                               
078500                                                                          
078600*----                                                                     
078700*----KVAVROP/VECKA                                                        
078800*----                                                                     
078900                                                                          
079000     MOVE ZERO   TO WS-KVAVROP-SUM                                        
079100     MOVE ZERO   TO SPAR-DAAVROP-AVS                                      
079200                                                                          
079300     MOVE +1  TO TAB-IX                                                   
079400     PERFORM UNTIL WS-KVAVROP-SUM NOT < WS-KVAVROP-TOT                    
079500                                                                          
079600       IF TAB-DAAVROP-AVS(TAB-IX) = SPAR-DAAVROP-AVS                      
079700         CONTINUE                                                         
079800       ELSE                                                               
079900         MOVE TAB-DAAVROP-AVS(TAB-IX)  TO SPAR-DAAVROP-AVS                
080000                                                                          
080100         IF TAB-DAAVROP-AVS(TAB-IX) > ZERO                                
080200           IF LAES-WDD905-AVROP                                           
080300             MOVE TAB-DAAVROP-AVS(TAB-IX)  TO W-DAAVROP-AVS-MIN           
080400                                              W-DAAVROP-AVS-MAX           
080500             MOVE ZERO                     TO W-TILEVDAG-MIN              
080600             MOVE +5                       TO W-TILEVDAG-MAX              
080700                                                                          
080800             PERFORM IMS-GU-WDD905-AVROP                                  
080900             IF SEGMENT-FINNS                                             
081000               ADD WS-KVULOAD     TO TAB-KVAVROP-VV(TAB-IX)               
081100               ADD WS-KVULOAD     TO WS-KVAVROP-SUM                       
081200             ELSE                                                         
081300               ADD WS-KVPALL      TO TAB-KVAVROP-VV(TAB-IX)               
081400               ADD WS-KVPALL      TO WS-KVAVROP-SUM                       
081500             END-IF                                                       
081600           ELSE                                                           
081700             ADD WS-KVULOAD       TO TAB-KVAVROP-VV(TAB-IX)               
081800             ADD WS-KVULOAD       TO WS-KVAVROP-SUM                       
081900           END-IF                                                         
082000         END-IF                                                           
082100       END-IF                                                             
082200       ADD +1  TO TAB-IX                                                  
082300                                                                          
082400       IF TAB-IX > TAB-IX-MAX                                             
082500       OR TAB-DAAVROP-AVS(TAB-IX) = ZERO                                  
082600         MOVE +1          TO TAB-IX                                       
082700         MOVE ZERO        TO SPAR-DAAVROP-AVS                             
082800                                                                          
082900         MOVE NEJ  TO SW-LAES-WDD905-AVROP                                
083000       END-IF                                                             
083100     END-PERFORM                                                          
083200                                                                          
083300*----                                                                     
083400*----KVAVROP/DAGLIGA                                                      
083500*----                                                                     
083600     PERFORM DA-FOERDELA-AVROP-DAG                                        
083700                                                                          
083800     .                                                                    
083900     EJECT                                                                
084000 DA-FOERDELA-AVROP-DAG    SECTION.                                        
084100     MOVE 'DA-FOERDELA-AVROP-DAG '  TO CURRENT-SECTION                    
084200                                                                          
084300     MOVE ZERO   TO WS-KVAVROP-TOT-VV                                     
084400                    SPAR-DAAVROP-AVS                                      
084500                                                                          
084600     MOVE +1  TO TAB-IX                                                   
084700     PERFORM UNTIL  TAB-IX > TAB-IX-MAX                                   
084800       PERFORM S03-RENSA-AVROPS-TAB2                                      
084900       MOVE TAB-DAAVROP-AVS(TAB-IX) TO SPAR-DAAVROP-AVS                   
085000                                       TAB2-DAAVROP-AVS(1)                
085100                                       TAB2-DAAVROP-AVS(2)                
085200                                       TAB2-DAAVROP-AVS(3)                
085300                                       TAB2-DAAVROP-AVS(4)                
085400                                       TAB2-DAAVROP-AVS(5)                
085500                                                                          
085600       MOVE TAB-KVAVROP-VV(TAB-IX)  TO WS-KVAVROP-TOT-VV                  
085700                                                                          
085800                                                                          
085900       MOVE JA TO SW-SAMMA-VECKOR                                         
086000       PERFORM UNTIL (TAB-IX > TAB-IX-MAX) OR EJ-SAMMA-VECKOR             
086100                                                                          
086200         IF TAB-TILEVDAG(TAB-IX) > ZERO                                   
086300           MOVE TAB-TILEVDAG(TAB-IX)  TO IX-DAG                           
086400                                         TAB2-TILEVDAG(IX-DAG)            
086500           MOVE TAB-TIAAMMDD-AVS(TAB-IX) TO                               
086600                                     TAB2-TIAAMMDD-AVS(IX-DAG)            
086700         END-IF                                                           
086800                                                                          
086900         ADD +1  TO TAB-IX                                                
087000         IF TAB-DAAVROP-AVS(TAB-IX) = SPAR-DAAVROP-AVS                    
087100           CONTINUE                                                       
087200         ELSE                                                             
087300           MOVE NEJ TO SW-SAMMA-VECKOR                                    
087400         END-IF                                                           
087500       END-PERFORM                                                        
087600                                                                          
087700       PERFORM DAA-SKAPA-DAGL-AVROP                                       
087800       PERFORM DAB-UPPDATERA-AVROP-WDD905                                 
087900                                                                          
088000       IF TAB-DAAVROP-AVS(TAB-IX) = ZERO                                  
088100         MOVE TAB-IX-MAX  TO TAB-IX                                       
088200         ADD +1           TO TAB-IX                                       
088300       END-IF                                                             
088400     END-PERFORM                                                          
088500     .                                                                    
088600     EJECT                                                                
088700 DAA-SKAPA-DAGL-AVROP    SECTION.                                         
088800     MOVE 'DAA-SKAPA-DAGL-AVROP  '  TO CURRENT-SECTION                    
088900                                                                          
089000     MOVE ZERO   TO WS-KVAVROP-SUM                                        
089100     MOVE JA     TO SW-LAES-WDD905-AVROP                                  
089200                                                                          
089300     IF WS-KVAVROP-TOT-VV  > ZERO                                         
089400       PERFORM UNTIL WS-KVAVROP-SUM NOT < WS-KVAVROP-TOT-VV               
089500                                                                          
089600          MOVE +1  TO IX-DAG                                              
089700          PERFORM UNTIL IX-DAG > 5                                        
089800             IF TAB2-TILEVDAG(IX-DAG) > ZERO                              
089900               IF WS-KVAVROP-SUM < WS-KVAVROP-TOT-VV                      
090000                 IF LAES-WDD905-AVROP                                     
090100                   MOVE TAB2-DAAVROP-AVS(IX-DAG) TO                       
090200                                            W-DAAVROP-AVS-MIN             
090300                                            W-DAAVROP-AVS-MAX             
090400                   MOVE TAB2-TILEVDAG(IX-DAG) TO                          
090500                                            W-TILEVDAG-MIN                
090600                                            W-TILEVDAG-MAX                
090700                                                                          
090800                   PERFORM IMS-GU-WDD905-AVROP                            
090900                   IF SEGMENT-FINNS                                       
091000                     ADD WS-KVULOAD   TO TAB2-KVAVROP(IX-DAG)             
091100                     ADD WS-KVULOAD   TO WS-KVAVROP-SUM                   
091200                   ELSE                                                   
091300                     ADD WS-KVPALL    TO TAB2-KVAVROP(IX-DAG)             
091400                     ADD WS-KVPALL    TO WS-KVAVROP-SUM                   
091500                   END-IF                                                 
091600                 ELSE                                                     
091700                   ADD WS-KVULOAD     TO TAB2-KVAVROP(IX-DAG)             
091800                   ADD WS-KVULOAD     TO WS-KVAVROP-SUM                   
091900                 END-IF                                                   
092000               END-IF                                                     
092100             END-IF                                                       
092200             ADD +1               TO IX-DAG                               
092300             IF IX-DAG = 6                                                
092400               MOVE NEJ  TO SW-LAES-WDD905-AVROP                          
092500             END-IF                                                       
092600          END-PERFORM                                                     
092700       END-PERFORM                                                        
092800     END-IF                                                               
092900     .                                                                    
093000     EJECT                                                                
093100 DAB-UPPDATERA-AVROP-WDD905  SECTION.                                     
093200     MOVE 'DAB-UPPDATERA-AVROP-WDD905'   TO CURRENT-SECTION               
093300                                                                          
093400     MOVE +1  TO TAB2-IX                                                  
093500     PERFORM UNTIL TAB2-IX > TAB2-IX-MAX                                  
093600       IF TAB2-DAAVROP-AVS(TAB2-IX) = ZERO                                
093700         MOVE TAB2-IX-MAX  TO TAB2-IX                                     
093800       ELSE                                                               
093900         IF TAB2-KVAVROP(TAB2-IX) > ZERO                                  
094000           MOVE TAB2-DAAVROP-AVS(TAB2-IX) TO W-DAAVROP-AVS                
094100           MOVE TAB2-TILEVDAG(TAB2-IX)    TO W-TILEVDAG                   
094200                                                                          
094300           PERFORM IMS-GHU-WDD905                                         
094400                                                                          
094500           IF SEGMENT-FINNS                                               
094600             ADD TAB2-KVAVROP(TAB2-IX)    TO D905-KVAVROP                 
094700             PERFORM IMS-REPL-WDD905                                      
094800           ELSE                                                           
094900             MOVE BLOC-KDAVROP            TO D905-KDAVROP                 
095000             MOVE TAB2-DAAVROP-AVS(TAB2-IX) TO D905-DAAVROP-AVS           
095100             MOVE TAB2-TILEVDAG(TAB2-IX)  TO D905-TILEVDAG                
095200                                                                          
095300             PERFORM DABA-BERAKNA-INL-DISP-AAMMDD                         
095400                                                                          
095500             MOVE TAB2-KVAVROP(TAB2-IX)   TO D905-KVAVROP                 
095600                                                                          
095700             PERFORM IMS-ISRT-WDD905                                      
095800           END-IF                                                         
095900         END-IF                                                           
096000       END-IF                                                             
096100       ADD +1  TO TAB2-IX                                                 
096200     END-PERFORM                                                          
096300     .                                                                    
096400     EJECT                                                                
096500 DABA-BERAKNA-INL-DISP-AAMMDD  SECTION.                                   
096600     MOVE 'DABA-BERAKNA-INL-DISP-AAMMDD '  TO CURRENT-SECTION             
096700                                                                          
096800*INL                                                                      
096900      MOVE 2                   TO WORK-KDCALL                             
097000      MOVE BLOC-IDDC           TO WORK-IDDC                               
097100      MOVE TAB2-TIAAMMDD-AVS(TAB2-IX) TO WORK-TIAAMMDD-FOM                
097200      MOVE BLOC-KVDAGAR-TT     TO WORK-KVWORKD                            
097300      ADD +1                   TO WORK-KVWORKD                            
097400      CALL WORKDAY USING  WORK-KDCALL                                     
097500           WORK-DATE-AREA WORK-KDSVAR                                     
097600      MOVE WORK-TIAAMMDD-TOM   TO D905-TIAVRDAT-INL                       
097700                                                                          
097800*DISP                                                                     
097900      MOVE 2                   TO WORK-KDCALL                             
098000      MOVE BLOC-IDDC           TO WORK-IDDC                               
098100      MOVE D905-TIAVRDAT-INL   TO WORK-TIAAMMDD-FOM                       
098200      MOVE BLOC-KVDAGAR-INLEV  TO WORK-KVWORKD                            
098300      ADD +1                   TO WORK-KVWORKD                            
098400      CALL WORKDAY USING  WORK-KDCALL                                     
098500           WORK-DATE-AREA WORK-KDSVAR                                     
098600      MOVE WORK-TIAAMMDD-TOM   TO D905-TIAVRDAT-DISP                      
098700                                                                          
098800     .                                                                    
098900     EJECT                                                                
099000 S01-RENSA-TABELL  SECTION.                                               
099100     MOVE 'S01-RENSA-TABELL  '  TO CURRENT-SECTION                        
099200                                                                          
099300     MOVE +1  TO TAB-IX                                                   
099400     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
099500       MOVE +0   TO TAB-DAAVROP-AVS(TAB-IX)                               
099600                    TAB-TIAAMMDD-AVS(TAB-IX)                              
099700                    TAB-TILEVDAG(TAB-IX)                                  
099800                    TAB-KVAVROP(TAB-IX)                                   
099900                    TAB-KVAVROP-VV(TAB-IX)                                
100000                                                                          
100100       ADD +1    TO TAB-IX                                                
100200     END-PERFORM                                                          
100300                                                                          
100400     .                                                                    
100500     EJECT                                                                
100600 S02-CALL-DATKONV   SECTION.                                              
100700     MOVE 'S02-CALL-DATKONV '  TO CURRENT-SECTION                         
100800                                                                          
100902     COMPUTE WS-DAYS-TIDATE1-AAVVD = 10 * WS-DAAVROP-AAVV +               
101100                                     WS-TILEVDAG                          
101200     MOVE WS-DAYS-TIDATE1-AAVVD  TO DAYS-TIDATE1                          
101300     MOVE 'YYWWD'             TO DAYS-KDDATFMT1                           
101400     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
101500     MOVE 0                   TO DAYS-KVDAYS                              
101600     MOVE SPACE               TO DAYS-TIDATE2                             
101700                                 DAYS-IDCALEND                            
101800     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
101860                                                                          
101900*                                                                         
102000     IF DAYS-KDRC = 8                                                     
102400       MOVE 'FEL FRÅN WZ20DAYS = ' TO FELTEXT-STR                         
102500       MOVE WS-CALL-DATKONV        TO FELTEXT-STR(20:1)                   
102600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
102700     ELSE                                                                 
102800       MOVE DAYS-TIDATE2(1:6) TO ARB-TIAAMMDD-AVS                         
102900     END-IF                                                               
104500     .                                                                    
104600     EJECT                                                                
104700 S03-RENSA-AVROPS-TAB2  SECTION.                                          
104800     MOVE 'S03-RENSA-AVROPS-TAB2 '  TO CURRENT-SECTION                    
104900                                                                          
105000     MOVE +1  TO TAB2-IX                                                  
105100     PERFORM UNTIL TAB2-IX > TAB2-IX-MAX                                  
105200       MOVE +0   TO TAB2-DAAVROP-AVS(TAB2-IX)                             
105300                    TAB2-TIAAMMDD-AVS(TAB2-IX)                            
105400                    TAB2-TILEVDAG(TAB2-IX)                                
105500                    TAB2-KVAVROP(TAB2-IX)                                 
105600                                                                          
105700       ADD +1    TO TAB2-IX                                               
105800     END-PERFORM                                                          
105900                                                                          
106000     .                                                                    
106100     EJECT                                                                
106200* --- IMS SEKTIONER ---                                                   
106300                                                                          
106400     EJECT                                                                
106500 IMS-GU-WDD905-AVROP SECTION.                                             
106600     MOVE 'IMS-GU-WDD905-AVROP ' TO DBS-SECTION                           
106700                                                                          
106800     MOVE SPACES              TO SSA1 SSA2 SSA3                           
106900     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
107000          DELIMITED BY SIZE INTO SSA1                                     
107100     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
107200          DELIMITED BY SIZE INTO SSA2                                     
107300     STRING 'WDD905  (WDD905KY>=' W-WDD905KY-MIN-X                        
107400                    '&WDD905KY<=' W-WDD905KY-MAX-X                        
107500                    '&KDAVROP  =' W-KDAVROP-X ')'                         
107600          DELIMITED BY SIZE INTO SSA3                                     
107700     MOVE '  GE'              TO GODK-STATUSKODER                         
107800     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3         
107900     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     .                                                                    
108200     EJECT                                                                
108300 IMS-GHU-WDD905 SECTION.                                                  
108400     MOVE 'IMS-GHU-WDD905  ' TO DBS-SECTION                               
108500                                                                          
108600     MOVE SPACES              TO SSA1 SSA2 SSA3                           
108700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
108800          DELIMITED BY SIZE INTO SSA1                                     
108900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
109000          DELIMITED BY SIZE INTO SSA2                                     
109100     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
109200                    '&KDAVROP  =' W-KDAVROP-X ')'                         
109300          DELIMITED BY SIZE INTO SSA3                                     
109400     MOVE '  GE'              TO GODK-STATUSKODER                         
109500     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3         
109600     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
109700     PERFORM IMS-STATUSKONTROLL                                           
109800     .                                                                    
109900     EJECT                                                                
110000 IMS-REPL-WDD905 SECTION.                                                 
110100     MOVE 'IMS-REPL-WDD905 ' TO DBS-SECTION                               
110200                                                                          
110300     MOVE '  '                TO GODK-STATUSKODER                         
110400     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD905                       
110500     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
110600     PERFORM IMS-STATUSKONTROLL                                           
110700     .                                                                    
110800     EJECT                                                                
110900 IMS-ISRT-WDD905 SECTION.                                                 
111000     MOVE 'IMS-ISRT-WDD905 ' TO DBS-SECTION                               
111100                                                                          
111200     MOVE SPACES              TO SSA1 SSA2 SSA3                           
111300     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
111400          DELIMITED BY SIZE INTO SSA1                                     
111500     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
111600          DELIMITED BY SIZE INTO SSA2                                     
111700     MOVE 'WDD905 '           TO SSA3                                     
111800     MOVE '  '                TO GODK-STATUSKODER                         
111900     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3        
112000     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
112100     PERFORM IMS-STATUSKONTROLL                                           
112200     .                                                                    
112300     EJECT                                                                
112400 IMS-GU-WDF301 SECTION.                                                   
112500     MOVE 'IMS-GU-WDF301 ' TO DBS-SECTION                                 
112600                                                                          
112700     MOVE SPACE               TO SSA1                                     
112800     STRING 'WDF301  (WDF301KY =' W-WDF301KY-X ')'                        
112900          DELIMITED BY SIZE INTO SSA1                                     
113000     MOVE '  GE' TO GODK-STATUSKODER                                      
113100     CALL CBLTDLI USING GU WDF3-PCB DLI-IO-WDF301 SSA1                    
113200     MOVE WDF3-STATUS-CODE TO STATUS-WS                                   
113300     PERFORM IMS-STATUSKONTROLL                                           
113400     .                                                                    
113500     EJECT                                                                
113600 IMS-GU-WDR220 SECTION.                                                   
113700     MOVE 'IMS-GU-WDR220 '  TO DBS-SECTION                                
113800                                                                          
113900     MOVE SPACE               TO SSA1 SSA2                                
114000     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
114100          DELIMITED BY SIZE INTO SSA1                                     
114200     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
114300          DELIMITED BY SIZE INTO SSA2                                     
114400     MOVE '  GE' TO GODK-STATUSKODER                                      
114500     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2232 SSA1 SSA2             
114600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
114700     PERFORM IMS-STATUSKONTROLL                                           
114800     .                                                                    
114900     SKIP2                                                                
115000 IMS-GHU-WDR501 SECTION.                                                  
115100     MOVE 'IMS-GHU-WDR501 '   TO DBS-SECTION                              
115200                                                                          
115300     MOVE SPACE               TO SSA1                                     
115400     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
115500          DELIMITED BY SIZE INTO SSA1                                     
115600     MOVE '  GE' TO GODK-STATUSKODER                                      
115700     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX2223 SSA1                 
115800     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
115900     PERFORM IMS-STATUSKONTROLL                                           
116000     .                                                                    
116100     SKIP2                                                                
116200 IMS-ISRT-WDR501-WDGX2223 SECTION.                                        
116300     MOVE 'IMS-ISRT-WDR501-WDGX2223 '  TO DBS-SECTION                     
116400                                                                          
116500     MOVE SPACE               TO SSA1                                     
116600     MOVE  'WDR501  '         TO SSA1                                     
116700     MOVE '  II' TO GODK-STATUSKODER                                      
116800     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2223 SSA1                
116900     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
117000     PERFORM IMS-STATUSKONTROLL                                           
117100     .                                                                    
117200     SKIP2                                                                
117300 IMS-ISRT-WDR550-WDGX2224 SECTION.                                        
117400     MOVE 'IMS-ISRT-WDR550-WDGX2224 '   TO DBS-SECTION                    
117500                                                                          
117600     MOVE SPACE               TO SSA1                                     
117700     MOVE  'WDR550  '         TO SSA1                                     
117800     MOVE '  II' TO GODK-STATUSKODER                                      
117900     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2224 SSA1                
118000     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
118100     PERFORM IMS-STATUSKONTROLL                                           
118200     .                                                                    
118300     EJECT                                                                
118400 IMS-STATUSKONTROLL SECTION.                                              
118500                                                                          
118600     SET STATUS-IX TO 1                                                   
118700     SEARCH GODK-STATUS                                                   
118800       AT END                                                             
118900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
119000           DELIMITED BY SIZE INTO FELTEXT                                 
119100         DISPLAY FELTEXT                                                  
119200         CALL FELLOG                                                      
119300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
119400         CONTINUE                                                         
119500     END-SEARCH                                                           
119600     .                                                                    
119700     EJECT                                                                
120000     -COPY WY2000P3                                                       
