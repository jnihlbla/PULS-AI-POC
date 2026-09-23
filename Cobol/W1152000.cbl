000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W1152000                                     
000300 AUTHOR.                     JANNE MELANDER.                              
000400 DATE-WRITTEN.               SEPT 1988.                                   
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700                                                                          
000800*    FUNKTION.                                                            
000900*    LÄSER WDD201 MED SB.                                                 
001000*    LÄSER WDD6, XXAP, MED DLI.                                           
001100*                                                                         
001200*                                                                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700     SELECT  UTFIL      ASSIGN      W11520D1.                             
001800*            *** OUTPUT-FILE ***                                          
001900*                                                                         
002000     SELECT  FELFIL     ASSIGN      W11520D2.                             
002100*            *** OUTPUT-FILE ***                                          
002200*                                                                         
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  UTFIL                                                                
002800     LABEL RECORDS STANDARD                                               
002900     RECORDING      F                                                     
003000     BLOCK CONTAINS 0.                                                    
003100                                                                          
003200*01  UTPOST  -COPY W11522       -L.                                       
003300                                                                          
003400     SKIP3                                                                
003500 FD  FELFIL                                                               
003600     LABEL RECORDS STANDARD                                               
003700     RECORDING      F                                                     
003800     BLOCK CONTAINS 0.                                                    
003900                                                                          
004000 01  FELPOST.                                                             
004100     03  FEL-ARTIKELNR       PIC  9(9)  VALUE ZERO.                       
004200     03  FILLER              PIC  X(1)  VALUE SPACE.                      
004300     03  FEL-TEXT            PIC  X(70) VALUE SPACE.                      
004400                                                                          
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700     SKIP2                                                                
004800                                                                          
004900*    -- CHECKED BY WY2000                                                 
005000 77  PROGRAM-NAMN            PIC X(8) VALUE 'W1152000'.                   
005100*- - - - - - - - - - - - - - - - - KONSTANTER.                            
005200 77  JA                      PIC X       VALUE 'J'.                       
005300 77  NEJ                     PIC X       VALUE 'N'.                       
005400 77  SW-KORTFIL-EOF          PIC X       VALUE 'N'.                       
005500 77  SW-WDD211               PIC X       VALUE 'J'.                       
005600 77  INDX                    PIC S9(9)   VALUE +0 COMP SYNC.              
005700 77  WS-KVBASLM              PIC S9(7)   VALUE ZERO.                      
005800 77  WS-TIGENORD             PIC S9(7)   VALUE ZERO.                      
005900 77  WS-TIBEHOV              PIC S9(6)   VALUE ZERO.                      
006000 77  WS-FL-DIST-KVANT        PIC X       VALUE 'N'.                       
006100 77  DISPLAY-KDPRODSL        PIC  9(3)   VALUE ZERO.                      
006200                                                                          
006300*- - - - - - - - - - - - - - - - - DATUM-FAELT                            
006400 01  DAGENS-DATUM             PIC 9(6) VALUE ZERO.                        
006500*                                                                         
006600 01  W-AAVV.                                                              
006700    03  W-AA                 PIC 9(2)     VALUE ZERO .                    
006800    03  W-VV                 PIC 9(2)     VALUE ZERO .                    
006900*                                                                         
007000 01  WS-AAVV                 PIC  9(4)    VALUE ZERO.                     
007100*- - - - - - - - - - - - - - - - - DYNAMISKA-SUB-PGM                      
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
007400     03  FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
007500     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
007600     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
007700*01  -COPY WDATAREA                                                       
007800*                                                                         
007900     EJECT                                                                
008000*- - - - - - - - - - - - - - - - - ARBETSAREOR  IMS-SEKTIONEN.            
008100*01  AREA              -COPY W11522     -PRE UT-                          
008200*                                                                         
008300*01  POST              -COPY W11522     -PRE SPAR-                        
008400*                                                                         
008500                                                                          
008600*01            -COPY WWPRODSL                                             
008700                                                                          
008800     EJECT                                                                
008900*01            -COPY W0005      -PRE POSTSUM-                             
009000                                                                          
009100     EJECT                                                                
009200 01    IMS-WS.                                                            
009300   03  FILLER           PIC X(8)   VALUE 'IMS-WS  '.                      
009400*                                                                         
009500*- - - - - - - - - - - - - - - - - NYCKLAR TILL DLI                       
009600   03 W-IDARTNR-X.                                                        
009700      05 W-IDARTNR         PIC S9(9)  COMP-3 VALUE ZERO.                  
009800                                                                          
009900   03 W-1123-KEY-X.                                                       
010000      05 FILLER            PIC  X(4)            VALUE '1123'.             
010100      05 W-1123-KDPRODSL   PIC S9(3)   COMP-3   VALUE ZERO.               
010200      05 W-1123-IDPROJ     PIC X(4)             VALUE SPACE.              
010300      05 FILLER            PIC X(20)            VALUE LOW-VALUE.          
010400                                                                          
010500   03 W-1126-KEY-X.                                                       
010600      05 W-1126-KDBASLM    PIC X(6)             VALUE SPACE.              
010700      05 FILLER            PIC X(09)            VALUE LOW-VALUE.          
010800                                                                          
010900*- - - - - - - - - - - - - - - - - STATUSKOD FRÅN IMS                     
011000   03    STATUS-WS      PIC XX.                                           
011100     88  SEGMENT-FINNS             VALUE '  '.                            
011200     88  SEGMENT-SAKNAS            VALUE 'GE'.                            
011300     88  BASEN-SLUT                VALUE 'GB'.                            
011400*                                                                         
011500*                                                                         
011600   03    SSA1           PIC X(64).                                        
011700   03    SSA2           PIC X(64).                                        
011800*                                                                         
011900   03    GODK-STATUSKODER.                                                
012000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     EJECT                                                                
012200*- - - - - - - - - - - - - - - - - IMS-CALL FUNKTIONER                    
012300*01      -COPY W0003.                                                     
012400     EJECT                                                                
012500*- - - - - - - - - - - - - - - - - IMS - COMM-AREA                        
012600 01  DLI-IO-AREA.                                                         
012700     03 IO-AREA             PIC X(600).                                   
012800*                                                                         
012900*    03 AREA  -COPY WDD201  -PRE WDD201-  -RED IO-AREA.                   
013000     EJECT                                                                
013100*    03 AREA  -COPY WDD211  -PRE WDD211-  -RED IO-AREA.                   
013200     EJECT                                                                
013300 01  DLI-IO-AREA2.                                                        
013400     03 IO-AREA2            PIC X(700).                                   
013500*    03       -COPY WDK601                   -RED IO-AREA2.               
013600     EJECT                                                                
013700*    03 AREA  -COPY WDGX1124C0 -PRE XXAP11-  -RED IO-AREA2.               
013800     EJECT                                                                
013900*    03 AREA  -COPY WDGX1126C0 -PRE XXAP12-  -RED IO-AREA2.               
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200     SKIP2                                                                
014300*    -COPY W0008 -PRE WDD2-.                                              
014400          05  FILLER         PIC XX.                                      
014500     EJECT                                                                
014600*    -COPY W0008 -PRE ARTC-.                                              
014700          05  FILLER         PIC XX.                                      
014800     EJECT                                                                
014900*    -COPY W0008 -PRE XXAP-.                                              
015000          05  FILLER         PIC XX.                                      
015100     EJECT                                                                
015200 PROCEDURE DIVISION  USING WDD2-PCB ARTC-PCB                              
015300                                    XXAP-PCB.                             
015400     ENTRY 'DLITCBL' USING WDD2-PCB ARTC-PCB                              
015500                                    XXAP-PCB.                             
015600     EJECT                                                                
015700     PERFORM A-INIT                                                       
015800     PERFORM IMS-GET-WDD2                                                 
015900     PERFORM UNTIL BASEN-SLUT                                             
016000         EVALUATE  WDD2-SEG-NAME-FB                                       
016100            WHEN  'WDD201  '                                              
016200               INITIALIZE     SPAR-POST                                   
016300               PERFORM B-KONTROLL-WDD201                                  
016400            WHEN   'WDD211'                                               
016500               IF SW-WDD211 = JA                                          
016600                  IF WDD211-ART-KVBASLM = ZERO                            
016700                     CONTINUE                                             
016800                  ELSE                                                    
016900                     PERFORM C-KONTROLL-WDD211                            
017000                  END-IF                                                  
017100               END-IF                                                     
017200         END-EVALUATE                                                     
017300         PERFORM IMS-GET-WDD2                                             
017400     END-PERFORM                                                          
017500     PERFORM Z-FINIT                                                      
017600     MOVE ZERO               TO RETURN-CODE                               
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018100                                                                          
018200     OPEN OUTPUT UTFIL                                                    
018300                 FELFIL                                                   
018400                                                                          
018500     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
018600     .                                                                    
018700     EJECT                                                                
018800                                                                          
018900 B-KONTROLL-WDD201 SECTION.                                               
019000                                                                          
019100     MOVE JA TO SW-WDD211                                                 
019200                                                                          
019300     IF WDD201-ART-KVBASL > ZERO                                          
019400        MOVE WDD201-ART-IDPROJ TO W-1123-IDPROJ                           
019500        MOVE WDD201-ART-IDARTNR  TO W-IDARTNR                             
019600                                    SPAR-IDARTNR                          
019700        PERFORM IMS-GU-ARTC01                                             
019800        IF SEGMENT-FINNS                                                  
019900           PERFORM BA-KTR-PRODSLAG                                        
020000           IF SW-WDD211 = JA                                              
020100              PERFORM IMS-GET-PROJ-ROT                                    
020200              IF SEGMENT-FINNS                                            
020300                 PERFORM IMS-GET-1124                                     
020400                 IF SEGMENT-FINNS                                         
020500                    MOVE XXAP11-1124-TIGENORD TO WS-TIGENORD              
020600                 ELSE                                                     
020700                    STRING 'PRODSL/PROJ SAKNAR GENERELL '                 
020800                    'ORDERVECKA / MARKNADS-UNIK ORDERVECKA'               
020900                    DELIMITED BY SIZE INTO FEL-TEXT                       
021000                    PERFORM S02-FELRUTIN                                  
021100                    MOVE NEJ TO SW-WDD211                                 
021200                 END-IF                                                   
021300              ELSE                                                        
021400                 MOVE W-1123-KDPRODSL TO DISPLAY-KDPRODSL                 
021500                 STRING 'FEL PÅ PROJEKT ELLER PRUDUKTSLAG '               
021600                        'PROJ  ='  W-1123-IDPROJ     ' '                  
021700                        'PRODSL= ' DISPLAY-KDPRODSL                       
021800                 DELIMITED BY SIZE INTO FEL-TEXT                          
021900                 PERFORM S02-FELRUTIN                                     
022000                 MOVE NEJ TO SW-WDD211                                    
022100              END-IF                                                      
022200           END-IF                                                         
022300        ELSE                                                              
022400           STRING                                                         
022500           'ARTIKEL SAKNAS PÅ ARTIKEL-REG. MEN FINNS PÅ '                 
022600           'NYPON-REGISTRET'                                              
022700           DELIMITED BY SIZE INTO FEL-TEXT                                
022800           PERFORM S02-FELRUTIN                                           
022900           MOVE NEJ TO SW-WDD211                                          
023000        END-IF                                                            
023100     ELSE                                                                 
023200        MOVE NEJ TO SW-WDD211                                             
023300     END-IF                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 BA-KTR-PRODSLAG   SECTION.                                               
023700                                                                          
023800     MOVE ART-KDPRODSL TO TEST-KDPRODSL                                   
023900                                                                          
024000     IF KDPRODSL-VOLVO-UTAN-EMB                                           
024100        MOVE +11          TO W-1123-KDPRODSL                              
024200     ELSE                                                                 
024300        MOVE NEJ TO SW-WDD211                                             
024400     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 C-KONTROLL-WDD211 SECTION.                                               
024800                                                                          
024900     MOVE ZERO TO WS-TIBEHOV                                              
025000     MOVE WDD211-ART-KDBASLM TO W-1126-KDBASLM                            
025100                                                                          
025200     PERFORM IMS-GET-1126-KDBASLM-UNIK                                    
025300     IF SEGMENT-FINNS                                                     
025400        IF XXAP12-1126-TIBASORD = ZERO  OR = +1                           
025500           IF XXAP12-1126-TIMARKORD = ZERO                                
025600              MOVE WS-TIGENORD            TO WS-TIBEHOV                   
025700           ELSE                                                           
025800              MOVE XXAP12-1126-TIMARKORD  TO WS-TIBEHOV                   
025900           END-IF                                                         
026000           PERFORM CA-DATCONV                                             
026100           IF DAT-KDSVAR-OK                                               
026200              MOVE DAT-TIAA-VECKA         TO W-AA                         
026300              MOVE DAT-TIVV               TO W-VV                         
026400              MOVE W-AAVV                 TO WS-AAVV                      
026500              MOVE WS-AAVV                TO SPAR-TIBEHOV                 
026600              IF WDD211-ART-IDDISTR(2) = ZERO                             
026700                 COMPUTE                                                  
026800                         WS-KVBASLM = XXAP12-1126-KVBLKIT *               
026900                                      WDD211-ART-KVBASLKIT +              
027000                                      WDD211-ART-KVBASLMD(1)              
027100                 IF WS-KVBASLM = ZERO                                     
027200                    STRING                                                
027300                    ' 1126-KVBLKIT BORTTAGEN EFTER LAGD KVANT.  '         
027400                    'MARKNAD = ' WDD211-ART-KDBASLM                       
027500                    DELIMITED BY SIZE INTO  FEL-TEXT                      
027600                    PERFORM S02-FELRUTIN                                  
027700                 ELSE                                                     
027800                    MOVE WS-KVBASLM                                       
027900                                TO SPAR-SUTPO-EJPB                        
028000                    MOVE XXAP12-1126-TIBASORD                             
028100                                TO SPAR-TIBASORD                          
028200                    MOVE +1     TO SPAR-KDCLAGER                          
028300                    PERFORM S01-SKRIV-UTPOST                              
028400                 END-IF                                                   
028500              ELSE                                                        
028600                 PERFORM CB-RAKNA-DISTR-KVANT                             
028700              END-IF                                                      
028800           ELSE                                                           
028900              STRING                                                      
029000              ' DAT-KDSVAR FEL, KAN EJ BERÄKNA ORDERVECKA '               
029100              '( GEN-ORDVECKA/MARKN.UNUK ORDERVECKA)'                     
029200              DELIMITED BY SIZE INTO  FEL-TEXT                            
029300              PERFORM S02-FELRUTIN                                        
029400           END-IF                                                         
029500        END-IF                                                            
029600     ELSE                                                                 
029700        STRING 'MARKNAD SAKNAS PÅ PROJEKT-STRUKTUR MARKNAD= '             
029800                    W-1126-KDBASLM                                        
029900        DELIMITED BY SIZE INTO FEL-TEXT                                   
030000        PERFORM S02-FELRUTIN                                              
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400 CA-DATCONV       SECTION.                                                
030500                                                                          
030600     MOVE WS-TIBEHOV                TO DAT-I-TIDATUM                      
030700     MOVE 'AAMMDD'                  TO DAT-KDDATFORM                      
030800     CALL WDATKONV USING               DAT-KDDATFORM                      
030900                                       DAT-I-TIDATUM                      
031000                                       DAT-O-TIDATUM                      
031100                                       DAT-KDSVAR                         
031200     .                                                                    
031300                                                                          
031400     EJECT                                                                
031500 CB-RAKNA-DISTR-KVANT SECTION.                                            
031600                                                                          
031700     MOVE NEJ TO WS-FL-DIST-KVANT                                         
031800     MOVE 1   TO INDX                                                     
031900     PERFORM UNTIL INDX > 6                                               
032000        IF WDD211-ART-KVBASLMD(INDX) > ZERO                               
032100           MOVE JA TO WS-FL-DIST-KVANT                                    
032200           MOVE WDD211-ART-KVBASLMD(INDX) TO                              
032300                                   SPAR-SUTPO-EJPB                        
032400           MOVE XXAP12-1126-TIBASORD  TO                                  
032500                                   SPAR-TIBASORD                          
032600           MOVE +1     TO SPAR-KDCLAGER                                   
032700           PERFORM S01-SKRIV-UTPOST                                       
032800        END-IF                                                            
032900        ADD 1 TO INDX                                                     
033000     END-PERFORM                                                          
033100                                                                          
033200     IF WS-FL-DIST-KVANT = JA                                             
033300        CONTINUE                                                          
033400     ELSE                                                                 
033500        MOVE 1 TO INDX                                                    
033600        PERFORM UNTIL ( INDX > 6 )                                        
033700                   OR ( WDD211-ART-IDDISTR(INDX) = ZERO )                 
033800           COMPUTE                                                        
033900           SPAR-SUTPO-EJPB = WDD211-ART-KVBASLM *                         
034000                XXAP12-1126-REBLFORD(INDX) / 100 + 0.5                    
034100           MOVE XXAP12-1126-TIBASORD  TO                                  
034200                                   SPAR-TIBASORD                          
034300           MOVE +1     TO SPAR-KDCLAGER                                   
034400           PERFORM S01-SKRIV-UTPOST                                       
034500           ADD 1 TO INDX                                                  
034600        END-PERFORM                                                       
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 S01-SKRIV-UTPOST  SECTION.                                               
035100                                                                          
035200     MOVE SPAR-POST TO UT-AREA                                            
035300     WRITE UTPOST FROM UT-AREA                                            
035400                                                                          
035500     MOVE 'W11520'   TO POSTSUM-FDNAMN                                    
035600     MOVE 'W11520D1' TO POSTSUM-DDNAMN2                                   
035700     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
035800     CALL POSTSUM USING POSTSUM-PARM                                      
035900                                                                          
036000     .                                                                    
036100     EJECT                                                                
036200 S02-FELRUTIN        SECTION.                                             
036300                                                                          
036400     MOVE W-IDARTNR TO FEL-ARTIKELNR                                      
036500     WRITE FELPOST                                                        
036600     .                                                                    
036700     EJECT                                                                
036800 Z-FINIT SECTION.                                                         
036900     SKIP2                                                                
037000     MOVE 'S' TO POSTSUM-OPKOD                                            
037100     CALL POSTSUM USING POSTSUM-PARM                                      
037200                                                                          
037300     CLOSE   UTFIL                                                        
037400            FELFIL                                                        
037500     EJECT                                                                
037600     .                                                                    
037700******* I M S   S E C T I O N  ***                                        
037800 IMS-GET-WDD2 SECTION.                                                    
037900*                                                                         
038000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
038100     CALL CBLTDLI USING GN WDD2-PCB DLI-IO-AREA                           
038200     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
038300     PERFORM IMS-STATUSKONTROLL                                           
038400     .                                                                    
038500     EJECT                                                                
038600 IMS-GET-PROJ-ROT SECTION.                                                
038700*                                                                         
038800     STRING 'WLXXAP01(WDGXKEY  =' W-1123-KEY-X ')'                        
038900         DELIMITED BY SIZE INTO SSA1                                      
039000     MOVE '  GE'   TO GODK-STATUSKODER                                    
039100     CALL CBLTDLI USING GU XXAP-PCB DLI-IO-AREA2 SSA1                     
039200     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
039300     PERFORM IMS-STATUSKONTROLL                                           
039400     SKIP2                                                                
039500     .                                                                    
039600     EJECT                                                                
039700 IMS-GET-1124 SECTION.                                                    
039800*                                                                         
039900     MOVE 'WLXXAP11 ' TO SSA1                                             
040000     MOVE '  GE'   TO GODK-STATUSKODER                                    
040100     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA2 SSA1                    
040200     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
040300     PERFORM IMS-STATUSKONTROLL                                           
040400     SKIP2                                                                
040500     .                                                                    
040600     EJECT                                                                
040700 IMS-GET-1126-KDBASLM-UNIK SECTION.                                       
040800*                                                                         
040900     STRING 'WLXXAP12(WDGXKEY  =' W-1126-KEY-X ')'                        
041000         DELIMITED BY SIZE INTO SSA1                                      
041100     MOVE '  GE'   TO GODK-STATUSKODER                                    
041200     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA2 SSA1                    
041300     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
041400     PERFORM IMS-STATUSKONTROLL                                           
041500     SKIP2                                                                
041600     .                                                                    
041700     EJECT                                                                
041800 IMS-GU-ARTC01 SECTION.                                                   
041900*                                                                         
042000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
042100         DELIMITED BY SIZE INTO SSA1                                      
042200     MOVE '  GE'   TO GODK-STATUSKODER                                    
042300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA2 SSA1                     
042400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
042500     PERFORM IMS-STATUSKONTROLL                                           
042600     SKIP2                                                                
042700     .                                                                    
042800     EJECT                                                                
042900 IMS-STATUSKONTROLL SECTION.                                              
043000*                                                                         
043100     SET STATUS-IX TO 1                                                   
043200     SEARCH GODK-STATUS AT END CALL FELLOG                                
043300         WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                         
043400         CONTINUE                                                         
043500     END-SEARCH                                                           
043600     .                                                                    
