000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4405V00.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
000400 DATE-WRITTEN.   APRIL 2015                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER TRANSAR FRÅN VORKÖ NY.                                     
001000*        OM SAMMA ARTIKEL FÖREKOMMER FLERA GÅNGER SLÅS DEN IHOP.          
001100*        UTPOSTEN ANPASSAD FÖR EXCEL.                                     
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400                                                                          
002500*          --- RADER FRÅN VORKÖ NY                                        
002600     SELECT W4405V                     ASSIGN TO W4405VD1.                
002700                                                                          
002800*          --- POSTER TILL UPPF. 24 EXCEL ANPASSAT                        
002900     SELECT W4405X                     ASSIGN TO W4405VD2.                
003000                                                                          
003100*          --- POSTER TILL UPPF. DAG EXCEL ANPASSAT                       
003200     SELECT W4405Y                     ASSIGN TO W4405VD3.                
003300                                                                          
003400                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800                                                                          
003900 FD  W4405V                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W4405V      -L.                                                
004400                                                                          
004500 FD  W4405X                                                               
004600     RECORDING       V                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900 01  VOR24-HEAD-POST PIC X(100).                                          
005000*01  POST -COPY W4405E -PRE  UT24-  -L.                                   
005100                                                                          
005200                                                                          
005300 FD  W4405Y                                                               
005400     RECORDING       V                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700 01  VORDAG-HEAD-POST PIC X(100).                                         
005800*01  POST -COPY W4405E -PRE  UTDAG-  -L.                                  
005900                                                                          
006000                                                                          
006100                                                                          
006200 WORKING-STORAGE SECTION.                                                 
006300                                                                          
006400 77  IDPGM                       PIC X(8)    VALUE 'W4405V00'.            
006500 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800 77  HORIZTAB                    PIC X       VALUE X'05'.                 
006900 77  HEX-5E                      PIC X       VALUE ';'.                   
007000 77  ABENDTEXT                   PIC X(30)   VALUE ' '.                   
007100                                                                          
007200 77  IN-EJ-TOM-SW                PIC X       VALUE 'N'.                   
007300   88  IN-EJ-TOM                 VALUE 'J'.                               
007700 77  W4405V-EOF-SW               PIC X       VALUE 'N'.                   
007800     88  END-OF-W4405V                       VALUE 'J'.                   
007900                                                                          
008210 01  WS-TIKLATID                 PIC S9(9)   VALUE ZERO COMP-3.           
008220 01  WS-TIREGTID                 PIC S9(9)   VALUE ZERO COMP-3.           
008300 01  WS-KVWORKD                  PIC S9(3)   VALUE ZERO COMP-3.           
008400                                                                          
008500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009000                                                                          
009100                                                                          
009200 01  DYNAMISKA-SUBPROGRAM.                                                
009300                                                                          
009400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
009700                                                                          
009800*    --- PARAMETRAR TILL ABEND                                            
009900                                                                          
010000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010300                                                                          
010400 01  FELTEXT.                                                             
010500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010700                                                                          
010800                                                                          
010900*    --- PARAMETRAR TILL POSTSUM                                          
011000                                                                          
011100*01  -COPY W0005   -PRE  POSTSUM-                                         
011200                                                                          
011300*01  -COPY WORKAREA                                                       
011400                                                                          
011500 01  IN-AREA-START               PIC X(24)   VALUE                        
011600                                 'IN-AREA-START  '.                       
011700                                                                          
011800                                                                          
011900*01  AREA -COPY W4405V     -PRE IN-                                       
012000     EJECT                                                                
012100 01  UT-AREA-START               PIC X(24)   VALUE                        
012200                                 'UT-AREA-24-START'.                      
012300                                                                          
012400*01  AREA -COPY W4405E     -PRE UT-                                       
012500                                                                          
012600                                                                          
012700 01  FIRST-AREA-START            PIC X(24)   VALUE                        
012800                                 'FIRST-AREA-START '.                     
012900                                                                          
013000*01  AREA -COPY W4405V     -PRE FIRST-                                    
013100                                                                          
013110                                                                          
013120                                                                          
013130 01  LAST-AREA-START             PIC X(24)   VALUE                        
013140                                 'LAST-AREA-START '.                      
013150                                                                          
013160*01  AREA -COPY W4405V     -PRE LAST-                                     
013170                                                                          
013200 01  VOR24-HEAD-AREA                 PIC X(100).                          
013300 01  VORDAG-HEAD-AREA                PIC X(100).                          
013400                                                                          
013500                                                                          
013600                                                                          
013700 PROCEDURE DIVISION.                                                      
013800 MAIN SECTION.                                                            
013900                                                                          
014000                                                                          
014100     PERFORM A-INIT                                                       
014200     PERFORM S01-LAES-W4405V                                              
014300                                                                          
014400     IF NOT END-OF-W4405V                                                 
014500        MOVE JA             TO IN-EJ-TOM-SW                               
014600        MOVE IN-W4405V      TO FIRST-W4405V                               
014700        MOVE IN-W4405V      TO LAST-W4405V                                
014800     END-IF                                                               
014900                                                                          
015000     PERFORM UNTIL END-OF-W4405V                                          
015904                                                                          
015910        IF  IN-IDPTYP        = FIRST-IDPTYP                               
015920        AND IN-IDDISTR       = FIRST-IDDISTR                              
015930        AND IN-IDKUNDNR      = FIRST-IDKUNDNR                             
015940        AND IN-IDKUNDRF      = FIRST-IDKUNDRF                             
015950        AND IN-TIREGDAT-URSP = FIRST-TIREGDAT-URSP                        
015960        AND IN-TIREGTID-URSP = FIRST-TIREGTID-URSP                        
015970        AND IN-IDARTNR       = FIRST-IDARTNR                              
015980                                                                          
016000                                                                          
016010            MOVE IN-W4405V  TO LAST-W4405V                                
016100            IF IN-KDVORATG < FIRST-KDVORATG                               
016200               MOVE IN-KDVORATG TO FIRST-KDVORATG                         
016300            END-IF                                                        
016400                                                                          
016500        ELSE                                                              
016600*------------ OBS ' ' SKALL INNEHÅLLA TAB (HEX-05)                        
016800           PERFORM B-KOLLA-DAGAR                                          
016900                                                                          
017000           MOVE ALL '	'             TO UT-AREA                            
017100           MOVE FIRST-IDDISTR       TO UT-IDDISTR                         
017200           MOVE FIRST-IDKUNDNR      TO UT-IDKUNDNR                        
017300           MOVE FIRST-IDKUNDRF      TO UT-IDKUNDRF                        
017400           MOVE FIRST-TIREGDAT-URSP TO UT-TIREGDAT-URSP                   
017500           MOVE FIRST-IDARTNR       TO UT-IDARTNR                         
017600           MOVE WS-KVWORKD          TO UT-KVWORKD                         
017700           MOVE 1                   TO UT-KVRADER                         
017800                                                                          
017900           IF FIRST-KDVORATG = '0'                                        
018000           OR FIRST-KDVORATG = '1'                                        
018100           OR FIRST-KDVORATG = '2'                                        
018200              MOVE 'P'             TO UT-KDVORSTA                         
018300           ELSE                                                           
018400              IF FIRST-KDVORATG = '3'                                     
018500                 MOVE 'N'          TO UT-KDVORSTA                         
018600              ELSE                                                        
018700                 MOVE 'A'          TO UT-KDVORSTA                         
018800              END-IF                                                      
018900           END-IF                                                         
019000                                                                          
019100           IF FIRST-IDPTYP = '24 '                                        
019200              PERFORM S11-SKRIV-W4405X                                    
019300           ELSE                                                           
019400              PERFORM S12-SKRIV-W4405Y                                    
019500           END-IF                                                         
019800                                                                          
019900          MOVE IN-W4405V        TO FIRST-W4405V                           
020000          MOVE IN-W4405V        TO LAST-W4405V                            
020100       END-IF                                                             
020200                                                                          
020300       PERFORM S01-LAES-W4405V                                            
020400     END-PERFORM                                                          
020500                                                                          
020600     IF IN-EJ-TOM                                                         
020700*------------ OBS ' ' SKALL INNEHÅLLA TAB (HEX-05)                        
020710        PERFORM B-KOLLA-DAGAR                                             
020720                                                                          
020800        MOVE ALL '	'             TO UT-AREA                               
020900        MOVE FIRST-IDDISTR       TO UT-IDDISTR                            
021000        MOVE FIRST-IDKUNDNR      TO UT-IDKUNDNR                           
021100        MOVE FIRST-IDKUNDRF      TO UT-IDKUNDRF                           
021200        MOVE FIRST-TIREGDAT-URSP TO UT-TIREGDAT-URSP                      
021300        MOVE FIRST-IDARTNR       TO UT-IDARTNR                            
021400        MOVE WS-KVWORKD          TO UT-KVWORKD                            
021500        MOVE 1                   TO UT-KVRADER                            
021600                                                                          
021700        IF    FIRST-KDVORATG = '0'                                        
021800        OR    FIRST-KDVORATG = '1'                                        
021900        OR    FIRST-KDVORATG = '2'                                        
022000           MOVE 'P'              TO UT-KDVORSTA                           
022100        ELSE                                                              
022200           IF FIRST-KDVORATG = '3'                                        
022300              MOVE 'N'             TO UT-KDVORSTA                         
022400           ELSE                                                           
022500              MOVE 'A'             TO UT-KDVORSTA                         
022600           END-IF                                                         
022700        END-IF                                                            
022800                                                                          
022900        IF FIRST-IDPTYP = '24 '                                           
023000           PERFORM S11-SKRIV-W4405X                                       
023100        ELSE                                                              
023200           PERFORM S12-SKRIV-W4405Y                                       
023300        END-IF                                                            
023400     END-IF                                                               
023500                                                                          
023600     PERFORM Z-FINIT                                                      
023700                                                                          
023800     MOVE ZERO TO RETURN-CODE                                             
023900     GOBACK                                                               
024000     .                                                                    
024100                                                                          
024200                                                                          
024300                                                                          
024400 A-INIT SECTION.                                                          
024500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
024600                                                                          
024700     OPEN INPUT  W4405V                                                   
024800                                                                          
024900     OPEN OUTPUT W4405X W4405Y                                            
025000                                                                          
025100     ACCEPT DAGENS-DATUM  FROM DATE                                       
025200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025300     PERFORM S02-TILL-VOR24-HEAD                                          
025400     PERFORM S03-TILL-VORDAG-HEAD                                         
025500     .                                                                    
025600                                                                          
025700                                                                          
025800 B-KOLLA-DAGAR SECTION.                                                   
025900     MOVE 'B-KOLLA-DAGAR   ' TO CURRENT-SECTION                           
026000                                                                          
026100*  JÄMFÖR URPRUNGLIG REGDATUM MED KLARTID I SISTA POSTEN                  
026300*  OM KLARTIDEN I SISTA POSTEN ÄR NOLL SÄTTS KVWORKD = 999                
026400                                                                          
026500     IF LAST-TIKLAR = ZERO                                                
026600        MOVE 999             TO WS-KVWORKD                                
026700     ELSE                                                                 
026800        MOVE 001                 TO WORK-KDCALL                           
026900        MOVE '11'                TO WORK-IDDC                             
027000        MOVE FIRST-TIREGDAT-URSP TO WORK-TIAAMMDD-FOM                     
027100        MOVE LAST-TIKLAR         TO WORK-TIAAMMDD-TOM                     
027200                                                                          
027300        CALL WORKDAY USING                                                
027400             WORK-KDCALL                                                  
027500             WORK-DATE-AREA                                               
027600             WORK-KDSVAR                                                  
027700                                                                          
027800        IF WORK-KDSVAR = SPACE                                            
027900           MOVE WORK-KVWORKD   TO WS-KVWORKD                              
028000           IF WS-KVWORKD = 0                                              
028100              MOVE 1           TO WS-KVWORKD                              
028200           END-IF                                                         
028300        ELSE                                                              
028400           MOVE 'FELSVAR FRÅN WORKDAY 1' TO ABENDTEXT                     
028500           MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                         
028600           PERFORM S99-ABEND                                              
028700        END-IF                                                            
028800                                                                          
028900        MOVE FIRST-TIREGDAT-URSP TO WORK-TIAAMMDD-FOM                     
029000        MOVE FIRST-TIREGDAT-URSP TO WORK-TIAAMMDD-TOM                     
029100                                                                          
029200        CALL WORKDAY USING                                                
029300             WORK-KDCALL                                                  
029400             WORK-DATE-AREA                                               
029500             WORK-KDSVAR                                                  
029600                                                                          
029700        IF WORK-KDSVAR = SPACE                                            
029800           IF WORK-KVWORKD = 0                                            
029900*  ARBETSFRI DAG                                                          
030000              MOVE 0                   TO WS-TIREGTID                     
030100           ELSE                                                           
030200              MOVE FIRST-TIREGTID-URSP TO WS-TIREGTID                     
030300           END-IF                                                         
030400        ELSE                                                              
030500           MOVE 'FELSVAR FRÅN WORKDAY 2' TO ABENDTEXT                     
030600           MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                         
030700           PERFORM S99-ABEND                                              
030800        END-IF                                                            
030900                                                                          
031000        MOVE LAST-TIKLAR     TO WORK-TIAAMMDD-FOM                         
031100        MOVE LAST-TIKLAR     TO WORK-TIAAMMDD-TOM                         
031200                                                                          
031300        CALL WORKDAY USING                                                
031400             WORK-KDCALL                                                  
031500             WORK-DATE-AREA                                               
031600             WORK-KDSVAR                                                  
031700                                                                          
031800        IF WORK-KDSVAR = SPACE                                            
031900           IF WORK-KVWORKD = 0                                            
032000*  ARBETSFRI DAG                                                          
032100              MOVE 24000000      TO WS-TIKLATID                           
032200           ELSE                                                           
032300              COMPUTE WS-TIKLATID = LAST-TIKLATID                         
032400                                  * 100                                   
032500           END-IF                                                         
032600        ELSE                                                              
032700           MOVE 'FELSVAR FRÅN WORKDAY 3' TO ABENDTEXT                     
032800           MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                         
032900           PERFORM S99-ABEND                                              
033000        END-IF                                                            
033100                                                                          
033200        IF WS-TIKLATID <= WS-TIREGTID                                     
033300           SUBTRACT 1  FROM WS-KVWORKD                                    
033400        END-IF                                                            
033500     END-IF                                                               
033600     .                                                                    
033700                                                                          
033800                                                                          
033900 Z-FINIT SECTION.                                                         
034000     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
034100                                                                          
034200     CLOSE W4405V                                                         
034300           W4405X W4405Y                                                  
034400                                                                          
034500     MOVE 'S' TO POSTSUM-OPKOD                                            
034600     CALL POSTSUM USING POSTSUM-PARM                                      
034700     .                                                                    
034800                                                                          
034900                                                                          
035000 S01-LAES-W4405V  SECTION.                                                
035100     READ W4405V INTO IN-AREA                                             
035200     AT END                                                               
035300        MOVE HIGH-VALUE TO IN-AREA                                        
035400        SET END-OF-W4405V TO TRUE                                         
035500                                                                          
035600     NOT AT END                                                           
035700        MOVE 'W4405V'   TO POSTSUM-FDNAMN                                 
035800        MOVE 'W4405VD1' TO POSTSUM-DDNAMN2                                
035900        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
036000        CALL POSTSUM USING POSTSUM-PARM                                   
036100     END-READ                                                             
036200     .                                                                    
036300                                                                          
036400                                                                          
036500**** RUBRIK TILL EXCEL-FIL ****                                           
036600 S02-TILL-VOR24-HEAD  SECTION.                                            
036700                                                                          
036800     STRING 'DISTRICT', HORIZTAB,                                         
036900            'CUST NO', HORIZTAB,                                          
037000            'ORDER NO', HORIZTAB,                                         
037100            'REG DATE', HORIZTAB,                                         
037200            'PART NO', HORIZTAB,                                          
037300            'STATUS', HORIZTAB,                                           
037400            'WORKDAYS', HORIZTAB,                                         
037500            'LINES'                                                       
037600     DELIMITED BY SIZE INTO VOR24-HEAD-AREA                               
037700                                                                          
037800     WRITE VOR24-HEAD-POST FROM VOR24-HEAD-AREA                           
037900     MOVE 'HEAD'   TO POSTSUM-TRANSTYP                                    
038000     MOVE 'W4405X' TO POSTSUM-FDNAMN                                      
038100     MOVE 'W4405VD2' TO POSTSUM-DDNAMN2                                   
038200     CALL POSTSUM USING POSTSUM-PARM                                      
038300     .                                                                    
038400                                                                          
038500**** RUBRIK TILL EXCEL-FIL ****                                           
038600 S03-TILL-VORDAG-HEAD  SECTION.                                           
038700                                                                          
038800     STRING 'DISTRICT', HORIZTAB,                                         
038900            'CUST NO', HORIZTAB,                                          
039000            'ORDER NO', HORIZTAB,                                         
039100            'REG DATE', HORIZTAB,                                         
039200            'PART NO', HORIZTAB,                                          
039300            'STATUS', HORIZTAB,                                           
039400            'WORKDAYS', HORIZTAB,                                         
039500            'LINES'                                                       
039600     DELIMITED BY SIZE INTO VORDAG-HEAD-AREA                              
039700                                                                          
039800     WRITE VORDAG-HEAD-POST FROM VORDAG-HEAD-AREA                         
039900     MOVE 'HEAD'   TO POSTSUM-TRANSTYP                                    
040000     MOVE 'W4405Y' TO POSTSUM-FDNAMN                                      
040100     MOVE 'W4405VD3' TO POSTSUM-DDNAMN2                                   
040200     CALL POSTSUM USING POSTSUM-PARM                                      
040300     .                                                                    
040400                                                                          
040500                                                                          
040600 S11-SKRIV-W4405X SECTION.                                                
040700                                                                          
040800     WRITE UT24-POST FROM UT-AREA                                         
040900                                                                          
041000     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
041100     MOVE 'W4405X'   TO POSTSUM-FDNAMN                                    
041200     MOVE 'W4405ED2' TO POSTSUM-DDNAMN2                                   
041300     CALL POSTSUM USING POSTSUM-PARM                                      
041400     .                                                                    
041500                                                                          
041600                                                                          
041700 S12-SKRIV-W4405Y SECTION.                                                
041800                                                                          
041900     WRITE UTDAG-POST FROM UT-AREA                                        
042000                                                                          
042100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
042200     MOVE 'W4405Y'   TO POSTSUM-FDNAMN                                    
042300     MOVE 'W4405ED3' TO POSTSUM-DDNAMN2                                   
042400     CALL POSTSUM USING POSTSUM-PARM                                      
042500     .                                                                    
042510                                                                          
042520                                                                          
042600 S99-ABEND SECTION.                                                       
042700                                                                          
042800     SKIP2                                                                
042900     MOVE 'S' TO POSTSUM-OPKOD                                            
043000     CALL POSTSUM USING POSTSUM-PARM                                      
043100     CALL ABEND USING RKOD-ABEND                                          
043200     .                                                                    
