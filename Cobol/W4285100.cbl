000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4285100.                                                
000300 AUTHOR.         SHILPA MADHURI G.                                        
000400 DATE-WRITTEN.   11/11/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700******************************************************************        
000800*                                                                *        
000900* THE PROGRAM GENERATES A FILE TAKING THE INPUT FROM W4285000    *        
001000* EPLUS PROGRAM WHICH WILL BE USED TO CREATE 'DEALER RETURN      *        
001100* WEEKLY FOLLOW UP' REPORT ON WEB.                               *        
001200*                                                                *        
001210* E'TRACKER:         10143271 - CHINA WAREHOUSE PROJECT-1        *        
001220*                                                                *        
001300******************************************************************        
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900*          --- W42850 FILE                                                
002000       SELECT W42850                     ASSIGN TO W42851D1.              
002100*          --- W42851 FILE                                                
002200       SELECT W428511                    ASSIGN TO W42851D2.              
002300       EJECT                                                              
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600 FD   W42850                                                              
002700       RECORDING       F                                                  
002800       BLOCK CONTAINS  0.                                                 
002900                                                                          
003000*01   -COPY W42850      -PRE W42850-                                      
003100                                                                          
003200 FD   W428511                                                             
003300       RECORDING       V                                                  
003400       BLOCK CONTAINS  0.                                                 
003500 01  UT-DAP-POST.                                                         
003600       03  FILLER         PIC X(21).                                      
003700*01   -COPY W428511       -PRE U1-                                        
003800       EJECT                                                              
003900*01   -COPY W428512       -PRE U2-                                        
004000       EJECT                                                              
004100*01   -COPY W428513       -PRE U3-                                        
004200       EJECT                                                              
004300                                                                          
004400                                                                          
004500 01   W42851-001-LINE             PIC X(121).                             
004600       EJECT                                                              
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77   IDPGM                       PIC X(8)    VALUE 'W4285100'.           
005000 77   YES                         PIC X       VALUE 'J'.                  
005100 77   NOO                         PIC X       VALUE 'N'.                  
005200 77   WS-CODE-COUNT               PIC 9(5)    VALUE ZEROS.                
005300 77   WS-BINSCR-CNT               PIC 9(5)    VALUE ZEROS.                
005400 77   EDI-IDPTYP                  PIC X(3)    VALUE SPACE.                
005500 77   WS-TOT-DC                   PIC 9(2)    VALUE ZEROS.                
005600 77   WS-PREV-IDDC                PIC X(2)    VALUE SPACES.               
005700 77   WS-PREV-CODE                PIC X(2)    VALUE SPACES.               
005800 77   WS-SUM-AVGTIME              PIC 9(4)V9  VALUE ZEROS.                
005900 77   WS-AVG-DCTIME               PIC 9(4)V9  VALUE ZEROS.                
006000 77   W001-TOT-BIN                PIC 9(6)    VALUE ZEROS.                
006100 77   W001-TOT-SCRAP              PIC 9(6)    VALUE ZEROS.                
006110 77   WS-SUM-BIN                  PIC 9(6)    VALUE ZEROS.                
006120 77   WS-SUM-SCRAP                PIC 9(6)    VALUE ZEROS.                
006200                                                                          
006300 77   W428F1-EOF-SW               PIC X       VALUE 'N'.                  
006400      88  END-OF-W42850                       VALUE 'Y'.                  
006500      88  NO-EOF-W42850                       VALUE 'N'.                  
006600                                                                          
006700 77   W001-HEADER-SW              PIC X       VALUE 'N'.                  
006800      88  HDR-WRITTEN                         VALUE 'Y'.                  
006900      88  HDR-NOTWRITTEN                      VALUE 'N'.                  
007000                                                                          
007100 77   W001-W42850-SW              PIC X       VALUE 'N'.                  
007200      88  W42850-EMPTY                        VALUE 'Y'.                  
007300      88  W42850-NOTEMPTY                     VALUE 'N'.                  
007400                                                                          
007500       EJECT                                                              
007600 01   GENERAL-SUBPROGRAMS.                                                
007700*                                                                         
007800      03  ABEND                   PIC X(8)    VALUE 'ABEND'.              
007900      03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.            
008000      03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.            
008100      03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.           
008300*     TEMPORARY WORKING STORAGE VARIABLES USED                            
008400 01   WS-TEMP-STORAGE-AREA.                                               
008500      03  WS-TEMP-CODE            PIC X(2)    VALUE SPACES.               
008600      03  WS-TEMP-BIN             PIC 9(6)    VALUE ZEROS.                
008700      03  WS-TEMP-DEV             PIC 9(7)    VALUE ZEROS.                
008800      03  WS-TEMP-SCRAP           PIC 9(6)    VALUE ZEROS.                
008900      03  WS-TEMP-QUAL            PIC 9(7)    VALUE ZEROS.                
009000      03  WS-TEMP-AVTIM           PIC 9(4)V9  VALUE ZEROS.                
009100 01   WS-TOTAL-STORAGE-AREA.                                              
009200      03  WS-TOT-CODE             PIC 9(2)    VALUE ZEROS.                
009300      03  WS-TOT-BIN              PIC 9(6)    VALUE ZEROS.                
009400      03  WS-TOT-DEV              PIC 9(7)    VALUE ZEROS.                
009500      03  WS-TOT-SCRAP            PIC 9(6)    VALUE ZEROS.                
009600      03  WS-TOT-QUAL             PIC 9(7)    VALUE ZEROS.                
009700      03  WS-TOT-AVTIM            PIC 9(4)V9  VALUE ZEROS.                
009800*     --- PARAMETERS TO ABEND                                             
009900                                                                          
010000 77   RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.              
010100 77   RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.             
010200 77   RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.           
010300 01  DAP-AREA-TYPE.                                                       
010400       03  FILLER                  PIC X(15)   VALUE                      
010500           ' ¤DAPW42851-001'.                                             
010600 01  DAP-AREA-SUBTYPE.                                                    
010700       03  FILLER                  PIC X(5)    VALUE                      
010800        ' ¤DAP'.                                                          
010900       03  DAP-IDDC                PIC X(2)    VALUE SPACES.              
011000                                                                          
011100       EJECT                                                              
011200                                                                          
011300 01   PROGRAM-NAME                PIC X(6)    VALUE 'W42851'.             
011400*     --- PARAMETRAR TILL POSTSUM                                         
011500*01   -COPY W0005   -PRE  POSTSUM-                                        
011600       EJECT                                                              
011700                                                                          
011800*01   -COPY WDATAREA                                                      
011900       EJECT                                                              
012000                                                                          
012400 01   W428D1-AREA-START           PIC X(24)   VALUE                       
012500                                   'W428D1-AREA-START  '.                 
012600                                                                          
012700*01   AREA -COPY W42850     -PRE W428F1-                                  
012800       EJECT                                                              
012900 01   W001-LINE.                                                          
013000*                                                                         
013100      03  FILLER                  PIC X(121)  VALUE SPACE.                
013200      EJECT                                                               
013300 PROCEDURE DIVISION.                                                      
013400 MAIN SECTION.                                                            
013500                                                                          
013600       PERFORM A-INIT                                                     
013700       PERFORM S01-READ-W42850                                            
013800       IF END-OF-W42850                                                   
013900          SET W42850-EMPTY TO TRUE                                        
014000       END-IF                                                             
014100                                                                          
014200       IF NOT END-OF-W42850                                               
014300        PERFORM UNTIL END-OF-W42850                                       
014400         IF NOT END-OF-W42850                                             
014600           PERFORM B-PROCESS-W42850                                       
014700           PERFORM S01-READ-W42850                                        
014800         END-IF                                                           
014900        END-PERFORM                                                       
015000         IF END-OF-W42850                                                 
015200            PERFORM BB-WRITE-CODE-LINE                                    
015300            PERFORM BC-WRITE-DC-TOTAL-LINE                                
015400         END-IF                                                           
015500       ELSE                                                               
015600        DISPLAY 'NO RECORDS IN INPUT FILE'                                
015700       END-IF                                                             
015800                                                                          
016000       PERFORM Z-FINIT                                                    
016100                                                                          
016200       MOVE ZERO TO RETURN-CODE                                           
016300       GOBACK                                                             
016400       .                                                                  
016500       EJECT                                                              
016600 A-INIT SECTION.                                                          
016700                                                                          
016800       OPEN INPUT  W42850                                                 
016900                                                                          
017000       OPEN OUTPUT W428511                                                
017100       MOVE SPACES TO WS-PREV-IDDC WS-PREV-CODE                           
017200       .                                                                  
017300       EJECT                                                              
017400 Z-FINIT SECTION.                                                         
017500       CLOSE W42850                                                       
017600             W428511                                                      
017700       DISPLAY '************************************'                     
017800       DISPLAY '---------SUMMARY OF THE REPORT------'                     
017900       DISPLAY 'TOTAL NO OF REPORTS GENERATED =' WS-TOT-DC                
018000       DISPLAY '************************************'                     
018100       MOVE 'S' TO POSTSUM-OPKOD                                          
018200       CALL POSTSUM USING POSTSUM-PARM                                    
018300       .                                                                  
018400       EJECT                                                              
018500 S01-READ-W42850  SECTION.                                                
018600       READ W42850 INTO W428F1-AREA                                       
018700       AT END                                                             
018800         MOVE HIGH-VALUE   TO W428F1-AREA                                 
018900         DISPLAY 'EOF1 REACHED'                                           
019000         SET END-OF-W42850 TO TRUE                                        
019100                                                                          
019200       NOT AT END                                                         
019400         MOVE 'W42850'     TO POSTSUM-FDNAMN                              
019500         MOVE 'W42851D1'   TO POSTSUM-DDNAMN2                             
019600         MOVE SPACES       TO POSTSUM-TRANSTYP                            
019700         CALL POSTSUM   USING POSTSUM-PARM                                
019800         SET  W42850-NOTEMPTY TO TRUE                                     
019900       END-READ                                                           
020000       .                                                                  
020100       EJECT                                                              
020200 B-PROCESS-W42850    SECTION.                                             
020300     IF NOT END-OF-W42850                                                 
020500       IF W428F1-IDDC-RET = WS-PREV-IDDC                                  
020600         MOVE W428F1-IDDC-RET TO WS-PREV-IDDC                             
020700         PERFORM BA-PROCESS-CODE-FIELDS                                   
020800       ELSE                                                               
020900         IF WS-PREV-IDDC = SPACES                                         
021100           MOVE W428F1-IDDC-RET TO WS-PREV-IDDC                           
021200           PERFORM S20-SKRIV-START-POST                                   
021300           PERFORM S21A-WRITE-HEADERS                                     
021400           IF W428F1-IDDC-RET NOT EQUAL TO SPACES                         
021500             MOVE W428F1-IDDC-RET TO WS-PREV-IDDC                         
021600             PERFORM BA-PROCESS-CODE-FIELDS                               
021700           END-IF                                                         
021800         ELSE                                                             
021900           PERFORM BB-WRITE-CODE-LINE                                     
022000           PERFORM BC-WRITE-DC-TOTAL-LINE                                 
022100           MOVE W428F1-IDDC-RET TO WS-PREV-IDDC                           
022200           PERFORM BA-PROCESS-CODE-FIELDS                                 
022300           IF NOT END-OF-W42850                                           
022400             PERFORM S20-SKRIV-START-POST                                 
022500             PERFORM S21A-WRITE-HEADERS                                   
022600           END-IF                                                         
022700         END-IF                                                           
022800       END-IF                                                             
022900       END-IF                                                             
023000       .                                                                  
023100       EJECT                                                              
023200 BA-PROCESS-CODE-FIELDS  SECTION.                                         
023400       IF  W428F1-KDANMORS = WS-PREV-CODE                                 
023500           PERFORM BAA-SUM-CODE-FIELDS                                    
023600           MOVE W428F1-KDANMORS TO WS-PREV-CODE                           
023700       ELSE                                                               
023800         IF WS-PREV-CODE = SPACES                                         
023900           MOVE W428F1-KDANMORS TO WS-PREV-CODE                           
024000           PERFORM BAA-SUM-CODE-FIELDS                                    
024100         ELSE                                                             
024200           PERFORM BB-WRITE-CODE-LINE                                     
024300           PERFORM BAA-SUM-CODE-FIELDS                                    
024400           MOVE W428F1-KDANMORS TO WS-PREV-CODE                           
024500         END-IF                                                           
024600       END-IF                                                             
024700       .                                                                  
024800       EJECT                                                              
024900 BAA-SUM-CODE-FIELDS SECTION.                                             
025100       IF W42850-KVRETINL > 0                                             
025200         ADD 1                    TO WS-TEMP-BIN                          
025300         ADD 1                    TO WS-TOT-BIN                           
025400         ADD W42850-KVRETINL      TO W001-TOT-BIN                         
025500       END-IF                                                             
025600       IF W42850-KVAVV-KVANT > 0                                          
025700         ADD 1                    TO WS-TEMP-DEV                          
025800         ADD 1                    TO WS-TOT-DEV                           
025900       END-IF                                                             
026000       IF W42850-KVRETINL-SKR > 0                                         
026100         ADD 1                    TO WS-TEMP-SCRAP                        
026200         ADD 1                    TO WS-TOT-SCRAP                         
026300         ADD W42850-KVRETINL-SKR  TO W001-TOT-SCRAP                       
026400       END-IF                                                             
026500       IF W42850-KVAVV-KVAL   > 0                                         
026600         ADD 1                    TO WS-TEMP-QUAL                         
026700         ADD 1                    TO WS-TOT-QUAL                          
026800       END-IF                                                             
026900       ADD W42850-KVDAGAR-INL     TO WS-TEMP-AVTIM                        
027000         .                                                                
027100         EJECT                                                            
027200 BB-WRITE-CODE-LINE  SECTION.                                             
027400         MOVE WS-PREV-CODE        TO U2-DOC-KDANMORS                      
027500         MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                      
027600         MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                   
027700         MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                  
027800         MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                    
027900         COMPUTE WS-BINSCR-CNT = W001-TOT-BIN + W001-TOT-SCRAP            
028000         IF WS-BINSCR-CNT NOT = 0                                         
028100          COMPUTE  WS-TOT-AVTIM = (WS-TEMP-AVTIM / WS-BINSCR-CNT)         
028200         END-IF                                                           
028300         MOVE WS-TOT-AVTIM        TO U2-DOC-KVDAGDEC                      
028400         ADD  WS-TEMP-AVTIM       TO WS-SUM-AVGTIME                       
028500         ADD  W001-TOT-BIN        TO WS-SUM-BIN                           
028510         ADD  W001-TOT-SCRAP      TO WS-SUM-SCRAP                         
028600         MOVE '2         '        TO U2-DOC-IDAFPRCD                      
028700         MOVE U2-DOC-W428512      TO W001-LINE                            
028800         PERFORM S21-WRITE-W42851-001                                     
028900         INITIALIZE WS-TEMP-STORAGE-AREA WS-BINSCR-CNT                    
029000         INITIALIZE WS-TOT-AVTIM W001-TOT-BIN W001-TOT-SCRAP              
029100                                                                          
029200       .                                                                  
029300       EJECT                                                              
029400 BC-WRITE-DC-TOTAL-LINE SECTION.                                          
029600         MOVE WS-TOT-BIN         TO U3-DOC-KVRETINL                       
029700         MOVE WS-TOT-DEV         TO U3-DOC-KVAVV-KVANT                    
029800         MOVE WS-TOT-SCRAP       TO U3-DOC-KVRETINL-SKR                   
029900         MOVE WS-TOT-QUAL        TO U3-DOC-KVAVV-KVAL                     
029910         COMPUTE WS-CODE-COUNT = WS-SUM-BIN + WS-SUM-SCRAP                
030000         IF WS-CODE-COUNT NOT = 0                                         
030100          COMPUTE WS-AVG-DCTIME =                                         
030200                  (WS-SUM-AVGTIME / WS-CODE-COUNT)                        
030300         END-IF                                                           
030400         MOVE WS-AVG-DCTIME      TO U3-DOC-KVDAGDEC                       
030500         MOVE '3         '       TO U3-DOC-IDAFPRCD                       
030600         MOVE U3-DOC-W428513     TO W001-LINE                             
030700         PERFORM S21-WRITE-W42851-001                                     
030800         INITIALIZE WS-TOTAL-STORAGE-AREA WS-PREV-CODE                    
030900         INITIALIZE WS-AVG-DCTIME WS-SUM-AVGTIME WS-CODE-COUNT            
030910         INITIALIZE WS-SUM-SCRAP    WS-SUM-BIN                            
031000         ADD 1                   TO WS-TOT-DC                             
031100       .                                                                  
031200       EJECT                                                              
031300 S21-WRITE-W42851-001  SECTION.                                           
031400                                                                          
031500       WRITE W42851-001-LINE FROM W001-LINE                               
031600       INITIALIZE                 W001-LINE                               
031700       .                                                                  
031800       EJECT                                                              
031900 S21A-WRITE-HEADERS SECTION.                                              
032000                                                                          
032100       MOVE '1         '     TO U1-DOC-IDAFPRCD                           
032300       MOVE W42850-TISAAVV-INLINL-TIAAVV TO U1-DOC-TIAAVV                 
032400       MOVE DAP-IDDC         TO U1-DOC-IDDC                               
032600       INITIALIZE               W001-LINE                                 
032700       MOVE U1-DOC-W428511   TO W001-LINE                                 
032900       WRITE W42851-001-LINE FROM W001-LINE                               
033000       SET HDR-WRITTEN       TO TRUE                                      
033100       INITIALIZE               U1-DOC-W428511                            
033200       .                                                                  
033300       EJECT                                                              
033400 S20-SKRIV-START-POST SECTION.                                            
033500*                                                                         
033600       WRITE UT-DAP-POST                FROM DAP-AREA-TYPE                
033700       MOVE 'DAP'                       TO   EDI-IDPTYP                   
033800                                                                          
033900       MOVE EDI-IDPTYP                  TO POSTSUM-TRANSTYP               
034000       MOVE 'W428511'                   TO POSTSUM-FDNAMN                 
034100       MOVE 'W42851D2'                  TO POSTSUM-DDNAMN2                
034200       CALL POSTSUM                  USING POSTSUM-PARM                   
034300       MOVE  W428F1-IDDC-RET            TO DAP-IDDC                       
034400       MOVE  DAP-IDDC                   TO U1-DOC-IDDC                    
034600       WRITE UT-DAP-POST                FROM DAP-AREA-SUBTYPE             
034700       MOVE 'DAP'                       TO   EDI-IDPTYP                   
034800*                                                                         
034900       MOVE EDI-IDPTYP                  TO POSTSUM-TRANSTYP               
035000       MOVE 'W428511'                   TO POSTSUM-FDNAMN                 
035100       MOVE 'W42851D2'                  TO POSTSUM-DDNAMN2                
035200       CALL POSTSUM                  USING POSTSUM-PARM                   
035300      .                                                                   
035400      EJECT                                                               
035500 S99-ABEND SECTION.                                                       
035600                                                                          
035700       MOVE 'S' TO POSTSUM-OPKOD                                          
035800       CALL POSTSUM USING POSTSUM-PARM                                    
035900       CALL ABEND USING RKOD-ABEND                                        
036000       .                                                                  
