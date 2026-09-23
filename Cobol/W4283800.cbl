000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4283800.                                                
000300 AUTHOR.         SHILPA MADHURI G.                                        
000400 DATE-WRITTEN.   11/10/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700******************************************************************        
000800*                                                                *        
000900* FUNCTION:                                                      *        
001000*     THIS PROGRAM CREATES A FILE WHICH IS USED TO CREATE THE    *        
001100* 'DEALER RETURN DAILY FOLLOW UP' REPORT ON WEB.                 *        
001200* INPUT FILES ARE FROM THE PROGRAM W4283700                      *        
001300*                                                                *        
001400* E'TRACKER:         10143271 - CHINA WAREHOUSE PROJECT-1        *        
001500*                                                                *        
001600******************************************************************        
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200*          --- W42837 FILE                                                
002300       SELECT W42837                     ASSIGN TO W42838D1.              
002400*          --- W42838 FILE                                                
002500       SELECT W42838                     ASSIGN TO W42838D2.              
002600*          --- REPORT FILE                                                
002700       SELECT W428381                    ASSIGN TO W42838D3.              
002800       EJECT                                                              
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100 FD   W42837                                                              
003200       RECORDING       F                                                  
003300       BLOCK CONTAINS  0.                                                 
003400                                                                          
003500*01   -COPY W42837      -PRE W42837-                                      
003600 FD   W42838                                                              
003700       RECORDING       F                                                  
003800       BLOCK CONTAINS  0.                                                 
003900                                                                          
004000*01   -COPY W42838      -PRE W42838-                                      
004100 FD   W428381                                                             
004200       RECORDING       V                                                  
004300       BLOCK CONTAINS  0.                                                 
004400 01  UT-DAP-POST.                                                         
004500       03  FILLER         PIC X(21).                                      
004600*01   -COPY W428381       -PRE U1-                                        
004700       EJECT                                                              
004800*01   -COPY W428382       -PRE U2-                                        
004900       EJECT                                                              
005000*01   -COPY W428383       -PRE U3-                                        
005100       EJECT                                                              
005200                                                                          
005300                                                                          
005400 01   W42838-001-LINE             PIC X(121).                             
005500      EJECT                                                               
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800 77   IDPGM                       PIC X(8)    VALUE 'W4283800'.           
005900 77   YES                         PIC X       VALUE 'J'.                  
006000 77   NOO                         PIC X       VALUE 'N'.                  
006100 77   WS-CODE-COUNT               PIC 9(5)    VALUE ZEROS.                
006200 77   WS0-KVDAGDEC                PIC 9(4)V9  VALUE ZEROS.                
006300 77   WS1-KVDAGDEC                PIC 9(4)V9  VALUE ZEROS.                
006400 77   EDI-IDPTYP                  PIC X(3)    VALUE SPACE.                
006500 77   WS1-AVGLEAD-TIME            PIC 9(4)V9  VALUE ZEROS.                
006600 77   WS-TOT-DC                   PIC 9(2)    VALUE ZEROS.                
006700 77   WS-PREV-IDDC                PIC X(2)    VALUE SPACES.               
006800 77   WS-FILE2-IDDC               PIC X(2)    VALUE SPACES.               
006900 77   WS-PRFL2-IDDC               PIC X(2)    VALUE SPACES.               
007000 77   WS-PRSUR-DAY                PIC 9(9)    VALUE ZEROS.                
007100 77   WS-SURADER-DAY              PIC 9(9)    VALUE ZEROS.                
007200 77   WS-SUM-AVTIM                PIC 9(4)V9  VALUE ZEROS.                
007300                                                                          
007400 77   W428F1-EOF-SW               PIC X       VALUE 'N'.                  
007500      88  END-OF-W42837                       VALUE 'Y'.                  
007600      88  NO-EOF-W42837                       VALUE 'N'.                  
007700                                                                          
007800 77   W428F2-EOF-SW               PIC X       VALUE 'N'.                  
007900      88  END-OF-W42838                       VALUE 'Y'.                  
008000      88  NO-EOF-W42838                       VALUE 'N'.                  
008100                                                                          
008200 77   W001-HEADER-SW              PIC X       VALUE 'N'.                  
008300      88  HDR-WRITTEN                         VALUE 'Y'.                  
008400      88  HDR-NOTWRITTEN                      VALUE 'N'.                  
008500                                                                          
008600 77   W001-W42837-SW              PIC X       VALUE 'N'.                  
008700      88  W42837-EMPTY                        VALUE 'Y'.                  
008800      88  W42837-NOTEMPTY                     VALUE 'N'.                  
008900                                                                          
009000 77   W001-W42838-SW              PIC X       VALUE 'N'.                  
009100      88  W42838-EMPTY                        VALUE 'Y'.                  
009200      88  W42838-NOTEMPTY                     VALUE 'N'.                  
009300                                                                          
009400 77   W001-LINE2-STAT             PIC X       VALUE 'N'.                  
009500      88  LINE2-WRITE                         VALUE 'Y'.                  
009600      88  LINE2-WROTE                         VALUE 'N'.                  
009700                                                                          
009800 77   W001-LINE3-STAT             PIC X       VALUE 'N'.                  
009900      88  LINE3-WRITE                         VALUE 'Y'.                  
010000      88  LINE3-WROTE                         VALUE 'N'.                  
010100      EJECT                                                               
010200 01   TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                 
010300 01   FILLER REDEFINES TODAYS-DATE.                                       
010400      03  TODAYS-DATE-YEAR        PIC 9(2).                               
010500      03  TODAYS-DATE-MONTH       PIC 9(2).                               
010600      03  TODAYS-DATE-DAY         PIC 9(2).                               
010700      EJECT                                                               
010800 01   GENERAL-SUBPROGRAMS.                                                
010900*                                                                         
011000       03  ABEND                   PIC X(8)    VALUE 'ABEND'.             
011100       03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.           
011200       03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.           
011300       03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.          
011500*     TEMPORARY WORKING STORAGE VARIABLES USED                            
011600 01   WS-TEMP-STORAGE-AREA.                                               
011700       03  WS-TEMP-CODE            PIC X(2)    VALUE SPACES.              
011800       03  WS-TEMP-NOTBIN          PIC 9(9)    VALUE ZEROS.               
011900       03  WS-TEMP-BIN             PIC 9(6)    VALUE ZEROS.               
012000       03  WS-TEMP-DEV             PIC 9(7)    VALUE ZEROS.               
012100       03  WS-TEMP-SCRAP           PIC 9(6)    VALUE ZEROS.               
012200       03  WS-TEMP-QUAL            PIC 9(7)    VALUE ZEROS.               
012300       03  WS-TEMP-AVTIM           PIC 9(4)V9  VALUE ZEROS.               
012400 01   WS-TOTAL-STORAGE-AREA.                                              
012500       03  WS-TOT-CODE             PIC 9(2)    VALUE ZEROS.               
012600       03  WS-TOT-NOTBIN           PIC Z(8)9  .                           
012700       03  WS-TOT-BIN              PIC Z(5)9  .                           
012800       03  WS-TOT-DEV              PIC Z(6)9  .                           
012900       03  WS-TOT-SCRAP            PIC Z(5)9  .                           
013000       03  WS-TOT-QUAL             PIC Z(6)9  .                           
013100       03  WS-TOT-AVTIM            PIC Z(3)9.9.                           
013200*     --- PARAMETERS TO ABEND                                             
013300                                                                          
013400 77   RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.              
013500 77   RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.             
013600 77   RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.           
013700 01   ERROR-TEXT.                                                         
013800       03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.        
013900       03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.               
014000       EJECT                                                              
014100 01  DAP-AREA-TYPE.                                                       
014200       03  FILLER                  PIC X(15)   VALUE                      
014300           ' ¤DAPW42838-001'.                                             
014400 01  DAP-AREA-SUBTYPE.                                                    
014500       03  FILLER                  PIC X(5)   VALUE                       
014600        ' ¤DAP'.                                                          
014700       03  DAP-IDDC                PIC X(2) VALUE SPACES.                 
014800                                                                          
014900       EJECT                                                              
015000                                                                          
015100*     --- PARAMETERS TO DATKORT                                           
015200*                                                                         
015300 01   PROGRAM-NAME                PIC X(6)    VALUE 'W42838'.             
015400 01   DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.             
015500*01   -COPY WDATKORT                                                      
015600       EJECT                                                              
015700*     --- PARAMETRAR TILL POSTSUM                                         
015800*                                                                         
015900*01   -COPY W0005   -PRE  POSTSUM-                                        
016000       EJECT                                                              
016100*01   -COPY WDATAREA                                                      
016200       EJECT                                                              
016500 01   W428D1-AREA-START           PIC X(24)   VALUE                       
016600                                   'W428D1-AREA-START  '.                 
016700                                                                          
016800*01   AREA -COPY W42837     -PRE W428F1-                                  
016900       EJECT                                                              
017000 01   W428D2-AREA-START           PIC X(24)   VALUE                       
017100                                   'W428D2-AREA-START  '.                 
017200                                                                          
017300*01   AREA -COPY W42838     -PRE W428F2-                                  
017400       EJECT                                                              
017500 01   W001-LINE.                                                          
017600*                                                                         
017700       03  FILLER                  PIC X(121)  VALUE SPACE.               
017800       EJECT                                                              
017900 01   W001-HEADER1.                                                       
018000*                                                                         
018100       03  W001-IDDC               PIC X(2) VALUE SPACE.                  
018200       03  W001-DATE               PIC XXXXXX.                            
018300       EJECT                                                              
018400 01   W001-DETAIL1.                                                       
018500       03  W001-CODE               PIC X(2) VALUE SPACE.                  
018600       03  W001-NOT-BINNED         PIC 9(9).                              
018700       03  W001-BINNED             PIC 9(6) VALUE ZEROS.                  
018800       03  W001-DEVIATED           PIC 9(7) VALUE ZEROS.                  
018900       03  W001-SCRAPPED           PIC 9(6) VALUE ZEROS.                  
019000       03  W001-QUALITY            PIC 9(7) VALUE ZEROS.                  
019100       03  W001-AVGLEAD-TIME       PIC 9(4)V9 VALUE ZEROS.                
019200       EJECT                                                              
019300 01   W001-DETAIL2.                                                       
019400       03  W001-TOT-NOTBIN         PIC 9(9).                              
019500       03  W001-TOT-BINNED         PIC 9(6) VALUE ZEROS.                  
019600       03  W001-TOT-DEVIATED       PIC 9(7) VALUE ZEROS.                  
019700       03  W001-TOT-SCRAPPED       PIC 9(6) VALUE ZEROS.                  
019800       03  W001-TOT-QUALITY        PIC 9(7) VALUE ZEROS.                  
019900       03  W001-TOT-AVGLEAD-TIME   PIC 9(4)V9 VALUE ZEROS.                
020000       EJECT                                                              
020100 PROCEDURE DIVISION.                                                      
020200 MAIN SECTION.                                                            
020300                                                                          
020400        PERFORM A-INIT                                                    
020500        PERFORM S01-READ-W42837                                           
020600        PERFORM S02-READ-W42838                                           
020700                                                                          
020800        IF END-OF-W42837                                                  
020900          SET W42837-EMPTY TO TRUE                                        
021000        END-IF                                                            
021100        IF END-OF-W42838                                                  
021200          SET W42838-EMPTY TO TRUE                                        
021300        END-IF                                                            
021400                                                                          
021500        PERFORM UNTIL (END-OF-W42837 AND END-OF-W42838)                   
021600          PERFORM B-CHECK-FILES                                           
021700        END-PERFORM                                                       
021800                                                                          
021900        PERFORM E-AT-END-PROCESS                                          
022100        PERFORM Z-FINIT                                                   
022200                                                                          
022300        MOVE ZERO TO RETURN-CODE                                          
022400       GOBACK                                                             
022500       .                                                                  
022600       EJECT                                                              
022700 A-INIT SECTION.                                                          
022800                                                                          
022900       OPEN INPUT            W42837                                       
023000                             W42838                                       
023100                                                                          
023200       OPEN OUTPUT           W428381                                      
023300       CALL DATKORT   USING  PROGRAM-NAME DATECARD-ID DATUMKORT           
023400       MOVE D-AAR       TO   TODAYS-DATE-YEAR                             
023500       MOVE D-MAANAD    TO   TODAYS-DATE-MONTH                            
023600       MOVE D-DAG       TO   TODAYS-DATE-DAY                              
023700       MOVE IDPGM       TO   POSTSUM-PROGNAMN                             
023800       MOVE TODAYS-DATE TO   U1-DOC-TIAAMMDD                              
023900       MOVE SPACES      TO   WS-PREV-IDDC                                 
024000       INITIALIZE W001-DETAIL1 W001-DETAIL2 WS-SUM-AVTIM                  
024100       .                                                                  
024200       EJECT                                                              
024300 B-CHECK-FILES       SECTION.                                             
024400        IF NOT (END-OF-W42837 OR END-OF-W42838)                           
024600           PERFORM BA-COMPARE-IDDC UNTIL                                  
024700                (END-OF-W42837 OR END-OF-W42838)                          
024800        ELSE                                                              
024900         IF NOT END-OF-W42837                                             
025100            PERFORM UNTIL END-OF-W42837                                   
025200              PERFORM BB-PROCESS-W42837                                   
025300              PERFORM S01-READ-W42837                                     
025400            END-PERFORM                                                   
025500            PERFORM BC-WRITE-DETAIL1                                      
025600            PERFORM BD-DETAIL2-LOAD                                       
025700         ELSE                                                             
025800            IF NOT END-OF-W42838                                          
025900             IF NOT W42837-EMPTY                                          
026000              PERFORM BC-WRITE-DETAIL1                                    
026100              PERFORM BD-DETAIL2-LOAD                                     
026200             END-IF                                                       
026400             IF WS-PRFL2-IDDC > DAP-IDDC                                  
026500              MOVE WS-PRFL2-IDDC         TO   U1-DOC-IDDC DAP-IDDC        
026600              MOVE 'DAP'                 TO   EDI-IDPTYP                  
026700              WRITE UT-DAP-POST          FROM DAP-AREA-TYPE               
026800              WRITE UT-DAP-POST          FROM DAP-AREA-SUBTYPE            
026900              MOVE TODAYS-DATE           TO   U1-DOC-TIAAMMDD             
027000              INITIALIZE                      W001-LINE                   
027100              INITIALIZE                      U3-DOC-W428383              
027200              MOVE '1         '          TO   U1-DOC-IDAFPRCD             
027300              MOVE DAP-IDDC              TO   U1-DOC-IDDC                 
027400              MOVE U1-DOC-W428381        TO   W001-LINE                   
027500              WRITE W42838-001-LINE      FROM W001-LINE                   
027600              MOVE EDI-IDPTYP            TO   POSTSUM-TRANSTYP            
027700              MOVE 'W428D1'              TO   POSTSUM-FDNAMN              
027800              MOVE 'W42838D3'            TO   POSTSUM-DDNAMN2             
027900              CALL POSTSUM              USING POSTSUM-PARM                
028000              MOVE '3         '          TO   U3-DOC-IDAFPRCD             
028200              MOVE WS-PRSUR-DAY          TO U3-DOC-SURADER-DAY            
028300              MOVE ZEROS                 TO U3-DOC-KVRETINL               
028400              MOVE ZEROS                 TO U3-DOC-KVAVV-KVANT            
028500              MOVE ZEROS                 TO U3-DOC-KVRETINL-SKR           
028600              MOVE ZEROS                 TO U3-DOC-KVAVV-KVAL             
028700              MOVE ZEROS                 TO U3-DOC-KVDAGDEC               
028800              INITIALIZE W001-LINE                                        
028900              MOVE U3-DOC-W428383        TO W001-LINE                     
029000              PERFORM S21-WRITE-W42838-001                                
029100              ADD 1    TO WS-TOT-DC                                       
029200             END-IF                                                       
029300             PERFORM UNTIL END-OF-W42838                                  
029400              PERFORM S20-SKRIV-START-POST                                
029500              PERFORM S21A-WRITE-HEADERS                                  
029600              INITIALIZE                    U3-DOC-W428383                
029700              MOVE '3         '          TO U3-DOC-IDAFPRCD               
029900              INITIALIZE                    U3-DOC-SURADER-DAY            
030000              IF DAP-IDDC = WS-FILE2-IDDC                                 
030100               MOVE WS-SURADER-DAY       TO U3-DOC-SURADER-DAY            
030300              END-IF                                                      
030400              INITIALIZE W001-LINE                                        
030500              MOVE U3-DOC-W428383        TO W001-LINE                     
030600              PERFORM S21-WRITE-W42838-001                                
030700              ADD 1                      TO WS-TOT-DC                     
030800              INITIALIZE     U3-DOC-W428383 WS-SUM-AVTIM                  
030900              PERFORM S02-READ-W42838                                     
031000             END-PERFORM                                                  
031100           END-IF                                                         
031200          END-IF                                                          
031300        END-IF                                                            
031400        .                                                                 
031500        EJECT                                                             
031600                                                                          
031700 BA-COMPARE-IDDC     SECTION.                                             
031800                                                                          
031900     PERFORM UNTIL END-OF-W42837 OR END-OF-W42838                         
032200     IF W428F1-IDDC-RET = W428F2-IDDC-RET                                 
032300      PERFORM UNTIL W428F1-IDDC-RET NOT = W428F2-IDDC-RET                 
032400       PERFORM BB-PROCESS-W42837                                          
032500       PERFORM S01-READ-W42837                                            
032700      END-PERFORM                                                         
032800       IF W428F1-IDDC-RET     NOT = WS-PREV-IDDC                          
032900         PERFORM CC-ENDIDDC-W42837                                        
033000         PERFORM BD-DETAIL2-LOAD                                          
033100         PERFORM S02-READ-W42838                                          
033200       END-IF                                                             
033300     ELSE                                                                 
033400       IF W428F1-IDDC-RET <= W428F2-IDDC-RET                              
033500         PERFORM UNTIL (W428F1-IDDC-RET > W428F2-IDDC-RET OR              
033600                (END-OF-W42837 OR END-OF-W42838))                         
033700          PERFORM BB-PROCESS-W42837                                       
033800          PERFORM S01-READ-W42837                                         
033900         END-PERFORM                                                      
034000         MOVE WS-SURADER-DAY   TO WS-PRSUR-DAY                            
034100         MOVE WS-FILE2-IDDC    TO WS-PRFL2-IDDC                           
034300         IF NOT END-OF-W42838                                             
034400          IF W428F1-IDDC-RET > W428F2-IDDC-RET                            
034500           PERFORM S02-READ-W42838                                        
034600          END-IF                                                          
034700         END-IF                                                           
034800       ELSE                                                               
034900            PERFORM BC-WRITE-DETAIL1                                      
035000            PERFORM BD-DETAIL2-LOAD                                       
035100           IF W428F2-IDDC-RET NOT = DAP-IDDC                              
035200              PERFORM S20-SKRIV-START-POST                                
035300              PERFORM S21A-WRITE-HEADERS                                  
035400           END-IF                                                         
035500            INITIALIZE U3-DOC-SURADER-DAY                                 
035600           IF WS-FILE2-IDDC = DAP-IDDC                                    
035700            MOVE WS-SURADER-DAY   TO U3-DOC-SURADER-DAY                   
035900            INITIALIZE WS-SURADER-DAY                                     
036000           END-IF                                                         
036100              MOVE ZEROS                 TO U3-DOC-KVRETINL               
036200              MOVE ZEROS                 TO U3-DOC-KVAVV-KVANT            
036300              MOVE ZEROS                 TO U3-DOC-KVRETINL-SKR           
036400              MOVE ZEROS                 TO U3-DOC-KVAVV-KVAL             
036500              MOVE ZEROS                 TO U3-DOC-KVDAGDEC               
036600              MOVE '3         '          TO U3-DOC-IDAFPRCD               
036800            INITIALIZE                      W001-LINE                     
036900            IF LINE3-WRITE                                                
037000            MOVE U3-DOC-W428383          TO W001-LINE                     
037100            PERFORM S21-WRITE-W42838-001                                  
037200            ADD 1                        TO WS-TOT-DC                     
037300            END-IF                                                        
037400            INITIALIZE      U3-DOC-W428383 WS-SUM-AVTIM                   
037500            PERFORM S02-READ-W42838                                       
037600            MOVE W428F1-IDDC-RET         TO WS-PREV-IDDC                  
037700           MOVE W428F1-IDDC-RET          TO U1-DOC-IDDC                   
037800         IF NOT END-OF-W42837                                             
037900           PERFORM S20-SKRIV-START-POST                                   
038000           PERFORM S21A-WRITE-HEADERS                                     
038100         END-IF                                                           
038200       END-IF                                                             
038300     END-IF                                                               
038400     END-PERFORM                                                          
038500     .                                                                    
038600     EJECT                                                                
038700 BB-PROCESS-W42837   SECTION.                                             
038800     IF NOT END-OF-W42837                                                 
039000       IF W428F1-IDDC-RET = WS-PREV-IDDC                                  
039100         MOVE W428F1-IDDC-RET TO WS-PREV-IDDC                             
039200         PERFORM BBA-DETAIL1-LOAD                                         
039300       ELSE                                                               
039400         IF WS-PREV-IDDC = SPACES                                         
039600           MOVE W428F1-IDDC-RET TO U1-DOC-IDDC                            
039700           MOVE W428F1-IDDC-RET TO WS-PREV-IDDC                           
039800           PERFORM S20-SKRIV-START-POST                                   
039900          PERFORM S21A-WRITE-HEADERS                                      
040000          IF W428F1-IDDC-RET NOT EQUAL TO SPACES                          
040100           MOVE W428F1-IDDC-RET TO WS-PREV-IDDC                           
040200           PERFORM BBA-DETAIL1-LOAD                                       
040300          END-IF                                                          
040400         ELSE                                                             
040500           PERFORM CC-ENDIDDC-W42837                                      
040600           PERFORM BD-DETAIL2-LOAD                                        
040700           MOVE W428F1-IDDC-RET TO WS-PREV-IDDC                           
040800           MOVE W428F1-IDDC-RET TO U1-DOC-IDDC                            
040900         IF NOT END-OF-W42837                                             
041000           PERFORM S20-SKRIV-START-POST                                   
041100           PERFORM S21A-WRITE-HEADERS                                     
041200           MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS          
041300                                       WS-TEMP-CODE                       
041400         END-IF                                                           
041500         END-IF                                                           
041600       END-IF                                                             
041700       END-IF                                                             
041800       .                                                                  
041900       EJECT                                                              
042000 BBA-DETAIL1-LOAD    SECTION.                                             
042100       IF  W001-CODE = W428F1-KDANMORS                                    
042200         MOVE W428F1-KVRETINL     TO W001-BINNED                          
042400         ADD W001-BINNED          TO WS-TEMP-BIN                          
042500         MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                        
042600         ADD W001-DEVIATED        TO WS-TEMP-DEV                          
042700         MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                        
042800         ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                        
042900         MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                         
043000         ADD W001-QUALITY         TO WS-TEMP-QUAL                         
043100         ADD W428F1-KVDAGAR-INL   TO WS-TEMP-AVTIM                        
043200         MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS            
043300                                     WS-TEMP-CODE                         
043500       ELSE                                                               
043600        IF W001-CODE = SPACES                                             
043700          MOVE W428F1-KVRETINL     TO W001-BINNED                         
043800          ADD W001-BINNED          TO WS-TEMP-BIN                         
044100          MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                       
044200          ADD W001-DEVIATED        TO WS-TEMP-DEV                         
044300          MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                       
044400          ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                       
044500          MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                        
044600          ADD W001-QUALITY         TO WS-TEMP-QUAL                        
044700          ADD W428F1-KVDAGAR-INL   TO WS-TEMP-AVTIM                       
044800          MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS           
044900                                     WS-TEMP-CODE                         
045100        ELSE                                                              
045200         IF W001-CODE NOT = W428F1-KDANMORS                               
045300         MOVE WS-TEMP-CODE        TO U2-DOC-KDANMORS                      
045400         MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                      
045500         MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                   
045600         MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                  
045700         MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                    
045800         ADD  WS-TEMP-BIN         TO W001-TOT-BINNED                      
045900         ADD  WS-TEMP-DEV         TO W001-TOT-DEVIATED                    
046000         ADD  WS-TEMP-SCRAP       TO W001-TOT-SCRAPPED                    
046100         ADD  WS-TEMP-QUAL        TO W001-TOT-QUALITY                     
046200         COMPUTE WS-CODE-COUNT = WS-TEMP-BIN + WS-TEMP-SCRAP              
046300          IF WS-CODE-COUNT        NOT = 0                                 
046400           COMPUTE WS0-KVDAGDEC  = (WS-TEMP-AVTIM / WS-CODE-COUNT)        
046600          END-IF                                                          
046700         ADD  WS-TEMP-AVTIM       TO WS-SUM-AVTIM                         
046800         MOVE WS0-KVDAGDEC        TO U2-DOC-KVDAGDEC                      
046900         MOVE '2          '       TO U2-DOC-IDAFPRCD                      
047100         INITIALIZE W001-LINE                                             
047200         MOVE U2-DOC-W428382      TO W001-LINE                            
047300         PERFORM S21-WRITE-W42838-001                                     
047400         INITIALIZE                  U2-DOC-W428382                       
047500         INITIALIZE                  WS-TEMP-STORAGE-AREA                 
047600                                     WS-CODE-COUNT                        
047700                                     WS0-KVDAGDEC                         
047800                                     W001-DETAIL1                         
047900         MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS            
048000                                     WS-TEMP-CODE                         
048100         MOVE W428F1-KVRETINL     TO W001-BINNED                          
048300         COMPUTE WS-TEMP-BIN = WS-TEMP-BIN + W428F1-KVRETINL              
048400         MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                        
048500         ADD W001-DEVIATED        TO WS-TEMP-DEV                          
048600         MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                        
048700         ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                        
048800         MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                         
048900         ADD W001-QUALITY         TO WS-TEMP-QUAL                         
049000         ADD W428F1-KVDAGAR-INL   TO WS-TEMP-AVTIM                        
049100       END-IF                                                             
049200       END-IF                                                             
049300       END-IF                                                             
049400       .                                                                  
049500       EJECT                                                              
049600 CC-ENDIDDC-W42837   SECTION.                                             
049700                                                                          
049900         MOVE WS-TEMP-CODE        TO U2-DOC-KDANMORS                      
050100         MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                      
050200         MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                   
050300         MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                  
050400         MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                    
050500         COMPUTE WS-CODE-COUNT = WS-TEMP-BIN + WS-TEMP-SCRAP              
050600         IF WS-CODE-COUNT NOT = 0                                         
050700          COMPUTE WS1-KVDAGDEC  = (WS-TEMP-AVTIM / WS-CODE-COUNT)         
051000         END-IF                                                           
051100         ADD  WS-TEMP-AVTIM       TO WS-SUM-AVTIM                         
051200         MOVE WS1-KVDAGDEC        TO U2-DOC-KVDAGDEC                      
051300         ADD  WS-TEMP-BIN         TO W001-TOT-BINNED                      
051400         ADD  WS-TEMP-DEV         TO W001-TOT-DEVIATED                    
051500         ADD  WS-TEMP-QUAL        TO W001-TOT-QUALITY                     
051600         ADD  WS-TEMP-SCRAP       TO W001-TOT-SCRAPPED                    
051700         MOVE '2         '        TO U2-DOC-IDAFPRCD                      
051900         INITIALIZE W001-LINE                                             
052000         IF LINE2-WRITE                                                   
052100          MOVE U2-DOC-W428382     TO W001-LINE                            
052200          PERFORM S21-WRITE-W42838-001                                    
052300          SET LINE2-WROTE TO TRUE                                         
052400         END-IF                                                           
052500         INITIALIZE               U2-DOC-W428382                          
052600         INITIALIZE WS-TEMP-STORAGE-AREA WS-CODE-COUNT                    
052700         INITIALIZE W001-DETAIL1         WS1-KVDAGDEC                     
052800         IF NOT END-OF-W42837                                             
052900          MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS           
053000                                     WS-TEMP-CODE                         
053100          MOVE W428F1-KVRETINL     TO W001-BINNED                         
053200          COMPUTE WS-TEMP-BIN = WS-TEMP-BIN + W428F1-KVRETINL             
053400          MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                       
053500          ADD W001-DEVIATED        TO WS-TEMP-DEV                         
053600          MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                       
053700          ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                       
053800          MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                        
053900          ADD W001-QUALITY         TO WS-TEMP-QUAL                        
054000          ADD W428F1-KVDAGAR-INL   TO WS-TEMP-AVTIM                       
054100         END-IF                                                           
054200       .                                                                  
054300       EJECT                                                              
054400 BD-DETAIL2-LOAD     SECTION.                                             
054600       INITIALIZE U3-DOC-SURADER-DAY                                      
054800       IF WS-FILE2-IDDC = DAP-IDDC                                        
054900        MOVE WS-SURADER-DAY        TO U3-DOC-SURADER-DAY                  
055100        INITIALIZE WS-SURADER-DAY                                         
055200       ELSE                                                               
055300        IF WS-PRFL2-IDDC = DAP-IDDC                                       
055400         MOVE WS-PRSUR-DAY         TO U3-DOC-SURADER-DAY                  
055600         INITIALIZE WS-PRSUR-DAY                                          
055700        END-IF                                                            
055800       END-IF                                                             
055900       MOVE W001-TOT-BINNED       TO U3-DOC-KVRETINL                      
056000       MOVE W001-TOT-DEVIATED     TO U3-DOC-KVAVV-KVANT                   
056100       MOVE W001-TOT-SCRAPPED     TO U3-DOC-KVRETINL-SKR                  
056200       MOVE W001-TOT-QUALITY      TO U3-DOC-KVAVV-KVAL                    
056300       COMPUTE WS-TOT-CODE = W001-TOT-BINNED + W001-TOT-SCRAPPED          
056400       IF WS-TOT-CODE NOT = 0                                             
056500        COMPUTE WS1-AVGLEAD-TIME =                                        
056600                         WS-SUM-AVTIM / WS-TOT-CODE                       
056800       END-IF                                                             
056900       MOVE WS1-AVGLEAD-TIME    TO U3-DOC-KVDAGDEC                        
057000       MOVE '3         '        TO U3-DOC-IDAFPRCD                        
057200       INITIALIZE W001-LINE                                               
057300       IF LINE3-WRITE                                                     
057400        MOVE U3-DOC-W428383         TO W001-LINE                          
057500        PERFORM S21-WRITE-W42838-001                                      
057600        SET LINE3-WROTE TO TRUE                                           
057700       END-IF                                                             
057800       INITIALIZE          U3-DOC-W428383                                 
057900       ADD 1    TO WS-TOT-DC                                              
058000       INITIALIZE  WS-TOT-CODE W001-CODE                                  
058100                   W001-DETAIL2 WS-SUM-AVTIM                              
058200       .                                                                  
058300       EJECT                                                              
058400 BC-WRITE-DETAIL1    SECTION.                                             
058500        IF WS-TEMP-CODE NOT EQUAL TO SPACES                               
058600         MOVE WS-TEMP-CODE        TO U2-DOC-KDANMORS                      
058800         MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                      
058900         MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                   
059000         MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                  
059100         MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                    
059200         ADD  WS-TEMP-BIN         TO W001-TOT-BINNED                      
059300         ADD  WS-TEMP-DEV         TO W001-TOT-DEVIATED                    
059400         ADD  WS-TEMP-QUAL        TO W001-TOT-QUALITY                     
059500         ADD  WS-TEMP-SCRAP       TO W001-TOT-SCRAPPED                    
059600         COMPUTE WS-CODE-COUNT = WS-TEMP-BIN + WS-TEMP-SCRAP              
059700         IF WS-CODE-COUNT NOT EQUAL TO ZERO                               
059800          COMPUTE WS0-KVDAGDEC  = (WS-TEMP-AVTIM / WS-CODE-COUNT)         
060000         END-IF                                                           
060100         ADD  WS-TEMP-AVTIM       TO WS-SUM-AVTIM                         
060200         MOVE WS0-KVDAGDEC        TO U2-DOC-KVDAGDEC                      
060300         MOVE '2         '        TO U2-DOC-IDAFPRCD                      
060500         INITIALIZE W001-LINE                                             
060600         IF LINE2-WRITE                                                   
060700          MOVE U2-DOC-W428382     TO W001-LINE                            
060800          PERFORM S21-WRITE-W42838-001                                    
060900         END-IF                                                           
061000         INITIALIZE               U2-DOC-W428382                          
061100         INITIALIZE               WS-TEMP-STORAGE-AREA                    
061200                                  WS-CODE-COUNT                           
061300                                  WS0-KVDAGDEC                            
061400                                  W001-DETAIL1                            
061500        END-IF                                                            
061600         .                                                                
061700         EJECT                                                            
061800 E-AT-END-PROCESS      SECTION.                                           
061900                                                                          
062000     IF WS-FILE2-IDDC >= WS-PREV-IDDC                                     
062100       IF WS-TEMP-CODE NOT EQUAL TO SPACES                                
062300        PERFORM BC-WRITE-DETAIL1                                          
062400        PERFORM BD-DETAIL2-LOAD                                           
062500       END-IF                                                             
062600     END-IF                                                               
062700     IF WS-PREV-IDDC < WS-FILE2-IDDC                                      
062800       IF DAP-IDDC NOT = WS-FILE2-IDDC                                    
062900        MOVE WS-FILE2-IDDC              TO   U1-DOC-IDDC DAP-IDDC         
063000        MOVE 'DAP'                      TO   EDI-IDPTYP                   
063100        WRITE UT-DAP-POST               FROM DAP-AREA-TYPE                
063200        WRITE UT-DAP-POST               FROM DAP-AREA-SUBTYPE             
063300        MOVE TODAYS-DATE                TO   U1-DOC-TIAAMMDD              
063400        INITIALIZE                           W001-LINE                    
063500        INITIALIZE                           U3-DOC-W428383               
063600        MOVE '1         '               TO   U1-DOC-IDAFPRCD              
063700        MOVE DAP-IDDC                   TO   U1-DOC-IDDC                  
063800        MOVE U1-DOC-W428381             TO   W001-LINE                    
063900        WRITE W42838-001-LINE           FROM W001-LINE                    
064000        MOVE EDI-IDPTYP                 TO   POSTSUM-TRANSTYP             
064100        MOVE 'W428381'                  TO   POSTSUM-FDNAMN               
064200        MOVE 'W42838D3'                 TO   POSTSUM-DDNAMN2              
064300        CALL POSTSUM                 USING   POSTSUM-PARM                 
              MOVE '3         '        TO   U3-DOC-IDAFPRCD                     
              MOVE WS-SURADER-DAY      TO U3-DOC-SURADER-DAY                    
              MOVE ZEROS               TO U3-DOC-KVRETINL                       
              MOVE ZEROS               TO U3-DOC-KVAVV-KVANT                    
              MOVE ZEROS               TO U3-DOC-KVRETINL-SKR                   
              MOVE ZEROS               TO U3-DOC-KVAVV-KVAL                     
              MOVE ZEROS               TO U3-DOC-KVDAGDEC                       
              INITIALIZE                  W001-LINE                             
              MOVE U3-DOC-W428383      TO W001-LINE                             
              PERFORM S21-WRITE-W42838-001                                      
              ADD 1                    TO WS-TOT-DC                             
065600       END-IF                                                             
065700     END-IF                                                               
065800     .                                                                    
065900     EJECT                                                                
066000 S21-WRITE-W42838-001  SECTION.                                           
066100                                                                          
066200       WRITE W42838-001-LINE FROM W001-LINE                               
066300       INITIALIZE                 W001-LINE                               
066400       .                                                                  
066500       EJECT                                                              
066600 S21A-WRITE-HEADERS SECTION.                                              
066700                                                                          
066800       MOVE '1         '     TO U1-DOC-IDAFPRCD                           
067000       MOVE TODAYS-DATE      TO U1-DOC-TIAAMMDD                           
067100       MOVE W001-IDDC        TO U1-DOC-IDDC                               
067300       INITIALIZE               W001-LINE                                 
067400       MOVE U1-DOC-W428381   TO W001-LINE                                 
067600       WRITE W42838-001-LINE FROM W001-LINE                               
067700       SET HDR-WRITTEN       TO TRUE                                      
067800       INITIALIZE               U1-DOC-W428381                            
067900       SET LINE2-WRITE       TO TRUE                                      
068000       SET LINE3-WRITE       TO TRUE                                      
068100       .                                                                  
068200       EJECT                                                              
068300 S20-SKRIV-START-POST SECTION.                                            
068400*                                                                         
068500       WRITE UT-DAP-POST      FROM DAP-AREA-TYPE                          
068600       MOVE 'DAP'             TO   EDI-IDPTYP                             
068700                                                                          
068800       MOVE EDI-IDPTYP       TO POSTSUM-TRANSTYP                          
068900       MOVE 'W428381'        TO POSTSUM-FDNAMN                            
069000       MOVE 'W42838D3'       TO POSTSUM-DDNAMN2                           
069100       CALL POSTSUM       USING POSTSUM-PARM                              
069200      IF END-OF-W42837                                                    
069300       MOVE  W428F2-IDDC-RET TO DAP-IDDC                                  
069400                                W001-IDDC                                 
069500                                U1-DOC-IDDC                               
069700      ELSE                                                                
069800       MOVE  W428F1-IDDC-RET TO DAP-IDDC                                  
069900                                W001-IDDC                                 
070000                                U1-DOC-IDDC                               
070200      END-IF                                                              
070300      IF (NO-EOF-W42837 AND NO-EOF-W42838)                                
070400       IF W428F1-IDDC-RET < W428F2-IDDC-RET                               
070500        MOVE  W428F1-IDDC-RET            TO DAP-IDDC                      
070600                                            W001-IDDC                     
070700                                            U1-DOC-IDDC                   
070900       ELSE                                                               
071000        MOVE  W428F2-IDDC-RET            TO DAP-IDDC                      
071100                                            W001-IDDC                     
071200                                            U1-DOC-IDDC                   
071400       END-IF                                                             
071500      END-IF                                                              
071700       WRITE UT-DAP-POST               FROM DAP-AREA-SUBTYPE              
071800       MOVE 'DAP'                      TO   EDI-IDPTYP                    
071900       MOVE TODAYS-DATE                TO U1-DOC-TIAAMMDD                 
072000       MOVE EDI-IDPTYP                 TO POSTSUM-TRANSTYP                
072100       MOVE 'W428381'                  TO POSTSUM-FDNAMN                  
072200       MOVE 'W42838D3'                 TO POSTSUM-DDNAMN2                 
072300       CALL POSTSUM                 USING POSTSUM-PARM                    
072400      .                                                                   
072500      EJECT                                                               
072600 Z-FINIT SECTION.                                                         
072700       CLOSE                 W42837                                       
072800                             W42838                                       
072900                             W428381                                      
073300       DISPLAY '************************************'                     
073100       DISPLAY '---------SUMMARY OF THE REPORT------'                     
073200       DISPLAY 'TOTAL NO OF DC REPORTS GENERATED =' WS-TOT-DC             
073300       DISPLAY '************************************'                     
073400       MOVE 'S'           TO POSTSUM-OPKOD                                
073500       CALL POSTSUM    USING POSTSUM-PARM                                 
073600       .                                                                  
073700       EJECT                                                              
073800 S01-READ-W42837  SECTION.                                                
073900       READ W42837       INTO W428F1-AREA                                 
074000       AT END                                                             
074100         MOVE HIGH-VALUE   TO W428F1-AREA                                 
074200         DISPLAY 'EOF1 REACHED'                                           
074300         SET END-OF-W42837 TO TRUE                                        
074400                                                                          
074500       NOT AT END                                                         
074700         MOVE 'W42837'     TO POSTSUM-FDNAMN                              
074800         MOVE 'W42838D1'   TO POSTSUM-DDNAMN2                             
074900         MOVE SPACES       TO POSTSUM-TRANSTYP                            
075000         CALL POSTSUM   USING POSTSUM-PARM                                
075100         SET  W42837-NOTEMPTY TO TRUE                                     
075200       END-READ                                                           
075300       .                                                                  
075400       EJECT                                                              
075500 S02-READ-W42838  SECTION.                                                
075600       READ W42838 INTO W428F2-AREA                                       
075700       AT END                                                             
075800          MOVE HIGH-VALUE   TO W428F2-AREA                                
075900          SET END-OF-W42838 TO TRUE                                       
076000         DISPLAY 'EOF2 REACHED'                                           
076100                                                                          
076200       NOT AT END                                                         
076300          MOVE 'W42838'     TO POSTSUM-FDNAMN                             
076400          MOVE 'W42838D2'   TO POSTSUM-DDNAMN2                            
076500          MOVE SPACES       TO  POSTSUM-TRANSTYP                          
076600          CALL POSTSUM    USING POSTSUM-PARM                              
076800            MOVE W428F2-SURADER  TO WS-SURADER-DAY                        
076900            MOVE W428F2-IDDC-RET TO WS-FILE2-IDDC                         
077100         SET  W42838-NOTEMPTY    TO TRUE                                  
077200       END-READ                                                           
077300       .                                                                  
077400       EJECT                                                              
077500 S99-ABEND SECTION.                                                       
077600                                                                          
077700       MOVE 'S'                        TO POSTSUM-OPKOD                   
077800       CALL POSTSUM                 USING POSTSUM-PARM                    
077900       CALL ABEND                   USING RKOD-ABEND                      
078000       .                                                                  
