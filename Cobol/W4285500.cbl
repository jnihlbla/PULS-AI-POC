000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4285500.                                                
000300 AUTHOR.         SHILPA MADHURI G.                                        
000400 DATE-WRITTEN.   11/11/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700******************************************************************        
000800*                                                                *        
000900* FUNCTION:                                                      *        
001000*     THIS PROGRAM CREATES A FILE WHICH IS USED TO CREATE THE    *        
001100* 'MANAGEMENT DEALER PERIODIC FOLLOW UP' REPORT ON WEB           *        
001200* INPUT FILES ARE FROM THE PROGRAM W4285700                      *        
001300*                                                                *        
001400* E'TRACKER:         10143271 - CHINA WAREHOUSE PROJECT-1        *        
001500* E'TRACKER: 121105  10181786 - WEB AUSTRALIEN FOLLOW-UP         *        
001600*                                                                *        
001700******************************************************************        
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300*          --- W4285A FILE                                                
002400       SELECT W4285A                     ASSIGN TO W42855D1.              
002500*          --- W4285B FILE                                                
002600       SELECT W4285B                     ASSIGN TO W42855D2.              
002700*          --- REPORT FILE                                                
002800       SELECT W428551                    ASSIGN TO W42855D3.              
002900       EJECT                                                              
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200 FD   W4285A                                                              
003300       RECORDING       F                                                  
003400       BLOCK CONTAINS  0.                                                 
003500                                                                          
003600*01   -COPY W42850      -PRE W4285A-                                      
003700 FD   W4285B                                                              
003800       RECORDING       F                                                  
003900       BLOCK CONTAINS  0.                                                 
004000                                                                          
004100*01   -COPY W42851      -PRE W4285B-                                      
004200 FD   W428551                                                             
004300       RECORDING       V                                                  
004400       BLOCK CONTAINS  0.                                                 
004500 01  UT-DAP-POST.                                                         
004600       03  FILLER         PIC X(21).                                      
004700*01   -COPY W428551       -PRE U1-                                        
004800       EJECT                                                              
004900*01   -COPY W428552       -PRE U2-                                        
005000       EJECT                                                              
005100*01   -COPY W428553       -PRE U3-                                        
005200       EJECT                                                              
005300                                                                          
005400                                                                          
005500 01   W42855-001-LINE             PIC X(121).                             
005600      EJECT                                                               
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900 77   IDPGM                       PIC X(8)    VALUE 'W4285500'.           
006000 77   CNTR                        PIC 9(3)    VALUE 0.                    
006100 77   YES                         PIC X       VALUE 'J'.                  
006200 77   NOO                         PIC X       VALUE 'N'.                  
006300 77   SUB-IX                      PIC 9(4)    VALUE ZERO.                 
006400 77   WS-CODE-COUNT               PIC 9(5)    VALUE ZEROS.                
006500 77   WS0-KVDAGDEC                PIC 9(4)V9  VALUE ZEROS.                
006600 77   WS1-KVDAGDEC                PIC 9(4)V9  VALUE ZEROS.                
006700 77   EDI-IDPTYP                  PIC X(3)    VALUE SPACE.                
006800 77   WS1-AVGLEAD-TIME            PIC 9(4)V9  VALUE ZEROS.                
006900 77   WS-TOT-DC                   PIC 9(2).                               
007000 77   WS-PREV-IDDC                PIC X(2)    VALUE SPACES.               
007100 77   WS-PREVF1-IDDC              PIC X(2)    VALUE SPACES.               
007200 77   WS-SEARCH-IDDC              PIC X(2)    VALUE SPACES.               
007300 77   WS-PREV-ADCITY              PIC X(20)   VALUE SPACES.               
007400 77   WS-SUM-AVTIM                PIC 9(4)V9  VALUE ZEROS.                
007500 77   WS-DAP-EU                   PIC X(2)    VALUE 'EU'.                 
007600 77   WS-DAP-CN                   PIC X(2)    VALUE 'CN'.                 
007800 77   DAP-KDMFUP                  PIC X(2)    VALUE SPACES.               
007900 77   WS-DAP-IDFTG                PIC 9(2)    VALUE 0.                    
008000 77   FELTEXT                     PIC X(80)   VALUE SPACE.                
008100                                                                          
008200 77   W428F1-EOF-SW               PIC X       VALUE 'N'.                  
008300      88  END-OF-W4285A                       VALUE 'Y'.                  
008400      88  NO-EOF-W4285A                       VALUE 'N'.                  
008500                                                                          
008600 77   W428F2-EOF-SW               PIC X       VALUE 'N'.                  
008700      88  END-OF-W4285B                       VALUE 'Y'.                  
008800      88  NO-EOF-W4285B                       VALUE 'N'.                  
008900                                                                          
009000 77   W001-HEADER-SW              PIC X       VALUE 'N'.                  
009100      88  HDR-WRITTEN                         VALUE 'Y'.                  
009200      88  HDR-NOTWRITTEN                      VALUE 'N'.                  
009300                                                                          
009400 77   W001-W4285A-SW              PIC X       VALUE 'N'.                  
009500      88  W4285A-EMPTY                        VALUE 'Y'.                  
009600      88  W4285A-NOTEMPTY                     VALUE 'N'.                  
009700                                                                          
009800 77   W001-W4285B-SW              PIC X       VALUE 'N'.                  
009900      88  W4285B-EMPTY                        VALUE 'Y'.                  
010000      88  W4285B-NOTEMPTY                     VALUE 'N'.                  
010100                                                                          
010200 77   W001-LINE2-STAT             PIC X       VALUE 'N'.                  
010300      88  LINE2-WRITE                         VALUE 'Y'.                  
010400      88  LINE2-WROTE                         VALUE 'N'.                  
010500                                                                          
010600 77   W001-LINE3-STAT             PIC X       VALUE 'N'.                  
010700      88  LINE3-WRITE                         VALUE 'Y'.                  
010800      88  LINE3-WROTE                         VALUE 'N'.                  
010900      EJECT                                                               
011000*     ARRAY DECLARATION FOR W4285B FILE                                   
011100 01   W4285B-NOT-BINNED-TABLE.                                            
011200      03  W4285B-LINE OCCURS 1000 TIMES                                   
011300                      INDEXED BY IX.                                      
011400         05  TAB-KDMFUP           PIC X(2).                               
011500         05  TAB-IDDC-RET         PIC X(2).                               
011600         05  TAB-IDARTNR          PIC 9(8).                               
011700         05  TAB-KVLEVANM-BEKR    PIC 9(6).                               
011800         05  TAB-SURADER          PIC 9(9).                               
011900         05  TAB-SUARTSTD         PIC 9(8)V9(2).                          
012000                                                                          
012100 01  GODK-STATUSKODER.                                                    
012200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012500 01  STATUS-WS                   PIC XX.                                  
012600       88  SEGMENT-FINNS                       VALUE '  '.                
012700       88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                
012800       88  SEGMENT-SAKNAS                      VALUE 'GE'.                
012900       SKIP2                                                              
013000 01  NYCKLAR-TILL-DLI.                                                    
013100     03  W-IDDC-X.                                                        
013200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013300 01   GENERAL-SUBPROGRAMS.                                                
013400*                                                                         
013500      03  ABEND                   PIC X(8)    VALUE 'ABEND   '.           
013600      03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.           
013700      03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.           
013800      03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.           
013900      03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.           
014000      03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.           
014100*     TEMPORARY WORKING STORAGE VARIABLES USED                            
014200 01   WS-TEMP-STORAGE-AREA.                                               
014300       03  WS-TEMP-CODE            PIC X(2)    VALUE SPACES.              
014400       03  WS-TEMP-NOTBIN          PIC 9(9)    VALUE ZEROS.               
014500       03  WS-TEMP-BIN             PIC 9(6)    VALUE ZEROS.               
014600       03  WS-TEMP-DEV             PIC 9(7)    VALUE ZEROS.               
014700       03  WS-TEMP-SCRAP           PIC 9(6)    VALUE ZEROS.               
014800       03  WS-TEMP-QUAL            PIC 9(7)    VALUE ZEROS.               
014900       03  WS-TEMP-AVTIM           PIC 9(4)V9  VALUE ZEROS.               
015000       03  WS-TEMP-SUARTSTD        PIC 9(8)V9(2) VALUE ZEROS.             
015100                                                                          
015200 01   WS-TOTAL-STORAGE-AREA.                                              
015300       03  WS-TOT-CODE             PIC 9(5)    VALUE ZEROS.               
015400*     --- PARAMETERS TO ABEND                                             
015500                                                                          
015600 77   RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.              
015700 77   RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.             
015800 77   RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.           
015900 01   ERROR-TEXT.                                                         
016000       03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.        
016100       03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.               
016200       EJECT                                                              
016300 01  DAP-AREA-TYPE.                                                       
016400       03  FILLER                  PIC X(15)   VALUE                      
016500           ' ¤DAPW42855-001'.                                             
016600 01  DAP-AREA-SUBTYPE.                                                    
016700       03  FILLER                  PIC X(5)   VALUE                       
016800        ' ¤DAP'.                                                          
016900       03  DAP-ACODE               PIC X(2) VALUE SPACES.                 
017000       03  DAP-DATE                PIC X(4) VALUE SPACES.                 
017100                                                                          
017200       EJECT                                                              
017300                                                                          
017400 01   TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                 
017500 01   FILLER REDEFINES TODAYS-DATE.                                       
017600      03  TODAYS-DATE-YEAR        PIC 9(2).                               
017700      03  TODAYS-DATE-MONTH       PIC 9(2).                               
017800      03  TODAYS-DATE-DAY         PIC 9(2).                               
017900      EJECT                                                               
018000*     --- PARAMETERS TO DATKORT                                           
018100*                                                                         
018200 01   PROGRAM-NAME                PIC X(6)    VALUE 'W42855'.             
018300 01   DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.             
018400*01   -COPY WDATKORT                                                      
018500*                                                                         
018600*01   -COPY W0005   -PRE  POSTSUM-                                        
018700       EJECT                                                              
018800*01   -COPY WDATAREA                                                      
018900       EJECT                                                              
019200*01   -COPY W0003                                                         
019300       EJECT                                                              
019400 01   W428D1-AREA-START           PIC X(24)   VALUE                       
019500                                   'W428D1-AREA-START  '.                 
019600                                                                          
019700*01   AREA -COPY W42850     -PRE W428F1-                                  
019800       EJECT                                                              
019900*01   AREA -COPY W42851     -PRE W428F2-                                  
020000       EJECT                                                              
020100 01   W001-LINE.                                                          
020200*                                                                         
020300       03  FILLER                  PIC X(121)  VALUE SPACE.               
020400       EJECT                                                              
020500 01   W001-DETAIL1.                                                       
020600       03  W001-CODE               PIC X(2) VALUE SPACE.                  
020700       03  W001-NOT-BINNED         PIC 9(9).                              
020800       03  W001-BINNED             PIC 9(6) VALUE ZEROS.                  
020900       03  W001-DEVIATED           PIC 9(7) VALUE ZEROS.                  
021000       03  W001-SCRAPPED           PIC 9(6) VALUE ZEROS.                  
021100       03  W001-QUALITY            PIC 9(7) VALUE ZEROS.                  
021200       03  W001-AVGLEAD-TIME       PIC 9(4)V9 VALUE ZEROS.                
021300       03  W001-SUARTSTD           PIC 9(8)V9(2) VALUE ZEROS.             
021400       EJECT                                                              
021500 01   W001-DETAIL2.                                                       
021600       03  W001-TOT-NOTBIN         PIC 9(9).                              
021700       03  W001-TOT-BINNED         PIC 9(6) VALUE ZEROS.                  
021800       03  W001-TOT-DEVIATED       PIC 9(7) VALUE ZEROS.                  
021900       03  W001-TOT-SCRAPPED       PIC 9(6) VALUE ZEROS.                  
022000       03  W001-TOT-QUALITY        PIC 9(7) VALUE ZEROS.                  
022100       03  W001-TOT-AVGLEAD-TIME   PIC 9(4)V9 VALUE ZEROS.                
022200       03  W001-TOT-SUARTSTD       PIC 9(8)V9(2) VALUE ZEROS.             
022300       EJECT                                                              
022400 01  FILLER               PIC X(13)   VALUE 'DLI-IO-WDB601'.              
022500 01   DLI-IO-WDB601.                                                      
022600*     03  -COPY WDB601                                                    
022700                                                                          
022800 LINKAGE SECTION.                                                         
022900*01  -COPY W0008   -PRE WDB6-                                             
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200 PROCEDURE DIVISION  USING WDB6-PCB.                                      
023300 MAIN SECTION.                                                            
023400     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
023500                                                                          
023600     PERFORM A-INIT                                                       
023700     PERFORM S01-READ-W4285A                                              
023800     SET IX TO 1                                                          
023900     PERFORM S02-READ-W4285B UNTIL END-OF-W4285B                          
024100     MOVE      1              TO  SUB-IX                                  
024200                                                                          
024300     EVALUATE TRUE                                                        
024400      WHEN NO-EOF-W4285A                                                  
024500      WHEN IX > 1                                                         
024600       PERFORM S20-SKRIV-START-POST                                       
024700       PERFORM S21A-WRITE-HEADERS                                         
024800                                                                          
024900       PERFORM D-CHECK-FILES                                              
025000       PERFORM J-AT-END-PROCESS                                           
025100     END-EVALUATE                                                         
025200                                                                          
025300     PERFORM Z-FINIT                                                      
025400                                                                          
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 A-INIT SECTION.                                                          
026000                                                                          
026100     OPEN INPUT            W4285A                                         
026200                           W4285B                                         
026300                                                                          
026400     OPEN OUTPUT           W428551                                        
026500     MOVE SPACES      TO   WS-PREV-IDDC                                   
026600     INITIALIZE W001-DETAIL1 W001-DETAIL2 WS-SUM-AVTIM                    
026700     INITIALIZE U1-DOC-W428551 U2-DOC-W428552 U3-DOC-W428553              
026800     SET LINE2-WRITE  TO TRUE                                             
026900      CALL DATKORT   USING  PROGRAM-NAME DATECARD-ID DATUMKORT            
027000      MOVE D-AAR       TO   TODAYS-DATE-YEAR                              
027100      MOVE D-MAANAD    TO   TODAYS-DATE-MONTH                             
027200      MOVE D-DAG       TO   TODAYS-DATE-DAY                               
027300      MOVE IDPGM       TO   POSTSUM-PROGNAMN                              
027400      MOVE TODAYS-DATE TO   DAT-I-TIDATUM                                 
027500     .                                                                    
027600     EJECT                                                                
027700                                                                          
027800 D-CHECK-FILES       SECTION.                                             
027900                                                                          
028000     PERFORM UNTIL END-OF-W4285A                                          
028100       IF W428F1-KDMFUP   <= TAB-KDMFUP   (SUB-IX)                        
028200         MOVE W428F1-IDDC-RET TO WS-PREVF1-IDDC                           
028300         PERFORM DA-COMPARE-IDDC                                          
028400         PERFORM S01-READ-W4285A                                          
028500         IF W428F1-IDDC-RET NOT = WS-PREVF1-IDDC                          
028600           PERFORM CC-ENDIDDC-W4285A                                      
028700           PERFORM DD-DETAIL2-LOAD                                        
028800           IF TAB-KDMFUP   (SUB-IX) < W428F1-KDMFUP OR                    
028900              (TAB-KDMFUP   (SUB-IX) = W428F1-KDMFUP AND                  
028910               TAB-IDDC-RET (SUB-IX) < W428F1-IDDC-RET)                   
029100             ADD 1 TO SUB-IX                                              
029200           END-IF                                                         
029300         END-IF                                                           
029400       ELSE                                                               
029500         PERFORM F-SEARCH-IDDC                                            
029600         PERFORM G-PROCESS-W4285B                                         
029700         ADD 1 TO SUB-IX                                                  
029900       END-IF                                                             
030000     END-PERFORM                                                          
030100     IF U2-DOC-KDANMORS NOT = ( LOW-VALUE AND                             
030200                               SPACES)                                    
030300       PERFORM CC-ENDIDDC-W4285A                                          
030400       PERFORM DD-DETAIL2-LOAD                                            
030500     END-IF                                                               
030600     PERFORM UNTIL TAB-IDDC-RET (SUB-IX) = HIGH-VALUE                     
030700        PERFORM F-SEARCH-IDDC                                             
030800        PERFORM G-PROCESS-W4285B                                          
030900        ADD 1 TO SUB-IX                                                   
031000     END-PERFORM                                                          
031100     .                                                                    
031200     EJECT                                                                
031300                                                                          
031400 DA-COMPARE-IDDC     SECTION.                                             
031500                                                                          
031600     IF W428F1-KDMFUP   = DAP-KDMFUP                                      
031700      IF W428F1-IDDC-RET = WS-PREV-IDDC                                   
031800       MOVE W428F1-IDDC-RET           TO WS-PREV-IDDC                     
031900       MOVE W428F1-IDDC-RET           TO WS-SEARCH-IDDC                   
032000       PERFORM DBA-DETAIL1-LOAD                                           
032100      ELSE                                                                
032200       IF WS-PREV-IDDC = SPACES                                           
032300        MOVE W428F1-IDDC-RET          TO WS-PREV-IDDC                     
032400        MOVE W428F1-IDDC-RET          TO WS-SEARCH-IDDC                   
032500        PERFORM DBA-DETAIL1-LOAD                                          
032600       ELSE                                                               
032700        PERFORM CC-ENDIDDC-W4285A                                         
032800        PERFORM DD-DETAIL2-LOAD                                           
032900        MOVE W428F1-IDDC-RET          TO WS-PREV-IDDC                     
033000        MOVE W428F1-IDDC-RET          TO U2-DOC-IDDC-RET                  
033100        MOVE W428F1-IDDC-RET          TO WS-SEARCH-IDDC                   
033200        MOVE W428F1-ADCITY            TO WS-PREV-ADCITY                   
033300       END-IF                                                             
033400      END-IF                                                              
033500     ELSE                                                                 
033800       PERFORM S20-SKRIV-START-POST                                       
033900       PERFORM S21A-WRITE-HEADERS                                         
034000       MOVE W428F1-IDDC-RET           TO WS-PREV-IDDC                     
034100       MOVE W428F1-IDDC-RET           TO WS-SEARCH-IDDC                   
034200       MOVE W428F1-ADCITY             TO WS-PREV-ADCITY                   
034300       PERFORM DBA-DETAIL1-LOAD                                           
034400     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 DBA-DETAIL1-LOAD    SECTION.                                             
034800     IF  W001-CODE = W428F1-KDANMORS                                      
034900         MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS            
035000                                     WS-TEMP-CODE                         
035100         MOVE W428F1-ADCITY       TO WS-PREV-ADCITY                       
035200         MOVE W428F1-KVRETINL     TO W001-BINNED                          
035300         MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                        
035400         MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                        
035500         MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                         
035600         MOVE W428F1-SUARTSTD     TO W001-SUARTSTD                        
035700         MOVE W428F1-KVDAGAR-INL  TO W001-AVGLEAD-TIME                    
035800         ADD W001-BINNED          TO WS-TEMP-BIN                          
035900         ADD W001-DEVIATED        TO WS-TEMP-DEV                          
036000         ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                        
036100         ADD W001-QUALITY         TO WS-TEMP-QUAL                         
036200         ADD W001-SUARTSTD        TO WS-TEMP-SUARTSTD                     
036300         ADD W001-AVGLEAD-TIME    TO WS-TEMP-AVTIM                        
036400     ELSE                                                                 
036500        IF W001-CODE = SPACES                                             
036600          MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS           
036700                                      WS-TEMP-CODE                        
036800          MOVE W428F1-ADCITY       TO WS-PREV-ADCITY                      
036900          MOVE W428F1-KVRETINL     TO W001-BINNED                         
037000          MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                       
037100          MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                       
037200          MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                        
037300          MOVE W428F1-SUARTSTD     TO W001-SUARTSTD                       
037400          MOVE W428F1-KVDAGAR-INL  TO W001-AVGLEAD-TIME                   
037500          ADD W001-BINNED          TO WS-TEMP-BIN                         
037600          ADD W001-DEVIATED        TO WS-TEMP-DEV                         
037700          ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                       
037800          ADD W001-QUALITY         TO WS-TEMP-QUAL                        
037900          ADD W001-SUARTSTD        TO WS-TEMP-SUARTSTD                    
038000          ADD W001-AVGLEAD-TIME    TO WS-TEMP-AVTIM                       
038100        ELSE                                                              
038200         IF W001-CODE NOT = W428F1-KDANMORS                               
038300          IF LINE2-WRITE                                                  
038400           MOVE WS-PREV-IDDC        TO U2-DOC-IDDC-RET                    
038500           MOVE WS-PREV-ADCITY      TO U2-DOC-ADCITY                      
038600           SET LINE2-WROTE TO TRUE                                        
038700          END-IF                                                          
038800          MOVE WS-TEMP-CODE        TO U2-DOC-KDANMORS                     
038900          MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                     
039000          MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                  
039100          MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                 
039200          MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                   
039300          MOVE WS-TEMP-SUARTSTD    TO U2-DOC-SUARTSTD-INL                 
039400          ADD  WS-TEMP-BIN         TO W001-TOT-BINNED                     
039500          ADD  WS-TEMP-DEV         TO W001-TOT-DEVIATED                   
039600          ADD  WS-TEMP-SCRAP       TO W001-TOT-SCRAPPED                   
039700          ADD  WS-TEMP-QUAL        TO W001-TOT-QUALITY                    
039800          ADD  WS-TEMP-SUARTSTD    TO W001-TOT-SUARTSTD                   
039900          COMPUTE WS-CODE-COUNT = WS-TEMP-BIN + WS-TEMP-SCRAP             
040000          IF WS-CODE-COUNT        NOT = 0                                 
040100           COMPUTE WS0-KVDAGDEC  = (WS-TEMP-AVTIM / WS-CODE-COUNT)        
040200          END-IF                                                          
040300          ADD  WS-TEMP-AVTIM       TO WS-SUM-AVTIM                        
040400          MOVE WS0-KVDAGDEC        TO U2-DOC-KVDAGDEC                     
040500          MOVE '2          '       TO U2-DOC-IDAFPRCD                     
040600          INITIALIZE W001-LINE                                            
040700          MOVE U2-DOC-W428552      TO W001-LINE                           
040800          PERFORM S21-WRITE-W42855-001                                    
040900          INITIALIZE                  U2-DOC-W428552                      
041000                                      WS-TEMP-STORAGE-AREA                
041100                                      WS-CODE-COUNT                       
041200                                      WS0-KVDAGDEC                        
041300                                      W001-DETAIL1                        
041400          MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS           
041500                                      WS-TEMP-CODE                        
041600          MOVE W428F1-ADCITY       TO WS-PREV-ADCITY                      
041700          MOVE W428F1-KVRETINL     TO W001-BINNED                         
041800          MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                       
041900          MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                        
042000          MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                       
042100          MOVE W428F1-KVDAGAR-INL  TO W001-AVGLEAD-TIME                   
042200          MOVE W428F1-SUARTSTD     TO W001-SUARTSTD                       
042300          ADD W001-BINNED          TO WS-TEMP-BIN                         
042400          ADD W001-DEVIATED        TO WS-TEMP-DEV                         
042500          ADD W001-QUALITY         TO WS-TEMP-QUAL                        
042600          ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                       
042700          ADD W001-AVGLEAD-TIME    TO WS-TEMP-AVTIM                       
042800          ADD  W001-SUARTSTD       TO WS-TEMP-SUARTSTD                    
042900         END-IF                                                           
043000        END-IF                                                            
043100     END-IF                                                               
043200       .                                                                  
043300       EJECT                                                              
043400 CC-ENDIDDC-W4285A   SECTION.                                             
043500                                                                          
043600     IF LINE2-WRITE                                                       
043700       MOVE WS-PREV-IDDC      TO U2-DOC-IDDC-RET                          
043800       MOVE WS-PREV-ADCITY    TO U2-DOC-ADCITY                            
043900       SET LINE2-WROTE        TO TRUE                                     
044000     END-IF                                                               
044100     MOVE WS-TEMP-CODE        TO U2-DOC-KDANMORS                          
044200     MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                          
044300     MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                       
044400     MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                      
044500     MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                        
044600     MOVE WS-TEMP-SUARTSTD    TO U2-DOC-SUARTSTD-INL                      
044700     COMPUTE WS-CODE-COUNT = WS-TEMP-BIN + WS-TEMP-SCRAP                  
044800     IF WS-CODE-COUNT         NOT = 0                                     
044900       COMPUTE WS1-KVDAGDEC  = (WS-TEMP-AVTIM / WS-CODE-COUNT)            
045000     END-IF                                                               
045100     ADD  WS-TEMP-AVTIM       TO WS-SUM-AVTIM                             
045200     MOVE WS1-KVDAGDEC        TO U2-DOC-KVDAGDEC                          
045300     ADD  WS-TEMP-BIN         TO W001-TOT-BINNED                          
045400     ADD  WS-TEMP-DEV         TO W001-TOT-DEVIATED                        
045500     ADD  WS-TEMP-QUAL        TO W001-TOT-QUALITY                         
045600     ADD  WS-TEMP-SCRAP       TO W001-TOT-SCRAPPED                        
045700     ADD  WS-TEMP-SUARTSTD    TO W001-TOT-SUARTSTD                        
045800     MOVE '2         '        TO U2-DOC-IDAFPRCD                          
045900     INITIALIZE W001-LINE                                                 
046000     MOVE U2-DOC-W428552     TO W001-LINE                                 
046100     PERFORM S21-WRITE-W42855-001                                         
046200     INITIALIZE               U2-DOC-W428552                              
046300     INITIALIZE WS-TEMP-STORAGE-AREA WS-CODE-COUNT                        
046400     INITIALIZE W001-DETAIL1         WS1-KVDAGDEC                         
046500     IF NOT END-OF-W4285A                                                 
046600       MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS              
046700                                   WS-TEMP-CODE                           
046800       MOVE W428F1-KVRETINL     TO W001-BINNED                            
046900       MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                          
047000       MOVE W428F1-SUARTSTD     TO W001-SUARTSTD                          
047100       MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                           
047200       MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                          
047300       MOVE W428F1-KVDAGAR-INL  TO W001-AVGLEAD-TIME                      
047400       MOVE W428F1-ADCITY       TO WS-PREV-ADCITY                         
047500       MOVE W428F1-IDDC-RET     TO WS-PREV-IDDC                           
048200     END-IF                                                               
048300     .                                                                    
048400     EJECT                                                                
048500 DD-DETAIL2-LOAD     SECTION.                                             
048600     MOVE W001-TOT-BINNED       TO U3-DOC-KVRETINL                        
048700     MOVE W001-TOT-DEVIATED     TO U3-DOC-KVAVV-KVANT                     
048800     MOVE W001-TOT-SCRAPPED     TO U3-DOC-KVRETINL-SKR                    
048900     MOVE W001-TOT-QUALITY      TO U3-DOC-KVAVV-KVAL                      
049000     MOVE W001-TOT-SUARTSTD     TO U3-DOC-SUARTSTD-INL                    
049100     COMPUTE WS-TOT-CODE = W001-TOT-BINNED + W001-TOT-SCRAPPED            
049200     IF WS-TOT-CODE NOT = 0                                               
049300       COMPUTE WS1-AVGLEAD-TIME =                                         
049400                         WS-SUM-AVTIM / WS-TOT-CODE                       
049500     END-IF                                                               
049600     MOVE WS1-AVGLEAD-TIME      TO U3-DOC-KVDAGDEC                        
049700     MOVE '3         '          TO U3-DOC-IDAFPRCD                        
049800*** SEARCH FROM THE W4285B TABLE NOT BINNED VALUE AND LINES FOR           
049900*** PARTICULAR COMPANY CODE AND IDDC                                      
050000     SET IX    TO  1                                                      
050100                                                                          
050200     SEARCH W4285B-LINE                                                   
050300       AT END                                                             
050400            MOVE ZERO                 TO U3-DOC-SURADER-RET               
050500            MOVE ZERO                 TO U3-DOC-SUARTSTD-RET              
050600       WHEN TAB-KDMFUP (IX) = DAP-KDMFUP AND                              
050700            TAB-IDDC-RET (IX) = WS-SEARCH-IDDC                            
050800            MOVE TAB-SURADER (IX)     TO U3-DOC-SURADER-RET               
050900            MOVE TAB-SUARTSTD (IX)    TO U3-DOC-SUARTSTD-RET              
051000     END-SEARCH                                                           
051100     INITIALIZE W001-LINE                                                 
051200     MOVE U3-DOC-W428553           TO W001-LINE                           
051300     PERFORM S21-WRITE-W42855-001                                         
051400     INITIALIZE          U3-DOC-W428553                                   
051500     ADD 1    TO WS-TOT-DC                                                
051600     INITIALIZE  WS-TOT-CODE                                              
051700                 W001-DETAIL2 WS-SUM-AVTIM                                
051800     SET LINE2-WRITE                TO TRUE                               
051900     .                                                                    
052000     EJECT                                                                
052100 DC-WRITE-DETAIL1    SECTION.                                             
052200     IF WS-TEMP-CODE NOT EQUAL TO SPACES                                  
052300       MOVE WS-TEMP-CODE        TO U2-DOC-KDANMORS                        
052400       MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                        
052500       IF LINE2-WRITE                                                     
052600         MOVE WS-PREV-IDDC      TO U2-DOC-IDDC-RET                        
052700         MOVE WS-PREV-ADCITY    TO U2-DOC-ADCITY                          
052800         SET LINE2-WROTE        TO TRUE                                   
052900       END-IF                                                             
053000       MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                     
053100       MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                    
053200       MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                      
053300       MOVE WS-TEMP-SUARTSTD    TO U2-DOC-SUARTSTD-INL                    
053400       ADD  WS-TEMP-BIN         TO W001-TOT-BINNED                        
053500       ADD  WS-TEMP-DEV         TO W001-TOT-DEVIATED                      
053600       ADD  WS-TEMP-QUAL        TO W001-TOT-QUALITY                       
053700       ADD  WS-TEMP-SCRAP       TO W001-TOT-SCRAPPED                      
053800       ADD  WS-TEMP-SUARTSTD    TO W001-TOT-SUARTSTD                      
053900       COMPUTE WS-CODE-COUNT = WS-TEMP-BIN + WS-TEMP-SCRAP                
054000       IF WS-CODE-COUNT NOT EQUAL TO ZERO                                 
054100         COMPUTE WS0-KVDAGDEC  = (WS-TEMP-AVTIM / WS-CODE-COUNT)          
054200       END-IF                                                             
054300       ADD  WS-TEMP-AVTIM       TO WS-SUM-AVTIM                           
054400       MOVE WS0-KVDAGDEC        TO U2-DOC-KVDAGDEC                        
054500       MOVE '2         '        TO U2-DOC-IDAFPRCD                        
054600       INITIALIZE W001-LINE                                               
054700       MOVE U2-DOC-W428552      TO W001-LINE                              
054800       PERFORM S21-WRITE-W42855-001                                       
054900       INITIALIZE               WS-TEMP-STORAGE-AREA                      
055000                                WS-CODE-COUNT                             
055100                                WS0-KVDAGDEC U2-DOC-W428552               
055200                                W001-DETAIL1                              
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600 F-SEARCH-IDDC         SECTION.                                           
055800     IF TAB-KDMFUP(SUB-IX) NOT = DAP-KDMFUP                               
055900       PERFORM S20-SKRIV-START-POST                                       
056000       PERFORM S21A-WRITE-HEADERS                                         
056100     END-IF                                                               
056300     .                                                                    
056400     EJECT                                                                
056500 G-PROCESS-W4285B      SECTION.                                           
056600     INITIALIZE U2-DOC-W428552                                            
056700     MOVE TAB-IDDC-RET (SUB-IX)  TO U2-DOC-IDDC-RET                       
056800                                    W-IDDC                                
056900     MOVE '2         '           TO U2-DOC-IDAFPRCD                       
057000     PERFORM IMS-GU-WDB601                                                
057100     IF SEGMENT-FINNS                                                     
057200       MOVE DCS-ADGMT-PADR(11:20) TO U2-DOC-ADCITY                        
057300     END-IF                                                               
057400     INITIALIZE W001-LINE                                                 
057500     MOVE U2-DOC-W428552         TO W001-LINE                             
057600     PERFORM S21-WRITE-W42855-001                                         
057700     INITIALIZE U2-DOC-W428552                                            
057800     MOVE '3         '           TO U3-DOC-IDAFPRCD                       
057900     MOVE TAB-SURADER (SUB-IX)   TO U3-DOC-SURADER-RET                    
058000     MOVE TAB-SUARTSTD (SUB-IX)  TO U3-DOC-SUARTSTD-RET                   
058100     INITIALIZE W001-LINE                                                 
058200     MOVE U3-DOC-W428553         TO W001-LINE                             
058300     PERFORM S21-WRITE-W42855-001                                         
058400     INITIALIZE U3-DOC-SURADER-RET U3-DOC-SUARTSTD-RET                    
058500     .                                                                    
058600     EJECT                                                                
058700 J-AT-END-PROCESS      SECTION.                                           
058800                                                                          
058900     IF WS-TEMP-CODE NOT EQUAL TO SPACES                                  
059000       PERFORM DC-WRITE-DETAIL1                                           
059100       PERFORM DD-DETAIL2-LOAD                                            
059200     END-IF                                                               
059300     .                                                                    
059400     EJECT                                                                
059500 S21-WRITE-W42855-001  SECTION.                                           
059600                                                                          
059700     WRITE W42855-001-LINE FROM W001-LINE                                 
059800     INITIALIZE                 W001-LINE                                 
059900     .                                                                    
060000     EJECT                                                                
060100 S20-SKRIV-START-POST SECTION.                                            
060200*                                                                         
060300     WRITE UT-DAP-POST         FROM DAP-AREA-TYPE                         
060400     MOVE 'DAP'                  TO POSTSUM-TRANSTYP                      
060500     MOVE 'W428551'              TO POSTSUM-FDNAMN                        
060600     MOVE 'W42855D3'             TO POSTSUM-DDNAMN2                       
060700     CALL POSTSUM             USING POSTSUM-PARM                          
060800                                                                          
060900     IF W428F1-KDMFUP   <= TAB-KDMFUP   (SUB-IX)                          
061000       MOVE W428F1-KDMFUP        TO DAP-KDMFUP                            
061100       MOVE W4285A-TISAAPP-INLINL-TIAAPP                                  
061200                                 TO DAP-DATE                              
061300     ELSE                                                                 
061400       MOVE TAB-KDMFUP(SUB-IX)   TO DAP-KDMFUP                            
061500       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
061600       CALL WDATKONV          USING DAT-KDDATFORM DAT-I-TIDATUM           
061700                                    DAT-O-TIDATUM DAT-KDSVAR              
061800                                                                          
061900       IF DAT-KDSVAR-FEL                                                  
062000         MOVE 'DATUMKONVERTERINGEN HAR GÅTT SNETT'                        
062100                                 TO ERROR-TEXT                            
062200         CALL FELLOG                                                      
062300       END-IF                                                             
062400       MOVE DAT-TIAAPP           TO DAP-DATE                              
062500     END-IF                                                               
062600                                                                          
062700     MOVE DAP-KDMFUP             TO DAP-ACODE                             
062800                                                                          
063600                                                                          
063700     WRITE UT-DAP-POST         FROM DAP-AREA-SUBTYPE                      
063800     MOVE 'DAP'                  TO POSTSUM-TRANSTYP                      
063900     MOVE 'W428551'              TO POSTSUM-FDNAMN                        
064000     MOVE 'W42855D3'             TO POSTSUM-DDNAMN2                       
064100     CALL POSTSUM             USING POSTSUM-PARM                          
064200     .                                                                    
064300     EJECT                                                                
064400 S21A-WRITE-HEADERS SECTION.                                              
064500                                                                          
064600     MOVE '1         '           TO U1-DOC-IDAFPRCD                       
064700     MOVE DAP-DATE               TO U1-DOC-TIAAPP                         
064800     MOVE U1-DOC-W428551         TO W001-LINE                             
064900     PERFORM S21-WRITE-W42855-001                                         
065000     SET HDR-WRITTEN             TO TRUE                                  
065100     INITIALIZE U1-DOC-W428551 WS-PREV-IDDC                               
065200     .                                                                    
065300     EJECT                                                                
065400 Z-FINIT SECTION.                                                         
065500     CLOSE                 W4285A                                         
065600                           W4285B                                         
065700                           W428551                                        
065800     DISPLAY '************************************'                       
065900     DISPLAY '---------SUMMARY OF THE REPORT------'                       
066000     DISPLAY 'TOTAL NO OF REPORTS GENERATED =' WS-TOT-DC                  
066100     DISPLAY '************************************'                       
066200     MOVE 'S'            TO POSTSUM-OPKOD                                 
066300     CALL POSTSUM     USING POSTSUM-PARM                                  
066400     .                                                                    
066500     EJECT                                                                
066600 S01-READ-W4285A  SECTION.                                                
066700     READ W4285A       INTO W428F1-AREA                                   
066800     AT END                                                               
066900       MOVE HIGH-VALUE   TO W428F1-AREA                                   
067000       SET END-OF-W4285A TO TRUE                                          
067100                                                                          
067200     NOT AT END                                                           
067300       MOVE 'W4285A'     TO POSTSUM-FDNAMN                                
067400       MOVE 'W42855D1'   TO POSTSUM-DDNAMN2                               
067500       MOVE SPACES       TO POSTSUM-TRANSTYP                              
067600       CALL POSTSUM   USING POSTSUM-PARM                                  
067700     END-READ                                                             
067800     .                                                                    
067900     EJECT                                                                
068000 S02-READ-W4285B  SECTION.                                                
068100     READ W4285B INTO W4285B-LINE (IX)                                    
068200     AT END                                                               
068300        MOVE HIGH-VALUE   TO W4285B-LINE (IX)                             
068400        SET END-OF-W4285B TO TRUE                                         
068500                                                                          
068600     NOT AT END                                                           
068700         SET IX UP BY 1                                                   
068800         MOVE 'W4285B'     TO POSTSUM-FDNAMN                              
068900         MOVE 'W42855D2'   TO POSTSUM-DDNAMN2                             
069000         MOVE SPACES       TO  POSTSUM-TRANSTYP                           
069100         CALL POSTSUM    USING POSTSUM-PARM                               
069200     END-READ                                                             
069300     .                                                                    
069400     EJECT                                                                
069500 IMS-GU-WDB601    SECTION.                                                
069600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
069700          DELIMITED BY SIZE INTO SSA1                                     
069800     MOVE '  GE' TO GODK-STATUSKODER                                      
069900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601    SSA1                 
070000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
070100     PERFORM IMS-STATUSKONTROLL                                           
070200     IF SEGMENT-SAKNAS                                                    
070300         MOVE SPACE TO DCS-KDDC                                           
070400     END-IF                                                               
070500     .                                                                    
070600 IMS-STATUSKONTROLL SECTION.                                              
070700                                                                          
070800     SET STATUS-IX TO 1                                                   
070900     SEARCH GODK-STATUS                                                   
071000       AT END                                                             
071100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
071200         DELIMITED BY SIZE INTO FELTEXT                                   
071300         CALL FELLOG                                                      
071400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
071500         CONTINUE                                                         
071600     END-SEARCH                                                           
071700     .                                                                    
