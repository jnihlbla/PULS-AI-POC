000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4285400.                                                
000300 AUTHOR.         SHILPA MADHURI G.                                        
000400 DATE-WRITTEN.   11/11/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700******************************************************************        
000800*                                                                *        
000900* FUNCTION:                                                      *        
001000*     THIS PROGRAM CREATES A FILE WHICH IS USED TO CREATE THE    *        
001100* 'MANAGEMENT DEALER WEEKLY FOLLOW UP' REPORT ON WEB             *        
001200* INPUT FILES ARE FROM THE PROGRAM W4285700                      *        
001300*                                                                *        
001400* E'TRACKER:           10143271 - CHINA WAREHOUSE PROJECT-1      *        
001500* E'TRACKER: 20121105  10181786 WEB AUSTRALIEN FOLLOW-UP         *        
001510*                                                                *        
001600******************************************************************        
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200*          --- W4285A FILE                                                
002300       SELECT W4285A                     ASSIGN TO W42854D1.              
002400*          --- W4285B FILE                                                
002500       SELECT W4285B                     ASSIGN TO W42854D2.              
002600*          --- REPORT FILE                                                
002700       SELECT W428541                    ASSIGN TO W42854D3.              
002800       EJECT                                                              
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100 FD   W4285A                                                              
003200       RECORDING       F                                                  
003300       BLOCK CONTAINS  0.                                                 
003400                                                                          
003500*01   -COPY W42850      -PRE W4285A-                                      
003600 FD   W4285B                                                              
003700       RECORDING       F                                                  
003800       BLOCK CONTAINS  0.                                                 
003900                                                                          
004000*01   -COPY W42851      -PRE W4285B-                                      
004100 FD   W428541                                                             
004200       RECORDING       V                                                  
004300       BLOCK CONTAINS  0.                                                 
004400 01  UT-DAP-POST.                                                         
004500       03  FILLER         PIC X(21).                                      
004600*01   -COPY W428541       -PRE U1-                                        
004700       EJECT                                                              
004800*01   -COPY W428542       -PRE U2-                                        
004900       EJECT                                                              
005000*01   -COPY W428543       -PRE U3-                                        
005100       EJECT                                                              
005200                                                                          
005300                                                                          
005400 01   W42854-001-LINE             PIC X(121).                             
005500      EJECT                                                               
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800 77   IDPGM                       PIC X(8)    VALUE 'W4285400'.           
005900 77   WS-CODE-COUNT               PIC 9(5)    VALUE ZEROS.                
006000 77   WS0-KVDAGDEC                PIC 9(4)V9  VALUE ZEROS.                
006100 77   WS1-KVDAGDEC                PIC 9(4)V9  VALUE ZEROS.                
006300 77   WS1-AVGLEAD-TIME            PIC 9(4)V9  VALUE ZEROS.                
006400 77   WS-TOT-DC                   PIC 9(2)    VALUE ZEROS.                
006500 77   WS-PREV-IDDC                PIC X(2)    VALUE SPACES.               
006600 77   WS-SEARCH-IDDC              PIC X(2)    VALUE SPACES.               
006700 77   WS-PREVF1-IDDC              PIC X(2)    VALUE SPACES.               
006800 77   WS-PREV-ADCITY              PIC X(20)   VALUE SPACES.               
006900 77   WS-SUM-AVTIM                PIC 9(4)V9  VALUE ZEROS.                
007000 77   SUB-IX                      PIC 9(4).                               
007100 77   WS-DAP-EU                   PIC X(2)    VALUE 'EU'.                 
007200 77   WS-DAP-CN                   PIC X(2)    VALUE 'CN'.                 
007310 77   DAP-KDMFUP                  PIC X(2)    VALUE SPACES.               
007400 77   FELTEXT                     PIC X(80)   VALUE SPACE.                
007500                                                                          
007600 77   W428F1-EOF-SW               PIC X       VALUE 'N'.                  
007700      88  END-OF-W4285A                       VALUE 'Y'.                  
007800      88  NO-EOF-W4285A                       VALUE 'N'.                  
007900                                                                          
008000 77   W428F2-EOF-SW               PIC X       VALUE 'N'.                  
008100      88  END-OF-W4285B                       VALUE 'Y'.                  
008200      88  NO-EOF-W4285B                       VALUE 'N'.                  
008300                                                                          
008400 77   W001-HEADER-SW              PIC X       VALUE 'N'.                  
008500      88  HDR-WRITTEN                         VALUE 'Y'.                  
008600      88  HDR-NOTWRITTEN                      VALUE 'N'.                  
008700                                                                          
009600 77   W001-LINE2-STAT             PIC X       VALUE 'N'.                  
009700      88  LINE2-WRITE                         VALUE 'Y'.                  
009800      88  LINE2-WROTE                         VALUE 'N'.                  
009900                                                                          
010300      EJECT                                                               
010400*     ARRAY DECLARATION FOR W4285B FILE                                   
010500 01   W4285B-NOT-BINNED-TABLE.                                            
010600      03  W4285B-LINE OCCURS 1000 TIMES                                   
010700                      INDEXED BY IX.                                      
010800         05  TAB-KDMFUP           PIC X(2).                               
010900         05  TAB-IDDC-RET         PIC X(2).                               
011000         05  TAB-IDARTNR          PIC 9(8).                               
011100         05  TAB-KVLEVANM-BEKR    PIC 9(6).                               
011200         05  TAB-SURADER          PIC 9(9).                               
011300         05  TAB-SUARTSTD         PIC 9(8)V9(2).                          
011400                                                                          
017700 01  GODK-STATUSKODER.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900 01  SSA1                        PIC X(64).                               
018000 01  SSA2                        PIC X(64).                               
018100 01   GENERAL-SUBPROGRAMS.                                                
018200*                                                                         
018300      03  ABEND                   PIC X(8)    VALUE 'ABEND   '.           
018400      03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.           
018500      03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.           
018600      03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.           
018700      03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.           
018800      03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.           
018900*     TEMPORARY WORKING STORAGE VARIABLES USED                            
019000 01   WS-TEMP-STORAGE-AREA.                                               
019100       03  WS-TEMP-CODE            PIC X(2)    VALUE SPACES.              
019200       03  WS-TEMP-NOTBIN          PIC 9(9)    VALUE ZEROS.               
019300       03  WS-TEMP-BIN             PIC 9(6)    VALUE ZEROS.               
019400       03  WS-TEMP-DEV             PIC 9(7)    VALUE ZEROS.               
019500       03  WS-TEMP-SCRAP           PIC 9(6)    VALUE ZEROS.               
019600       03  WS-TEMP-QUAL            PIC 9(7)    VALUE ZEROS.               
019700       03  WS-TEMP-AVTIM           PIC 9(4)V9  VALUE ZEROS.               
019800       03  WS-TEMP-SUARTSTD        PIC 9(8)V9(2) VALUE ZEROS.             
019900                                                                          
020000 01   WS-TOTAL-STORAGE-AREA.                                              
020100       03  WS-TOT-CODE             PIC 9(5)    VALUE ZEROS.               
020200       03  WS-TOT-NOTBIN           PIC Z(8)9  .                           
020300       03  WS-TOT-BIN              PIC Z(5)9  .                           
020400       03  WS-TOT-DEV              PIC Z(6)9  .                           
020500       03  WS-TOT-SCRAP            PIC Z(5)9  .                           
020600       03  WS-TOT-QUAL             PIC Z(6)9  .                           
020700       03  WS-TOT-AVTIM            PIC Z(3)9.9.                           
020800*     --- PARAMETERS TO ABEND                                             
020900                                                                          
021000 01   TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                 
021100 01   FILLER REDEFINES TODAYS-DATE.                                       
021200      03  TODAYS-DATE-YEAR        PIC 9(2).                               
021300      03  TODAYS-DATE-MONTH       PIC 9(2).                               
021400      03  TODAYS-DATE-DAY         PIC 9(2).                               
021500      EJECT                                                               
021600 01   DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.             
021700*01   -COPY WDATKORT                                                      
021800*                                                                         
021900 77   RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.              
022000 77   RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.             
022100 77   RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.           
022200 01   ERROR-TEXT.                                                         
022300       03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.        
022400       03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.               
022500       EJECT                                                              
022600 01  DAP-AREA-TYPE.                                                       
022700       03  FILLER                  PIC X(15)   VALUE                      
022800           ' ¤DAPW42854-001'.                                             
022900 01  DAP-AREA-SUBTYPE.                                                    
023000       03  FILLER                  PIC X(5)   VALUE                       
023100        ' ¤DAP'.                                                          
023200       03  DAP-ACODE               PIC X(2) VALUE SPACES.                 
023300       03  DAP-DATE                PIC X(4) VALUE SPACES.                 
023400                                                                          
023500       EJECT                                                              
023600                                                                          
023700 01  STATUS-WS                   PIC XX.                                  
023800       88  SEGMENT-FINNS                       VALUE '  '.                
023900       88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                
024000       88  SEGMENT-SAKNAS                      VALUE 'GE'.                
024001       SKIP2                                                              
024002 01  NYCKLAR-TILL-DLI.                                                    
024003     03  W-IDDC-X.                                                        
024004         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
024005*     --- PARAMETERS TO DATKORT                                           
024006*                                                                         
024007 01   PROGRAM-NAME                PIC X(6)    VALUE 'W42854'.             
024008*                                                                         
024009*01   -COPY W0005   -PRE  POSTSUM-                                        
024010       EJECT                                                              
024020*01   -COPY WDATAREA                                                      
024030       EJECT                                                              
024060*01   -COPY W0003                                                         
024070       EJECT                                                              
024080 01   W428D1-AREA-START           PIC X(24)   VALUE                       
024090                                   'W428D1-AREA-START  '.                 
024100                                                                          
024200*01   AREA -COPY W42850     -PRE W428F1-                                  
024300       EJECT                                                              
024400*01   AREA -COPY W42851     -PRE W428F2-                                  
024500       EJECT                                                              
024600 01   W001-LINE.                                                          
024700*                                                                         
024800       03  FILLER                  PIC X(121)  VALUE SPACE.               
024900       EJECT                                                              
025000 01   W001-DETAIL1.                                                       
025100       03  W001-CODE               PIC X(2) VALUE SPACE.                  
025200       03  W001-NOT-BINNED         PIC 9(9).                              
025300       03  W001-BINNED             PIC 9(6) VALUE ZEROS.                  
025400       03  W001-DEVIATED           PIC 9(7) VALUE ZEROS.                  
025500       03  W001-SCRAPPED           PIC 9(6) VALUE ZEROS.                  
025600       03  W001-QUALITY            PIC 9(7) VALUE ZEROS.                  
025700       03  W001-AVGLEAD-TIME       PIC 9(4)V9 VALUE ZEROS.                
025800       03  W001-SUARTSTD           PIC 9(8)V9(2) VALUE ZEROS.             
025900       EJECT                                                              
026000 01   W001-DETAIL2.                                                       
026100       03  W001-TOT-NOTBIN         PIC 9(9).                              
026200       03  W001-TOT-BINNED         PIC 9(6) VALUE ZEROS.                  
026300       03  W001-TOT-DEVIATED       PIC 9(7) VALUE ZEROS.                  
026400       03  W001-TOT-SCRAPPED       PIC 9(6) VALUE ZEROS.                  
026500       03  W001-TOT-QUALITY        PIC 9(7) VALUE ZEROS.                  
026600       03  W001-TOT-AVGLEAD-TIME   PIC 9(4)V9 VALUE ZEROS.                
026700       03  W001-TOT-SUARTSTD       PIC 9(8)V9(2) VALUE ZEROS.             
026800       EJECT                                                              
026900 01  FILLER               PIC X(13)   VALUE 'DLI-IO-WDB601'.              
027000 01   DLI-IO-WDB601.                                                      
027100*     03  -COPY WDB601                                                    
027200                                                                          
027300 LINKAGE SECTION.                                                         
027400*01  -COPY W0008   -PRE WDB6-                                             
027500     05  FILLER                  PIC X.                                   
027600     EJECT                                                                
027700 PROCEDURE DIVISION  USING WDB6-PCB.                                      
027800 MAIN SECTION.                                                            
027900     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
028000                                                                          
028100     PERFORM A-INIT                                                       
028200     PERFORM S01-READ-W4285A                                              
028210     SET IX TO 1                                                          
028300     PERFORM S02-READ-W4285B UNTIL END-OF-W4285B                          
028500     MOVE      1              TO  SUB-IX                                  
028600                                                                          
028700     EVALUATE TRUE                                                        
028800      WHEN NO-EOF-W4285A                                                  
028900      WHEN IX > 1                                                         
029100       PERFORM S20-SKRIV-START-POST                                       
029200       PERFORM S21A-WRITE-HEADERS                                         
029300                                                                          
029600       PERFORM D-CHECK-FILES                                              
029700       PERFORM J-AT-END-PROCESS                                           
029800     END-EVALUATE                                                         
029900                                                                          
030000     PERFORM Z-FINIT                                                      
030100                                                                          
030200     MOVE ZERO TO RETURN-CODE                                             
030300     GOBACK                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 A-INIT SECTION.                                                          
030700                                                                          
030800     OPEN INPUT            W4285A                                         
030900                           W4285B                                         
031000                                                                          
031100     OPEN OUTPUT           W428541                                        
031200     MOVE SPACES      TO   WS-PREV-IDDC                                   
031300     INITIALIZE W001-DETAIL1 W001-DETAIL2 WS-SUM-AVTIM                    
031400     SET LINE2-WRITE  TO TRUE                                             
031500      CALL DATKORT   USING  PROGRAM-NAME DATECARD-ID DATUMKORT            
031600      MOVE D-AAR       TO   TODAYS-DATE-YEAR                              
031700      MOVE D-MAANAD    TO   TODAYS-DATE-MONTH                             
031800      MOVE D-DAG       TO   TODAYS-DATE-DAY                               
031900      MOVE IDPGM       TO   POSTSUM-PROGNAMN                              
032000      MOVE TODAYS-DATE TO   DAT-I-TIDATUM                                 
032100     .                                                                    
032200     EJECT                                                                
032300                                                                          
032400 D-CHECK-FILES       SECTION.                                             
032500                                                                          
032600     PERFORM UNTIL END-OF-W4285A                                          
032920       IF W428F1-KDMFUP   <= TAB-KDMFUP   (SUB-IX)                        
033000         MOVE W428F1-IDDC-RET TO WS-PREVF1-IDDC                           
033100         PERFORM DA-COMPARE-IDDC                                          
033200         PERFORM S01-READ-W4285A                                          
033310         IF W428F1-IDDC-RET NOT = WS-PREVF1-IDDC                          
033400           PERFORM CC-ENDIDDC-W4285A                                      
033500           PERFORM DD-DETAIL2-LOAD                                        
033610           IF TAB-KDMFUP   (SUB-IX) < W428F1-KDMFUP OR                    
033611              (TAB-KDMFUP   (SUB-IX) = W428F1-KDMFUP AND                  
033620               TAB-IDDC-RET (SUB-IX) < W428F1-IDDC-RET)                   
033710             ADD 1 TO SUB-IX                                              
033800           END-IF                                                         
034000         END-IF                                                           
034100       ELSE                                                               
034200         PERFORM F-SEARCH-IDDC                                            
034300         PERFORM G-PROCESS-W4285B                                         
034400         ADD 1 TO SUB-IX                                                  
034600       END-IF                                                             
034900     END-PERFORM                                                          
035000     IF U2-DOC-KDANMORS NOT = ( LOW-VALUE AND                             
035100                               SPACES)                                    
035200       PERFORM CC-ENDIDDC-W4285A                                          
035300       PERFORM DD-DETAIL2-LOAD                                            
035400     END-IF                                                               
035500     PERFORM UNTIL TAB-IDDC-RET (SUB-IX) = HIGH-VALUE                     
035600        PERFORM F-SEARCH-IDDC                                             
035610        PERFORM G-PROCESS-W4285B                                          
035700        ADD 1 TO SUB-IX                                                   
035800     END-PERFORM                                                          
035900     .                                                                    
036000     EJECT                                                                
036100                                                                          
036200 DA-COMPARE-IDDC     SECTION.                                             
036300                                                                          
036400     IF W428F1-KDMFUP   = DAP-KDMFUP                                      
036500      IF W428F1-IDDC-RET = WS-PREV-IDDC                                   
036600       MOVE W428F1-IDDC-RET           TO WS-PREV-IDDC                     
036700       MOVE W428F1-IDDC-RET           TO WS-SEARCH-IDDC                   
036800       PERFORM DBA-DETAIL1-LOAD                                           
036900      ELSE                                                                
037000       IF WS-PREV-IDDC = SPACES                                           
037100        MOVE W428F1-IDDC-RET          TO WS-PREV-IDDC                     
037200        MOVE W428F1-IDDC-RET          TO WS-SEARCH-IDDC                   
037300        PERFORM DBA-DETAIL1-LOAD                                          
037400       ELSE                                                               
037500        PERFORM CC-ENDIDDC-W4285A                                         
037600        PERFORM DD-DETAIL2-LOAD                                           
037700        MOVE W428F1-IDDC-RET          TO WS-PREV-IDDC                     
037800        MOVE W428F1-IDDC-RET          TO U2-DOC-IDDC-RET                  
037900        MOVE W428F1-IDDC-RET          TO WS-SEARCH-IDDC                   
038000        MOVE W428F1-ADCITY            TO WS-PREV-ADCITY                   
038100       END-IF                                                             
038200      END-IF                                                              
038300     ELSE                                                                 
038600       PERFORM S20-SKRIV-START-POST                                       
038700       PERFORM S21A-WRITE-HEADERS                                         
038800       MOVE W428F1-IDDC-RET           TO WS-PREV-IDDC                     
038900       MOVE W428F1-IDDC-RET           TO WS-SEARCH-IDDC                   
039000       MOVE W428F1-ADCITY             TO WS-PREV-ADCITY                   
039010       PERFORM DBA-DETAIL1-LOAD                                           
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 DBA-DETAIL1-LOAD    SECTION.                                             
039500     IF  W001-CODE = W428F1-KDANMORS                                      
039510         MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS            
039520                                     WS-TEMP-CODE                         
039600         MOVE W428F1-ADCITY       TO WS-PREV-ADCITY                       
039700         MOVE W428F1-KVRETINL     TO W001-BINNED                          
039800         MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                        
039900         MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                        
039910         MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                         
040000         MOVE W428F1-SUARTSTD     TO W001-SUARTSTD                        
040010         MOVE W428F1-KVDAGAR-INL  TO W001-AVGLEAD-TIME                    
040400         ADD W001-BINNED          TO WS-TEMP-BIN                          
040500         ADD W001-DEVIATED        TO WS-TEMP-DEV                          
040600         ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                        
040700         ADD W001-QUALITY         TO WS-TEMP-QUAL                         
040800         ADD W001-SUARTSTD        TO WS-TEMP-SUARTSTD                     
040900         ADD W001-AVGLEAD-TIME    TO WS-TEMP-AVTIM                        
041000     ELSE                                                                 
041100       IF W001-CODE = SPACES                                              
041110          MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS           
041120                                      WS-TEMP-CODE                        
041130          MOVE W428F1-ADCITY       TO WS-PREV-ADCITY                      
041200          MOVE W428F1-KVRETINL     TO W001-BINNED                         
041400          MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                       
041410          MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                       
041500          MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                        
041600          MOVE W428F1-SUARTSTD     TO W001-SUARTSTD                       
041610          MOVE W428F1-KVDAGAR-INL  TO W001-AVGLEAD-TIME                   
042000          ADD W001-BINNED          TO WS-TEMP-BIN                         
042100          ADD W001-DEVIATED        TO WS-TEMP-DEV                         
042200          ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                       
042300          ADD W001-QUALITY         TO WS-TEMP-QUAL                        
042400          ADD W001-SUARTSTD        TO WS-TEMP-SUARTSTD                    
042500          ADD W001-AVGLEAD-TIME    TO WS-TEMP-AVTIM                       
042600       ELSE                                                               
042700         IF W001-CODE NOT = W428F1-KDANMORS                               
042800          IF LINE2-WRITE                                                  
042900           MOVE WS-PREV-IDDC        TO U2-DOC-IDDC-RET                    
043000           MOVE WS-PREV-ADCITY      TO U2-DOC-ADCITY                      
043100           SET LINE2-WROTE TO TRUE                                        
043200          END-IF                                                          
043300          MOVE WS-TEMP-CODE        TO U2-DOC-KDANMORS                     
043400          MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                     
043500          MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                  
043600          MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                 
043700          MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                   
043800          MOVE WS-TEMP-SUARTSTD    TO U2-DOC-SUARTSTD-INL                 
043900          ADD  WS-TEMP-BIN         TO W001-TOT-BINNED                     
044000          ADD  WS-TEMP-DEV         TO W001-TOT-DEVIATED                   
044100          ADD  WS-TEMP-SCRAP       TO W001-TOT-SCRAPPED                   
044200          ADD  WS-TEMP-QUAL        TO W001-TOT-QUALITY                    
044300          ADD  WS-TEMP-SUARTSTD    TO W001-TOT-SUARTSTD                   
044400          COMPUTE WS-CODE-COUNT = WS-TEMP-BIN + WS-TEMP-SCRAP             
044500          IF WS-CODE-COUNT        NOT = 0                                 
044600           COMPUTE WS0-KVDAGDEC  = (WS-TEMP-AVTIM / WS-CODE-COUNT)        
044700          END-IF                                                          
044800          ADD  WS-TEMP-AVTIM       TO WS-SUM-AVTIM                        
044900          MOVE WS0-KVDAGDEC        TO U2-DOC-KVDAGDEC                     
045000          MOVE '2          '       TO U2-DOC-IDAFPRCD                     
045100          INITIALIZE W001-LINE                                            
045200          MOVE U2-DOC-W428542      TO W001-LINE                           
045300          PERFORM S21-WRITE-W42854-001                                    
045400          INITIALIZE                  U2-DOC-W428542                      
045500                                      WS-TEMP-STORAGE-AREA                
045600                                      WS-CODE-COUNT                       
045700                                      WS0-KVDAGDEC                        
045800                                      W001-DETAIL1                        
045900          MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS           
046000                                      WS-TEMP-CODE                        
046010          MOVE W428F1-ADCITY       TO WS-PREV-ADCITY                      
046100          MOVE W428F1-KVRETINL     TO W001-BINNED                         
046300          MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                       
046400          MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                        
046600          MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                       
046801          MOVE W428F1-KVDAGAR-INL  TO W001-AVGLEAD-TIME                   
046802          MOVE W428F1-SUARTSTD     TO W001-SUARTSTD                       
046804          ADD W001-BINNED          TO WS-TEMP-BIN                         
046810          ADD W001-DEVIATED        TO WS-TEMP-DEV                         
046900          ADD W001-QUALITY         TO WS-TEMP-QUAL                        
047100          ADD W001-SCRAPPED        TO WS-TEMP-SCRAP                       
047110          ADD W001-AVGLEAD-TIME    TO WS-TEMP-AVTIM                       
047200          ADD  W001-SUARTSTD       TO WS-TEMP-SUARTSTD                    
047300         END-IF                                                           
047400       END-IF                                                             
047500     END-IF                                                               
047600       .                                                                  
047700       EJECT                                                              
047800 CC-ENDIDDC-W4285A   SECTION.                                             
047900                                                                          
048000     IF LINE2-WRITE                                                       
048100        MOVE WS-PREV-IDDC      TO U2-DOC-IDDC-RET                         
048200        MOVE WS-PREV-ADCITY    TO U2-DOC-ADCITY                           
048300        SET LINE2-WROTE        TO TRUE                                    
048400     END-IF                                                               
048500     MOVE WS-TEMP-CODE        TO U2-DOC-KDANMORS                          
048600     MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                          
048700     MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                       
048800     MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                      
048900     MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                        
049000     MOVE WS-TEMP-SUARTSTD    TO U2-DOC-SUARTSTD-INL                      
049100     COMPUTE WS-CODE-COUNT = WS-TEMP-BIN + WS-TEMP-SCRAP                  
049200     IF WS-CODE-COUNT         NOT = 0                                     
049300       COMPUTE WS1-KVDAGDEC  = (WS-TEMP-AVTIM / WS-CODE-COUNT)            
049400     END-IF                                                               
049500     ADD  WS-TEMP-AVTIM       TO WS-SUM-AVTIM                             
049600     MOVE WS1-KVDAGDEC        TO U2-DOC-KVDAGDEC                          
049700     ADD  WS-TEMP-BIN         TO W001-TOT-BINNED                          
049800     ADD  WS-TEMP-DEV         TO W001-TOT-DEVIATED                        
049900     ADD  WS-TEMP-QUAL        TO W001-TOT-QUALITY                         
050000     ADD  WS-TEMP-SCRAP       TO W001-TOT-SCRAPPED                        
050100     ADD  WS-TEMP-SUARTSTD    TO W001-TOT-SUARTSTD                        
050200     MOVE '2         '        TO U2-DOC-IDAFPRCD                          
050300     INITIALIZE W001-LINE                                                 
050400     MOVE U2-DOC-W428542     TO W001-LINE                                 
050500     PERFORM S21-WRITE-W42854-001                                         
050600     INITIALIZE W001-DETAIL1  U2-DOC-W428542 WS1-KVDAGDEC                 
050700     INITIALIZE WS-TEMP-STORAGE-AREA WS-CODE-COUNT                        
050800     IF NOT END-OF-W4285A                                                 
050900        MOVE W428F1-KDANMORS     TO W001-CODE U2-DOC-KDANMORS             
051000                                   WS-TEMP-CODE                           
051100        MOVE W428F1-KVRETINL     TO W001-BINNED                           
051300        MOVE W428F1-KVAVV-KVANT  TO W001-DEVIATED                         
051400        MOVE W428F1-SUARTSTD     TO W001-SUARTSTD                         
051500        MOVE W428F1-KVAVV-KVAL   TO W001-QUALITY                          
051600        MOVE W428F1-KVRETINL-SKR TO W001-SCRAPPED                         
051610        MOVE W428F1-KVDAGAR-INL  TO W001-AVGLEAD-TIME                     
051700        MOVE W428F1-ADCITY       TO WS-PREV-ADCITY                        
051800        MOVE W428F1-IDDC-RET     TO WS-PREV-IDDC                          
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 DD-DETAIL2-LOAD     SECTION.                                             
052800     MOVE W001-TOT-BINNED       TO U3-DOC-KVRETINL                        
052900     MOVE W001-TOT-DEVIATED     TO U3-DOC-KVAVV-KVANT                     
053000     MOVE W001-TOT-SCRAPPED     TO U3-DOC-KVRETINL-SKR                    
053100     MOVE W001-TOT-QUALITY      TO U3-DOC-KVAVV-KVAL                      
053200     MOVE W001-TOT-SUARTSTD     TO U3-DOC-SUARTSTD-INL                    
053300     COMPUTE WS-TOT-CODE = W001-TOT-BINNED + W001-TOT-SCRAPPED            
053400     IF WS-TOT-CODE NOT = 0                                               
053500      COMPUTE WS1-AVGLEAD-TIME =                                          
053600                       WS-SUM-AVTIM / WS-TOT-CODE                         
053700     END-IF                                                               
053800     MOVE WS1-AVGLEAD-TIME      TO U3-DOC-KVDAGDEC                        
053900     MOVE '3         '          TO U3-DOC-IDAFPRCD                        
054000*** SEARCH FROM THE W4285B TABLE NOT BINNED VALUE AND LINES FOR           
054100*** PARTICULAR COMPANY CODE AND IDDC                                      
054200     SET IX    TO  1                                                      
054300                                                                          
054400     SEARCH W4285B-LINE                                                   
054500         AT END                                                           
054600            MOVE ZERO                 TO U3-DOC-SURADER-RET               
054700            MOVE ZERO                 TO U3-DOC-SUARTSTD-RET              
054800     WHEN TAB-KDMFUP(IX) = DAP-KDMFUP AND                                 
054900            TAB-IDDC-RET (IX) = WS-SEARCH-IDDC                            
055000            MOVE TAB-SURADER (IX)     TO U3-DOC-SURADER-RET               
055100            MOVE TAB-SUARTSTD (IX)    TO U3-DOC-SUARTSTD-RET              
055200     END-SEARCH                                                           
055300     INITIALIZE W001-LINE                                                 
055400     MOVE U3-DOC-W428543           TO W001-LINE                           
055500     PERFORM S21-WRITE-W42854-001                                         
055600     INITIALIZE          U3-DOC-W428543                                   
055700     ADD 1    TO WS-TOT-DC                                                
055800     INITIALIZE  WS-TOT-CODE                                              
055900                W001-DETAIL2 WS-SUM-AVTIM                                 
056000     SET LINE2-WRITE                TO TRUE                               
056100     .                                                                    
056200     EJECT                                                                
056300 DC-WRITE-DETAIL1    SECTION.                                             
056400     IF WS-TEMP-CODE NOT EQUAL TO SPACES                                  
056500       MOVE WS-TEMP-CODE        TO U2-DOC-KDANMORS                        
056600       MOVE WS-TEMP-BIN         TO U2-DOC-KVRETINL                        
056700       IF LINE2-WRITE                                                     
056800         MOVE WS-PREV-IDDC      TO U2-DOC-IDDC-RET                        
056900         MOVE WS-PREV-ADCITY    TO U2-DOC-ADCITY                          
057000         SET LINE2-WROTE        TO TRUE                                   
057100       END-IF                                                             
057200       MOVE WS-TEMP-DEV         TO U2-DOC-KVAVV-KVANT                     
057300       MOVE WS-TEMP-SCRAP       TO U2-DOC-KVRETINL-SKR                    
057400       MOVE WS-TEMP-QUAL        TO U2-DOC-KVAVV-KVAL                      
057500       MOVE WS-TEMP-SUARTSTD    TO U2-DOC-SUARTSTD-INL                    
057600       ADD  WS-TEMP-BIN         TO W001-TOT-BINNED                        
057700       ADD  WS-TEMP-DEV         TO W001-TOT-DEVIATED                      
057800       ADD  WS-TEMP-QUAL        TO W001-TOT-QUALITY                       
057900       ADD  WS-TEMP-SCRAP       TO W001-TOT-SCRAPPED                      
058000       ADD  WS-TEMP-SUARTSTD    TO W001-TOT-SUARTSTD                      
058100       COMPUTE WS-CODE-COUNT = WS-TEMP-BIN + WS-TEMP-SCRAP                
058200       IF WS-CODE-COUNT NOT EQUAL TO ZERO                                 
058300         COMPUTE WS0-KVDAGDEC  = (WS-TEMP-AVTIM / WS-CODE-COUNT)          
058400       END-IF                                                             
058500       ADD  WS-TEMP-AVTIM       TO WS-SUM-AVTIM                           
058600       MOVE WS0-KVDAGDEC        TO U2-DOC-KVDAGDEC                        
058700       MOVE '2         '        TO U2-DOC-IDAFPRCD                        
058800       INITIALIZE W001-LINE                                               
058900       MOVE U2-DOC-W428542     TO W001-LINE                               
059000       PERFORM S21-WRITE-W42854-001                                       
059100       INITIALIZE               WS-TEMP-STORAGE-AREA                      
059200                                  WS-CODE-COUNT U2-DOC-W428542            
059300                                  WS0-KVDAGDEC                            
059400                                  W001-DETAIL1                            
059500     END-IF                                                               
059600     .                                                                    
059700     EJECT                                                                
059800 F-SEARCH-IDDC         SECTION.                                           
060000     IF TAB-KDMFUP(SUB-IX) NOT = DAP-KDMFUP                               
060100       PERFORM S20-SKRIV-START-POST                                       
060200       PERFORM S21A-WRITE-HEADERS                                         
060300     END-IF                                                               
060401     .                                                                    
060402     EJECT                                                                
060410 G-PROCESS-W4285B      SECTION.                                           
060900     INITIALIZE U2-DOC-W428542                                            
061000     MOVE TAB-IDDC-RET (SUB-IX)  TO U2-DOC-IDDC-RET                       
061100                                    W-IDDC                                
061200     MOVE '2         '           TO U2-DOC-IDAFPRCD                       
061300     PERFORM IMS-GU-WDB601                                                
061400     IF SEGMENT-FINNS                                                     
061500       MOVE DCS-ADGMT-PADR(11:20) TO U2-DOC-ADCITY                        
061600     END-IF                                                               
061700     INITIALIZE W001-LINE                                                 
061800     MOVE U2-DOC-W428542         TO W001-LINE                             
061900     PERFORM S21-WRITE-W42854-001                                         
062000     INITIALIZE U2-DOC-W428542                                            
062100     MOVE '3         '           TO U3-DOC-IDAFPRCD                       
062200     MOVE TAB-SURADER (SUB-IX)   TO U3-DOC-SURADER-RET                    
062300     MOVE TAB-SUARTSTD (SUB-IX)  TO U3-DOC-SUARTSTD-RET                   
062400     INITIALIZE W001-LINE                                                 
062500     MOVE U3-DOC-W428543         TO W001-LINE                             
062600     PERFORM S21-WRITE-W42854-001                                         
062800     INITIALIZE U3-DOC-SURADER-RET U3-DOC-SUARTSTD-RET                    
063000     .                                                                    
063100     EJECT                                                                
063200 J-AT-END-PROCESS      SECTION.                                           
063300                                                                          
063400     IF WS-TEMP-CODE NOT EQUAL TO SPACES                                  
063500      PERFORM DC-WRITE-DETAIL1                                            
063600      PERFORM DD-DETAIL2-LOAD                                             
063700     END-IF                                                               
063800     .                                                                    
063900     EJECT                                                                
064000 S21-WRITE-W42854-001  SECTION.                                           
064100                                                                          
064200     WRITE W42854-001-LINE FROM W001-LINE                                 
064300     INITIALIZE                 W001-LINE                                 
064400     .                                                                    
064500     EJECT                                                                
066800 S20-SKRIV-START-POST SECTION.                                            
066900*                                                                         
067000     WRITE UT-DAP-POST         FROM DAP-AREA-TYPE                         
067100     MOVE 'DAP'                  TO POSTSUM-TRANSTYP                      
067400     MOVE 'W428541'              TO POSTSUM-FDNAMN                        
067500     MOVE 'W42854D3'             TO POSTSUM-DDNAMN2                       
067600     CALL POSTSUM             USING POSTSUM-PARM                          
067610                                                                          
067630     IF W428F1-KDMFUP   <= TAB-KDMFUP   (SUB-IX)                          
067800       MOVE W428F1-KDMFUP        TO DAP-KDMFUP                            
067810       MOVE W4285A-TISAAVV-INLINL-TIAAVV                                  
067820                                 TO DAP-DATE                              
067900     ELSE                                                                 
068000       MOVE TAB-KDMFUP(SUB-IX)   TO DAP-KDMFUP                            
068100       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
068101       CALL WDATKONV          USING DAT-KDDATFORM DAT-I-TIDATUM           
068102                                    DAT-O-TIDATUM DAT-KDSVAR              
068103                                                                          
068104       IF DAT-KDSVAR-FEL                                                  
068105         MOVE 'DATUMKONVERTERINGEN HAR GÅTT SNETT'                        
068106                                 TO ERROR-TEXT                            
068107         CALL FELLOG                                                      
068108       END-IF                                                             
068109       MOVE DAT-TIAAVV-GRP       TO DAP-DATE                              
068110     END-IF                                                               
068111                                                                          
068120     MOVE DAP-KDMFUP             TO DAP-ACODE                             
068130                                                                          
068900                                                                          
070300     WRITE UT-DAP-POST         FROM DAP-AREA-SUBTYPE                      
070400     MOVE 'DAP'                  TO POSTSUM-TRANSTYP                      
070600     MOVE 'W428541'              TO POSTSUM-FDNAMN                        
070700     MOVE 'W42854D3'             TO POSTSUM-DDNAMN2                       
070800     CALL POSTSUM             USING POSTSUM-PARM                          
070900     .                                                                    
071000     EJECT                                                                
071010 S21A-WRITE-HEADERS SECTION.                                              
071020                                                                          
071040     MOVE '1         '           TO U1-DOC-IDAFPRCD                       
071050     MOVE DAP-DATE               TO U1-DOC-TIAAVV                         
071098     MOVE U1-DOC-W428541         TO W001-LINE                             
071099     PERFORM S21-WRITE-W42854-001                                         
071100     SET HDR-WRITTEN             TO TRUE                                  
071101     INITIALIZE U1-DOC-W428541 WS-PREV-IDDC                               
071102     .                                                                    
071103     EJECT                                                                
071104 Z-FINIT SECTION.                                                         
071200     CLOSE                 W4285A                                         
071300                           W4285B                                         
071400                           W428541                                        
071500     DISPLAY '************************************'                       
071600     DISPLAY '---------SUMMARY OF THE REPORT------'                       
071700     DISPLAY 'TOTAL NO OF REPORTS GENERATED =' WS-TOT-DC                  
071800     DISPLAY '************************************'                       
071900     MOVE 'S'            TO POSTSUM-OPKOD                                 
072000     CALL POSTSUM     USING POSTSUM-PARM                                  
072100     .                                                                    
072200     EJECT                                                                
072300 S01-READ-W4285A  SECTION.                                                
072400     READ W4285A       INTO W428F1-AREA                                   
072500     AT END                                                               
072600       MOVE HIGH-VALUE   TO W428F1-AREA                                   
072700       SET END-OF-W4285A TO TRUE                                          
072800                                                                          
072900     NOT AT END                                                           
073000       MOVE 'W4285A'     TO POSTSUM-FDNAMN                                
073100       MOVE 'W42854D1'   TO POSTSUM-DDNAMN2                               
073200       MOVE SPACES       TO POSTSUM-TRANSTYP                              
073300       CALL POSTSUM   USING POSTSUM-PARM                                  
073500     END-READ                                                             
073600     .                                                                    
073700     EJECT                                                                
073800 S02-READ-W4285B  SECTION.                                                
073900     READ W4285B INTO W4285B-LINE (IX)                                    
074000     AT END                                                               
074100        MOVE HIGH-VALUES  TO W4285B-LINE (IX)                             
074200        SET END-OF-W4285B TO TRUE                                         
074300                                                                          
074400     NOT AT END                                                           
074500        SET IX UP BY 1                                                    
074600        MOVE 'W4285B'     TO POSTSUM-FDNAMN                               
074700        MOVE 'W42854D2'   TO POSTSUM-DDNAMN2                              
074800        MOVE SPACES       TO  POSTSUM-TRANSTYP                            
074900        CALL POSTSUM    USING POSTSUM-PARM                                
075000     END-READ                                                             
075100     .                                                                    
075200     EJECT                                                                
131800 IMS-GU-WDB601    SECTION.                                                
131900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
132000          DELIMITED BY SIZE INTO SSA1                                     
132100     MOVE '  GE' TO GODK-STATUSKODER                                      
132200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601    SSA1                 
132300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
132400     PERFORM IMS-STATUSKONTROLL                                           
132500     IF SEGMENT-SAKNAS                                                    
132600         MOVE SPACE TO DCS-KDDC                                           
132700     END-IF                                                               
132800     .                                                                    
138700 IMS-STATUSKONTROLL SECTION.                                              
138800                                                                          
138900     SET STATUS-IX TO 1                                                   
139000     SEARCH GODK-STATUS                                                   
139100       AT END                                                             
139200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
139300         DELIMITED BY SIZE INTO FELTEXT                                   
139400         CALL FELLOG                                                      
139500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
139600         CONTINUE                                                         
139700     END-SEARCH                                                           
139800     .                                                                    
