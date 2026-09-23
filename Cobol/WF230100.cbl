000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF230100.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   2011-11-25.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THE PGM                                                          
001000*        - READS FILE WITH DOCUMENT DATA RECORDS                          
001100*        - WRITES DOCUMENT DATA RECORDS FOR NONVCC TO A FILE.             
001200*        - USES WZ14DAP4 IN THE JCL TO SEND THE FILE TO                   
001300*          DISTRIBUTION & PRINT                                           
001400*                                                                         
001500*        INDEX DESCRIBING DIFFERENT LINE-TYPES (IDAFPRCD),                
001600**** DOCUMENT ALWAYS STARTS WITH A HEADER 1                               
001700*        1        HEADER   - STANDARD                                     
001800**** AFTER HEADER COMES ONE TO MANY DETAILED ROWS 2                       
001900*        2        DETAIL   - STANDARD                                     
002000*        2.1      DETAIL   - BREAK                                        
002100*        2.1 TRAC DETAIL   - BREAK                                        
002200*        2.1 SOFT DETAIL   - BREAK, SOFTWARE                              
002300*        2.3      DETAIL   - BREAK, PACKING-HANDLING/FREIGTH ETC          
002400*        2.4      DETAIL   - BREAK, LOCAL CURRENCY ON INVOICE             
002500**** SUMMATION OF THE INVOICE IS DONE IN THE FOOTER 3                     
002600*        3        FOOTER   - STANDARD                                     
002700*        3.1      FOOTER   - LOCAL CURRENCY                               
002800*        3.2      FOOTER   - FOREIGN CURRENCY                             
002900**** ADDITIONAL APPENDIX 4, 5, 6                                          
003000*        4        APPENDIX - STANDARD                                     
003100*        4.1      APPENDIX - BREAK                                        
003200*        5        APPENDIX - STANDARD, INSURANCE TEXT                     
003300*        5.1      APPENDIX - BREAK, INSURANCE TEXT                        
003400*        6        APPENDIX - STANDARD, CREDIT INFO/DEL. ADDRESS           
003500*        6.1      APPENDIX - BREAK, CREDIT INFO/DEL. ADDRESS              
003600*                                                                         
003700*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
003800*                                                                         
003900*                                                                         
004000 ENVIRONMENT DIVISION.                                                    
004100                                                                          
004200 INPUT-OUTPUT SECTION.                                                    
004300                                                                          
004400 FILE-CONTROL.                                                            
004500*          --- DOCUMENT DATA RECORDS                                      
004600     SELECT WF2031                     ASSIGN TO WF2301D1.                
004700*          --- DOCUMENT DATA RECORDS - OUTPUT                             
004800     SELECT WF2301                     ASSIGN TO WF2301D2.                
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100                                                                          
005200 FILE SECTION.                                                            
005300 FD  WF2031                                                               
005400     RECORDING       V                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700 01  IN-DATA.                                                             
005800     03  IN-IDPTYP               PIC X(3).                                
005900     03  FILLER                  PIC X(16).                               
006000     03  IN-IDLEGSEL             PIC X(4).                                
006100     03  FILLER                  PIC X(5672).                             
006200     EJECT                                                                
006300                                                                          
006400 FD  WF2301                                                               
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800 01  WF2301-001                  PIC X(3000).                             
006900     EJECT                                                                
007000 WORKING-STORAGE SECTION.                                                 
007100                                                                          
007200 77  IDPGM                       PIC X(8)    VALUE 'WF230100'.            
007300 77  YES                         PIC X(1)    VALUE 'J'.                   
007400 77  NOO                         PIC X(1)    VALUE 'N'.                   
007500 77  WS-ZERO                     PIC X(5)    VALUE '00000'.               
007600 77  WS-NINE                     PIC X(5)    VALUE '99999'.               
007700 77  WS-EIGHT                    PIC X(5)    VALUE '88888'.               
007800 77  WS-DOC-HEAD                 PIC X(3)    VALUE '1  '.                 
007900 77  WS-DOC-LINE                 PIC X(3)    VALUE '2  '.                 
008000 77  WS-DOC-LINE-SOFT            PIC X(8)    VALUE '2   SOFT'.            
008100 77  WS-DOC-LINE-TRAC            PIC X(8)    VALUE '2   TRAC'.            
008200 77  WS-DOC-GROUP                PIC X(3)    VALUE '2.1'.                 
008300 77  WS-DOC-GROUP-SOFT           PIC X(8)    VALUE '2.1 SOFT'.            
008400 77  WS-DOC-GROUP-TRAC           PIC X(8)    VALUE '2.1 TRAC'.            
008500 77  WS-DOC-FIPH-SUM             PIC X(3)    VALUE '2.3'.                 
008600 77  WS-DOC-LOCC-JUS             PIC X(3)    VALUE '2.4'.                 
008700 77  WS-DOC-FOOTER               PIC X(3)    VALUE '3  '.                 
008800 77  WS-DOC-FOOTER-LOC           PIC X(3)    VALUE '3.1'.                 
008900 77  WS-DOC-FOOTER-SND           PIC X(3)    VALUE '3.2'.                 
009000 77  WS-DOC-NEW-APPENDIX         PIC X(3)    VALUE '4.1'.                 
009100 77  WS-DOC-APPENDIX             PIC X(3)    VALUE '4  '.                 
009200 77  WS-DOC-INSURANCE            PIC X(3)    VALUE '5.1'.                 
009300 77  WS-DOC-INSURANCE-ROW        PIC X(3)    VALUE '5  '.                 
009400 77  WS-DOC-INSURANCE-ADD        PIC X(3)    VALUE '5.2'.                 
009500 77  WS-DOC-ADD-INSURANCE        PIC X(3)    VALUE '5.3'.                 
009600 77  WS-DOC-ADD-INSURANCE-ROW    PIC X(3)    VALUE '5.4'.                 
009700 77  WS-DOC-CREDIT               PIC X(3)    VALUE '6.1'.                 
009800 77  WS-DOC-CREDIT-ROW           PIC X(3)    VALUE '6  '.                 
009900 77  WS-DOC-DADR                 PIC X(3)    VALUE '6.1'.                 
010000 77  WS-DOC-DADR-ROW             PIC X(3)    VALUE '6  '.                 
010100 77  WS-IX                       PIC S9(3)   COMP-3 VALUE ZERO.           
010200 77  WS-IDFINDOC                 PIC 9(9)    VALUE ZERO.                  
010300 77  WS-IDFINDOC1                PIC 9(9)    VALUE ZERO.                  
010400 77  WS-KDFINDOC                 PIC X(4)    VALUE SPACE.                 
010500 77  WS-IDREF                    PIC X(15)   VALUE SPACE.                 
010600 77  WS-BETEXT                   PIC X(50)   VALUE SPACE.                 
010700 77  WS-BEANST                   PIC X(35)   VALUE SPACE.                 
010800 77  WS-IDEXCUST-2-OLD           PIC X(15)   VALUE SPACE.                 
010900 77  WS-IDFINDOC-OLD             PIC 9(9)    VALUE ZERO.                  
011000 77  WS-IDREF-OLD                PIC X(15)   VALUE SPACE.                 
011100 77  WS-DAREFDAT-OLD             PIC 9(8)    VALUE ZERO.                  
011200 77  WS-KDFRAKT                  PIC 9(2)    VALUE ZERO.                  
011300 77  WS-HEAD-PRKURS            PIC S9(6)V9(5) COMP-3 VALUE ZERO.          
011400 77  WS-SUNTO-SND-LOC          PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
011500 77  WS2-PRARTBTO              PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
011600 77  WS2-KVLEVART              PIC S9(11) COMP-3 VALUE ZERO.              
011700 77  WS-LOCC-JUS-SUNTO         PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
011800 77  WS2-LOCC-JUS-SUNTO        PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
011900 77  WS-LOCC-JUS-CHECK         PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
012000                                                                          
012100 77  WF2031-FIRST-RECORD         PIC X       VALUE ' '.                   
012200     88  FIRST-RECORD                        VALUE 'J'.                   
012300     88  OTHER-RECORDS                       VALUE 'N'.                   
012400                                                                          
012500 77  WF2031-EOF-SW               PIC X       VALUE 'N'.                   
012600     88  END-OF-WF2031                       VALUE 'J'.                   
012700                                                                          
012800 77  NEW-HEAD-SW                 PIC X       VALUE 'J'.                   
012900     88  NEW-HEAD                            VALUE 'J'.                   
013000     88  OTHER-ROW                           VALUE 'N'.                   
013100                                                                          
013200 77  NEW-FIPH-SUM-ROW-SW         PIC X       VALUE 'N'.                   
013300     88  NEW-FIPH-SUM-ROW                    VALUE 'J'.                   
013400     88  NON-FIPH-SUM-ROW                    VALUE 'N'.                   
013500                                                                          
013600 77  NEW-LOCC-JUS-ROW-SW         PIC X       VALUE 'N'.                   
013700     88  NEW-LOCC-JUS-ROW                    VALUE 'J'.                   
013800     88  NON-LOCC-JUS-ROW                    VALUE 'N'.                   
013900                                                                          
014000 77  NEW-APPENDIX-SW             PIC X       VALUE 'J'.                   
014100     88  NEW-APPENDIX                        VALUE 'J'.                   
014200                                                                          
014300 77  NEW-INSURANCE-SW            PIC X       VALUE 'N'.                   
014400     88  NEW-INSURANCE                       VALUE 'J'.                   
014500     88  NO-INSURANCE                        VALUE 'N'.                   
014600                                                                          
014700 77  NEW-ADD-INSURANCE-SW        PIC X       VALUE 'N'.                   
014800     88  NEW-ADD-INSURANCE                   VALUE 'J'.                   
014900     88  NO-ADD-INSURANCE                    VALUE 'N'.                   
015000                                                                          
015100 77  NEW-CREDIT-SW               PIC X       VALUE 'N'.                   
015200     88  NEW-CREDIT                          VALUE 'J'.                   
015300     88  NO-CREDIT                           VALUE 'N'.                   
015400                                                                          
015500 77  NEW-DADR-SW                 PIC X       VALUE 'N'.                   
015600     88  NEW-DADR                            VALUE 'J'.                   
015700     88  NO-DADR                             VALUE 'N'.                   
015800                                                                          
015900 77  WS-FOOT-LOC-SW              PIC X       VALUE 'N'.                   
016000     88  NEW-FOOT-LOC                        VALUE 'J'.                   
016100     88  NO-FOOT-LOC                         VALUE 'N'.                   
016200                                                                          
016300 77  WS-FOOT-SND-SW              PIC X       VALUE 'N'.                   
016400     88  NEW-FOOT-SND                        VALUE 'J'.                   
016500     88  NO-FOOT-SND                         VALUE 'N'.                   
016600                                                                          
016700 01  WS-TIMESTAMP.                                                        
016800     03  FILLER                  PIC X       VALUE 'D'.                   
016900     03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
017000*    03  FILLER                  PIC X       VALUE '-'.                   
017100     03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
017200*    03  FILLER                  PIC X       VALUE '-'.                   
017300     03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
017400     03  FILLER                  PIC X       VALUE '_'.                   
017500     03  FILLER                  PIC X       VALUE 'T'.                   
017600     03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
017700*    03  FILLER                  PIC X       VALUE ':'.                   
017800     03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
017900*    03  FILLER                  PIC X       VALUE ':'.                   
018000     03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
018100*    03  FILLER                  PIC X       VALUE '.'.                   
018200*    03  WS-DECIMAL              PIC X(2)    VALUE SPACE.                 
018300*    03  FILLER                  PIC X(4)    VALUE '0000'.                
018400                                                                          
018500 01  WS-TABELL-FIPH-SUM.                                                  
018600     03 TABELL OCCURS 35.                                                 
018700       05 WS-BEART               PIC X(25).                               
018800       05 WS-KVLEVART            PIC S9(6) COMP-3.                        
018900       05 WS-SUNTO               PIC S9(11)V9(2) COMP-3.                  
019000                                                                          
019100 01  ERRTEXT.                                                             
019200     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
019300     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
019400 01  KDRC-DISPLAY                PIC Z(5).                                
019500     EJECT                                                                
019600                                                                          
019700 01  GENERAL-SUBPROGRAMS.                                                 
019800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
019900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
020000     EJECT                                                                
020100                                                                          
020200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
020300                                                                          
020400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
020500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
020600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
020700     EJECT                                                                
020800                                                                          
020900*    --- IN-AREOR                                                         
021000 01  INPUT-AREA                 PIC X(24)   VALUE                         
021100                                'INPUT-AREA     '.                        
021200 01  IN-AREA-HEAD.                                                        
021300*    03  -COPY WF201101                                                   
021400                                                                          
021500 01  IN-AREA-LINE.                                                        
021600*    03  -COPY WF201102                                                   
021700                                                                          
021800 01  IN-AREA-FOOT.                                                        
021900*    03  -COPY WF201103                                                   
022000                                                                          
022100 01  IN-AREA-APPX.                                                        
022200*    03  -COPY WF201104                                                   
022300     EJECT                                                                
022400                                                                          
022500 01  W001-DAP.                                                            
022600     03  FILLER              PIC X(165)  VALUE SPACE.                     
022700     EJECT                                                                
022800*    --- UT-AREOR                                                         
022900 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
023000                                'OUTPUT-AREA     '.                       
023100 01  WS-HDR-AREA-1.                                                       
023200     03 WS-IDOUTTYPE             PIC X(15).                               
023300     EJECT                                                                
023400 01  WS-HDR-AREA-2.                                                       
023500     03 WS-IDOUTREC              PIC X(30).                               
023600     EJECT                                                                
023700 01  WS-HDR-AREA-3.                                                       
023800     03 WS-IDLIST                PIC X(10).                               
023900     EJECT                                                                
024000 01  WS-HDR-AREA-4.                                                       
024100     03 WS-DAFINDOC              PIC Z(8).                                
024200     EJECT                                                                
024300 01  WS-HDR-AREA-5.                                                       
024400     03 WS-IDPARTNR              PIC X(9).                                
024500     EJECT                                                                
024600 01  WS-HDR-AREA-6.                                                       
024700     03 WS-IDEXCUST-1            PIC X(15).                               
024800     EJECT                                                                
024900 01  WS-HDR-AREA-7.                                                       
025000     03 WS-IDFINDOC2             PIC Z(9).                                
025100     EJECT                                                                
025200 01  WS-HDR-AREA-8.                                                       
025300     03 WS-IDDC                  PIC X(2).                                
025400     EJECT                                                                
025500 01  WS-HDR-AREA-9.                                                       
025600     03 WS-BEBET-NAME1           PIC X(35).                               
025700     EJECT                                                                
025800 01  WS-HDR-AREA-10.                                                      
025900     03 WS-KDTRADP               PIC X(4).                                
026000     EJECT                                                                
026100                                                                          
026200 01  DOC-HEAD-AREA.                                                       
026300*    03  -COPY WF210101                                                   
026400     EJECT                                                                
026500                                                                          
026600 01  DOC-LINE-AREA.                                                       
026700*    03  -COPY WF210102                                                   
026800     EJECT                                                                
026900                                                                          
027000 01  DOC-FOOT-AREA.                                                       
027100*    03  -COPY WF210103                                                   
027200     EJECT                                                                
027300                                                                          
027400 01  DOC-APPX-AREA.                                                       
027500*    03  -COPY WF210104                                                   
027600     EJECT                                                                
027700                                                                          
027800 01  DOC-INSUR.                                                           
027900*    03  -COPY WF210106                                                   
028000     EJECT                                                                
028100                                                                          
028200 01  DOC-CREDIT.                                                          
028300*    03  -COPY WF210107                                                   
028400     EJECT                                                                
028500                                                                          
028600 01  DOC-DADR.                                                            
028700*    03  -COPY WF210108                                                   
028800     EJECT                                                                
028900                                                                          
029000 01  DOC-FIPH-SUM.                                                        
029100*    03  -COPY WF210109                                                   
029200     EJECT                                                                
029300                                                                          
029400 01  DOC-LOCC-JUS.                                                        
029500*    03  -COPY WF210110                                                   
029600     EJECT                                                                
029700                                                                          
029800 01  DOC-ADD-INSUR.                                                       
029900*    03  -COPY WF210112                                                   
030000     EJECT                                                                
030100                                                                          
030200*    --- SAVE-AREOR                                                       
030300 01  SAVE-AREA                 PIC X(24)   VALUE                          
030400                                'SAVE-AREA     '.                         
030500                                                                          
030600 01  SAVE-DP-INSUR.                                                       
030700*    03  -COPY WF210106 -PRE SAVE-                                        
030800     EJECT                                                                
030900                                                                          
031000 01  SAVE-DP-CREDI.                                                       
031100*    03  -COPY WF210107 -PRE SAVE-                                        
031200     EJECT                                                                
031300                                                                          
031400 01  SAVE-DP-DADR.                                                        
031500*    03  -COPY WF210108 -PRE SAVE-                                        
031600     EJECT                                                                
031700                                                                          
031800 01  SAVE-FIPH-SUM.                                                       
031900*    03  -COPY WF210109 -PRE SAVE-                                        
032000     EJECT                                                                
032100                                                                          
032200 01  SAVE-LOCC-JUS.                                                       
032300*    03  -COPY WF210110 -PRE SAVE-                                        
032400     EJECT                                                                
032500                                                                          
032600 01  SAVE-DP-ADD-INSUR.                                                   
032700*    03  -COPY WF210112 -PRE SAVE-                                        
032800     EJECT                                                                
032900                                                                          
033000                                                                          
033100 LINKAGE SECTION.                                                         
033200                                                                          
033300 PROCEDURE DIVISION.                                                      
033400 MAIN SECTION.                                                            
033500                                                                          
033600     PERFORM A-INIT                                                       
033700     PERFORM B-EXECUTE                                                    
033800     PERFORM Z-FINIT                                                      
033900                                                                          
034000     MOVE ZERO TO RETURN-CODE                                             
034100     GOBACK                                                               
034200     .                                                                    
034300                                                                          
034400 A-INIT SECTION.                                                          
034500     OPEN INPUT WF2031                                                    
034600     OPEN OUTPUT WF2301                                                   
034700                                                                          
034800     MOVE NOO TO NEW-INSURANCE-SW                                         
034900     MOVE NOO TO NEW-ADD-INSURANCE-SW                                     
035000     MOVE NOO TO NEW-CREDIT-SW                                            
035100     MOVE NOO TO NEW-DADR-SW                                              
035200     MOVE NOO TO WS-FOOT-LOC-SW                                           
035300     MOVE NOO TO WS-FOOT-SND-SW                                           
035400     MOVE ZERO TO WS-IX                                                   
035500                                                                          
035600     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
035700     MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
035800     MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
035900     MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
036000     MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
036100     MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
036200*    MOVE FUNCTION CURRENT-DATE (15:6) TO WS-DECIMAL                      
036300     .                                                                    
036400                                                                          
036500 B-EXECUTE SECTION.                                                       
036600     PERFORM S01-READ-WF2031                                              
036700                                                                          
036800     IF END-OF-WF2031                                                     
036900       CONTINUE                                                           
037000     ELSE                                                                 
037100       MOVE YES TO WF2031-FIRST-RECORD                                    
037200                                                                          
037300       PERFORM UNTIL END-OF-WF2031                                        
037400           IF IN-IDPTYP = WS-DOC-HEAD                                     
037500             MOVE YES TO NEW-HEAD-SW                                      
037600             MOVE +1 TO WS-IX                                             
037700             PERFORM UNTIL WS-IX > 35                                     
037800               MOVE SPACE          TO WS-BEART(WS-IX)                     
037900               MOVE 0              TO WS-KVLEVART(WS-IX)                  
038000               MOVE 0              TO WS-SUNTO(WS-IX)                     
038100               ADD +1 TO WS-IX                                            
038200             END-PERFORM                                                  
038300             IF FIRST-RECORD                                              
038400               MOVE NOO TO WF2031-FIRST-RECORD                            
038500             ELSE                                                         
038600*IF THE PREVIOUS RECORD HAS AN INSURANCE TEXT TO BE PRINTED               
038700               IF NEW-INSURANCE                                           
038800                 PERFORM BF-HANDLE-INSUR                                  
038900               END-IF                                                     
039000*IF THE PREVIOUS RECORD HAS AN ADDITIONAL INSURANCE TEXT TO BE            
039100*PRINTED                                                                  
039200               IF NEW-ADD-INSURANCE                                       
039300                 PERFORM BF-HANDLE-ADD-INSUR                              
039400               END-IF                                                     
039500*IF THE PREVIOUS RECORD HAS A CREDIT TEXT TO BE PRINTED                   
039600               IF NEW-CREDIT                                              
039700                 PERFORM BG-HANDLE-CREDIT                                 
039800               END-IF                                                     
039900*IF THE PREVIOUS RECORD HAS A DELIVERY ADRESS TO BE PRINTED               
040000               IF NEW-DADR                                                
040100                 PERFORM BJ-HANDLE-DADR                                   
040200               END-IF                                                     
040300             END-IF                                                       
040400             PERFORM BA-HANDLE-HEADER                                     
040500           ELSE                                                           
040600             IF IN-IDPTYP = WS-DOC-LINE                                   
040700               PERFORM BB-HANDLE-LINES                                    
040800             ELSE                                                         
040900               IF IN-IDPTYP = WS-DOC-FOOTER                               
041000                 PERFORM BC-HANDLE-FOOTER                                 
041100               ELSE                                                       
041200                 IF IN-IDPTYP = WS-DOC-APPENDIX                           
041300                   PERFORM BD-HANDLE-APPENDIX                             
041400                 END-IF                                                   
041500               END-IF                                                     
041600             END-IF                                                       
041700           END-IF                                                         
041800         PERFORM S01-READ-WF2031                                          
041900       END-PERFORM                                                        
042000                                                                          
042100*IF THE LAST RECORD HAS AN INSURANCE TEXT TO BE PRINTED                   
042200       IF NEW-INSURANCE                                                   
042300         PERFORM BF-HANDLE-INSUR                                          
042400       END-IF                                                             
042500                                                                          
042600*IF THE PREVIOUS RECORD HAS AN ADDITIONAL INSURANCE TEXT TO BE            
042700*PRINTED                                                                  
042800       IF NEW-ADD-INSURANCE                                               
042900         PERFORM BF-HANDLE-ADD-INSUR                                      
043000       END-IF                                                             
043100*IF THE LAST RECORD HAS A CREDIT TEXT TO BE PRINTED                       
043200       IF NEW-CREDIT                                                      
043300         PERFORM BG-HANDLE-CREDIT                                         
043400       END-IF                                                             
043500                                                                          
043600*IF THE LAST RECORD HAS A DELIVERY ADRESS TO BE PRINTED                   
043700       IF NEW-DADR                                                        
043800         PERFORM BJ-HANDLE-DADR                                           
043900       END-IF                                                             
044000                                                                          
044100     END-IF                                                               
044200     .                                                                    
044300                                                                          
044400 BA-HANDLE-HEADER SECTION.                                                
044500     MOVE 'J'                        TO NEW-APPENDIX-SW                   
044600     MOVE SPACE                      TO WS-IDEXCUST-2-OLD                 
044700     MOVE IN-DATA                    TO IN-AREA-HEAD                      
044800                                                                          
044900     MOVE HEAD-BEFORMS               TO WS-IDOUTTYPE                      
045000     MOVE SPACE                      TO WS-IDOUTREC                       
045100* DENNA KOD ÄR FÖR ATT SKRIVA VALUTAJUSTERINGEN FÖR FAKTUROR SOM          
045200* ÄR OMRÄKNADE TILL LOKAL VALUTA                                          
045300     MOVE ZERO TO WS-LOCC-JUS-SUNTO                                       
045400*                                                                         
045500* DENNA KOD HÄRRÖR FRÅN ÄT GÄLLANDE MIN-GRÄNS FÖR                         
045600* UTSKRIFT AV KREDITNOTA                                                  
045700     IF HEAD-KDFINDOC = 'CR'                                              
045800       IF HEAD-SUBTO-TOT < HEAD-SUDOCLIM                                  
045900         MOVE 'X2'                   TO WS-IDOUTREC(1:2)                  
046000         MOVE HEAD-IDLANDX3-BET(1:2) TO WS-IDOUTREC(3:2)                  
046100       ELSE                                                               
046200         MOVE 'X3'                   TO WS-IDOUTREC(1:2)                  
046300         MOVE HEAD-IDLANDX3-BET(1:2) TO WS-IDOUTREC(3:2)                  
046400       END-IF                                                             
046500     ELSE                                                                 
046600       MOVE HEAD-IDLANDX3-SEND(1:2)  TO WS-IDOUTREC(1:2)                  
046700       MOVE HEAD-IDLANDX3-BET(1:2)   TO WS-IDOUTREC(3:2)                  
046800     END-IF                                                               
046900     IF  HEAD-FLSOFT        = 'J'                                         
047000     AND HEAD-IDLANDX3-SEND = 'SE'                                        
047100       MOVE WS-NINE                  TO WS-IDOUTREC(5:5)                  
047200     ELSE                                                                 
047300       IF HEAD-KDFINDOC = 'CR'                                            
047400         MOVE SPACE                  TO HEAD-IDLEVNR                      
047500       END-IF                                                             
047600       IF HEAD-IDLEVNR > SPACE                                            
047700         MOVE HEAD-IDLEVNR           TO WS-IDOUTREC(5:5)                  
047800       ELSE                                                               
047900         MOVE WS-ZERO                TO WS-IDOUTREC(5:5)                  
048000       END-IF                                                             
048100     END-IF                                                               
048200     IF  HEAD-FLFREE        = 'J'                                         
048300       MOVE WS-EIGHT                 TO WS-IDOUTREC(5:5)                  
048400     ELSE                                                                 
048500       MOVE ZERO                     TO WS-IDOUTREC(5:5)                  
048600     END-IF                                                               
048700     MOVE HEAD-IDPARTNR              TO WS-IDOUTREC(10:9)                 
048800* TO ADD KDFRAKT/FC. APPLICABLE FOR INVOICE, INTERNAL                     
048900     IF (HEAD-KDFINDOC = 'INV' AND                                        
049000        HEAD-BEFORMS = 'INVOICE') OR                                      
049100        (HEAD-KDFINDOC = 'INT' AND                                        
049200        HEAD-BEFORMS = 'INTERNAL')                                        
049300       MOVE HEAD-KDFRAKT             TO WS-KDFRAKT                        
049400       MOVE WS-KDFRAKT               TO WS-IDOUTREC(19:2)                 
049500     END-IF                                                               
049600                                                                          
049700     MOVE HEAD-IDFINDOC              TO WS-IDLIST                         
049800                                        WS-IDFINDOC1                      
049900                                        WS-IDFINDOC2                      
050000     MOVE HEAD-DAFINDOC              TO WS-DAFINDOC                       
050100     MOVE HEAD-IDPARTNR              TO WS-IDPARTNR                       
050200     MOVE HEAD-BEBET-NAME1           TO WS-BEBET-NAME1                    
050300     MOVE HEAD-KDTRADP               TO WS-KDTRADP                        
050400     MOVE HEAD-KDFINDOC              TO WS-KDFINDOC                       
050500                                                                          
050600     PERFORM S90-WRITE-HEADER                                             
050700                                                                          
050800     MOVE HEAD-IDPTYP                TO DP-HEAD-IDAFPRCD                  
050900     MOVE HEAD-IDFINDOC              TO DP-HEAD-IDFINDOC                  
051000                                        DP-APPX-IDFINDOC                  
051100                                        DP-INSUR-IDFINDOC                 
051200                                        DP-ADD-IDFINDOC                   
051300                                        DP-CREDI-IDFINDOC                 
051400                                        DP-DADR-IDFINDOC                  
051500     MOVE HEAD-DAFINDOC              TO DP-HEAD-DAFINDOC                  
051600                                        DP-APPX-DAFINDOC                  
051700                                        DP-INSUR-DAFINDOC                 
051800                                        DP-ADD-DAFINDOC                   
051900                                        DP-CREDI-DAFINDOC                 
052000                                        DP-DADR-DAFINDOC                  
052100     MOVE HEAD-BELEGRAD-1            TO DP-HEAD-BELEGRAD-1                
052200                                        DP-APPX-BELEGRAD-1                
052300                                        DP-INSUR-BELEGRAD-1               
052400                                        DP-ADD-BELEGRAD-1                 
052500                                        DP-CREDI-BELEGRAD-1               
052600                                        DP-DADR-BELEGRAD-1                
052700     MOVE HEAD-BELEGRAD-2            TO DP-HEAD-BELEGRAD-2                
052800                                        DP-APPX-BELEGRAD-2                
052900                                        DP-INSUR-BELEGRAD-2               
053000                                        DP-ADD-BELEGRAD-2                 
053100                                        DP-CREDI-BELEGRAD-2               
053200                                        DP-DADR-BELEGRAD-2                
053300     MOVE HEAD-ADLEG-STREET          TO DP-HEAD-ADLEG-STREET              
053400                                        DP-APPX-ADLEG-STREET              
053500                                        DP-INSUR-ADLEG-STREET             
053600                                        DP-ADD-ADLEG-STREET               
053700                                        DP-CREDI-ADLEG-STREET             
053800                                        DP-DADR-ADLEG-STREET              
053900     MOVE HEAD-ADLEG-BOX             TO DP-HEAD-ADLEG-BOX                 
054000                                        DP-APPX-ADLEG-BOX                 
054100                                        DP-INSUR-ADLEG-BOX                
054200                                        DP-ADD-ADLEG-BOX                  
054300                                        DP-CREDI-ADLEG-BOX                
054400                                        DP-DADR-ADLEG-BOX                 
054500     MOVE HEAD-ADLEG-CITY            TO DP-HEAD-ADLEG-CITY                
054600                                        DP-APPX-ADLEG-CITY                
054700                                        DP-INSUR-ADLEG-CITY               
054800                                        DP-ADD-ADLEG-CITY                 
054900                                        DP-CREDI-ADLEG-CITY               
055000                                        DP-DADR-ADLEG-CITY                
055100     MOVE HEAD-ADLEG-PCODE           TO DP-HEAD-ADLEG-PCODE               
055200                                        DP-APPX-ADLEG-PCODE               
055300                                        DP-INSUR-ADLEG-PCODE              
055400                                        DP-ADD-ADLEG-PCODE                
055500                                        DP-CREDI-ADLEG-PCODE              
055600                                        DP-DADR-ADLEG-PCODE               
055700     MOVE HEAD-BELAND-LEG            TO DP-HEAD-BELAND-LEG                
055800                                        DP-APPX-BELAND-LEG                
055900                                        DP-INSUR-BELAND-LEG               
056000                                        DP-ADD-BELAND-LEG                 
056100                                        DP-CREDI-BELAND-LEG               
056200                                        DP-DADR-BELAND-LEG                
056300     MOVE HEAD-IDTFN-LEG             TO DP-HEAD-IDTFN-LEG                 
056400                                        DP-APPX-IDTFN-LEG                 
056500                                        DP-INSUR-IDTFN-LEG                
056600                                        DP-ADD-IDTFN-LEG                  
056700                                        DP-CREDI-IDTFN-LEG                
056800                                        DP-DADR-IDTFN-LEG                 
056900     MOVE HEAD-IDTFX-LEG             TO DP-HEAD-IDTFX-LEG                 
057000                                        DP-APPX-IDTFX-LEG                 
057100                                        DP-INSUR-IDTFX-LEG                
057200                                        DP-ADD-IDTFX-LEG                  
057300                                        DP-CREDI-IDTFX-LEG                
057400                                        DP-DADR-IDTFX-LEG                 
057500     MOVE HEAD-IDBG-LEG              TO DP-HEAD-IDBG-LEG                  
057600                                        DP-APPX-IDBG-LEG                  
057700                                        DP-INSUR-IDBG-LEG                 
057800                                        DP-ADD-IDBG-LEG                   
057900                                        DP-CREDI-IDBG-LEG                 
058000                                        DP-DADR-IDBG-LEG                  
058100     MOVE HEAD-IDPG-LEG              TO DP-HEAD-IDPG-LEG                  
058200                                        DP-APPX-IDPG-LEG                  
058300                                        DP-INSUR-IDPG-LEG                 
058400                                        DP-ADD-IDPG-LEG                   
058500                                        DP-CREDI-IDPG-LEG                 
058600                                        DP-DADR-IDPG-LEG                  
058700     MOVE HEAD-IDVAT-LEG             TO DP-HEAD-IDVAT-LEG                 
058800                                        DP-APPX-IDVAT-LEG                 
058900                                        DP-INSUR-IDVAT-LEG                
059000                                        DP-ADD-IDVAT-LEG                  
059100                                        DP-CREDI-IDVAT-LEG                
059200                                        DP-DADR-IDVAT-LEG                 
059300     MOVE HEAD-IDVAT-AGENT           TO DP-HEAD-IDVAT-AGENT               
059400                                        DP-APPX-IDVAT-AGENT               
059500                                        DP-INSUR-IDVAT-AGENT              
059600                                        DP-ADD-IDVAT-AGENT                
059700                                        DP-CREDI-IDVAT-AGENT              
059800                                        DP-DADR-IDVAT-AGENT               
059900     MOVE HEAD-BERESPRA-1            TO DP-HEAD-BERESPRA-1                
060000                                        DP-APPX-BERESPRA-1                
060100                                        DP-INSUR-BERESPRA-1               
060200                                        DP-ADD-BERESPRA-1                 
060300                                        DP-CREDI-BERESPRA-1               
060400                                        DP-DADR-BERESPRA-1                
060500     MOVE HEAD-IDTFN-RESP            TO DP-HEAD-IDTFN-RESP                
060600                                        DP-APPX-IDTFN-RESP                
060700                                        DP-INSUR-IDTFN-RESP               
060800                                        DP-ADD-IDTFN-RESP                 
060900                                        DP-CREDI-IDTFN-RESP               
061000                                        DP-DADR-IDTFN-RESP                
061100     MOVE HEAD-IDTFX-RESP            TO DP-HEAD-IDTFX-RESP                
061200                                        DP-APPX-IDTFX-RESP                
061300                                        DP-INSUR-IDTFX-RESP               
061400                                        DP-ADD-IDTFX-RESP                 
061500                                        DP-CREDI-IDTFX-RESP               
061600                                        DP-DADR-IDTFX-RESP                
061700     MOVE HEAD-IDVAT-RESP            TO DP-HEAD-IDVAT-RESP                
061800                                        DP-APPX-IDVAT-RESP                
061900                                        DP-INSUR-IDVAT-RESP               
062000                                        DP-ADD-IDVAT-RESP                 
062100                                        DP-CREDI-IDVAT-RESP               
062200                                        DP-DADR-IDVAT-RESP                
062300     MOVE HEAD-KDVALISO              TO DP-HEAD-KDVALISO                  
062400                                        DP-APPX-KDVALISO                  
062500                                        DP-INSUR-KDVALISO                 
062600                                        DP-ADD-KDVALISO                   
062700                                        DP-CREDI-KDVALISO                 
062800                                        DP-DADR-KDVALISO                  
062900     IF HEAD-KDVALISO-LOC NOT = HEAD-KDVALISO                             
063000       IF   HEAD-REVALUTA > 1                                             
063100         COMPUTE HEAD-PRKURS-LOC ROUNDED =                                
063200                 HEAD-PRKURS-LOC / HEAD-REVALUTA                          
063300       END-IF                                                             
063400       MOVE HEAD-PRKURS-LOC          TO DP-HEAD-PRKURS                    
063500                                        DP-APPX-PRKURS                    
063600                                        DP-INSUR-PRKURS                   
063700                                        DP-ADD-PRKURS                     
063800                                        DP-CREDI-PRKURS                   
063900                                        DP-DADR-PRKURS                    
064000                                        WS-HEAD-PRKURS                    
064100     ELSE                                                                 
064200       MOVE 1                        TO DP-HEAD-PRKURS                    
064300                                        DP-APPX-PRKURS                    
064400                                        DP-INSUR-PRKURS                   
064500                                        DP-ADD-PRKURS                     
064600                                        DP-CREDI-PRKURS                   
064700                                        DP-DADR-PRKURS                    
064800                                        WS-HEAD-PRKURS                    
064900     END-IF                                                               
065000     MOVE HEAD-IDPARTNR              TO DP-HEAD-IDPARTNR                  
065100                                        DP-APPX-IDPARTNR                  
065200                                        DP-INSUR-IDPARTNR                 
065300                                        DP-ADD-IDPARTNR                   
065400                                        DP-CREDI-IDPARTNR                 
065500                                        DP-DADR-IDPARTNR                  
065600                                        WS-IDPARTNR                       
065700     MOVE HEAD-BEBETVIL              TO DP-HEAD-BEBETVIL                  
065800                                        DP-APPX-BEBETVIL                  
065900                                        DP-INSUR-BEBETVIL                 
066000                                        DP-ADD-BEBETVIL                   
066100                                        DP-CREDI-BEBETVIL                 
066200                                        DP-DADR-BEBETVIL                  
066300     MOVE HEAD-BEBET-NAME1           TO DP-HEAD-BEBET-NAME1               
066400                                        DP-APPX-BEBET-NAME1               
066500                                        DP-INSUR-BEBET-NAME1              
066600                                        DP-ADD-BEBET-NAME1                
066700                                        DP-CREDI-BEBET-NAME1              
066800                                        DP-DADR-BEBET-NAME1               
066900                                        WS-BEBET-NAME1                    
067000     MOVE HEAD-BEBET-NAME2           TO DP-HEAD-BEBET-NAME2               
067100                                        DP-APPX-BEBET-NAME2               
067200                                        DP-INSUR-BEBET-NAME2              
067300                                        DP-ADD-BEBET-NAME2                
067400                                        DP-CREDI-BEBET-NAME2              
067500                                        DP-DADR-BEBET-NAME2               
067600     MOVE HEAD-ADBET-BOX             TO DP-HEAD-ADBET-BOX                 
067700                                        DP-APPX-ADBET-BOX                 
067800                                        DP-INSUR-ADBET-BOX                
067900                                        DP-ADD-ADBET-BOX                  
068000                                        DP-CREDI-ADBET-BOX                
068100                                        DP-DADR-ADBET-BOX                 
068200     MOVE HEAD-ADBET-STREET          TO DP-HEAD-ADBET-STREET              
068300                                        DP-APPX-ADBET-STREET              
068400                                        DP-INSUR-ADBET-STREET             
068500                                        DP-ADD-ADBET-STREET               
068600                                        DP-CREDI-ADBET-STREET             
068700                                        DP-DADR-ADBET-STREET              
068800     MOVE HEAD-ADBET-PCODE           TO DP-HEAD-ADBET-PCODE               
068900                                        DP-APPX-ADBET-PCODE               
069000                                        DP-INSUR-ADBET-PCODE              
069100                                        DP-ADD-ADBET-PCODE                
069200                                        DP-CREDI-ADBET-PCODE              
069300                                        DP-DADR-ADBET-PCODE               
069400     MOVE HEAD-ADBET-CITY            TO DP-HEAD-ADBET-CITY                
069500                                        DP-APPX-ADBET-CITY                
069600                                        DP-INSUR-ADBET-CITY               
069700                                        DP-ADD-ADBET-CITY                 
069800                                        DP-CREDI-ADBET-CITY               
069900                                        DP-DADR-ADBET-CITY                
070000     MOVE HEAD-BELAND-BET            TO DP-HEAD-BELAND-BET                
070100                                        DP-APPX-BELAND-BET                
070200                                        DP-INSUR-BELAND-BET               
070300                                        DP-ADD-BELAND-BET                 
070400                                        DP-CREDI-BELAND-BET               
070500                                        DP-DADR-BELAND-BET                
070600     MOVE HEAD-IDVAT-BET             TO DP-HEAD-IDVAT-BET                 
070700                                        DP-APPX-IDVAT-BET                 
070800                                        DP-INSUR-IDVAT-BET                
070900                                        DP-ADD-IDVAT-BET                  
071000                                        DP-CREDI-IDVAT-BET                
071100                                        DP-DADR-IDVAT-BET                 
071200     MOVE HEAD-SUNTO-PART            TO DP-HEAD-SUNTO-PART                
071300                                        DP-APPX-SUNTO-PART                
071400                                        DP-INSUR-SUNTO-PART               
071500                                        DP-ADD-SUNTO-PART                 
071600                                        DP-CREDI-SUNTO-PART               
071700                                        DP-DADR-SUNTO-PART                
071800     MOVE HEAD-SUBTO-PART            TO DP-HEAD-SUBTO-PART                
071900                                        DP-APPX-SUBTO-PART                
072000                                        DP-INSUR-SUBTO-PART               
072100                                        DP-ADD-SUBTO-PART                 
072200                                        DP-CREDI-SUBTO-PART               
072300                                        DP-DADR-SUBTO-PART                
072400     IF HEAD-SUBTO-PART < ZERO                                            
072500       MOVE '-'                      TO DP-HEAD-REF-MINUS                 
072600                                        DP-APPX-REF-MINUS                 
072700     ELSE                                                                 
072800       MOVE SPACE                    TO DP-HEAD-REF-MINUS                 
072900                                        DP-APPX-REF-MINUS                 
073000     END-IF                                                               
073100     MOVE SPACE                      TO DP-HEAD-IDVAT-LEG-REG-NO          
073200                                        DP-APPX-IDVAT-LEG-REG-NO          
073300                                        DP-INSUR-IDVAT-LEG-REG-NO         
073400                                        DP-ADD-IDVAT-LEG-REG-NO           
073500                                        DP-CREDI-IDVAT-LEG-REG-NO         
073600                                       DP-DADR-IDVAT-LEG-REG-NO           
073700     MOVE HEAD-IDVAT-LEG             TO DP-HEAD-IDVAT-LEG-REG-NO          
073800     MOVE HEAD-BEANST                TO DP-HEAD-BEANST                    
073900                                        DP-APPX-BEANST                    
074000                                        DP-INSUR-BEANST                   
074100                                        DP-ADD-BEANST                     
074200                                        DP-CREDI-BEANST                   
074300                                        DP-DADR-BEANST                    
074400                                        WS-BEANST                         
074500     MOVE HEAD-IDUSER                TO DP-HEAD-IDUSER                    
074600                                        DP-APPX-IDUSER                    
074700                                        DP-INSUR-IDUSER                   
074800                                        DP-ADD-IDUSER                     
074900                                        DP-CREDI-IDUSER                   
075000                                        DP-DADR-IDUSER                    
075100     MOVE HEAD-KDTRADP               TO DP-HEAD-KDTRADP                   
075200     MOVE HEAD-BETEXT                TO DP-HEAD-BETEXT                    
075300     MOVE HEAD-BETEXT                TO WS-BETEXT                         
075400     MOVE HEAD-BETEXT-1              TO DP-HEAD-BETEXT-1                  
075500     MOVE HEAD-BETEXT-2              TO DP-HEAD-BETEXT-2                  
075600     MOVE HEAD-BETEXT-3              TO DP-HEAD-BETEXT-3                  
075700     MOVE HEAD-BETEXT-4              TO DP-HEAD-BETEXT-4                  
075800     MOVE HEAD-BETEXT-5              TO DP-HEAD-BETEXT-5                  
075900     MOVE HEAD-BETEXT-6              TO DP-HEAD-BETEXT-6                  
076000     MOVE HEAD-BETEXT-7              TO DP-HEAD-BETEXT-7                  
076100     MOVE HEAD-BETEXT-8              TO DP-HEAD-BETEXT-8                  
076200* DENNA KOD ÄR FÖR ATT SKRIVA EN FÖRSÄKRANSTEXT                           
076300     IF HEAD-BETEXT-9 > ' '                                               
076400       MOVE YES TO NEW-INSURANCE-SW                                       
076500       MOVE HEAD-BETEXT-9            TO SAVE-DP-INSUR-BETEXT-9            
076600       MOVE HEAD-BETEXT-10           TO SAVE-DP-INSUR-BETEXT-10           
076700       MOVE HEAD-BETEXT-11           TO SAVE-DP-INSUR-BETEXT-11           
076800       MOVE HEAD-BETEXT-12           TO SAVE-DP-INSUR-BETEXT-12           
076900       MOVE HEAD-BETEXT-13           TO SAVE-DP-INSUR-BETEXT-13           
077000       MOVE HEAD-BETEXT-14           TO SAVE-DP-INSUR-BETEXT-14           
077100       MOVE HEAD-BETEXT-15           TO SAVE-DP-INSUR-BETEXT-15           
077200       MOVE HEAD-BETEXT-16           TO SAVE-DP-INSUR-BETEXT-16           
077300       MOVE HEAD-BETEXT-17           TO SAVE-DP-INSUR-BETEXT-17           
077400       MOVE HEAD-BETEXT-18           TO SAVE-DP-INSUR-BETEXT-18           
077500       MOVE HEAD-BETEXT-19           TO SAVE-DP-INSUR-BETEXT-19           
077600       MOVE HEAD-BETEXT-20           TO SAVE-DP-INSUR-BETEXT-20           
077700       MOVE HEAD-BETEXT-21           TO SAVE-DP-INSUR-BETEXT-21           
077800       MOVE HEAD-BETEXT-22           TO SAVE-DP-INSUR-BETEXT-22           
077900       MOVE HEAD-BETEXT-23           TO SAVE-DP-INSUR-BETEXT-23           
078000       MOVE HEAD-BETEXT-24           TO SAVE-DP-INSUR-BETEXT-24           
078100       MOVE HEAD-BETEXT-25           TO SAVE-DP-ADD-BETEXT-25             
078200       MOVE HEAD-BETEXT-26           TO SAVE-DP-ADD-BETEXT-26             
078300       MOVE HEAD-BETEXT-27           TO SAVE-DP-ADD-BETEXT-27             
078400       MOVE HEAD-BETEXT-28           TO SAVE-DP-ADD-BETEXT-28             
078500       MOVE HEAD-BETEXT-29           TO SAVE-DP-ADD-BETEXT-29             
078600       MOVE HEAD-BETEXT-30           TO SAVE-DP-ADD-BETEXT-30             
078700       MOVE HEAD-BETEXT-31           TO SAVE-DP-ADD-BETEXT-31             
078800       MOVE HEAD-BETEXT-32           TO SAVE-DP-ADD-BETEXT-32             
078900     ELSE                                                                 
079000       MOVE NOO TO NEW-INSURANCE-SW                                       
079100     END-IF                                                               
079200                                                                          
079300     IF HEAD-BETEXT-33 > ' '                                              
079400       MOVE YES TO NEW-ADD-INSURANCE-SW                                   
079500       MOVE HEAD-BETEXT-33       TO SAVE-DP-ADD-BETEXT-33                 
079600       MOVE HEAD-BETEXT-34       TO SAVE-DP-ADD-BETEXT-34                 
079700       MOVE HEAD-BETEXT-35       TO SAVE-DP-ADD-BETEXT-35                 
079800       MOVE HEAD-BETEXT-36       TO SAVE-DP-ADD-BETEXT-36                 
079900       MOVE HEAD-BETEXT-37       TO SAVE-DP-ADD-BETEXT-37                 
080000       MOVE HEAD-BETEXT-38       TO SAVE-DP-ADD-BETEXT-38                 
080100       MOVE HEAD-BETEXT-39       TO SAVE-DP-ADD-BETEXT-39                 
080200       MOVE HEAD-BETEXT-40       TO SAVE-DP-ADD-BETEXT-40                 
080300       MOVE HEAD-BETEXT-41       TO SAVE-DP-ADD-BETEXT-41                 
080400       MOVE HEAD-BETEXT-42       TO SAVE-DP-ADD-BETEXT-42                 
080500       MOVE HEAD-BETEXT-43       TO SAVE-DP-ADD-BETEXT-43                 
080600       MOVE HEAD-BETEXT-44       TO SAVE-DP-ADD-BETEXT-44                 
080700     ELSE                                                                 
080800       MOVE NOO TO NEW-ADD-INSURANCE-SW                                   
080900     END-IF                                                               
081000                                                                          
081100     IF HEAD-BETEXT   > ' '                                               
081200       IF HEAD-KDFINDOC = 'CR'                                            
081300         MOVE YES TO NEW-CREDIT-SW                                        
081400         MOVE HEAD-BETEXT              TO SAVE-DP-CREDI-BETEXT            
081500         MOVE HEAD-BETEXT-CRE          TO SAVE-DP-CREDI-BETEXT-CRE        
081600       ELSE                                                               
081700         MOVE NOO TO NEW-CREDIT-SW                                        
081800       END-IF                                                             
081900       IF HEAD-KDFINDOC = 'INV'                                           
082000         MOVE YES TO NEW-DADR-SW                                          
082100         MOVE HEAD-BETEXT            TO SAVE-DP-DADR-BETEXT               
082200       ELSE                                                               
082300         MOVE NOO TO NEW-DADR-SW                                          
082400       END-IF                                                             
082500     ELSE                                                                 
082600       MOVE NOO TO NEW-CREDIT-SW                                          
082700       MOVE NOO TO NEW-DADR-SW                                            
082800     END-IF                                                               
082900     .                                                                    
083000                                                                          
083100 BB-HANDLE-LINES SECTION.                                                 
083200     MOVE IN-DATA               TO IN-AREA-LINE                           
083300                                                                          
083400*                                                                         
083500*    -- IF NEW GROUP,                                                     
083600*          MARK IDAFPRCD WITH '2.1'                                       
083700     IF LINE-IDEXCUST-2   = WS-IDEXCUST-2-OLD                             
083800     AND LINE-IDFINDOC    = WS-IDFINDOC-OLD                               
083900     AND LINE-IDREF       = WS-IDREF-OLD                                  
084000     AND LINE-DAREFDAT    = WS-DAREFDAT-OLD                               
084100       IF HEAD-FLSOFT     = 'N'                                           
084200         IF LINE-IDTRACK-1 > SPACE                                        
084300           MOVE WS-DOC-LINE-TRAC  TO DP-LINE-IDAFPRCD                     
084400         ELSE                                                             
084500           MOVE LINE-IDPTYP       TO DP-LINE-IDAFPRCD                     
084600         END-IF                                                           
084700       ELSE                                                               
084800         MOVE WS-DOC-LINE-SOFT  TO DP-LINE-IDAFPRCD                       
084900       END-IF                                                             
085000     ELSE                                                                 
085100       IF HEAD-FLSOFT     = 'N'                                           
085200         IF LINE-IDTRACK-1 > SPACE                                        
085300           MOVE WS-DOC-GROUP-TRAC TO DP-LINE-IDAFPRCD                     
085400         ELSE                                                             
085500           MOVE WS-DOC-GROUP      TO DP-LINE-IDAFPRCD                     
085600         END-IF                                                           
085700       ELSE                                                               
085800         MOVE WS-DOC-GROUP-SOFT TO DP-LINE-IDAFPRCD                       
085900       END-IF                                                             
086000       MOVE LINE-IDEXCUST-2     TO WS-IDEXCUST-2-OLD                      
086100       MOVE LINE-IDREF          TO WS-IDREF-OLD                           
086200       MOVE LINE-DAREFDAT       TO WS-DAREFDAT-OLD                        
086300       MOVE LINE-IDFINDOC       TO WS-IDFINDOC-OLD                        
086400     END-IF                                                               
086500     MOVE LINE-IDEXCUST-1       TO DP-LINE-IDEXCUST-1                     
086600                                   DP-FIPH-IDEXCUST-1                     
086700     MOVE LINE-IDEXCUST-2       TO DP-LINE-IDEXCUST-2                     
086800                                   DP-FIPH-IDEXCUST-2                     
086900     MOVE LINE-IDBUNDLE         TO DP-LINE-IDBUNDLE                       
087000                                   DP-FIPH-IDBUNDLE                       
087100     MOVE LINE-IDOPTION-1       TO DP-LINE-IDOPTION-1                     
087200                                   DP-FIPH-IDOPTION-1                     
087300     MOVE LINE-IDOPTION-2       TO DP-LINE-IDOPTION-2                     
087400                                   DP-FIPH-IDOPTION-2                     
087500     MOVE LINE-IDOPTION-3       TO DP-LINE-IDOPTION-3                     
087600                                   DP-FIPH-IDOPTION-3                     
087700     MOVE LINE-IDOPTION-5       TO DP-LINE-IDOPTION-5                     
087800                                   DP-FIPH-IDOPTION-5                     
087900     MOVE LINE-IDREF            TO DP-LINE-IDREF                          
088000                                   DP-FIPH-IDREF                          
088100     MOVE LINE-BEVOLREF         TO DP-LINE-BEVOLREF                       
088200                                   DP-FIPH-BEVOLREF                       
088300     MOVE LINE-IDARTNR-FINANCE  TO DP-LINE-IDARTNR-FINANCE                
088400                                   DP-FIPH-IDARTNR-FINANCE                
088500* DENNA KOD ÄR FÖR ATT SKRIVA KONTROLLSIFFRA TILL ARTIKELN                
088600     IF LINE-KDFINDOC = 'INV'                                             
088700       IF LINE-IDARTNR-CNTRL = SPACE                                      
088800         CONTINUE                                                         
088900       ELSE                                                               
089000         IF LINE-IDARTNR-FINANCE(1:1) = SPACE                             
089100* DENNA KOD ÄR FÖR ATT SKRIVA SUMMA RAD FÖR FREIGHT, INSURANCE            
089200* , PACKING AND HANDLING FAKTURAVIS                                       
089300* GÄLLER FÖR TILLFÄLLET ENDAST OM KONTROLLSIFFRA SKA SKRIVAS              
089400           MOVE YES TO NEW-FIPH-SUM-ROW-SW                                
089500****                                                                      
089600         ELSE                                                             
089700         IF LINE-IDARTNR-FINANCE(2:1) = SPACE                             
089800           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(2:2)        
089900         ELSE                                                             
090000         IF LINE-IDARTNR-FINANCE(3:1) = SPACE                             
090100           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(3:2)        
090200         ELSE                                                             
090300         IF LINE-IDARTNR-FINANCE(4:1) = SPACE                             
090400           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(4:2)        
090500         ELSE                                                             
090600         IF LINE-IDARTNR-FINANCE(5:1) = SPACE                             
090700           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(5:2)        
090800         ELSE                                                             
090900         IF LINE-IDARTNR-FINANCE(6:1) = SPACE                             
091000           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(6:2)        
091100         ELSE                                                             
091200         IF LINE-IDARTNR-FINANCE(7:1) = SPACE                             
091300           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(7:2)        
091400         ELSE                                                             
091500         IF LINE-IDARTNR-FINANCE(8:1) = SPACE                             
091600           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(8:2)        
091700         ELSE                                                             
091800         IF LINE-IDARTNR-FINANCE(9:1) = SPACE                             
091900           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(9:2)        
092000         ELSE                                                             
092100         IF LINE-IDARTNR-FINANCE(10:1) = SPACE                            
092200          MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(10:2)        
092300         ELSE                                                             
092400           CONTINUE                                                       
092500         END-IF                                                           
092600         END-IF                                                           
092700         END-IF                                                           
092800         END-IF                                                           
092900         END-IF                                                           
093000         END-IF                                                           
093100         END-IF                                                           
093200         END-IF                                                           
093300         END-IF                                                           
093400         END-IF                                                           
093500       END-IF                                                             
093600     END-IF                                                               
093700     MOVE LINE-BEART            TO DP-LINE-BEART                          
093800     MOVE LINE-KVBEART          TO DP-LINE-KVBEART                        
093900     MOVE LINE-KVLEVART         TO DP-LINE-KVLEVART                       
094000     MOVE LINE-PRARTBTO         TO DP-LINE-PRARTBTO                       
094100                                   DP-FIPH-PRARTBTO                       
094200     IF LINE-PRARTBTO   < ZERO                                            
094300       MOVE '-'                      TO DP-LINE-REF-MINUS                 
094400     ELSE                                                                 
094500       MOVE SPACE                    TO DP-LINE-REF-MINUS                 
094600     END-IF                                                               
094700     MOVE LINE-PRARTNTO         TO DP-LINE-PRARTNTO                       
094800                                   DP-FIPH-PRARTNTO                       
094900     MOVE LINE-REARTRAB         TO DP-LINE-REARTRAB                       
095000                                   DP-FIPH-REARTRAB                       
095100     IF LINE-FLSPECPR = 'Y' OR 'J'                                        
095200       MOVE '*'                 TO DP-LINE-FLSPECPR                       
095300                                   DP-FIPH-FLSPECPR                       
095400     ELSE                                                                 
095500       MOVE SPACE               TO DP-LINE-FLSPECPR                       
095600                                   DP-FIPH-FLSPECPR                       
095700     END-IF                                                               
095800     IF LINE-FLPCOO   = 'Y' OR 'J'                                        
095900       MOVE '*'                 TO DP-LINE-FLPCOO                         
096000     ELSE                                                                 
096100       MOVE SPACE               TO DP-LINE-FLPCOO                         
096200     END-IF                                                               
096300     MOVE LINE-SUNTO            TO DP-LINE-SUNTO                          
096400                                   DP-FIPH-SUNTO                          
096500     MOVE LINE-REVAT            TO DP-LINE-REVAT                          
096600                                   DP-FIPH-REVAT                          
096700     MOVE LINE-SUVAT-BILLIT     TO DP-LINE-SUVAT-BILLIT                   
096800                                   DP-FIPH-SUVAT-BILLIT                   
096900     MOVE LINE-SUBTO            TO DP-LINE-SUBTO                          
097000                                   DP-FIPH-SUBTO                          
097100     MOVE LINE-VKARTNTO         TO DP-LINE-VKARTNTO                       
097200                                   DP-FIPH-VKARTNTO                       
097300     MOVE LINE-VKORDBTO-KOLLI   TO DP-LINE-VKORDBTO-KOLLI                 
097400                                   DP-FIPH-VKORDBTO-KOLLI                 
097500     MOVE LINE-KDARTURS         TO DP-LINE-KDARTURS                       
097600                                   DP-FIPH-KDARTURS                       
097700     MOVE LINE-IDSTATNR         TO DP-LINE-IDSTATNR                       
097800                                   DP-FIPH-IDSTATNR                       
097900     MOVE LINE-KDFRAKT          TO DP-LINE-KDFRAKT                        
098000                                   DP-FIPH-KDFRAKT                        
098100     MOVE LINE-IDACCNT-1        TO DP-LINE-IDACCNT-1                      
098200                                   DP-FIPH-IDACCNT-1                      
098300     MOVE LINE-IDACCNT-2        TO DP-LINE-IDACCNT-2                      
098400                                   DP-FIPH-IDACCNT-2                      
098500     MOVE LINE-IDACCNT-3        TO DP-LINE-IDACCNT-3                      
098600                                   DP-FIPH-IDACCNT-3                      
098700     MOVE LINE-KDANMORS         TO DP-LINE-KDANMORS                       
098800                                   DP-FIPH-KDANMORS                       
098900     MOVE LINE-IDFAKREF         TO DP-LINE-IDFAKREF                       
099000                                   DP-FIPH-IDFAKREF                       
099100     MOVE LINE-DAFAKREF         TO DP-LINE-DAFAKREF                       
099200                                   DP-FIPH-DAFAKREF                       
099300     MOVE LINE-DAREFDAT         TO DP-LINE-DAREFDAT                       
099400                                   DP-FIPH-DAREFDAT                       
099500     MOVE LINE-IDDC             TO DP-LINE-IDDC                           
099600                                   DP-FIPH-IDDC                           
099700     MOVE LINE-BELEVVIL         TO DP-LINE-BELEVVIL                       
099800                                   DP-FIPH-BELEVVIL                       
099900     MOVE LINE-IDLEVNR-ART      TO DP-LINE-IDLEVNR-ART                    
100000     MOVE LINE-IDTRACK-1        TO DP-LINE-IDTRACK-1                      
100100     MOVE LINE-KVANT-TRACK-1    TO DP-LINE-KVANT-TRACK-1                  
100200     MOVE LINE-IDTRACK-2        TO DP-LINE-IDTRACK-2                      
100300     MOVE LINE-KVANT-TRACK-2    TO DP-LINE-KVANT-TRACK-2                  
100400     MOVE LINE-IDTRACK-3        TO DP-LINE-IDTRACK-3                      
100500     MOVE LINE-KVANT-TRACK-3    TO DP-LINE-KVANT-TRACK-3                  
100600     MOVE LINE-IDTRACK-4        TO DP-LINE-IDTRACK-4                      
100700     MOVE LINE-KVANT-TRACK-4    TO DP-LINE-KVANT-TRACK-4                  
100800     MOVE LINE-IDTRACK-5        TO DP-LINE-IDTRACK-5                      
100900     MOVE LINE-KVANT-TRACK-5    TO DP-LINE-KVANT-TRACK-5                  
101000     MOVE HEAD-BEANST           TO DP-LINE-BEANST                         
101100                                   DP-FIPH-BEANST                         
101200     MOVE HEAD-IDUSER           TO DP-LINE-IDUSER                         
101300                                   DP-FIPH-IDUSER                         
101400                                                                          
101500*    --- GET DOC-HEAD-DATA FROM BILLIT-LINE-DATA                          
101600     IF NEW-HEAD                                                          
101700       MOVE LINE-IDEXCUST-1     TO DP-HEAD-IDEXCUST-1                     
101800                                   DP-APPX-IDEXCUST-1                     
101900                                   DP-INSUR-IDEXCUST-1                    
102000                                   DP-ADD-IDEXCUST-1                      
102100                                   DP-CREDI-IDEXCUST-1                    
102200                                   DP-DADR-IDEXCUST-1                     
102300                                   WS-IDEXCUST-1                          
102400       MOVE LINE-IDEXCUST-2     TO DP-HEAD-IDEXCUST-2                     
102500                                   DP-APPX-IDEXCUST-2                     
102600                                   DP-INSUR-IDEXCUST-2                    
102700                                   DP-ADD-IDEXCUST-2                      
102800                                   DP-CREDI-IDEXCUST-2                    
102900                                   DP-DADR-IDEXCUST-2                     
103000       MOVE LINE-BELEVVIL       TO DP-HEAD-BELEVVIL                       
103100                                   DP-APPX-BELEVVIL                       
103200                                   DP-INSUR-BELEVVIL                      
103300                                   DP-ADD-BELEVVIL                        
103400                                   DP-CREDI-BELEVVIL                      
103500                                   DP-DADR-BELEVVIL                       
103600       MOVE LINE-IDDC           TO DP-HEAD-IDDC                           
103700                                   DP-APPX-IDDC                           
103800                                   DP-INSUR-IDDC                          
103900                                   DP-ADD-IDDC                            
104000                                   DP-CREDI-IDDC                          
104100                                   DP-DADR-IDDC                           
104200                                   WS-IDDC                                
104300       PERFORM S91-SKRIV-DAP5-S                                           
104400* FÖR DESSA KODER SKA MOMSTEXTER ALLTID                                   
104500* LÄGGAS UT PÅ DOKUMENTET                                                 
104600       IF LINE-KDVAT = 'BD'                                               
104700         MOVE LINE-BEVAT        TO DP-HEAD-BEVAT                          
104800                                   DP-APPX-BEVAT                          
104900                                   DP-INSUR-BEVAT                         
105000                                   DP-ADD-BEVAT                           
105100                                   DP-CREDI-BEVAT                         
105200                                   DP-DADR-BEVAT                          
105300       ELSE                                                               
105400         MOVE SPACE             TO DP-HEAD-BEVAT                          
105500                                   DP-APPX-BEVAT                          
105600                                   DP-INSUR-BEVAT                         
105700                                   DP-ADD-BEVAT                           
105800                                   DP-CREDI-BEVAT                         
105900                                   DP-DADR-BEVAT                          
106000       END-IF                                                             
106100                                                                          
106200       PERFORM S90-WRITE-DOC-HEAD                                         
106300       MOVE NOO                 TO NEW-HEAD-SW                            
106400     END-IF                                                               
106500                                                                          
106600* DENNA KOD ÄR FÖR ATT SKRIVA VALUTAJUSTERINGEN FÖR FAKTUROR SOM          
106700* ÄR OMRÄKNADE TILL LOKAL VALUTA                                          
106800     MOVE LINE-SUNTO           TO WS2-LOCC-JUS-SUNTO                      
106900     COMPUTE WS-LOCC-JUS-SUNTO = WS-LOCC-JUS-SUNTO +                      
107000                                   WS2-LOCC-JUS-SUNTO                     
107100                                                                          
107200* DENNA KOD ÄR FÖR ATT SKRIVA SUMMA RAD FÖR FREIGHT, INSURANCE            
107300* , PACKING AND HANDLING FAKTURAVIS                                       
107400     IF LINE-IDARTNR-FINANCE > SPACE                                      
107500       PERFORM S90-WRITE-DOC-LINE                                         
107600     ELSE                                                                 
107700       IF LINE-KDFINDOC = 'INV'                                           
107800         IF LINE-IDARTNR-CNTRL = SPACE                                    
107900           PERFORM S90-WRITE-DOC-LINE                                     
108000         ELSE                                                             
108100           MOVE +1 TO WS-IX                                               
108200           PERFORM UNTIL WS-IX > 35                                       
108300             IF LINE-BEART = WS-BEART(WS-IX)                              
108400               COMPUTE WS-KVLEVART(WS-IX)     = LINE-KVLEVART +           
108500                                                WS-KVLEVART(WS-IX)        
108600               COMPUTE WS-SUNTO(WS-IX)        = LINE-SUNTO +              
108700                                                WS-SUNTO(WS-IX)           
108800               ADD +35 TO WS-IX                                           
108900             ELSE                                                         
109000               IF WS-BEART(WS-IX) > SPACE                                 
109100                 ADD +1 TO WS-IX                                          
109200               ELSE                                                       
109300                 MOVE LINE-BEART        TO WS-BEART(WS-IX)                
109400                 MOVE LINE-KVLEVART     TO WS-KVLEVART(WS-IX)             
109500                 MOVE LINE-SUNTO        TO WS-SUNTO(WS-IX)                
109600                 ADD +35 TO WS-IX                                         
109700               END-IF                                                     
109800             END-IF                                                       
109900           END-PERFORM                                                    
110000         END-IF                                                           
110100       ELSE                                                               
110200         PERFORM S90-WRITE-DOC-LINE                                       
110300       END-IF                                                             
110400     END-IF                                                               
110500     .                                                                    
110600                                                                          
110700 BC-HANDLE-FOOTER SECTION.                                                
110800     MOVE IN-DATA               TO IN-AREA-FOOT                           
110900                                                                          
111000     MOVE FOOT-IDPTYP           TO DP-FOOT-IDAFPRCD                       
111100     MOVE FOOT-SUNTO-TOT        TO WS-SUNTO-SND-LOC                       
111200     MOVE FOOT-SUNTO-TOT        TO WS-LOCC-JUS-CHECK                      
111300     MOVE FOOT-SUNTO-TOT        TO DP-FOOT-SUNTO-TOT                      
111400     MOVE FOOT-SUVAT-BILLIT-TOT TO DP-FOOT-SUVAT-BILLIT-TOT               
111500     MOVE FOOT-SUBTO-TOT        TO DP-FOOT-SUBTO-TOT                      
111600     IF FOOT-SUBTO-TOT < ZERO                                             
111700       MOVE '-'                 TO DP-FOOT-REF-MINUS                      
111800     ELSE                                                                 
111900       MOVE SPACE               TO DP-FOOT-REF-MINUS                      
112000     END-IF                                                               
112100     MOVE FOOT-KDVALISO         TO DP-FOOT-KDVALISO                       
112200     MOVE FOOT-KDVALISO         TO DP-FOOT-KDVALISO-TXT                   
112300     MOVE FOOT-BETEXT-1         TO DP-FOOT-BETEXT-1                       
112400     MOVE FOOT-BETEXT-2         TO DP-FOOT-BETEXT-2                       
112500     MOVE FOOT-BETEXT-3         TO DP-FOOT-BETEXT-3                       
112600     MOVE FOOT-BETEXT-4         TO DP-FOOT-BETEXT-4                       
112700*                                                                         
112800* INDICATES LOCAL CURRENCY ON THE INVOICE                                 
112900     IF FOOT-KDVALISO-LOC NOT = FOOT-KDVALISO                             
113000       MOVE FOOT-SUNTO-TOT-LOC  TO DP-FOOT-SUNTO-TOT-LOC                  
113100       MOVE FOOT-SUNTO-TOT-LOC  TO WS-SUNTO-SND-LOC                       
113200       MOVE FOOT-SUBTO-TOT-LOC  TO DP-FOOT-SUBTO-TOT-LOC                  
113300       MOVE FOOT-SUVAT-BILLIT-TOT-LOC                                     
113400                                TO DP-FOOT-SUVAT-BILLIT-TOT-LOC           
113500       MOVE FOOT-KDVALISO-LOC   TO DP-FOOT-KDVALISO-LOC                   
113600       MOVE FOOT-KDVALISO-LOC   TO DP-FOOT-KDVALISO-TXT                   
113700       MOVE 'J'                 TO WS-FOOT-LOC-SW                         
113800     ELSE                                                                 
113900       MOVE ZERO                TO DP-FOOT-SUNTO-TOT-LOC                  
114000                                   DP-FOOT-SUBTO-TOT-LOC                  
114100                                   DP-FOOT-SUVAT-BILLIT-TOT-LOC           
114200       MOVE SPACE               TO DP-FOOT-KDVALISO-LOC                   
114300     END-IF                                                               
114400*                                                                         
114500* INDICATES SENDING CURRENCY ON THE INVOICE                               
114600     IF FOOT-KDVALISO-SND = FOOT-KDVALISO                                 
114700       MOVE ZERO                TO DP-FOOT-SUNTO-TOT-SND                  
114800                                   DP-FOOT-SUBTO-TOT-SND                  
114900                                   DP-FOOT-SUVAT-BILLIT-TOT-SND           
115000       MOVE SPACE               TO DP-FOOT-KDVALISO-SND                   
115100     ELSE                                                                 
115200       IF FOOT-KDVALISO-SND = FOOT-KDVALISO-LOC                           
115300         MOVE ZERO                TO DP-FOOT-SUNTO-TOT-SND                
115400                                     DP-FOOT-SUBTO-TOT-SND                
115500                                     DP-FOOT-SUVAT-BILLIT-TOT-SND         
115600         MOVE SPACE               TO DP-FOOT-KDVALISO-SND                 
115700       ELSE                                                               
115800         MOVE FOOT-SUNTO-TOT-SND  TO DP-FOOT-SUNTO-TOT-SND                
115900         MOVE FOOT-SUBTO-TOT-SND  TO DP-FOOT-SUBTO-TOT-SND                
116000         MOVE FOOT-SUVAT-BILLIT-TOT-SND                                   
116100                                  TO DP-FOOT-SUVAT-BILLIT-TOT-SND         
116200         MOVE FOOT-KDVALISO-SND   TO DP-FOOT-KDVALISO-SND                 
116300         MOVE 'J'                 TO WS-FOOT-SND-SW                       
116400         IF FOOT-SUNTO-TOT-SND = ZERO                                     
116500           MOVE ZERO TO DP-FOOT-PRKURS-SND                                
116600         ELSE                                                             
116700*          COMPUTE DP-FOOT-PRKURS-SND ROUNDED = WS-SUNTO-SND-LOC /        
116800*                                             FOOT-SUNTO-TOT-SND          
116900         MOVE FOOT-PRKURS-SND     TO DP-FOOT-PRKURS-SND                   
117000         END-IF                                                           
117100       END-IF                                                             
117200     END-IF                                                               
117300                                                                          
117400* DENNA KOD ÄR FÖR ATT SKRIVA SUMMA RAD FÖR FREIGHT, INSURANCE            
117500* , PACKING AND HANDLING FAKTURAVIS                                       
117600     IF NEW-FIPH-SUM-ROW                                                  
117700       MOVE +1 TO WS-IX                                                   
117800       PERFORM UNTIL WS-IX > 35                                           
117900         IF WS-BEART(WS-IX) > ' '                                         
118000           PERFORM BK-HANDLE-FIPH-SUM                                     
118100           ADD +1 TO WS-IX                                                
118200         ELSE                                                             
118300           ADD +35 TO WS-IX                                               
118400         END-IF                                                           
118500       END-PERFORM                                                        
118600     END-IF                                                               
118700                                                                          
118800* DENNA KOD ÄR FÖR ATT SKRIVA VALUTAJUSTERINGEN FÖR FAKTUROR SOM          
118900* ÄR OMRÄKNADE TILL LOKAL VALUTA                                          
119000     PERFORM BL-HANDLE-LOCC-JUS                                           
119100                                                                          
119200     PERFORM S90-WRITE-DOC-FOOT                                           
119300                                                                          
119400     IF WS-FOOT-LOC-SW = 'J'                                              
119500       PERFORM BH-HANDLE-FOOT-LOC                                         
119600     END-IF                                                               
119700                                                                          
119800     IF WS-FOOT-SND-SW = 'J'                                              
119900       PERFORM BI-HANDLE-FOOT-SND                                         
120000     END-IF                                                               
120100                                                                          
120200     MOVE NOO TO WS-FOOT-LOC-SW                                           
120300     MOVE NOO TO WS-FOOT-SND-SW                                           
120400     .                                                                    
120500                                                                          
120600 BD-HANDLE-APPENDIX SECTION.                                              
120700     MOVE IN-DATA                 TO IN-AREA-APPX                         
120800                                                                          
120900*    -- IF NEW APPENDIX                                                   
121000*          MARK IDAFPRCD WITH '4.1'                                       
121100     IF NEW-APPENDIX-SW = 'J'                                             
121200       MOVE WS-DOC-NEW-APPENDIX   TO DP-APPX-IDAFPRCD                     
121300       MOVE 'N'                   TO NEW-APPENDIX-SW                      
121400       PERFORM S90-WRITE-DOC-APPX                                         
121500     END-IF                                                               
121600     MOVE APPX-IDPTYP             TO DP-APPX-IDAFPRCD                     
121700     MOVE APPX-KDAPPEND           TO DP-APPX-KDAPPEND                     
121800     MOVE APPX-IDAPPEND           TO DP-APPX-IDAPPEND                     
121900     MOVE APPX-SUNTO-APP          TO DP-APPX-SUNTO-APP                    
122000     MOVE APPX-SUVAT-BILLIT-APP   TO DP-APPX-SUVAT-BILLIT-APP             
122100     MOVE APPX-SUBTO-APP          TO DP-APPX-SUBTO-APP                    
122200                                                                          
122300     PERFORM S90-WRITE-DOC-APPX                                           
122400     .                                                                    
122500                                                                          
122600* DENNA KOD ÄR FÖR ATT SKRIVA ETT APPENDIX FÖR FÖRSÄKRANSTEXT             
122700* DOKUMENTVIS                                                             
122800 BF-HANDLE-INSUR SECTION.                                                 
122900*    -- IF NEW INSURANCE TEXT                                             
123000*    MARK IDAFPRCD WITH '5.1'                                             
123100     IF NEW-INSURANCE-SW = 'J'                                            
123200       MOVE WS-DOC-INSURANCE      TO DP-INSUR-IDAFPRCD                    
123300       MOVE 'N'                   TO NEW-INSURANCE-SW                     
123400       PERFORM S90-WRITE-DOC-INSUR                                        
123500     END-IF                                                               
123600*    MARK IDAFPRCD WITH '5'                                               
123700     MOVE WS-DOC-INSURANCE-ROW    TO DP-INSUR-IDAFPRCD                    
123800     MOVE SAVE-DP-INSUR-BETEXT-9  TO DP-INSUR-BETEXT-9                    
123900     MOVE SAVE-DP-INSUR-BETEXT-10 TO DP-INSUR-BETEXT-10                   
124000     MOVE SAVE-DP-INSUR-BETEXT-11 TO DP-INSUR-BETEXT-11                   
124100     MOVE SAVE-DP-INSUR-BETEXT-12 TO DP-INSUR-BETEXT-12                   
124200     MOVE SAVE-DP-INSUR-BETEXT-13 TO DP-INSUR-BETEXT-13                   
124300     MOVE SAVE-DP-INSUR-BETEXT-14 TO DP-INSUR-BETEXT-14                   
124400     MOVE SAVE-DP-INSUR-BETEXT-15 TO DP-INSUR-BETEXT-15                   
124500     MOVE SAVE-DP-INSUR-BETEXT-16 TO DP-INSUR-BETEXT-16                   
124600     MOVE SAVE-DP-INSUR-BETEXT-17 TO DP-INSUR-BETEXT-17                   
124700     MOVE SAVE-DP-INSUR-BETEXT-18 TO DP-INSUR-BETEXT-18                   
124800     MOVE SAVE-DP-INSUR-BETEXT-19 TO DP-INSUR-BETEXT-19                   
124900     MOVE SAVE-DP-INSUR-BETEXT-20 TO DP-INSUR-BETEXT-20                   
125000     MOVE SAVE-DP-INSUR-BETEXT-21 TO DP-INSUR-BETEXT-21                   
125100     MOVE SAVE-DP-INSUR-BETEXT-22 TO DP-INSUR-BETEXT-22                   
125200     MOVE SAVE-DP-INSUR-BETEXT-23 TO DP-INSUR-BETEXT-23                   
125300     MOVE SAVE-DP-INSUR-BETEXT-24 TO DP-INSUR-BETEXT-24                   
125400                                                                          
125500     PERFORM S90-WRITE-DOC-INSUR                                          
125600*    MARK IDAFPRCD WITH '5.2'                                             
125700     MOVE WS-DOC-INSURANCE-ADD    TO DP-INSUR-IDAFPRCD                    
125800     MOVE SAVE-DP-ADD-BETEXT-25   TO DP-INSUR-BETEXT-9                    
125900     MOVE SAVE-DP-ADD-BETEXT-26   TO DP-INSUR-BETEXT-10                   
126000     MOVE SAVE-DP-ADD-BETEXT-27   TO DP-INSUR-BETEXT-11                   
126100     MOVE SAVE-DP-ADD-BETEXT-28   TO DP-INSUR-BETEXT-12                   
126200     MOVE SAVE-DP-ADD-BETEXT-29   TO DP-INSUR-BETEXT-13                   
126300     MOVE SAVE-DP-ADD-BETEXT-30   TO DP-INSUR-BETEXT-14                   
126400     MOVE SAVE-DP-ADD-BETEXT-31   TO DP-INSUR-BETEXT-15                   
126500     MOVE SAVE-DP-ADD-BETEXT-32   TO DP-INSUR-BETEXT-16                   
126600                                                                          
126700     PERFORM S90-WRITE-DOC-INSUR                                          
126800                                                                          
126900     .                                                                    
127000*                                                                         
127100 BF-HANDLE-ADD-INSUR SECTION.                                             
127200*    -- IF NEW ADDITIONAL INSURANCE TEXT                                  
127300*    MARK IDAFPRCD WITH '5.3'                                             
127400     IF NEW-ADD-INSURANCE-SW = 'J'                                        
127500       MOVE WS-DOC-ADD-INSURANCE  TO DP-ADD-IDAFPRCD                      
127600       MOVE 'N'                   TO NEW-ADD-INSURANCE-SW                 
127700       PERFORM S90-WRITE-ADD-INSUR                                        
127800     END-IF                                                               
127900*    MARK IDAFPRCD WITH '5.4'                                             
128000     MOVE WS-DOC-ADD-INSURANCE-ROW TO DP-ADD-IDAFPRCD                     
128100     MOVE SAVE-DP-ADD-BETEXT-33   TO DP-ADD-BETEXT-33                     
128200     MOVE SAVE-DP-ADD-BETEXT-34   TO DP-ADD-BETEXT-34                     
128300     MOVE SAVE-DP-ADD-BETEXT-35   TO DP-ADD-BETEXT-35                     
128400     MOVE SAVE-DP-ADD-BETEXT-36   TO DP-ADD-BETEXT-36                     
128500     MOVE SAVE-DP-ADD-BETEXT-37   TO DP-ADD-BETEXT-37                     
128600     MOVE SAVE-DP-ADD-BETEXT-38   TO DP-ADD-BETEXT-38                     
128700     MOVE SAVE-DP-ADD-BETEXT-39   TO DP-ADD-BETEXT-39                     
128800     MOVE SAVE-DP-ADD-BETEXT-40   TO DP-ADD-BETEXT-40                     
128900     MOVE SAVE-DP-ADD-BETEXT-41   TO DP-ADD-BETEXT-41                     
129000     MOVE SAVE-DP-ADD-BETEXT-42   TO DP-ADD-BETEXT-42                     
129100     MOVE SAVE-DP-ADD-BETEXT-43   TO DP-ADD-BETEXT-43                     
129200     MOVE SAVE-DP-ADD-BETEXT-44   TO DP-ADD-BETEXT-44                     
129300                                                                          
129400     PERFORM S90-WRITE-ADD-INSUR                                          
129500                                                                          
129600     .                                                                    
129700*                                                                         
129800                                                                          
129900 BG-HANDLE-CREDIT SECTION.                                                
130000*    -- IF NEW CREDIT TEXT                                                
130100*          MARK IDAFPRCD WITH '6.1'                                       
130200     IF NEW-CREDIT-SW = 'J'                                               
130300       MOVE WS-DOC-CREDIT         TO DP-CREDI-IDAFPRCD                    
130400       MOVE 'N'                   TO NEW-CREDIT-SW                        
130500       PERFORM S90-WRITE-DOC-CREDIT                                       
130600     END-IF                                                               
130700     MOVE WS-DOC-CREDIT-ROW       TO DP-CREDI-IDAFPRCD                    
130800     MOVE SAVE-DP-CREDI-BETEXT    TO DP-CREDI-BETEXT                      
130900     MOVE SAVE-DP-CREDI-BETEXT-CRE TO DP-CREDI-BETEXT-CRE                 
131000                                                                          
131100     PERFORM S90-WRITE-DOC-CREDIT                                         
131200     .                                                                    
131300*                                                                         
131400 BH-HANDLE-FOOT-LOC SECTION.                                              
131500*    -- IF NEW LOCAL CURRENCY DIFFERENT FROM INVOICE CURRENCY             
131600*          MARK IDAFPRCD WITH '3.1'                                       
131700     MOVE WS-DOC-FOOTER-LOC   TO DP-FOOT-IDAFPRCD                         
131800     MOVE FOOT-SUNTO-TOT-LOC  TO DP-FOOT-SUNTO-TOT-LOC                    
131900     MOVE FOOT-SUBTO-TOT-LOC  TO DP-FOOT-SUBTO-TOT-LOC                    
132000     MOVE FOOT-SUVAT-BILLIT-TOT-LOC                                       
132100                              TO DP-FOOT-SUVAT-BILLIT-TOT-LOC             
132200     MOVE FOOT-KDVALISO-LOC   TO DP-FOOT-KDVALISO-LOC                     
132300                                                                          
132400     PERFORM S90-WRITE-DOC-FOOT                                           
132500     .                                                                    
132600*                                                                         
132700 BI-HANDLE-FOOT-SND SECTION.                                              
132800*    -- IF NEW SENDING CURRENCY DIFFERENT FROM INVOICE CURRENCY           
132900*          MARK IDAFPRCD WITH '3.2'                                       
133000     MOVE WS-DOC-FOOTER-SND   TO DP-FOOT-IDAFPRCD                         
133100     MOVE FOOT-SUNTO-TOT-SND  TO DP-FOOT-SUNTO-TOT-SND                    
133200     MOVE FOOT-SUBTO-TOT-SND  TO DP-FOOT-SUBTO-TOT-SND                    
133300     MOVE FOOT-SUVAT-BILLIT-TOT-SND                                       
133400                              TO DP-FOOT-SUVAT-BILLIT-TOT-SND             
133500     MOVE FOOT-KDVALISO-SND   TO DP-FOOT-KDVALISO-SND                     
133600                                                                          
133700     PERFORM S90-WRITE-DOC-FOOT                                           
133800     .                                                                    
133900*                                                                         
134000 BJ-HANDLE-DADR SECTION.                                                  
134100*    -- IF NEW DELIVERY ADRESS                                            
134200*          MARK IDAFPRCD WITH '6.1'                                       
134300     IF NEW-DADR-SW = 'J'                                                 
134400       MOVE WS-DOC-DADR              TO DP-DADR-IDAFPRCD                  
134500       MOVE 'N'                      TO NEW-DADR-SW                       
134600       PERFORM S90-WRITE-DOC-DADR                                         
134700     END-IF                                                               
134800     MOVE WS-DOC-DADR-ROW            TO DP-DADR-IDAFPRCD                  
134900     MOVE SAVE-DP-DADR-BETEXT        TO DP-DADR-BETEXT                    
135000                                                                          
135100     PERFORM S90-WRITE-DOC-DADR                                           
135200     .                                                                    
135300                                                                          
135400* DENNA KOD ÄR FÖR ATT SKRIVA SUMMA RAD FÖR FREIGHT, INSURANCE            
135500* , PACKING AND HANDLING FAKTURAVIS                                       
135600 BK-HANDLE-FIPH-SUM SECTION.                                              
135700     MOVE WS-DOC-FIPH-SUM        TO DP-FIPH-IDAFPRCD                      
135800     MOVE WS-BEART(WS-IX)        TO DP-FIPH-BEART                         
135900     MOVE WS-KVLEVART(WS-IX)     TO DP-FIPH-KVLEVART                      
136000     MOVE WS-SUNTO(WS-IX)        TO DP-FIPH-SUNTO                         
136100     IF WS-SUNTO(WS-IX) < ZERO                                            
136200       MOVE '-'                      TO DP-FIPH-REF-MINUS                 
136300     ELSE                                                                 
136400       MOVE SPACE                    TO DP-FIPH-REF-MINUS                 
136500     END-IF                                                               
136600                                                                          
136700     PERFORM S90-WRITE-DOC-FIPH-SUM                                       
136800     .                                                                    
136900                                                                          
137000* DENNA KOD ÄR FÖR ATT SKRIVA VALUTAJUSTERINGEN FÖR FAKTUROR SOM          
137100* ÄR OMRÄKNADE TILL LOKAL VALUTA                                          
137200 BL-HANDLE-LOCC-JUS SECTION.                                              
137300     IF WS-LOCC-JUS-SUNTO = WS-LOCC-JUS-CHECK                             
137400       CONTINUE                                                           
137500     ELSE                                                                 
137600       IF WS-LOCC-JUS-SUNTO > WS-LOCC-JUS-CHECK                           
137700         MOVE WS-DOC-LOCC-JUS        TO DP-LOCC-IDAFPRCD                  
137800         MOVE '-'                    TO DP-LOCC-MINUS                     
137900         COMPUTE WS-LOCC-JUS-SUNTO = WS-LOCC-JUS-CHECK -                  
138000                                     WS-LOCC-JUS-SUNTO                    
138100         MOVE WS-LOCC-JUS-SUNTO      TO DP-LOCC-SUNTO                     
138200       ELSE                                                               
138300         MOVE WS-DOC-LOCC-JUS        TO DP-LOCC-IDAFPRCD                  
138400         MOVE SPACE                  TO DP-LOCC-MINUS                     
138500         COMPUTE WS-LOCC-JUS-SUNTO = WS-LOCC-JUS-CHECK -                  
138600                                     WS-LOCC-JUS-SUNTO                    
138700         MOVE WS-LOCC-JUS-SUNTO      TO DP-LOCC-SUNTO                     
138800       END-IF                                                             
138900       PERFORM S90-WRITE-DOC-LOCC-JUS                                     
139000     END-IF                                                               
139100     .                                                                    
139200                                                                          
139300 Z-FINIT SECTION.                                                         
139400     CLOSE WF2031                                                         
139500           WF2301                                                         
139600     .                                                                    
139700                                                                          
139800 S01-READ-WF2031  SECTION.                                                
139900     READ WF2031                                                          
140000       AT END                                                             
140100         SET END-OF-WF2031 TO TRUE                                        
140200     END-READ                                                             
140300     .                                                                    
140400                                                                          
140500 S90-WRITE-HEADER SECTION.                                                
140600                                                                          
140700     PERFORM S91-SKRIV-DAP1-S                                             
140800     PERFORM S91-SKRIV-DAP2-S                                             
140900     PERFORM S91-SKRIV-DAP3-S                                             
141000     PERFORM S91-SKRIV-DAP4-S                                             
141100     .                                                                    
141200 S90-WRITE-DOC-HEAD SECTION.                                              
141300                                                                          
141400     WRITE WF2301-001   FROM DOC-HEAD-AREA                                
141500     .                                                                    
141600 S90-WRITE-DOC-LINE SECTION.                                              
141700                                                                          
141800     WRITE WF2301-001   FROM DOC-LINE-AREA                                
141900     .                                                                    
142000 S90-WRITE-DOC-FOOT SECTION.                                              
142100                                                                          
142200     WRITE WF2301-001   FROM DOC-FOOT-AREA                                
142300     .                                                                    
142400 S90-WRITE-DOC-APPX SECTION.                                              
142500                                                                          
142600     WRITE WF2301-001   FROM DOC-APPX-AREA                                
142700     .                                                                    
142800 S90-WRITE-DOC-INSUR SECTION.                                             
142900                                                                          
143000     WRITE WF2301-001   FROM DOC-INSUR                                    
143100     .                                                                    
143200 S90-WRITE-ADD-INSUR SECTION.                                             
143300                                                                          
143400     WRITE WF2301-001   FROM DOC-ADD-INSUR                                
143500     .                                                                    
143600 S90-WRITE-DOC-CREDIT SECTION.                                            
143700                                                                          
143800     WRITE WF2301-001   FROM DOC-CREDIT                                   
143900     .                                                                    
144000 S90-WRITE-DOC-FIPH-SUM SECTION.                                          
144100                                                                          
144200     WRITE WF2301-001   FROM DOC-FIPH-SUM                                 
144300     .                                                                    
144400 S90-WRITE-DOC-DADR SECTION.                                              
144500                                                                          
144600     WRITE WF2301-001   FROM DOC-DADR                                     
144700     .                                                                    
144800 S90-WRITE-DOC-LOCC-JUS SECTION.                                          
144900                                                                          
145000     WRITE WF2301-001   FROM DOC-LOCC-JUS                                 
145100     .                                                                    
145200 S91-SKRIV-DAP1-S SECTION.                                                
145300                                                                          
145400     STRING ' ¤DAP' WS-HDR-AREA-1                                         
145500            DELIMITED BY SIZE INTO W001-DAP                               
145600     WRITE WF2301-001     FROM W001-DAP                                   
145700                                                                          
145800     MOVE SPACE TO W001-DAP                                               
145900     .                                                                    
146000                                                                          
146100 S91-SKRIV-DAP2-S SECTION.                                                
146200                                                                          
146300     STRING ' ¤DAP' WS-HDR-AREA-2                                         
146400            DELIMITED BY SIZE INTO W001-DAP                               
146500     WRITE WF2301-001         FROM W001-DAP                               
146600                                                                          
146700     MOVE SPACE TO W001-DAP                                               
146800     .                                                                    
146900 S91-SKRIV-DAP3-S SECTION.                                                
147000                                                                          
147100     STRING ' ¤DAP' WS-HDR-AREA-3                                         
147200            DELIMITED BY SIZE INTO W001-DAP                               
147300     WRITE WF2301-001         FROM W001-DAP                               
147400                                                                          
147500     MOVE SPACE TO W001-DAP                                               
147600     .                                                                    
147700 S91-SKRIV-DAP4-S SECTION.                                                
147800                                                                          
147900     STRING ' ¤METADOCUMENT_DATE=' WS-HDR-AREA-4                          
148000            DELIMITED BY SIZE INTO W001-DAP                               
148100     WRITE WF2301-001         FROM W001-DAP                               
148200                                                                          
148300     MOVE SPACE TO W001-DAP                                               
148400                                                                          
148500     STRING ' ¤METAFINANCIAL_CUSTOMER=' WS-HDR-AREA-5                     
148600            DELIMITED BY SIZE INTO W001-DAP                               
148700     WRITE WF2301-001         FROM W001-DAP                               
148800                                                                          
148900     MOVE SPACE TO W001-DAP                                               
149000                                                                          
149100     STRING ' ¤METADOCUMENT_NUMBER=' WS-HDR-AREA-7                        
149200            DELIMITED BY SIZE INTO W001-DAP                               
149300     WRITE WF2301-001         FROM W001-DAP                               
149400                                                                          
149500     MOVE SPACE TO W001-DAP                                               
149600                                                                          
149700     STRING ' ¤METABUYER=' WS-HDR-AREA-9                                  
149800            DELIMITED BY SIZE INTO W001-DAP                               
149900     WRITE WF2301-001         FROM W001-DAP                               
150000                                                                          
150100     MOVE SPACE TO W001-DAP                                               
150200                                                                          
150300     STRING ' ¤METAMARKET=' WS-HDR-AREA-10                                
150400            DELIMITED BY SIZE INTO W001-DAP                               
150500     WRITE WF2301-001         FROM W001-DAP                               
150600                                                                          
150700     MOVE SPACE TO W001-DAP                                               
150800                                                                          
150900     STRING ' ¤METAFILE_NAME='                                            
151000            DELIMITED BY SIZE                                             
151100            WS-KDFINDOC                                                   
151200            DELIMITED BY SPACE                                            
151300            '_'                                                           
151400            DELIMITED BY SIZE                                             
151500            WS-IDFINDOC1                                                  
151600            DELIMITED BY SIZE                                             
151700            '_'                                                           
151800            DELIMITED BY SIZE                                             
151900            WS-TIMESTAMP                                                  
152000            DELIMITED BY SIZE INTO W001-DAP                               
152100     WRITE WF2301-001         FROM W001-DAP                               
152200                                                                          
152300     MOVE SPACE TO W001-DAP                                               
152400     .                                                                    
152500 S91-SKRIV-DAP5-S SECTION.                                                
152600                                                                          
152700     STRING ' ¤METADISTRICT=' WS-HDR-AREA-6                               
152800            DELIMITED BY SIZE INTO W001-DAP                               
152900     WRITE WF2301-001         FROM W001-DAP                               
153000                                                                          
153100     MOVE SPACE TO W001-DAP                                               
153200                                                                          
153300     STRING ' ¤METADC=' WS-HDR-AREA-8                                     
153400            DELIMITED BY SIZE INTO W001-DAP                               
153500     WRITE WF2301-001         FROM W001-DAP                               
153600                                                                          
153700     MOVE SPACE TO W001-DAP                                               
153800     .                                                                    
