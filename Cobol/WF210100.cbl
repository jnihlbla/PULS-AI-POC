000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF210100.                                                
000300 AUTHOR.         BERNT LUNDH.                                             
000400 DATE-WRITTEN.   2002-04-25.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THE PGM                                                          
001000*        - READS FILE WITH DOCUMENT DATA RECORDS                          
001100*        - WRITES DOCUMENT DATA RECORDS FOR VCCS TO A FILE.               
001200*        - USES WZ14DAP4 IN THE JCL TO SEND THE FILE TO                   
001300*          DISTRIBUTION & PRINT                                           
001400*                                                                         
001500*        INDEX DESCRIBING DIFFERENT LINE-TYPES (IDAFPRCD),                
001600**** DOCUMENT ALWAYS STARTS WITH A HEADER 1                               
001700*        1        HEADER   - STANDARD                                     
001800**** AFTER HEADER COMES ONE TO MANY DETAILED ROWS 2                       
001900*        2        DETAIL   - STANDARD                                     
002000*        2.1      DETAIL   - BREAK                                        
002100*        2.1 SOFT DETAIL   - BREAK, SOFTWARE                              
002200*        2.1 LYNK DETAIL   - BREAK, LYNK                                  
002300*        2.1 TRAC DETAIL   - BREAK, TRACKING NO                           
002400*        2.3      DETAIL   - BREAK, PACKING-HANDLING/FREIGTH ETC          
002500*        2.4      DETAIL   - BREAK, LOCAL CURRENCY ON INVOICE             
002600**** SUMMATION OF THE INVOICE IS DONE IN THE FOOTER 3                     
002700*        3        FOOTER   - STANDARD                                     
002800*        3.1      FOOTER   - LOCAL CURRENCY                               
002900*        3.2      FOOTER   - FOREIGN CURRENCY                             
003000**** ADDITIONAL APPENDIX 4, 5, 6                                          
003100*        4        APPENDIX - STANDARD                                     
003200*        4.1      APPENDIX - BREAK                                        
003300*        5        APPENDIX - STANDARD, INSURANCE TEXT                     
003400*        5.1      APPENDIX - BREAK, INSURANCE TEXT                        
003500*        5.2      APPENDIX - STANDARD, INSURANCE ADDITIONAL               
003600*        5.4      APPENDIX - BREAK, ADDITIONAL INSURANCE TEXT             
003700*        5.5      APPENDIX - STANDARD,ADDITIONAL INSURANCE TEXT           
003800*        6        APPENDIX - STANDARD, CREDIT INFO/DEL. ADDRESS           
003900*        6.1      APPENDIX - BREAK, CREDIT INFO/DEL. ADDRESS              
004000*                                                                         
004100 ENVIRONMENT DIVISION.                                                    
004200                                                                          
004300 INPUT-OUTPUT SECTION.                                                    
004400                                                                          
004500 FILE-CONTROL.                                                            
004600*          --- DOCUMENT DATA RECORDS                                      
004700     SELECT WF2011                     ASSIGN TO WF2101D1.                
004800*          --- DOCUMENT DATA RECORDS - OUTPUT                             
004900     SELECT WF2101                     ASSIGN TO WF2101D2.                
005000     EJECT                                                                
005100 DATA DIVISION.                                                           
005200                                                                          
005300 FILE SECTION.                                                            
005400 FD  WF2011                                                               
005500     RECORDING       V                                                    
005600     BLOCK CONTAINS  0.                                                   
005700                                                                          
005800 01  IN-DATA.                                                             
005900     03  IN-IDPTYP               PIC X(3).                                
006000     03  FILLER                  PIC X(16).                               
006100     03  IN-IDLEGSEL             PIC X(4).                                
006200     03  FILLER                  PIC X(5672).                             
006300     EJECT                                                                
006400 FD  WF2101                                                               
006500     RECORDING       V                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800 01  WF2101-001                  PIC X(3000).                             
006900     EJECT                                                                
007000                                                                          
007100 WORKING-STORAGE SECTION.                                                 
007200                                                                          
007300 77  IDPGM                       PIC X(8)    VALUE 'WF210100'.            
007400 77  YES                         PIC X(1)    VALUE 'J'.                   
007500 77  NOO                         PIC X(1)    VALUE 'N'.                   
007600 77  WS-ZERO                     PIC X(5)    VALUE '00000'.               
007700 77  WS-NINE                     PIC X(5)    VALUE '99999'.               
007800 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
007900 77  WS-DOC-HEAD                 PIC X(3)    VALUE '1  '.                 
008000 77  WS-DOC-LINE                 PIC X(3)    VALUE '2  '.                 
008100 77  WS-DOC-LINE-SOFT            PIC X(8)    VALUE '2   SOFT'.            
008200 77  WS-DOC-LINE-TRAC            PIC X(8)    VALUE '2   TRAC'.            
008300 77  WS-DOC-LINE-LYNK            PIC X(8)    VALUE '2   LYNK'.            
008400 77  WS-DOC-GROUP                PIC X(3)    VALUE '2.1'.                 
008500 77  WS-DOC-GROUP-SOFT           PIC X(8)    VALUE '2.1 SOFT'.            
008600 77  WS-DOC-GROUP-TRAC           PIC X(8)    VALUE '2.1 TRAC'.            
008700 77  WS-DOC-GROUP-LYNK           PIC X(8)    VALUE '2.1 LYNK'.            
008800 77  WS-DOC-FIPH-SUM             PIC X(3)    VALUE '2.3'.                 
008900 77  WS-DOC-LOCC-JUS             PIC X(3)    VALUE '2.4'.                 
009000 77  WS-DOC-PROF                 PIC X(3)    VALUE '2.5'.                 
009100 77  WS-DOC-FOOTER               PIC X(3)    VALUE '3  '.                 
009200 77  WS-DOC-FOOTER-LOC           PIC X(3)    VALUE '3.1'.                 
009300 77  WS-DOC-FOOTER-SND           PIC X(3)    VALUE '3.2'.                 
009400 77  WS-DOC-NEW-APPENDIX         PIC X(3)    VALUE '4.1'.                 
009500 77  WS-DOC-APPENDIX             PIC X(3)    VALUE '4  '.                 
009600 77  WS-DOC-INSURANCE            PIC X(3)    VALUE '5.1'.                 
009700 77  WS-DOC-INSURANCE-ROW        PIC X(3)    VALUE '5  '.                 
009800 77  WS-DOC-INSURANCE-ADD        PIC X(3)    VALUE '5.2'.                 
009900 77  WS-DOC-ADD-INSURANCE        PIC X(3)    VALUE '5.3'.                 
010000 77  WS-DOC-ADD-INSURANCE-ROW    PIC X(3)    VALUE '5.4'.                 
010100 77  WS-DOC-CREDIT               PIC X(3)    VALUE '6.1'.                 
010200 77  WS-DOC-CREDIT-ROW           PIC X(3)    VALUE '6  '.                 
010300 77  WS-DOC-DADR                 PIC X(3)    VALUE '6.1'.                 
010400 77  WS-DOC-DADR-ROW             PIC X(3)    VALUE '6  '.                 
010500 77  WS-IX                       PIC S9(3)   COMP-3 VALUE ZERO.           
010600 77  WS-IDFINDOC                 PIC 9(9)    VALUE ZERO.                  
010700 77  WS-IDREF                    PIC X(15)   VALUE SPACE.                 
010800 77  WS-IDREF-NUM                PIC 9(05)   VALUE ZERO.                  
010900 77  WS-BETEXT                   PIC X(50)   VALUE SPACE.                 
011000 77  WS-BEANST                   PIC X(35)   VALUE SPACE.                 
011100 77  WS-IDEXCUST-2-OLD           PIC X(15)   VALUE SPACE.                 
011200 77  WS-IDFINDOC-OLD             PIC 9(9)    VALUE ZERO.                  
011300 77  WS-IDREF-OLD                PIC X(15)   VALUE SPACE.                 
011400 77  WS-DAREFDAT-OLD             PIC 9(8)    VALUE ZERO.                  
011500 77  WS-KDFRAKT                  PIC 9(2)    VALUE ZERO.                  
011600 77  WS-HEAD-PRKURS            PIC S9(6)V9(5) COMP-3 VALUE ZERO.          
011700 77  WS-SUNTO-SND-LOC          PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
011800 77  WS-SUNTO-FLCURRND         PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
011900 77  WS2-PRARTBTO              PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
012000 77  WS2-KVLEVART              PIC S9(11) COMP-3 VALUE ZERO.              
012100 77  WS-LOCC-JUS-SUNTO         PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
012200 77  WS2-LOCC-JUS-SUNTO        PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
012300 77  WS-LOCC-JUS-CHECK         PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
012400 77  PROF-SUNTO                PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
012500 77  WS-KDFINDOC               PIC X(4) VALUE SPACES.                     
012600                                                                          
012700 01  WS-TIMESTAMP.                                                        
012800     03  FILLER                  PIC X       VALUE 'D'.                   
012900     03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
013000     03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
013100     03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
013200     03  FILLER                  PIC X       VALUE '_'.                   
013300     03  FILLER                  PIC X       VALUE 'T'.                   
013400     03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
013500     03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
013600     03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
013700                                                                          
013800 77  WF2011-FIRST-RECORD         PIC X       VALUE ' '.                   
013900     88  FIRST-RECORD                        VALUE 'J'.                   
014000     88  OTHER-RECORDS                       VALUE 'N'.                   
014100                                                                          
014200 77  WF2011-EOF-SW               PIC X       VALUE 'N'.                   
014300     88  END-OF-WF2011                       VALUE 'J'.                   
014400                                                                          
014500 77  NEW-HEAD-SW                 PIC X       VALUE 'J'.                   
014600     88  NEW-HEAD                            VALUE 'J'.                   
014700     88  OTHER-ROW                           VALUE 'N'.                   
014800                                                                          
014900 77  NEW-FIPH-SUM-ROW-SW         PIC X       VALUE 'N'.                   
015000     88  NEW-FIPH-SUM-ROW                    VALUE 'J'.                   
015100     88  NON-FIPH-SUM-ROW                    VALUE 'N'.                   
015200                                                                          
015300 77  NEW-LOCC-JUS-ROW-SW         PIC X       VALUE 'N'.                   
015400     88  NEW-LOCC-JUS-ROW                    VALUE 'J'.                   
015500     88  NON-LOCC-JUS-ROW                    VALUE 'N'.                   
015600                                                                          
015700 77  NEW-APPENDIX-SW             PIC X       VALUE 'J'.                   
015800     88  NEW-APPENDIX                        VALUE 'J'.                   
015900                                                                          
016000 77  NEW-INSURANCE-SW            PIC X       VALUE 'N'.                   
016100     88  NEW-INSURANCE                       VALUE 'J'.                   
016200     88  NO-INSURANCE                        VALUE 'N'.                   
016300                                                                          
016400 77  NEW-ADD-INSURANCE-SW        PIC X       VALUE 'N'.                   
016500     88  NEW-ADD-INSURANCE                   VALUE 'J'.                   
016600     88  NO-ADD-INSURANCE                    VALUE 'N'.                   
016700                                                                          
016800 77  NEW-CREDIT-SW               PIC X       VALUE 'N'.                   
016900     88  NEW-CREDIT                          VALUE 'J'.                   
017000     88  NO-CREDIT                           VALUE 'N'.                   
017100                                                                          
017200 77  NEW-DADR-SW                 PIC X       VALUE 'N'.                   
017300     88  NEW-DADR                            VALUE 'J'.                   
017400     88  NO-DADR                             VALUE 'N'.                   
017500                                                                          
017600 77  YES-PROF-SW                 PIC X       VALUE 'N'.                   
017700     88  YES-PROF                            VALUE 'J'.                   
017800     88  NO-PROF                             VALUE 'N'.                   
017900                                                                          
018000 77  WS-FOOT-LOC-SW              PIC X       VALUE 'N'.                   
018100     88  NEW-FOOT-LOC                        VALUE 'J'.                   
018200     88  NO-FOOT-LOC                         VALUE 'N'.                   
018300                                                                          
018400 77  WS-FOOT-SND-SW              PIC X       VALUE 'N'.                   
018500     88  NEW-FOOT-SND                        VALUE 'J'.                   
018600     88  NO-FOOT-SND                         VALUE 'N'.                   
018700                                                                          
018800 01  WS-LINE-IDAPPEND            PIC 9(02).                               
018900     88 KDPRODSL-LYNK                       VALUE 31 THRU 39.             
019000                                                                          
019100 01  WS-SUNTO-CHK                PIC 9(13).9.                             
019200 01  SUNTO-DEC REDEFINES WS-SUNTO-CHK.                                    
019300     03 WS-NUMERIC-SUNTO         PIC Z(11).                               
019400     03 WS-DOT-SUNTO             PIC X(1).                                
019500     03 WS-DECIMAL-SUNTO         PIC 9(2).                                
019600 01  SUBTO-NDEC REDEFINES WS-SUNTO-CHK.                                   
019700     03 WS-NUMERIC-SUNTO-NUM     PIC Z(11).                               
019800     03 FILLER                   PIC X(3).                                
019900                                                                          
020000 01  WS-SUBTO-CHK                PIC 9(13).9.                             
020100 01  SUBTO-DEC REDEFINES WS-SUBTO-CHK.                                    
020200     03 WS-NUMERIC-SUBTO         PIC Z(11).                               
020300     03 WS-DOT-SUBTO             PIC X(1).                                
020400     03 WS-DECIMAL-SUBTO         PIC 9(2).                                
020500 01  SUBTO-NDEC REDEFINES WS-SUBTO-CHK.                                   
020600     03 WS-NUMERIC-SUBTO-NUM     PIC Z(11).                               
020700     03 FILLER                   PIC X(3).                                
020800                                                                          
020900 01  WS-SUVAT-CHK                PIC 9(13).9.                             
021000 01  SUVAT-DEC REDEFINES WS-SUVAT-CHK.                                    
021100     03 WS-NUMERIC-SUVAT         PIC Z(11).                               
021200     03 WS-DOT-SUVAT             PIC X(1).                                
021300     03 WS-DECIMAL-SUVAT         PIC 9(2).                                
021400 01  SUBTO-NDEC REDEFINES WS-SUVAT-CHK.                                   
021500     03 WS-NUMERIC-SUVAT-NUM     PIC Z(11).                               
021600     03 FILLER                   PIC X(3).                                
021700                                                                          
021800 01  WS-TABELL-FIPH-SUM.                                                  
021900     03 TABELL OCCURS 35.                                                 
022000       05 WS-BEART               PIC X(25).                               
022100       05 WS-KVLEVART            PIC S9(6) COMP-3.                        
022200       05 WS-SUNTO               PIC S9(11)V9(2) COMP-3.                  
022300                                                                          
022400 01  ERRTEXT.                                                             
022500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
022600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
022700 01  KDRC-DISPLAY                PIC Z(5).                                
022800     EJECT                                                                
022900                                                                          
023000 01  GENERAL-SUBPROGRAMS.                                                 
023100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
023200     EJECT                                                                
023300                                                                          
023400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
023500                                                                          
023600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
023700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
023800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
023900     EJECT                                                                
024000                                                                          
024100                                                                          
024200*    --- IN-AREOR                                                         
024300 01  INPUT-AREA                 PIC X(24)   VALUE                         
024400                                'INPUT-AREA     '.                        
024500 01  IN-AREA-HEAD.                                                        
024600*    03  -COPY WF201101                                                   
024700                                                                          
024800 01  IN-AREA-LINE.                                                        
024900*    03  -COPY WF201102                                                   
025000                                                                          
025100 01  IN-AREA-FOOT.                                                        
025200*    03  -COPY WF201103                                                   
025300                                                                          
025400 01  IN-AREA-APPX.                                                        
025500*    03  -COPY WF201104                                                   
025600     EJECT                                                                
025700                                                                          
025800 01  W001-DAP.                                                            
025900     03  FILLER              PIC X(165)  VALUE SPACE.                     
026000     EJECT                                                                
026100*    --- UT-AREOR                                                         
026200 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
026300                                'OUTPUT-AREA     '.                       
026400 01  WS-HDR-AREA-1.                                                       
026500     03 WS-IDOUTTYPE             PIC X(15).                               
026600     EJECT                                                                
026700 01  WS-HDR-AREA-2.                                                       
026800     03 WS-IDOUTREC              PIC X(30).                               
026900     EJECT                                                                
027000 01  WS-HDR-AREA-3.                                                       
027100     03 WS-IDLIST                PIC X(10).                               
027200     EJECT                                                                
027300 01  WS-HDR-AREA-4.                                                       
027400     03 WS-DAFINDOC              PIC 9(8).                                
027500     EJECT                                                                
027600 01  WS-HDR-AREA-5.                                                       
027700     03 WS-IDPARTNR              PIC X(9).                                
027800     EJECT                                                                
027900 01  WS-HDR-AREA-6.                                                       
028000     03 WS-IDEXCUST-1            PIC X(15).                               
028100     EJECT                                                                
028200 01  WS-HDR-AREA-7.                                                       
028300     03 WS-IDFINDOC1             PIC X(9).                                
028400     EJECT                                                                
028500 01  WS-HDR-AREA-8.                                                       
028600     03 WS-IDDC                  PIC X(2).                                
028700     EJECT                                                                
028800 01  WS-HDR-AREA-9.                                                       
028900     03 WS-BEBET-NAME1           PIC X(35).                               
029000     EJECT                                                                
029100 01  WS-HDR-AREA-10.                                                      
029200     03 WS-KDTRADP               PIC X(4).                                
029300     EJECT                                                                
029400                                                                          
029500 01  DOC-HEAD-AREA.                                                       
029600*    03  -COPY WF210101                                                   
029700     EJECT                                                                
029800                                                                          
029900 01  DOC-LINE-AREA.                                                       
030000*    03  -COPY WF210102                                                   
030100     EJECT                                                                
030200                                                                          
030300***************************************************************           
030400*RDEFINE THE BELOW FIELD FROM WF210103 TO GET NEW FIELD WITH *            
030500*OUT DECIMAL VERSION                                          *           
030600***************************************************************           
030700 01  DOC-FOOT-AREA.                                                       
030800*    03  -COPY WF210103                                                   
030900     03 FILLER REDEFINES DP-FOOT-WF2101.                                  
031000       05 FILLER PIC X(52).                                               
031100       05 D1-FOOT-SUNTO-TOT-LOC  PIC Z(13)9.                              
031200       05 D1-FOOT-SUVAT-BILLIT-TOT-LOC PIC Z(13)9.                        
031300       05 D1-FOOT-SUBTO-TOT-LOC  PIC Z(13)9.                              
031400       05 FILLER PIC X(267).                                              
031500     03 FILLER REDEFINES DP-FOOT-WF2101.                                  
031600       05 FILLER PIC X(10).                                               
031700       05 D2-FOOT-SUNTO-TOT  PIC Z(13)9.                                  
031800       05 D2-FOOT-SUVAT-BILLIT-TOT PIC Z(13)9.                            
031900       05 D2-FOOT-SUBTO-TOT  PIC Z(13)9.                                  
032000       05 FILLER PIC X(309).                                              
032100     EJECT                                                                
032200                                                                          
032300 01  DOC-APPX-AREA.                                                       
032400*    03  -COPY WF210104                                                   
032500     EJECT                                                                
032600                                                                          
032700 01  DOC-INSUR.                                                           
032800*    03  -COPY WF210106                                                   
032900     EJECT                                                                
033000                                                                          
033100 01  DOC-CREDIT.                                                          
033200*    03  -COPY WF210107                                                   
033300     EJECT                                                                
033400                                                                          
033500 01  DOC-DADR.                                                            
033600*    03  -COPY WF210108                                                   
033700     EJECT                                                                
033800                                                                          
033900 01  DOC-FIPH-SUM.                                                        
034000*    03  -COPY WF210109                                                   
034100     EJECT                                                                
034200                                                                          
034300 01  DOC-LOCC-JUS.                                                        
034400*    03  -COPY WF210110                                                   
034500     EJECT                                                                
034600                                                                          
034700 01  DOC-PROF.                                                            
034800*    03  -COPY WF210111                                                   
034900     EJECT                                                                
035000                                                                          
035100 01  DOC-ADD-INSUR.                                                       
035200*    03  -COPY WF210112                                                   
035300     EJECT                                                                
035400                                                                          
035500*    --- SAVE-AREOR                                                       
035600 01  SAVE-AREA                 PIC X(24)   VALUE                          
035700                                'SAVE-AREA     '.                         
035800                                                                          
035900 01  SAVE-DP-INSUR.                                                       
036000*    03  -COPY WF210106 -PRE SAVE-                                        
036100     EJECT                                                                
036200                                                                          
036300 01  SAVE-DP-CREDI.                                                       
036400*    03  -COPY WF210107 -PRE SAVE-                                        
036500     EJECT                                                                
036600                                                                          
036700 01  SAVE-DP-DADR.                                                        
036800*    03  -COPY WF210108 -PRE SAVE-                                        
036900     EJECT                                                                
037000                                                                          
037100 01  SAVE-FIPH-SUM.                                                       
037200*    03  -COPY WF210109 -PRE SAVE-                                        
037300     EJECT                                                                
037400                                                                          
037500 01  SAVE-LOCC-JUS.                                                       
037600*    03  -COPY WF210110 -PRE SAVE-                                        
037700     EJECT                                                                
037800                                                                          
037900 01  SAVE-DP-ADD-INSUR.                                                   
038000*    03  -COPY WF210112 -PRE SAVE-                                        
038100     EJECT                                                                
038200                                                                          
038300 LINKAGE SECTION.                                                         
038400                                                                          
038500 PROCEDURE DIVISION.                                                      
038600 MAIN SECTION.                                                            
038700                                                                          
038800     PERFORM A-INIT                                                       
038900     PERFORM B-EXECUTE                                                    
039000     PERFORM Z-FINIT                                                      
039100                                                                          
039200     MOVE ZERO TO RETURN-CODE                                             
039300     GOBACK                                                               
039400     .                                                                    
039500                                                                          
039600 A-INIT SECTION.                                                          
039700     OPEN INPUT WF2011                                                    
039800                                                                          
039900     OPEN OUTPUT WF2101                                                   
040000     MOVE NOO TO NEW-INSURANCE-SW                                         
040100     MOVE NOO TO NEW-ADD-INSURANCE-SW                                     
040200     MOVE NOO TO NEW-CREDIT-SW                                            
040300     MOVE NOO TO NEW-DADR-SW                                              
040400     MOVE NOO TO YES-PROF-SW                                              
040500     MOVE NOO TO WS-FOOT-LOC-SW                                           
040600     MOVE NOO TO WS-FOOT-SND-SW                                           
040700     MOVE ZERO TO WS-IX                                                   
040800                                                                          
040900     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
041000     MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
041100     MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
041200     MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
041300     MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
041400     MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
041500     .                                                                    
041600                                                                          
041700 B-EXECUTE SECTION.                                                       
041800     PERFORM S01-READ-WF2011                                              
041900                                                                          
042000     IF END-OF-WF2011                                                     
042100       CONTINUE                                                           
042200     ELSE                                                                 
042300       MOVE YES TO WF2011-FIRST-RECORD                                    
042400                                                                          
042500       PERFORM UNTIL END-OF-WF2011                                        
042600         IF IN-IDLEGSEL = WS-IDLEGSEL-VCCS                                
042700           IF IN-IDPTYP = WS-DOC-HEAD                                     
042800             MOVE YES TO NEW-HEAD-SW                                      
042900             MOVE +1 TO WS-IX                                             
043000             PERFORM UNTIL WS-IX > 35                                     
043100               MOVE SPACE          TO WS-BEART(WS-IX)                     
043200               MOVE 0              TO WS-KVLEVART(WS-IX)                  
043300               MOVE 0              TO WS-SUNTO(WS-IX)                     
043400               ADD +1 TO WS-IX                                            
043500             END-PERFORM                                                  
043600             IF FIRST-RECORD                                              
043700               MOVE NOO TO WF2011-FIRST-RECORD                            
043800             ELSE                                                         
043900*IF THE PREVIOUS RECORD HAS AN INSURANCE TEXT TO BE PRINTED               
044000               IF NEW-INSURANCE                                           
044100                 PERFORM BF-HANDLE-INSUR                                  
044200               END-IF                                                     
044300*IF THE PREVIOUS RECORD HAS AN ADDITIONAL INSURANCE TEXT TO BE            
044400*PRINTED                                                                  
044500               IF NEW-ADD-INSURANCE                                       
044600                 PERFORM BF-HANDLE-ADD-INSUR                              
044700               END-IF                                                     
044800*IF THE PREVIOUS RECORD HAS A CREDIT TEXT TO BE PRINTED                   
044900               IF NEW-CREDIT                                              
045000                 PERFORM BG-HANDLE-CREDIT                                 
045100               END-IF                                                     
045200*IF THE PREVIOUS RECORD HAS A DELIVERY ADRESS TO BE PRINTED               
045300               IF NEW-DADR                                                
045400                 PERFORM BJ-HANDLE-DADR                                   
045500               END-IF                                                     
045600             END-IF                                                       
045700             PERFORM BA-HANDLE-HEADER                                     
045800           ELSE                                                           
045900             IF IN-IDPTYP = WS-DOC-LINE                                   
046000               PERFORM BB-HANDLE-LINES                                    
046100             ELSE                                                         
046200               IF IN-IDPTYP = WS-DOC-FOOTER                               
046300                 PERFORM BC-HANDLE-FOOTER                                 
046400               ELSE                                                       
046500                 IF IN-IDPTYP = WS-DOC-APPENDIX                           
046600                   PERFORM BD-HANDLE-APPENDIX                             
046700                 END-IF                                                   
046800               END-IF                                                     
046900             END-IF                                                       
047000           END-IF                                                         
047100         END-IF                                                           
047200         PERFORM S01-READ-WF2011                                          
047300       END-PERFORM                                                        
047400                                                                          
047500*IF THE LAST RECORD HAS AN INSURANCE TEXT TO BE PRINTED                   
047600       IF NEW-INSURANCE                                                   
047700         PERFORM BF-HANDLE-INSUR                                          
047800       END-IF                                                             
047900                                                                          
048000*IF THE PREVIOUS RECORD HAS AN ADDITIONAL INSURANCE TEXT TO BE            
048100*PRINTED                                                                  
048200       IF NEW-ADD-INSURANCE                                               
048300         PERFORM BF-HANDLE-ADD-INSUR                                      
048400       END-IF                                                             
048500*IF THE LAST RECORD HAS A CREDIT TEXT TO BE PRINTED                       
048600       IF NEW-CREDIT                                                      
048700         PERFORM BG-HANDLE-CREDIT                                         
048800       END-IF                                                             
048900                                                                          
049000*IF THE LAST RECORD HAS A DELIVERY ADRESS TO BE PRINTED                   
049100       IF NEW-DADR                                                        
049200         PERFORM BJ-HANDLE-DADR                                           
049300       END-IF                                                             
049400     END-IF                                                               
049500     .                                                                    
049600                                                                          
049700 BA-HANDLE-HEADER SECTION.                                                
049800     MOVE 'J'                        TO NEW-APPENDIX-SW                   
049900     MOVE SPACE                      TO WS-IDEXCUST-2-OLD                 
050000     MOVE IN-DATA                    TO IN-AREA-HEAD                      
050100                                                                          
050200     MOVE HEAD-BEFORMS               TO WS-IDOUTTYPE                      
050300     MOVE SPACE                      TO WS-IDOUTREC                       
050400* DENNA KOD ÄR FÖR ATT SKRIVA VALUTAJUSTERINGEN FÖR FAKTUROR SOM          
050500* ÄR OMRÄKNADE TILL LOKAL VALUTA                                          
050600     MOVE ZERO TO WS-LOCC-JUS-SUNTO                                       
050700*                                                                         
050800* DENNA KOD HÄRRÖR FRÅN ÄT GÄLLANDE MIN-GRÄNS FÖR                         
050900* UTSKRIFT AV KREDITNOTA                                                  
051000     IF HEAD-KDFINDOC = 'CR'                                              
051100       IF HEAD-SUBTO-TOT < HEAD-SUDOCLIM                                  
051200         MOVE 'X0'                   TO WS-IDOUTREC(1:2)                  
051300         MOVE HEAD-IDLANDX3-BET(1:2) TO WS-IDOUTREC(3:2)                  
051400       ELSE                                                               
051500         MOVE 'X1'                   TO WS-IDOUTREC(1:2)                  
051600         MOVE HEAD-IDLANDX3-BET(1:2) TO WS-IDOUTREC(3:2)                  
051700       END-IF                                                             
051800* DENNA KOD HÄRRÖR SÅ ATT JAPAN OCH AUSTRALIEN FÅR                        
051900* UTSKRIFT AV KREDITNOTA SOM ÄR SKAPADE I LANDET                          
052000       IF HEAD-IDLANDX3-BET(1:2) = 'JP'                                   
052100         IF HEAD-IDPARTNR = '357346'                                      
052200           IF HEAD-IDBREAK-2(1:2) = '61' OR '6A'                          
052300             MOVE 'X2'               TO WS-IDOUTREC(1:2)                  
052400           END-IF                                                         
052500         END-IF                                                           
052600       END-IF                                                             
052700       IF HEAD-IDLANDX3-BET(1:2) = 'AU'                                   
052800         IF HEAD-IDPARTNR = '17828'                                       
052900           IF HEAD-IDBREAK-2(1:2) = '62'                                  
053000             MOVE 'X2'               TO WS-IDOUTREC(1:2)                  
053100           END-IF                                                         
053200         END-IF                                                           
053300       END-IF                                                             
053400     ELSE                                                                 
053500       MOVE HEAD-IDLANDX3-SEND(1:2)  TO WS-IDOUTREC(1:2)                  
053600       MOVE HEAD-IDLANDX3-BET(1:2)   TO WS-IDOUTREC(3:2)                  
053700     END-IF                                                               
053800* SLUTAR HÄR                                                              
053900     IF  HEAD-FLSOFT        = 'J'                                         
054000     AND HEAD-IDLANDX3-SEND = 'SE'                                        
054100       MOVE WS-NINE                  TO WS-IDOUTREC(5:5)                  
054200     ELSE                                                                 
054300       IF HEAD-IDLEVNR > SPACE                                            
054400         IF HEAD-KDFINDOC = 'CR'                                          
054500         OR HEAD-KDFINDOC = 'INT2'                                        
054600           MOVE WS-ZERO              TO WS-IDOUTREC(5:5)                  
054700           MOVE SPACE                TO HEAD-IDLEVNR                      
054800         ELSE                                                             
054900           MOVE HEAD-IDLEVNR         TO WS-IDOUTREC(5:5)                  
055000         END-IF                                                           
055100       ELSE                                                               
055200         MOVE WS-ZERO                TO WS-IDOUTREC(5:5)                  
055300       END-IF                                                             
055400     END-IF                                                               
055500     MOVE HEAD-IDPARTNR              TO WS-IDOUTREC(10:9)                 
055600                                        WS-IDPARTNR                       
055700* TO ADD KDFRAKT/FC. APPLICABLE FOR INVOICE, INTERNAL                     
055800     IF (HEAD-KDFINDOC = 'INV' AND                                        
055900        HEAD-BEFORMS = 'INVOICE') OR                                      
056000        (HEAD-KDFINDOC = 'INT' AND                                        
056100        HEAD-BEFORMS = 'INTERNAL')                                        
056200       MOVE HEAD-KDFRAKT             TO WS-KDFRAKT                        
056300       MOVE WS-KDFRAKT               TO WS-IDOUTREC(19:2)                 
056400     END-IF                                                               
056500                                                                          
056600     MOVE HEAD-KDFINDOC              TO WS-KDFINDOC                       
056700     MOVE HEAD-IDFINDOC              TO WS-IDLIST                         
056800                                        WS-IDFINDOC1                      
056900     MOVE HEAD-DAFINDOC              TO WS-DAFINDOC                       
057000     MOVE HEAD-KDTRADP               TO WS-KDTRADP                        
057100     PERFORM S90-WRITE-HEADER                                             
057200                                                                          
057300     MOVE HEAD-IDPTYP                TO DP-HEAD-IDAFPRCD                  
057400     MOVE HEAD-IDFINDOC              TO DP-HEAD-IDFINDOC                  
057500                                        DP-APPX-IDFINDOC                  
057600                                        DP-INSUR-IDFINDOC                 
057700                                        DP-ADD-IDFINDOC                   
057800                                        DP-CREDI-IDFINDOC                 
057900                                        DP-DADR-IDFINDOC                  
058000     MOVE HEAD-DAFINDOC              TO DP-HEAD-DAFINDOC                  
058100                                        DP-APPX-DAFINDOC                  
058200                                        DP-INSUR-DAFINDOC                 
058300                                        DP-ADD-DAFINDOC                   
058400                                        DP-CREDI-DAFINDOC                 
058500                                        DP-DADR-DAFINDOC                  
058600     MOVE HEAD-BELEGRAD-1            TO DP-HEAD-BELEGRAD-1                
058700                                        DP-APPX-BELEGRAD-1                
058800                                        DP-INSUR-BELEGRAD-1               
058900                                        DP-ADD-BELEGRAD-1                 
059000                                        DP-CREDI-BELEGRAD-1               
059100                                        DP-DADR-BELEGRAD-1                
059200     MOVE HEAD-BELEGRAD-2            TO DP-HEAD-BELEGRAD-2                
059300                                        DP-APPX-BELEGRAD-2                
059400                                        DP-INSUR-BELEGRAD-2               
059500                                        DP-ADD-BELEGRAD-2                 
059600                                        DP-CREDI-BELEGRAD-2               
059700                                        DP-DADR-BELEGRAD-2                
059800     MOVE HEAD-ADLEG-STREET          TO DP-HEAD-ADLEG-STREET              
059900                                        DP-APPX-ADLEG-STREET              
060000                                        DP-INSUR-ADLEG-STREET             
060100                                        DP-ADD-ADLEG-STREET               
060200                                        DP-CREDI-ADLEG-STREET             
060300                                        DP-DADR-ADLEG-STREET              
060400     MOVE HEAD-ADLEG-BOX             TO DP-HEAD-ADLEG-BOX                 
060500                                        DP-APPX-ADLEG-BOX                 
060600                                        DP-INSUR-ADLEG-BOX                
060700                                        DP-ADD-ADLEG-BOX                  
060800                                        DP-CREDI-ADLEG-BOX                
060900                                        DP-DADR-ADLEG-BOX                 
061000     MOVE HEAD-ADLEG-CITY            TO DP-HEAD-ADLEG-CITY                
061100                                        DP-APPX-ADLEG-CITY                
061200                                        DP-INSUR-ADLEG-CITY               
061300                                        DP-ADD-ADLEG-CITY                 
061400                                        DP-CREDI-ADLEG-CITY               
061500                                        DP-DADR-ADLEG-CITY                
061600     MOVE HEAD-ADLEG-PCODE           TO DP-HEAD-ADLEG-PCODE               
061700                                        DP-APPX-ADLEG-PCODE               
061800                                        DP-INSUR-ADLEG-PCODE              
061900                                        DP-ADD-ADLEG-PCODE                
062000                                        DP-CREDI-ADLEG-PCODE              
062100                                        DP-DADR-ADLEG-PCODE               
062200     MOVE HEAD-BELAND-LEG            TO DP-HEAD-BELAND-LEG                
062300                                        DP-APPX-BELAND-LEG                
062400                                        DP-INSUR-BELAND-LEG               
062500                                        DP-ADD-BELAND-LEG                 
062600                                        DP-CREDI-BELAND-LEG               
062700                                        DP-DADR-BELAND-LEG                
062800     MOVE HEAD-IDTFN-LEG             TO DP-HEAD-IDTFN-LEG                 
062900                                        DP-APPX-IDTFN-LEG                 
063000                                        DP-INSUR-IDTFN-LEG                
063100                                        DP-ADD-IDTFN-LEG                  
063200                                        DP-CREDI-IDTFN-LEG                
063300                                        DP-DADR-IDTFN-LEG                 
063400     MOVE HEAD-IDTFX-LEG             TO DP-HEAD-IDTFX-LEG                 
063500                                        DP-APPX-IDTFX-LEG                 
063600                                        DP-INSUR-IDTFX-LEG                
063700                                        DP-ADD-IDTFX-LEG                  
063800                                        DP-CREDI-IDTFX-LEG                
063900                                        DP-DADR-IDTFX-LEG                 
064000     MOVE HEAD-IDBG-LEG              TO DP-HEAD-IDBG-LEG                  
064100                                        DP-APPX-IDBG-LEG                  
064200                                        DP-INSUR-IDBG-LEG                 
064300                                        DP-ADD-IDBG-LEG                   
064400                                        DP-CREDI-IDBG-LEG                 
064500                                        DP-DADR-IDBG-LEG                  
064600     MOVE HEAD-IDPG-LEG              TO DP-HEAD-IDPG-LEG                  
064700                                        DP-APPX-IDPG-LEG                  
064800                                        DP-INSUR-IDPG-LEG                 
064900                                        DP-ADD-IDPG-LEG                   
065000                                        DP-CREDI-IDPG-LEG                 
065100                                        DP-DADR-IDPG-LEG                  
065200     MOVE HEAD-IDVAT-LEG             TO DP-HEAD-IDVAT-LEG                 
065300                                        DP-APPX-IDVAT-LEG                 
065400                                        DP-INSUR-IDVAT-LEG                
065500                                        DP-ADD-IDVAT-LEG                  
065600                                        DP-CREDI-IDVAT-LEG                
065700                                        DP-DADR-IDVAT-LEG                 
065800     MOVE HEAD-IDVAT-AGENT           TO DP-HEAD-IDVAT-AGENT               
065900                                        DP-APPX-IDVAT-AGENT               
066000                                        DP-INSUR-IDVAT-AGENT              
066100                                        DP-ADD-IDVAT-AGENT                
066200                                        DP-CREDI-IDVAT-AGENT              
066300                                        DP-DADR-IDVAT-AGENT               
066400     MOVE HEAD-BERESPRA-1            TO DP-HEAD-BERESPRA-1                
066500                                        DP-APPX-BERESPRA-1                
066600                                        DP-INSUR-BERESPRA-1               
066700                                        DP-ADD-BERESPRA-1                 
066800                                        DP-CREDI-BERESPRA-1               
066900                                        DP-DADR-BERESPRA-1                
067000     MOVE HEAD-IDTFN-RESP            TO DP-HEAD-IDTFN-RESP                
067100                                        DP-APPX-IDTFN-RESP                
067200                                        DP-INSUR-IDTFN-RESP               
067300                                        DP-ADD-IDTFN-RESP                 
067400                                        DP-CREDI-IDTFN-RESP               
067500                                        DP-DADR-IDTFN-RESP                
067600     MOVE HEAD-IDTFX-RESP            TO DP-HEAD-IDTFX-RESP                
067700                                        DP-APPX-IDTFX-RESP                
067800                                        DP-INSUR-IDTFX-RESP               
067900                                        DP-ADD-IDTFX-RESP                 
068000                                        DP-CREDI-IDTFX-RESP               
068100                                        DP-DADR-IDTFX-RESP                
068200     MOVE HEAD-IDVAT-RESP            TO DP-HEAD-IDVAT-RESP                
068300                                        DP-APPX-IDVAT-RESP                
068400                                        DP-INSUR-IDVAT-RESP               
068500                                        DP-ADD-IDVAT-RESP                 
068600                                        DP-CREDI-IDVAT-RESP               
068700                                        DP-DADR-IDVAT-RESP                
068800**** DIRTY FIX FOR SENDING OUT GOODS FROM IT                              
068900     IF  HEAD-IDLANDX3-SEND(1:2) = 'IT'                                   
069000     AND HEAD-IDLANDX3-BET(1:2) NOT = 'IT'                                
069100       MOVE 'IT04265320376'  TO DP-HEAD-IDVAT-RESP                        
069200                                DP-APPX-IDVAT-RESP                        
069300                                DP-INSUR-IDVAT-RESP                       
069400                                DP-ADD-IDVAT-RESP                         
069500                                DP-CREDI-IDVAT-RESP                       
069600                                DP-HEAD-IDVAT-RESP                        
069700                                DP-DADR-IDVAT-RESP                        
069800     END-IF                                                               
069900**** DIRTY FIX FOR INT2 SOLUTION                                          
070000     IF  HEAD-KDFINDOC = 'INT2'                                           
070100       MOVE 'SE556074308901' TO DP-HEAD-IDVAT-RESP                        
070200                                DP-APPX-IDVAT-RESP                        
070300                                DP-INSUR-IDVAT-RESP                       
070400                                DP-ADD-IDVAT-RESP                         
070500                                DP-CREDI-IDVAT-RESP                       
070600                                DP-HEAD-IDVAT-RESP                        
070700                                DP-DADR-IDVAT-RESP                        
070800     END-IF                                                               
070900     MOVE HEAD-KDVALISO              TO DP-HEAD-KDVALISO                  
071000                                        DP-APPX-KDVALISO                  
071100                                        DP-INSUR-KDVALISO                 
071200                                        DP-ADD-KDVALISO                   
071300                                        DP-CREDI-KDVALISO                 
071400                                        DP-DADR-KDVALISO                  
071500     IF HEAD-KDVALISO-LOC NOT = HEAD-KDVALISO                             
071600       IF   HEAD-REVALUTA > 1                                             
071700         COMPUTE HEAD-PRKURS-LOC ROUNDED =                                
071800                 HEAD-PRKURS-LOC / HEAD-REVALUTA                          
071900       END-IF                                                             
072000       MOVE HEAD-PRKURS-LOC          TO DP-HEAD-PRKURS                    
072100                                        DP-APPX-PRKURS                    
072200                                        DP-INSUR-PRKURS                   
072300                                        DP-ADD-PRKURS                     
072400                                        DP-CREDI-PRKURS                   
072500                                        DP-DADR-PRKURS                    
072600                                        WS-HEAD-PRKURS                    
072700     ELSE                                                                 
072800       MOVE 1                        TO DP-HEAD-PRKURS                    
072900                                        DP-APPX-PRKURS                    
073000                                        DP-INSUR-PRKURS                   
073100                                        DP-ADD-PRKURS                     
073200                                        DP-CREDI-PRKURS                   
073300                                        DP-DADR-PRKURS                    
073400                                        WS-HEAD-PRKURS                    
073500     END-IF                                                               
073600     MOVE HEAD-IDPARTNR              TO DP-HEAD-IDPARTNR                  
073700                                        DP-APPX-IDPARTNR                  
073800                                        DP-INSUR-IDPARTNR                 
073900                                        DP-ADD-IDPARTNR                   
074000                                        DP-CREDI-IDPARTNR                 
074100                                        DP-DADR-IDPARTNR                  
074200     MOVE HEAD-BEBETVIL              TO DP-HEAD-BEBETVIL                  
074300                                        DP-APPX-BEBETVIL                  
074400                                        DP-INSUR-BEBETVIL                 
074500                                        DP-ADD-BEBETVIL                   
074600                                        DP-CREDI-BEBETVIL                 
074700                                        DP-DADR-BEBETVIL                  
074800     MOVE HEAD-BEBET-NAME1           TO DP-HEAD-BEBET-NAME1               
074900                                        DP-APPX-BEBET-NAME1               
075000                                        DP-INSUR-BEBET-NAME1              
075100                                        DP-ADD-BEBET-NAME1                
075200                                        DP-CREDI-BEBET-NAME1              
075300                                        DP-DADR-BEBET-NAME1               
075400                                        WS-BEBET-NAME1                    
075500     MOVE HEAD-BEBET-NAME2           TO DP-HEAD-BEBET-NAME2               
075600                                        DP-APPX-BEBET-NAME2               
075700                                        DP-INSUR-BEBET-NAME2              
075800                                        DP-ADD-BEBET-NAME2                
075900                                        DP-CREDI-BEBET-NAME2              
076000                                        DP-DADR-BEBET-NAME2               
076100     MOVE HEAD-ADBET-BOX             TO DP-HEAD-ADBET-BOX                 
076200                                        DP-APPX-ADBET-BOX                 
076300                                        DP-INSUR-ADBET-BOX                
076400                                        DP-ADD-ADBET-BOX                  
076500                                        DP-CREDI-ADBET-BOX                
076600                                        DP-DADR-ADBET-BOX                 
076700     MOVE HEAD-ADBET-STREET          TO DP-HEAD-ADBET-STREET              
076800                                        DP-APPX-ADBET-STREET              
076900                                        DP-INSUR-ADBET-STREET             
077000                                        DP-ADD-ADBET-STREET               
077100                                        DP-CREDI-ADBET-STREET             
077200                                        DP-DADR-ADBET-STREET              
077300     MOVE HEAD-ADBET-PCODE           TO DP-HEAD-ADBET-PCODE               
077400                                        DP-APPX-ADBET-PCODE               
077500                                        DP-INSUR-ADBET-PCODE              
077600                                        DP-ADD-ADBET-PCODE                
077700                                        DP-CREDI-ADBET-PCODE              
077800                                        DP-DADR-ADBET-PCODE               
077900     MOVE HEAD-ADBET-CITY            TO DP-HEAD-ADBET-CITY                
078000                                        DP-APPX-ADBET-CITY                
078100                                        DP-INSUR-ADBET-CITY               
078200                                        DP-ADD-ADBET-CITY                 
078300                                        DP-CREDI-ADBET-CITY               
078400                                        DP-DADR-ADBET-CITY                
078500     MOVE HEAD-BELAND-BET            TO DP-HEAD-BELAND-BET                
078600                                        DP-APPX-BELAND-BET                
078700                                        DP-INSUR-BELAND-BET               
078800                                        DP-ADD-BELAND-BET                 
078900                                        DP-CREDI-BELAND-BET               
079000                                        DP-DADR-BELAND-BET                
079100     MOVE HEAD-IDVAT-BET             TO DP-HEAD-IDVAT-BET                 
079200                                        DP-APPX-IDVAT-BET                 
079300                                        DP-INSUR-IDVAT-BET                
079400                                        DP-ADD-IDVAT-BET                  
079500                                        DP-CREDI-IDVAT-BET                
079600                                        DP-DADR-IDVAT-BET                 
079700     MOVE HEAD-SUNTO-PART            TO DP-HEAD-SUNTO-PART                
079800                                        DP-APPX-SUNTO-PART                
079900                                        DP-INSUR-SUNTO-PART               
080000                                        DP-ADD-SUNTO-PART                 
080100                                        DP-CREDI-SUNTO-PART               
080200                                        DP-DADR-SUNTO-PART                
080300     MOVE HEAD-SUBTO-PART            TO DP-HEAD-SUBTO-PART                
080400                                        DP-APPX-SUBTO-PART                
080500                                        DP-INSUR-SUBTO-PART               
080600                                        DP-ADD-SUBTO-PART                 
080700                                        DP-CREDI-SUBTO-PART               
080800                                        DP-DADR-SUBTO-PART                
080900     IF HEAD-SUBTO-PART < ZERO                                            
081000       MOVE '-'                      TO DP-HEAD-REF-MINUS                 
081100                                        DP-APPX-REF-MINUS                 
081200     ELSE                                                                 
081300       MOVE SPACE                    TO DP-HEAD-REF-MINUS                 
081400                                        DP-APPX-REF-MINUS                 
081500     END-IF                                                               
081600     MOVE SPACE                      TO DP-HEAD-IDVAT-LEG-REG-NO          
081700                                        DP-APPX-IDVAT-LEG-REG-NO          
081800                                        DP-INSUR-IDVAT-LEG-REG-NO         
081900                                        DP-ADD-IDVAT-LEG-REG-NO           
082000                                        DP-CREDI-IDVAT-LEG-REG-NO         
082100                                       DP-DADR-IDVAT-LEG-REG-NO           
082200     MOVE HEAD-IDVAT-LEG             TO DP-HEAD-IDVAT-LEG-REG-NO          
082300     MOVE HEAD-BEANST                TO DP-HEAD-BEANST                    
082400                                        DP-APPX-BEANST                    
082500                                        DP-INSUR-BEANST                   
082600                                        DP-ADD-BEANST                     
082700                                        DP-CREDI-BEANST                   
082800                                        DP-DADR-BEANST                    
082900                                        WS-BEANST                         
083000     MOVE HEAD-IDUSER                TO DP-HEAD-IDUSER                    
083100                                        DP-APPX-IDUSER                    
083200                                        DP-INSUR-IDUSER                   
083300                                        DP-ADD-IDUSER                     
083400                                        DP-CREDI-IDUSER                   
083500                                        DP-DADR-IDUSER                    
083600     MOVE HEAD-BETEXT                TO DP-HEAD-BETEXT                    
083700     MOVE HEAD-BETEXT                TO WS-BETEXT                         
083800     MOVE HEAD-BETEXT-1              TO DP-HEAD-BETEXT-1                  
083900     MOVE HEAD-BETEXT-2              TO DP-HEAD-BETEXT-2                  
084000     MOVE HEAD-BETEXT-3              TO DP-HEAD-BETEXT-3                  
084100     MOVE HEAD-BETEXT-4              TO DP-HEAD-BETEXT-4                  
084200     MOVE HEAD-BETEXT-5              TO DP-HEAD-BETEXT-5                  
084300     MOVE HEAD-BETEXT-6              TO DP-HEAD-BETEXT-6                  
084400     MOVE HEAD-BETEXT-7              TO DP-HEAD-BETEXT-7                  
084500     MOVE HEAD-BETEXT-8              TO DP-HEAD-BETEXT-8                  
084600* DENNA KOD ÄR FÖR ATT SKRIVA EN FÖRSÄKRANSTEXT                           
084700     IF HEAD-BETEXT-9 > ' '                                               
084800       MOVE YES TO NEW-INSURANCE-SW                                       
084900       MOVE HEAD-BETEXT-9            TO SAVE-DP-INSUR-BETEXT-9            
085000       MOVE HEAD-BETEXT-10           TO SAVE-DP-INSUR-BETEXT-10           
085100       MOVE HEAD-BETEXT-11           TO SAVE-DP-INSUR-BETEXT-11           
085200       MOVE HEAD-BETEXT-12           TO SAVE-DP-INSUR-BETEXT-12           
085300       MOVE HEAD-BETEXT-13           TO SAVE-DP-INSUR-BETEXT-13           
085400       MOVE HEAD-BETEXT-14           TO SAVE-DP-INSUR-BETEXT-14           
085500       MOVE HEAD-BETEXT-15           TO SAVE-DP-INSUR-BETEXT-15           
085600       MOVE HEAD-BETEXT-16           TO SAVE-DP-INSUR-BETEXT-16           
085700       MOVE HEAD-BETEXT-17           TO SAVE-DP-INSUR-BETEXT-17           
085800       MOVE HEAD-BETEXT-18           TO SAVE-DP-INSUR-BETEXT-18           
085900       MOVE HEAD-BETEXT-19           TO SAVE-DP-INSUR-BETEXT-19           
086000       MOVE HEAD-BETEXT-20           TO SAVE-DP-INSUR-BETEXT-20           
086100       MOVE HEAD-BETEXT-21           TO SAVE-DP-INSUR-BETEXT-21           
086200       MOVE HEAD-BETEXT-22           TO SAVE-DP-INSUR-BETEXT-22           
086300       MOVE HEAD-BETEXT-23           TO SAVE-DP-INSUR-BETEXT-23           
086400       MOVE HEAD-BETEXT-24           TO SAVE-DP-INSUR-BETEXT-24           
086500       MOVE HEAD-BETEXT-25           TO SAVE-DP-ADD-BETEXT-25             
086600       MOVE HEAD-BETEXT-26           TO SAVE-DP-ADD-BETEXT-26             
086700       MOVE HEAD-BETEXT-27           TO SAVE-DP-ADD-BETEXT-27             
086800       MOVE HEAD-BETEXT-28           TO SAVE-DP-ADD-BETEXT-28             
086900       MOVE HEAD-BETEXT-29           TO SAVE-DP-ADD-BETEXT-29             
087000       MOVE HEAD-BETEXT-30           TO SAVE-DP-ADD-BETEXT-30             
087100       MOVE HEAD-BETEXT-31           TO SAVE-DP-ADD-BETEXT-31             
087200       MOVE HEAD-BETEXT-32           TO SAVE-DP-ADD-BETEXT-32             
087300     ELSE                                                                 
087400       MOVE NOO TO NEW-INSURANCE-SW                                       
087500     END-IF                                                               
087600                                                                          
087700     IF HEAD-BETEXT-33 > ' '                                              
087800       MOVE YES TO NEW-ADD-INSURANCE-SW                                   
087900       MOVE HEAD-BETEXT-33           TO SAVE-DP-ADD-BETEXT-33             
088000       MOVE HEAD-BETEXT-34           TO SAVE-DP-ADD-BETEXT-34             
088100       MOVE HEAD-BETEXT-35           TO SAVE-DP-ADD-BETEXT-35             
088200       MOVE HEAD-BETEXT-36           TO SAVE-DP-ADD-BETEXT-36             
088300       MOVE HEAD-BETEXT-37           TO SAVE-DP-ADD-BETEXT-37             
088400       MOVE HEAD-BETEXT-38           TO SAVE-DP-ADD-BETEXT-38             
088500       MOVE HEAD-BETEXT-39           TO SAVE-DP-ADD-BETEXT-39             
088600       MOVE HEAD-BETEXT-40           TO SAVE-DP-ADD-BETEXT-40             
088700       MOVE HEAD-BETEXT-41           TO SAVE-DP-ADD-BETEXT-41             
088800       MOVE HEAD-BETEXT-42           TO SAVE-DP-ADD-BETEXT-42             
088900       MOVE HEAD-BETEXT-43           TO SAVE-DP-ADD-BETEXT-43             
089000       MOVE HEAD-BETEXT-44           TO SAVE-DP-ADD-BETEXT-44             
089100     ELSE                                                                 
089200       MOVE NOO TO NEW-ADD-INSURANCE-SW                                   
089300     END-IF                                                               
089400                                                                          
089500     IF HEAD-BETEXT   > ' '                                               
089600       IF HEAD-KDFINDOC = 'CR'                                            
089700         MOVE YES TO NEW-CREDIT-SW                                        
089800         MOVE HEAD-BETEXT              TO SAVE-DP-CREDI-BETEXT            
089900         MOVE HEAD-BETEXT-CRE          TO SAVE-DP-CREDI-BETEXT-CRE        
090000       ELSE                                                               
090100         MOVE NOO TO NEW-CREDIT-SW                                        
090200       END-IF                                                             
090300       IF HEAD-KDFINDOC = 'INV'                                           
090400         MOVE YES TO NEW-DADR-SW                                          
090500         MOVE HEAD-BETEXT            TO SAVE-DP-DADR-BETEXT               
090600       ELSE                                                               
090700         MOVE NOO TO NEW-DADR-SW                                          
090800       END-IF                                                             
090900     ELSE                                                                 
091000       MOVE NOO TO NEW-CREDIT-SW                                          
091100       MOVE NOO TO NEW-DADR-SW                                            
091200     END-IF                                                               
091300     .                                                                    
091400                                                                          
091500 BB-HANDLE-LINES SECTION.                                                 
091600     MOVE IN-DATA               TO IN-AREA-LINE                           
091700                                                                          
091800* DENNA KOD ÄR FÖR ATT SKRIVA DET GAMLA FAKTURANUMMRET I HUVUDET          
091900* FÖR REFILL VID ÖVER/UNDERLEVERANS                                       
092000     IF HEAD-BEFORMS = 'INTERNAL-REFILL'                                  
092100       MOVE LINE-IDREF      TO WS-IDREF                                   
092200       MOVE LINE-IDFINDOC   TO WS-IDFINDOC                                
092300       IF WS-IDREF(1:1) =  SPACE                                          
092400         MOVE ZERO          TO LINE-IDFINDOC                              
092500       ELSE                                                               
092600       IF WS-IDREF(2:1) =  SPACE                                          
092700         MOVE WS-IDREF(1:1) TO LINE-IDFINDOC                              
092800       ELSE                                                               
092900       IF WS-IDREF(3:1) =  SPACE                                          
093000         MOVE WS-IDREF(1:2) TO LINE-IDFINDOC                              
093100       ELSE                                                               
093200       IF WS-IDREF(4:1) =  SPACE                                          
093300         MOVE WS-IDREF(1:3) TO LINE-IDFINDOC                              
093400       ELSE                                                               
093500       IF WS-IDREF(5:1) =  SPACE                                          
093600         MOVE WS-IDREF(1:4) TO LINE-IDFINDOC                              
093700       ELSE                                                               
093800       IF WS-IDREF(6:1) =  SPACE                                          
093900         MOVE WS-IDREF(1:5) TO LINE-IDFINDOC                              
094000       ELSE                                                               
094100       IF WS-IDREF(7:1) =  SPACE                                          
094200         MOVE WS-IDREF(1:6) TO LINE-IDFINDOC                              
094300       ELSE                                                               
094400       IF WS-IDREF(8:1) =  SPACE                                          
094500         MOVE WS-IDREF(1:7) TO LINE-IDFINDOC                              
094600       ELSE                                                               
094700       IF WS-IDREF(9:1) =  SPACE                                          
094800         MOVE WS-IDREF(1:8) TO LINE-IDFINDOC                              
094900       ELSE                                                               
095000       IF WS-IDREF(10:1) = SPACE                                          
095100         MOVE WS-IDREF(1:9) TO LINE-IDFINDOC                              
095200       END-IF                                                             
095300       END-IF                                                             
095400       END-IF                                                             
095500       END-IF                                                             
095600       END-IF                                                             
095700       END-IF                                                             
095800       END-IF                                                             
095900       END-IF                                                             
096000       END-IF                                                             
096100       END-IF                                                             
096200       MOVE WS-IDFINDOC   TO LINE-IDBUNDLE                                
096300       MOVE 'REFILL'      TO LINE-IDREF                                   
096400     END-IF                                                               
096500                                                                          
096600*    -- IF NEW GROUP,                                                     
096700*          MARK IDAFPRCD WITH '2.1'                                       
096800     IF LINE-IDEXCUST-2   = WS-IDEXCUST-2-OLD                             
096900     AND LINE-IDFINDOC    = WS-IDFINDOC-OLD                               
097000     AND LINE-IDREF       = WS-IDREF-OLD                                  
097100     AND LINE-DAREFDAT    = WS-DAREFDAT-OLD                               
097200       IF HEAD-FLSOFT     = 'N'                                           
097300         IF HEAD-KDPARTGR      = 'NSC-LYNK'                               
097400           MOVE WS-DOC-LINE-LYNK  TO DP-LINE-IDAFPRCD                     
097500         ELSE                                                             
097600           IF LINE-IDTRACK-1 > SPACE                                      
097700             MOVE WS-DOC-LINE-TRAC TO DP-LINE-IDAFPRCD                    
097800           ELSE                                                           
097900             MOVE LINE-IDPTYP      TO DP-LINE-IDAFPRCD                    
098000           END-IF                                                         
098100         END-IF                                                           
098200       ELSE                                                               
098300         MOVE WS-DOC-LINE-SOFT    TO DP-LINE-IDAFPRCD                     
098400       END-IF                                                             
098500     ELSE                                                                 
098600       IF HEAD-FLSOFT     = 'N'                                           
098700         IF HEAD-KDPARTGR = 'NSC-LYNK'                                    
098800           MOVE WS-DOC-GROUP-LYNK TO DP-LINE-IDAFPRCD                     
098900         ELSE                                                             
099000           IF LINE-IDTRACK-1 > SPACE                                      
099100             MOVE WS-DOC-GROUP-TRAC TO DP-LINE-IDAFPRCD                   
099200           ELSE                                                           
099300             MOVE WS-DOC-GROUP      TO DP-LINE-IDAFPRCD                   
099400           END-IF                                                         
099500         END-IF                                                           
099600       ELSE                                                               
099700         MOVE WS-DOC-GROUP-SOFT   TO DP-LINE-IDAFPRCD                     
099800       END-IF                                                             
099900       MOVE LINE-IDEXCUST-2     TO WS-IDEXCUST-2-OLD                      
100000       MOVE LINE-IDREF          TO WS-IDREF-OLD                           
100100       MOVE LINE-DAREFDAT       TO WS-DAREFDAT-OLD                        
100200       MOVE LINE-IDFINDOC       TO WS-IDFINDOC-OLD                        
100300     END-IF                                                               
100400     MOVE LINE-IDEXCUST-1       TO DP-LINE-IDEXCUST-1                     
100500                                   DP-FIPH-IDEXCUST-1                     
100600     MOVE LINE-IDEXCUST-2       TO DP-LINE-IDEXCUST-2                     
100700                                   DP-FIPH-IDEXCUST-2                     
100800     MOVE LINE-IDEXCUST-3       TO DP-LINE-IDEXCUST-3                     
100900     MOVE LINE-IDBUNDLE         TO DP-LINE-IDBUNDLE                       
101000                                   DP-FIPH-IDBUNDLE                       
101100     MOVE LINE-IDOPTION-1       TO DP-LINE-IDOPTION-1                     
101200                                   DP-FIPH-IDOPTION-1                     
101300     MOVE LINE-IDOPTION-2       TO DP-LINE-IDOPTION-2                     
101400                                   DP-FIPH-IDOPTION-2                     
101500     MOVE LINE-IDOPTION-3       TO DP-LINE-IDOPTION-3                     
101600                                   DP-FIPH-IDOPTION-3                     
101700     MOVE LINE-IDOPTION-5       TO DP-LINE-IDOPTION-5                     
101800                                   DP-FIPH-IDOPTION-5                     
101900     IF  LINE-IDLEGSEL = 'VCCS'                                           
102000     AND LINE-FLSOFT   = 'J'                                              
102100        MOVE LINE-IDREF(1:5)    TO WS-IDREF-NUM                           
102200        IF WS-IDREF-NUM > 10000 AND                                       
102300           WS-IDREF-NUM < 20000                                           
102400           MOVE 'CONS.INV.'     TO DP-LINE-IDOPTION-5                     
102500        END-IF                                                            
102600     END-IF                                                               
102700     MOVE LINE-IDREF            TO DP-LINE-IDREF                          
102800                                   DP-FIPH-IDREF                          
102900     MOVE LINE-BEVOLREF         TO DP-LINE-BEVOLREF                       
103000                                   DP-FIPH-BEVOLREF                       
103100     MOVE LINE-IDARTNR-FINANCE  TO DP-LINE-IDARTNR-FINANCE                
103200                                   DP-FIPH-IDARTNR-FINANCE                
103300* DENNA KOD ÄR FÖR ATT SKRIVA KONTROLLSIFFRA TILL ARTIKELN                
103400     IF LINE-KDFINDOC = 'INV'                                             
103500       IF LINE-IDARTNR-CNTRL = SPACE                                      
103600         CONTINUE                                                         
103700       ELSE                                                               
103800         IF LINE-IDARTNR-FINANCE(1:1) = SPACE                             
103900* DENNA KOD ÄR FÖR ATT SKRIVA SUMMA RAD FÖR FREIGHT, INSURANCE            
104000* , PACKING AND HANDLING FAKTURAVIS                                       
104100* GÄLLER FÖR TILLFÄLLET ENDAST OM KONTROLLSIFFRA SKA SKRIVAS              
104200           MOVE YES TO NEW-FIPH-SUM-ROW-SW                                
104300****                                                                      
104400         ELSE                                                             
104500         IF LINE-IDARTNR-FINANCE(2:1) = SPACE                             
104600           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(2:2)        
104700         ELSE                                                             
104800         IF LINE-IDARTNR-FINANCE(3:1) = SPACE                             
104900           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(3:2)        
105000         ELSE                                                             
105100         IF LINE-IDARTNR-FINANCE(4:1) = SPACE                             
105200           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(4:2)        
105300         ELSE                                                             
105400         IF LINE-IDARTNR-FINANCE(5:1) = SPACE                             
105500           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(5:2)        
105600         ELSE                                                             
105700         IF LINE-IDARTNR-FINANCE(6:1) = SPACE                             
105800           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(6:2)        
105900         ELSE                                                             
106000         IF LINE-IDARTNR-FINANCE(7:1) = SPACE                             
106100           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(7:2)        
106200         ELSE                                                             
106300         IF LINE-IDARTNR-FINANCE(8:1) = SPACE                             
106400           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(8:2)        
106500         ELSE                                                             
106600         IF LINE-IDARTNR-FINANCE(9:1) = SPACE                             
106700           MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(9:2)        
106800         ELSE                                                             
106900         IF LINE-IDARTNR-FINANCE(10:1) = SPACE                            
107000          MOVE LINE-IDARTNR-CNTRL TO DP-LINE-IDARTNR-FINANCE(10:2)        
107100         ELSE                                                             
107200           CONTINUE                                                       
107300         END-IF                                                           
107400         END-IF                                                           
107500         END-IF                                                           
107600         END-IF                                                           
107700         END-IF                                                           
107800         END-IF                                                           
107900         END-IF                                                           
108000         END-IF                                                           
108100         END-IF                                                           
108200         END-IF                                                           
108300       END-IF                                                             
108400     END-IF                                                               
108500     MOVE LINE-BEART            TO DP-LINE-BEART                          
108600     MOVE LINE-KVBEART          TO DP-LINE-KVBEART                        
108700     MOVE LINE-KVLEVART         TO DP-LINE-KVLEVART                       
108800     MOVE LINE-PRARTBTO         TO DP-LINE-PRARTBTO                       
108900                                   DP-FIPH-PRARTBTO                       
109000     MOVE LINE-IDAPPEND(2:2)    TO WS-LINE-IDAPPEND                       
109100     IF HEAD-FLSOFT     = 'N'                                             
109200       IF KDPRODSL-LYNK                                                   
109300         MOVE LINE-IDARTNR-FINANCE(11:12) TO                              
109400                                       DP-LINE-BEART(1:12)                
109500         MOVE LINE-BEART       TO      DP-LINE-BEART(13:13)               
109600       END-IF                                                             
109700     END-IF                                                               
109800     IF LINE-PRARTBTO   < ZERO                                            
109900       MOVE '-'                      TO DP-LINE-REF-MINUS                 
110000     ELSE                                                                 
110100       MOVE SPACE                    TO DP-LINE-REF-MINUS                 
110200     END-IF                                                               
110300     MOVE LINE-PRARTNTO         TO DP-LINE-PRARTNTO                       
110400                                   DP-FIPH-PRARTNTO                       
110500     MOVE LINE-REARTRAB         TO DP-LINE-REARTRAB                       
110600                                   DP-FIPH-REARTRAB                       
110700     IF LINE-FLSPECPR = 'Y' OR 'J'                                        
110800       MOVE '*'                 TO DP-LINE-FLSPECPR                       
110900                                   DP-FIPH-FLSPECPR                       
111000     ELSE                                                                 
111100       MOVE SPACE               TO DP-LINE-FLSPECPR                       
111200                                   DP-FIPH-FLSPECPR                       
111300     END-IF                                                               
111400     IF LINE-FLPCOO   = 'Y' OR 'J'                                        
111500       MOVE '*'                 TO DP-LINE-FLPCOO                         
111600     ELSE                                                                 
111700       MOVE SPACE               TO DP-LINE-FLPCOO                         
111800     END-IF                                                               
111900**** IF FLCURRND IS MARKED THEN WE SHALL USE                              
112000**** THE ROUNDED VALUE TO CALCULATE THE NET VALUE                         
112100**** ON THE INVOICE                                                       
112200     IF LINE-FLCURRND = 'Y' OR 'J'                                        
112300       COMPUTE WS-SUNTO-FLCURRND ROUNDED =                                
112400               LINE-PRARTNTO * LINE-KVLEVART                              
112500       MOVE WS-SUNTO-FLCURRND   TO DP-LINE-SUNTO                          
112600                                   DP-FIPH-SUNTO                          
112700                                   LINE-SUNTO                             
112800     ELSE                                                                 
112900       MOVE LINE-SUNTO          TO DP-LINE-SUNTO                          
113000                                   DP-FIPH-SUNTO                          
113100     END-IF                                                               
113200     MOVE LINE-REVAT            TO DP-LINE-REVAT                          
113300                                   DP-FIPH-REVAT                          
113400     MOVE LINE-SUVAT-BILLIT     TO DP-LINE-SUVAT-BILLIT                   
113500                                   DP-FIPH-SUVAT-BILLIT                   
113600     MOVE LINE-SUBTO            TO DP-LINE-SUBTO                          
113700                                   DP-FIPH-SUBTO                          
113800     MOVE LINE-VKARTNTO         TO DP-LINE-VKARTNTO                       
113900                                   DP-FIPH-VKARTNTO                       
114000     MOVE LINE-VKORDBTO-KOLLI   TO DP-LINE-VKORDBTO-KOLLI                 
114100                                   DP-FIPH-VKORDBTO-KOLLI                 
114200     MOVE LINE-KDARTURS         TO DP-LINE-KDARTURS                       
114300                                   DP-FIPH-KDARTURS                       
114400     IF HEAD-IDPARTNR = '395566'                                          
114500       MOVE ZERO                TO DP-LINE-IDSTATNR                       
114600                                   DP-FIPH-IDSTATNR                       
114700     ELSE                                                                 
114800       MOVE LINE-IDSTATNR       TO DP-LINE-IDSTATNR                       
114900                                   DP-FIPH-IDSTATNR                       
115000     END-IF                                                               
115100     MOVE LINE-KDFRAKT          TO DP-LINE-KDFRAKT                        
115200                                   DP-FIPH-KDFRAKT                        
115300     MOVE LINE-IDACCNT-1        TO DP-LINE-IDACCNT-1                      
115400                                   DP-FIPH-IDACCNT-1                      
115500     MOVE LINE-IDACCNT-2        TO DP-LINE-IDACCNT-2                      
115600                                   DP-FIPH-IDACCNT-2                      
115700     MOVE LINE-IDACCNT-3        TO DP-LINE-IDACCNT-3                      
115800                                   DP-FIPH-IDACCNT-3                      
115900     MOVE LINE-KDANMORS         TO DP-LINE-KDANMORS                       
116000                                   DP-FIPH-KDANMORS                       
116100     MOVE LINE-IDFAKREF         TO DP-LINE-IDFAKREF                       
116200                                   DP-FIPH-IDFAKREF                       
116300     MOVE LINE-DAFAKREF         TO DP-LINE-DAFAKREF                       
116400                                   DP-FIPH-DAFAKREF                       
116500     MOVE LINE-DAREFDAT         TO DP-LINE-DAREFDAT                       
116600                                   DP-FIPH-DAREFDAT                       
116700     MOVE LINE-IDDC             TO DP-LINE-IDDC                           
116800                                   DP-FIPH-IDDC                           
116900     MOVE LINE-BELEVVIL         TO DP-LINE-BELEVVIL                       
117000                                   DP-FIPH-BELEVVIL                       
117100     MOVE LINE-IDLEVNR-ART      TO DP-LINE-IDLEVNR-ART                    
117200     MOVE LINE-IDTRACK-1        TO DP-LINE-IDTRACK-1                      
117300     MOVE LINE-KVANT-TRACK-1    TO DP-LINE-KVANT-TRACK-1                  
117400     MOVE LINE-IDTRACK-2        TO DP-LINE-IDTRACK-2                      
117500     MOVE LINE-KVANT-TRACK-2    TO DP-LINE-KVANT-TRACK-2                  
117600     MOVE LINE-IDTRACK-3        TO DP-LINE-IDTRACK-3                      
117700     MOVE LINE-KVANT-TRACK-3    TO DP-LINE-KVANT-TRACK-3                  
117800     MOVE LINE-IDTRACK-4        TO DP-LINE-IDTRACK-4                      
117900     MOVE LINE-KVANT-TRACK-4    TO DP-LINE-KVANT-TRACK-4                  
118000     MOVE LINE-IDTRACK-5        TO DP-LINE-IDTRACK-5                      
118100     MOVE LINE-KVANT-TRACK-5    TO DP-LINE-KVANT-TRACK-5                  
118200     MOVE HEAD-BEANST           TO DP-LINE-BEANST                         
118300                                   DP-FIPH-BEANST                         
118400     MOVE HEAD-IDUSER           TO DP-LINE-IDUSER                         
118500                                   DP-FIPH-IDUSER                         
118600                                                                          
118700*    --- GET DOC-HEAD-DATA FROM BILLIT-LINE-DATA                          
118800     IF NEW-HEAD                                                          
118900       MOVE LINE-IDEXCUST-1     TO DP-HEAD-IDEXCUST-1                     
119000                                   DP-APPX-IDEXCUST-1                     
119100                                   DP-INSUR-IDEXCUST-1                    
119200                                   DP-ADD-IDEXCUST-1                      
119300                                   DP-CREDI-IDEXCUST-1                    
119400                                   DP-DADR-IDEXCUST-1                     
119500                                   WS-IDEXCUST-1                          
119600       MOVE LINE-IDEXCUST-2     TO DP-HEAD-IDEXCUST-2                     
119700                                   DP-APPX-IDEXCUST-2                     
119800                                   DP-INSUR-IDEXCUST-2                    
119900                                   DP-ADD-IDEXCUST-2                      
120000                                   DP-CREDI-IDEXCUST-2                    
120100                                   DP-DADR-IDEXCUST-2                     
120200       MOVE LINE-BELEVVIL       TO DP-HEAD-BELEVVIL                       
120300                                   DP-APPX-BELEVVIL                       
120400                                   DP-INSUR-BELEVVIL                      
120500                                   DP-ADD-BELEVVIL                        
120600                                   DP-CREDI-BELEVVIL                      
120700                                   DP-DADR-BELEVVIL                       
120800       MOVE LINE-IDDC           TO DP-HEAD-IDDC                           
120900                                   DP-APPX-IDDC                           
121000                                   DP-INSUR-IDDC                          
121100                                   DP-ADD-IDDC                            
121200                                   DP-CREDI-IDDC                          
121300                                   DP-DADR-IDDC                           
121400                                   WS-IDDC                                
121500        PERFORM S91-SKRIV-DAP5-S                                          
121600* FÖR DESSA KODER SKA MOMSTEXTER ALLTID                                   
121700* LÄGGAS UT PÅ DOKUMENTET                                                 
121800       IF LINE-KDVAT = 'BD' OR                                            
121900          LINE-KDVAT = 'NZ'                                               
122000         MOVE LINE-BEVAT        TO DP-HEAD-BEVAT                          
122100                                   DP-APPX-BEVAT                          
122200                                   DP-INSUR-BEVAT                         
122300                                   DP-ADD-BEVAT                           
122400                                   DP-CREDI-BEVAT                         
122500                                   DP-DADR-BEVAT                          
122600       ELSE                                                               
122700         MOVE SPACE             TO DP-HEAD-BEVAT                          
122800                                   DP-APPX-BEVAT                          
122900                                   DP-INSUR-BEVAT                         
123000                                   DP-ADD-BEVAT                           
123100                                   DP-CREDI-BEVAT                         
123200                                   DP-DADR-BEVAT                          
123300       END-IF                                                             
123400                                                                          
123500       IF HEAD-BEFORMS = 'INTERNAL-REFILL'                                
123600         MOVE LINE-IDFINDOC     TO HEAD-IDFINDOC                          
123700         MOVE HEAD-IDFINDOC     TO DP-HEAD-IDFINDOC                       
123800                                   DP-APPX-IDFINDOC                       
123900                                   DP-INSUR-IDFINDOC                      
124000                                   DP-ADD-IDFINDOC                        
124100                                   DP-CREDI-IDFINDOC                      
124200                                   DP-DADR-IDFINDOC                       
124300       END-IF                                                             
124400                                                                          
124500       PERFORM S90-WRITE-DOC-HEAD                                         
124600       MOVE NOO                 TO NEW-HEAD-SW                            
124700     END-IF                                                               
124800                                                                          
124900* DENNA KOD ÄR FÖR ATT SKRIVA VALUTAJUSTERINGEN FÖR FAKTUROR SOM          
125000* ÄR OMRÄKNADE TILL LOKAL VALUTA                                          
125100     MOVE LINE-SUNTO           TO WS2-LOCC-JUS-SUNTO                      
125200     COMPUTE WS-LOCC-JUS-SUNTO = WS-LOCC-JUS-SUNTO +                      
125300                                   WS2-LOCC-JUS-SUNTO                     
125400                                                                          
125500* DENNA KOD ÄR FÖR ATT SKRIVA SUMMA RAD FÖR FREIGHT, INSURANCE            
125600* , PACKING AND HANDLING FAKTURAVIS                                       
125700     IF LINE-IDARTNR-FINANCE > SPACE                                      
125800       PERFORM S90-WRITE-DOC-LINE                                         
125900     ELSE                                                                 
126000       IF LINE-KDFINDOC = 'INV'                                           
126100         IF LINE-IDARTNR-CNTRL = SPACE                                    
126200           PERFORM S90-WRITE-DOC-LINE                                     
126300         ELSE                                                             
126400           MOVE +1 TO WS-IX                                               
126500           PERFORM UNTIL WS-IX > 35                                       
126600             IF LINE-BEART = WS-BEART(WS-IX)                              
126700               COMPUTE WS-KVLEVART(WS-IX)     = LINE-KVLEVART +           
126800                                                WS-KVLEVART(WS-IX)        
126900               COMPUTE WS-SUNTO(WS-IX)        = LINE-SUNTO +              
127000                                                WS-SUNTO(WS-IX)           
127100               ADD +35 TO WS-IX                                           
127200             ELSE                                                         
127300               IF WS-BEART(WS-IX) > SPACE                                 
127400                 ADD +1 TO WS-IX                                          
127500               ELSE                                                       
127600                 MOVE LINE-BEART        TO WS-BEART(WS-IX)                
127700                 MOVE LINE-KVLEVART     TO WS-KVLEVART(WS-IX)             
127800                 MOVE LINE-SUNTO        TO WS-SUNTO(WS-IX)                
127900                 ADD +35 TO WS-IX                                         
128000               END-IF                                                     
128100             END-IF                                                       
128200           END-PERFORM                                                    
128300         END-IF                                                           
128400       ELSE                                                               
128500         PERFORM S90-WRITE-DOC-LINE                                       
128600       END-IF                                                             
128700     END-IF                                                               
128800     IF LINE-KDFINDOC = 'PROF'                                            
128900       IF LINE-IDARTNR-FINANCE(1:1) = SPACE                               
129000         CONTINUE                                                         
129100       ELSE                                                               
129200         MOVE YES TO YES-PROF-SW                                          
129300         COMPUTE PROF-SUNTO = LINE-SUNTO + PROF-SUNTO                     
129400       END-IF                                                             
129500     END-IF                                                               
129600     .                                                                    
129700                                                                          
129800 BC-HANDLE-FOOTER SECTION.                                                
129900     MOVE IN-DATA               TO IN-AREA-FOOT                           
130000                                                                          
130100     IF YES-PROF                                                          
130200       PERFORM BM-HANDLE-PROF                                             
130300       MOVE NOO  TO YES-PROF-SW                                           
130400       MOVE ZERO TO PROF-SUNTO                                            
130500     END-IF                                                               
130600                                                                          
130700     MOVE FOOT-IDPTYP           TO DP-FOOT-IDAFPRCD                       
130800     MOVE FOOT-SUNTO-TOT        TO WS-SUNTO-SND-LOC                       
130900     MOVE FOOT-SUNTO-TOT        TO WS-LOCC-JUS-CHECK                      
131000     IF FOOT-KDVALISO-LOC = FOOT-KDVALISO                                 
131100     OR FOOT-KDVALISO-LOC = SPACES                                        
131200       IF FOOT-FLDECIMAL        = 'N'                                     
131300         MOVE FOOT-SUNTO-TOT        TO D2-FOOT-SUNTO-TOT                  
131400         MOVE FOOT-SUBTO-TOT        TO D2-FOOT-SUBTO-TOT                  
131500         MOVE FOOT-SUVAT-BILLIT-TOT     TO                                
131600                                  D2-FOOT-SUVAT-BILLIT-TOT                
131700       ELSE                                                               
131800         MOVE FOOT-SUNTO-TOT        TO DP-FOOT-SUNTO-TOT                  
131900         MOVE FOOT-SUVAT-BILLIT-TOT TO DP-FOOT-SUVAT-BILLIT-TOT           
132000         MOVE FOOT-SUBTO-TOT        TO DP-FOOT-SUBTO-TOT                  
132100       END-IF                                                             
132200     ELSE                                                                 
132300       MOVE FOOT-SUNTO-TOT        TO DP-FOOT-SUNTO-TOT                    
132400       MOVE FOOT-SUVAT-BILLIT-TOT TO DP-FOOT-SUVAT-BILLIT-TOT             
132500       MOVE FOOT-SUBTO-TOT        TO DP-FOOT-SUBTO-TOT                    
132600     END-IF                                                               
132700     IF FOOT-SUBTO-TOT < ZERO                                             
132800       MOVE '-'                      TO DP-FOOT-REF-MINUS                 
132900     ELSE                                                                 
133000       MOVE SPACE                    TO DP-FOOT-REF-MINUS                 
133100     END-IF                                                               
133200     MOVE FOOT-KDVALISO         TO DP-FOOT-KDVALISO                       
133300     MOVE FOOT-KDVALISO         TO DP-FOOT-KDVALISO-TXT                   
133400     MOVE FOOT-BETEXT-1         TO DP-FOOT-BETEXT-1                       
133500     MOVE FOOT-BETEXT-2         TO DP-FOOT-BETEXT-2                       
133600     MOVE FOOT-BETEXT-3         TO DP-FOOT-BETEXT-3                       
133700     MOVE FOOT-BETEXT-4         TO DP-FOOT-BETEXT-4                       
133800*                                                                         
133900* INDICATES LOCAL CURRENCY ON THE INVOICE                                 
134000     IF FOOT-KDVALISO-LOC NOT = FOOT-KDVALISO                             
134100       MOVE FOOT-SUNTO-TOT-LOC  TO WS-SUNTO-SND-LOC                       
134200       MOVE FOOT-KDVALISO-LOC   TO DP-FOOT-KDVALISO-LOC                   
134300       MOVE FOOT-KDVALISO-LOC   TO DP-FOOT-KDVALISO-TXT                   
134400       MOVE 'J'                 TO WS-FOOT-LOC-SW                         
134500       IF FOOT-FLDECIMAL        = 'N'                                     
134600         MOVE FOOT-SUNTO-TOT-LOC    TO D1-FOOT-SUNTO-TOT-LOC              
134700         MOVE FOOT-SUBTO-TOT-LOC    TO D1-FOOT-SUBTO-TOT-LOC              
134800         MOVE FOOT-SUVAT-BILLIT-TOT-LOC TO                                
134900                                  D1-FOOT-SUVAT-BILLIT-TOT-LOC            
135000       ELSE                                                               
135100         MOVE FOOT-SUNTO-TOT-LOC  TO DP-FOOT-SUNTO-TOT-LOC                
135200         MOVE FOOT-SUBTO-TOT-LOC  TO DP-FOOT-SUBTO-TOT-LOC                
135300         MOVE FOOT-SUVAT-BILLIT-TOT-LOC  TO                               
135400                              DP-FOOT-SUVAT-BILLIT-TOT-LOC                
135500       END-IF                                                             
135600     ELSE                                                                 
135700       MOVE ZERO                TO DP-FOOT-SUNTO-TOT-LOC                  
135800                                   DP-FOOT-SUBTO-TOT-LOC                  
135900                                   DP-FOOT-SUVAT-BILLIT-TOT-LOC           
136000       MOVE SPACE               TO DP-FOOT-KDVALISO-LOC                   
136100     END-IF                                                               
136200*                                                                         
136300* INDICATES SENDING CURRENCY ON THE INVOICE                               
136400     IF FOOT-KDVALISO-SND = FOOT-KDVALISO                                 
136500       MOVE ZERO                TO DP-FOOT-SUNTO-TOT-SND                  
136600                                   DP-FOOT-SUBTO-TOT-SND                  
136700                                   DP-FOOT-SUVAT-BILLIT-TOT-SND           
136800       MOVE SPACE               TO DP-FOOT-KDVALISO-SND                   
136900     ELSE                                                                 
137000       IF FOOT-KDVALISO-SND = FOOT-KDVALISO-LOC                           
137100         MOVE ZERO                TO DP-FOOT-SUNTO-TOT-SND                
137200                                     DP-FOOT-SUBTO-TOT-SND                
137300                                     DP-FOOT-SUVAT-BILLIT-TOT-SND         
137400         MOVE SPACE               TO DP-FOOT-KDVALISO-SND                 
137500       ELSE                                                               
137600         IF FOOT-FLCURINF = 'J'                                           
137700           MOVE FOOT-SUNTO-TOT-SND  TO DP-FOOT-SUNTO-TOT-SND              
137800           MOVE FOOT-SUBTO-TOT-SND  TO DP-FOOT-SUBTO-TOT-SND              
137900           MOVE FOOT-SUVAT-BILLIT-TOT-SND                                 
138000                                    TO                                    
138100                                     DP-FOOT-SUVAT-BILLIT-TOT-SND         
138200           MOVE FOOT-KDVALISO-SND   TO DP-FOOT-KDVALISO-SND               
138300           MOVE 'J'                 TO WS-FOOT-SND-SW                     
138400           IF FOOT-SUNTO-TOT-SND = ZERO                                   
138500             MOVE ZERO              TO DP-FOOT-PRKURS-SND                 
138600           ELSE                                                           
138700             COMPUTE DP-FOOT-PRKURS-SND ROUNDED =                         
138800                        WS-SUNTO-SND-LOC / FOOT-SUNTO-TOT-SND             
138900           END-IF                                                         
139000         ELSE                                                             
139100           MOVE 'N'             TO WS-FOOT-SND-SW                         
139200           MOVE ZERO            TO DP-FOOT-SUNTO-TOT-SND                  
139300                                   DP-FOOT-SUBTO-TOT-SND                  
139400                                   DP-FOOT-SUVAT-BILLIT-TOT-SND           
139500           MOVE SPACE           TO DP-FOOT-KDVALISO-SND                   
139600         END-IF                                                           
139700       END-IF                                                             
139800     END-IF                                                               
139900                                                                          
140000* DENNA KOD ÄR FÖR ATT SKRIVA SUMMA RAD FÖR FREIGHT, INSURANCE            
140100* , PACKING AND HANDLING FAKTURAVIS                                       
140200     IF NEW-FIPH-SUM-ROW                                                  
140300       MOVE +1 TO WS-IX                                                   
140400       PERFORM UNTIL WS-IX > 35                                           
140500         IF WS-BEART(WS-IX) > ' '                                         
140600           PERFORM BK-HANDLE-FIPH-SUM                                     
140700           ADD +1 TO WS-IX                                                
140800         ELSE                                                             
140900           ADD +35 TO WS-IX                                               
141000         END-IF                                                           
141100       END-PERFORM                                                        
141200     END-IF                                                               
141300                                                                          
141400* DENNA KOD ÄR FÖR ATT SKRIVA VALUTAJUSTERINGEN FÖR FAKTUROR SOM          
141500* ÄR OMRÄKNADE TILL LOKAL VALUTA                                          
141600     PERFORM BL-HANDLE-LOCC-JUS                                           
141700                                                                          
141800     PERFORM S90-WRITE-DOC-FOOT                                           
141900                                                                          
142000     IF WS-FOOT-LOC-SW = 'J'                                              
142100       PERFORM BH-HANDLE-FOOT-LOC                                         
142200     END-IF                                                               
142300                                                                          
142400     IF WS-FOOT-SND-SW = 'J'                                              
142500       PERFORM BI-HANDLE-FOOT-SND                                         
142600     END-IF                                                               
142700                                                                          
142800     MOVE NOO TO WS-FOOT-LOC-SW                                           
142900     MOVE NOO TO WS-FOOT-SND-SW                                           
143000     .                                                                    
143100                                                                          
143200 BD-HANDLE-APPENDIX SECTION.                                              
143300     MOVE IN-DATA                 TO IN-AREA-APPX                         
143400                                                                          
143500*    -- IF NEW APPENDIX                                                   
143600*          MARK IDAFPRCD WITH '4.1'                                       
143700     IF NEW-APPENDIX-SW = 'J'                                             
143800       MOVE WS-DOC-NEW-APPENDIX   TO DP-APPX-IDAFPRCD                     
143900       MOVE 'N'                   TO NEW-APPENDIX-SW                      
144000       PERFORM S90-WRITE-DOC-APPX                                         
144100     END-IF                                                               
144200     MOVE APPX-IDPTYP             TO DP-APPX-IDAFPRCD                     
144300     MOVE APPX-KDAPPEND           TO DP-APPX-KDAPPEND                     
144400     MOVE APPX-IDAPPEND           TO DP-APPX-IDAPPEND                     
144500     MOVE APPX-SUNTO-APP          TO DP-APPX-SUNTO-APP                    
144600     MOVE APPX-SUVAT-BILLIT-APP   TO DP-APPX-SUVAT-BILLIT-APP             
144700     MOVE APPX-SUBTO-APP          TO DP-APPX-SUBTO-APP                    
144800                                                                          
144900     PERFORM S90-WRITE-DOC-APPX                                           
145000     .                                                                    
145100                                                                          
145200* DENNA KOD ÄR FÖR ATT SKRIVA ETT APPENDIX FÖR FÖRSÄKRANSTEXT             
145300* DUKUMENTVIS                                                             
145400 BF-HANDLE-INSUR SECTION.                                                 
145500*    -- IF NEW INSURANCE TEXT                                             
145600*    MARK IDAFPRCD WITH '5.1'                                             
145700     IF NEW-INSURANCE-SW = 'J'                                            
145800       MOVE SPACE                 TO DP-INSUR-BETEXT-9                    
145900                                     DP-INSUR-BETEXT-10                   
146000                                     DP-INSUR-BETEXT-11                   
146100                                     DP-INSUR-BETEXT-12                   
146200                                     DP-INSUR-BETEXT-13                   
146300                                     DP-INSUR-BETEXT-14                   
146400                                     DP-INSUR-BETEXT-15                   
146500                                     DP-INSUR-BETEXT-16                   
146600                                     DP-INSUR-BETEXT-17                   
146700                                     DP-INSUR-BETEXT-18                   
146800                                     DP-INSUR-BETEXT-19                   
146900                                     DP-INSUR-BETEXT-20                   
147000                                     DP-INSUR-BETEXT-21                   
147100                                     DP-INSUR-BETEXT-22                   
147200                                     DP-INSUR-BETEXT-23                   
147300                                     DP-INSUR-BETEXT-24                   
147400       MOVE WS-DOC-INSURANCE      TO DP-INSUR-IDAFPRCD                    
147500       MOVE 'N'                   TO NEW-INSURANCE-SW                     
147600       PERFORM S90-WRITE-DOC-INSUR                                        
147700     END-IF                                                               
147800*    MARK IDAFPRCD WITH '5'                                               
147900     MOVE WS-DOC-INSURANCE-ROW    TO DP-INSUR-IDAFPRCD                    
148000     MOVE SAVE-DP-INSUR-BETEXT-9  TO DP-INSUR-BETEXT-9                    
148100     MOVE SAVE-DP-INSUR-BETEXT-10 TO DP-INSUR-BETEXT-10                   
148200     MOVE SAVE-DP-INSUR-BETEXT-11 TO DP-INSUR-BETEXT-11                   
148300     MOVE SAVE-DP-INSUR-BETEXT-12 TO DP-INSUR-BETEXT-12                   
148400     MOVE SAVE-DP-INSUR-BETEXT-13 TO DP-INSUR-BETEXT-13                   
148500     MOVE SAVE-DP-INSUR-BETEXT-14 TO DP-INSUR-BETEXT-14                   
148600     MOVE SAVE-DP-INSUR-BETEXT-15 TO DP-INSUR-BETEXT-15                   
148700     MOVE SAVE-DP-INSUR-BETEXT-16 TO DP-INSUR-BETEXT-16                   
148800     MOVE SAVE-DP-INSUR-BETEXT-17 TO DP-INSUR-BETEXT-17                   
148900     MOVE SAVE-DP-INSUR-BETEXT-18 TO DP-INSUR-BETEXT-18                   
149000     MOVE SAVE-DP-INSUR-BETEXT-19 TO DP-INSUR-BETEXT-19                   
149100     MOVE SAVE-DP-INSUR-BETEXT-20 TO DP-INSUR-BETEXT-20                   
149200     MOVE SAVE-DP-INSUR-BETEXT-21 TO DP-INSUR-BETEXT-21                   
149300     MOVE SAVE-DP-INSUR-BETEXT-22 TO DP-INSUR-BETEXT-22                   
149400     MOVE SAVE-DP-INSUR-BETEXT-23 TO DP-INSUR-BETEXT-23                   
149500     MOVE SAVE-DP-INSUR-BETEXT-24 TO DP-INSUR-BETEXT-24                   
149600                                                                          
149700     PERFORM S90-WRITE-DOC-INSUR                                          
149800                                                                          
149900*    MARK IDAFPRCD WITH '5.2'                                             
150000     MOVE WS-DOC-INSURANCE-ADD    TO DP-INSUR-IDAFPRCD                    
150100     MOVE SAVE-DP-ADD-BETEXT-25   TO DP-INSUR-BETEXT-9                    
150200     MOVE SAVE-DP-ADD-BETEXT-26   TO DP-INSUR-BETEXT-10                   
150300     MOVE SAVE-DP-ADD-BETEXT-27   TO DP-INSUR-BETEXT-11                   
150400     MOVE SAVE-DP-ADD-BETEXT-28   TO DP-INSUR-BETEXT-12                   
150500     MOVE SAVE-DP-ADD-BETEXT-29   TO DP-INSUR-BETEXT-13                   
150600     MOVE SAVE-DP-ADD-BETEXT-30   TO DP-INSUR-BETEXT-14                   
150700     MOVE SAVE-DP-ADD-BETEXT-31   TO DP-INSUR-BETEXT-15                   
150800     MOVE SAVE-DP-ADD-BETEXT-32   TO DP-INSUR-BETEXT-16                   
150900                                                                          
151000     PERFORM S90-WRITE-DOC-INSUR                                          
151100                                                                          
151200     .                                                                    
151300*                                                                         
151400                                                                          
151500 BF-HANDLE-ADD-INSUR SECTION.                                             
151600*    -- IF NEW ADDITIONAL INSURANCE TEXT                                  
151700*    MARK IDAFPRCD WITH '5.3'                                             
151800     IF NEW-ADD-INSURANCE-SW = 'J'                                        
151900       MOVE SPACE                 TO DP-ADD-BETEXT-33                     
152000                                     DP-ADD-BETEXT-34                     
152100                                     DP-ADD-BETEXT-35                     
152200                                     DP-ADD-BETEXT-36                     
152300                                     DP-ADD-BETEXT-37                     
152400                                     DP-ADD-BETEXT-38                     
152500                                     DP-ADD-BETEXT-39                     
152600                                     DP-ADD-BETEXT-40                     
152700                                     DP-ADD-BETEXT-41                     
152800                                     DP-ADD-BETEXT-42                     
152900                                     DP-ADD-BETEXT-43                     
153000                                     DP-ADD-BETEXT-44                     
153100       MOVE WS-DOC-ADD-INSURANCE  TO DP-ADD-IDAFPRCD                      
153200       MOVE 'N'                   TO NEW-ADD-INSURANCE-SW                 
153300       PERFORM S90-WRITE-ADD-INSUR                                        
153400     END-IF                                                               
153500*    MARK IDAFPRCD WITH '5.4'                                             
153600     MOVE WS-DOC-ADD-INSURANCE-ROW TO DP-ADD-IDAFPRCD                     
153700     MOVE SAVE-DP-ADD-BETEXT-33   TO DP-ADD-BETEXT-33                     
153800     MOVE SAVE-DP-ADD-BETEXT-34   TO DP-ADD-BETEXT-34                     
153900     MOVE SAVE-DP-ADD-BETEXT-35   TO DP-ADD-BETEXT-35                     
154000     MOVE SAVE-DP-ADD-BETEXT-36   TO DP-ADD-BETEXT-36                     
154100     MOVE SAVE-DP-ADD-BETEXT-37   TO DP-ADD-BETEXT-37                     
154200     MOVE SAVE-DP-ADD-BETEXT-38   TO DP-ADD-BETEXT-38                     
154300     MOVE SAVE-DP-ADD-BETEXT-39   TO DP-ADD-BETEXT-39                     
154400     MOVE SAVE-DP-ADD-BETEXT-40   TO DP-ADD-BETEXT-40                     
154500     MOVE SAVE-DP-ADD-BETEXT-41   TO DP-ADD-BETEXT-41                     
154600     MOVE SAVE-DP-ADD-BETEXT-42   TO DP-ADD-BETEXT-42                     
154700     MOVE SAVE-DP-ADD-BETEXT-43   TO DP-ADD-BETEXT-43                     
154800     MOVE SAVE-DP-ADD-BETEXT-44   TO DP-ADD-BETEXT-44                     
154900                                                                          
155000     PERFORM S90-WRITE-ADD-INSUR                                          
155100                                                                          
155200     .                                                                    
155300*                                                                         
155400                                                                          
155500 BG-HANDLE-CREDIT SECTION.                                                
155600*    -- IF NEW CREDIT TEXT                                                
155700*          MARK IDAFPRCD WITH '6.1'                                       
155800     IF NEW-CREDIT-SW = 'J'                                               
155900       MOVE WS-DOC-CREDIT         TO DP-CREDI-IDAFPRCD                    
156000       MOVE 'N'                   TO NEW-CREDIT-SW                        
156100       PERFORM S90-WRITE-DOC-CREDIT                                       
156200     END-IF                                                               
156300     MOVE WS-DOC-CREDIT-ROW       TO DP-CREDI-IDAFPRCD                    
156400     MOVE SAVE-DP-CREDI-BETEXT    TO DP-CREDI-BETEXT                      
156500     MOVE SAVE-DP-CREDI-BETEXT-CRE TO DP-CREDI-BETEXT-CRE                 
156600                                                                          
156700     PERFORM S90-WRITE-DOC-CREDIT                                         
156800     .                                                                    
156900*                                                                         
157000 BH-HANDLE-FOOT-LOC SECTION.                                              
157100*    -- IF NEW LOCAL CURRENCY DIFFERENT FROM INVOICE CURRENCY             
157200*          MARK IDAFPRCD WITH '3.1'                                       
157300     MOVE WS-DOC-FOOTER-LOC   TO DP-FOOT-IDAFPRCD                         
157400     IF FOOT-FLDECIMAL        = 'N'                                       
157500       MOVE FOOT-SUNTO-TOT-LOC   TO D1-FOOT-SUNTO-TOT-LOC                 
157600       MOVE FOOT-SUBTO-TOT-LOC   TO D1-FOOT-SUBTO-TOT-LOC                 
157700       MOVE FOOT-SUVAT-BILLIT-TOT-LOC TO                                  
157800                               D1-FOOT-SUVAT-BILLIT-TOT-LOC               
157900     ELSE                                                                 
158000       MOVE FOOT-SUNTO-TOT-LOC   TO DP-FOOT-SUNTO-TOT-LOC                 
158100       MOVE FOOT-SUBTO-TOT-LOC   TO DP-FOOT-SUBTO-TOT-LOC                 
158200       MOVE FOOT-SUVAT-BILLIT-TOT-LOC TO                                  
158300                               DP-FOOT-SUVAT-BILLIT-TOT-LOC               
158400     END-IF                                                               
158500     MOVE WS-DOC-FOOTER-LOC     TO DP-FOOT-IDAFPRCD                       
158600     MOVE FOOT-KDVALISO-LOC     TO DP-FOOT-KDVALISO-LOC                   
158700                                                                          
158800     PERFORM S90-WRITE-DOC-FOOT                                           
158900     .                                                                    
159000*                                                                         
159100 BI-HANDLE-FOOT-SND SECTION.                                              
159200*    -- IF NEW SENDING CURRENCY DIFFERENT FROM INVOICE CURRENCY           
159300*          MARK IDAFPRCD WITH '3.2'                                       
159400     MOVE WS-DOC-FOOTER-SND   TO DP-FOOT-IDAFPRCD                         
159500     MOVE FOOT-SUNTO-TOT-SND  TO DP-FOOT-SUNTO-TOT-SND                    
159600     MOVE FOOT-SUBTO-TOT-SND  TO DP-FOOT-SUBTO-TOT-SND                    
159700     MOVE FOOT-SUVAT-BILLIT-TOT-SND                                       
159800                              TO DP-FOOT-SUVAT-BILLIT-TOT-SND             
159900     MOVE FOOT-KDVALISO-SND   TO DP-FOOT-KDVALISO-SND                     
160000                                                                          
160100     PERFORM S90-WRITE-DOC-FOOT                                           
160200     .                                                                    
160300*                                                                         
160400 BJ-HANDLE-DADR SECTION.                                                  
160500*    -- IF NEW DELIVERY ADRESS                                            
160600*          MARK IDAFPRCD WITH '6.1'                                       
160700     IF NEW-DADR-SW = 'J'                                                 
160800       MOVE WS-DOC-DADR              TO DP-DADR-IDAFPRCD                  
160900       MOVE 'N'                      TO NEW-DADR-SW                       
161000       PERFORM S90-WRITE-DOC-DADR                                         
161100     END-IF                                                               
161200     MOVE WS-DOC-DADR-ROW            TO DP-DADR-IDAFPRCD                  
161300     MOVE SAVE-DP-DADR-BETEXT        TO DP-DADR-BETEXT                    
161400                                                                          
161500     PERFORM S90-WRITE-DOC-DADR                                           
161600     .                                                                    
161700                                                                          
161800* DENNA KOD ÄR FÖR ATT SKRIVA SUMMA RAD FÖR FREIGHT, INSURANCE            
161900* , PACKING AND HANDLING FAKTURAVIS                                       
162000 BK-HANDLE-FIPH-SUM SECTION.                                              
162100     MOVE WS-DOC-FIPH-SUM        TO DP-FIPH-IDAFPRCD                      
162200     MOVE WS-BEART(WS-IX)        TO DP-FIPH-BEART                         
162300     MOVE WS-KVLEVART(WS-IX)     TO DP-FIPH-KVLEVART                      
162400     MOVE WS-SUNTO(WS-IX)        TO DP-FIPH-SUNTO                         
162500     IF WS-SUNTO(WS-IX) < ZERO                                            
162600       MOVE '-'                      TO DP-FIPH-REF-MINUS                 
162700     ELSE                                                                 
162800       MOVE SPACE                    TO DP-FIPH-REF-MINUS                 
162900     END-IF                                                               
163000                                                                          
163100     PERFORM S90-WRITE-DOC-FIPH-SUM                                       
163200     .                                                                    
163300                                                                          
163400* DENNA KOD ÄR FÖR ATT SKRIVA VALUTAJUSTERINGEN FÖR FAKTUROR SOM          
163500* ÄR OMRÄKNADE TILL LOKAL VALUTA                                          
163600 BL-HANDLE-LOCC-JUS SECTION.                                              
163700     IF WS-LOCC-JUS-SUNTO = WS-LOCC-JUS-CHECK                             
163800       CONTINUE                                                           
163900     ELSE                                                                 
164000       IF WS-LOCC-JUS-SUNTO > WS-LOCC-JUS-CHECK                           
164100         MOVE WS-DOC-LOCC-JUS          TO DP-LOCC-IDAFPRCD                
164200         MOVE '-'                      TO DP-LOCC-MINUS                   
164300         COMPUTE WS-LOCC-JUS-SUNTO = WS-LOCC-JUS-CHECK -                  
164400                                     WS-LOCC-JUS-SUNTO                    
164500         MOVE WS-LOCC-JUS-SUNTO        TO DP-LOCC-SUNTO                   
164600       ELSE                                                               
164700         MOVE WS-DOC-LOCC-JUS          TO DP-LOCC-IDAFPRCD                
164800         MOVE SPACE                    TO DP-LOCC-MINUS                   
164900         COMPUTE WS-LOCC-JUS-SUNTO = WS-LOCC-JUS-CHECK -                  
165000                                     WS-LOCC-JUS-SUNTO                    
165100         MOVE WS-LOCC-JUS-SUNTO        TO DP-LOCC-SUNTO                   
165200       END-IF                                                             
165300       PERFORM S90-WRITE-DOC-LOCC-JUS                                     
165400     END-IF                                                               
165500     .                                                                    
165600                                                                          
165700* DENNA KOD ÄR FÖR ATT SKRIVA GOODS VALUE PÅ PROFORMA                     
165800 BM-HANDLE-PROF SECTION.                                                  
165900     IF PROF-SUNTO > ZERO                                                 
166000       MOVE WS-DOC-PROF       TO DP-PROF-IDAFPRCD                         
166100       MOVE PROF-SUNTO        TO DP-PROF-SUNTO                            
166200       PERFORM S90-WRITE-DOC-PROF                                         
166300     ELSE                                                                 
166400       CONTINUE                                                           
166500     END-IF                                                               
166600     .                                                                    
166700                                                                          
166800 Z-FINIT SECTION.                                                         
166900     CLOSE WF2011                                                         
167000           WF2101                                                         
167100     .                                                                    
167200                                                                          
167300 S01-READ-WF2011  SECTION.                                                
167400     READ WF2011                                                          
167500       AT END                                                             
167600         SET END-OF-WF2011 TO TRUE                                        
167700     END-READ                                                             
167800     .                                                                    
167900                                                                          
168000 S90-WRITE-HEADER SECTION.                                                
168100                                                                          
168200     PERFORM S91-SKRIV-DAP1-S                                             
168300     PERFORM S91-SKRIV-DAP2-S                                             
168400     PERFORM S91-SKRIV-DAP3-S                                             
168500     PERFORM S91-SKRIV-DAP4-S                                             
168600     .                                                                    
168700 S90-WRITE-DOC-HEAD SECTION.                                              
168800                                                                          
168900     WRITE WF2101-001   FROM DOC-HEAD-AREA                                
169000     .                                                                    
169100 S90-WRITE-DOC-LINE SECTION.                                              
169200                                                                          
169300     WRITE WF2101-001   FROM DOC-LINE-AREA                                
169400     .                                                                    
169500 S90-WRITE-DOC-FOOT SECTION.                                              
169600                                                                          
169700     WRITE WF2101-001   FROM DOC-FOOT-AREA                                
169800     .                                                                    
169900 S90-WRITE-DOC-APPX SECTION.                                              
170000                                                                          
170100     WRITE WF2101-001   FROM DOC-APPX-AREA                                
170200     .                                                                    
170300 S90-WRITE-DOC-INSUR SECTION.                                             
170400                                                                          
170500     WRITE WF2101-001   FROM DOC-INSUR                                    
170600     .                                                                    
170700 S90-WRITE-ADD-INSUR SECTION.                                             
170800                                                                          
170900     WRITE WF2101-001   FROM DOC-ADD-INSUR                                
171000     .                                                                    
171100 S90-WRITE-DOC-CREDIT SECTION.                                            
171200                                                                          
171300     WRITE WF2101-001   FROM DOC-CREDIT                                   
171400     .                                                                    
171500 S90-WRITE-DOC-FIPH-SUM SECTION.                                          
171600                                                                          
171700     WRITE WF2101-001   FROM DOC-FIPH-SUM                                 
171800     .                                                                    
171900 S90-WRITE-DOC-DADR SECTION.                                              
172000                                                                          
172100     WRITE WF2101-001   FROM DOC-DADR                                     
172200     .                                                                    
172300 S90-WRITE-DOC-LOCC-JUS SECTION.                                          
172400                                                                          
172500     WRITE WF2101-001   FROM DOC-LOCC-JUS                                 
172600     .                                                                    
172700 S90-WRITE-DOC-PROF SECTION.                                              
172800                                                                          
172900     WRITE WF2101-001   FROM DOC-PROF                                     
173000     .                                                                    
173100 S91-SKRIV-DAP1-S SECTION.                                                
173200                                                                          
173300     STRING ' ¤DAP' WS-HDR-AREA-1                                         
173400            DELIMITED BY SIZE INTO W001-DAP                               
173500     WRITE WF2101-001     FROM W001-DAP                                   
173600                                                                          
173700     MOVE SPACE TO W001-DAP                                               
173800     .                                                                    
173900                                                                          
174000 S91-SKRIV-DAP2-S SECTION.                                                
174100                                                                          
174200     STRING ' ¤DAP' WS-HDR-AREA-2                                         
174300            DELIMITED BY SIZE INTO W001-DAP                               
174400     WRITE WF2101-001         FROM W001-DAP                               
174500                                                                          
174600     MOVE SPACE TO W001-DAP                                               
174700     .                                                                    
174800 S91-SKRIV-DAP3-S SECTION.                                                
174900                                                                          
175000     STRING ' ¤DAP' WS-HDR-AREA-3                                         
175100            DELIMITED BY SIZE INTO W001-DAP                               
175200     WRITE WF2101-001         FROM W001-DAP                               
175300                                                                          
175400     MOVE SPACE TO W001-DAP                                               
175500     .                                                                    
175600 S91-SKRIV-DAP4-S SECTION.                                                
175700                                                                          
175800     STRING ' ¤METADOCUMENT_DATE=' WS-HDR-AREA-4                          
175900            DELIMITED BY SIZE INTO W001-DAP                               
176000     WRITE WF2101-001         FROM W001-DAP                               
176100                                                                          
176200     MOVE SPACE TO W001-DAP                                               
176300                                                                          
176400     STRING ' ¤METAFINANCIAL_CUSTOMER=' WS-HDR-AREA-5                     
176500            DELIMITED BY SIZE INTO W001-DAP                               
176600     WRITE WF2101-001         FROM W001-DAP                               
176700                                                                          
176800     MOVE SPACE TO W001-DAP                                               
176900                                                                          
177000     STRING ' ¤METAFILE_NAME='                                            
177100            DELIMITED BY SIZE                                             
177200            WS-KDFINDOC                                                   
177300            DELIMITED BY SPACE                                            
177400            '_'                                                           
177500            DELIMITED BY SIZE                                             
177600            WS-IDFINDOC1                                                  
177700            DELIMITED BY SIZE                                             
177800            '_'                                                           
177900            DELIMITED BY SIZE                                             
178000            WS-TIMESTAMP                                                  
178100            DELIMITED BY SIZE INTO W001-DAP                               
178200     WRITE WF2101-001         FROM W001-DAP                               
178300                                                                          
178400     MOVE SPACE TO W001-DAP                                               
178500     .                                                                    
178600 S91-SKRIV-DAP5-S SECTION.                                                
178700                                                                          
178800     IF HEAD-BEFORMS NOT = 'WEBSHOP-INVOICE'                              
178900     STRING ' ¤METADISTRICT=' WS-HDR-AREA-6                               
179000            DELIMITED BY SIZE INTO W001-DAP                               
179100     WRITE WF2101-001         FROM W001-DAP                               
179200     END-IF                                                               
179300                                                                          
179400     MOVE SPACE TO W001-DAP                                               
179500                                                                          
179600     INSPECT WS-HDR-AREA-7 REPLACING LEADING ZERO  BY SPACE               
179700     STRING ' ¤METADOCUMENT_NUMBER='  WS-HDR-AREA-7                       
179800            DELIMITED BY SIZE INTO W001-DAP                               
179900     WRITE WF2101-001         FROM W001-DAP                               
180000                                                                          
180100     MOVE SPACE TO W001-DAP                                               
180200     IF HEAD-BEFORMS NOT = 'WEBSHOP-INVOICE'                              
180300     STRING ' ¤METADC=' WS-HDR-AREA-8                                     
180400            DELIMITED BY SIZE INTO W001-DAP                               
180500     WRITE WF2101-001         FROM W001-DAP                               
180600     END-IF                                                               
180700                                                                          
180800     MOVE SPACE TO W001-DAP                                               
180900                                                                          
181000     STRING ' ¤METABUYER=' WS-HDR-AREA-9                                  
181100            DELIMITED BY SIZE INTO W001-DAP                               
181200     WRITE WF2101-001         FROM W001-DAP                               
181300                                                                          
181400     MOVE SPACE TO W001-DAP                                               
181500                                                                          
181600     IF HEAD-BEFORMS NOT = 'WEBSHOP-INVOICE'                              
181700     STRING ' ¤METAMARKET=' WS-HDR-AREA-10                                
181800            DELIMITED BY SIZE INTO W001-DAP                               
181900     WRITE WF2101-001         FROM W001-DAP                               
182000     END-IF                                                               
182100                                                                          
182200     MOVE SPACE TO W001-DAP                                               
182300     .                                                                    
