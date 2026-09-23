000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4051100.                                                
000300 AUTHOR.         STEFANO GIOBBI.                                          
000400 DATE-WRITTEN.   95/02/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        QUERY PROGRAM - DISTRICT, CUSTOMER, ORDER, ORDER VALUE           
000900*                                                                         
001000*        THE PROGRAM READS     WDQ2                                       
001100*        THE PROGRAM READS     WDQ3                                       
001200*        THE PROGRAM READS     WDE6                                       
001300*        THE PROGRAM READS     WDE8                                       
001400*        THE PROGRAM READS     WDB6                                       
001500*        THE PROGRAM READS     WDB2                                       
001600*        THE PROGRAM READS     WDB1                                       
001700*        THE PROGRAM READS     WDG2                                       
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W4T511                                              
002100*        MID:         W4I51101                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        MOD:         W4O51101                                            
002500*                                                                         
002600*    PROGRAM REWRITTEN MAY 2006 BY LINDA NILSSON                          
002700*    ETRACKER 899925 (SCREEN SORTING)                                     
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W4051100'.            
003600                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  FILLER                      PIC X(08) VALUE 'FELTEXT:'.              
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004400 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
004500 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
004600                                                                          
004700*    --- INDEX FOR SCROLL LINES                                           
004800 77  MOD-INDX                    PIC S9(3)   VALUE +00   COMP-3.          
004900 77  MAX-MOD-INDX                PIC S9(3)   VALUE +14   COMP-3.          
005000                                                                          
005100 77  ALL-SW                      PIC X       VALUE 'J'.                   
005200     88  ALL-OK                              VALUE 'J'.                   
005300                                                                          
005400 01  W-IDKUNDNR                  PIC X.                                   
005500 01  WS-TEASTRIX                 PIC X       VALUE SPACE.                 
005600 01  WS-KDVALISO                 PIC X(3)    VALUE SPACE.                 
005700 01  WS-SUORDV                   PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005800 01  WS-SEK                      PIC X(3)    VALUE 'SEK'.                 
005900 01  HELP-SUM                    PIC S9(9)V9(2) VALUE ZERO COMP-3.        
006000 01  WS-TEDDI.                                                            
006100     03 FILLER                   PIC X(4) VALUE SPACE.                    
006200     03 WS-ODEL-KDVALISO         PIC X(3).                                
006300     03 FILLER                   PIC X(4) VALUE SPACE.                    
006400                                                                          
006500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
006600                                                                          
006700                                                                          
006800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006900     88  KEYS-OK                             VALUE 'J'.                   
007000     88  KEYS-WRONG                          VALUE 'N'.                   
007100                                                                          
007200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007300     88  OWN-MID                             VALUE '4511'.                
007400     88  GOOD-MID                            VALUE '4501' '4502'          
007500                                                   '4503' '4508'          
007600                                                   '4509' '4511'          
007700                                                   '4512' '4513'          
007800                                                   '4514' '4515'          
007900                                                   '4517'.                
008000     88  HELP-MID                            VALUE '0551'.                
008100     EJECT                                                                
008200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008300 01  GENERAL-SUBPROGRAMS.                                                 
008400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008800     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
008900     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
009000     EJECT                                                                
009100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009200*01 -COPY WMEDAREA                                                        
009300     SKIP3                                                                
009400*    --- PARAMETERS FOR SUBPROGRAM W510CURR                               
009500*01 -COPY W510CURR                                                        
009600     SKIP3                                                                
009700 01  MESSAGE-CODES.                                                       
009800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009900     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010000     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
010100     03  INF-NO-MORE-PF6         PIC X(3)    VALUE '368'.                 
010200     03  ERR-WRONG-DC            PIC X(3)    VALUE '026'.                 
010300     03  ERR-LINES-MISSING       PIC X(3)    VALUE '029'.                 
010400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010500     EJECT                                                                
010600*    --- FIELDS FOR START OTHER SCREEN                                    
010700 77  START-OTHER-SCREEN-SW       PIC X       VALUE 'N'.                   
010800     88  START-OTHER-SCREEN                  VALUE 'J'.                   
010900                                                                          
011000 01  START-SCREEN-AREAS.                                                  
011100                                                                          
011200     03  W-SCREEN                PIC X(4)    VALUE SPACE.                 
011300     03  W-SWITCH-IDTRANS.                                                
011400         05 FILLER               PIC X(1)    VALUE 'W'.                   
011500         05 W-SWITCH-IDTRANS-2   PIC X(1).                                
011600         05 FILLER               PIC X(1)    VALUE 'T'.                   
011700         05 W-SWITCH-IDTRANS-4-6 PIC X(3).                                
011800         05 FILLER               PIC X(2)    VALUE SPACE.                 
011900                                                                          
012000     03 FILLER                   PIC X(16)   VALUE 'P-TO-P-AREA'.         
012100     03  P-TO-P-SW.                                                       
012200         05 P-TO-P-KVLL          PIC S9(4)   VALUE +117 COMP SYNC.        
012300         05 P-TO-P-KDZ1          PIC X(1)    VALUE LOW-VALUE.             
012400         05 P-TO-P-KDZ2          PIC X(1)    VALUE LOW-VALUE.             
012500         05 P-TO-P-KDTRANS       PIC X(8).                                
012600         05 P-TO-P-IDTRANS       PIC X(4).                                
012700         05 P-TO-P-KDMFSFOR      PIC X(1).                                
012800         05 P-TO-P-DATA          PIC X(100)  VALUE ALL '+'.               
012900                                                                          
013000*    --- SUBPROGRAM                                                       
013100 01  GEMENSAMMA-SUBPROGRAM.                                               
013200     03  W411EXCH                PIC X(8)    VALUE 'W411EXCH'.            
013300     EJECT                                                                
013400 01  FILLER                      PIC X(18)   VALUE                        
013500                                           'CALCULATE-CURRENCY'.          
013600*01  -COPY W411EXCH                                                       
013700*                                                                         
013800     EJECT                                                                
013900*                                                                         
014000*    --- PARAMETERS FOR SUB PROGRAM WSECURIT                              
014100*                                                                         
014200*01  -COPY WSECAREA                                                       
014300*                                                                         
014400     EJECT                                                                
014500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
014600*                                                                         
014700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014800     SKIP3                                                                
014900*01 -COPY WMSGINIT                                                        
015000     EJECT                                                                
015100*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
015200*                                                                         
015300*                                                                         
015400*      --- VALID IDDC CODES                                               
015500*                                                                         
015600*01    -COPY WWDC99                                                       
015700*                                                                         
015800*01    -COPY WWDCKONS                                                     
015900                                                                          
016000 01  TEST-IDDISTR              PIC S9(5)    COMP-3.                       
016100*    ---DISTRICTS BELONGING TO USA, CANADA, PACIFIC, CHINA                
016200 01  FILLER REDEFINES TEST-IDDISTR.                                       
016300*    03     -COPY WWDIST07.                                               
016400     EJECT                                                                
016500*    ---SCRAP DISTRICTS                                                   
016600 01  FILLER REDEFINES TEST-IDDISTR.                                       
016700*    03     -COPY WWDIST18.                                               
016800     EJECT                                                                
016900*    ---SDC/NDC BELONGING                                                 
017000 01  FILLER REDEFINES TEST-IDDISTR.                                       
017100*    03     -COPY WWDIST40.                                               
017200     EJECT                                                                
017300*    ---DEALER-PRICE DISTRICTS                                            
017400 01  FILLER REDEFINES TEST-IDDISTR.                                       
017500*    03     -COPY WWDIST79.                                               
017600     EJECT                                                                
017700*    ---EXCHANGE DISTRICTS                                                
017800 01  FILLER REDEFINES TEST-IDDISTR.                                       
017900*    03     -COPY WWDIS134.                                               
018000     EJECT                                                                
018100                                                                          
018200 01  FILLER                        PIC X(08) VALUE 'SAVEAREA'.            
018300 01  SAVE-AREA.                                                           
018400     03  SAVE-IDTRANS              PIC X(4)  VALUE SPACE.                 
018500     03  PGNO                      PIC 9(2)  VALUE ZERO.                  
018600     03  FIRST-SW                  PIC X     VALUE 'J'.                   
018700     03  SAVE-AREA-KEYS.                                                  
018800       04 SAVE-AREA-PREV OCCURS 20.                                       
018900         05 SAVE-IDKUNDNR-PREV     PIC S9(7) VALUE ZERO COMP-3.           
019000         05 SAVE-IDKUNDRF-PREV     PIC X(10) VALUE SPACE.                 
019100         05 SAVE-IDDC-PREV         PIC X(2)  VALUE SPACE.                 
019200         05 SAVE-IDPRODNR-PREV     PIC S9(7) VALUE ZERO COMP-3.           
019300         05 SAVE-IDORDER-PREV      PIC S9(7) VALUE ZERO COMP-3.           
019400       04 SAVE-AREA-NEXT.                                                 
019500         05 SAVE-IDKUNDNR-NEXT     PIC S9(7) VALUE ZERO COMP-3.           
019600         05 SAVE-IDKUNDRF-NEXT     PIC X(10) VALUE SPACE.                 
019700         05 SAVE-IDDC-NEXT         PIC X(2)  VALUE SPACE.                 
019800         05 SAVE-IDPRODNR-NEXT     PIC S9(7) VALUE ZERO COMP-3.           
019900         05 SAVE-IDORDER-NEXT      PIC S9(7) VALUE ZERO COMP-3.           
020000     03  SAVE-FLSORT               PIC X     VALUE 'N'.                   
020100     03  SAVE-FLSOFT               PIC X     VALUE 'N'.                   
020200     03  SAVE-FLPROF               PIC X     VALUE 'N'.                   
020300     SKIP3                                                                
020400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
020500*                                                                         
020600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
020700     SKIP3                                                                
020800*01  MID -COPY W4I51101                                                   
020900     EJECT                                                                
021000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021100     SKIP3                                                                
021200*01  -COPY WMSGAREA                                                       
021300     EJECT                                                                
021400     03  MOD REDEFINES MSG-AREA.                                          
021500*        05  -COPY W4O51101                                               
021600     EJECT                                                                
021700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
021800     SKIP3                                                                
021900*01  -COPY WMFSAREA                                                       
022000     EJECT                                                                
022100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
022200*                                                                         
022300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022400     SKIP3                                                                
022500 01  KEYS-TO-DLI.                                                         
022600                                                                          
022700     03  W-WDQ3GSEQ-MIN-X.                                                
022800         05  W-IDDISTR-GSEQ-MIN  PIC S9(5) VALUE ZERO COMP-3.             
022900         05  W-IDKUNDNR-GSEQ-MIN PIC S9(7) VALUE ZERO COMP-3.             
023000         05  W-IDDC-GSEQ-MIN     PIC X(2) VALUE SPACE.                    
023100         05  W-IDKUNDRF-GSEQ-MIN PIC X(10) VALUE SPACE.                   
023200         05  W-IDORDER-GSEQ-MIN  PIC S9(7) VALUE ZERO COMP-3.             
023300         05  W-IDPRODNR-GSEQ-MIN PIC S9(7) VALUE ZERO COMP-3.             
023400         05  W-IDPLKSLT-GSEQ-MIN PIC S9(3) VALUE ZERO COMP-3.             
023500                                                                          
023600     03  W-WDQ3GSEQ-MAX-X.                                                
023700         05  W-IDDISTR-GSEQ-MAX  PIC S9(5) VALUE +99999 COMP-3.           
023800         05  W-IDKUNDNR-GSEQ-MAX PIC S9(7) VALUE +9999999 COMP-3.         
023900         05  W-IDDC-GSEQ-MAX     PIC X(2) VALUE SPACE.                    
024000         05  W-IDKUNDRF-GSEQ-MAX PIC X(10) VALUE SPACE.                   
024100         05  W-IDORDER-GSEQ-MAX  PIC S9(7) VALUE ZERO COMP-3.             
024200         05  W-IDPRODNR-GSEQ-MAX PIC S9(7) VALUE ZERO COMP-3.             
024300         05  W-IDPLKSLT-GSEQ-MAX PIC S9(3) VALUE ZERO COMP-3.             
024400                                                                          
024500     03  W-WDQ2F1KY-MIN-X.                                                
024600         05  W-FLSOFT-F1KY-MIN   PIC X     VALUE SPACE.                   
024700         05  W-IDDISTR-F1KY-MIN  PIC S9(5) VALUE ZERO COMP-3.             
024800         05  W-TIREGDAT-F1KY-MIN PIC S9(7) VALUE ZERO COMP-3.             
024900         05  W-IDKUNDNR-F1KY-MIN PIC S9(7) VALUE ZERO COMP-3.             
025000         05  W-IDORDER-F1KY-MIN  PIC S9(7) VALUE ZERO COMP-3.             
025100                                                                          
025200     03  W-WDQ2F1KY-MAX-X.                                                
025300         05  W-FLSOFT-F1KY-MAX   PIC X     VALUE SPACE.                   
025400         05  W-IDDISTR-F1KY-MAX  PIC S9(5) VALUE +99999 COMP-3.           
025500         05  W-TIREGDAT-F1KY-MAX PIC S9(7) VALUE +9999999 COMP-3.         
025600         05  W-IDKUNDNR-F1KY-MAX PIC S9(7) VALUE +9999999 COMP-3.         
025700         05  W-IDORDER-F1KY-MAX  PIC S9(7) VALUE +9999999 COMP-3.         
025800                                                                          
025900     03  W-WDQ2D1KY-MIN-X.                                                
026000         05  W-FLSOFT-D1KY-MIN   PIC X     VALUE SPACE.                   
026100         05  W-IDDISTR-D1KY-MIN  PIC S9(5) VALUE ZERO COMP-3.             
026200         05  W-IDKUNDNR-D1KY-MIN PIC S9(7) VALUE ZERO COMP-3.             
026300         05  W-TIREGDAT-D1KY-MIN PIC S9(7) VALUE ZERO COMP-3.             
026400         05  W-IDORDER-D1KY-MIN  PIC S9(7) VALUE ZERO COMP-3.             
026500                                                                          
026600     03  W-WDQ2D1KY-MAX-X.                                                
026700         05  W-FLSOFT-D1KY-MAX   PIC X     VALUE SPACE.                   
026800         05  W-IDDISTR-D1KY-MAX  PIC S9(5) VALUE +99999 COMP-3.           
026900         05  W-IDKUNDNR-D1KY-MAX PIC S9(7) VALUE +9999999 COMP-3.         
027000         05  W-TIREGDAT-D1KY-MAX PIC S9(7) VALUE +9999999 COMP-3.         
027100         05  W-IDORDER-D1KY-MAX  PIC S9(7) VALUE +9999999 COMP-3.         
027200                                                                          
027300     03  W-IDDC-Q212-X.                                                   
027400         05  W-IDDC-Q212         PIC X(2)   VALUE SPACE.                  
027500                                                                          
027600     03  W-IDDC-WDQ3G1-X.                                                 
027700         05  W-IDDC-WDQ3G1       PIC X(2)   VALUE SPACE.                  
027800                                                                          
027900     03  W-WDQ301KY-MIN-X.                                                
028000         05  W-IDORDER-Q3-MIN    PIC S9(7)  VALUE ZERO COMP-3.            
028100         05  W-IDDC-Q3-MIN       PIC X(2)   VALUE SPACE.                  
028200         05  W-IDPRODNR-Q3-MIN   PIC S9(7)  VALUE ZERO COMP-3.            
028300         05  W-IDPLKLST-Q3-MIN   PIC S9(3)  VALUE ZERO COMP-3.            
028400                                                                          
028500     03  W-WDQ301KY-MAX-X.                                                
028600         05  W-IDORDER-Q3-MAX    PIC S9(7)  VALUE ZERO COMP-3.            
028700         05  W-IDDC-Q3-MAX       PIC X(2)   VALUE SPACE.                  
028800         05  W-IDPRODNR-Q3-MAX   PIC S9(7)  VALUE +9999999 COMP-3.        
028900         05  W-IDPLKLST-Q3-MAX   PIC S9(3)  VALUE +999 COMP-3.            
029000                                                                          
029100     03  W-WDE801KY-MIN-X.                                                
029200         05  W-IDDISTR-E8-MIN    PIC S9(5)  VALUE ZERO COMP-3.            
029300         05  W-IDKUNDNR-E8-MIN   PIC S9(7)  VALUE ZERO COMP-3.            
029400         05  W-IDKUNDRF-E8-MIN   PIC X(10)  VALUE SPACE.                  
029500                                                                          
029600     03  W-WDE801KY-MAX-X.                                                
029700         05  W-IDDISTR-E8-MAX    PIC S9(5)  VALUE ZERO COMP-3.            
029800         05  W-IDKUNDNR-E8-MAX   PIC S9(7)  VALUE ZERO COMP-3.            
029900         05  W-IDKUNDRF-E8-MAX   PIC X(10)  VALUE SPACE.                  
030000                                                                          
030100     03  W-IDORDER-X2.                                                    
030200         05  W-IDORDER2          PIC S9(7) COMP-3.                        
030300                                                                          
030400     03  W-IDDC-B6-X.                                                     
030500         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
030600                                                                          
030700     03  W-IDGMT-B2-X.                                                    
030800         05  W-IDDISTR-B2        PIC S9(5)  VALUE ZERO COMP-3.            
030900         05  W-IDKUNDNR-B2       PIC S9(7)  VALUE ZERO COMP-3.            
031000                                                                          
031100     03  W-WDB101KY-X.                                                    
031200         05  W-IDPARTNR-B1       PIC X(9)   VALUE SPACE.                  
031300         05  W-IDFTG-B1          PIC 9(2)   VALUE ZERO.                   
031400                                                                          
031500     03  W-IDPRODNR-E6-X.                                                 
031600         05  W-IDPRODNR-E6       PIC S9(7)  VALUE ZERO COMP-3.            
031700                                                                          
031800     03  W-IDPRODNR2             PIC S9(7)  VALUE ZERO COMP-3.            
031900     03  W-IDORDER-X.                                                     
032000         05  W-IDORDER           PIC S9(7)  VALUE ZERO COMP-3.            
032100                                                                          
032200     SKIP2                                                                
032300*    --- STATUS-KOD FRÅN IMS                                              
032400 01  STATUS-WS                   PIC XX.                                  
032500     88  STATUS-OK                           VALUE '  '.                  
032600     88  SEGMENT-FOUND                       VALUE '  '.                  
032700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
032800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
032900     88  END-OF-DB                           VALUE 'GB'.                  
033000     88  SECURITY-ERROR                      VALUE 'A4'.                  
033100     SKIP2                                                                
033200 01  W-STATUS-WDQ212            PIC X VALUE SPACE.                        
033300 01  GOOD-STATUSCODES.                                                    
033400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
033500     SKIP3                                                                
033600 01  SSA1                        PIC X(256).                              
033700 01  SSA2                        PIC X(256).                              
033800     EJECT                                                                
033900*    --- IMS FUNCTION CODES                                               
034000*01  -COPY W0003                                                          
034100     EJECT                                                                
034200*    ---  DLI INPUT-OUTPUT AREA                                           
034300                                                                          
034400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
034500 01  DLI-IO-WDQ201.                                                       
034600*    03  -COPY WDQ201                                                     
034700     EJECT                                                                
034800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ2D1'.                      
034900 01  DLI-IO-WDQ2D1.                                                       
035000*    03  -COPY WDQ2D1                                                     
035100     EJECT                                                                
035200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ2F1'.                      
035300 01  DLI-IO-WDQ2F1.                                                       
035400*    03  -COPY WDQ2F1                                                     
035500     EJECT                                                                
035600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ212'.                      
035700 01  DLI-IO-WDQ212.                                                       
035800*    03  -COPY WDQ212                                                     
035900     EJECT                                                                
036000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ301'.                      
036100 01  DLI-IO-WDQ301.                                                       
036200*    03  -COPY WDQ301                                                     
036300     EJECT                                                                
036400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ3G1'.                      
036500 01  DLI-IO-WDQ3G1.                                                       
036600*    03  -COPY WDQ3G1                                                     
036700     EJECT                                                                
036800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE801'.                      
036900 01  DLI-IO-WDE801.                                                       
037000*    03  -COPY WDE801                                                     
037100     EJECT                                                                
037200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
037300 01  DLI-IO-WDB601.                                                       
037400*    03  -COPY WDB601                                                     
037500     EJECT                                                                
037600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
037700 01  DLI-IO-WDB201.                                                       
037800*    03  -COPY WDB201                                                     
037900     EJECT                                                                
038000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
038100 01  DLI-IO-WDB101.                                                       
038200*    03  -COPY WDB101                                                     
038300     EJECT                                                                
038400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
038500 01  DLI-IO-WDE601.                                                       
038600*    03  -COPY WDE601                                                     
038700     EJECT                                                                
038800 LINKAGE SECTION.                                                         
038900*01  -COPY W0009  -PRE MSG-                                               
039000*01  -COPY W0009  -PRE ALT-                                               
039100*01  -COPY W0008  -PRE WDP7-                                              
039200     05  FILLER                  PIC X.                                   
039300                                                                          
039400*01  -COPY W0008  -PRE WDQ2D-                                             
039500     05  FILLER                  PIC X.                                   
039600                                                                          
039700*01  -COPY W0008  -PRE WDQ3GSEQ-                                          
039800     05  FILLER                  PIC X.                                   
039900                                                                          
040000*01  -COPY W0008  -PRE WDQ2-                                              
040100     05  FILLER                  PIC X.                                   
040200                                                                          
040300*01  -COPY W0008  -PRE WDQ3-                                              
040400     05  FILLER                  PIC X.                                   
040500                                                                          
040600*01  -COPY W0008  -PRE WDE8-                                              
040700     05  FILLER                  PIC X.                                   
040800                                                                          
040900*01  -COPY W0008  -PRE WDB6-                                              
041000     05  FILLER                  PIC X.                                   
041100                                                                          
041200*01  -COPY W0008  -PRE WDB2-                                              
041300     05  FILLER                  PIC X.                                   
041400                                                                          
041500*01  -COPY W0008  -PRE WDB1-                                              
041600     05  FILLER                  PIC X.                                   
041700                                                                          
041800*01  -COPY W0008  -PRE WDG2-                                              
041900     05  FILLER                  PIC X.                                   
042000                                                                          
042100*01  -COPY W0008  -PRE WDE6-                                              
042200     05  FILLER                  PIC X.                                   
042300                                                                          
042400*01  -COPY W0008  -PRE WDQ2F-                                             
042500     05  FILLER                  PIC X.                                   
042600                                                                          
042700     EJECT                                                                
042800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDQ3GSEQ-PCB          
042900                WDQ2D-PCB WDQ3-PCB WDE8-PCB                               
043000                WDB6-PCB WDB2-PCB WDB1-PCB WDG2-PCB WDE6-PCB              
043100                WDQ2-PCB WDQ2F-PCB.                                       
043200 MAIN SECTION.                                                            
043300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDQ3GSEQ-PCB          
043400                WDQ2D-PCB WDQ3-PCB WDE8-PCB                               
043500                WDB6-PCB WDB2-PCB WDB1-PCB WDG2-PCB WDE6-PCB              
043600                WDQ2-PCB WDQ2F-PCB.                                       
043700                                                                          
043800     PERFORM IMS-GET-MSG                                                  
043900     IF SEGMENT-FOUND                                                     
044000        PERFORM A-INIT                                                    
044100        PERFORM B-CHECK-KEYS                                              
044200        IF KEYS-OK                                                        
044300           IF MFS-FIRST                                                   
044400              PERFORM C-FIRST-PAGE                                        
044500           ELSE                                                           
044600              IF MFS-NEXT                                                 
044700                 PERFORM D-NEXT-PAGE                                      
044800              ELSE                                                        
044900                 IF MFS-PREVIOUS                                          
045000                    PERFORM I-PREVIOUS-PAGE                               
045100                 ELSE                                                     
045200                    PERFORM E-SAME-PAGE                                   
045300                 END-IF                                                   
045400              END-IF                                                      
045500           END-IF                                                         
045600           IF START-OTHER-SCREEN                                          
045700              CONTINUE                                                    
045800           ELSE                                                           
045900              IF ALL-OK                                                   
046000                 IF SAVE-FLPROF = JA OR YES                               
046100                    PERFORM G-READ-SHOW-PROFORMA                          
046200                 ELSE                                                     
046300                    PERFORM F-READ-SHOW-INFO                              
046400                 END-IF                                                   
046500              END-IF                                                      
046600              PERFORM S03-CHECK-WSECURIT                                  
046700           END-IF                                                         
046800        END-IF                                                            
046900        IF START-OTHER-SCREEN                                             
047000           CONTINUE                                                       
047100        ELSE                                                              
047200           COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51101 + 4                  
047300           PERFORM IMS-INSERT-MSG                                         
047400        END-IF                                                            
047500     END-IF                                                               
047600                                                                          
047700     MOVE ZERO TO RETURN-CODE                                             
047800     GOBACK                                                               
047900     .                                                                    
048000     EJECT                                                                
048100 A-INIT SECTION.                                                          
048200                                                                          
048300     IF MSG-DOUBLE-TRANSACTIONS                                           
048400        MOVE MSG-INDATA-MINUS-2-TRANSACT TO MID-W4I51101                  
048500        MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                
048600        MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                               
048700     ELSE                                                                 
048800        MOVE MSG-INDATA-MINUS-1-TRANSACT TO MID-W4I51101                  
048900        MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                
049000        MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                               
049100     END-IF                                                               
049200                                                                          
049300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
049400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
049500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
049600                                                                          
049700     MOVE LOW-VALUE  TO MSG-AREA                                          
049800     MOVE 'W4O511N1' TO MFS-IDMOD                                         
049900     MOVE '4511'     TO MOD-IDTRANS                                       
050000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
050100                                                                          
050200     IF OWN-MID OR HELP-MID                                               
050300        CONTINUE                                                          
050400     ELSE                                                                 
050500        MOVE SPACE TO MFS-KDTRTYP                                         
050600        MOVE '7'   TO MFS-IDPFK                                           
050700     END-IF                                                               
050800                                                                          
050900     MOVE LOW-VALUE  TO W-WDQ3GSEQ-MIN-X                                  
051000                        W-WDQ2D1KY-MIN-X                                  
051100                        W-WDQ2F1KY-MIN-X                                  
051200                        W-WDQ301KY-MIN-X                                  
051300                        W-WDE801KY-MIN-X                                  
051400     MOVE HIGH-VALUE TO W-WDQ3GSEQ-MAX-X                                  
051500                        W-WDQ2D1KY-MAX-X                                  
051600                        W-WDQ2F1KY-MAX-X                                  
051700                        W-WDQ301KY-MAX-X                                  
051800                        W-WDE801KY-MAX-X                                  
051900     MOVE JA TO ALL-SW                                                    
052000     MOVE ZERO TO SEQG-IDPRODNR                                           
052100     .                                                                    
052200     EJECT                                                                
052300                                                                          
052400 B-CHECK-KEYS SECTION.                                                    
052500                                                                          
052600     MOVE ALL '+'            TO MSGI-WMSGINIT                             
052700     MOVE '001'              TO MSGI-KDCALL                               
052800     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
052900     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
053000     MOVE '4511'             TO MSGI-IDTRANS                              
053100     IF OWN-MID                                                           
053200        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
053300        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
053400        IF MID-IDKUNDNR-IN = SPACE                                        
053500           MOVE SPACE TO MID-IDDC-IN                                      
053600        END-IF                                                            
053700        MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                             
053800     ELSE                                                                 
053900        MOVE SPACE           TO MSGI-IDDC-KEY                             
054000     END-IF                                                               
054100                                                                          
054200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
054300                                                                          
054400     MOVE MSGI-SPAR-AREA   TO SAVE-AREA                                   
054500                                                                          
054600                                                                          
054700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
054800                                                                          
054900     IF GOOD-MID OR HELP-MID                                              
055000       MOVE JA TO KEYS-SW                                                 
055100     ELSE                                                                 
055200       MOVE NOO TO KEYS-SW                                                
055300     END-IF                                                               
055400                                                                          
055500*    -- CHECK IDDISTR                                                     
055600     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
055700                                                                          
055800     IF MID-IDDISTR-IN NOT = ALL '+'                                      
055900        MOVE '7'   TO MFS-IDPFK                                           
056000        MOVE SPACE TO MFS-KDTRTYP                                         
056100     END-IF                                                               
056200                                                                          
056300     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
056400     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
056500        MOVE MSGI-IDDISTR       TO W-IDDISTR-GSEQ-MIN                     
056600                                   W-IDDISTR-GSEQ-MAX                     
056700                                   W-IDDISTR-F1KY-MIN                     
056800                                   W-IDDISTR-F1KY-MAX                     
056900                                   W-IDDISTR-D1KY-MIN                     
057000                                   W-IDDISTR-D1KY-MAX                     
057100                                   W-IDDISTR-E8-MIN                       
057200                                   W-IDDISTR-E8-MAX                       
057300                                   W-IDDISTR-B2                           
057400                                   TEST-IDDISTR                           
057500     ELSE                                                                 
057600        MOVE NOO TO KEYS-SW                                               
057700     END-IF                                                               
057800                                                                          
057900     MOVE MSGI-IDDISTR TO MOD-IDDISTR-UT                                  
058000     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
058100                                                                          
058200     MOVE SPACE                 TO WS-KDVALISO                            
058300                                                                          
058400*    -- CHECK IDKUNDNR                                                    
058500     MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR-IN                              
058600                                                                          
058700     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
058800        MOVE '7'   TO MFS-IDPFK                                           
058900        MOVE SPACE TO MFS-KDTRTYP                                         
059000     END-IF                                                               
059100                                                                          
059200     IF MSGI-IDKUNDNR NOT = ALL '+' AND                                   
059300        MSGI-IDKUNDNR NOT NUMERIC                                         
059400       MOVE ALL '0'              TO W-IDKUNDNR-GSEQ-MIN                   
059500                                    W-IDKUNDNR-F1KY-MIN                   
059600                                    W-IDKUNDNR-D1KY-MIN                   
059700                                    W-IDKUNDNR-E8-MIN                     
059800       MOVE ALL '9'              TO W-IDKUNDNR-GSEQ-MAX                   
059900                                    W-IDKUNDNR-F1KY-MAX                   
060000                                    W-IDKUNDNR-D1KY-MAX                   
060100                                    W-IDKUNDNR-E8-MAX                     
060200                                                                          
060300     ELSE                                                                 
060400        INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO             
060500        IF MSGI-IDKUNDNR NUMERIC                                          
060600          IF MSGI-IDKUNDNR = ZERO                                         
060700            MOVE ALL '0'         TO W-IDKUNDNR-GSEQ-MIN                   
060800                                    W-IDKUNDNR-F1KY-MIN                   
060900                                    W-IDKUNDNR-D1KY-MIN                   
061000                                    W-IDKUNDNR-E8-MIN                     
061100            MOVE ALL '9'         TO W-IDKUNDNR-GSEQ-MAX                   
061200                                    W-IDKUNDNR-F1KY-MAX                   
061300                                    W-IDKUNDNR-D1KY-MAX                   
061400                                    W-IDKUNDNR-E8-MAX                     
061500          ELSE                                                            
061600            MOVE MSGI-IDKUNDNR   TO W-IDKUNDNR-GSEQ-MIN                   
061700                                    W-IDKUNDNR-GSEQ-MAX                   
061800                                    W-IDKUNDNR-F1KY-MIN                   
061900                                    W-IDKUNDNR-F1KY-MAX                   
062000                                    W-IDKUNDNR-D1KY-MIN                   
062100                                    W-IDKUNDNR-D1KY-MAX                   
062200                                    W-IDKUNDNR-E8-MIN                     
062300                                    W-IDKUNDNR-E8-MAX                     
062400                                    W-IDKUNDNR-B2                         
062500          END-IF                                                          
062600        ELSE                                                              
062700          MOVE NOO TO KEYS-SW                                             
062800        END-IF                                                            
062900     END-IF                                                               
063000                                                                          
063100     MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                                
063200     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
063300                                                                          
063400     IF MSGI-IDKUNDNR = ZERO                                              
063500        MOVE '     0' TO MOD-IDKUNDNR-UT                                  
063600     END-IF                                                               
063700                                                                          
063800*    -- CHECK IDDC                                                        
063900     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
064000                                                                          
064100     IF MID-IDDC-IN NOT = ALL '+'                                         
064200        MOVE '7'              TO MFS-IDPFK                                
064300        MOVE SPACE            TO MFS-KDTRTYP                              
064400                                                                          
064500        IF MID-IDDC-IN > SPACE                                            
064600           MOVE MSGI-IDDC-KEY TO W-IDDC-Q3-MIN                            
064700                                 W-IDDC-Q3-MAX                            
064800                                 W-IDDC-GSEQ-MAX                          
064900                                 W-IDDC-GSEQ-MIN                          
065000                                 W-IDDC-B6                                
065100                                 W-IDDC-WDQ3G1                            
065200                                 WS-IDDC                                  
065300           PERFORM BA-CHECK-PROBABILITY                                   
065400        ELSE                                                              
065500           MOVE LOW-VALUE  TO W-IDDC-Q3-MIN                               
065600           MOVE HIGH-VALUE TO W-IDDC-Q3-MAX                               
065700           MOVE LOW-VALUE  TO W-IDDC-GSEQ-MIN                             
065800           MOVE HIGH-VALUE TO W-IDDC-GSEQ-MAX                             
065900        END-IF                                                            
066000     ELSE                                                                 
066100        IF MSGI-IDDC-KEY > SPACE                                          
066200           MOVE MSGI-IDDC-KEY  TO W-IDDC-Q3-MIN                           
066300                                  W-IDDC-Q3-MAX                           
066400                                  W-IDDC-GSEQ-MAX                         
066500                                  W-IDDC-GSEQ-MIN                         
066600                                  W-IDDC-B6                               
066700                                  W-IDDC-WDQ3G1                           
066800                                  WS-IDDC                                 
066900           PERFORM BA-CHECK-PROBABILITY                                   
067000        ELSE                                                              
067100          IF OWN-MID                                                      
067200             MOVE LOW-VALUE  TO W-IDDC-Q3-MIN                             
067300             MOVE HIGH-VALUE TO W-IDDC-Q3-MAX                             
067400             MOVE LOW-VALUE  TO W-IDDC-GSEQ-MIN                           
067500             MOVE HIGH-VALUE TO W-IDDC-GSEQ-MAX                           
067600          ELSE                                                            
067700             MOVE MSGI-IDDC      TO W-IDDC-Q3-MIN                         
067800                                    W-IDDC-Q3-MAX                         
067900                                    W-IDDC-GSEQ-MAX                       
068000                                    W-IDDC-GSEQ-MIN                       
068100                                    W-IDDC-B6                             
068200                                    W-IDDC-WDQ3G1                         
068300                                    MSGI-IDDC-KEY                         
068400             PERFORM BA-CHECK-PROBABILITY                                 
068500* ANROPA INT FÖR ATT STÄLLA OM MSGI-IDDC-KEY                              
068600             CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                   
068700          END-IF                                                          
068800        END-IF                                                            
068900     END-IF                                                               
069000     MOVE MSGI-IDDC-KEY TO MOD-IDDC-UT                                    
069100     IF MID-IDDC-IN = SPACE                                               
069200        MOVE SPACE TO MOD-IDDC-UT                                         
069300     END-IF                                                               
069400                                                                          
069500     PERFORM BB-CHECK-PROFORMA                                            
069600     PERFORM BC-CHECK-DATESORT                                            
069700     PERFORM BD-CHECK-SOFTWARE                                            
069800                                                                          
069900     IF KEYS-WRONG                                                        
070000        IF MED-IDMFSFEL = SPACE                                           
070100        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
070200        END-IF                                                            
070300        CALL WMEDKONV USING MED-WMEDAREA                                  
070400        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
070500        PERFORM MFS-ERASE-FIELD-IN                                        
070600        PERFORM MFS-ERASE-FIELD-OUT                                       
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000 BA-CHECK-PROBABILITY SECTION.                                            
071100                                                                          
071200     PERFORM IMS-GU-WDB601                                                
071300     IF SEGMENT-MISSING                                                   
071400        MOVE SPACE TO DCS-KDDC                                            
071500        MOVE ERR-WRONG-DC TO MED-IDMFSFEL                                 
071600        MOVE NOO   TO KEYS-SW                                             
071700     END-IF                                                               
071800                                                                          
071900     IF DCS-DDC                                                           
072000        CONTINUE                                                          
072100     ELSE                                                                 
072200        IF DCS-SDC                                                        
072300           IF NOT DIST18-SKROT                                            
072400              IF (DIST40-SDC-21 AND NOT                                   
072500                 (DCS-SDC AND DCS-IDLANDX2 = 'NL'))                       
072600                                                                          
072700              OR (DIST40-SDC-23 AND NOT                                   
072800                 (DCS-SDC AND DCS-IDLANDX2 = 'GB'))                       
072900                                                                          
073000              OR (DIST40-LDC-3A AND NOT                                   
073100                 (DCS-SDC AND DCS-IDLANDX2 = 'GB'))                       
073200                                                                          
073300              OR (DIST40-SDC-24 AND NOT                                   
073400                 (DCS-SDC AND DCS-IDLANDX2 = 'ES'))                       
073500                                                                          
073600              OR (DIST40-SDC-25 AND NOT                                   
073700                 (DCS-SDC AND DCS-IDLANDX2 = 'IT'))                       
073800                                                                          
073900              OR (DIST40-SDC-26 AND NOT                                   
074000                 (DCS-SDC AND DCS-IDLANDX2 = 'AT'))                       
074100                                                                          
074200              OR DIST40-NDC-NA                                            
074300              OR DIST40-NDC-PACIFIC                                       
074400              MOVE NOO TO KEYS-SW                                         
074500              END-IF                                                      
074600           END-IF                                                         
074700        ELSE                                                              
074800           IF DCS-NDC-NA OR DCS-NDC-PF OR                                 
074900              DCS-NDC-OTHERS OR DCS-NDC-SA                                
075000              IF NOT DIST18-SKROT                                         
075100                 IF (DIST40-NDC-USA AND NOT                               
075200                    (DCS-NDC-NA AND DCS-IDLANDX2 = 'US'))                 
075300                                                                          
075400                 OR (DIST40-NDC-CAN AND NOT                               
075500                    (DCS-NDC-NA AND DCS-IDLANDX2 = 'CA'))                 
075600                                                                          
075700                 OR (DIST40-NDC-JAP AND NOT                               
075800                    (DCS-NDC-PF AND DCS-IDLANDX2 = 'JP'))                 
075900                                                                          
076000                 OR (DIST40-NDC-AU AND NOT                                
076100                    (DCS-NDC-PF AND DCS-IDLANDX2 = 'AU'))                 
076200                                                                          
076300                 OR (DIST40-NDC-IN AND NOT                                
076400                    (DCS-NDC-PF AND DCS-IDLANDX2 = 'IN'))                 
076500                                                                          
076600                 OR (DIST40-NDC-KR AND NOT                                
076700                    (DCS-NDC-PF AND DCS-IDLANDX2 = 'KR'))                 
076800                                                                          
076900                 OR (DIST40-NDC-MY AND NOT                                
077000                    (DCS-NDC-PF AND DCS-IDLANDX2 = 'MY'))                 
077100                                                                          
077200                 OR (DIST40-NDC-TR AND NOT                                
077300                    (DCS-NDC-OTHERS AND DCS-IDLANDX2 = 'TR'))             
077400                                                                          
077500                 OR (DIST40-NDC-TH AND NOT                                
077600                    (DCS-NDC-PF AND DCS-IDLANDX2 = 'TH'))                 
077700                                                                          
077800                 OR (DIST40-NDC-TW AND NOT                                
077900                    (DCS-NDC-PF AND DCS-IDLANDX2 = 'TW'))                 
078000                                                                          
078100                 OR (DIST40-NDC-MX AND NOT                                
078200                    (DCS-NDC-SA AND DCS-IDLANDX2 = 'MX'))                 
078300                                                                          
078400                 OR (DIST40-NDC-BR AND NOT                                
078500                    (DCS-NDC-SA AND DCS-IDLANDX2 = 'BR'))                 
078600                                                                          
078610                 OR (DIST40-NDC-ZA AND NOT                                
078620                    (DCS-NDC-OTHERS AND DCS-IDLANDX2 = 'ZA'))             
078630                                                                          
078700                 OR DIST40-SDC                                            
078800                 MOVE NOO TO KEYS-SW                                      
078900                 END-IF                                                   
079000              END-IF                                                      
079100           ELSE                                                           
079200              CONTINUE                                                    
079300           END-IF                                                         
079400        END-IF                                                            
079500     END-IF                                                               
079600     .                                                                    
079700     EJECT                                                                
079800 BB-CHECK-PROFORMA SECTION.                                               
079900                                                                          
080000     MOVE MFS-ERASE-FIELD TO MOD-FLPROF                                   
080100      IF MID-FLPROF = ALL '+'                                             
080200         IF MFS-FIRST AND SAVE-IDTRANS NOT = '4511'                       
080300            MOVE NOO TO SAVE-FLPROF                                       
080400         END-IF                                                           
080500         IF MID-IDDISTR-IN NOT = ALL '+' OR                               
080600            MID-IDKUNDNR-IN NOT = ALL '+'                                 
080700          OR MID-IDDC-IN NOT = ALL '+'                                    
080800            MOVE NOO TO SAVE-FLPROF                                       
080900         END-IF                                                           
081000      ELSE                                                                
081100         IF MID-FLPROF = JA                                               
081200            MOVE JA TO SAVE-FLPROF                                        
081300         ELSE                                                             
081400            IF MID-FLPROF = YES                                           
081500               MOVE YES TO SAVE-FLPROF                                    
081600            ELSE                                                          
081700               MOVE NOO TO SAVE-FLPROF                                    
081800            END-IF                                                        
081900         END-IF                                                           
082000         MOVE '7'   TO MFS-IDPFK                                          
082100         MOVE SPACE TO MFS-KDTRTYP                                        
082200      END-IF                                                              
082300      MOVE SPACE       TO MOD-FLPROF                                      
082400      MOVE SAVE-FLPROF TO MOD-FLPROF-UT                                   
082500     .                                                                    
082600     EJECT                                                                
082700 BC-CHECK-DATESORT SECTION.                                               
082800                                                                          
082900     MOVE MFS-ERASE-FIELD TO MOD-FLSORT                                   
083000      IF MID-FLSORT = ALL '+'                                             
083100         IF MFS-FIRST AND SAVE-IDTRANS NOT = '4511'                       
083200            MOVE NOO TO SAVE-FLSORT                                       
083300         END-IF                                                           
083400         IF MID-IDDISTR-IN NOT = ALL '+'                                  
083500         OR MID-IDKUNDNR-IN NOT = ALL '+'                                 
083600          OR MID-IDDC-IN NOT = ALL '+'                                    
083700            MOVE NOO TO SAVE-FLSORT                                       
083800         END-IF                                                           
083900      ELSE                                                                
084000         IF MID-FLSORT = JA                                               
084100            MOVE JA TO SAVE-FLSORT                                        
084200         ELSE                                                             
084300            IF MID-FLSORT = YES                                           
084400               MOVE YES TO SAVE-FLSORT                                    
084500            ELSE                                                          
084600               MOVE NOO TO SAVE-FLSORT                                    
084700            END-IF                                                        
084800         END-IF                                                           
084900         MOVE '7'   TO MFS-IDPFK                                          
085000         MOVE SPACE TO MFS-KDTRTYP                                        
085100      END-IF                                                              
085200      MOVE SPACE       TO MOD-FLSORT                                      
085300      MOVE SAVE-FLSORT TO MOD-FLSORT-UT                                   
085400     .                                                                    
085500     EJECT                                                                
085600 BD-CHECK-SOFTWARE SECTION.                                               
085700                                                                          
085800      MOVE 'N' TO W-FLSOFT-F1KY-MIN                                       
085900      MOVE 'N' TO W-FLSOFT-F1KY-MAX                                       
086000      MOVE 'N' TO W-FLSOFT-D1KY-MIN                                       
086100      MOVE 'N' TO W-FLSOFT-D1KY-MAX                                       
086200      MOVE MFS-ERASE-FIELD TO MOD-FLSOFT                                  
086300      IF MID-FLSOFT = ALL '+'                                             
086400         IF MFS-FIRST AND SAVE-IDTRANS NOT = '4511'                       
086500            MOVE NOO TO SAVE-FLSOFT                                       
086600         END-IF                                                           
086700         IF MID-IDDISTR-IN NOT = ALL '+'                                  
086800         OR MID-IDKUNDNR-IN NOT = ALL '+'                                 
086900          OR MID-IDDC-IN NOT = ALL '+'                                    
087000            MOVE NOO TO SAVE-FLSOFT                                       
087100         END-IF                                                           
087200         IF SAVE-FLSOFT = JA                                              
087300            MOVE 'J' TO W-FLSOFT-D1KY-MIN                                 
087400            MOVE 'J' TO W-FLSOFT-D1KY-MAX                                 
087500            MOVE 'J' TO W-FLSOFT-F1KY-MIN                                 
087600            MOVE 'J' TO W-FLSOFT-F1KY-MAX                                 
087700         END-IF                                                           
087800         IF SAVE-FLSOFT = YES                                             
087900            MOVE 'J' TO W-FLSOFT-D1KY-MIN                                 
088000            MOVE 'J' TO W-FLSOFT-D1KY-MAX                                 
088100            MOVE 'J' TO W-FLSOFT-F1KY-MIN                                 
088200            MOVE 'J' TO W-FLSOFT-F1KY-MAX                                 
088300         END-IF                                                           
088400      ELSE                                                                
088500         IF MID-FLSOFT = JA                                               
088600            MOVE JA TO SAVE-FLSOFT                                        
088700            MOVE 'J' TO W-FLSOFT-D1KY-MIN                                 
088800            MOVE 'J' TO W-FLSOFT-D1KY-MAX                                 
088900            MOVE 'J' TO W-FLSOFT-F1KY-MIN                                 
089000            MOVE 'J' TO W-FLSOFT-F1KY-MAX                                 
089100         ELSE                                                             
089200            IF MID-FLSOFT = YES                                           
089300               MOVE YES TO SAVE-FLSOFT                                    
089400               MOVE 'J' TO W-FLSOFT-D1KY-MIN                              
089500               MOVE 'J' TO W-FLSOFT-D1KY-MAX                              
089600               MOVE 'J' TO W-FLSOFT-F1KY-MIN                              
089700               MOVE 'J' TO W-FLSOFT-F1KY-MAX                              
089800            ELSE                                                          
089900               MOVE NOO TO SAVE-FLSOFT                                    
090000           END-IF                                                         
090100         END-IF                                                           
090200         MOVE '7'   TO MFS-IDPFK                                          
090300         MOVE SPACE TO MFS-KDTRTYP                                        
090400      END-IF                                                              
090500      MOVE SPACE       TO MOD-FLSOFT                                      
090600      MOVE SAVE-FLSOFT TO MOD-FLSOFT-UT                                   
090700     .                                                                    
090800     EJECT                                                                
090900 C-FIRST-PAGE SECTION.                                                    
091000                                                                          
091100     MOVE INF-FIRST-PAGE         TO MED-IDMFSFEL                          
091200     CALL WMEDKONV            USING MED-WMEDAREA                          
091300     MOVE MED-TEMFSFEL           TO MOD-TEMFSFEL                          
091400                                                                          
091500     INITIALIZE SAVE-AREA-KEYS                                            
091600     MOVE 1                      TO PGNO                                  
091700     MOVE JA                     TO FIRST-SW                              
091800     PERFORM MFS-ERASE-FIELD-IN                                           
091900     .                                                                    
092000     EJECT                                                                
092100 D-NEXT-PAGE SECTION.                                                     
092200                                                                          
092300     IF SAVE-IDTRANS = '4511'                                             
092400       IF SAVE-IDKUNDNR-NEXT > ZERO                                       
092500       OR SAVE-IDPRODNR-NEXT > ZERO                                       
092600       OR SAVE-IDKUNDRF-NEXT > SPACE                                      
092700       OR SAVE-IDDC-NEXT     > SPACE                                      
092800       OR SAVE-IDORDER-NEXT  > ZERO                                       
092900         IF SAVE-IDKUNDNR-NEXT > ZERO                                     
093000           MOVE SAVE-IDKUNDNR-NEXT                                        
093100                                 TO W-IDKUNDNR-GSEQ-MIN                   
093200                                    W-IDKUNDNR-F1KY-MIN                   
093300                                    W-IDKUNDNR-D1KY-MIN                   
093400         END-IF                                                           
093500         IF SAVE-IDPRODNR-NEXT > ZERO                                     
093600           MOVE SAVE-IDPRODNR-NEXT                                        
093700                                 TO W-IDPRODNR-Q3-MIN                     
093800         END-IF                                                           
093900         IF SAVE-IDKUNDRF-NEXT > SPACE                                    
094000           MOVE SAVE-IDKUNDRF-NEXT                                        
094100                                 TO W-IDKUNDRF-GSEQ-MIN                   
094200                                    W-IDKUNDRF-E8-MIN                     
094300         END-IF                                                           
094400         IF SAVE-IDDC-NEXT     > SPACE                                    
094500           MOVE SAVE-IDDC-NEXT   TO W-IDDC-GSEQ-MIN                       
094600                                    W-IDDC-Q3-MIN                         
094700         END-IF                                                           
094800         IF SAVE-IDORDER-NEXT  > ZERO                                     
094900           MOVE SAVE-IDORDER-NEXT                                         
095000                                 TO W-IDORDER                             
095100         END-IF                                                           
095200         IF (  SAVE-IDKUNDNR-PREV (PGNO) NUMERIC                          
095210           AND SAVE-IDKUNDNR-PREV (PGNO) NOT = SAVE-IDKUNDNR-NEXT)        
095300         OR (SAVE-IDKUNDRF-PREV (PGNO) NOT = SAVE-IDKUNDRF-NEXT)          
095400         OR (  SAVE-IDPRODNR-PREV (PGNO) NUMERIC                          
095410           AND SAVE-IDPRODNR-PREV (PGNO) NOT = SAVE-IDPRODNR-NEXT)        
095500*        OR (SAVE-IDDC-PREV     (PGNO) NOT = SAVE-IDDC-NEXT    )          
095600*        OR (SAVE-IDORDER-PREV  (PGNO) NOT = SAVE-IDORDER-NEXT )          
095700           IF PGNO = 20                                                   
095800             PERFORM VARYING PGNO FROM 1 BY 1                             
095900               UNTIL PGNO = 20                                            
096000               MOVE SAVE-IDKUNDNR-PREV(PGNO + 1)                          
096100                                 TO SAVE-IDKUNDNR-PREV(PGNO)              
096200               MOVE SAVE-IDPRODNR-PREV(PGNO + 1)                          
096300                                 TO SAVE-IDPRODNR-PREV(PGNO)              
096400               MOVE SAVE-IDKUNDRF-PREV(PGNO + 1)                          
096500                                 TO SAVE-IDKUNDRF-PREV(PGNO)              
096600               MOVE SAVE-IDDC-PREV    (PGNO + 1)                          
096700                                 TO SAVE-IDDC-PREV    (PGNO)              
096800               MOVE SAVE-IDORDER-PREV (PGNO + 1)                          
096900                                 TO SAVE-IDORDER-PREV (PGNO)              
097000                                                                          
097100             END-PERFORM                                                  
097200             MOVE NOO            TO FIRST-SW                              
097300           ELSE                                                           
097400             ADD 1               TO PGNO                                  
097500           END-IF                                                         
097600         END-IF                                                           
097700       ELSE                                                               
097800         MOVE INF-LAST-PAGE      TO MED-IDMFSINF                          
097900         CALL WMEDKONV        USING MED-WMEDAREA                          
098000         MOVE MED-MFSINF         TO MOD-TEMFSINF                          
098100         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
098200         MOVE NOO                TO ALL-SW                                
098300       END-IF                                                             
098400     END-IF                                                               
098500     .                                                                    
098600     EJECT                                                                
098700 E-SAME-PAGE SECTION.                                                     
098800                                                                          
098900     IF OWN-MID OR HELP-MID                                               
099000       IF SAVE-IDTRANS = '4511'                                           
099010       AND PGNO                      NUMERIC                              
099011       AND SAVE-IDKUNDNR-PREV (PGNO) NUMERIC                              
099020       AND SAVE-IDPRODNR-PREV (PGNO) NUMERIC                              
099030       AND SAVE-IDORDER-PREV (PGNO)  NUMERIC                              
099100         IF SAVE-IDKUNDNR-PREV (PGNO) > ZERO                              
099200           MOVE SAVE-IDKUNDNR-PREV (PGNO)                                 
099300                                 TO W-IDKUNDNR-GSEQ-MIN                   
099400                                    W-IDKUNDNR-F1KY-MIN                   
099500                                    W-IDKUNDNR-D1KY-MIN                   
099600         END-IF                                                           
099700         IF SAVE-IDPRODNR-PREV (PGNO) > ZERO                              
099800           MOVE SAVE-IDPRODNR-PREV (PGNO)                                 
099900                                 TO W-IDPRODNR-Q3-MIN                     
100000         END-IF                                                           
100100         IF SAVE-IDKUNDRF-PREV (PGNO) > SPACE                             
100200           MOVE SAVE-IDKUNDRF-PREV (PGNO)                                 
100300                                 TO W-IDKUNDRF-GSEQ-MIN                   
100400                                    W-IDKUNDRF-E8-MIN                     
100500         END-IF                                                           
100600         IF SAVE-IDDC-PREV (PGNO)     > SPACE                             
100700           MOVE SAVE-IDDC-PREV (PGNO)                                     
100800                                 TO W-IDDC-GSEQ-MIN                       
100900                                    W-IDDC-Q3-MIN                         
101000         END-IF                                                           
101100         IF SAVE-IDORDER-PREV (PGNO)  > ZERO                              
101200           MOVE SAVE-IDORDER-PREV (PGNO)                                  
101300                                 TO W-IDORDER                             
101400         END-IF                                                           
101500       ELSE                                                               
101600         INITIALIZE SAVE-AREA-KEYS                                        
101700         MOVE 1                  TO PGNO                                  
101800         MOVE JA                 TO FIRST-SW                              
101900       END-IF                                                             
102000                                                                          
102100       MOVE +1                   TO MOD-INDX                              
102200       PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                              
102300         IF MID-IDTRANS (MOD-INDX) = ALL '+'                              
102400           CONTINUE                                                       
102500         ELSE                                                             
102600           IF MID-IDTRANS (MOD-INDX) NUMERIC                              
102700             PERFORM EA-START-OTHER-SCREEN                                
102800             MOVE JA             TO START-OTHER-SCREEN-SW                 
102900             MOVE MAX-MOD-INDX TO MOD-INDX                                
103000           END-IF                                                         
103100         END-IF                                                           
103200         ADD +1                  TO MOD-INDX                              
103300       END-PERFORM                                                        
103400     ELSE                                                                 
103500       INITIALIZE SAVE-AREA-KEYS                                          
103600       MOVE 1                    TO PGNO                                  
103700       MOVE JA                   TO FIRST-SW                              
103800       PERFORM MFS-ERASE-FIELD-IN                                         
103900     END-IF                                                               
104000     .                                                                    
104100     EJECT                                                                
104200 EA-START-OTHER-SCREEN SECTION.                                           
104300                                                                          
104400     INSPECT MID-IDKUNDNR (MOD-INDX)                                      
104500                                REPLACING LEADING SPACE BY ZERO           
104600     MOVE MID-IDKUNDNR (MOD-INDX)  TO MSGI-IDKUNDNR                       
104700     INSPECT MID-IDORDNR7 (MOD-INDX)                                      
104800                                REPLACING LEADING SPACE BY ZERO           
104900     MOVE MID-IDORDNR7 (MOD-INDX)  TO MSGI-IDKUNDRF(1:7)                  
105000     INSPECT MID-IDPRODNR (MOD-INDX)                                      
105100                                REPLACING LEADING SPACE BY ZERO           
105200     MOVE MID-IDPRODNR (MOD-INDX) TO MSGI-IDPRODNR                        
105300     MOVE '001'                   TO MSGI-KDCALL                          
105400     MOVE MSG-SIGNON-USERID       TO MSGI-IDUSER                          
105500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
105600                                                                          
105700     MOVE LOW-VALUE               TO P-TO-P-KDZ1                          
105800     MOVE LOW-VALUE               TO P-TO-P-KDZ2                          
105900     MOVE MID-IDTRANS (MOD-INDX) (1:1)                                    
106000                                  TO W-SWITCH-IDTRANS-2                   
106100     MOVE MID-IDTRANS (MOD-INDX) (2:3)                                    
106200                                  TO W-SWITCH-IDTRANS-4-6                 
106300     MOVE W-SWITCH-IDTRANS        TO P-TO-P-KDTRANS                       
106400     MOVE '4511'                  TO P-TO-P-IDTRANS                       
106500     MOVE MFS-KDMFSFOR            TO P-TO-P-KDMFSFOR                      
106600                                                                          
106700     PERFORM S02-INSERT-ALTMSG                                            
106800     .                                                                    
106900     EJECT                                                                
107000 I-PREVIOUS-PAGE SECTION.                                                 
107100                                                                          
107200     IF SAVE-IDTRANS = '4511'                                             
107300       IF  PGNO NUMERIC                                                   
107310       AND PGNO > 1                                                       
107400         COMPUTE PGNO = PGNO - 1                                          
107500         IF  SAVE-IDKUNDNR-PREV (PGNO) NUMERIC                            
107510         AND SAVE-IDKUNDNR-PREV (PGNO) > ZERO                             
107600           MOVE SAVE-IDKUNDNR-PREV (PGNO)                                 
107700                                 TO W-IDKUNDNR-GSEQ-MIN                   
107800                                    W-IDKUNDNR-F1KY-MIN                   
107900                                    W-IDKUNDNR-D1KY-MIN                   
108000         END-IF                                                           
108100                                                                          
108200         IF  SAVE-IDPRODNR-PREV (PGNO) NUMERIC                            
108210         AND SAVE-IDPRODNR-PREV (PGNO) > ZERO                             
108300           MOVE SAVE-IDPRODNR-PREV (PGNO)                                 
108400                                 TO W-IDPRODNR-Q3-MIN                     
108500         END-IF                                                           
108600                                                                          
108700         IF SAVE-IDKUNDRF-PREV (PGNO) > SPACE                             
108800           MOVE SAVE-IDKUNDRF-PREV (PGNO)                                 
108900                                 TO W-IDKUNDRF-GSEQ-MIN                   
109000                                    W-IDKUNDRF-E8-MIN                     
109100         END-IF                                                           
109200                                                                          
109300         IF SAVE-IDDC-PREV (PGNO)     > SPACE                             
109400           MOVE SAVE-IDDC-PREV (PGNO)                                     
109500                                 TO W-IDDC-GSEQ-MIN                       
109600                                    W-IDDC-Q3-MIN                         
109700         END-IF                                                           
109800                                                                          
109900         IF  SAVE-IDORDER-PREV (PGNO)  NUMERIC                            
109910         AND SAVE-IDORDER-PREV (PGNO)  > ZERO                             
110000           MOVE SAVE-IDORDER-PREV (PGNO)                                  
110100                                 TO W-IDORDER                             
110200         END-IF                                                           
110300       ELSE                                                               
110301*        To initialize page number                                        
110310         MOVE 1                  TO PGNO                                  
110320*                                                                         
110400         IF  SAVE-IDKUNDNR-PREV (1) NUMERIC                               
110410         AND SAVE-IDKUNDNR-PREV (1) > ZERO                                
110500           MOVE SAVE-IDKUNDNR-PREV (1)                                    
110600                                 TO W-IDKUNDNR-GSEQ-MIN                   
110700                                    W-IDKUNDNR-F1KY-MIN                   
110800                                    W-IDKUNDNR-D1KY-MIN                   
110900         END-IF                                                           
111000                                                                          
111100         IF  SAVE-IDPRODNR-PREV (1) NUMERIC                               
111110         AND SAVE-IDPRODNR-PREV (1) > ZERO                                
111200           MOVE SAVE-IDPRODNR-PREV (1)                                    
111300                                 TO W-IDPRODNR-Q3-MIN                     
111400         END-IF                                                           
111500                                                                          
111600         IF SAVE-IDKUNDRF-PREV (1) > SPACE                                
111700           MOVE SAVE-IDKUNDRF-PREV (1)                                    
111800                                 TO W-IDKUNDRF-GSEQ-MIN                   
111900                                    W-IDKUNDRF-E8-MIN                     
112000         END-IF                                                           
112100                                                                          
112200         IF SAVE-IDDC-PREV (1)        > SPACE                             
112300           MOVE SAVE-IDDC-PREV (1)                                        
112400                                 TO W-IDDC-GSEQ-MIN                       
112500                                    W-IDDC-Q3-MIN                         
112600         END-IF                                                           
112700                                                                          
112800         IF  SAVE-IDORDER-PREV (1)  NUMERIC                               
112810         AND SAVE-IDORDER-PREV (1)  > ZERO                                
112900           MOVE SAVE-IDORDER-PREV (1)                                     
113000                                 TO W-IDORDER                             
113100         END-IF                                                           
113200                                                                          
113300         IF PGNO = 1                                                      
113400           IF FIRST-SW = JA                                               
113500             MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                          
113600             CALL WMEDKONV    USING MED-WMEDAREA                          
113700             MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                          
113800           ELSE                                                           
113900             MOVE INF-NO-MORE-PF6                                         
114000                                 TO MED-IDMFSFEL                          
114100             CALL WMEDKONV    USING MED-WMEDAREA                          
114200             MOVE MED-MFSFEL     TO MOD-TEMFSFEL                          
114300           END-IF                                                         
114400         END-IF                                                           
114500                                                                          
114600       END-IF                                                             
114610     ELSE                                                                 
114620       INITIALIZE SAVE-AREA-KEYS                                          
114630       MOVE 1                    TO PGNO                                  
114640       MOVE JA                   TO FIRST-SW                              
114700     END-IF                                                               
114800     .                                                                    
114900     EJECT                                                                
115000 F-READ-SHOW-INFO SECTION.                                                
115100                                                                          
115200     INITIALIZE SAVE-AREA-NEXT                                            
115300                                                                          
115400     IF SAVE-FLSORT = JA OR YES                                           
115500       IF MFS-ENTER OR MFS-NEXT OR MFS-PREVIOUS                           
115600         IF MSGI-IDKUNDNR NUMERIC AND MSGI-IDKUNDNR > 0                   
115700           PERFORM IMS-GU-WDQ201-DSEQ-UNIQUE                              
115800         ELSE                                                             
115900           PERFORM IMS-GU-WDQ201-FSEQ-UNIQUE                              
116000         END-IF                                                           
116100       ELSE                                                               
116200         IF MSGI-IDKUNDNR NUMERIC AND MSGI-IDKUNDNR > 0                   
116300           PERFORM IMS-GU-WDQ201-DSEQ                                     
116400         ELSE                                                             
116500           PERFORM IMS-GU-WDQ201-FSEQ                                     
116600         END-IF                                                           
116700       END-IF                                                             
116800     ELSE                                                                 
116900       PERFORM S04-READ-WDQ3G                                             
117000       MOVE SEQG-IDWDQ301                  TO W-WDQ301KY-MIN-X            
117100                                              W-WDQ301KY-MAX-X            
117200       PERFORM IMS-GU-WDQ301                                              
117300*1557961 REMOVE CURRENCY CODE.                                            
117400*      IF ODEL-IDDC-EXP = SPACE                                           
117500*        MOVE ODEL-KDVALISO                TO WS-ODEL-KDVALISO            
117600*        MOVE WS-TEDDI                     TO MOD-TEDDI                   
117700*      ELSE                                                               
117800*        IF ODEL-IDDC-EXP = WC-CDC-SE                                     
117900*          *BOUNCE DC = 11                                                
118000*          MOVE ODEL-KDVALISO              TO WS-ODEL-KDVALISO            
118100*          MOVE WS-TEDDI                   TO MOD-TEDDI                   
118200*        ELSE                                                             
118300*          *BOUNCE VOR                                                    
118400*          MOVE WS-SEK                     TO WS-ODEL-KDVALISO            
118500*          MOVE WS-TEDDI                   TO MOD-TEDDI                   
118600*        END-IF                                                           
118700*      END-IF                                                             
118800     END-IF                                                               
118900     IF SEGMENT-MISSING                                                   
119000       IF MFS-NEXT                                                        
119100         MOVE INF-LAST-PAGE                TO MED-IDMFSINF                
119200         CALL WMEDKONV                  USING MED-WMEDAREA                
119300         MOVE MED-MFSINF                   TO MOD-TEMFSINF                
119400       ELSE                                                               
119500         MOVE ERR-LINES-MISSING            TO MED-IDMFSFEL                
119600         CALL WMEDKONV                  USING MED-WMEDAREA                
119700         MOVE MED-MFSFEL                   TO MOD-TEMFSFEL                
119800       END-IF                                                             
119900     ELSE                                                                 
120000       MOVE +1                             TO MOD-INDX                    
120100       PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                              
120200                 AND W-IDPRODNR-E6 NOT = SEQG-IDPRODNR                    
120300         IF SEGMENT-FOUND                                                 
120400           IF (OHUV-FLSOFT = 'J' AND SAVE-FLSOFT = NOO)   OR              
120500              ((OHUV-FLSOFT  = 'N') AND (SAVE-FLSOFT = JA OR YES))        
120600             IF SAVE-FLSORT = JA OR YES                                   
120700               CONTINUE                                                   
120800             ELSE                                                         
120900               PERFORM S04-READ-WDQ3G                                     
121000             END-IF                                                       
121100           ELSE                                                           
121200             IF SAVE-FLSORT = JA OR YES                                   
121300               IF MSGI-IDKUNDNR NUMERIC AND MSGI-IDKUNDNR > 0             
121400                 MOVE SEQD-IDORDER           TO W-IDORDER-Q3-MIN          
121500                                                W-IDORDER-Q3-MAX          
121600               ELSE                                                       
121700                 MOVE SEQF-IDORDER           TO W-IDORDER-Q3-MIN          
121800                                                W-IDORDER-Q3-MAX          
121900               END-IF                                                     
122000               PERFORM IMS-GU-WDQ301                                      
122100               IF SEGMENT-FOUND                                           
122200                 IF MSGI-IDKUNDNR NUMERIC AND MSGI-IDKUNDNR > 0           
122300                   MOVE SEQD-IDORDER         TO W-IDORDER2                
122400                 ELSE                                                     
122500                   MOVE SEQF-IDORDER         TO W-IDORDER2                
122600                 END-IF                                                   
122700                 PERFORM IMS-GU-WDQ201                                    
122800*1557961 REMOVE CURRENCY CODE.                                            
122900*                IF ODEL-IDDC-EXP = SPACE                                 
123000*                  MOVE ODEL-KDVALISO      TO WS-ODEL-KDVALISO            
123100*                  MOVE WS-TEDDI           TO MOD-TEDDI                   
123200*                ELSE                                                     
123300*                  IF ODEL-IDDC-EXP = WC-CDC-SE                           
123400*                    *BOUNCE DC = 11                                      
123500*                    MOVE ODEL-KDVALISO    TO WS-ODEL-KDVALISO            
123600*                    MOVE WS-TEDDI         TO MOD-TEDDI                   
123700*                  ELSE                                                   
123800*                    *BOUNCE VOR                                          
123900*                    MOVE WS-SEK           TO WS-ODEL-KDVALISO            
124000*                    MOVE WS-TEDDI         TO MOD-TEDDI                   
124100*                  END-IF                                                 
124200*                END-IF                                                   
124300               END-IF                                                     
124400               PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                      
124500                                       OR SEGMENT-MISSING                 
124600                                       OR END-OF-DB                       
124700                 PERFORM FA-SHOW-LINES                                    
124800                 PERFORM IMS-GN-WDQ301                                    
124900                 IF SEGMENT-FOUND                                         
125000                   IF MSGI-IDKUNDNR NUMERIC AND MSGI-IDKUNDNR > 0         
125100                     MOVE SEQD-IDORDER       TO W-IDORDER2                
125200                   ELSE                                                   
125300                     MOVE SEQF-IDORDER       TO W-IDORDER2                
125400                   END-IF                                                 
125500                   PERFORM IMS-GU-WDQ201                                  
125600                 END-IF                                                   
125700               END-PERFORM                                                
125800               IF MOD-INDX > MAX-MOD-INDX                                 
125900                 IF SEGMENT-FOUND                                         
126000                   MOVE ODEL-IDDC          TO SAVE-IDDC-NEXT              
126100                   MOVE ODEL-IDPRODNR      TO SAVE-IDPRODNR-NEXT          
126200                 ELSE                                                     
126300                   MOVE SPACE              TO SAVE-IDDC-NEXT              
126400                   MOVE ZERO               TO SAVE-IDPRODNR-NEXT          
126500                   MOVE OHUV-IDKUNDNR      TO SAVE-IDKUNDNR-NEXT          
126600                   MOVE OHUV-IDKUNDRF      TO SAVE-IDKUNDRF-NEXT          
126700                   MOVE OHUV-IDORDER       TO SAVE-IDORDER-NEXT           
126800                 END-IF                                                   
126900               END-IF                                                     
127000               MOVE ZERO                   TO W-IDPRODNR-Q3-MIN           
127100                                              W-IDPLKLST-Q3-MIN           
127200               MOVE MSGI-IDDC-KEY          TO W-IDDC-Q3-MIN               
127300             ELSE                                                         
127400*              PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                      
127500*                                      OR SEGMENT-MISSING                 
127600*                                      OR END-OF-DB                       
127700               IF SEGMENT-FOUND                                           
127800                 PERFORM FA-SHOW-LINES                                    
127900               END-IF                                                     
128000               PERFORM S04-READ-WDQ3G                                     
128100               IF MOD-INDX > MAX-MOD-INDX                                 
128200                 IF SEGMENT-FOUND                                         
128300                   MOVE SEQG-IDDC          TO SAVE-IDDC-NEXT              
128400                   MOVE SEQG-IDPRODNR      TO SAVE-IDPRODNR-NEXT          
128500                 ELSE                                                     
128600                   MOVE SPACE              TO SAVE-IDDC-NEXT              
128700                   MOVE ZERO               TO SAVE-IDPRODNR-NEXT          
128800                   MOVE SEQG-IDKUNDNR      TO SAVE-IDKUNDNR-NEXT          
128900                   MOVE SEQG-IDKUNDRF      TO SAVE-IDKUNDRF-NEXT          
129000                   MOVE SEQG-IDORDER       TO SAVE-IDORDER-NEXT           
129100                 END-IF                                                   
129200               END-IF                                                     
129300               MOVE ZERO                   TO W-IDPRODNR-Q3-MIN           
129400                                              W-IDPLKLST-Q3-MIN           
129500               MOVE SPACE                  TO W-IDDC-Q3-MIN               
129600*              END-PERFORM                                                
129700             END-IF                                                       
129800           END-IF                                                         
129900           IF SEGMENT-MISSING                                             
130000             IF SAVE-FLSORT = JA OR YES                                   
130100               IF MSGI-IDKUNDNR NUMERIC AND MSGI-IDKUNDNR > 0             
130200                  PERFORM IMS-GN-WDQ201-DSEQ                              
130300               ELSE                                                       
130400                  PERFORM IMS-GN-WDQ201-FSEQ                              
130500               END-IF                                                     
130600             ELSE                                                         
130700               PERFORM S04-READ-WDQ3G                                     
130800             END-IF                                                       
130900           END-IF                                                         
131000         ELSE                                                             
131100           PERFORM MFS-ERASE-FIELD-OUT                                    
131200           ADD +1                          TO MOD-INDX                    
131300           MOVE 1                          TO W-IDPRODNR-E6               
131400         END-IF                                                           
131500       END-PERFORM                                                        
131600                                                                          
131700       IF SEGMENT-FOUND                                                   
131800         IF SAVE-FLSORT = JA OR YES                                       
131900           IF MSGI-IDKUNDNR NUMERIC AND MSGI-IDKUNDNR > 0                 
132000             MOVE SEQD-IDORDER               TO W-IDORDER2                
132100           ELSE                                                           
132200             MOVE SEQF-IDORDER               TO W-IDORDER2                
132300           END-IF                                                         
132400           PERFORM IMS-GU-WDQ201                                          
132500         END-IF                                                           
132600         MOVE OHUV-IDKUNDNR                TO SAVE-IDKUNDNR-NEXT          
132700         MOVE OHUV-IDKUNDRF                TO SAVE-IDKUNDRF-NEXT          
132800         MOVE OHUV-IDORDER                 TO SAVE-IDORDER-NEXT           
132900         MOVE INF-MORE-INFO-EXISTS         TO MED-IDMFSINF                
133000         CALL WMEDKONV                  USING MED-WMEDAREA                
133100         MOVE MED-MFSINF                   TO MOD-TEMFSINF                
133200       ELSE                                                               
133300         MOVE INF-LAST-PAGE                TO MED-IDMFSINF                
133400         CALL WMEDKONV                  USING MED-WMEDAREA                
133500         MOVE MED-MFSINF                   TO MOD-TEMFSINF                
133600       END-IF                                                             
133700     END-IF                                                               
133800                                                                          
133900     MOVE '002'                            TO MSGI-KDCALL                 
134000     MOVE '4511'                           TO SAVE-IDTRANS                
134100     MOVE MOD-FLPROF-UT                    TO SAVE-FLPROF                 
134200     MOVE MOD-FLSORT-UT                    TO SAVE-FLSORT                 
134300     MOVE MOD-FLSOFT-UT                    TO SAVE-FLSOFT                 
134400     MOVE SAVE-AREA                        TO MSGI-SPAR-AREA              
134500     CALL W005INIT                      USING MSGI-WMSGINIT               
134600                                              WDP7-PCB                    
134700     .                                                                    
134800     EJECT                                                                
134900 FA-SHOW-LINES SECTION.                                                   
135000                                                                          
135100     IF MOD-INDX = +1                                                     
135200                                                                          
135300         MOVE OHUV-IDKUNDNR TO SAVE-IDKUNDNR-PREV (PGNO)                  
135400         MOVE OHUV-IDORDNR7 TO SAVE-IDKUNDRF-PREV (PGNO)                  
135500         MOVE OHUV-IDORDER  TO SAVE-IDORDER-PREV (PGNO)                   
135600         IF SAVE-FLSORT = JA OR YES                                       
135700            MOVE ODEL-IDDC     TO SAVE-IDDC-PREV (PGNO)                   
135800            MOVE ODEL-IDPRODNR TO SAVE-IDPRODNR-PREV (PGNO)               
135900         ELSE                                                             
136000            MOVE SEQG-IDDC     TO SAVE-IDDC-PREV (PGNO)                   
136100            MOVE SEQG-IDPRODNR TO SAVE-IDPRODNR-PREV (PGNO)               
136200         END-IF                                                           
136300     END-IF                                                               
136400                                                                          
136500         IF SAVE-FLSORT = JA OR YES                                       
136600            MOVE ODEL-IDPRODNR TO W-IDPRODNR2                             
136700         ELSE                                                             
136800            MOVE SEQG-IDPRODNR TO W-IDPRODNR2                             
136900         END-IF                                                           
137000         IF W-IDPRODNR2 NOT = W-IDPRODNR-E6                               
137100           MOVE +0 TO WS-SUORDV                                           
137200           PERFORM FB-CHECK-STATUS-E6                                     
137300           IF SAVE-FLSORT = JA OR YES                                     
137400              MOVE ODEL-IDKUNDNR TO MOD-IDKUNDNR (MOD-INDX)               
137500              MOVE ODEL-IDORDNR7 TO MOD-IDORDNR7 (MOD-INDX)               
137600              MOVE ODEL-IDPRODNR TO MOD-IDPRODNR (MOD-INDX)               
137700              MOVE ODEL-IDDC     TO MOD-IDDC (MOD-INDX)                   
137800              MOVE ODEL-TIREGDAT TO MOD-TIREGDAT (MOD-INDX)               
137900              IF ODEL-SUORDV-LOCPREL > ZERO                               
138000                 MOVE '*' TO WS-TEASTRIX                                  
138100              ELSE                                                        
138200                 MOVE ' ' TO WS-TEASTRIX                                  
138300              END-IF                                                      
138400            ELSE                                                          
138500              MOVE SEQG-IDKUNDNR TO MOD-IDKUNDNR (MOD-INDX)               
138600              MOVE SEQG-IDORDNR7 TO MOD-IDORDNR7 (MOD-INDX)               
138700              MOVE SEQG-IDPRODNR TO MOD-IDPRODNR (MOD-INDX)               
138800              MOVE SEQG-IDDC     TO MOD-IDDC (MOD-INDX)                   
138900              MOVE SEQG-TIREGDAT TO MOD-TIREGDAT (MOD-INDX)               
139000              IF SEQG-SUORDV-LOCPREL > ZERO                               
139100                 MOVE '*' TO WS-TEASTRIX                                  
139200              ELSE                                                        
139300                 MOVE ' ' TO WS-TEASTRIX                                  
139400              END-IF                                                      
139500            END-IF                                                        
139600                                                                          
139700        MOVE OHUV-KDORDKL  TO MOD-KDORDKL (MOD-INDX)                      
139800        IF OHUV-FLORDSPE = JA AND OHUV-IDSYSTEM NOT = 'W216'              
139900           IF OHUV-IDSYSTEM = 'SOFT'                                      
140000              MOVE 'SOFT ORDER     ' TO MOD-BEKUNDRF (MOD-INDX)           
140100           ELSE                                                           
140200              MOVE 'INT.SPEC.ORDER ' TO MOD-BEKUNDRF (MOD-INDX)           
140300           END-IF                                                         
140400        ELSE                                                              
140500           MOVE OHUV-BEKUNDRF TO MOD-BEKUNDRF (MOD-INDX)                  
140600        END-IF                                                            
140700        MOVE OHUV-IDUSER TO MOD-IDUSER (MOD-INDX)                         
140800                                                                          
140900        IF GOOD-DDC                                                       
141000          MOVE WC-CDC-SE   TO W-IDDC-Q212                                 
141100        ELSE                                                              
141200         IF SAVE-FLSORT = JA OR YES                                       
141300          MOVE ODEL-IDDC   TO W-IDDC-Q212                                 
141400         ELSE                                                             
141500          MOVE SEQG-IDDC   TO W-IDDC-Q212                                 
141600         END-IF                                                           
141700        END-IF                                                            
141800                                                                          
141900        IF W-STATUS-WDQ212 = ' '                                          
142000           PERFORM IMS-GNP-WDQ212                                         
142100        ELSE                                                              
142200           MOVE 'GE' TO STATUS-WS                                         
142300        END-IF                                                            
142400                                                                          
142500        IF SEGMENT-FOUND                                                  
142600           MOVE ARB-KDFRAKT TO MOD-KDFRAKT (MOD-INDX)                     
142700        ELSE                                                              
142800           MOVE ZERO        TO MOD-KDFRAKT (MOD-INDX)                     
142900        END-IF                                                            
143000        MOVE WS-TEASTRIX    TO MOD-TEASTRIX (MOD-INDX)                    
143100        ADD +1 TO MOD-INDX                                                
143200        IF MOD-INDX < 15                                                  
143300           MOVE SPACE          TO MOD-TEASTRIX (MOD-INDX)                 
143400        END-IF                                                            
143500     END-IF                                                               
143600                                                                          
143700     IF SAVE-FLSORT = JA OR YES                                           
143800        ADD ODEL-SUORDV TO WS-SUORDV                                      
143900        ADD ODEL-SUORDV-LOC TO WS-SUORDV                                  
144000        ADD ODEL-SUORDV-LOCPREL TO WS-SUORDV                              
144100     ELSE                                                                 
144200        ADD SEQG-SUORDV TO WS-SUORDV                                      
144300        ADD SEQG-SUORDV-LOC TO WS-SUORDV                                  
144400        ADD SEQG-SUORDV-LOCPREL TO WS-SUORDV                              
144500     END-IF                                                               
144600     MOVE WS-SUORDV TO MOD-SUORDV (MOD-INDX - 1)                          
144700                                                                          
144800     .                                                                    
144900     EJECT                                                                
145000 FB-CHECK-STATUS-E6 SECTION.                                              
145100                                                                          
145200     MOVE W-IDPRODNR2 TO W-IDPRODNR-E6                                    
145300     PERFORM IMS-GU-WDE601                                                
145400     IF SEGMENT-FOUND                                                     
145500        IF VORD-KDORDSTA < 3                                              
145600           IF VORD-KVORDRAD-PACK > ZERO                                   
145700           OR VORD-KVKOLLI-FL > ZERO                                      
145800              MOVE 'U*' TO MOD-KDORDSTA (MOD-INDX)                        
145900           ELSE                                                           
146000              MOVE 'U ' TO MOD-KDORDSTA (MOD-INDX)                        
146100           END-IF                                                         
146200        ELSE                                                              
146300           IF VORD-KDORDSTA = 3                                           
146400              IF VORD-KVKOLLI-FL > ZERO                                   
146500                 MOVE 'P*' TO MOD-KDORDSTA (MOD-INDX)                     
146600              ELSE                                                        
146700                 MOVE 'P ' TO MOD-KDORDSTA (MOD-INDX)                     
146800              END-IF                                                      
146900           ELSE                                                           
147000              IF VORD-KDORDSTA = 4                                        
147100                 IF VORD-KVKOLLI-FAKT = ZERO                              
147200                    MOVE 'S ' TO MOD-KDORDSTA (MOD-INDX)                  
147300                 ELSE                                                     
147400                    IF VORD-KVKOLLI-FAKT > VORD-KVKOLLI                   
147500                       MOVE 'S*' TO MOD-KDORDSTA (MOD-INDX)               
147600                    ELSE                                                  
147700                       MOVE 'SF' TO MOD-KDORDSTA (MOD-INDX)               
147800                    END-IF                                                
147900                 END-IF                                                   
148000              ELSE                                                        
148100                 IF VORD-KDORDSTA = 5                                     
148200                    MOVE 'SF' TO MOD-KDORDSTA (MOD-INDX)                  
148300                 END-IF                                                   
148400              END-IF                                                      
148500           END-IF                                                         
148600        END-IF                                                            
148700     ELSE                                                                 
148800        MOVE 'R ' TO MOD-KDORDSTA (MOD-INDX)                              
148900     END-IF                                                               
149000     .                                                                    
149100     EJECT                                                                
149200 G-READ-SHOW-PROFORMA SECTION.                                            
149300                                                                          
149400     PERFORM IMS-GU-WDE801                                                
149500                                                                          
149600     IF SEGMENT-MISSING                                                   
149700        IF MFS-NEXT                                                       
149800           MOVE INF-LAST-PAGE TO MED-IDMFSFEL                             
149900           CALL WMEDKONV USING MED-WMEDAREA                               
150000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
150100        ELSE                                                              
150200           MOVE ERR-LINES-MISSING TO MED-IDMFSFEL                         
150300           CALL WMEDKONV USING MED-WMEDAREA                               
150400           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
150500        END-IF                                                            
150600     ELSE                                                                 
150700        MOVE +1 TO MOD-INDX                                               
150800        MOVE PHUV-IDKUNDNR  TO SAVE-IDKUNDNR-PREV (PGNO)                  
150900        MOVE PHUV-IDORDNR7  TO SAVE-IDKUNDRF-PREV (PGNO)                  
151000        PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                             
151100                          OR SEGMENT-MISSING                              
151200                          OR END-OF-DB                                    
151300           IF PHUV-FLBORT = NOO                                           
151400              MOVE PHUV-IDKUNDNR   TO MOD-IDKUNDNR (MOD-INDX)             
151500              MOVE PHUV-IDORDNR7   TO MOD-IDORDNR7 (MOD-INDX)             
151600              MOVE ZERO            TO MOD-IDPRODNR (MOD-INDX)             
151700              MOVE WC-CDC-SE       TO MOD-IDDC (MOD-INDX)                 
151800              MOVE PHUV-KDFRAKT    TO MOD-KDFRAKT (MOD-INDX)              
151900              MOVE PHUV-KDORDKL    TO MOD-KDORDKL (MOD-INDX)              
152000              MOVE PHUV-KDPROTYP   TO MOD-KDORDSTA (MOD-INDX)             
152100              MOVE PHUV-BEKUNDRF   TO MOD-BEKUNDRF (MOD-INDX)             
152200              MOVE PHUV-TIREGDAT   TO MOD-TIREGDAT (MOD-INDX)             
152300              MOVE PHUV-IDUSER     TO MOD-IDUSER (MOD-INDX)               
152400              MOVE PHUV-IDKUNDNR   TO W-IDKUNDNR-E8-MIN                   
152500                                      W-IDKUNDNR-B2                       
152600              MOVE PHUV-IDKUNDRF   TO W-IDKUNDRF-E8-MIN                   
152700              IF DIST79-DEALER-PRICE                                      
152800                 COMPUTE HELP-SUM = PHUV-SUORDV-LOC +                     
152900                                    PHUV-SUORDV-LOCPREL                   
153000                 MOVE PHUV-KDVALISO       TO WS-KDVALISO                  
153100                 PERFORM S01-CALCULATE-TO-SEK                             
153200                 MOVE EXCH-SUORDV-UT      TO MOD-SUORDV (MOD-INDX)        
153300                 IF PHUV-SUORDV-LOCPREL = +0                              
153400                    MOVE ' '       TO WS-TEASTRIX                         
153500                 ELSE                                                     
153600                    MOVE '*'       TO WS-TEASTRIX                         
153700                 END-IF                                                   
153800              ELSE                                                        
153900                 MOVE PHUV-SUORDV  TO MOD-SUORDV (MOD-INDX)               
154000                 MOVE ' '          TO WS-TEASTRIX                         
154100              END-IF                                                      
154200              MOVE WS-TEASTRIX     TO MOD-TEASTRIX (MOD-INDX)             
154300              PERFORM IMS-GU-WDB201                                       
154400              IF SEGMENT-FOUND                                            
154500                 MOVE GMT-IDPARTNR TO W-IDPARTNR-B1                       
154600                 MOVE GMT-IDFTG    TO W-IDFTG-B1                          
154700                 PERFORM IMS-GU-WDB101                                    
154800                 IF SEGMENT-FOUND                                         
154900                    AND W-IDPARTNR-B1 NOT = SPACE                         
155000                    IF BET-KDKREDSP = '1'                                 
155100                       IF MSGI-IDLAND-SPR = 'SE'                          
155200                          MOVE 'BET. STOPPAD   '                          
155300                                   TO MOD-BEKUNDRF (MOD-INDX)             
155400                       ELSE                                               
155500                          MOVE 'CUST. STOPPED  '                          
155600                                   TO MOD-BEKUNDRF (MOD-INDX)             
155700                       END-IF                                             
155800                    ELSE                                                  
155900                       MOVE PHUV-BEKUNDRF                                 
156000                                       TO MOD-BEKUNDRF (MOD-INDX)         
156100                    END-IF                                                
156200                    MOVE PHUV-KDVALISO TO WS-KDVALISO                     
156300                 ELSE                                                     
156400                    MOVE PHUV-BEKUNDRF TO MOD-BEKUNDRF (MOD-INDX)         
156500                 END-IF                                                   
156600              ELSE                                                        
156700                 PERFORM MFS-ERASE-FIELD-OUT                              
156800              END-IF                                                      
156900              ADD +1 TO MOD-INDX                                          
157000           END-IF                                                         
157100           PERFORM IMS-GN-WDE801                                          
157200        END-PERFORM                                                       
157300     END-IF                                                               
157400                                                                          
157500     IF SEGMENT-FOUND                                                     
157600       MOVE PHUV-IDKUNDNR        TO SAVE-IDKUNDNR-PREV (PGNO)             
157700       MOVE PHUV-IDKUNDRF        TO SAVE-IDKUNDRF-PREV (PGNO)             
157800       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
157900       CALL WMEDKONV          USING MED-WMEDAREA                          
158000       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
158100     ELSE                                                                 
158200       MOVE INF-LAST-PAGE        TO MED-IDMFSINF                          
158300       CALL WMEDKONV          USING MED-WMEDAREA                          
158400       MOVE MED-MFSINF           TO MOD-TEMFSINF                          
158500     END-IF                                                               
158600     MOVE '002'                  TO MSGI-KDCALL                           
158700     MOVE '4511'                 TO SAVE-IDTRANS                          
158800     MOVE MOD-FLPROF-UT          TO SAVE-FLPROF                           
158900     MOVE MOD-FLSORT-UT          TO SAVE-FLSORT                           
159000     MOVE MOD-FLSOFT-UT          TO SAVE-FLSOFT                           
159100     MOVE SAVE-AREA              TO MSGI-SPAR-AREA                        
159200     CALL W005INIT            USING MSGI-WMSGINIT WDP7-PCB                
159300     .                                                                    
159400     EJECT                                                                
159500 S01-CALCULATE-TO-SEK  SECTION.                                           
159600                                                                          
159700     IF WS-KDVALISO > SPACE                                               
159800        MOVE WS-KDVALISO   TO CURR-KDVALISO-ROW                           
159900        MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)               
160000        MOVE FUNCTION CURRENT-DATE(5:2) TO W-DATE-AAMM(3:2)               
160100                                                                          
160200     ELSE                                                                 
160300        PERFORM IMS-GU-WDB201                                             
160400        IF SEGMENT-FOUND                                                  
160500           CONTINUE                                                       
160600        ELSE                                                              
160700           PERFORM IMS-GU-WDB201                                          
160800        END-IF                                                            
160900        MOVE GMT-IDPARTNR TO W-IDPARTNR-B1                                
161000        MOVE GMT-IDFTG    TO W-IDFTG-B1                                   
161100        PERFORM IMS-GU-WDB101                                             
161200                                                                          
161300        MOVE BET-KDVALISO TO CURR-KDVALISO-ROW                            
161400                             WS-KDVALISO                                  
161500        MOVE FUNCTION CURRENT-DATE(3:2) TO W-DATE-AAMM(1:2)               
161600        MOVE FUNCTION CURRENT-DATE(5:2) TO W-DATE-AAMM(3:2)               
161700     END-IF                                                               
161800                                                                          
161900     MOVE W-DATE-AAMM        TO CURR-TIAAMM                               
162000     MOVE WS-KDVALISO-HUV    TO CURR-KDVALISO-HUV                         
162100     MOVE 'M'                TO CURR-KDVALTYP                             
162200     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
162300     IF CURR-KDSVAR = ' '                                                 
162400        MOVE CURR-PRKURS-NEW TO EXCH-PRKURS                               
162500     ELSE                                                                 
162600        MOVE 1               TO EXCH-PRKURS                               
162700     END-IF                                                               
162800*    --- +1 KDCALL = LOKAL VALUTA TILL SEK                                
162900     MOVE +1          TO EXCH-KDCALL                                      
163000     MOVE HELP-SUM    TO EXCH-SUORDV-IN                                   
163100     MOVE +0          TO EXCH-PRARTNTO-IN                                 
163200     CALL W411EXCH USING EXCH-W411EXCH                                    
163300     .                                                                    
163400     EJECT                                                                
163500 S02-INSERT-ALTMSG SECTION.                                               
163600                                                                          
163700     MOVE P-TO-P-SW TO MSG-IO-AREA                                        
163800     PERFORM IMS-CHANGE-ALTMSG                                            
163900     IF STATUS-OK                                                         
164000        PERFORM IMS-INSERT-ALTMSG                                         
164100     ELSE                                                                 
164200        MOVE LOW-VALUE          TO MSG-AREA                               
164300        MOVE P-TO-P-KDTRANS (2:1) TO W-SCREEN (1:1)                       
164400        MOVE P-TO-P-KDTRANS (4:3) TO W-SCREEN (2:3)                       
164500        IF SECURITY-ERROR                                                 
164600           STRING 'NOT AUTHORIZED TO USE ' W-SCREEN                       
164700           DELIMITED BY SIZE INTO MOD-TEMFSINF                            
164800        ELSE                                                              
164900           STRING 'WRONG PICTURE ' W-SCREEN                               
165000           DELIMITED BY SIZE INTO MOD-TEMFSINF                            
165100        END-IF                                                            
165200        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
165300        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51101 + 4                     
165400        PERFORM IMS-INSERT-MSG                                            
165500     END-IF                                                               
165600     .                                                                    
165700     EJECT                                                                
165800 S03-CHECK-WSECURIT SECTION.                                              
165900                                                                          
166000     MOVE MSG-SIGNON-USERID TO SEC-IDUSER                                 
166100     MOVE '4511'            TO SEC-IDTRANS                                
166200     MOVE MSGI-IDDISTR      TO SEC-IDKEY                                  
166300                                                                          
166400     CALL WSECURIT USING SEC-IDUSER                                       
166500                         SEC-IDTRANS                                      
166600                         SEC-IDKEY                                        
166700                         SEC-KDSVAR                                       
166800                                                                          
166900* WSECURIT CAN RETURN: F, SPACE, 1, 2, 3, 4, 5 AND 6                      
167000     IF SEC-KDSVAR = 2 OR 6 OR 'F'                                        
167100        MOVE +1                 TO MOD-INDX                               
167200        PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                             
167300           MOVE MFS-RENSA-FAELT TO MOD-SUORDV (MOD-INDX)                  
167400           ADD  +1              TO MOD-INDX                               
167500        END-PERFORM                                                       
167600     END-IF                                                               
167700     .                                                                    
167800     EJECT                                                                
167900 MFS-ERASE-FIELD-OUT SECTION.                                             
168000                                                                          
168100*    --- ALL OUTDATA FIELDS                                               
168200     IF MOD-INDX > 0 AND MOD-INDX NOT > MAX-MOD-INDX                      
168300     MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR (MOD-INDX)                      
168400                             MOD-IDORDNR7 (MOD-INDX)                      
168500                             MOD-IDPRODNR (MOD-INDX)                      
168600                             MOD-IDDC (MOD-INDX)                          
168700                             MOD-KDFRAKT (MOD-INDX)                       
168800                             MOD-KDORDKL (MOD-INDX)                       
168900                             MOD-KDORDSTA (MOD-INDX)                      
169000                             MOD-BEKUNDRF (MOD-INDX)                      
169100                             MOD-TIREGDAT (MOD-INDX)                      
169200                             MOD-IDUSER (MOD-INDX)                        
169300                             MOD-SUORDV (MOD-INDX)                        
169400     END-IF                                                               
169500     .                                                                    
169600     SKIP3                                                                
169700 MFS-ERASE-FIELD-IN SECTION.                                              
169800                                                                          
169900*    --- ALL INDATA FIELDS                                                
170000     IF MOD-INDX > ZERO                                                   
170100        MOVE MFS-ERASE-FIELD TO MOD-IDTRANS-HOPP (MOD-INDX)               
170200     END-IF                                                               
170300     .                                                                    
170400     EJECT                                                                
170500 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
170600                                                                          
170700*    --- ALL OUTDATA FIELDS INCL. SCROLL KEYS AND LINE DATA               
170800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLSORT                            
170900                                    MOD-FLSOFT                            
171000                                    MOD-FLPROF                            
171100                                    MOD-TEDDI                             
171200                                                                          
171300     MOVE +1 TO MOD-INDX                                                  
171400     PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                                
171500        PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                             
171600        ADD +1 TO MOD-INDX                                                
171700     END-PERFORM                                                          
171800     .                                                                    
171900     SKIP2                                                                
172000 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
172100                                                                          
172200*    --- OUTDATA FIELDS ON SCROLL KEYS                                    
172300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDKUNDNR (MOD-INDX)               
172400                                    MOD-IDORDNR7 (MOD-INDX)               
172500                                    MOD-IDPRODNR (MOD-INDX)               
172600                                    MOD-IDDC (MOD-INDX)                   
172700                                    MOD-KDFRAKT (MOD-INDX)                
172800                                    MOD-KDORDKL (MOD-INDX)                
172900                                    MOD-KDORDSTA (MOD-INDX)               
173000                                    MOD-BEKUNDRF (MOD-INDX)               
173100                                    MOD-TIREGDAT (MOD-INDX)               
173200                                    MOD-IDUSER (MOD-INDX)                 
173300                                    MOD-SUORDV (MOD-INDX)                 
173400                                    MOD-TEASTRIX (MOD-INDX)               
173500     .                                                                    
173600     EJECT                                                                
173700 S04-READ-WDQ3G SECTION.                                                  
173800                                                                          
173900*--WHEN DISTRICT AND IDDC IS ENTERED AND IDKUNDNR IS NUMERIC--*           
174000     IF MSGI-IDKUNDNR IS NUMERIC                                          
174100       IF MSGI-IDKUNDNR = ZERO                                            
174200         PERFORM IMS-GN-WDQ3G1-IDDC                                       
174300       ELSE                                                               
174400*--WHEN DISTRICT AND IDDC IS ENTERED AND IDKUNDNR IS NOT ZERO-*           
174500         PERFORM IMS-GN-WDQ3G1                                            
174600       END-IF                                                             
174700     ELSE                                                                 
174800*--WHEN DISTRICT AND IDDC IS ENTERED AND IDDC CAN BE --*                  
174900*--SPACES OR ZERO AND IDKUNDNR IS SPACES--*                               
175000       IF MSGI-IDDC-KEY = SPACE                                           
175100           PERFORM IMS-GN-WDQ3G1                                          
175200       ELSE                                                               
175300           PERFORM IMS-GN-WDQ3G1-IDDC                                     
175400       END-IF                                                             
175500     END-IF                                                               
175600     IF SEGMENT-FOUND                                                     
175700       MOVE SEQG-IDORDER         TO W-IDORDER2                            
175800       MOVE ' '                  TO W-STATUS-WDQ212                       
175900       PERFORM IMS-GU-WDQ201                                              
176000       IF SEGMENT-MISSING                                                 
176100         MOVE SPACE              TO STATUS-WS                             
176200         MOVE 'SAKNAS'           TO OHUV-BEKUNDRF                         
176300         MOVE SEQG-IDKUNDNR      TO OHUV-IDKUNDNR                         
176400         MOVE SEQG-IDKUNDRF      TO OHUV-IDKUNDRF                         
176500         MOVE SEQG-IDORDER       TO OHUV-IDORDER                          
176600         IF OHUV-IDKUNDNR NOT NUMERIC                                     
176700           MOVE SAVE-IDKUNDNR-PREV(PGNO)                                  
176800                                 TO OHUV-IDKUNDNR                         
176900           MOVE SAVE-IDORDER-PREV(PGNO)                                   
177000                                 TO OHUV-IDORDER                          
177100         END-IF                                                           
177200         MOVE ZERO               TO OHUV-KDORDKL                          
177300         MOVE 'Y'                TO W-STATUS-WDQ212                       
177400       END-IF                                                             
177500     END-IF                                                               
177600     .                                                                    
177700     SKIP3                                                                
177800* --- IMS SECTIONS ---                                                    
177900 IMS-GET-MSG SECTION.                                                     
178000                                                                          
178100     MOVE '  QC' TO GOOD-STATUSCODES                                      
178200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
178300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
178400     PERFORM IMS-STATUSCHECK                                              
178500     .                                                                    
178600     SKIP3                                                                
178700 IMS-INSERT-MSG SECTION.                                                  
178800                                                                          
178900     IF MSGI-IDLAND-SPR = 'SE'                                            
179000        MOVE '0' TO MFS-KDHUVOMR                                          
179100     END-IF                                                               
179200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
179300     MOVE SPACE TO GOOD-STATUSCODES                                       
179400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
179500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
179600     PERFORM IMS-STATUSCHECK                                              
179700     .                                                                    
179800     EJECT                                                                
179900 IMS-CHANGE-ALTMSG SECTION.                                               
180000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
180100     MOVE '  A4A1' TO GOOD-STATUSCODES                                    
180200     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
180300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
180400     PERFORM IMS-STATUSCHECK                                              
180500     .                                                                    
180600     SKIP3                                                                
180700 IMS-INSERT-ALTMSG SECTION.                                               
180800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
180900     MOVE SPACE TO GOOD-STATUSCODES                                       
181000     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
181100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
181200     PERFORM IMS-STATUSCHECK                                              
181300     .                                                                    
181400     EJECT                                                                
181500 IMS-GU-WDQ201 SECTION.                                                   
181600                                                                          
181700     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X2 ')'                        
181800          DELIMITED BY SIZE INTO SSA1                                     
181900     MOVE '  GE' TO GOOD-STATUSCODES                                      
182000     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
182100     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
182200     PERFORM IMS-STATUSCHECK                                              
182300     .                                                                    
182400     EJECT                                                                
182500 IMS-GN-WDQ3G1 SECTION.                                                   
182600                                                                          
182700     STRING 'WDQ3G1  (WDQ3G1KY>=' W-WDQ3GSEQ-MIN-X                        
182800                    '&WDQ3G1KY<=' W-WDQ3GSEQ-MAX-X ')'                    
182900          DELIMITED BY SIZE INTO SSA1                                     
183000     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
183100     CALL CBLTDLI USING GN WDQ3GSEQ-PCB DLI-IO-WDQ3G1 SSA1                
183200     MOVE WDQ3GSEQ-STATUS-CODE TO STATUS-WS                               
183300     PERFORM IMS-STATUSCHECK                                              
183400     .                                                                    
183500     EJECT                                                                
183600 IMS-GN-WDQ3G1-IDDC SECTION.                                              
183700                                                                          
183800     STRING 'WDQ3G1  (WDQ3G1KY>=' W-WDQ3GSEQ-MIN-X                        
183900                    '&WDQ3G1KY<=' W-WDQ3GSEQ-MAX-X                        
184000                    '&IDDC     =' W-IDDC-WDQ3G1-X ')'                     
184100          DELIMITED BY SIZE INTO SSA1                                     
184200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
184300     CALL CBLTDLI USING GN WDQ3GSEQ-PCB DLI-IO-WDQ3G1 SSA1                
184400     MOVE WDQ3GSEQ-STATUS-CODE TO STATUS-WS                               
184500     PERFORM IMS-STATUSCHECK                                              
184600     .                                                                    
184700     EJECT                                                                
184800 IMS-GNP-WDQ212 SECTION.                                                  
184900                                                                          
185000     STRING 'WDQ212  *F(IDDC     =' W-IDDC-Q212-X ')'                     
185100          DELIMITED BY SIZE INTO SSA1                                     
185200     MOVE '  GE' TO GOOD-STATUSCODES                                      
185300     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-WDQ212 SSA1                   
185400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
185500     PERFORM IMS-STATUSCHECK                                              
185600     .                                                                    
185700     EJECT                                                                
185800 IMS-GU-WDQ201-DSEQ SECTION.                                              
185900                                                                          
186000     STRING 'WDQ2D1  (WDQ2D1KY>=' W-WDQ2D1KY-MIN-X                        
186100                    '&WDQ2D1KY<=' W-WDQ2D1KY-MAX-X ')'                    
186200          DELIMITED BY SIZE INTO SSA1                                     
186300     MOVE '  GE' TO GOOD-STATUSCODES                                      
186400     CALL CBLTDLI USING GU WDQ2D-PCB DLI-IO-WDQ2D1 SSA1                   
186500     MOVE WDQ2D-STATUS-CODE TO STATUS-WS                                  
186600     PERFORM IMS-STATUSCHECK                                              
186700     .                                                                    
186800     EJECT                                                                
186900 IMS-GN-WDQ201-DSEQ SECTION.                                              
187000                                                                          
187100     STRING 'WDQ2D1  (WDQ2D1KY>=' W-WDQ2D1KY-MIN-X                        
187200                    '&WDQ2D1KY<=' W-WDQ2D1KY-MAX-X ')'                    
187300          DELIMITED BY SIZE INTO SSA1                                     
187400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
187500     CALL CBLTDLI USING GN WDQ2D-PCB DLI-IO-WDQ2D1 SSA1                   
187600     MOVE WDQ2D-STATUS-CODE TO STATUS-WS                                  
187700     PERFORM IMS-STATUSCHECK                                              
187800     .                                                                    
187900     EJECT                                                                
188000 IMS-GU-WDQ201-DSEQ-UNIQUE SECTION.                                       
188100                                                                          
188200     STRING 'WDQ2D1  (WDQ2D1KY>=' W-WDQ2D1KY-MIN-X                        
188300                    '&WDQ2D1KY<=' W-WDQ2D1KY-MAX-X                        
188400                    '&IDORDER  =' W-IDORDER-X ')'                         
188500          DELIMITED BY SIZE INTO SSA1                                     
188600     MOVE '  GE' TO GOOD-STATUSCODES                                      
188700     CALL CBLTDLI USING GU WDQ2D-PCB DLI-IO-WDQ2D1 SSA1                   
188800     MOVE WDQ2D-STATUS-CODE TO STATUS-WS                                  
188900     PERFORM IMS-STATUSCHECK                                              
189000     .                                                                    
189100     EJECT                                                                
189200 IMS-GU-WDQ201-FSEQ SECTION.                                              
189300                                                                          
189400     STRING 'WDQ2F1  (WDQ2F1KY>=' W-WDQ2F1KY-MIN-X                        
189500                    '&WDQ2F1KY<=' W-WDQ2F1KY-MAX-X ')'                    
189600          DELIMITED BY SIZE INTO SSA1                                     
189700     MOVE '  GE' TO GOOD-STATUSCODES                                      
189800     CALL CBLTDLI USING GU WDQ2F-PCB DLI-IO-WDQ2F1 SSA1                   
189900     MOVE WDQ2F-STATUS-CODE TO STATUS-WS                                  
190000     PERFORM IMS-STATUSCHECK                                              
190100     .                                                                    
190200     EJECT                                                                
190300 IMS-GN-WDQ201-FSEQ SECTION.                                              
190400                                                                          
190500     STRING 'WDQ2F1  (WDQ2F1KY>=' W-WDQ2F1KY-MIN-X                        
190600                    '&WDQ2F1KY<=' W-WDQ2F1KY-MAX-X ')'                    
190700          DELIMITED BY SIZE INTO SSA1                                     
190800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
190900     CALL CBLTDLI USING GN WDQ2F-PCB DLI-IO-WDQ2F1 SSA1                   
191000     MOVE WDQ2F-STATUS-CODE TO STATUS-WS                                  
191100     PERFORM IMS-STATUSCHECK                                              
191200     .                                                                    
191300     EJECT                                                                
191400 IMS-GU-WDQ201-FSEQ-UNIQUE SECTION.                                       
191500                                                                          
191600     STRING 'WDQ2F1  (WDQ2F1KY>=' W-WDQ2F1KY-MIN-X                        
191700                    '&WDQ2F1KY<=' W-WDQ2F1KY-MAX-X                        
191800                    '&IDORDER  =' W-IDORDER-X ')'                         
191900          DELIMITED BY SIZE INTO SSA1                                     
192000     MOVE '  GE' TO GOOD-STATUSCODES                                      
192100     CALL CBLTDLI USING GU WDQ2F-PCB DLI-IO-WDQ2F1 SSA1                   
192200     MOVE WDQ2F-STATUS-CODE TO STATUS-WS                                  
192300     PERFORM IMS-STATUSCHECK                                              
192400     .                                                                    
192500     EJECT                                                                
192600 IMS-GU-WDQ301 SECTION.                                                   
192700                                                                          
192800     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
192900                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
193000          DELIMITED BY SIZE INTO SSA1                                     
193100     MOVE '  GE' TO GOOD-STATUSCODES                                      
193200     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
193300     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
193400     PERFORM IMS-STATUSCHECK                                              
193500     .                                                                    
193600     EJECT                                                                
193700 IMS-GN-WDQ301 SECTION.                                                   
193800                                                                          
193900     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
194000                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
194100          DELIMITED BY SIZE INTO SSA1                                     
194200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
194300     CALL CBLTDLI USING GN WDQ3-PCB DLI-IO-WDQ301 SSA1                    
194400     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
194500     PERFORM IMS-STATUSCHECK                                              
194600     .                                                                    
194700     EJECT                                                                
194800 IMS-GU-WDE801 SECTION.                                                   
194900                                                                          
195000     STRING 'WDE801  (WDE801KY>=' W-WDE801KY-MIN-X                        
195100                    '&WDE801KY<=' W-WDE801KY-MAX-X ')'                    
195200          DELIMITED BY SIZE INTO SSA1                                     
195300     MOVE '  GE' TO GOOD-STATUSCODES                                      
195400     CALL CBLTDLI USING GU WDE8-PCB DLI-IO-WDE801 SSA1                    
195500     MOVE WDE8-STATUS-CODE TO STATUS-WS                                   
195600     PERFORM IMS-STATUSCHECK                                              
195700     .                                                                    
195800     EJECT                                                                
195900 IMS-GN-WDE801 SECTION.                                                   
196000                                                                          
196100     STRING 'WDE801  (WDE801KY>=' W-WDE801KY-MIN-X                        
196200                    '&WDE801KY<=' W-WDE801KY-MAX-X ')'                    
196300          DELIMITED BY SIZE INTO SSA1                                     
196400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
196500     CALL CBLTDLI USING GN WDE8-PCB DLI-IO-WDE801  SSA1                   
196600     MOVE WDE8-STATUS-CODE TO STATUS-WS                                   
196700     PERFORM IMS-STATUSCHECK                                              
196800     .                                                                    
196900     EJECT                                                                
197000 IMS-GU-WDB601    SECTION.                                                
197100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
197200            DELIMITED BY SIZE INTO SSA1                                   
197300     MOVE '  GE' TO GOOD-STATUSCODES                                      
197400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
197500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
197600     PERFORM IMS-STATUSCHECK                                              
197700     EJECT                                                                
197800     .                                                                    
197900 IMS-GU-WDB201    SECTION.                                                
198000     STRING 'WDB201  (IDGMT    =' W-IDGMT-B2-X ')'                        
198100            DELIMITED BY SIZE INTO SSA1                                   
198200     MOVE '  GE' TO GOOD-STATUSCODES                                      
198300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
198400     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
198500     PERFORM IMS-STATUSCHECK                                              
198600     EJECT                                                                
198700     .                                                                    
198800 IMS-GU-WDB101    SECTION.                                                
198900     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
199000            DELIMITED BY SIZE INTO SSA1                                   
199100     MOVE '  GE' TO GOOD-STATUSCODES                                      
199200     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
199300     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
199400     PERFORM IMS-STATUSCHECK                                              
199500     EJECT                                                                
199600     .                                                                    
199700 IMS-GU-WDE601    SECTION.                                                
199800     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-E6-X ')'                     
199900            DELIMITED BY SIZE INTO SSA1                                   
200000     MOVE '  GE' TO GOOD-STATUSCODES                                      
200100     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
200200     MOVE WDE6-STATUS-CODE    TO STATUS-WS                                
200300     PERFORM IMS-STATUSCHECK                                              
200400     EJECT                                                                
200500     .                                                                    
200600 IMS-STATUSCHECK SECTION.                                                 
200700                                                                          
200800     SET STATUS-IX TO 1                                                   
200900     SEARCH GOOD-STATUS                                                   
201000       AT END                                                             
201100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
201200         DELIMITED BY SIZE INTO ERROR-TEXT                                
201300         CALL FELLOG                                                      
201400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
201500         CONTINUE                                                         
201600     END-SEARCH                                                           
201700     .                                                                    
