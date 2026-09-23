000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W476VERS.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   02/08/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000800*    FUNCTION:                                                            
000900*        SUBPROGRAM TO WRITE 'CARGO VALUE SPECIFICATION' TRANSPORT        
001000*        DOCUMENT. IT IS CALLED BY A PROGRAM W40631. DOCUMENT SHOW        
001100*        GOODS VALUE, ADDING COST, TOTAL VALUE, DELIVERY TERMS            
001200*        AND AFFIRMATION TEXT.                                            
001300*                                                                         
001400*        THE PROGRAM READS     WDE1                                       
001500*        THE PROGRAM READS     WDG7                                       
001600*        THE PROGRAM READS     WDR1                                       
001700*                                                                         
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400                                                                          
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                         PIC X(8)    VALUE 'W476VERS'.          
002700 77  W-CURRENT                     PIC X(50)   VALUE SPACE.               
002800                                                                          
002900 77  YES                           PIC X       VALUE 'J'.                 
003000 77  NOO                           PIC X       VALUE 'N'.                 
003100 77  INDX                          PIC S9(4)   VALUE +0 COMP SYNC.        
003200 77  INDX2                         PIC S9(4)   VALUE +0 COMP SYNC.        
003300 77  WS-IX                         PIC S9(4)   VALUE +0 COMP SYNC.        
003400                                                                          
003500 77  WEB-OUTPUT-SW                 PIC X(01)   VALUE 'N'.                 
003600     88 WEB-OUTPUT                             VALUE 'J'.                 
003700                                                                          
003800 77  WS-PAGE-NO                    PIC 9(3)    VALUE ZERO.                
003900 77  W-KDSPRAK                     PIC S9      COMP-3.                    
004000 77  W-SUORDV                   PIC S9(9)V9(2) VALUE ZERO COMP-3.         
004100 77  W-KDVALISO                    PIC X(3)    VALUE SPACE.               
004200 77  DUMMY-AREA                    PIC X(50)   VALUE SPACE.               
004300 77  WS-LDC-HDR                    PIC S9(3)   VALUE ZERO COMP-3.         
004400 77  BOAT-TRP                      PIC S9(3)   VALUE +43 COMP-3.          
004500                                                                          
004600 77  FLLOCCUR-SW                 PIC X(01)  VALUE 'N'.                    
004700     88 FLLOCCUR                            VALUE 'J'.                    
004800                                                                          
004810 77  SW-ISRAEL-C1-FC17           PIC X       VALUE 'N'.                   
004820     88 ISRAEL-C1-FC17                       VALUE 'J'.                   
004830                                                                          
004900 01  WS-MONEY                    PIC S9(9)V9(2) COMP-3 VALUE ZERO.        
005000 01  WS-REVALUTA-LOCCUR          PIC S9(5)      COMP-3 VALUE 1.           
005100 01  WS-PRKURS-LOCCUR            PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
005200 01  WS-PRKURS-EXC               PIC S9(6)V9(5) COMP-3 VALUE ZERO.        
005300 01  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
005400 01  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
005500                                                                          
005600*    --- STYRTECKEN PRINTER                                               
005700 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
005800 01  WS-SKIP1                      PIC X      VALUE ' '.                  
005900 01  WS-SKIP2                      PIC X      VALUE '0'.                  
006000 01  WS-SKIP3                      PIC X      VALUE '-'.                  
006100                                                                          
006200 01  WS-TYP-IDSHIP                 PIC X(25) VALUE                        
006300                                 'CARGO VALUE SPECIFICATION'.             
006400                                                                          
006500 01  WS-META                       PIC X(5) VALUE '¤META'.                
006600 01  WS-IDDISTR                    PIC Z(4)9.                             
006700 01  WS-IDSHIPM-Z                  PIC Z(6)9.                             
006800 01  WS-YYMMDD                     PIC 9(6)  VALUE ZERO.                  
006900 01  WS-GOODS-VALUE                PIC S9(8)V9(2) VALUE ZERO.             
007000 01  WS-GOODS-VALUE-EXC            PIC S9(8)V9(2) VALUE ZERO.             
007100 01  WS-TOTAL-VALUE                PIC S9(8)V9(2) VALUE ZERO.             
007200 01  WS-TOTAL-VALUE-EXC            PIC S9(8)V9(2) VALUE ZERO.             
007300 01  WS-TEMP-VALUE                 PIC S9(8)V9(2) VALUE ZERO.             
007400 01  WS-PRFRAKT-VALUE              PIC S9(8)V9(2) VALUE ZERO.             
007500 01  WS-PRFRAKT-VALUE-EXC          PIC S9(8)V9(2) VALUE ZERO.             
007600 01  WS-PRFOERS-VALUE              PIC S9(8)V9(2) VALUE ZERO.             
007700 01  WS-PRFOERS-VALUE-EXC          PIC S9(8)V9(2) VALUE ZERO.             
007800 01  WS-PRLEGKST-VALUE             PIC S9(8)V9(2) VALUE ZERO.             
007900 01  WS-PRLEGKST-VALUE-EXC         PIC S9(8)V9(2) VALUE ZERO.             
008000 01  WS-PREMBHNT-VALUE             PIC S9(8)V9(2) VALUE ZERO.             
008100 01  WS-PREMBHNT-VALUE-EXC         PIC S9(8)V9(2) VALUE ZERO.             
008200 01  WS-PRAVDRAG-VALUE             PIC S9(8)V9(2) VALUE ZERO.             
008300 01  WS-PRAVDRAG-VALUE-EXC         PIC S9(8)V9(2) VALUE ZERO.             
008400 01  WS-KDVALISO                   PIC X(3) VALUE SPACE.                  
008500 01  WS-KDVALISO-2                 PIC X(3) VALUE SPACE.                  
008600 01  WS-KDFORSKN                   PIC 9(3) VALUE ZERO.                   
008700 01  WS-ANTAL-RAD-BEFORSKN         PIC 9(3) VALUE ZERO.                   
008800 01  WS-DELIVERY-TAB.                                                     
008900     03 FILLER OCCURS 6.                                                  
009000       05 WS-DELIVERY-TERMS-TERMS  PIC X(35) VALUE SPACE.                 
009100 01  WS-BEFORSKN-TAB.                                                     
009200     03 FILLER OCCURS 16.                                                 
009300       05 WS-BEFORSKN-RAD          PIC X(60) VALUE ZERO.                  
009400 01  WS-SKOLLI-SUORDV              PIC S9(9)V9(2) VALUE ZERO.             
009500 01  WS-TOTAL-VALUE-PRKURS         PIC S9(9)V9(2) VALUE ZERO.             
009600 01  WS-PRKURS                     PIC S9(5)V9(5) VALUE ZERO.             
009700                                                                          
009800 01  W-LINE.                                                              
009900     03  W-LINE-COUNT              PIC 9(02) VALUE ZERO.                  
010000     03  W-LINE-MAX                PIC 9(02) VALUE 43.                    
010100                                                                          
010200 01  TODAYS-DATE                   PIC 9(6)  VALUE ZERO.                  
010300 01  FILLER REDEFINES TODAYS-DATE.                                        
010400     03  TODAYS-DATE-YEAR          PIC 9(2).                              
010500     03  TODAYS-DATE-MONTH         PIC 9(2).                              
010600     03  TODAYS-DATE-DAY           PIC 9(2).                              
010700     EJECT                                                                
010800                                                                          
010900 01  WS-TIMESTAMP.                                                        
011000     03  FILLER                  PIC X       VALUE 'D'.                   
011100     03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
011200     03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
011300     03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
011400     03  FILLER                  PIC X       VALUE '_'.                   
011500     03  FILLER                  PIC X       VALUE 'T'.                   
011600     03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
011700     03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
011800     03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
011900                                                                          
012000 01  ARB-RAD                       PIC X(132) VALUE SPACE.                
012100                                                                          
012200 01  RAD1-HEAD.                                                           
012300     03  FILLER                    PIC X(15).                             
012400     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
012500     03  FILLER                    PIC X(2).                              
012600     03  RAD1H-IMPORTER            PIC X(35).                             
012700     03  FILLER                    PIC X(2).                              
012800**   03  FILLER                    PIC X(67).                             
012900     03  RAD1-TYP-IDSHIP           PIC X(25).                             
013000                                                                          
013100 01  RAD2X-HEAD.                                                          
013200     03  FILLER                    PIC X(30).                             
013300     03  RAD2H-IMPORTER            PIC X(35).                             
013400                                                                          
013500 01  RAD3-HEAD.                                                           
013600     03  FILLER                    PIC X(30).                             
013700     03  RAD3H-IMPORTER            PIC X(35).                             
013800                                                                          
013900 01  RAD5-HEAD.                                                           
014000     03  FILLER                    PIC X(30).                             
014100     03  RAD5H-IMPORTER            PIC X(35).                             
014200                                                                          
014300                                                                          
014400 01  RAD2-HEAD.                                                           
014500     03  FILLER                    PIC X(30).                             
014600     03  RAD4H-IMPORTER            PIC X(35).                             
014700     03  FILLER                    PIC X(02).                             
014800**   03  FILLER                    PIC X(67).                             
014900     03  RAD2H-TIYYMMDD            PIC 9(06).                             
015000     03  RAD2H-IDDISTR             PIC Z(4)9.                             
015100     03  FILLER                    PIC X(01).                             
015200     03  RAD2H-IDSHIPM             PIC Z(06)9.                            
015300     03  FILLER                    PIC X(04).                             
015400     03  RAD2H-IDTRPTNR            PIC Z(02)9.                            
015500     03  FILLER                    PIC X(01).                             
015600     03  RAD2H-IDLBBET             PIC X(12).                             
015700     03  FILLER                    PIC X(02).                             
015800     03  RAD2H-PAGE-NO             PIC Z(03).                             
015900                                                                          
016000 01  RAD1.                                                                
016100     03  FILLER                    PIC X(90).                             
016200     03  RAD1-CURRENCY-TEXT        PIC X(8).                              
016300                                                                          
016400 01  RAD2.                                                                
016500     03  FILLER                    PIC X(1).                              
016600     03  RAD2-GOODS-VALUE-TEXT     PIC X(16).                             
016700     03  FILLER                    PIC X(54).                             
016800     03  RAD2-GOODS-VALUE          PIC Z(8)9.9(2).                        
016900     03  FILLER                    PIC X(8).                              
017000     03  RAD2-KDVALISO             PIC X(3).                              
017100                                                                          
017200 01  RAD3.                                                                
017300     03  FILLER                    PIC X(1).                              
017400     03  RAD3-COMPLIMENTARY-TEXT   PIC X(30).                             
017500     03  FILLER                    PIC X(40).                             
017600     03  RAD3-COMPLIMENTARY-VALUE  PIC Z(8)9.9(2).                        
017700     03  FILLER                    PIC X(8).                              
017800     03  RAD3-KDVALISO             PIC X(3).                              
017900                                                                          
018000 01  RAD4.                                                                
018100     03  FILLER                    PIC X(1).                              
018200     03  RAD4-TOTAL-VALUE-TEXT     PIC X(16).                             
018300     03  FILLER                    PIC X(54).                             
018400     03  RAD4-TOTAL-VALUE          PIC Z(8)9.9(2).                        
018500     03  FILLER                    PIC X(8).                              
018600     03  RAD4-KDVALISO             PIC X(3).                              
018700                                                                          
018800 01  RAD5.                                                                
018900     03  FILLER                    PIC X(1).                              
019000     03  RAD5-DELIVERY-TERMS-TEXT  PIC X(30).                             
019100     03  FILLER                    PIC X(3).                              
019200     03  RAD5-DELIVERY-TERMS-TERMS PIC X(35).                             
019300                                                                          
019400 01  RAD6.                                                                
019500     03  FILLER                    PIC X(1).                              
019600     03  RAD6-EQUAL-TO-TEXT        PIC X(8).                              
019700     03  FILLER                    PIC X(1).                              
019800     03  RAD6-EQUAL-TO-KDVALISO    PIC X(3).                              
019900     03  FILLER                    PIC X(1).                              
020000     03  RAD6-RATE-TEXT            PIC X(4).                              
020100     03  FILLER                    PIC X(1).                              
020200     03  RAD6-RATE-PRKURS          PIC Z(5)9.9(5).                        
020300     03  FILLER                    PIC X(1).                              
020400     03  RAD6-TOTAL-VALUE-PRKURS   PIC Z(8)9.9(2).                        
020500                                                                          
020600 01  RAD7.                                                                
020700     03  FILLER                    PIC X(1).                              
020800     03  RAD7-AFFIRMATION-TEXT     PIC X(60).                             
020900                                                                          
021000*           COPY-TEXTER TILL WEB-LDC                                      
021100 01  WEB-AREOR.                                                           
021200     03  DOC-AREA                  PIC X(250)   VALUE SPACES.             
021300*                                                                         
021400*    03  -COPY W476CVS1                                                   
021500                                                                          
021600*    03  -COPY W476CVS2                                                   
021700                                                                          
021800 01  TEST-IDDISTR               PIC 9(5)   COMP-3.                        
021900*01  FILLER   -COPY WWDIST34    -RED TEST-IDDISTR.                        
022000     EJECT                                                                
022010*01  FILLER   -COPY WWDIST25    -RED TEST-IDDISTR.                        
022020     EJECT                                                                
022100*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
022200     EJECT                                                                
022300*01  FILLER   -COPY WWDIST41    -RED TEST-IDDISTR.                        
022400     EJECT                                                                
022500*01  FILLER   -COPY WWDIST76    -RED TEST-IDDISTR.                        
022600     EJECT                                                                
022700*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
022800     EJECT                                                                
022900*01  FILLER   -COPY WWDIS121    -RED TEST-IDDISTR.                        
023000     EJECT                                                                
023100*01    -COPY WWDC99                                                       
023200                                                                          
023300 01  FILLER          PIC X(32)  VALUE  'LEDTEXT TABLE'.                   
023400* 01 -COPY W475W551                                                       
023500     EJECT                                                                
023600* 01 -COPY W475W552                                                       
023700     EJECT                                                                
023800* 01 -COPY W475W554                                                       
023900     EJECT                                                                
024000* 01 -COPY W475W557                                                       
024100     EJECT                                                                
024200* 01 -COPY W476W001                                                       
024300     EJECT                                                                
024400                                                                          
024500 01  GENERAL-SUBPROGRAMS.                                                 
024600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
024700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
024800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024900     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
025000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
025100     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
025200     SKIP2                                                                
025300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
025400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
025500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
025600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
025700     SKIP2                                                                
025800 01  ERRTEXT.                                                             
025900     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
026000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
026100     EJECT                                                                
026200 77  KDRC-DISPLAY                PIC Z(5).                                
026300*    --- PARAMETERS FOR SUBPROGRAM W006PRS1                               
026400     EJECT                                                                
026500*01  -COPY W006PRAR                                                       
026600 01  W-NYSIDA-RAD10          PIC S9(3)   VALUE +910  COMP-3.              
026700*                                        WRITE ON NEW LINE 10             
026800 01  W-IDPRTLST                  PIC X(8).                                
026900*    --- PARAMETRAR TILL W510CURR                                         
027000*01  -COPY W510CURR                                                       
027100     SKIP2                                                                
027200*    --- AREAS FOR IMS-SECTIONS                                           
027300*                                                                         
027400     EJECT                                                                
027500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
027600     SKIP3                                                                
027700 01  KEYS-TO-DLI.                                                         
027800                                                                          
027900     03  W-IDSHIPM-X.                                                     
028000         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
028100                                                                          
028200     03  W-WDE111KY-X.                                                    
028300         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
028400         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
028500                                                                          
028600     03  W-WDE111KY-MIN.                                                  
028700         05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.          
028800         05  FILLER               PIC X(04)   VALUE LOW-VALUES.           
028900                                                                          
029000     03  W-WDE111KY-MAX.                                                  
029100         05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.          
029200         05  FILLER               PIC X(04)   VALUE HIGH-VALUES.          
029300                                                                          
029400     03  W-WDB101KY-X.                                                    
029500         05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                 
029600         05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                  
029700                                                                          
029800     03  W-WDB201KY-X.                                                    
029900         05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
030000         05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
030100                                                                          
030200     03  W-IDORDER-X.                                                     
030300         05  W-IDORDER           PIC S9(07)  VALUE ZERO COMP-3.           
030400                                                                          
030500*    03     -COPY WDGX01                                                  
030600                                                                          
030700*    03     -COPY WDGK4731                                                
030800                                                                          
030900*    03     -COPY WDGK4735                                                
031000                                                                          
031100                                                                          
031200*    --- STATUS-KOD FRÅN IMS                                              
031300 01  STATUS-WS                   PIC XX.                                  
031400     88  SEGMENT-FOUND                       VALUE '  '.                  
031500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
031600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
031700                                                                          
031800 01  GOOD-STATUSCODES.                                                    
031900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032000                                                                          
032100 01  SSA1                        PIC X(64).                               
032200 01  SSA2                        PIC X(64).                               
032300 01  SSA3                        PIC X(64).                               
032400     EJECT                                                                
032500                                                                          
032600 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
032700 01  SEND-AREA.                                                           
032800*    03  -COPY WZ01SEND                                                   
032900                                                                          
033000 01  SEND-RAD-STYRTECKEN.                                                 
033100     03  STYRTECKEN-RAD          PIC X.                                   
033200     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
033300                                                                          
033400 01  DAP-AREA-START              PIC X(24)   VALUE                        
033500                                             'DAP-AREA-START'.            
033600                                                                          
033700 01  FILLER                 PIC X(16)   VALUE 'HDR-AREA'.                 
033800 01  HDR-AREA.                                                            
033900*    03  -COPY WZ01REQU  -PRE HDR-                                        
034000*    03  -COPY WZ04HDR                                                    
034100                                                                          
034200*    ---  IMS FUNCTION CODES                                              
034300*01  -COPY W0003                                                          
034400     EJECT                                                                
034500                                                                          
034600*    ---  DLI INPUT-OUTPUT AREA                                           
034700 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDE101-WDE111'.               
034800 01  DLI-IO-WDE101-11.                                                    
034900     03  DLI-IO-WDE101.                                                   
035000*        05  -COPY WDE101                                                 
035100     03  DLI-IO-WDE111.                                                   
035200*        05  -COPY WDE111                                                 
035300     EJECT                                                                
035400                                                                          
035500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
035600 01  DLI-IO-WDE121.                                                       
035700*    03  -COPY WDE121                                                     
035800                                                                          
035900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE122'.                      
036000 01  DLI-IO-WDE122.                                                       
036100*    03  -COPY WDE122                                                     
036200                                                                          
036300 01  DLI-IO-AREA.                                                         
036400*    03  -COPY WDGX4731                                                   
036500                                                                          
036600*    03  -COPY WDGX4735                                                   
036700                                                                          
036800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
036900 01  DLI-IO-WDB101.                                                       
037000*    03  -COPY WDB101                                                     
037100                                                                          
037200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
037300 01  DLI-IO-WDB201.                                                       
037400*    03  -COPY WDB201                                                     
037500                                                                          
037600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
037700 01  DLI-IO-WDQ201.                                                       
037800*    03  -COPY WDQ201                                                     
037900                                                                          
038000     EJECT                                                                
038100                                                                          
038200 LINKAGE SECTION.                                                         
038300*01  -COPY W476TRPD                                                       
038400                                                                          
038500 01  ALT-PCB                     PIC X(32).                               
038600                                                                          
038700*01  -COPY W0008  -PRE WDE1-                                              
038800     05  FILLER                  PIC X.                                   
038900                                                                          
039000*01  -COPY W0008  -PRE 4731-                                              
039100     05  FILLER                  PIC X.                                   
039200                                                                          
039300*01  -COPY W0008  -PRE 4735-                                              
039400     05  FILLER                  PIC X.                                   
039500                                                                          
039600*01  -COPY W0008  -PRE WDB2-                                              
039700     05  FILLER                  PIC X.                                   
039800                                                                          
039900*01  -COPY W0008  -PRE WDB1-                                              
040000     05  FILLER                  PIC X.                                   
040100                                                                          
040200*01  -COPY W0008  -PRE WDQ2-                                              
040300     05  FILLER                  PIC X.                                   
040400                                                                          
040500*01  -COPY W0008  -PRE WDG2-                                              
040600     05  FILLER                  PIC X.                                   
040700                                                                          
040800     EJECT                                                                
040900                                                                          
041000 PROCEDURE DIVISION  USING                                                
041100                           TRPD-W476TRPD ALT-PCB WDE1-PCB                 
041200                           4731-PCB 4735-PCB                              
041300                           WDB2-PCB WDB1-PCB                              
041400                           WDQ2-PCB WDG2-PCB.                             
041500 MAIN SECTION.                                                            
041600     ENTRY 'DLITCBL' USING                                                
041700                           TRPD-W476TRPD ALT-PCB WDE1-PCB                 
041800                           4731-PCB 4735-PCB                              
041900                           WDB2-PCB WDB1-PCB                              
042000                           WDQ2-PCB WDG2-PCB.                             
042100     PERFORM A-INIT                                                       
042200                                                                          
042300     PERFORM IMS-GU-WDE101                                                
042400     MOVE SHIP-IDSHIPM        TO HDR-IDLIST                               
042500     MOVE SHIP-IDDC           TO WS-IDDC                                  
042600                                                                          
042700     IF ((CDC-SE OR DDC-SE) AND                                           
042800        (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                       
042900                           OR                                             
043000        ((CDC-SE OR DDC-SE) AND                                           
043010        (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL))                    
043100       PERFORM C-OMVANDLA-VALUTA                                          
043200     END-IF                                                               
043300                                                                          
043400     PERFORM IMS-GNP-WDE111                                               
043500     PERFORM UNTIL SEGMENT-MISSING                                        
043600       MOVE SGMT-IDDISTR  TO W-WDE111-IDDISTR                             
043700       MOVE SGMT-IDKUNDNR TO W-WDE111-IDKUNDNR                            
043800       MOVE SGMT-PRKURS   TO WS-PRKURS-LOCCUR                             
043900       PERFORM B-CARGO-DETAIL                                             
044000       PERFORM IMS-GNP-WDE111                                             
044100     END-PERFORM                                                          
044200                                                                          
044300     PERFORM E-PRINT-DOCUMENT                                             
044400                                                                          
044500     MOVE ZERO TO RETURN-CODE                                             
044600     GOBACK                                                               
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000 A-INIT SECTION.                                                          
045100     MOVE FUNCTION CURRENT-DATE (3:6)  TO TODAYS-DATE                     
045200                                                                          
046000     MOVE SPACE           TO  RAD1-HEAD                                   
046100                              RAD2-HEAD                                   
046200                              RAD2X-HEAD                                  
046300                              RAD3-HEAD                                   
046400                              RAD5-HEAD                                   
046500                              RAD1                                        
046600                              RAD2                                        
046700                              RAD3                                        
046800                              RAD4                                        
046900                              RAD5                                        
047000                              RAD6                                        
047100                              RAD7                                        
047200                              WS-KDVALISO                                 
047300     MOVE +1               TO INDX2                                       
047400     PERFORM UNTIL INDX2 > +16                                            
047500       MOVE SPACES         TO WS-BEFORSKN-RAD(INDX2)                      
047600       ADD +1              TO INDX2                                       
047700     END-PERFORM                                                          
047800                                                                          
047900     MOVE TRPD-IDPRTLST   TO  W-IDPRTLST                                  
048000     MOVE TRPD-IDSHIPM    TO  W-IDSHIPM                                   
048100     MOVE TRPD-PFDEF-OVR  TO  PRT-PFDEF-OVR                               
048200     MOVE TRPD-IDDISTR    TO  W-WDE111-IDDISTR-MIN                        
048300                              W-WDE111-IDDISTR-MAX                        
048400                              W-WDB201-IDDISTR                            
048500                              TEST-IDDISTR                                
048600                                                                          
048700     MOVE ZERO            TO  WS-TEMP-VALUE                               
048800                              WS-GOODS-VALUE                              
048900                              WS-GOODS-VALUE-EXC                          
049000                              WS-TOTAL-VALUE                              
049100                              WS-TOTAL-VALUE-EXC                          
049200                              WS-YYMMDD                                   
049300                              WS-SKOLLI-SUORDV                            
049400                              WS-PRFRAKT-VALUE                            
049500                              WS-PRFOERS-VALUE                            
049600                              WS-PRLEGKST-VALUE                           
049700                              WS-PREMBHNT-VALUE                           
049800                              WS-PRAVDRAG-VALUE                           
049900                              WS-PRFRAKT-VALUE-EXC                        
050000                              WS-PRFOERS-VALUE-EXC                        
050100                              WS-PRLEGKST-VALUE-EXC                       
050200                              WS-PREMBHNT-VALUE-EXC                       
050300                              WS-PRAVDRAG-VALUE-EXC                       
050400                              WS-PAGE-NO                                  
050500                              INDX                                        
050600                              INDX2                                       
050700                              WS-IX                                       
050800                              WS-IDDISTR                                  
050900                              WS-IDSHIPM-Z                                
051000                                                                          
051100     MOVE 2                TO W-KDSPRAK                                   
051200     IF TRPD-FLLDCKND = YES                                               
051300       MOVE 'CARGO-V-SPEC'  TO HDR-IDOUTTYPE                              
051400       MOVE YES             TO WEB-OUTPUT-SW                              
051500       MOVE 001             TO HDR-REQU-IDMSGVER                          
051600       MOVE SPACE           TO HDR-REQU-KDPGMACT                          
051700       MOVE SPACE           TO HDR-REQU-IDUSER                            
051800       MOVE +1              TO SEND-IDCOM                                 
051900     END-IF                                                               
052000     MOVE ZERO              TO WS-LDC-HDR                                 
052100     .                                                                    
052200     EJECT                                                                
052300 B-CARGO-DETAIL SECTION.                                                  
052400                                                                          
052500     PERFORM S20-HAMTA-WDB2                                               
052600                                                                          
052700     PERFORM BA-COMPLEMENTARY-VALUE                                       
052800                                                                          
052900     PERFORM BB-GOODS-VALUE                                               
053000                                                                          
053100     PERFORM BC-TOTAL-VALUE                                               
053200                                                                          
053300     PERFORM BD-DELIVERY-TERMS-TEXT                                       
053400                                                                          
053500     IF ((CDC-SE OR DDC-SE) AND                                           
053600        (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                       
053700                     OR                                                   
053800        ((CDC-SE OR DDC-SE) AND                                           
053900        (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL))                    
054000       CONTINUE                                                           
054100     ELSE                                                                 
054200       PERFORM BE-EQUAL-TO                                                
054300     END-IF                                                               
054400     .                                                                    
054500     EJECT                                                                
054600 BA-COMPLEMENTARY-VALUE SECTION.                                          
054700                                                                          
054800     PERFORM IMS-GNP-WDE122                                               
054900     IF SEGMENT-FOUND                                                     
055000       IF TILL-PRFRAKT > ZERO                                             
055100         COMPUTE WS-PRFRAKT-VALUE = WS-PRFRAKT-VALUE +                    
055200                                    TILL-PRFRAKT                          
055300       END-IF                                                             
055400                                                                          
055500       IF TILL-PRFOERS > ZERO                                             
055600         COMPUTE WS-PRFOERS-VALUE = WS-PRFOERS-VALUE +                    
055700                                    TILL-PRFOERS                          
055800       END-IF                                                             
055900                                                                          
056000       IF TILL-PRLEGKST > ZERO                                            
056100         COMPUTE WS-PRLEGKST-VALUE = WS-PRLEGKST-VALUE +                  
056200                                     TILL-PRLEGKST                        
056300       END-IF                                                             
056400                                                                          
056500       IF TILL-PREMBHNT > ZERO                                            
056600         COMPUTE WS-PREMBHNT-VALUE = WS-PREMBHNT-VALUE +                  
056700                                     TILL-PREMBHNT                        
056800       END-IF                                                             
056900                                                                          
057000       IF TILL-PRAVDRAG > ZERO                                            
057100         COMPUTE WS-PRAVDRAG-VALUE = WS-PRAVDRAG-VALUE +                  
057200                                     TILL-PRAVDRAG                        
057300       END-IF                                                             
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 BB-GOODS-VALUE SECTION.                                                  
057800                                                                          
057810     MOVE NOO                        TO SW-ISRAEL-C1-FC17                 
057820                                                                          
057900     PERFORM IMS-GNP-WDE121                                               
058000                                                                          
058100     PERFORM UNTIL SEGMENT-MISSING                                        
058200        PERFORM BBA-COMPUTE-VALUE                                         
058300        PERFORM IMS-GNP-WDE121                                            
058400     END-PERFORM                                                          
058500                                                                          
058600     PERFORM BBB-AFFIRMATION-TEXT                                         
058700     .                                                                    
058800     EJECT                                                                
058900 BBA-COMPUTE-VALUE SECTION.                                               
059000                                                                          
059100     MOVE ZERO                       TO WS-TEMP-VALUE                     
059200     IF SKOLLI-SUORDV > ZERO                                              
059300       MOVE SKOLLI-SUORDV            TO WS-TEMP-VALUE                     
059400                                        WS-SKOLLI-SUORDV                  
059500       MOVE 'SEK'                    TO WS-KDVALISO                       
059600     ELSE                                                                 
059700       COMPUTE WS-TEMP-VALUE =                                            
059800       SKOLLI-SUORDV-LOC + SKOLLI-SUORDV-LOCPREL                          
059900       MOVE SKOLLI-KDVALISO          TO WS-KDVALISO                       
060000     END-IF                                                               
060100                                                                          
060200     COMPUTE WS-GOODS-VALUE = WS-GOODS-VALUE + WS-TEMP-VALUE              
060210                                                                          
060220*FOR ISRAEL CLASS 1 AND FC 17 , INCOTERM SHOULD BE 'FCA'                  
060221     IF DIST25-ISRAEL-FRAKT AND                                           
060230        SKOLLI-KDORDKL  = 1 AND                                           
060240        SKOLLI-KDFRAKT  = 17                                              
060250        MOVE YES                     TO SW-ISRAEL-C1-FC17                 
060260     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 BBB-AFFIRMATION-TEXT SECTION.                                            
060600                                                                          
060700     IF SGMT-KDFORSKN > ZERO                                              
060800       MOVE '4731'                   TO IDHTYP                            
060900       MOVE LOW-VALUE                TO NYCKEL-VALFRI                     
061000       MOVE SGMT-KDFORSKN            TO KDFORSKN-4731                     
061100                                                                          
061200       MOVE +1                       TO INDX2                             
061300       MOVE ZERO                     TO WS-ANTAL-RAD-BEFORSKN             
061400                                                                          
061500       PERFORM IMS-4731-GET-ROOT                                          
061600                                                                          
061700       IF SEGMENT-FOUND                                                   
061800         PERFORM IMS-4731-GET-SEGMENT                                     
061900         PERFORM UNTIL SEGMENT-MISSING OR INDX2 > +16                     
062000           ADD +1                    TO WS-ANTAL-RAD-BEFORSKN             
062100           MOVE FOERS-BEFORSKN       TO WS-BEFORSKN-RAD(INDX2)            
062200           PERFORM IMS-4731-GET-SEGMENT                                   
062300           ADD +1                    TO INDX2                             
062400         END-PERFORM                                                      
062500       ELSE                                                               
062600         MOVE SPACE                  TO WS-BEFORSKN-RAD(INDX2)            
062700         MOVE ZERO                   TO WS-ANTAL-RAD-BEFORSKN             
062800         MOVE +17                    TO INDX2                             
062900       END-IF                                                             
063000                                                                          
063100       PERFORM UNTIL INDX2 > +16                                          
063200         MOVE SPACES                 TO WS-BEFORSKN-RAD(INDX2)            
063300         ADD +1                      TO INDX2                             
063400       END-PERFORM                                                        
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800 BC-TOTAL-VALUE SECTION.                                                  
063900                                                                          
064000     MOVE TRPD-IDDISTR    TO  TEST-IDDISTR                                
064100     MOVE SHIP-IDDC       TO  WS-IDDC                                     
064200                                                                          
064300*    OMVANDLAR TILL RESP VALUTA FÖRST,SUMMERAR SEN, FÖR ATT SLIPPA        
064400*    AVRUNDNINGSFEL I TOTAL                                               
064500                                                                          
064600     IF ((CDC-SE OR DDC-SE) AND                                           
064700        (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                       
064800                          OR                                              
064900        ((CDC-SE OR DDC-SE) AND                                           
064910        (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL))                    
065000                                                                          
065100       COMPUTE WS-GOODS-VALUE-EXC    ROUNDED = WS-GOODS-VALUE *           
065200                                               WS-PRKURS-EXC              
065300       COMPUTE WS-PRFRAKT-VALUE-EXC  ROUNDED = WS-PRFRAKT-VALUE *         
065400                                               WS-PRKURS-EXC              
065500       COMPUTE WS-PRFOERS-VALUE-EXC  ROUNDED = WS-PRFOERS-VALUE *         
065600                                               WS-PRKURS-EXC              
065700       COMPUTE WS-PRLEGKST-VALUE-EXC ROUNDED = WS-PRLEGKST-VALUE *        
065800                                               WS-PRKURS-EXC              
065900       COMPUTE WS-PREMBHNT-VALUE-EXC ROUNDED = WS-PREMBHNT-VALUE *        
066000                                               WS-PRKURS-EXC              
066100       COMPUTE WS-PRAVDRAG-VALUE-EXC ROUNDED = WS-PRAVDRAG-VALUE *        
066200                                               WS-PRKURS-EXC              
066300                                                                          
066400       COMPUTE WS-TOTAL-VALUE-EXC = WS-GOODS-VALUE-EXC  +                 
066500                                    WS-PRFRAKT-VALUE-EXC +                
066600                                    WS-PRFOERS-VALUE-EXC +                
066700                                    WS-PRLEGKST-VALUE-EXC +               
066800                                    WS-PREMBHNT-VALUE-EXC -               
066900                                    WS-PRAVDRAG-VALUE-EXC                 
067000     ELSE                                                                 
067100       COMPUTE WS-TOTAL-VALUE =     WS-GOODS-VALUE  +                     
067200                                    WS-PRFRAKT-VALUE +                    
067300                                    WS-PRFOERS-VALUE +                    
067400                                    WS-PRLEGKST-VALUE +                   
067500                                    WS-PREMBHNT-VALUE -                   
067600                                    WS-PRAVDRAG-VALUE                     
067700     END-IF                                                               
067800     .                                                                    
067900     EJECT                                                                
068000 BD-DELIVERY-TERMS-TEXT SECTION.                                          
068100                                                                          
068200     IF SGMT-KDLEVVIL > ZERO                                              
068300       MOVE '4735'                   TO IDHTYP                            
068400       MOVE LOW-VALUE                TO NYCKEL-VALFRI                     
068500       MOVE SGMT-KDLEVVIL            TO KDLEVVIL-4735                     
068600                                                                          
068700       PERFORM IMS-4735-GET-ROOT                                          
068800                                                                          
068900       IF SEGMENT-FOUND                                                   
069000         PERFORM IMS-4735-GET-SEGMENT                                     
069100         IF SEGMENT-FOUND                                                 
069200           MOVE +1                   TO INDX                              
069300           PERFORM UNTIL INDX > 6                                         
069400             MOVE LEVVIL-BELEVVIL(INDX)                                   
069500                            TO WS-DELIVERY-TERMS-TERMS(INDX)              
069600             ADD +1                  TO INDX                              
069700           END-PERFORM                                                    
069800         ELSE                                                             
069900           MOVE +1                   TO INDX                              
070000           PERFORM UNTIL INDX > 6                                         
070100             MOVE SPACE     TO WS-DELIVERY-TERMS-TERMS(INDX)              
070200             ADD +1                  TO INDX                              
070300           END-PERFORM                                                    
070400         END-IF                                                           
070500       ELSE                                                               
070600         MOVE +1                   TO INDX                                
070700         PERFORM UNTIL INDX > 6                                           
070800           MOVE SPACE     TO WS-DELIVERY-TERMS-TERMS(INDX)                
070900           ADD +1                  TO INDX                                
071000         END-PERFORM                                                      
071100       END-IF                                                             
071200       MOVE WS-DELIVERY-TERMS-TERMS(W-KDSPRAK) TO                         
071300            RAD5-DELIVERY-TERMS-TERMS                                     
071400     ELSE                                                                 
071500       PERFORM BDA-WRITE-SPEC-LEVVIL                                      
071600     END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900 BDA-WRITE-SPEC-LEVVIL SECTION.                                           
072000                                                                          
072100     MOVE SGMT-IDDISTR TO TEST-IDDISTR                                    
072200     IF DIST41-DDU-FRAKT                                                  
072300       MOVE       'DDU CONSIGNEE (INCOTERMS 2010) '   TO                  
072400                   RAD5-DELIVERY-TERMS-TERMS                              
072500     ELSE                                                                 
072600       IF DIST41-CIP-FRAKT                                                
072700         MOVE     'CIP                            '   TO                  
072800                   RAD5-DELIVERY-TERMS-TERMS                              
072810         IF ISRAEL-C1-FC17                                                
072820            MOVE  'FCA                            '   TO                  
072830                   RAD5-DELIVERY-TERMS-TERMS                              
072840         END-IF                                                           
072900       ELSE                                                               
073000         MOVE SGMT-IDDISTR TO TEST-IDDISTR                                
073100         MOVE SGMT-IDDC    TO WS-IDDC                                     
073200         IF WS-PRFRAKT-VALUE = ZERO                                       
073300           IF CDC-SE OR DDC-SE                                            
073400             EVALUATE TRUE                                                
073500             WHEN DIST76-ROMANIA                                          
073600               MOVE 'CIP CONSIGNEE                  '     TO              
073700                     RAD5-DELIVERY-TERMS-TERMS                            
073800             WHEN DIST76-RYSSLAND                                         
073900               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
074000                     RAD5-DELIVERY-TERMS-TERMS                            
074100             WHEN DIST76-RYSSLAND-2606                                    
074200               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
074300                     RAD5-DELIVERY-TERMS-TERMS                            
074400             WHEN DIST76-VITRYSSLAND                                      
074500               MOVE 'CIP DNEPROPETROVSK             '     TO              
074600                     RAD5-DELIVERY-TERMS-TERMS                            
074700             WHEN DIST76-PERU                                             
074800               MOVE 'FCA GOTHENBURG                 '     TO              
074900                     RAD5-DELIVERY-TERMS-TERMS                            
075000             WHEN DIST76-BELARUS                                          
075100               MOVE 'CIP                            '     TO              
075200                     RAD5-DELIVERY-TERMS-TERMS                            
075300             WHEN DIST76-MOLDAVIA                                         
075400               MOVE 'DAP CHISINAU                   '     TO              
075500                     RAD5-DELIVERY-TERMS-TERMS                            
075600             WHEN DIST76-OMAN                                             
075700               MOVE 'DDP                            '     TO              
075800                     RAD5-DELIVERY-TERMS-TERMS                            
075900             WHEN DIST76-COLUMBIA                                         
076000               MOVE 'FCA GOTHENBURG                 '     TO              
076100                     RAD5-DELIVERY-TERMS-TERMS                            
076200             WHEN DIST76-CANADA-REF                                       
076300               MOVE 'CIP                            '     TO              
076400                     RAD5-DELIVERY-TERMS-TERMS                            
076500             WHEN DIST76-SLOVENIA                                         
076600               MOVE 'CIP DEALER                     '     TO              
076700                     RAD5-DELIVERY-TERMS-TERMS                            
076800             WHEN DIST76-BOSNIA                                           
076900               MOVE 'CIP LJUBLJANA                  '     TO              
077000                     RAD5-DELIVERY-TERMS-TERMS                            
077100             WHEN DIST76-MACEDONIA                                        
077200               MOVE 'CIP LJUBLJANA                  '     TO              
077300                     RAD5-DELIVERY-TERMS-TERMS                            
077400             WHEN DIST76-SERBIA                                           
077500               MOVE 'CIP LJUBLJANA                  '     TO              
077600                     RAD5-DELIVERY-TERMS-TERMS                            
077700             WHEN DIST76-GEORGIA                                          
077800               MOVE 'CIP TBILISI                    '     TO              
077900                     RAD5-DELIVERY-TERMS-TERMS                            
078000             WHEN DIST76-TUNISIA                                          
078100               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
078200                     RAD5-DELIVERY-TERMS-TERMS                            
078300             WHEN OTHER                                                   
078400               MOVE 'FCA GÖTEBORG   (INCOTERMS 2010)'     TO              
078500                     RAD5-DELIVERY-TERMS-TERMS                            
078600             END-EVALUATE                                                 
078700                                                                          
078800           ELSE                                                           
078900             EVALUATE TRUE                                                
079000             WHEN DIST76-ROMANIA                                          
079100               MOVE 'CIP CONSIGNEE                  '     TO              
079200                     RAD5-DELIVERY-TERMS-TERMS                            
079300             WHEN DIST76-RYSSLAND                                         
079400               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
079500                     RAD5-DELIVERY-TERMS-TERMS                            
079600             WHEN DIST76-RYSSLAND-2606                                    
079700               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
079800                     RAD5-DELIVERY-TERMS-TERMS                            
079900             WHEN DIST76-VITRYSSLAND                                      
080000               MOVE 'CIP DNEPROPETROVSK             '     TO              
080100                     RAD5-DELIVERY-TERMS-TERMS                            
080200             WHEN DIST76-BELARUS                                          
080300               MOVE 'CIP                            '     TO              
080400                     RAD5-DELIVERY-TERMS-TERMS                            
080500             WHEN DIST76-MOLDAVIA                                         
080600               MOVE 'DAP CHISINAU                   '     TO              
080700                     RAD5-DELIVERY-TERMS-TERMS                            
080800             WHEN DIST76-OMAN                                             
080900               MOVE 'DDP                            '     TO              
081000                     RAD5-DELIVERY-TERMS-TERMS                            
081100             WHEN DIST76-SLOVENIA                                         
081200               MOVE 'CIP DEALER                     '     TO              
081300                     RAD5-DELIVERY-TERMS-TERMS                            
081400             WHEN DIST76-BOSNIA                                           
081500               MOVE 'CIP LJUBLJANA                  '     TO              
081600                     RAD5-DELIVERY-TERMS-TERMS                            
081700             WHEN DIST76-MACEDONIA                                        
081800               MOVE 'CIP LJUBLJANA                  '     TO              
081900                     RAD5-DELIVERY-TERMS-TERMS                            
082000             WHEN DIST76-SERBIA                                           
082100               MOVE 'CIP LJUBLJANA                  '     TO              
082200                     RAD5-DELIVERY-TERMS-TERMS                            
082300             WHEN DIST76-GEORGIA                                          
082400               MOVE 'CIP TBILISI                    '     TO              
082500                     RAD5-DELIVERY-TERMS-TERMS                            
082600             WHEN DIST76-TUNISIA                                          
082700               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
082800                     RAD5-DELIVERY-TERMS-TERMS                            
082900             WHEN OTHER                                                   
083000               MOVE 'FCA                            '     TO              
083100                     RAD5-DELIVERY-TERMS-TERMS                            
083200             END-EVALUATE                                                 
083300           END-IF                                                         
083400         ELSE                                                             
083500           IF WS-PRFOERS-VALUE = ZERO                                     
083600             EVALUATE TRUE                                                
083700             WHEN DIST76-ROMANIA                                          
083800               MOVE 'CIP CONSIGNEE                  '     TO              
083900                     RAD5-DELIVERY-TERMS-TERMS                            
084000             WHEN DIST76-RYSSLAND                                         
084100               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
084200                     RAD5-DELIVERY-TERMS-TERMS                            
084300             WHEN DIST76-RYSSLAND-2606                                    
084400               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
084500                     RAD5-DELIVERY-TERMS-TERMS                            
084600             WHEN DIST76-VITRYSSLAND                                      
084700               MOVE 'CIP DNEPROPETROVSK             '     TO              
084800                     RAD5-DELIVERY-TERMS-TERMS                            
084900             WHEN DIST76-PERU                                             
085000               MOVE 'CPT                            '     TO              
085100                     RAD5-DELIVERY-TERMS-TERMS                            
085200             WHEN DIST76-BELARUS                                          
085300               MOVE 'CIP                            '     TO              
085400                     RAD5-DELIVERY-TERMS-TERMS                            
085500             WHEN DIST76-MOLDAVIA                                         
085600               MOVE 'DAP CHISINAU                   '     TO              
085700                     RAD5-DELIVERY-TERMS-TERMS                            
085800             WHEN DIST76-OMAN                                             
085900               MOVE 'DDP                            '     TO              
086000                     RAD5-DELIVERY-TERMS-TERMS                            
086100             WHEN DIST76-COLUMBIA                                         
086200               MOVE 'CPT BOGOTA - COLOMBIA          '     TO              
086300                     RAD5-DELIVERY-TERMS-TERMS                            
086400             WHEN DIST76-CANADA-REF                                       
086500               MOVE 'CIP                            '     TO              
086600                     RAD5-DELIVERY-TERMS-TERMS                            
086700             WHEN DIST76-SLOVENIA                                         
086800               MOVE 'CIP DEALER                     '     TO              
086900                     RAD5-DELIVERY-TERMS-TERMS                            
087000             WHEN DIST76-BOSNIA                                           
087100               MOVE 'CIP LJUBLJANA                  '     TO              
087200                     RAD5-DELIVERY-TERMS-TERMS                            
087300             WHEN DIST76-MACEDONIA                                        
087400               MOVE 'CIP LJUBLJANA                  '     TO              
087500                     RAD5-DELIVERY-TERMS-TERMS                            
087600             WHEN DIST76-SERBIA                                           
087700               MOVE 'CIP LJUBLJANA                  '     TO              
087800                     RAD5-DELIVERY-TERMS-TERMS                            
087900             WHEN DIST76-GEORGIA                                          
088000               MOVE 'CIP TBILISI                    '     TO              
088100                     RAD5-DELIVERY-TERMS-TERMS                            
088200             WHEN DIST76-TUNISIA                                          
088300               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
088400                     RAD5-DELIVERY-TERMS-TERMS                            
088500             WHEN OTHER                                                   
088600               MOVE 'CPT            (INCOTERMS 2010)'     TO              
088700                       RAD5-DELIVERY-TERMS-TERMS                          
088800             END-EVALUATE                                                 
088900           ELSE                                                           
089000             EVALUATE TRUE                                                
089100             WHEN DIST76-ROMANIA                                          
089200               MOVE 'CIP CONSIGNEE                  '     TO              
089300                     RAD5-DELIVERY-TERMS-TERMS                            
089400             WHEN DIST76-RYSSLAND                                         
089500               MOVE 'CIP MOSCOW  (INCOTERMS 2010)   '     TO              
089600                     RAD5-DELIVERY-TERMS-TERMS                            
089700             WHEN DIST76-RYSSLAND-2606                                    
089800               MOVE 'FCA GOTHENBURG,SWE (INCOTERMS 2010)' TO              
089900                     RAD5-DELIVERY-TERMS-TERMS                            
090000             WHEN DIST76-VITRYSSLAND                                      
090100               MOVE 'CIP DNEPROPETROVSK             '     TO              
090200                     RAD5-DELIVERY-TERMS-TERMS                            
090300             WHEN DIST76-PERU                                             
090400               MOVE 'CIP                            '     TO              
090500                     RAD5-DELIVERY-TERMS-TERMS                            
090600             WHEN DIST76-BELARUS                                          
090700               MOVE 'CIP                            '     TO              
090800                     RAD5-DELIVERY-TERMS-TERMS                            
090900             WHEN DIST76-MOLDAVIA                                         
091000               MOVE 'DAP CHISINAU                   '     TO              
091100                     RAD5-DELIVERY-TERMS-TERMS                            
091200             WHEN DIST76-OMAN                                             
091300               MOVE 'DDP                            '      TO             
091400                     RAD5-DELIVERY-TERMS-TERMS                            
091500             WHEN DIST76-COLUMBIA                                         
091600               MOVE 'CPT BOGOTA - COLOMBIA          '      TO             
091700                     RAD5-DELIVERY-TERMS-TERMS                            
091800             WHEN DIST76-SLOVENIA                                         
091900               MOVE 'CIP DEALER                     '     TO              
092000                     RAD5-DELIVERY-TERMS-TERMS                            
092100             WHEN DIST76-BOSNIA                                           
092200               MOVE 'CIP LJUBLJANA                  '     TO              
092300                     RAD5-DELIVERY-TERMS-TERMS                            
092400             WHEN DIST76-MACEDONIA                                        
092500               MOVE 'CIP LJUBLJANA                  '     TO              
092600                     RAD5-DELIVERY-TERMS-TERMS                            
092700             WHEN DIST76-SERBIA                                           
092800               MOVE 'CIP LJUBLJANA                  '     TO              
092900                     RAD5-DELIVERY-TERMS-TERMS                            
093000             WHEN DIST76-GEORGIA                                          
093100               MOVE 'CIP TBILISI                    '     TO              
093200                     RAD5-DELIVERY-TERMS-TERMS                            
093300             WHEN DIST76-TUNISIA                                          
093400               MOVE 'CPT 2010 TUNIS-CARTHAGE AIRPORT'     TO              
093500                     RAD5-DELIVERY-TERMS-TERMS                            
093600             WHEN OTHER                                                   
093700               MOVE 'CIP            (INCOTERMS 2010)'      TO             
093800                     RAD5-DELIVERY-TERMS-TERMS                            
093900             END-EVALUATE                                                 
094000           END-IF                                                         
094100         END-IF                                                           
094200       END-IF                                                             
094300     END-IF                                                               
094400                                                                          
094500     IF CDC-SE OR DDC-SE                                                  
094600                                                                          
094700* FÖR BÅT TRANSPORTER DISTR. 7050-BRASILIEN, SKALL MAN HA EN ANNAN        
094800* LEV.VILLKORSTEXT, ENLIGT BJÖRN JENSEN, 29/5 '12.                        
094900* ANNAN TEXT GÄLLER ÄVEN FLYGTRANSPORTER, 01/11 '25 ENLIGT LINN A.        
095000                                                                          
095100       IF DIST76-BRASIL                                                   
095301         IF SHIP-IDTRPTNR < +100                                          
095400           MOVE 'CIP                            '          TO             
095501                       RAD5-DELIVERY-TERMS-TERMS                          
095600         END-IF                                                           
095700                                                                          
095800         IF SKOLLI-KDFRAKT = BOAT-TRP                                     
095900           MOVE 'FOB GÖTEBORG  (INCOTERMS 2010) '          TO             
096000                       RAD5-DELIVERY-TERMS-TERMS                          
096100         END-IF                                                           
096200       END-IF                                                             
096300                                                                          
096400* FÖR BÅT TRANSPORTER DISTR. COLUMBIA, DIST 7481 7482                     
096500* SKALL MAN HA EN ANNAN LEV.VILLKORSTEXT,                                 
096600* ENLIGT BJÖRN JENSEN, 20/8 '15.                                          
096700                                                                          
096800       IF DIST76-COLUMBIA                                                 
096900         IF SHIP-IDTRPTNR > +99                                           
097000           MOVE 'CPT BUENAVENTURA - COLUMBIA '             TO             
097100                       RAD5-DELIVERY-TERMS-TERMS                          
097200         END-IF                                                           
097300       END-IF                                                             
097400                                                                          
097500* FÖR FLYG TRANSPORTER DISTR. 4850-SAUDIARABIEN SKALL MAN HA EN           
097600* ANNAN LEV.VILLKORSTEXT, ENLIGT BJÖRN JENSEN, 26/11 '12.                 
097700       IF DIST76-SAUDI                                                    
097800         IF SHIP-IDTRPTNR < +100                                          
097900           MOVE 'FCA GLA GOTHENBURG (INCOTERMS 2010)'      TO             
098000                       RAD5-DELIVERY-TERMS-TERMS                          
098100         END-IF                                                           
098200       END-IF                                                             
098300                                                                          
098400* SPECIELL TEXT PÅ FAKTURN FÖR OMAN, DIST 6233                            
098500* GÄLLER BARA FÖR BÅTTRANSPORTER                                          
098600* 09/1 '20 ENLIGT BJÖRN JENSEN.                                           
098700                                                                          
098800       IF DIST76-OMAN                                                     
098900         IF SHIP-IDTRPTNR > +99                                           
099000           MOVE 'CPT SOHAR               '                 TO             
099100                       RAD5-DELIVERY-TERMS-TERMS                          
099200         END-IF                                                           
099300       END-IF                                                             
099400                                                                          
099500* SPECIELL TEXT PÅ FAKTURN FÖR UKRAINA DIST 2615                          
099600* GÄLLER BARA FÖR BIL O BÅTTRANSPORTER                                    
099700* 17/3 '20 ENLIGT BJÖRN JENSEN.                                           
099800                                                                          
099900       IF DIST76-UKRAINE                                                  
100000         IF SHIP-IDTRPTNR > +99                                           
100100           MOVE 'CIP COLOGNE             '                 TO             
100200                       RAD5-DELIVERY-TERMS-TERMS                          
100300         END-IF                                                           
100400       END-IF                                                             
100500                                                                          
101600                                                                          
101700     END-IF                                                               
101800     .                                                                    
101900     EJECT                                                                
102000 BE-EQUAL-TO SECTION.                                                     
102100                                                                          
102200     MOVE SGMT-PRKURS     TO WS-PRKURS                                    
102300     IF DIST79-DEALER-PRICE OR FLLOCCUR OR                                
102400        DIST79-ECOM-PRICE                                                 
102500       MOVE 'SEK' TO      WS-KDVALISO-2                                   
102600     ELSE                                                                 
102700       MOVE SGMT-KDVALISO TO WS-KDVALISO-2                                
102800       IF ( WS-PRFRAKT-VALUE  > ZERO OR                                   
102900            WS-PRFOERS-VALUE  > ZERO OR                                   
103000            WS-PRLEGKST-VALUE > ZERO OR                                   
103100            WS-PREMBHNT-VALUE > ZERO OR                                   
103200            WS-PRAVDRAG-VALUE > ZERO )                                    
103300         IF TILL-KDVALISO-MAN > ZERO AND                                  
103400            TILL-PRKURS-MAN > ZERO                                        
103500            MOVE TILL-KDVALISO-MAN TO WS-KDVALISO-2                       
103600            MOVE TILL-PRKURS-MAN TO WS-PRKURS                             
103700         END-IF                                                           
103800       END-IF                                                             
103900     END-IF                                                               
104000                                                                          
104100     IF DIST79-DEALER-PRICE OR                                            
104200        DIST79-ECOM-PRICE                                                 
104300       COMPUTE WS-TOTAL-VALUE-PRKURS ROUNDED =                            
104400               WS-TOTAL-VALUE * WS-PRKURS                                 
104500     ELSE                                                                 
104600       IF FLLOCCUR                                                        
104700         MOVE WS-TOTAL-VALUE   TO WS-TOTAL-VALUE-PRKURS                   
104800       ELSE                                                               
104900         COMPUTE WS-TOTAL-VALUE-PRKURS ROUNDED =                          
105000                 WS-TOTAL-VALUE / WS-PRKURS                               
105100       END-IF                                                             
105200     END-IF                                                               
105300     .                                                                    
105400     EJECT                                                                
105500                                                                          
105600 C-OMVANDLA-VALUTA SECTION.                                               
105700                                                                          
105800*    HÄMTA KURS FÖR OMRÄKNING TILL ANNAN VALUTA                           
105900     IF (CDC-SE OR DDC-SE) AND                                            
106000        (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                        
106100       MOVE 'INR'                    TO CURR-KDVALISO-ROW                 
106200     ELSE                                                                 
106300       IF (CDC-SE OR DDC-SE) AND                                          
106310          (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL)                   
106400         MOVE 'ZAR'                  TO CURR-KDVALISO-ROW                 
106500       END-IF                                                             
106600     END-IF                                                               
106700                                                                          
106800     MOVE TODAYS-DATE-YEAR           TO W-DATE-AAMM(1:2)                  
106900     MOVE TODAYS-DATE-MONTH          TO W-DATE-AAMM(3:2)                  
107000     MOVE W-DATE-AAMM                TO CURR-TIAAMM                       
107100     MOVE WS-KDVALISO-HUV            TO CURR-KDVALISO-HUV                 
107200     MOVE 'M'                        TO CURR-KDVALTYP                     
107300                                                                          
107400     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
107500     IF CURR-KDSVAR = ' '                                                 
107600        CONTINUE                                                          
107700     ELSE                                                                 
107800        MOVE 1                       TO CURR-PRKURS-NEW                   
107900     END-IF                                                               
108000     COMPUTE WS-PRKURS-EXC ROUNDED = 1 / CURR-PRKURS-NEW                  
108100     .                                                                    
108200                                                                          
108300     EJECT                                                                
108400 E-PRINT-DOCUMENT SECTION.                                                
108500                                                                          
108600     PERFORM S02-PRINT-META                                               
108700                                                                          
108800     PERFORM EA-PRINT-HEAD                                                
108900                                                                          
109000     PERFORM EB-PRINT-ROW1                                                
109100                                                                          
109200     PERFORM EC-PRINT-ROW2                                                
109300                                                                          
109400     PERFORM ED-PRINT-ROW3                                                
109500                                                                          
109600     PERFORM EE-PRINT-ROW4                                                
109700                                                                          
109800     PERFORM EF-PRINT-ROW5                                                
109900                                                                          
110000     PERFORM EG-PRINT-ROW6                                                
110100                                                                          
110200     PERFORM EH-PRINT-ROW7                                                
110300     IF WEB-OUTPUT                                                        
110400       IF WS-LDC-HDR = 0                                                  
110500         PERFORM S90-PUT-DAP-HEADER                                       
110600         MOVE 1                      TO WS-LDC-HDR                        
110700       END-IF                                                             
110800       PERFORM S11-SKAPA-W476CVS1                                         
110900       PERFORM S12-SKAPA-W476CVS2                                         
111000     END-IF                                                               
111100     .                                                                    
111200     EJECT                                                                
111300 EA-PRINT-HEAD SECTION.                                                   
111400     IF WEB-OUTPUT                                                        
111500       MOVE SPACE                    TO HDR-IDOUTREC                      
111600       MOVE SHIP-IDDC                TO HDR-IDOUTREC(1:2)                 
111700       MOVE '      '                 TO HDR-IDOUTREC(3:8)                 
111800       MOVE SKOLLI-IDORDER           TO W-IDORDER                         
111900       PERFORM IMS-GU-WDQ201                                              
112000       IF SEGMENT-FOUND                                                   
112100        IF OHUV-IDUSER > SPACE                                            
112200         MOVE OHUV-IDUSER            TO HDR-IDOUTREC(3:8)                 
112300                                        HDR-REQU-IDUSER                   
112400        ELSE                                                              
112500         MOVE OHUV-IDUSER            TO HDR-REQU-IDUSER                   
112600        END-IF                                                            
112700*        MOVE SPACE                  TO HDR-IDOUTREC(3:8)                 
112800       END-IF                                                             
112900     END-IF                                                               
113000     IF WS-IX = ZERO                                                      
113100       PERFORM S30-IMPORTER                                               
113200     END-IF                                                               
113300                                                                          
113400     MOVE SPACE                      TO SEND-RAD                          
113500     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
113600     PERFORM S90-PUT-DOC-LINE                                             
113700     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
113800     PERFORM S90-PUT-DOC-LINE                                             
113900     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
114000     PERFORM S90-PUT-DOC-LINE                                             
114100                                                                          
114200     ADD 1                           TO WS-PAGE-NO                        
114300     MOVE BEVARREF-LEDTEXT (W-KDSPRAK) TO RAD1H-IMPORTER-TEXT             
114400     MOVE WS-TYP-IDSHIP              TO RAD1-TYP-IDSHIP                   
114500     MOVE RAD1-HEAD                  TO ARB-RAD                           
114600                                        SEND-RAD                          
114700     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
114800     MOVE PRT-NYSIDA-RAD7            TO PRT-RADSKIP                       
114900     MOVE 7                          TO W-LINE-COUNT                      
115000     PERFORM S01-PRINT-LINE                                               
115100                                                                          
115200     MOVE SPACE                      TO SEND-RAD                          
115300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
115400*    PERFORM S90-PUT-DOC-LINE                                             
115500*    PERFORM S90-PUT-DOC-LINE                                             
115600                                                                          
115700     MOVE RAD2X-HEAD                 TO ARB-RAD                           
115800                                        SEND-RAD                          
115900     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
116000     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
116100     ADD 1                           TO W-LINE-COUNT                      
116200     PERFORM S01-PRINT-LINE                                               
116300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
116400                                                                          
116500     MOVE RAD3-HEAD                  TO ARB-RAD                           
116600                                        SEND-RAD                          
116700     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
116800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
116900     ADD 1                           TO W-LINE-COUNT                      
117000     PERFORM S01-PRINT-LINE                                               
117100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
117200                                                                          
117300     MOVE SPACE                      TO RAD2-HEAD                         
117400     MOVE BET-ADBETRAD-2             TO RAD4H-IMPORTER                    
117500     MOVE SHIP-TISKEPPN              TO WS-YYMMDD                         
117600     MOVE WS-YYMMDD                  TO RAD2H-TIYYMMDD                    
117700     MOVE SGMT-IDDISTR               TO RAD2H-IDDISTR                     
117800     MOVE SHIP-IDSHIPM               TO RAD2H-IDSHIPM                     
117900     MOVE SHIP-IDTRPTNR              TO RAD2H-IDTRPTNR                    
118000     MOVE SHIP-IDLBBET               TO RAD2H-IDLBBET                     
118100     MOVE WS-PAGE-NO                 TO RAD2H-PAGE-NO                     
118200     MOVE RAD2-HEAD                  TO ARB-RAD                           
118300                                        SEND-RAD                          
118400     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
118500     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
118600     ADD 1                           TO W-LINE-COUNT                      
118700     PERFORM S01-PRINT-LINE                                               
118800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
118900                                                                          
119000     MOVE RAD5-HEAD                  TO ARB-RAD                           
119100                                        SEND-RAD                          
119200     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
119300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
119400     ADD 1                           TO W-LINE-COUNT                      
119500     PERFORM S01-PRINT-LINE                                               
119600     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
119700     .                                                                    
119800     EJECT                                                                
119900 EB-PRINT-ROW1 SECTION.                                                   
120000                                                                          
120100     MOVE KDVALISO-LEDTEXT(W-KDSPRAK) TO RAD1-CURRENCY-TEXT               
120200     MOVE RAD1                       TO ARB-RAD                           
120300                                        SEND-RAD                          
120400     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
120500     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
120600     ADD 2                           TO W-LINE-COUNT                      
120700     PERFORM S01-PRINT-LINE                                               
120800     .                                                                    
120900     EJECT                                                                
121000 EC-PRINT-ROW2 SECTION.                                                   
121100                                                                          
121200     MOVE BEVAVARDE-LEDTEXT (W-KDSPRAK) TO RAD2-GOODS-VALUE-TEXT          
121300                                                                          
121400     IF (CDC-SE OR DDC-SE) AND                                            
121500        (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                        
121600       MOVE WS-GOODS-VALUE-EXC       TO RAD2-GOODS-VALUE                  
121700       MOVE 'INR'                    TO RAD2-KDVALISO                     
121800     ELSE                                                                 
121900       IF (CDC-SE OR DDC-SE) AND                                          
121910          (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL)                   
122000         MOVE WS-GOODS-VALUE-EXC     TO RAD2-GOODS-VALUE                  
122100         MOVE 'ZAR'                  TO RAD2-KDVALISO                     
122200       ELSE                                                               
122300         MOVE WS-GOODS-VALUE         TO RAD2-GOODS-VALUE                  
122400         MOVE WS-KDVALISO            TO RAD2-KDVALISO                     
122500         IF FLLOCCUR                                                      
122600           COMPUTE WS-MONEY ROUNDED = (WS-GOODS-VALUE *                   
122700             WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                       
122800           MOVE WS-MONEY             TO RAD2-GOODS-VALUE                  
122900           MOVE SGMT-KDVALISO        TO RAD2-KDVALISO                     
123000         END-IF                                                           
123100       END-IF                                                             
123200     END-IF                                                               
123300                                                                          
123400     MOVE RAD2                       TO ARB-RAD                           
123500                                        SEND-RAD                          
123600     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
123700     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
123800     ADD 2                           TO W-LINE-COUNT                      
123900     PERFORM S01-PRINT-LINE                                               
124000     .                                                                    
124100     EJECT                                                                
124200 ED-PRINT-ROW3 SECTION.                                                   
124300                                                                          
124400     IF WS-PRFRAKT-VALUE > ZERO                                           
124500       IF W-LINE-COUNT > W-LINE-MAX - 2                                   
124600         PERFORM EA-PRINT-HEAD                                            
124700         PERFORM EB-PRINT-ROW1                                            
124800       END-IF                                                             
124900       MOVE AVSLUT-PRFRAKT-LEDTEXT (W-KDSPRAK)                            
125000                                     TO RAD3-COMPLIMENTARY-TEXT           
125100       IF (CDC-SE OR DDC-SE) AND                                          
125200          (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                      
125300         MOVE WS-PRFRAKT-VALUE-EXC   TO RAD3-COMPLIMENTARY-VALUE          
125400         MOVE 'INR'                  TO RAD3-KDVALISO                     
125500       ELSE                                                               
125600         IF (CDC-SE OR DDC-SE) AND                                        
125610            (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL)                 
125700           MOVE WS-PRFRAKT-VALUE-EXC TO RAD3-COMPLIMENTARY-VALUE          
125800           MOVE 'ZAR'                TO RAD3-KDVALISO                     
125900         ELSE                                                             
126000           MOVE WS-PRFRAKT-VALUE     TO RAD3-COMPLIMENTARY-VALUE          
126100           MOVE WS-KDVALISO          TO RAD3-KDVALISO                     
126200           IF FLLOCCUR                                                    
126300            COMPUTE WS-MONEY ROUNDED = (WS-PRFRAKT-VALUE *                
126400             WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                       
126500            MOVE WS-MONEY            TO RAD3-COMPLIMENTARY-VALUE          
126600            MOVE SGMT-KDVALISO       TO RAD3-KDVALISO                     
126700           END-IF                                                         
126800         END-IF                                                           
126900       END-IF                                                             
127000       MOVE RAD3                     TO ARB-RAD                           
127100                                        SEND-RAD                          
127200       MOVE PRT-AFTER-2              TO PRT-RADSKIP                       
127300       MOVE WS-SKIP2                 TO STYRTECKEN-RAD                    
127400       ADD 2                         TO W-LINE-COUNT                      
127500       PERFORM S01-PRINT-LINE                                             
127600     END-IF                                                               
127700                                                                          
127800     IF WS-PRFOERS-VALUE > ZERO                                           
127900       IF W-LINE-COUNT > W-LINE-MAX - 2                                   
128000         PERFORM EA-PRINT-HEAD                                            
128100         PERFORM EB-PRINT-ROW1                                            
128200       END-IF                                                             
128300       MOVE AVSLUT-PRFOERS-LEDTEXT (W-KDSPRAK)                            
128400                                     TO RAD3-COMPLIMENTARY-TEXT           
128500       IF (CDC-SE OR DDC-SE) AND                                          
128600          (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                      
128700         MOVE WS-PRFOERS-VALUE-EXC   TO RAD3-COMPLIMENTARY-VALUE          
128800         MOVE 'INR'                  TO RAD3-KDVALISO                     
128900       ELSE                                                               
129000         IF (CDC-SE OR DDC-SE) AND                                        
129010            (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL)                 
129100           MOVE WS-PRFOERS-VALUE-EXC TO RAD3-COMPLIMENTARY-VALUE          
129200           MOVE 'ZAR'                TO RAD3-KDVALISO                     
129300         ELSE                                                             
129400           MOVE WS-PRFOERS-VALUE     TO RAD3-COMPLIMENTARY-VALUE          
129500           MOVE WS-KDVALISO          TO RAD3-KDVALISO                     
129600           IF FLLOCCUR                                                    
129700            COMPUTE WS-MONEY ROUNDED = (WS-PRFOERS-VALUE *                
129800             WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                       
129900            MOVE WS-MONEY            TO RAD3-COMPLIMENTARY-VALUE          
130000            MOVE SGMT-KDVALISO       TO RAD3-KDVALISO                     
130100           END-IF                                                         
130200         END-IF                                                           
130300         MOVE RAD3                   TO ARB-RAD                           
130400                                          SEND-RAD                        
130500         MOVE PRT-AFTER-1            TO PRT-RADSKIP                       
130600         MOVE WS-SKIP1               TO STYRTECKEN-RAD                    
130700         ADD 2                       TO W-LINE-COUNT                      
130800         PERFORM S01-PRINT-LINE                                           
130900       END-IF                                                             
131000     END-IF                                                               
131100                                                                          
131200     IF WS-PRLEGKST-VALUE > ZERO                                          
131300       IF W-LINE-COUNT > W-LINE-MAX - 2                                   
131400         PERFORM EA-PRINT-HEAD                                            
131500         PERFORM EB-PRINT-ROW1                                            
131600       END-IF                                                             
131700       MOVE AVSLUT-PRLEGKST-LEDTEXT (W-KDSPRAK)                           
131800                                     TO RAD3-COMPLIMENTARY-TEXT           
131900       IF (CDC-SE OR DDC-SE) AND                                          
132000          (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                      
132100         MOVE WS-PRLEGKST-VALUE-EXC  TO RAD3-COMPLIMENTARY-VALUE          
132200         MOVE 'INR'                  TO RAD3-KDVALISO                     
132300       ELSE                                                               
132400         IF (CDC-SE OR DDC-SE) AND                                        
132410            (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL)                 
132500           MOVE WS-PRLEGKST-VALUE-EXC TO RAD3-COMPLIMENTARY-VALUE         
132600           MOVE 'ZAR'                TO RAD3-KDVALISO                     
132700         ELSE                                                             
132800           MOVE WS-PRLEGKST-VALUE    TO RAD3-COMPLIMENTARY-VALUE          
132900           MOVE WS-KDVALISO          TO RAD3-KDVALISO                     
133000           IF FLLOCCUR                                                    
133100            COMPUTE WS-MONEY ROUNDED = (WS-PRLEGKST-VALUE *               
133200             WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                       
133300            MOVE WS-MONEY            TO RAD3-COMPLIMENTARY-VALUE          
133400            MOVE SGMT-KDVALISO       TO RAD3-KDVALISO                     
133500           END-IF                                                         
133600         END-IF                                                           
133700         MOVE RAD3                   TO ARB-RAD                           
133800                                          SEND-RAD                        
133900         MOVE PRT-AFTER-1            TO PRT-RADSKIP                       
134000         MOVE WS-SKIP1               TO STYRTECKEN-RAD                    
134100         ADD 2                       TO W-LINE-COUNT                      
134200         PERFORM S01-PRINT-LINE                                           
134300       END-IF                                                             
134400     END-IF                                                               
134500                                                                          
134600     IF WS-PREMBHNT-VALUE > ZERO                                          
134700       IF W-LINE-COUNT > W-LINE-MAX - 2                                   
134800         PERFORM EA-PRINT-HEAD                                            
134900         PERFORM EB-PRINT-ROW1                                            
135000       END-IF                                                             
135100       MOVE AVSLUT-PREMBHNT-LEDTEXT (W-KDSPRAK)                           
135200                                     TO RAD3-COMPLIMENTARY-TEXT           
135300       IF (CDC-SE OR DDC-SE) AND                                          
135400          (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                      
135500         MOVE WS-PREMBHNT-VALUE-EXC  TO RAD3-COMPLIMENTARY-VALUE          
135600         MOVE 'INR'                  TO RAD3-KDVALISO                     
135700       ELSE                                                               
135800         IF (CDC-SE OR DDC-SE) AND                                        
135810            (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL)                 
135900           MOVE WS-PREMBHNT-VALUE-EXC TO RAD3-COMPLIMENTARY-VALUE         
136000           MOVE 'ZAR'                TO RAD3-KDVALISO                     
136100         ELSE                                                             
136200           MOVE WS-PREMBHNT-VALUE    TO RAD3-COMPLIMENTARY-VALUE          
136300           MOVE WS-KDVALISO          TO RAD3-KDVALISO                     
136400           IF FLLOCCUR                                                    
136500            COMPUTE WS-MONEY ROUNDED = (WS-PREMBHNT-VALUE *               
136600             WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                       
136700            MOVE WS-MONEY            TO RAD3-COMPLIMENTARY-VALUE          
136800            MOVE SGMT-KDVALISO       TO RAD3-KDVALISO                     
136900           END-IF                                                         
137000         END-IF                                                           
137100       END-IF                                                             
137200                                                                          
137300       IF WS-PRLEGKST-VALUE > ZERO                                        
137400         MOVE RAD3                     TO ARB-RAD                         
137500                                          SEND-RAD                        
137600         MOVE PRT-AFTER-1              TO PRT-RADSKIP                     
137700         MOVE WS-SKIP1                 TO STYRTECKEN-RAD                  
137800         ADD 1                         TO W-LINE-COUNT                    
137900       ELSE                                                               
138000         MOVE RAD3                     TO ARB-RAD                         
138100                                          SEND-RAD                        
138200         MOVE PRT-AFTER-2              TO PRT-RADSKIP                     
138300         MOVE WS-SKIP2                 TO STYRTECKEN-RAD                  
138400         ADD 2                         TO W-LINE-COUNT                    
138500       END-IF                                                             
138600       PERFORM S01-PRINT-LINE                                             
138700     END-IF                                                               
138800                                                                          
138900     IF WS-PRAVDRAG-VALUE > ZERO                                          
139000       IF W-LINE-COUNT > W-LINE-MAX - 2                                   
139100         PERFORM EA-PRINT-HEAD                                            
139200         PERFORM EB-PRINT-ROW1                                            
139300       END-IF                                                             
139400       MOVE AVSLUT-PRAVDRAG-LEDTEXT (W-KDSPRAK)                           
139500                                     TO RAD3-COMPLIMENTARY-TEXT           
139600       IF (CDC-SE OR DDC-SE) AND                                          
139700          (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                      
139800         MOVE WS-PRAVDRAG-VALUE-EXC  TO RAD3-COMPLIMENTARY-VALUE          
139900         MOVE 'INR'                  TO RAD3-KDVALISO                     
140000       ELSE                                                               
140100         IF (CDC-SE OR DDC-SE) AND                                        
140110            (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL)                 
140200           MOVE WS-PRAVDRAG-VALUE-EXC TO RAD3-COMPLIMENTARY-VALUE         
140300           MOVE 'ZAR'                TO RAD3-KDVALISO                     
140400         ELSE                                                             
140500           MOVE WS-PRAVDRAG-VALUE    TO RAD3-COMPLIMENTARY-VALUE          
140600           MOVE WS-KDVALISO          TO RAD3-KDVALISO                     
140700           IF FLLOCCUR                                                    
140800            COMPUTE WS-MONEY ROUNDED = (WS-PRAVDRAG-VALUE *               
140900             WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                       
141000            MOVE WS-MONEY            TO RAD3-COMPLIMENTARY-VALUE          
141100            MOVE SGMT-KDVALISO       TO RAD3-KDVALISO                     
141200           END-IF                                                         
141300         END-IF                                                           
141400       END-IF                                                             
141500                                                                          
141600       IF WS-PRLEGKST-VALUE > ZERO OR WS-PREMBHNT-VALUE > ZERO            
141700         MOVE RAD3                     TO ARB-RAD                         
141800                                          SEND-RAD                        
141900         MOVE PRT-AFTER-1              TO PRT-RADSKIP                     
142000         MOVE WS-SKIP1                 TO STYRTECKEN-RAD                  
142100         ADD 1                         TO W-LINE-COUNT                    
142200       ELSE                                                               
142300         MOVE RAD3                     TO ARB-RAD                         
142400                                          SEND-RAD                        
142500         MOVE PRT-AFTER-2              TO PRT-RADSKIP                     
142600         MOVE WS-SKIP2                 TO STYRTECKEN-RAD                  
142700         ADD 2                         TO W-LINE-COUNT                    
142800       END-IF                                                             
142900       PERFORM S01-PRINT-LINE                                             
143000     END-IF                                                               
143100     .                                                                    
143200     EJECT                                                                
143300 EE-PRINT-ROW4 SECTION.                                                   
143400                                                                          
143500     IF W-LINE-COUNT > W-LINE-MAX - 2                                     
143600       PERFORM EA-PRINT-HEAD                                              
143700       PERFORM EB-PRINT-ROW1                                              
143800     END-IF                                                               
143900     MOVE SUFKTBEL-LEDTEXT (W-KDSPRAK) TO RAD4-TOTAL-VALUE-TEXT           
144000                                                                          
144100     MOVE TRPD-IDDISTR    TO  TEST-IDDISTR                                
144200     MOVE SHIP-IDDC       TO  WS-IDDC                                     
144300     IF (CDC-SE OR DDC-SE) AND                                            
144400        (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL)                        
144500       MOVE WS-TOTAL-VALUE-EXC       TO RAD4-TOTAL-VALUE                  
144600       MOVE 'INR'                    TO RAD4-KDVALISO                     
144700     ELSE                                                                 
144800       IF (CDC-SE OR DDC-SE) AND                                          
144810          (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL)                   
144900         MOVE WS-TOTAL-VALUE-EXC     TO RAD4-TOTAL-VALUE                  
145000         MOVE 'ZAR'                  TO RAD4-KDVALISO                     
145100       ELSE                                                               
145200         MOVE WS-TOTAL-VALUE         TO RAD4-TOTAL-VALUE                  
145300         MOVE WS-KDVALISO            TO RAD4-KDVALISO                     
145400         IF FLLOCCUR                                                      
145500          COMPUTE WS-MONEY ROUNDED = (WS-TOTAL-VALUE *                    
145600           WS-REVALUTA-LOCCUR) / WS-PRKURS-LOCCUR                         
145700          MOVE WS-MONEY              TO RAD4-TOTAL-VALUE                  
145800          MOVE SGMT-KDVALISO         TO RAD4-KDVALISO                     
145900         END-IF                                                           
146000       END-IF                                                             
146100     END-IF                                                               
146200     MOVE RAD4                       TO ARB-RAD                           
146300                                        SEND-RAD                          
146400     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
146500     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
146600     ADD 2                           TO W-LINE-COUNT                      
146700     PERFORM S01-PRINT-LINE                                               
146800     .                                                                    
146900     EJECT                                                                
147000 EF-PRINT-ROW5 SECTION.                                                   
147100                                                                          
147200     IF W-LINE-COUNT > W-LINE-MAX - 2                                     
147300       PERFORM EA-PRINT-HEAD                                              
147400     END-IF                                                               
147500     MOVE BELEVVIL-LEDTEXT (W-KDSPRAK)                                    
147600                                     TO RAD5-DELIVERY-TERMS-TEXT          
147700     MOVE RAD5                       TO ARB-RAD                           
147800                                        SEND-RAD                          
147900     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
148000     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
148100     ADD 2                           TO W-LINE-COUNT                      
148200     PERFORM S01-PRINT-LINE                                               
148300     .                                                                    
148400     EJECT                                                                
148500 EG-PRINT-ROW6 SECTION.                                                   
148600                                                                          
148700     IF ((CDC-SE OR DDC-SE) AND                                           
148800        (DIST34-INDIA-NDC OR DIST35-CDC-IN-REFILL))                       
148900                         OR                                               
149000        ((CDC-SE OR DDC-SE) AND                                           
149010        (DIS121-SOUTH-AFRICA OR DIST35-CDC-ZA-REFILL))                    
149100       CONTINUE                                                           
149200     ELSE                                                                 
149300       IF W-LINE-COUNT > W-LINE-MAX - 2                                   
149400         PERFORM EA-PRINT-HEAD                                            
149500       END-IF                                                             
149600       MOVE BELIKAMED-LEDTEXT (W-KDSPRAK) TO RAD6-EQUAL-TO-TEXT           
149700       MOVE WS-KDVALISO-2             TO RAD6-EQUAL-TO-KDVALISO           
149800       MOVE PRKURS-LEDTEXT (W-KDSPRAK) TO RAD6-RATE-TEXT                  
149900       MOVE WS-PRKURS                 TO RAD6-RATE-PRKURS                 
150000       MOVE WS-TOTAL-VALUE-PRKURS     TO RAD6-TOTAL-VALUE-PRKURS          
150100       MOVE RAD6                      TO ARB-RAD                          
150200                                           SEND-RAD                       
150300       MOVE PRT-AFTER-2               TO PRT-RADSKIP                      
150400       MOVE WS-SKIP2                  TO STYRTECKEN-RAD                   
150500       ADD 2                          TO W-LINE-COUNT                     
150600       PERFORM S01-PRINT-LINE                                             
150700     END-IF                                                               
150800     .                                                                    
150900     EJECT                                                                
151000 EH-PRINT-ROW7 SECTION.                                                   
151100                                                                          
151200     MOVE SPACES                    TO ARB-RAD                            
151300                                       SEND-RAD                           
151400     MOVE PRT-AFTER-1               TO PRT-RADSKIP                        
151500     MOVE WS-SKIP1                  TO STYRTECKEN-RAD                     
151600     ADD 1                          TO W-LINE-COUNT                       
151700     PERFORM S01-PRINT-LINE                                               
151800                                                                          
151900     IF W-LINE-COUNT > W-LINE-MAX - WS-ANTAL-RAD-BEFORSKN                 
152000       PERFORM EA-PRINT-HEAD                                              
152100     END-IF                                                               
152200                                                                          
152300     MOVE +1                          TO INDX2                            
152400     PERFORM UNTIL INDX2 > WS-ANTAL-RAD-BEFORSKN                          
152500       MOVE WS-BEFORSKN-RAD(INDX2)    TO RAD7-AFFIRMATION-TEXT            
152600       MOVE RAD7                      TO ARB-RAD                          
152700                                         SEND-RAD                         
152800       MOVE PRT-AFTER-1               TO PRT-RADSKIP                      
152900       MOVE WS-SKIP1                  TO STYRTECKEN-RAD                   
153000       ADD 1                          TO W-LINE-COUNT                     
153100       PERFORM S01-PRINT-LINE                                             
153200       ADD 1                          TO INDX2                            
153300     END-PERFORM                                                          
153400     .                                                                    
153500     EJECT                                                                
153600 S01-PRINT-LINE SECTION.                                                  
153700                                                                          
153800     IF NOT WEB-OUTPUT                                                    
153900       PERFORM S90-PUT-DOC-LINE                                           
154000       IF SHIP-FLSKRIV-NU = YES OR TRPD-IDPGM = 'W4062200'                
154100         CALL W006PRS1 USING PRT-SPOOL-OVR                                
154200                             PRT-WRITE                                    
154300                             W-IDPRTLST                                   
154400                             ALT-PCB                                      
154500                             PRT-RADSKIP                                  
154600                             ARB-RAD                                      
154700       END-IF                                                             
154800     END-IF                                                               
154900     .                                                                    
155000     EJECT                                                                
155100                                                                          
155200 S02-PRINT-META SECTION.                                                  
155300                                                                          
155400     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
155500     MOVE SHIP-IDSHIPM       TO WS-IDSHIPM-Z                              
155600     STRING WS-META                                                       
155700            'SHIPMENT_NUMBER='                                            
155800            WS-IDSHIPM-Z                                                  
155900            DELIMITED BY SIZE INTO SEND-RAD                               
156000                                                                          
156100     PERFORM S90-PUT-DOC-LINE                                             
156200                                                                          
156300     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
156400     STRING WS-META                                                       
156500            'DOCUMENT_TYPE='                                              
156600            WS-TYP-IDSHIP                                                 
156700            DELIMITED BY SIZE INTO SEND-RAD                               
156800                                                                          
156900     PERFORM S90-PUT-DOC-LINE                                             
157000                                                                          
157100     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
157200     MOVE SHIP-TISKEPPN      TO WS-YYMMDD                                 
157300     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
157400                                                                          
157500     STRING WS-META                                                       
157600            'SHIPPING_DATE='                                              
157700            WS-YEAR(1:2)                                                  
157800            WS-YYMMDD                                                     
157900            DELIMITED BY SIZE INTO SEND-RAD                               
158000                                                                          
158100     PERFORM S90-PUT-DOC-LINE                                             
158200                                                                          
158300     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
158400     MOVE SGMT-IDDISTR       TO WS-IDDISTR                                
158500     STRING WS-META                                                       
158600            'DISTRICT_NUMBER='                                            
158700            WS-IDDISTR                                                    
158800            DELIMITED BY SIZE INTO SEND-RAD                               
158900                                                                          
159000     PERFORM S90-PUT-DOC-LINE                                             
159100                                                                          
159200     MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
159300                                                                          
159400     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
159500     MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
159600     MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
159700     MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
159800     MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
159900     MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
160000                                                                          
160100     STRING WS-META                                                       
160200            'FILE_NAME='                                                  
160300            DELIMITED BY SIZE                                             
160400            'SHIPDOC_CVS'                                                 
160500            DELIMITED BY SIZE                                             
160600            '_'                                                           
160700            DELIMITED BY SIZE                                             
160800            FUNCTION TRIM (WS-IDSHIPM-Z)                                  
160900            DELIMITED BY SIZE                                             
161000            '_'                                                           
161100            FUNCTION TRIM (WS-IDDISTR)                                    
161200            DELIMITED BY SIZE                                             
161300            '_'                                                           
161400            DELIMITED BY SIZE                                             
161500            WS-TIMESTAMP                                                  
161600            DELIMITED BY SIZE INTO SEND-RAD                               
161700                                                                          
161800     PERFORM S90-PUT-DOC-LINE                                             
161900     .                                                                    
162000                                                                          
162100 S11-SKAPA-W476CVS1  SECTION.                                             
162200                                                                          
162300     MOVE '1'                    TO HDR-IDAFPRCD                          
162400     MOVE SHIP-IDDC              TO HDR-IDDC                              
162500     MOVE RAD1H-IMPORTER-TEXT    TO HDR-RAD1H-IMPORTER-TEXT               
162600     MOVE RAD1H-IMPORTER         TO HDR-RAD1H-IMPORTER                    
162700     MOVE RAD2H-IMPORTER         TO HDR-RAD2H-IMPORTER                    
162800     MOVE RAD3H-IMPORTER         TO HDR-RAD3H-IMPORTER                    
162900     MOVE RAD4H-IMPORTER         TO HDR-RAD4H-IMPORTER                    
163000     MOVE RAD5H-IMPORTER         TO HDR-RAD5H-IMPORTER                    
163100     MOVE RAD2H-TIYYMMDD         TO HDR-RAD1-TIAAMMDD                     
163200     MOVE RAD2H-IDDISTR          TO HDR-RAD1-IDDISTR                      
163300     MOVE RAD2H-IDSHIPM          TO HDR-RAD1-IDSHIPM                      
163400     MOVE RAD2H-IDTRPTNR         TO HDR-RAD1-IDTRPTNR                     
163500     MOVE RAD2H-IDLBBET          TO HDR-RAD1-IDLBBET                      
163600     MOVE HDR-W476CVS1           TO DOC-AREA                              
163700     MOVE LENGTH OF HDR-W476CVS1 TO SEND-KVDLEN                           
163800     PERFORM S90-PUT-DOC                                                  
163900     .                                                                    
164000     EJECT                                                                
164100 S12-SKAPA-W476CVS2  SECTION.                                             
164200                                                                          
164300     MOVE '2'                    TO SUBHDR-IDAFPRCD                       
164400     MOVE RAD2-GOODS-VALUE       TO SUBHDR-RAD2-GOODS-VALUE               
164500     MOVE RAD2-KDVALISO          TO SUBHDR-RAD2-KDVALISO                  
164600     MOVE RAD3-COMPLIMENTARY-VALUE                                        
164700                                 TO SUBHDR-RAD3-COMPL-VALUE               
164800     MOVE RAD3-KDVALISO          TO SUBHDR-RAD3-KDVALISO                  
164900     MOVE RAD4-TOTAL-VALUE       TO SUBHDR-RAD4-TOTAL-VALUE               
165000     MOVE RAD4-KDVALISO          TO SUBHDR-RAD4-KDVALISO                  
165100     MOVE RAD5-DELIVERY-TERMS-TERMS                                       
165200                                 TO SUBHDR-RAD5-DEL-TERMS-TERMS           
165300     MOVE RAD6-EQUAL-TO-KDVALISO TO SUBHDR-RAD6-EQUAL-TO-KDVALISO         
165400     MOVE RAD6-RATE-PRKURS       TO SUBHDR-RAD6-RATE-PRKURS               
165500     MOVE RAD6-TOTAL-VALUE-PRKURS                                         
165600                                TO SUBHDR-RAD6-TOTAL-VALUE-PRKURS         
165700     MOVE RAD7-AFFIRMATION-TEXT  TO SUBHDR-RAD7-AFFIRMATION-TEXT          
165800     MOVE SUBHDR-W476CVS2            TO DOC-AREA                          
165900     MOVE LENGTH OF SUBHDR-W476CVS2  TO SEND-KVDLEN                       
166000     PERFORM S90-PUT-DOC                                                  
166100     .                                                                    
166200     EJECT                                                                
166300 S20-HAMTA-WDB2  SECTION.                                                 
166400                                                                          
166500     MOVE SGMT-IDKUNDNR      TO W-WDB201-IDKUNDNR                         
166600     PERFORM IMS-GU-WDB201                                                
166700     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
166800       MOVE +2 TO W-KDSPRAK                                               
166900     ELSE                                                                 
167000       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
167100     END-IF                                                               
167200     .                                                                    
167300     EJECT                                                                
167400 S30-IMPORTER SECTION.                                                    
167500                                                                          
167600     MOVE SGMT-IDKUNDNR  TO W-WDE111-IDKUNDNR                             
167700                            W-WDB201-IDKUNDNR                             
167800     PERFORM IMS-GU-WDB201                                                
167900     MOVE SGMT-IDPARTNR  TO W-WDB101-IDPARTNR                             
168000     MOVE GMT-IDFTG      TO W-WDB101-IDFTG                                
168100     PERFORM IMS-GU-WDB101                                                
168200     MOVE BET-BEBETRAD-1 TO RAD1H-IMPORTER                                
168300     MOVE BET-BEBETRAD-2 TO RAD2H-IMPORTER                                
168400     MOVE BET-ADBETRAD-1 TO RAD3H-IMPORTER                                
168500     MOVE BET-ADBETRAD-2 TO RAD4H-IMPORTER                                
168600     MOVE BET-BELAND-SVE TO RAD5H-IMPORTER                                
168700     MOVE BET-FLLOCCUR   TO FLLOCCUR-SW                                   
168800     ADD +1 TO WS-IX                                                      
168900     .                                                                    
169000     EJECT                                                                
169100 S90-PUT-DOC-LINE SECTION.                                                
169200     IF (TRPD-IDPGM = 'W4063600' AND TRPD-KVCOPIES = '1')                 
169300                                 OR                                       
169400        (TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1')                 
169500                                                                          
169600       IF TRPD-FLSKRIV-ONDEM = YES AND NOT WEB-OUTPUT                     
169700         MOVE +1                          TO SEND-IDCOM                   
169800         MOVE 'PUT'                       TO SEND-KDFUNC                  
169900         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
170000         CALL WZ01SEND USING SEND-CONTROL-AREA                            
170100                             SEND-KVDLEN                                  
170200                             SEND-RAD-STYRTECKEN                          
170300         IF SEND-KDRC > ZERO                                              
170400           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
170500           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
170600           DELIMITED BY SIZE INTO ERRTEXT-STR                             
170700           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
170800         END-IF                                                           
170900       END-IF                                                             
171000     END-IF                                                               
171100     .                                                                    
171200     EJECT                                                                
171300 S90-PUT-DAP-HEADER SECTION.                                              
171400                                                                          
171500*    MOVE 'S90-PUT-DAP-HE' TO CURR-SECTION                                
171600                                                                          
171700     MOVE 'PUT'                           TO SEND-KDFUNC                  
171800     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
171900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
172000                         SEND-KVDLEN                                      
172100                         HDR-AREA                                         
172200     IF SEND-KDRC > ZERO                                                  
172300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
172400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
172500       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
172600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
172700     END-IF                                                               
172800     .                                                                    
172900     EJECT                                                                
173000 S90-PUT-DOC SECTION.                                                     
173100                                                                          
173200*    MOVE 'S90-PUT-DOC ' TO CURR-SECTION                                  
173300                                                                          
173400     MOVE 'PUT'                           TO SEND-KDFUNC                  
173500*    MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
173600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
173700                         SEND-KVDLEN                                      
173800                         DOC-AREA                                         
173900     IF SEND-KDRC > ZERO                                                  
174000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
174100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
174200       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
174300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
174400     END-IF                                                               
174500     .                                                                    
174600     EJECT                                                                
174700                                                                          
174800 IMS-GU-WDE101 SECTION.                                                   
174900                                                                          
175000     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
175100          DELIMITED BY SIZE INTO SSA1                                     
175200     MOVE '  GE' TO GOOD-STATUSCODES                                      
175300     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101  SSA1                   
175400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
175500     PERFORM IMS-STATUSCHECK                                              
175600     .                                                                    
175700     EJECT                                                                
175800 IMS-GNP-WDE111 SECTION.                                                  
175900                                                                          
176000     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
176100                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
176200          DELIMITED BY SIZE INTO SSA1                                     
176300     MOVE '  GE' TO GOOD-STATUSCODES                                      
176400     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
176500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
176600     PERFORM IMS-STATUSCHECK                                              
176700     .                                                                    
176800     EJECT                                                                
176900 IMS-GNP-WDE121 SECTION.                                                  
177000                                                                          
177100     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
177200          DELIMITED BY SIZE INTO SSA1                                     
177300     MOVE 'WDE121  '          TO SSA2                                     
177400     MOVE '  GE' TO GOOD-STATUSCODES                                      
177500     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
177600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
177700     PERFORM IMS-STATUSCHECK                                              
177800     .                                                                    
177900     EJECT                                                                
178000 IMS-GNP-WDE122 SECTION.                                                  
178100                                                                          
178200     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
178300          DELIMITED BY SIZE INTO SSA1                                     
178400     MOVE 'WDE122  '          TO SSA2                                     
178500     MOVE '  GE' TO GOOD-STATUSCODES                                      
178600     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE122 SSA1 SSA2              
178700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
178800     PERFORM IMS-STATUSCHECK                                              
178900     .                                                                    
179000     EJECT                                                                
179100 IMS-4731-GET-ROOT SECTION.                                               
179200     STRING 'WDG701  (WDGXKEY  =' WDGX01 ')'                              
179300          DELIMITED BY SIZE INTO SSA1                                     
179400     MOVE '  GE' TO GOOD-STATUSCODES                                      
179500     CALL CBLTDLI USING GU 4731-PCB DLI-IO-AREA SSA1                      
179600     MOVE 4731-STATUS-CODE TO STATUS-WS                                   
179700     PERFORM IMS-STATUSCHECK                                              
179800     .                                                                    
179900     EJECT                                                                
180000 IMS-4731-GET-SEGMENT SECTION.                                            
180100                                                                          
180200     MOVE 'WDG730  ' TO SSA1                                              
180300     MOVE '  GE' TO GOOD-STATUSCODES                                      
180400     CALL CBLTDLI USING GNP 4731-PCB DLI-IO-AREA SSA1                     
180500     MOVE 4731-STATUS-CODE TO STATUS-WS                                   
180600     PERFORM IMS-STATUSCHECK                                              
180700     .                                                                    
180800     EJECT                                                                
180900 IMS-4735-GET-ROOT SECTION.                                               
181000                                                                          
181100     STRING 'WDR101  (WDGXKEY  =' WDGX01 ')'                              
181200          DELIMITED BY SIZE INTO SSA1                                     
181300     MOVE '  GE' TO GOOD-STATUSCODES                                      
181400     CALL CBLTDLI USING GU 4735-PCB DLI-IO-AREA SSA1                      
181500     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
181600     PERFORM IMS-STATUSCHECK                                              
181700     .                                                                    
181800     EJECT                                                                
181900 IMS-4735-GET-SEGMENT SECTION.                                            
182000                                                                          
182100     MOVE 'WDGX4735' TO SSA1                                              
182200     MOVE '  GE' TO GOOD-STATUSCODES                                      
182300     CALL CBLTDLI USING GNP 4735-PCB DLI-IO-AREA SSA1                     
182400     MOVE 4735-STATUS-CODE TO STATUS-WS                                   
182500     PERFORM IMS-STATUSCHECK                                              
182600     .                                                                    
182700     EJECT                                                                
182800                                                                          
182900 IMS-GU-WDB101 SECTION.                                                   
183000                                                                          
183100     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
183200          DELIMITED BY SIZE INTO SSA1                                     
183300     MOVE '  GE' TO GOOD-STATUSCODES                                      
183400     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
183500     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
183600     PERFORM IMS-STATUSCHECK                                              
183700     .                                                                    
183800     EJECT                                                                
183900 IMS-GU-WDB201 SECTION.                                                   
184000                                                                          
184100     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
184200          DELIMITED BY SIZE INTO SSA1                                     
184300     MOVE '  GE' TO GOOD-STATUSCODES                                      
184400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
184500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
184600     PERFORM IMS-STATUSCHECK                                              
184700     .                                                                    
184800     EJECT                                                                
184900 IMS-GU-WDQ201 SECTION.                                                   
185000                                                                          
185100     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
185200          DELIMITED BY SIZE INTO SSA1                                     
185300     MOVE '  GE' TO GOOD-STATUSCODES                                      
185400     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ201 SSA1                    
185500     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
185600     PERFORM IMS-STATUSCHECK                                              
185700     .                                                                    
185800     EJECT                                                                
185900 IMS-STATUSCHECK SECTION.                                                 
186000                                                                          
186100     SET STATUS-IX TO 1                                                   
186200     SEARCH GOOD-STATUS                                                   
186300       AT END                                                             
186400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
186500           DELIMITED BY SIZE INTO ERRTEXT                                 
186600         DISPLAY ERRTEXT                                                  
186700         CALL FELLOG                                                      
186800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
186900         CONTINUE                                                         
187000     END-SEARCH                                                           
190000     .                                                                    
