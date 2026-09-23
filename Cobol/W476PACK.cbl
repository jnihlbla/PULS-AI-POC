000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W476PACK.                                                
000300 AUTHOR.         STINA MOGREN.                                            
000400 DATE-WRITTEN.   03/03/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SUBPROGRAM TO WRITE 'PACKING SPECIFICATION' TRANSPORT            
000900*        DOCUMENT. IT IS CALLED BY A PROGRAM W40631. DOCUMENT SHOW        
001000*        CASES PER CUSTOMER, FOR EVERY NEW COSTUMER THERE SHOULD          
001100*        BE A NEW LIST.                                                   
001200*        THIS DOCUMENT IS A SPECIAL FOR ITALY.                            
001300*                                                                         
001400*        THE PROGRAM READS     WDE1                                       
001500*        THE PROGRAM READS     WDE2                                       
001600*        THE PROGRAM READS     WDR1                                       
001700*        THE PROGRAM READS     WDB1                                       
001800*        THE PROGRAM READS     WDB2                                       
001900*                                                                         
002000*                                                                         
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500 DATA DIVISION.                                                           
002600 FILE SECTION.                                                            
002700                                                                          
002800 WORKING-STORAGE SECTION.                                                 
002900 77  IDPGM                         PIC X(8)    VALUE 'W476PACK'.          
003000 77  W-CURRENT                     PIC X(50)   VALUE SPACE.               
003100                                                                          
003200 77  YES                           PIC X       VALUE 'J'.                 
003300 77  NOO                           PIC X       VALUE 'N'.                 
003400 77  IX                            PIC S9(9)   VALUE +0 COMP SYNC.        
003500 77  MAX-IX                        PIC S9(9)  VALUE +16 COMP SYNC.        
003600 77  INDX                          PIC S9(4)   VALUE +0 COMP SYNC.        
003700 77  WS-IX                         PIC S9(4)   VALUE +0 COMP SYNC.        
003800 77  INDX2                         PIC S9(4)   VALUE +0 COMP SYNC.        
003900 77  IB                            PIC S9(3)   VALUE ZERO COMP-3.         
004000 77  IC                            PIC S9(3)   VALUE ZERO COMP-3.         
004100                                                                          
004200 77  W-FLE2                        PIC X       VALUE 'N'.                 
004300   88 E2-FINNS                     VALUE 'J'.                             
004400   88 E2-SAKNAS                    VALUE 'N'.                             
004500 77  W-STATUS                      PIC XX      VALUE SPACE.               
004600                                                                          
004700 77  WS-PAGE-NO                    PIC 9(3)    VALUE ZERO.                
004800 77  W-KDSPRAK                     PIC S9      COMP-3.                    
004900 77  W-SUORDV                   PIC S9(9)V9(2) VALUE ZERO COMP-3.         
005000 77  W-KDVALISO                    PIC X(3)    VALUE SPACE.               
005100 77  DUMMY-AREA                    PIC X(50)   VALUE SPACE.               
005200                                                                          
005300     EJECT                                                                
005400                                                                          
       01  WS-META                       PIC X(5) VALUE '¤META'.                
       01  WS-IDDISTR                    PIC Z(4)9.                             
       01  WS-IDSHIPM                    PIC Z(6)9.                             
005500 01  WS-YYMMDD                     PIC 9(6)  VALUE ZERO.                  
005600 01  WS-GOODS-VALUE                PIC 9(8)9V9(2) VALUE ZERO.             
005700 01  WS-GOODS-VALUE-VALUE          PIC 9(8)9V9(2) VALUE ZERO.             
005800 01  WS-TOTAL-VALUE                PIC 9(8)9V9(2) VALUE ZERO.             
005900 01  WS-TEMP-VALUE                 PIC 9(8)9V9(2) VALUE ZERO.             
006000 01  WS-PRLEGKST-VALUE             PIC 9(8)9V9(2) VALUE ZERO.             
006100 01  WS-PREMBHNT-VALUE             PIC 9(8)9V9(2) VALUE ZERO.             
006200 01  WS-PRAVDRAG-VALUE             PIC 9(8)9V9(2) VALUE ZERO.             
006300 01  WS-KDVALISO                   PIC X(3) VALUE SPACE.                  
006400 01  WS-KDVALISO-2                 PIC X(3) VALUE SPACE.                  
006500 01  WS-KDFORSKN                   PIC 9(3) VALUE ZERO.                   
006600 01  WS-ANTAL-RAD-BEFORSKN         PIC 9(3) VALUE ZERO.                   
006700*    WS-BEFOSKN ANVÄNDS EJ, BORTTAGEN PGA COBOL-5 PROBLEM                 
006800*01  WS-BEFORSKN-TAB.                                                     
006900*    03 FILLER OCCURS 16.                                                 
007000*      05 WS-BEFORSKN-RAD          PIC X(60) VALUE ZERO.                  
007100 01  WS-SKOLLI-SUORDV              PIC 9(9)V9(2) VALUE ZERO.              
007200 01  WS-TOTAL-VALUE-PRKURS         PIC 9(5)9V9(5) VALUE ZERO.             
007300 01  WS-PRKURS                     PIC 9(5)9V9(5) VALUE ZERO.             
007400 01  WS-NUMBER-OF-KOLLI            PIC 9(4) VALUE ZERO.                   
007500 01  WS-TOTAL-KG                   PIC 9(6)V9(1) VALUE ZERO.              
007600*01  WS-TOTAL-HG                   PIC 9(6)V9(1) VALUE ZERO.              
007700 01  WS-TOTAL-VKORDNTO             PIC 9(6)V9(3) VALUE ZERO.              
007800 01  WS-TOTAL-M3                   PIC 9(4)V9(3) VALUE ZERO.              
007900                                                                          
008000 01  WS-B                          PIC X(30)   VALUE SPACE.               
008100 01  FILLER                        REDEFINES WS-B.                        
008200     03  WS-BETEXT                 PIC X(25).                             
008300     03  FILLER                    PIC X(5).                              
008400                                                                          
008500 77  LYNK-PART-SW                  PIC X.                                 
008600     88 LYNK-NOT                              VALUE 'N'.                  
008700     88 LYNK-YES                              VALUE 'J'.                  
008800                                                                          
008900 01  W-LINE.                                                              
009000     03  W-LINE-COUNT              PIC 9(02) VALUE ZERO.                  
009100     03  W-LINE-MAX                PIC 9(02) VALUE 43.                    
009200*    --- STYRTECKEN PRINTER                                               
009300 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
009400 01  WS-SKIP1                      PIC X      VALUE ' '.                  
009500 01  WS-SKIP2                      PIC X      VALUE '0'.                  
009600 01  WS-SKIP3                      PIC X      VALUE '-'.                  
009700                                                                          
009800 01  WS-TYP-IDSHIP                 PIC X(21) VALUE                        
009900                                   'PACKING SPECIFICATION'.               
010000                                                                          
010100 01  TODAYS-DATE                   PIC 9(6)  VALUE ZERO.                  
010200 01  FILLER REDEFINES TODAYS-DATE.                                        
010300     03  TODAYS-DATE-YEAR          PIC 9(2).                              
010400     03  TODAYS-DATE-MONTH         PIC 9(2).                              
010500     03  TODAYS-DATE-DAY           PIC 9(2).                              
010600     EJECT                                                                
010700                                                                          
       01  WS-TIMESTAMP.                                                        
           03  FILLER                  PIC X       VALUE 'D'.                   
           03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
           03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
           03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
           03  FILLER                  PIC X       VALUE '_'.                   
           03  FILLER                  PIC X       VALUE 'T'.                   
           03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
           03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
           03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
010800 01  TABELL.                                                              
010900   03  HELP-TABELL OCCURS 16.                                             
011000     05  W-TEXT-BEEMBTYP         PIC X(12).                               
011100                                                                          
011200 01  ARB-RAD                       PIC X(132) VALUE SPACE.                
011300                                                                          
011400 01  RAD1-HEAD.                                                           
011500     03  FILLER                    PIC X(15).                             
011600     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
011700     03  FILLER                    PIC X(2).                              
011800     03  RAD1H-IMPORTER            PIC X(35).                             
011900     03  FILLER                    PIC X(2).                              
012000     03  RAD1H-TYP-IDSHIP          PIC X(25).                             
012100                                                                          
012200 01  RAD2-HEAD.                                                           
012300     03  FILLER                    PIC X(30).                             
012400     03  RAD2H-IMPORTER            PIC X(35).                             
012500                                                                          
012600 01  RAD3-HEAD.                                                           
012700     03  FILLER                    PIC X(30).                             
012800     03  RAD3H-IMPORTER            PIC X(35).                             
012900                                                                          
013000 01  RAD4-HEAD.                                                           
013100     03  FILLER                    PIC X(30).                             
013200     03  RAD4H-IMPORTER            PIC X(35).                             
013300     03  FILLER                    PIC X(02).                             
013400     03  RAD4H-TIYYMMDD            PIC 9(06).                             
013500     03  RAD4H-IDDISTR             PIC Z(4)9.                             
013600     03  FILLER                    PIC X(01).                             
013700     03  RAD4H-IDSHIPM             PIC Z(6)9.                             
013800     03  FILLER                    PIC X(2).                              
013900     03  RAD4H-IDTRPTNR            PIC Z(02)9.                            
014000     03  FILLER                    PIC X(01).                             
014100     03  RAD4H-IDLBBET             PIC X(12).                             
014200     03  FILLER                    PIC X(02).                             
014300     03  RAD4H-PAGE-NO             PIC Z(03).                             
014400                                                                          
014500 01  RAD5-HEAD.                                                           
014600     03  FILLER                    PIC X(30).                             
014700     03  RAD5H-IMPORTER            PIC X(35).                             
014800                                                                          
014900 01  RAD7-HEAD.                                                           
015000     03  FILLER                    PIC X(6).                              
015100     03  RAD7H-IDARTNR             PIC X(9).                              
015200     03  FILLER                    PIC X(2).                              
015300     03  RAD7H-BEART               PIC X(12).                             
015400     03  FILLER                    PIC X(16).                             
015500*                                                                         
015600     03  RAD7H-IDSTATNR            PIC X(8).                              
015700     03  FILLER                    PIC X(6).                              
015800*                                                                         
015900     03  RAD7H-KVBEART             PIC X(6).                              
016000     03  FILLER                    PIC X(5).                              
016100     03  RAD7H-VKORDNTO            PIC X(8).                              
016200     03  FILLER                    PIC X(10).                             
016300     03  RAD7H-IDKUNDRF-RO         PIC X(10).                             
016400                                                                          
016500 01  RAD1.                                                                
016600     03  FILLER                    PIC X(1).                              
016700     03  RAD1-DEALER-TEXT          PIC X(12).                             
016800     03  FILLER                    PIC X(2).                              
016900     03  RAD1-IDKUNDNR             PIC Z(7).                              
017000     03  FILLER                    PIC X(8).                              
017100     03  RAD1-BEGMT-1              PIC X(35).                             
017200     03  FILLER                    PIC X(5).                              
017300     03  RAD1-BEGMT-2              PIC X(35).                             
017400                                                                          
017500 01  RAD2.                                                                
017600     03  FILLER                    PIC X(30).                             
017700     03  RAD2-BEGMT-3              PIC X(35).                             
017800     03  FILLER                    PIC X(5).                              
017900     03  RAD2-BEGMT-4              PIC X(35).                             
018000                                                                          
018100 01  RAD3.                                                                
018200     03  FILLER                    PIC X(70).                             
018300     03  RAD3-BEGMT-5              PIC X(35).                             
018400                                                                          
018500 01  RAD4.                                                                
018600     03  FILLER                    PIC X(1).                              
018700     03  RAD4-ORDER-TEXT           PIC X(10).                             
018800     03  FILLER                    PIC X(2).                              
018900     03  RAD4-CASE-TEXT            PIC X(11).                             
019000     03  FILLER                    PIC X(2).                              
019100     03  RAD4-PACKAGE-TEXT         PIC X(12).                             
019200     03  FILLER                    PIC X(3).                              
019300     03  RAD4-L-TEXT               PIC X(5).                              
019400     03  FILLER                    PIC X(3).                              
019500     03  RAD4-W-TEXT               PIC X(3).                              
019600     03  FILLER                    PIC X(3).                              
019700     03  RAD4-H-TEXT               PIC X(3).                              
019800     03  FILLER                    PIC X(3).                              
019900     03  RAD4-GROSS-TEXT           PIC X(8).                              
020000     03  FILLER                    PIC X(4).                              
020100     03  RAD4-VOLUME-TEXT          PIC X(8).                              
020200     03  FILLER                    PIC X(2).                              
020300     03  RAD4-HAZARD-TEXT          PIC X(17).                             
020400                                                                          
020500 01  RAD5.                                                                
020600     03  FILLER                    PIC X(1).                              
020700     03  RAD5-ORDER                PIC Z(5).                              
020800     03  FILLER                    PIC X(8).                              
020900     03  RAD5-CASE                 PIC Z(5).                              
021000     03  FILLER                    PIC X(7).                              
021100     03  RAD5-PACKAGE              PIC X(12).                             
021200     03  FILLER                    PIC X(3).                              
021300     03  RAD5-L                    PIC Z(5).                              
021400     03  FILLER                    PIC X(3).                              
021500     03  RAD5-W                    PIC Z(3).                              
021600     03  FILLER                    PIC X(3).                              
021700     03  RAD5-H                    PIC Z(3).                              
021800     03  FILLER                    PIC X(3).                              
021900     03  RAD5-GROSS                PIC Z(5)9.9(1).                        
022000     03  FILLER                    PIC X(6).                              
022100     03  RAD5-VOLUME               PIC Z(3)9.9(3).                        
022200     03  FILLER                    PIC X(9).                              
022300     03  RAD5-HAZARD               PIC X(1).                              
022400                                                                          
022500 01  RAD6.                                                                
022600     03  FILLER                    PIC X(1).                              
022700     03  RAD6-TOTAL-TEXT           PIC X(12).                             
022800     03  FILLER                    PIC X(2).                              
022900     03  RAD6-TOTAL                PIC Z(5).                              
023000     03  FILLER                    PIC X(1).                              
023100     03  RAD6-PACKAGE-TEXT         PIC X(7).                              
023200     03  FILLER                    PIC X(5).                              
023300     03  RAD6-GROSS-TEXT           PIC X(8).                              
023400     03  RAD6-WEIGHT               PIC Z(5)9.9(1).                        
023500     03  FILLER                    PIC X(1).                              
023600     03  RAD6-KG-TEXT              PIC X(4).                              
023700     03  FILLER                    PIC X(5).                              
023800     03  RAD6-NET-TEXT             PIC X(8).                              
023900**** 03  RAD6-NET-WEIGHT           PIC Z(5)9.9(1).                        
024000     03  RAD6-NET-WEIGHT           PIC Z(5)9.9(3).                        
024100     03  FILLER                    PIC X(1).                              
024200     03  RAD6-KG-TEXT2             PIC X(4).                              
024300     03  FILLER                    PIC X(1).                              
024400     03  RAD6-VOLUME               PIC Z(3)9.9(3).                        
024500     03  FILLER                    PIC X(1).                              
024600     03  RAD6-M3-TEXT              PIC X(4).                              
024700                                                                          
024800 01  RAD7.                                                                
024900     03  FILLER                    PIC X(5).                              
025000     03  RAD7-ARTIKEL              PIC Z(10).                             
025100     03  FILLER                    PIC X(2).                              
025200     03  RAD7-ATEXT                PIC X(25).                             
025300     03  FILLER                    PIC X(2).                              
025400*                                                                         
025500     03  RAD7-IDSTATNR             PIC Z(9).                              
025600     03  FILLER                    PIC X(5).                              
025700*                                                                         
025800     03  RAD7-KVANT                PIC Z(7).                              
025900     03  FILLER                    PIC X(5).                              
026000     03  RAD7-WEIGHT               PIC Z(3)9.9(3).                        
026100     03  FILLER                    PIC X(10).                             
026200     03  RAD7-IDKUNDRF-RO          PIC X(10).                             
026300                                                                          
026400 01  LYNKRAD.                                                             
026500     03  FILLER                    PIC X(5).                              
026600     03  LYNK-ARTIKEL              PIC X(10).                             
026700     03  FILLER                    PIC X(2).                              
026800     03  LYNK-ATEXT                PIC X(25).                             
026900     03  FILLER                    PIC X(56).                             
027000*                                                                         
027100 01  TEST-IDDISTR               PIC 9(5)   COMP-3.                        
027200*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
027300     EJECT                                                                
027410*01  FILLER   -COPY WWDIST38    -RED TEST-IDDISTR.                        
027420     EJECT                                                                
027430                                                                          
027500 01  FILLER          PIC X(32)  VALUE  'LEDTEXT TABLE'.                   
027600* 01 -COPY W475W552                                                       
027700     EJECT                                                                
027800* 01 -COPY W475W553                                                       
027900     EJECT                                                                
028000* 01 -COPY W475W557                                                       
028100     EJECT                                                                
028200* 01 -COPY W476W001                                                       
028300     EJECT                                                                
028400                                                                          
028500 01  GENERAL-SUBPROGRAMS.                                                 
028600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
028700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
028800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
028900     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
029000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
029100     SKIP2                                                                
029200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
029300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
029400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
029500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
029600     SKIP2                                                                
029700 01  ERRTEXT.                                                             
029800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
029900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
030000 77  KDRC-DISPLAY                PIC Z(5).                                
030100     EJECT                                                                
030200*    --- PARAMETERS FOR SUBPROGRAM W006PRS1                               
030300     EJECT                                                                
030400*01  -COPY W006PRAR                                                       
030500 01  W-NYSIDA-RAD10          PIC S9(3)   VALUE +910  COMP-3.              
030600*                                        WRITE ON NEW LINE 10             
030700 01  W-IDPRTLST                  PIC X(8).                                
030800     SKIP2                                                                
030900*    --- AREAS FOR IMS-SECTIONS                                           
031000*                                                                         
031100     EJECT                                                                
031200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
031300     SKIP3                                                                
031400 01  KEYS-TO-DLI.                                                         
031500     03  W-WDB101KY-X.                                                    
031600         05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                 
031700         05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                  
031800                                                                          
031900     03  W-WDB201KY-X.                                                    
032000         05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
032100         05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
032200                                                                          
032300     03  W-IDSHIPM-X.                                                     
032400         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
032500                                                                          
032600     03  W-WDE111KY-X.                                                    
032700         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
032800         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
032900                                                                          
033000     03  W-WDE111KY-D.                                                    
033100         05  W-WDE111-IDDISTR-D  PIC S9(05)  VALUE ZERO COMP-3.           
033200         05  W-WDE111-IDKUNDNR-D PIC S9(07)  VALUE ZERO COMP-3.           
033300                                                                          
033400     03  W-WDE111KY-MIN.                                                  
033500         05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.          
033600         05  FILLER               PIC X(04)   VALUE LOW-VALUES.           
033700                                                                          
033800     03  W-WDE111KY-MAX.                                                  
033900         05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.          
034000         05  FILLER               PIC X(04)   VALUE HIGH-VALUES.          
034100                                                                          
034200     03  W-WDE121KY-X.                                                    
034300         05  W-WDE121-IDPRODNR   PIC S9(07)  VALUE ZERO COMP-3.           
034400         05  W-WDE121-IDKOLLI    PIC S9(05)  VALUE ZERO COMP-3.           
034500                                                                          
034600     03    W-KDEMBTYP-4738-X.                                             
034700         05    IDHTYP             PIC X(4)    VALUE '4738'.               
034800         05    W-KDEMBTYP-4738    PIC S9(3)   COMP-3 VALUE ZERO.          
034900         05    FILLER             PIC X(24)   VALUE LOW-VALUE.            
035000                                                                          
035100     03  W-IDARTNR-X.                                                     
035200         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
035300*    03     -COPY WDGX01                                                  
035400                                                                          
035500                                                                          
035600*    --- STATUS-KOD FRÅN IMS                                              
035700 01  STATUS-WS                   PIC XX.                                  
035800     88  SEGMENT-FOUND                       VALUE '  '.                  
035900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
036000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
036100                                                                          
036200 01  GOOD-STATUSCODES.                                                    
036300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
036400                                                                          
036500 01  SSA1                        PIC X(64).                               
036600 01  SSA2                        PIC X(64).                               
036700 01  SSA3                        PIC X(64).                               
036800 01  SSA4                        PIC X(64).                               
036900     EJECT                                                                
037000                                                                          
037100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
037200 01  SEND-AREA.                                                           
037300*    03  -COPY WZ01SEND                                                   
037400                                                                          
037500 01  SEND-RAD-STYRTECKEN.                                                 
037600     03  STYRTECKEN-RAD          PIC X.                                   
037700     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
037800                                                                          
037900 01  DAP-AREA-START              PIC X(24)   VALUE                        
038000                                             'DAP-AREA-START'.            
038100                                                                          
038200                                                                          
038300*    ---  IMS FUNCTION CODES                                              
038400*01  -COPY W0003                                                          
038500     EJECT                                                                
038600                                                                          
038700*    ---  DLI INPUT-OUTPUT AREA                                           
038800 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDE101-WDE111'.               
038900 01  DLI-IO-WDE101-11.                                                    
039000     03  DLI-IO-WDE101.                                                   
039100*        05  -COPY WDE101                                                 
039200     03  DLI-IO-WDE111.                                                   
039300*        05  -COPY WDE111                                                 
039400     EJECT                                                                
039500                                                                          
039600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
039700 01  DLI-IO-WDE121.                                                       
039800*    03  -COPY WDE121                                                     
039900                                                                          
040000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE131'.                      
040100 01  DLI-IO-WDE131.                                                       
040200*    03  -COPY WDE131                                                     
040300                                                                          
040400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE221'.                      
040500 01  DLI-IO-WDE221.                                                       
040600*    03  -COPY WDE221                                                     
040700                                                                          
040800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE231'.                      
040900 01  DLI-IO-WDE231.                                                       
041000*    03  -COPY WDE231                                                     
041100                                                                          
041200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
041300 01  DLI-IO-WDB101.                                                       
041400*    03  -COPY WDB101                                                     
041500                                                                          
041600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
041700 01  DLI-IO-WDB201.                                                       
041800*    03  -COPY WDB201                                                     
041900                                                                          
042000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDF502'.             
042100 01  DLI-IO-WDF502.                                                       
042200*  03  -COPY WDF502                                                       
042300                                                                          
042400 01  DLI-IO-AREA.                                                         
042500     03  IO-AREA  PIC X(100)                 VALUE SPACE.                 
042600     SKIP3                                                                
042700     03  WL473811 REDEFINES IO-AREA.                                      
042800*       05 -COPY WDGX4738                                                 
042900     EJECT                                                                
043000                                                                          
043100 LINKAGE SECTION.                                                         
043200*01  -COPY W476TRPD                                                       
043300                                                                          
043400 01  ALT-PCB                     PIC X(32).                               
043500                                                                          
043600*01  -COPY W0008  -PRE WDE1-                                              
043700     05  FILLER                  PIC X.                                   
043800                                                                          
043900*01  -COPY W0008  -PRE WDE2-                                              
044000     05  FILLER                  PIC X.                                   
044100                                                                          
044200*01  -COPY W0008  -PRE WDB1-                                              
044300     05  FILLER                  PIC X.                                   
044400                                                                          
044500*01  -COPY W0008  -PRE WDB2-                                              
044600     05  FILLER                  PIC X.                                   
044700                                                                          
044800*01  -COPY W0008  -PRE 4738-                                              
044900     05  FILLER                  PIC X.                                   
045000                                                                          
045100*01  -COPY W0008  -PRE WDF5-                                              
045200     05  FILLER                  PIC X.                                   
045300     EJECT                                                                
045400                                                                          
045500 PROCEDURE DIVISION  USING                                                
045600                           TRPD-W476TRPD ALT-PCB                          
045700                           WDE1-PCB WDE2-PCB WDB1-PCB WDB2-PCB            
045800                           4738-PCB WDF5-PCB.                             
045900 MAIN SECTION.                                                            
046000     ENTRY 'DLITCBL' USING                                                
046100                           TRPD-W476TRPD ALT-PCB                          
046200                           WDE1-PCB WDE2-PCB WDB1-PCB WDB2-PCB            
046300                           4738-PCB WDF5-PCB.                             
046400     PERFORM A-INIT                                                       
046500                                                                          
046600     PERFORM IMS-GU-WDE101                                                
046700     IF SHIP-IDSYSTEM (1:3) = 'LYN'                                       
046800        MOVE YES    TO LYNK-PART-SW                                       
046900     END-IF                                                               
047000                                                                          
047100     PERFORM IMS-GNP-WDE111                                               
047200     PERFORM UNTIL NOT SEGMENT-FOUND                                      
047300       IF WS-IX = ZERO                                                    
047400         PERFORM B-IMPORTER                                               
047500       END-IF                                                             
047600       PERFORM C-GOODS-RECEIVER                                           
047700       PERFORM IMS-GNP-WDE111                                             
047800     END-PERFORM                                                          
047900                                                                          
048000     MOVE ZERO TO RETURN-CODE                                             
048100     GOBACK                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 A-INIT SECTION.                                                          
048500                                                                          
048600     MOVE LENGTH OF SEND-RAD           TO SEND-KVDLEN                     
048700     MOVE FUNCTION CURRENT-DATE (3:6)  TO TODAYS-DATE                     
048800                                                                          
048900     MOVE SPACE           TO  RAD1-HEAD                                   
049000                              RAD2-HEAD                                   
049100                              RAD3-HEAD                                   
049200                              RAD4-HEAD                                   
049300                              RAD5-HEAD                                   
049400                              RAD7-HEAD                                   
049500                              RAD1                                        
049600                              RAD2                                        
049700                              RAD3                                        
049800                              RAD4                                        
049900                              RAD5                                        
050000                              RAD6                                        
050100                              RAD7                                        
050200*    WS-BEFOSKN ANVÄNDS EJ, BORTTAGEN PGA COBOL-5 PROBLEM                 
050300*                             WS-BEFORSKN-RAD(INDX2)                      
050400                                                                          
050500     MOVE TRPD-IDPRTLST   TO  W-IDPRTLST                                  
050600     MOVE TRPD-IDSHIPM    TO  W-IDSHIPM                                   
050700     MOVE TRPD-PFDEF-OVR  TO  PRT-PFDEF-OVR                               
050800     MOVE TRPD-IDDISTR    TO  W-WDE111-IDDISTR                            
050900                              W-WDB201-IDDISTR                            
051000                              W-WDE111-IDDISTR-MIN                        
051100                              W-WDE111-IDDISTR-MAX                        
051200                              TEST-IDDISTR                                
051300                                                                          
051400     MOVE ZERO            TO  WS-TEMP-VALUE                               
051500                              WS-IX                                       
051600                              WS-GOODS-VALUE                              
051700                              WS-TOTAL-VALUE                              
051800                              WS-TOTAL-KG                                 
051900***                           WS-TOTAL-HG                                 
052000                              WS-TOTAL-VKORDNTO                           
052100                              WS-TOTAL-M3                                 
052200                              WS-NUMBER-OF-KOLLI                          
052300                              WS-YYMMDD                                   
052400                              WS-KDVALISO                                 
052500                              WS-SKOLLI-SUORDV                            
052600                              WS-PRLEGKST-VALUE                           
052700                              WS-PREMBHNT-VALUE                           
052800                              WS-PRAVDRAG-VALUE                           
052900                              WS-PAGE-NO                                  
053000                              INDX                                        
053100                              INDX2                                       
                                    WS-IDDISTR                                  
                                    WS-IDSHIPM                                  
053200                                                                          
053300     MOVE 2                TO W-KDSPRAK                                   
053400     MOVE NOO              TO LYNK-PART-SW                                
053500                                                                          
053600     MOVE +1 TO IX                                                        
053700     PERFORM UNTIL IX > MAX-IX                                            
053800       MOVE SPACE TO W-TEXT-BEEMBTYP (IX)                                 
053900       ADD +1 TO IX                                                       
054000     END-PERFORM                                                          
054100                                                                          
054200     MOVE +1 TO IX                                                        
054300     MOVE IX TO W-KDEMBTYP-4738                                           
054400     PERFORM IMS-GET-4738                                                 
054500     IF SEGMENT-FOUND                                                     
054600       PERFORM UNTIL IX > MAX-IX                                          
054700         IF SEGMENT-FOUND                                                 
054800          MOVE EMBTYP-BEEMBTYP (2) TO W-TEXT-BEEMBTYP (IX)                
054900         END-IF                                                           
055000         ADD +1        TO IX                                              
055100         MOVE IX TO W-KDEMBTYP-4738                                       
055200         PERFORM IMS-GET-4738                                             
055300       END-PERFORM                                                        
055400     END-IF                                                               
055500     .                                                                    
055600     EJECT                                                                
055700 B-IMPORTER SECTION.                                                      
055800                                                                          
055900     MOVE SGMT-IDKUNDNR         TO W-WDE111-IDKUNDNR                      
056000                                   W-WDB201-IDKUNDNR                      
056100     PERFORM IMS-GU-WDB201                                                
056200     MOVE SGMT-IDPARTNR         TO W-WDB101-IDPARTNR                      
056300     MOVE GMT-IDFTG             TO W-WDB101-IDFTG                         
056400     PERFORM IMS-GU-WDB101                                                
056500     MOVE BET-BEBETRAD-1        TO RAD1H-IMPORTER                         
056600     MOVE BET-BEBETRAD-2        TO RAD2H-IMPORTER                         
056700     MOVE BET-ADBETRAD-1        TO RAD3H-IMPORTER                         
056800     MOVE BET-ADBETRAD-2        TO RAD4H-IMPORTER                         
056900     MOVE BET-BELAND-SVE        TO RAD5H-IMPORTER                         
057000     ADD +1 TO WS-IX                                                      
057100     .                                                                    
057200     EJECT                                                                
057300 C-GOODS-RECEIVER SECTION.                                                
057400                                                                          
057500     PERFORM CA-DEALER                                                    
057600                                                                          
057700     PERFORM CB-GOODS-DETAIL                                              
057800                                                                          
057900     PERFORM CC-TOTAL-ROWS                                                
058000     .                                                                    
058100     EJECT                                                                
058200 CA-DEALER SECTION.                                                       
058300                                                                          
058400     MOVE SGMT-IDKUNDNR         TO W-WDB201-IDKUNDNR                      
058500                                   RAD1-IDKUNDNR                          
058600     PERFORM IMS-GU-WDB201                                                
058700     MOVE GMT-BEGMT-RAD1        TO RAD1-BEGMT-1                           
058800     MOVE GMT-BEGMT-RAD2        TO RAD2-BEGMT-3                           
058900     MOVE GMT-ADGMT-GATA        TO RAD1-BEGMT-2                           
059000     MOVE GMT-ADGMT-PADR        TO RAD2-BEGMT-4                           
059100     MOVE GMT-ADGMT-LAND        TO RAD3-BEGMT-5                           
059200                                                                          
059300     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
059400       MOVE +2 TO W-KDSPRAK                                               
059500     ELSE                                                                 
059600       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
059700     END-IF                                                               
059800                                                                          
           IF WS-PAGE-NO = ZEROES                                               
              PERFORM S04-PRINT-META                                            
           END-IF                                                               
                                                                                
059900     PERFORM S01-PRINT-HEAD                                               
060000     PERFORM CAA-PRINT-ROW1                                               
060100     PERFORM CAB-PRINT-ROW2                                               
060200     PERFORM CAC-PRINT-ROW3                                               
060300     .                                                                    
060400     EJECT                                                                
060500 CAA-PRINT-ROW1 SECTION.                                                  
060600                                                                          
060700     MOVE IDDEALER-LEDTEXT (W-KDSPRAK) TO RAD1-DEALER-TEXT                
060800     MOVE RAD1                       TO ARB-RAD                           
060900                                        SEND-RAD                          
061000     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
061100     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
061200     ADD 2                           TO W-LINE-COUNT                      
061300     PERFORM S10-PRINT-LINE                                               
061400     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
061500     .                                                                    
061600     EJECT                                                                
061700 CAB-PRINT-ROW2 SECTION.                                                  
061800                                                                          
061900     MOVE RAD2                       TO ARB-RAD                           
062000                                        SEND-RAD                          
062100     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
062200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
062300     ADD 1                           TO W-LINE-COUNT                      
062400     PERFORM S10-PRINT-LINE                                               
062500     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
062600     .                                                                    
062700     EJECT                                                                
062800 CAC-PRINT-ROW3 SECTION.                                                  
062900                                                                          
063000     MOVE RAD3                       TO ARB-RAD                           
063100                                        SEND-RAD                          
063200     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
063300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
063400     ADD 1                           TO W-LINE-COUNT                      
063500     PERFORM S10-PRINT-LINE                                               
063600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
063700     .                                                                    
063800     EJECT                                                                
063900 CB-GOODS-DETAIL SECTION.                                                 
064000                                                                          
064100     MOVE SGMT-IDKUNDNR         TO W-WDE111-IDKUNDNR                      
064200     PERFORM IMS-GNP-WDE121                                               
064300     MOVE SKOLLI-IDPRODNR       TO W-WDE121-IDPRODNR                      
064400     MOVE SKOLLI-IDKOLLI        TO W-WDE121-IDKOLLI                       
064500                                                                          
064600     PERFORM S02-PRINT-ROW4                                               
064700     PERFORM S03-PRINT-ROW7H                                              
064800                                                                          
064900     PERFORM UNTIL SEGMENT-MISSING                                        
065000        PERFORM IMS-GU-WDE221                                             
065100        IF SEGMENT-MISSING                                                
065200          MOVE NOO   TO W-FLE2                                            
065300        ELSE                                                              
065400          MOVE YES   TO W-FLE2                                            
065500        END-IF                                                            
065600        PERFORM CBA-ORDER-ROWS                                            
065700        PERFORM IMS-GNP-WDE121                                            
065800        MOVE SKOLLI-IDPRODNR    TO W-WDE121-IDPRODNR                      
065900        MOVE SKOLLI-IDKOLLI     TO W-WDE121-IDKOLLI                       
066000     END-PERFORM                                                          
066100     .                                                                    
066200     EJECT                                                                
066300 CBA-ORDER-ROWS SECTION.                                                  
066400                                                                          
066500     MOVE SKOLLI-IDORDNR7(3:5)  TO RAD5-ORDER                             
066600     MOVE SKOLLI-IDKOLLI        TO RAD5-CASE                              
066700     MOVE SKOLLI-DIKOLLIL       TO RAD5-L                                 
066800     MOVE SKOLLI-DIKOLLIB       TO RAD5-W                                 
066900     MOVE SKOLLI-DIKOLLIH       TO RAD5-H                                 
067000     MOVE SKOLLI-VKORDBTO-KOLLI TO RAD5-GROSS                             
067100     MOVE SKOLLI-VLORDBTO-KOLLI TO RAD5-VOLUME                            
067200                                                                          
067300     IF SKOLLI-KDFARLIG-KOLLI = +2 OR +4 OR +5 OR +7                      
067400       MOVE YES   TO RAD5-HAZARD                                          
067500     ELSE                                                                 
067600       MOVE SPACE TO RAD5-HAZARD                                          
067700     END-IF                                                               
067800                                                                          
067900     IF SKOLLI-KDEMBTYP > +0                                              
068000       MOVE W-TEXT-BEEMBTYP (SKOLLI-KDEMBTYP) TO RAD5-PACKAGE             
068100     END-IF                                                               
068200                                                                          
068300     COMPUTE WS-NUMBER-OF-KOLLI = WS-NUMBER-OF-KOLLI + 1                  
068400                                                                          
068500     COMPUTE WS-TOTAL-KG = WS-TOTAL-KG + SKOLLI-VKORDBTO-KOLLI            
068600                                                                          
068700***  ANVÄND EJ FÄRDIG NETTOVIKT, BERÄKNA I CBAB-                          
068800***  COMPUTE WS-TOTAL-HG = WS-TOTAL-HG + SKOLLI-VKORDNTO-KOLLI            
068900                                                                          
069000     COMPUTE WS-TOTAL-M3 = WS-TOTAL-M3 + SKOLLI-VLORDBTO-KOLLI            
069100                                                                          
069200     PERFORM CBAA-PRINT-ROW5                                              
069300     PERFORM IMS-GNP-WDE131                                               
069400     MOVE STATUS-WS          TO W-STATUS                                  
069500     IF E2-FINNS                                                          
069600       PERFORM IMS-GNP-WDE231                                             
069700       IF SEGMENT-MISSING                                                 
069800         MOVE SPACE          TO BRAD-BEART-VIPS                           
069900       END-IF                                                             
070000     ELSE                                                                 
070100       MOVE SPACE            TO BRAD-BEART-VIPS                           
070200     END-IF                                                               
070300     MOVE W-STATUS           TO STATUS-WS                                 
070400                                                                          
070500     PERFORM UNTIL SEGMENT-MISSING                                        
070600       PERFORM CBAB-ARTICLE-ROWS                                          
070700       PERFORM IMS-GNP-WDE131                                             
070800       MOVE STATUS-WS        TO W-STATUS                                  
070900       IF E2-FINNS                                                        
071000         PERFORM IMS-GNP-WDE231                                           
071100         IF SEGMENT-MISSING                                               
071200           MOVE SPACE          TO BRAD-BEART-VIPS                         
071300         END-IF                                                           
071400       ELSE                                                               
071500         MOVE SPACE            TO BRAD-BEART-VIPS                         
071600       END-IF                                                             
071700       MOVE W-STATUS           TO STATUS-WS                               
071800     END-PERFORM                                                          
071900     .                                                                    
072000     EJECT                                                                
072100 CBAA-PRINT-ROW5 SECTION.                                                 
072200                                                                          
072300     IF W-LINE-COUNT > W-LINE-MAX - 2                                     
072400       PERFORM S01-PRINT-HEAD                                             
072500       PERFORM S02-PRINT-ROW4                                             
072600       PERFORM S03-PRINT-ROW7H                                            
072700     END-IF                                                               
072800     MOVE RAD5                       TO ARB-RAD                           
072900                                        SEND-RAD                          
073000     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
073100     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
073200     ADD 2                           TO W-LINE-COUNT                      
073300     PERFORM S10-PRINT-LINE                                               
073400     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
073500                                                                          
073600     MOVE SPACE                      TO ARB-RAD                           
073700                                        SEND-RAD                          
073800     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
073900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
074000     ADD 1                           TO W-LINE-COUNT                      
074100     PERFORM S10-PRINT-LINE                                               
074200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
074300     .                                                                    
074400     EJECT                                                                
074500 CBAB-ARTICLE-ROWS  SECTION.                                              
074600                                                                          
074700                                                                          
074800     MOVE SRAD-IDARTNR             TO RAD7-ARTIKEL                        
074900                                      W-IDARTNR                           
075000     MOVE SRAD-IDSTATNR            TO RAD7-IDSTATNR                       
075100     MOVE BRAD-BEART-VIPS          TO RAD7-ATEXT                          
075200*                                                                         
075300*    OM TAIWAN TAG BORT 'KIT' FRÅN TEXTEN PÅ ENGELSKA                     
075400     IF DIST38-EJ-KIT-TW                                                  
075500       PERFORM CBABB-KIT-BORT                                             
075600     END-IF                                                               
075700*                                                                         
075800     MOVE SRAD-KVLEVART            TO RAD7-KVANT                          
075900**   MOVE SRAD-VKARTNTO            TO RAD7-WEIGHT                         
076000     MOVE SRAD-VKART-NTO-KG        TO RAD7-WEIGHT                         
076100     MOVE SRAD-IDKUNDRF-RO         TO RAD7-IDKUNDRF-RO                    
076200     IF SRAD-IDKUNDRF-RO = '00000'                                        
076300       MOVE SPACE                  TO RAD7-IDKUNDRF-RO                    
076400     END-IF                                                               
076500                                                                          
076600*    SUMMERA KOLLITS NETTOVIKT UTIFRÅN ARTIKELS NETTOVIKT                 
076700     COMPUTE WS-TOTAL-VKORDNTO = WS-TOTAL-VKORDNTO +                      
076800**        (SRAD-VKARTNTO * SRAD-KVLEVART)                                 
076900          (SRAD-VKART-NTO-KG * SRAD-KVLEVART)                             
077000                                                                          
077100     PERFORM CBABA-PRINT-ROW7                                             
077200     .                                                                    
077300     EJECT                                                                
077400 CBABA-PRINT-ROW7 SECTION.                                                
077500                                                                          
077600     IF ( W-LINE-COUNT > W-LINE-MAX - 4 AND LYNK-YES )                    
077700        OR W-LINE-COUNT > W-LINE-MAX - 3                                  
077800          PERFORM S01-PRINT-HEAD                                          
077900          MOVE SPACE                  TO ARB-RAD                          
078000                                         SEND-RAD                         
078100          MOVE PRT-AFTER-1            TO PRT-RADSKIP                      
078200          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
078300          ADD 1                       TO W-LINE-COUNT                     
078400          PERFORM S10-PRINT-LINE                                          
078500          MOVE WS-SKIP1               TO STYRTECKEN-RAD                   
078600     END-IF                                                               
078700                                                                          
078800     MOVE RAD7                        TO ARB-RAD                          
078900                                         SEND-RAD                         
079000     MOVE PRT-AFTER-1                 TO PRT-RADSKIP                      
079100     MOVE WS-SKIP1                    TO STYRTECKEN-RAD                   
079200     ADD 1                            TO W-LINE-COUNT                     
079300     PERFORM S10-PRINT-LINE                                               
079400                                                                          
079500*ADD LYNK PART NO TO SHIPDOC                                              
079600     IF LYNK-YES                                                          
079700        PERFORM IMS-GU-WDF502                                             
079710        IF SEGMENT-FOUND                                                  
079800          MOVE XLEV-IDLEVART         TO LYNK-ARTIKEL                      
079810        END-IF                                                            
079900        MOVE 'LYNK&CO PART NO'     TO LYNK-ATEXT                          
080000        MOVE LYNKRAD               TO ARB-RAD                             
080100                                      SEND-RAD                            
080200        ADD 1                      TO W-LINE-COUNT                        
080300        PERFORM S10-PRINT-LINE                                            
080400     END-IF                                                               
080500     MOVE WS-SKIP1                    TO STYRTECKEN-RAD                   
080600     .                                                                    
080700     EJECT                                                                
080800 CBABB-KIT-BORT SECTION.                                                  
080900                                                                          
081000     MOVE BRAD-BEART-VIPS          TO WS-BETEXT                           
081100*     FÖRSÖK TA BORT KIT                                                  
081200     MOVE 1                        TO IB                                  
081300     MOVE ZERO                     TO IC                                  
081400     PERFORM UNTIL IB > 22                                                
081500       IF WS-BETEXT(IB:3) = 'KIT'                                         
081600         MOVE SPACE                TO WS-BETEXT(IB:3)                     
081700         ADD 3 TO IB       GIVING IC                                      
081800         MOVE 25                   TO IB                                  
081900       END-IF                                                             
082000       ADD 1                       TO IB                                  
082100     END-PERFORM                                                          
082200     IF IC < 25 AND NOT = ZERO                                            
082300       PERFORM UNTIL IC > 25                                              
082400         MOVE SPACE                TO WS-BETEXT(IC:1)                     
082500         ADD 1                     TO IC                                  
082600       END-PERFORM                                                        
082700     END-IF                                                               
082800     MOVE WS-BETEXT                TO RAD7-ATEXT                          
082900     .                                                                    
083000     EJECT                                                                
083100 CC-TOTAL-ROWS SECTION.                                                   
083200                                                                          
083300     MOVE WS-NUMBER-OF-KOLLI       TO RAD6-TOTAL                          
083400     MOVE WS-TOTAL-KG              TO RAD6-WEIGHT                         
083500***  MOVE WS-TOTAL-HG              TO RAD6-NET-WEIGHT                     
083600     MOVE WS-TOTAL-VKORDNTO        TO RAD6-NET-WEIGHT                     
083700     MOVE WS-TOTAL-M3              TO RAD6-VOLUME                         
083800                                                                          
083900     PERFORM CCA-PRINT-ROW6                                               
084000                                                                          
084100     MOVE ZERO                     TO WS-NUMBER-OF-KOLLI                  
084200                                      WS-TOTAL-KG                         
084300***                                   WS-TOTAL-HG                         
084400                                      WS-TOTAL-VKORDNTO                   
084500                                      WS-TOTAL-M3                         
084600     .                                                                    
084700     EJECT                                                                
084800 CCA-PRINT-ROW6 SECTION.                                                  
084900                                                                          
085000     IF W-LINE-COUNT > W-LINE-MAX - 3                                     
085100       PERFORM S01-PRINT-HEAD                                             
085200     END-IF                                                               
085300     MOVE PRODGRP-SUMMA-LEDTEXT (W-KDSPRAK) TO RAD6-TOTAL-TEXT            
085400     MOVE IDKUNDRF-IDKOLLI-TOT-LEDTEXT (W-KDSPRAK)                        
085500                                      TO RAD6-PACKAGE-TEXT                
085600     MOVE VKORDBTO-LEDTEXT (W-KDSPRAK)  TO RAD6-GROSS-TEXT                
085700     MOVE VKORDNTO-LEDTEXT (W-KDSPRAK)  TO RAD6-NET-TEXT                  
085800     MOVE TULLC2-VIKT-LEDTEXT         TO RAD6-KG-TEXT                     
085900                                         RAD6-KG-TEXT2                    
086000     MOVE TULLC2-VOL-LEDTEXT          TO RAD6-M3-TEXT                     
086100     MOVE RAD6                        TO ARB-RAD                          
086200                                         SEND-RAD                         
086300     MOVE PRT-AFTER-3                 TO PRT-RADSKIP                      
086400     MOVE WS-SKIP3                    TO STYRTECKEN-RAD                   
086500     ADD 3                            TO W-LINE-COUNT                     
086600     PERFORM S10-PRINT-LINE                                               
086700     MOVE WS-SKIP1                    TO STYRTECKEN-RAD                   
086800     .                                                                    
086900     EJECT                                                                
087000 S01-PRINT-HEAD SECTION.                                                  
087100                                                                          
087200     ADD 1                           TO WS-PAGE-NO                        
087300     MOVE BEVARREF-LEDTEXT (W-KDSPRAK) TO RAD1H-IMPORTER-TEXT             
087400                                                                          
087500     MOVE SPACE                      TO SEND-RAD                          
087600     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
087700     PERFORM S90-PUT-DOC-LINE                                             
087800     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
087900     PERFORM S90-PUT-DOC-LINE                                             
088000     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
088100     PERFORM S90-PUT-DOC-LINE                                             
088200                                                                          
088300     MOVE WS-TYP-IDSHIP              TO RAD1H-TYP-IDSHIP                  
088400     MOVE RAD1-HEAD                  TO ARB-RAD                           
088500                                        SEND-RAD                          
088600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
088700     MOVE PRT-NYSIDA-RAD7            TO PRT-RADSKIP                       
088800     MOVE 7                          TO W-LINE-COUNT                      
088900     PERFORM S10-PRINT-LINE                                               
089000     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
089100                                                                          
089200     MOVE RAD2-HEAD                  TO ARB-RAD                           
089300                                        SEND-RAD                          
089400     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
089500     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
089600     ADD 1                           TO W-LINE-COUNT                      
089700     PERFORM S10-PRINT-LINE                                               
089800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
089900                                                                          
090000     MOVE RAD3-HEAD                  TO ARB-RAD                           
090100                                        SEND-RAD                          
090200     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
090300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
090400     ADD 1                           TO W-LINE-COUNT                      
090500     PERFORM S10-PRINT-LINE                                               
090600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
090700                                                                          
090800     MOVE SHIP-TISKEPPN              TO WS-YYMMDD                         
090900     MOVE WS-YYMMDD                  TO RAD4H-TIYYMMDD                    
091000     MOVE SGMT-IDDISTR               TO RAD4H-IDDISTR                     
091100     MOVE SHIP-IDSHIPM               TO RAD4H-IDSHIPM                     
091200     MOVE SHIP-IDTRPTNR              TO RAD4H-IDTRPTNR                    
091300     MOVE SHIP-IDLBBET               TO RAD4H-IDLBBET                     
091400     MOVE WS-PAGE-NO                 TO RAD4H-PAGE-NO                     
091500     MOVE RAD4-HEAD                  TO ARB-RAD                           
091600                                        SEND-RAD                          
091700     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
091800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
091900     ADD 1                           TO W-LINE-COUNT                      
092000     PERFORM S10-PRINT-LINE                                               
092100     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
092200                                                                          
092300     MOVE RAD5-HEAD                  TO ARB-RAD                           
092400                                        SEND-RAD                          
092500     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
092600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
092700     ADD 1                           TO W-LINE-COUNT                      
092800     PERFORM S10-PRINT-LINE                                               
092900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
093000                                                                          
093100     .                                                                    
093200     EJECT                                                                
093300 S02-PRINT-ROW4 SECTION.                                                  
093400                                                                          
093500     MOVE IDKUNDRF-LEDTEXT (W-KDSPRAK) TO RAD4-ORDER-TEXT                 
093600     MOVE IDKUNDRF-IDKOLLI-LEDTEXT (W-KDSPRAK) TO RAD4-CASE-TEXT          
093700     MOVE BEEMBTYP-LEDTEXT (W-KDSPRAK) TO RAD4-PACKAGE-TEXT               
093800     MOVE DIKOLLIL-LEDTEXT (W-KDSPRAK) TO RAD4-L-TEXT                     
093900     MOVE DIKOLLIB-LEDTEXT (W-KDSPRAK) TO RAD4-W-TEXT                     
094000     MOVE DIKOLLIH-LEDTEXT (W-KDSPRAK) TO RAD4-H-TEXT                     
094100     MOVE VKORDBTO-LEDTEXT (W-KDSPRAK) TO RAD4-GROSS-TEXT                 
094200     MOVE VLORDBTO-LEDTEXT (W-KDSPRAK) TO RAD4-VOLUME-TEXT                
094300     MOVE BEFARLIG-TEXT (W-KDSPRAK)  TO RAD4-HAZARD-TEXT                  
094400     MOVE RAD4                       TO ARB-RAD                           
094500                                        SEND-RAD                          
094600     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
094700     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
094800     ADD 2                           TO W-LINE-COUNT                      
094900     PERFORM S10-PRINT-LINE                                               
095000     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
095100     .                                                                    
095200     EJECT                                                                
095300 S03-PRINT-ROW7H SECTION.                                                 
095400                                                                          
095500     MOVE IDARTNR-LEDTEXT (W-KDSPRAK)  TO RAD7H-IDARTNR                   
095600     MOVE BEART-LEDTEXT (W-KDSPRAK)    TO RAD7H-BEART                     
095700     MOVE IDSTATNR-LEDTEXT (W-KDSPRAK) TO RAD7H-IDSTATNR                  
095800     MOVE KVBEART-LEDTEXT (W-KDSPRAK)  TO RAD7H-KVBEART                   
095900     MOVE VKORDNTO-LEDTEXT (W-KDSPRAK) TO RAD7H-VKORDNTO                  
096000     MOVE IDKUNDRF-RO-LEDTEXT(W-KDSPRAK) TO RAD7H-IDKUNDRF-RO             
096100     MOVE RAD7-HEAD                    TO ARB-RAD                         
096200                                          SEND-RAD                        
096300     MOVE PRT-AFTER-2                  TO PRT-RADSKIP                     
096400     MOVE WS-SKIP2                     TO STYRTECKEN-RAD                  
096500     ADD 2                             TO W-LINE-COUNT                    
096600     PERFORM S10-PRINT-LINE                                               
096700     MOVE WS-SKIP1                     TO STYRTECKEN-RAD                  
096800     .                                                                    
096900     EJECT                                                                
                                                                                
       S04-PRINT-META SECTION.                                                  
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SHIP-IDSHIPM       TO WS-IDSHIPM                                
           STRING WS-META                                                       
                  'SHIPMENT_NUMBER='                                            
                  WS-IDSHIPM                                                    
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           STRING WS-META                                                       
                  'DOCUMENT_TYPE='                                              
                  WS-TYP-IDSHIP                                                 
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SHIP-TISKEPPN      TO WS-YYMMDD                                 
           MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
                                                                                
           STRING WS-META                                                       
                  'SHIPPING_DATE='                                              
                  WS-YEAR(1:2)                                                  
                  WS-YYMMDD                                                     
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
           MOVE SGMT-IDDISTR       TO WS-IDDISTR                                
           STRING WS-META                                                       
                  'DISTRICT_NUMBER='                                            
                  WS-IDDISTR                                                    
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
                                                                                
           MOVE SPACES             TO SEND-RAD-STYRTECKEN                       
                                                                                
           MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
           MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
           MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
           MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
           MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
           MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
                                                                                
           STRING WS-META                                                       
                  'FILE_NAME='                                                  
                  DELIMITED BY SIZE                                             
                  'SHIPDOC_PS'                                                  
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  FUNCTION TRIM (WS-IDSHIPM)                                    
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  FUNCTION TRIM (WS-IDDISTR)                                    
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  WS-TIMESTAMP                                                  
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
           .                                                                    
                                                                                
097000 S10-PRINT-LINE SECTION.                                                  
097100                                                                          
097200*    -- PRINT TO ON-DEMAND IF SO SPECIFIED ON 4456                        
097300     PERFORM S90-PUT-DOC-LINE                                             
097400                                                                          
097500*    -- PRINT TO PAPER IF SO SPECIFIED ON 4664 (FLSKRIV-NU = YES)         
097600*    -- OR ALWAYS IF WE COME FROM 4622                                    
097700*    -- OR ALWAYS IF IT IS A WEB DC (WHERE FLSKRIV-NU IS N AND            
097800*    -- CANNOT BE SET TO J)                                               
097900     IF SHIP-FLSKRIV-NU = YES OR TRPD-IDPGM = 'W4062200'                  
098000     OR TRPD-FLLDCKND = YES                                               
098100       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
098200                           PRT-WRITE                                      
098300                           W-IDPRTLST                                     
098400                           ALT-PCB                                        
098500                           PRT-RADSKIP                                    
098600                           ARB-RAD                                        
098700     END-IF                                                               
098800     .                                                                    
098900     EJECT                                                                
099000 S90-PUT-DOC-LINE SECTION.                                                
099100     IF (TRPD-IDPGM = 'W4063600' AND TRPD-KVCOPIES = '1')                 
099200                                 OR                                       
099300        (TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1' AND              
099400         SHIP-KDFAKSTA-EXP = 2)                                           
099410                                 OR                                       
099420        (TRPD-IDPGM = 'W4063400' AND TRPD-KVCOPIES = '1' AND              
099430         DIST35-CDC-AE-REFILL)                                            
099500       IF TRPD-FLSKRIV-ONDEM = YES                                        
099600         MOVE +1                          TO SEND-IDCOM                   
099700         MOVE 'PUT'                       TO SEND-KDFUNC                  
099800         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
099900         CALL WZ01SEND USING SEND-CONTROL-AREA                            
100000                             SEND-KVDLEN                                  
100100                             SEND-RAD-STYRTECKEN                          
100200         IF SEND-KDRC > ZERO                                              
100300           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
100400           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
100500           DELIMITED BY SIZE INTO ERRTEXT-STR                             
100600           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
100700         END-IF                                                           
100800       END-IF                                                             
100900     END-IF                                                               
101000     .                                                                    
101100     EJECT                                                                
101200 IMS-GU-WDE101 SECTION.                                                   
101300                                                                          
101400     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
101500          DELIMITED BY SIZE INTO SSA1                                     
101600     MOVE '  GE' TO GOOD-STATUSCODES                                      
101700     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
101800     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
101900     PERFORM IMS-STATUSCHECK                                              
102000     .                                                                    
102100     EJECT                                                                
102200 IMS-GNP-WDE111 SECTION.                                                  
102300                                                                          
102400     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
102500          DELIMITED BY SIZE INTO SSA1                                     
102600     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
102700                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
102800          DELIMITED BY SIZE INTO SSA2                                     
102900     MOVE '  GE' TO GOOD-STATUSCODES                                      
103000     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
103100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
103200     PERFORM IMS-STATUSCHECK                                              
103300     .                                                                    
103400     EJECT                                                                
103500 IMS-GNP-WDE121 SECTION.                                                  
103600                                                                          
103700     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
103800          DELIMITED BY SIZE INTO SSA1                                     
103900     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
104000          DELIMITED BY SIZE INTO SSA2                                     
104100     MOVE 'WDE121  '          TO SSA3                                     
104200     MOVE '  GE' TO GOOD-STATUSCODES                                      
104300     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3         
104400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
104500     PERFORM IMS-STATUSCHECK                                              
104600     .                                                                    
104700     EJECT                                                                
104800 IMS-GNP-WDE131 SECTION.                                                  
104900                                                                          
105000     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
105100          DELIMITED BY SIZE INTO SSA1                                     
105200     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
105300          DELIMITED BY SIZE INTO SSA2                                     
105400     STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
105500          DELIMITED BY SIZE INTO SSA3                                     
105600     MOVE 'WDE131  '          TO SSA4                                     
105700     MOVE '  GE' TO GOOD-STATUSCODES                                      
105800     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE131 SSA1 SSA2              
105900                                                   SSA3 SSA4              
106000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
106100     PERFORM IMS-STATUSCHECK                                              
106200     .                                                                    
106300     EJECT                                                                
106400 IMS-GU-WDE221 SECTION.                                                   
106500                                                                          
106600     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
106700          DELIMITED BY SIZE INTO SSA1                                     
106800     STRING 'WDE211  (WDE211KY =' W-WDE111KY-X ')'                        
106900          DELIMITED BY SIZE INTO SSA2                                     
107000     STRING 'WDE221  (WDE221KY =' W-WDE121KY-X ')'                        
107100          DELIMITED BY SIZE INTO SSA3                                     
107200     MOVE '  GE' TO GOOD-STATUSCODES                                      
107300     CALL CBLTDLI USING GU WDE2-PCB DLI-IO-WDE221 SSA1 SSA2               
107400                                                  SSA3                    
107500     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
107600     PERFORM IMS-STATUSCHECK                                              
107700     .                                                                    
107800     EJECT                                                                
107900 IMS-GNP-WDE231 SECTION.                                                  
108000                                                                          
108100     MOVE 'WDE231  '          TO SSA1                                     
108200     MOVE '  GE' TO GOOD-STATUSCODES                                      
108300     CALL CBLTDLI USING GNP WDE2-PCB DLI-IO-WDE231 SSA1                   
108400     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
108500     PERFORM IMS-STATUSCHECK                                              
108600     .                                                                    
108700     EJECT                                                                
108800 IMS-GU-WDB101 SECTION.                                                   
108900                                                                          
109000     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
109100          DELIMITED BY SIZE INTO SSA1                                     
109200     MOVE '  GE' TO GOOD-STATUSCODES                                      
109300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
109400     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
109500     PERFORM IMS-STATUSCHECK                                              
109600     .                                                                    
109700     EJECT                                                                
109800 IMS-GU-WDB201 SECTION.                                                   
109900                                                                          
110000     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
110100          DELIMITED BY SIZE INTO SSA1                                     
110200     MOVE '  GE' TO GOOD-STATUSCODES                                      
110300     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
110400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
110500     PERFORM IMS-STATUSCHECK                                              
110600     .                                                                    
110700     EJECT                                                                
110800 IMS-GET-4738 SECTION.                                                    
110900                                                                          
111000     STRING 'WDR101  (WDGXKEY  =' W-KDEMBTYP-4738-X ')'                   
111100            DELIMITED BY SIZE INTO SSA1                                   
111200     MOVE 'WDGX4738' TO SSA2                                              
111300     MOVE '  GE' TO GOOD-STATUSCODES                                      
111400     CALL CBLTDLI USING GU 4738-PCB DLI-IO-AREA SSA1 SSA2                 
111500     MOVE 4738-STATUS-CODE TO STATUS-WS                                   
111600     PERFORM IMS-STATUSCHECK                                              
111700     .                                                                    
111800     EJECT                                                                
111900 IMS-GU-WDF502 SECTION.                                                   
112000                                                                          
112100     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
112200            DELIMITED BY SIZE INTO SSA1                                   
112300     MOVE 'WDF502  '       TO SSA2                                        
112400     MOVE '  GE'           TO GOOD-STATUSCODES                            
112500     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
112600     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
112700     PERFORM IMS-STATUSCHECK                                              
112800     .                                                                    
112900                                                                          
113000 IMS-STATUSCHECK SECTION.                                                 
113100                                                                          
113200     SET STATUS-IX TO 1                                                   
113300     SEARCH GOOD-STATUS                                                   
113400       AT END                                                             
113500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
113600           DELIMITED BY SIZE INTO ERRTEXT                                 
113700         DISPLAY ERRTEXT                                                  
113800         CALL FELLOG                                                      
113900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
114000         CONTINUE                                                         
114100     END-SEARCH                                                           
114200     .                                                                    
