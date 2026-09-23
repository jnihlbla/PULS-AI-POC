000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W476GMTL.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   02/08/28.                                                
000500                                                                          
000600*    FUNCTION:                                                            
000700*        SUBPROGRAM TO WRITE 'GOODS RECEIVER LIST' TRANSPORT              
000800*        DOCUMENT. IT IS CALLED BY A PROGRAM W40631. DOCUMENT SHOW        
000900*        CASES PER CUSTOMER, FOR EVERY NEW COSTUMER THERE SHOULD          
001000*        BE A NEW LIST.                                                   
001100*                                                                         
001200*        THE PROGRAM READS     WDE1                                       
001300*        THE PROGRAM READS     WDR1                                       
001400*        THE PROGRAM READS     WDB1                                       
001500*        THE PROGRAM READS     WDB2                                       
001600*                                                                         
001700*                                                                         
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400                                                                          
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                         PIC X(8)    VALUE 'W476GMTL'.          
002700 77  W-CURRENT                     PIC X(50)   VALUE SPACE.               
002800                                                                          
002900 77  YES                           PIC X       VALUE 'J'.                 
003000 77  NOO                           PIC X       VALUE 'N'.                 
003100 77  IX                            PIC S9(9)   VALUE +0 COMP SYNC.        
003200 77  MAX-IX                        PIC S9(9)  VALUE +16 COMP SYNC.        
003300 77  INDX                          PIC S9(4)   VALUE +0 COMP SYNC.        
003400 77  WS-IX                         PIC S9(4)   VALUE +0 COMP SYNC.        
003500 77  INDX2                         PIC S9(4)   VALUE +0 COMP SYNC.        
003600                                                                          
003700 77  WS-PAGE-NO                    PIC 9(3)    VALUE ZERO.                
003800 77  W-KDSPRAK                     PIC S9      COMP-3.                    
003900 77  W-SUORDV                   PIC S9(9)V9(2) VALUE ZERO COMP-3.         
004000 77  W-KDVALISO                    PIC X(3)    VALUE SPACE.               
004100 77  DUMMY-AREA                    PIC X(50)  VALUE SPACE.                
004200                                                                          
004300     EJECT                                                                
004400                                                                          
004500*    --- STYRTECKEN PRINTER                                               
004600 01  WS-PAGESKIP                   PIC X      VALUE '1'.                  
004700 01  WS-SKIP1                      PIC X      VALUE ' '.                  
004800 01  WS-SKIP2                      PIC X      VALUE '0'.                  
004900 01  WS-SKIP3                      PIC X      VALUE '-'.                  
005000                                                                          
005100 01  WS-TYP-IDSHIP                 PIC X(19) VALUE                        
005200                                       'GOODS RECEIVER LIST'.             
       01  WS-META                       PIC X(5) VALUE '¤META'.                
       01  WS-IDDISTR                    PIC Z(4)9.                             
       01  WS-IDSHIPM                    PIC Z(6)9.                             
005300 01  WS-YYMMDD                     PIC 9(6)  VALUE ZERO.                  
005400 01  WS-GOODS-VALUE                PIC 9(8)9V9(2) VALUE ZERO.             
005500 01  WS-GOODS-VALUE-VALUE          PIC 9(8)9V9(2) VALUE ZERO.             
005600 01  WS-TOTAL-VALUE                PIC 9(8)9V9(2) VALUE ZERO.             
005700 01  WS-TEMP-VALUE                 PIC 9(8)9V9(2) VALUE ZERO.             
005800 01  WS-PRLEGKST-VALUE             PIC 9(8)9V9(2) VALUE ZERO.             
005900 01  WS-PREMBHNT-VALUE             PIC 9(8)9V9(2) VALUE ZERO.             
006000 01  WS-PRAVDRAG-VALUE             PIC 9(8)9V9(2) VALUE ZERO.             
006100 01  WS-KDVALISO                   PIC X(3) VALUE SPACE.                  
006200 01  WS-KDVALISO-2                 PIC X(3) VALUE SPACE.                  
006300 01  WS-KDFORSKN                   PIC 9(3) VALUE ZERO.                   
006400 01  WS-ANTAL-RAD-BEFORSKN         PIC 9(3) VALUE ZERO.                   
006500*    WS-BEFOSKN ANVÄNDS EJ, BORTTAGEN PGA COBOL-5 PROBLEM                 
006600*01  WS-BEFORSKN-TAB.                                                     
006700*    03 FILLER OCCURS 16.                                                 
006800*      05 WS-BEFORSKN-RAD          PIC X(60) VALUE ZERO.                  
006900 01  WS-SKOLLI-SUORDV              PIC 9(9)V9(2) VALUE ZERO.              
007000 01  WS-TOTAL-VALUE-PRKURS         PIC 9(5)9V9(5) VALUE ZERO.             
007100 01  WS-PRKURS                     PIC 9(5)9V9(5) VALUE ZERO.             
007200 01  WS-NUMBER-OF-KOLLI            PIC 9(4) VALUE ZERO.                   
007300 01  WS-TOTAL-KG                   PIC 9(6)V9(1) VALUE ZERO.              
007400 01  WS-TOTAL-M3                   PIC 9(4)V9(3) VALUE ZERO.              
007500                                                                          
007600 01  W-LINE.                                                              
007700     03  W-LINE-COUNT              PIC 9(02) VALUE ZERO.                  
007800     03  W-LINE-MAX                PIC 9(02) VALUE 43.                    
007900                                                                          
008000 01  TODAYS-DATE                   PIC 9(6)  VALUE ZERO.                  
008100 01  FILLER REDEFINES TODAYS-DATE.                                        
008200     03  TODAYS-DATE-YEAR          PIC 9(2).                              
008300     03  TODAYS-DATE-MONTH         PIC 9(2).                              
008400     03  TODAYS-DATE-DAY           PIC 9(2).                              
008500     EJECT                                                                
                                                                                
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
008600                                                                          
008700 01  TABELL.                                                              
008800   03  HELP-TABELL OCCURS 16.                                             
008900     05  W-TEXT-BEEMBTYP         PIC X(12).                               
009000                                                                          
009100 01  ARB-RAD                       PIC X(132) VALUE SPACE.                
009200                                                                          
009300 01  RAD1-HEAD.                                                           
009400     03  FILLER                    PIC X(15).                             
009500     03  RAD1H-IMPORTER-TEXT       PIC X(13).                             
009600     03  FILLER                    PIC X(2).                              
009700     03  RAD1H-IMPORTER            PIC X(35).                             
009800     03  FILLER                    PIC X(2).                              
009900     03  RAD1H-TYP-IDSHIP          PIC X(25).                             
010000 01  RAD2-HEAD.                                                           
010100     03  FILLER                    PIC X(30).                             
010200     03  RAD2H-IMPORTER            PIC X(35).                             
010300                                                                          
010400 01  RAD3-HEAD.                                                           
010500     03  FILLER                    PIC X(30).                             
010600     03  RAD3H-IMPORTER            PIC X(35).                             
010700                                                                          
010800 01  RAD4-HEAD.                                                           
010900     03  FILLER                    PIC X(30).                             
011000     03  RAD4H-IMPORTER            PIC X(35).                             
011100     03  FILLER                    PIC X(02).                             
011200     03  RAD4H-TIYYMMDD            PIC 9(06).                             
011300     03  RAD4H-IDDISTR             PIC Z(4)9.                             
011400     03  FILLER                    PIC X(01).                             
011500     03  RAD4H-IDSHIPM             PIC Z(6)9.                             
011600     03  FILLER                    PIC X(2).                              
011700     03  RAD4H-IDTRPTNR            PIC Z(02)9.                            
011800     03  FILLER                    PIC X(01).                             
011900     03  RAD4H-IDLBBET             PIC X(12).                             
012000     03  FILLER                    PIC X(02).                             
012100     03  RAD4H-PAGE-NO             PIC Z(03).                             
012200                                                                          
012300 01  RAD5-HEAD.                                                           
012400     03  FILLER                    PIC X(30).                             
012500     03  RAD5H-IMPORTER            PIC X(35).                             
012600                                                                          
012700 01  RAD1.                                                                
012800     03  FILLER                    PIC X(1).                              
012900     03  RAD1-DEALER-TEXT          PIC X(12).                             
013000     03  FILLER                    PIC X(2).                              
013100     03  RAD1-IDKUNDNR             PIC Z(7).                              
013200     03  FILLER                    PIC X(8).                              
013300     03  RAD1-BEGMT-1              PIC X(35).                             
013400     03  FILLER                    PIC X(5).                              
013500     03  RAD1-BEGMT-2              PIC X(35).                             
013600                                                                          
013700 01  RAD2.                                                                
013800     03  FILLER                    PIC X(30).                             
013900     03  RAD2-BEGMT-3              PIC X(35).                             
014000     03  FILLER                    PIC X(5).                              
014100     03  RAD2-BEGMT-4              PIC X(35).                             
014200                                                                          
014300 01  RAD3.                                                                
014400     03  FILLER                    PIC X(70).                             
014500     03  RAD3-BEGMT-5              PIC X(35).                             
014600                                                                          
014700 01  RAD4.                                                                
014800     03  FILLER                    PIC X(1).                              
014900     03  RAD4-ORDER-TEXT           PIC X(10).                             
015000     03  FILLER                    PIC X(2).                              
015100     03  RAD4-CASE-TEXT            PIC X(11).                             
015200     03  FILLER                    PIC X(2).                              
015300     03  RAD4-PACKAGE-TEXT         PIC X(12).                             
015400     03  FILLER                    PIC X(3).                              
015500     03  RAD4-L-TEXT               PIC X(5).                              
015600     03  FILLER                    PIC X(3).                              
015700     03  RAD4-W-TEXT               PIC X(3).                              
015800     03  FILLER                    PIC X(3).                              
015900     03  RAD4-H-TEXT               PIC X(3).                              
016000     03  FILLER                    PIC X(3).                              
016100     03  RAD4-GROSS-TEXT           PIC X(8).                              
016200     03  FILLER                    PIC X(4).                              
016300     03  RAD4-VOLUME-TEXT          PIC X(8).                              
016400     03  FILLER                    PIC X(2).                              
016500     03  RAD4-HAZARD-TEXT          PIC X(17).                             
016600                                                                          
016700 01  RAD5.                                                                
016800     03  FILLER                    PIC X(1).                              
016900     03  RAD5-ORDER                PIC Z(5).                              
017000     03  FILLER                    PIC X(8).                              
017100     03  RAD5-CASE                 PIC Z(5).                              
017200     03  FILLER                    PIC X(7).                              
017300     03  RAD5-PACKAGE              PIC X(12).                             
017400     03  FILLER                    PIC X(3).                              
017500     03  RAD5-L                    PIC Z(5).                              
017600     03  FILLER                    PIC X(3).                              
017700     03  RAD5-W                    PIC Z(3).                              
017800     03  FILLER                    PIC X(3).                              
017900     03  RAD5-H                    PIC Z(3).                              
018000     03  FILLER                    PIC X(3).                              
018100     03  RAD5-GROSS                PIC Z(6).9(1).                         
018200     03  FILLER                    PIC X(6).                              
018300     03  RAD5-VOLUME               PIC Z(4).9(3).                         
018400     03  FILLER                    PIC X(9).                              
018500     03  RAD5-HAZARD               PIC X(1).                              
018600                                                                          
018700 01  RAD6.                                                                
018800     03  FILLER                    PIC X(1).                              
018900     03  RAD6-TOTAL-TEXT           PIC X(12).                             
019000     03  FILLER                    PIC X(5).                              
019100     03  RAD6-TOTAL                PIC Z(5).                              
019200     03  FILLER                    PIC X(1).                              
019300     03  RAD6-PACKAGE-TEXT         PIC X(7).                              
019400     03  FILLER                    PIC X(5).                              
019500     03  RAD6-WEIGHT               PIC Z(6).9(1).                         
019600     03  FILLER                    PIC X(1).                              
019700     03  RAD6-KG-TEXT              PIC X(4).                              
019800     03  FILLER                    PIC X(5).                              
019900     03  RAD6-VOLUME               PIC Z(4).9(3).                         
020000     03  FILLER                    PIC X(1).                              
020100     03  RAD6-M3-TEXT              PIC X(4).                              
020200                                                                          
020300 01  FILLER          PIC X(32)  VALUE  'LEDTEXT TABLE'.                   
020400* 01 -COPY W475W552                                                       
020500     EJECT                                                                
020600* 01 -COPY W475W553                                                       
020700     EJECT                                                                
020800* 01 -COPY W475W557                                                       
020900     EJECT                                                                
021000* 01 -COPY W476W001                                                       
021100     EJECT                                                                
021200                                                                          
021300 01  GENERAL-SUBPROGRAMS.                                                 
021400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
021500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
021600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
021700     03  W006PRS1                PIC X(8)    VALUE 'W006PRS1'.            
021800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
021900     SKIP2                                                                
022000*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
022100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
022200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
022300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
022400     SKIP2                                                                
022500 01  ERRTEXT.                                                             
022600     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
022700     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
022800 77  KDRC-DISPLAY                PIC Z(5).                                
022900     EJECT                                                                
023000*    --- PARAMETERS FOR SUBPROGRAM W006PRS1                               
023100     EJECT                                                                
023200*01  -COPY W006PRAR                                                       
023300 01  W-NYSIDA-RAD10          PIC S9(3)   VALUE +910  COMP-3.              
023400*                                        WRITE ON NEW LINE 10             
023500 01  W-IDPRTLST                  PIC X(8).                                
023600     SKIP2                                                                
023700*    --- AREAS FOR IMS-SECTIONS                                           
023800*                                                                         
023900     EJECT                                                                
024000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024100     SKIP3                                                                
024200 01  KEYS-TO-DLI.                                                         
024300     03  W-WDB101KY-X.                                                    
024400         05  W-WDB101-IDPARTNR   PIC X(09)   VALUE SPACE.                 
024500         05  W-WDB101-IDFTG      PIC 9(02)   VALUE ZERO.                  
024600                                                                          
024700     03  W-WDB201KY-X.                                                    
024800         05  W-WDB201-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
024900         05  W-WDB201-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
025000                                                                          
025100     03  W-IDSHIPM-X.                                                     
025200         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
025300                                                                          
025400     03  W-WDE111KY-X.                                                    
025500         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
025600         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
025700                                                                          
025800     03  W-WDE111KY-D.                                                    
025900         05  W-WDE111-IDDISTR-D  PIC S9(05)  VALUE ZERO COMP-3.           
026000         05  W-WDE111-IDKUNDNR-D PIC S9(07)  VALUE ZERO COMP-3.           
026100                                                                          
026200     03  W-WDE111KY-MIN.                                                  
026300         05  W-WDE111-IDDISTR-MIN PIC S9(05)  VALUE ZERO COMP-3.          
026400         05  FILLER               PIC X(04)   VALUE LOW-VALUES.           
026500                                                                          
026600     03  W-WDE111KY-MAX.                                                  
026700         05  W-WDE111-IDDISTR-MAX PIC S9(05)  VALUE ZERO COMP-3.          
026800         05  FILLER               PIC X(04)   VALUE HIGH-VALUES.          
026900                                                                          
027000     03    W-KDEMBTYP-4738-X.                                             
027100         05    IDHTYP             PIC X(4)    VALUE '4738'.               
027200         05    W-KDEMBTYP-4738    PIC S9(3)   COMP-3 VALUE ZERO.          
027300         05    FILLER             PIC X(24)   VALUE LOW-VALUE.            
027400                                                                          
027500*    03     -COPY WDGX01                                                  
027600                                                                          
027700                                                                          
027800*    --- STATUS-KOD FRÅN IMS                                              
027900 01  STATUS-WS                   PIC XX.                                  
028000     88  SEGMENT-FOUND                       VALUE '  '.                  
028100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
028200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
028300                                                                          
028400 01  GOOD-STATUSCODES.                                                    
028500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028600                                                                          
028700 01  SSA1                        PIC X(64).                               
028800 01  SSA2                        PIC X(64).                               
028900 01  SSA3                        PIC X(64).                               
029000     EJECT                                                                
029100                                                                          
029200 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
029300 01  SEND-AREA.                                                           
029400*    03  -COPY WZ01SEND                                                   
029500                                                                          
029600 01  SEND-RAD-STYRTECKEN.                                                 
029700     03  STYRTECKEN-RAD          PIC X.                                   
029800     03  SEND-RAD                PIC X(120)  VALUE SPACE.                 
029900                                                                          
030000 01  DAP-AREA-START              PIC X(24)   VALUE                        
030100                                             'DAP-AREA-START'.            
030200                                                                          
030300*    ---  IMS FUNCTION CODES                                              
030400*01  -COPY W0003                                                          
030500     EJECT                                                                
030600                                                                          
030700*    ---  DLI INPUT-OUTPUT AREA                                           
030800 01  FILLER         PIC X(20) VALUE 'DLI-IO-WDE101-WDE111'.               
030900 01  DLI-IO-WDE101-11.                                                    
031000     03  DLI-IO-WDE101.                                                   
031100*        05  -COPY WDE101                                                 
031200     03  DLI-IO-WDE111.                                                   
031300*        05  -COPY WDE111                                                 
031400     EJECT                                                                
031500                                                                          
031600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
031700 01  DLI-IO-WDE121.                                                       
031800*    03  -COPY WDE121                                                     
031900                                                                          
032000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
032100 01  DLI-IO-WDB101.                                                       
032200*    03  -COPY WDB101                                                     
032300                                                                          
032400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
032500 01  DLI-IO-WDB201.                                                       
032600*    03  -COPY WDB201                                                     
032700                                                                          
032800 01  DLI-IO-AREA.                                                         
032900     03  IO-AREA  PIC X(100)                 VALUE SPACE.                 
033000     SKIP3                                                                
033100     03  WL473811 REDEFINES IO-AREA.                                      
033200*       05 -COPY WDGX4738                                                 
033300     EJECT                                                                
033400                                                                          
033500 LINKAGE SECTION.                                                         
033600*01  -COPY W476TRPD                                                       
033700                                                                          
033800 01  ALT-PCB                     PIC X(32).                               
033900                                                                          
034000*01  -COPY W0008  -PRE WDE1-                                              
034100     05  FILLER                  PIC X.                                   
034200                                                                          
034300*01  -COPY W0008  -PRE WDB1-                                              
034400     05  FILLER                  PIC X.                                   
034500                                                                          
034600*01  -COPY W0008  -PRE WDB2-                                              
034700     05  FILLER                  PIC X.                                   
034800                                                                          
034900*01  -COPY W0008  -PRE 4738-                                              
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200                                                                          
035300 PROCEDURE DIVISION  USING                                                
035400                           TRPD-W476TRPD ALT-PCB                          
035500                           WDE1-PCB WDB1-PCB WDB2-PCB 4738-PCB.           
035600                                                                          
035700 MAIN SECTION.                                                            
035800     ENTRY 'DLITCBL' USING                                                
035900                           TRPD-W476TRPD ALT-PCB                          
036000                           WDE1-PCB WDB1-PCB WDB2-PCB 4738-PCB.           
036100     PERFORM A-INIT                                                       
036200                                                                          
036300     PERFORM IMS-GU-WDE101                                                
036400     PERFORM IMS-GNP-WDE111                                               
036500     PERFORM UNTIL NOT SEGMENT-FOUND                                      
036600       IF WS-IX = ZERO                                                    
036700         PERFORM B-IMPORTER                                               
036800       END-IF                                                             
036900       PERFORM C-GOODS-RECEIVER                                           
037000       PERFORM IMS-GNP-WDE111                                             
037100     END-PERFORM                                                          
037200                                                                          
037300     MOVE ZERO TO RETURN-CODE                                             
037400     GOBACK                                                               
037500     .                                                                    
037600     EJECT                                                                
037700 A-INIT SECTION.                                                          
037800                                                                          
037900     MOVE LENGTH OF SEND-RAD           TO SEND-KVDLEN                     
038000     MOVE FUNCTION CURRENT-DATE (3:6)  TO TODAYS-DATE                     
038100                                                                          
038200     MOVE SPACE           TO  RAD1                                        
038300                              RAD2                                        
038400                              RAD3                                        
038500                              RAD4                                        
038600                              RAD5                                        
038700                              RAD6                                        
038800                              RAD1-HEAD                                   
038900                              RAD2-HEAD                                   
039000                              RAD3-HEAD                                   
039100                              RAD4-HEAD                                   
039200                              RAD5-HEAD                                   
039300*    WS-BEFOSKN ANVÄNDS EJ, BORTTAGEN PGA COBOL-5 PROBLEM                 
039400*                             WS-BEFORSKN-RAD(INDX2)                      
039500                                                                          
039600     MOVE TRPD-IDPRTLST   TO  W-IDPRTLST                                  
039700     MOVE TRPD-IDSHIPM    TO  W-IDSHIPM                                   
039800     MOVE TRPD-PFDEF-OVR  TO  PRT-PFDEF-OVR                               
039900     MOVE TRPD-IDDISTR    TO  W-WDE111-IDDISTR                            
040000                              W-WDB201-IDDISTR                            
040100                              W-WDE111-IDDISTR-MIN                        
040200                              W-WDE111-IDDISTR-MAX                        
040300                                                                          
040400     MOVE ZERO            TO  WS-TEMP-VALUE                               
040500                              WS-IX                                       
040600                              WS-GOODS-VALUE                              
040700                              WS-TOTAL-VALUE                              
040800                              WS-YYMMDD                                   
040900                              WS-KDVALISO                                 
041000                              WS-SKOLLI-SUORDV                            
041100                              WS-PRLEGKST-VALUE                           
041200                              WS-PREMBHNT-VALUE                           
041300                              WS-PRAVDRAG-VALUE                           
041400                              WS-PAGE-NO                                  
041500                              INDX                                        
041600                              INDX2                                       
                                    WS-IDDISTR                                  
                                    WS-IDSHIPM                                  
041700                                                                          
041800     MOVE 2                TO W-KDSPRAK                                   
041900                                                                          
042000     MOVE +1 TO IX                                                        
042100     PERFORM UNTIL IX > MAX-IX                                            
042200       MOVE SPACE TO W-TEXT-BEEMBTYP (IX)                                 
042300       ADD +1 TO IX                                                       
042400     END-PERFORM                                                          
042500                                                                          
042600     MOVE +1 TO IX                                                        
042700     MOVE IX TO W-KDEMBTYP-4738                                           
042800     PERFORM IMS-GET-4738                                                 
042900     IF SEGMENT-FOUND                                                     
043000       PERFORM UNTIL IX > MAX-IX                                          
043100         IF SEGMENT-FOUND                                                 
043200          MOVE EMBTYP-BEEMBTYP (2) TO W-TEXT-BEEMBTYP (IX)                
043300         END-IF                                                           
043400         ADD +1        TO IX                                              
043500         MOVE IX TO W-KDEMBTYP-4738                                       
043600         PERFORM IMS-GET-4738                                             
043700       END-PERFORM                                                        
043800     END-IF                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 B-IMPORTER SECTION.                                                      
044200                                                                          
044300     MOVE SGMT-IDKUNDNR  TO W-WDE111-IDKUNDNR                             
044400                            W-WDB201-IDKUNDNR                             
044500     PERFORM IMS-GU-WDB201                                                
044600     MOVE SGMT-IDPARTNR  TO W-WDB101-IDPARTNR                             
044700     MOVE GMT-IDFTG      TO W-WDB101-IDFTG                                
044800     PERFORM IMS-GU-WDB101                                                
044900     MOVE BET-BEBETRAD-1 TO RAD1H-IMPORTER                                
045000     MOVE BET-BEBETRAD-2 TO RAD2H-IMPORTER                                
045100     MOVE BET-ADBETRAD-1 TO RAD3H-IMPORTER                                
045200     MOVE BET-ADBETRAD-2 TO RAD4H-IMPORTER                                
045300     MOVE BET-BELAND-SVE TO RAD5H-IMPORTER                                
045400     ADD +1 TO WS-IX                                                      
045500     .                                                                    
045600     EJECT                                                                
045700 C-GOODS-RECEIVER SECTION.                                                
045800                                                                          
045900     PERFORM CA-DEALER                                                    
046000                                                                          
046100     PERFORM CB-GOODS-DETAIL                                              
046200                                                                          
046300     PERFORM CC-TOTAL-ROWS                                                
046400     .                                                                    
046500     EJECT                                                                
046600 CA-DEALER SECTION.                                                       
046700                                                                          
046800     MOVE SGMT-IDKUNDNR  TO W-WDB201-IDKUNDNR                             
046900                            RAD1-IDKUNDNR                                 
047000     PERFORM IMS-GU-WDB201                                                
047100     MOVE GMT-BEGMT-RAD1 TO RAD1-BEGMT-1                                  
047200     MOVE GMT-BEGMT-RAD2 TO RAD2-BEGMT-3                                  
047300     MOVE GMT-ADGMT-GATA TO RAD1-BEGMT-2                                  
047400     MOVE GMT-ADGMT-PADR TO RAD2-BEGMT-4                                  
047500     MOVE GMT-ADGMT-LAND TO RAD3-BEGMT-5                                  
047600                                                                          
047700     IF GMT-KDSPRAK  <  ZERO OR > +5                                      
047800       MOVE +2 TO W-KDSPRAK                                               
047900     ELSE                                                                 
048000       COMPUTE W-KDSPRAK = GMT-KDSPRAK + +1                               
048100     END-IF                                                               
048200                                                                          
           IF WS-PAGE-NO = ZEROES                                               
              PERFORM S03-PRINT-META                                            
           END-IF                                                               
                                                                                
048300     PERFORM S01-PRINT-HEAD                                               
048400     PERFORM CAA-PRINT-ROW1                                               
048500     PERFORM CAB-PRINT-ROW2                                               
048600     PERFORM CAC-PRINT-ROW3                                               
048700     .                                                                    
048800     EJECT                                                                
048900 CAA-PRINT-ROW1 SECTION.                                                  
049000                                                                          
049100     MOVE IDDEALER-LEDTEXT (W-KDSPRAK) TO RAD1-DEALER-TEXT                
049200     MOVE RAD1                       TO ARB-RAD                           
049300                                        SEND-RAD                          
049400     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
049500     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
049600     ADD 2                           TO W-LINE-COUNT                      
049700     PERFORM S10-PRINT-LINE                                               
049800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
049900     .                                                                    
050000     EJECT                                                                
050100 CAB-PRINT-ROW2 SECTION.                                                  
050200                                                                          
050300     MOVE RAD2                       TO ARB-RAD                           
050400                                        SEND-RAD                          
050500     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
050600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
050700     ADD 1                           TO W-LINE-COUNT                      
050800     PERFORM S10-PRINT-LINE                                               
050900     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
051000     .                                                                    
051100     EJECT                                                                
051200 CAC-PRINT-ROW3 SECTION.                                                  
051300                                                                          
051400     MOVE RAD3                       TO ARB-RAD                           
051500                                        SEND-RAD                          
051600     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
051700     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
051800     ADD 1                           TO W-LINE-COUNT                      
051900     PERFORM S10-PRINT-LINE                                               
052000     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
052100     .                                                                    
052200     EJECT                                                                
052300 CB-GOODS-DETAIL SECTION.                                                 
052400                                                                          
052500     MOVE SGMT-IDKUNDNR  TO W-WDE111-IDKUNDNR                             
052600     PERFORM IMS-GNP-WDE121                                               
052700     PERFORM S02-PRINT-ROW4                                               
052800                                                                          
052900     PERFORM UNTIL SEGMENT-MISSING                                        
053000        PERFORM CBA-ORDER-ROWS                                            
053100        PERFORM IMS-GNP-WDE121                                            
053200     END-PERFORM                                                          
053300     .                                                                    
053400     EJECT                                                                
053500 CBA-ORDER-ROWS SECTION.                                                  
053600                                                                          
053700     MOVE SKOLLI-IDORDNR7(3:5)  TO RAD5-ORDER                             
053800     MOVE SKOLLI-IDKOLLI        TO RAD5-CASE                              
053900     MOVE SKOLLI-DIKOLLIL       TO RAD5-L                                 
054000     MOVE SKOLLI-DIKOLLIB       TO RAD5-W                                 
054100     MOVE SKOLLI-DIKOLLIH       TO RAD5-H                                 
054200     MOVE SKOLLI-VKORDBTO-KOLLI TO RAD5-GROSS                             
054300     MOVE SKOLLI-VLORDBTO-KOLLI TO RAD5-VOLUME                            
054400                                                                          
054500     IF SKOLLI-KDFARLIG-KOLLI = +2 OR +4 OR +5 OR +7                      
054600       MOVE YES   TO RAD5-HAZARD                                          
054700     ELSE                                                                 
054800       MOVE SPACE TO RAD5-HAZARD                                          
054900     END-IF                                                               
055000                                                                          
055100     IF SKOLLI-KDEMBTYP > +0                                              
055200       MOVE W-TEXT-BEEMBTYP (SKOLLI-KDEMBTYP) TO RAD5-PACKAGE             
055300     END-IF                                                               
055400                                                                          
055500     COMPUTE WS-NUMBER-OF-KOLLI = WS-NUMBER-OF-KOLLI + 1                  
055600                                                                          
055700     COMPUTE WS-TOTAL-KG = WS-TOTAL-KG + SKOLLI-VKORDBTO-KOLLI            
055800                                                                          
055900     COMPUTE WS-TOTAL-M3 = WS-TOTAL-M3 + SKOLLI-VLORDBTO-KOLLI            
056000                                                                          
056100     PERFORM CBAA-PRINT-ROW5                                              
056200     .                                                                    
056300     EJECT                                                                
056400 CBAA-PRINT-ROW5 SECTION.                                                 
056500                                                                          
056600     IF W-LINE-COUNT > W-LINE-MAX - 2                                     
056700       PERFORM S01-PRINT-HEAD                                             
056800       PERFORM S02-PRINT-ROW4                                             
056900     END-IF                                                               
057000     MOVE RAD5                       TO ARB-RAD                           
057100                                        SEND-RAD                          
057200     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
057300     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
057400     ADD 2                           TO W-LINE-COUNT                      
057500     PERFORM S10-PRINT-LINE                                               
057600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
057700     .                                                                    
057800     EJECT                                                                
057900 CC-TOTAL-ROWS SECTION.                                                   
058000                                                                          
058100     MOVE WS-NUMBER-OF-KOLLI TO RAD6-TOTAL                                
058200     MOVE WS-TOTAL-KG        TO RAD6-WEIGHT                               
058300     MOVE WS-TOTAL-M3        TO RAD6-VOLUME                               
058400                                                                          
058500     PERFORM CCA-PRINT-ROW6                                               
058600     .                                                                    
058700     EJECT                                                                
058800 CCA-PRINT-ROW6 SECTION.                                                  
058900                                                                          
059000     IF W-LINE-COUNT > W-LINE-MAX - 3                                     
059100       PERFORM S01-PRINT-HEAD                                             
059200     END-IF                                                               
059300     MOVE PRODGRP-SUMMA-LEDTEXT (W-KDSPRAK) TO RAD6-TOTAL-TEXT            
059400     MOVE IDKUNDRF-IDKOLLI-TOT-LEDTEXT (W-KDSPRAK)                        
059500                                      TO RAD6-PACKAGE-TEXT                
059600     MOVE TULLC2-VIKT-LEDTEXT         TO RAD6-KG-TEXT                     
059700     MOVE TULLC2-VOL-LEDTEXT          TO RAD6-M3-TEXT                     
059800     MOVE RAD6                        TO ARB-RAD                          
059900                                         SEND-RAD                         
060000     MOVE PRT-AFTER-3                 TO PRT-RADSKIP                      
060100     MOVE WS-SKIP3                    TO STYRTECKEN-RAD                   
060200     ADD 3                            TO W-LINE-COUNT                     
060300     PERFORM S10-PRINT-LINE                                               
060400     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
060500     .                                                                    
060600     EJECT                                                                
060700 S01-PRINT-HEAD SECTION.                                                  
060800                                                                          
060900     ADD 1                           TO WS-PAGE-NO                        
061000     MOVE BEVARREF-LEDTEXT (W-KDSPRAK) TO RAD1H-IMPORTER-TEXT             
061100                                                                          
061200     MOVE SPACE                      TO SEND-RAD                          
061300     MOVE WS-PAGESKIP                TO STYRTECKEN-RAD                    
061400     PERFORM S90-PUT-DOC-LINE                                             
061500     MOVE WS-SKIP3                   TO STYRTECKEN-RAD                    
061600     PERFORM S90-PUT-DOC-LINE                                             
061700     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
061800     PERFORM S90-PUT-DOC-LINE                                             
061900                                                                          
062000     MOVE WS-TYP-IDSHIP              TO RAD1H-TYP-IDSHIP                  
062100     MOVE RAD1-HEAD                  TO ARB-RAD                           
062200                                        SEND-RAD                          
062300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
062400     MOVE PRT-NYSIDA-RAD7            TO PRT-RADSKIP                       
062500     MOVE 7                          TO W-LINE-COUNT                      
062600     PERFORM S10-PRINT-LINE                                               
062700     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
062800                                                                          
062900     MOVE RAD2-HEAD                  TO ARB-RAD                           
063000                                        SEND-RAD                          
063100     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
063200     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
063300     ADD 1                           TO W-LINE-COUNT                      
063400     PERFORM S10-PRINT-LINE                                               
063500     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
063600                                                                          
063700     MOVE RAD3-HEAD                  TO ARB-RAD                           
063800                                        SEND-RAD                          
063900     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
064000     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
064100     ADD 1                           TO W-LINE-COUNT                      
064200     PERFORM S10-PRINT-LINE                                               
064300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
064400                                                                          
064500     MOVE SHIP-TISKEPPN              TO WS-YYMMDD                         
064600     MOVE WS-YYMMDD                  TO RAD4H-TIYYMMDD                    
064700     MOVE SGMT-IDDISTR               TO RAD4H-IDDISTR                     
064800     MOVE SHIP-IDSHIPM               TO RAD4H-IDSHIPM                     
064900     MOVE SHIP-IDTRPTNR              TO RAD4H-IDTRPTNR                    
065000     MOVE SHIP-IDLBBET               TO RAD4H-IDLBBET                     
065100     MOVE WS-PAGE-NO                 TO RAD4H-PAGE-NO                     
065200     MOVE RAD4-HEAD                  TO ARB-RAD                           
065300                                        SEND-RAD                          
065400     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
065500     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
065600     ADD 1                           TO W-LINE-COUNT                      
065700     PERFORM S10-PRINT-LINE                                               
065800     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
065900                                                                          
066000     MOVE RAD5-HEAD                  TO ARB-RAD                           
066100                                        SEND-RAD                          
066200     MOVE PRT-AFTER-1                TO PRT-RADSKIP                       
066300     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
066400     ADD 1                           TO W-LINE-COUNT                      
066500     PERFORM S10-PRINT-LINE                                               
066600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
066700     .                                                                    
066800     EJECT                                                                
066900 S02-PRINT-ROW4 SECTION.                                                  
067000                                                                          
067100     MOVE IDKUNDRF-LEDTEXT (W-KDSPRAK) TO RAD4-ORDER-TEXT                 
067200     MOVE IDKUNDRF-IDKOLLI-LEDTEXT (W-KDSPRAK) TO RAD4-CASE-TEXT          
067300     MOVE BEEMBTYP-LEDTEXT (W-KDSPRAK) TO RAD4-PACKAGE-TEXT               
067400     MOVE DIKOLLIL-LEDTEXT (W-KDSPRAK) TO RAD4-L-TEXT                     
067500     MOVE DIKOLLIB-LEDTEXT (W-KDSPRAK) TO RAD4-W-TEXT                     
067600     MOVE DIKOLLIH-LEDTEXT (W-KDSPRAK) TO RAD4-H-TEXT                     
067700     MOVE VKORDBTO-LEDTEXT (W-KDSPRAK) TO RAD4-GROSS-TEXT                 
067800     MOVE VLORDBTO-LEDTEXT (W-KDSPRAK) TO RAD4-VOLUME-TEXT                
067900     MOVE BEFARLIG-TEXT (W-KDSPRAK)  TO RAD4-HAZARD-TEXT                  
068000     MOVE RAD4                       TO ARB-RAD                           
068100                                        SEND-RAD                          
068200     MOVE PRT-AFTER-2                TO PRT-RADSKIP                       
068300     MOVE WS-SKIP2                   TO STYRTECKEN-RAD                    
068400     ADD 2                           TO W-LINE-COUNT                      
068500     PERFORM S10-PRINT-LINE                                               
068600     MOVE WS-SKIP1                   TO STYRTECKEN-RAD                    
068700     .                                                                    
068800     EJECT                                                                
                                                                                
       S03-PRINT-META SECTION.                                                  
                                                                                
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
                  'SHIPDOC_GRL'                                                 
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  FUNCTION TRIM (WS-IDSHIPM)                                    
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  FUNCTION TRIM (WS-IDDISTR)                                    
                  DELIMITED BY SIZE                                             
                  '_'                                                           
                  DELIMITED BY SIZE                                             
                  WS-TIMESTAMP                                                  
                  DELIMITED BY SIZE INTO SEND-RAD                               
                                                                                
           PERFORM S90-PUT-DOC-LINE                                             
           .                                                                    
                                                                                
068900 S10-PRINT-LINE SECTION.                                                  
069000                                                                          
069100*    -- PRINT TO ON-DEMAND IF SO SPECIFIED ON 4456                        
069200     PERFORM S90-PUT-DOC-LINE                                             
069300                                                                          
069400*    -- PRINT TO PAPER IF SO SPECIFIED ON 4664 (FLSKRIV-NU = YES)         
069500*    -- OR ALWAYS IF WE COME FROM 4622                                    
069600*    -- OR ALWAYS IF IT IS A WEB DC (WHERE FLSKRIV-NU IS N AND            
069700*    -- CANNOT BE SET TO J)                                               
069800     IF SHIP-FLSKRIV-NU = YES OR TRPD-IDPGM = 'W4062200'                  
069900     OR TRPD-FLLDCKND = YES                                               
070000       CALL W006PRS1 USING PRT-SPOOL-OVR                                  
070100                           PRT-WRITE                                      
070200                           W-IDPRTLST                                     
070300                           ALT-PCB                                        
070400                           PRT-RADSKIP                                    
070500                           ARB-RAD                                        
070600     END-IF                                                               
070700     .                                                                    
070800     EJECT                                                                
070900 S90-PUT-DOC-LINE SECTION.                                                
071000     IF TRPD-IDPGM = 'W4063600' AND TRPD-KVCOPIES = '1'                   
071100       IF TRPD-FLSKRIV-ONDEM = YES                                        
071200         MOVE +1                          TO SEND-IDCOM                   
071300         MOVE 'PUT'                       TO SEND-KDFUNC                  
071400         MOVE LENGTH OF SEND-RAD-STYRTECKEN TO SEND-KVDLEN                
071500         CALL WZ01SEND USING SEND-CONTROL-AREA                            
071600                             SEND-KVDLEN                                  
071700                             SEND-RAD-STYRTECKEN                          
071800         IF SEND-KDRC > ZERO                                              
071900           MOVE SEND-KDRC                 TO KDRC-DISPLAY                 
072000           STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                   
072100           DELIMITED BY SIZE INTO ERRTEXT-STR                             
072200           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
072300         END-IF                                                           
072400       END-IF                                                             
072500     END-IF                                                               
072600     .                                                                    
072700     EJECT                                                                
072800 IMS-GU-WDE101 SECTION.                                                   
072900                                                                          
073000     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
073100          DELIMITED BY SIZE INTO SSA1                                     
073200     MOVE '  GE' TO GOOD-STATUSCODES                                      
073300     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
073400     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
073500     PERFORM IMS-STATUSCHECK                                              
073600     .                                                                    
073700     EJECT                                                                
073800 IMS-GNP-WDE111 SECTION.                                                  
073900                                                                          
074000     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
074100          DELIMITED BY SIZE INTO SSA1                                     
074200     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
074300                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
074400          DELIMITED BY SIZE INTO SSA2                                     
074500     MOVE '  GE' TO GOOD-STATUSCODES                                      
074600     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2              
074700     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
074800     PERFORM IMS-STATUSCHECK                                              
074900     .                                                                    
075000     EJECT                                                                
075100 IMS-GNP-WDE121 SECTION.                                                  
075200                                                                          
075300     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
075400          DELIMITED BY SIZE INTO SSA1                                     
075500     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
075600          DELIMITED BY SIZE INTO SSA2                                     
075700     MOVE 'WDE121  '          TO SSA3                                     
075800     MOVE '  GE' TO GOOD-STATUSCODES                                      
075900     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2 SSA3         
076000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
076100     PERFORM IMS-STATUSCHECK                                              
076200     .                                                                    
076300     EJECT                                                                
076400 IMS-GU-WDB101 SECTION.                                                   
076500                                                                          
076600     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
076700          DELIMITED BY SIZE INTO SSA1                                     
076800     MOVE '  GE' TO GOOD-STATUSCODES                                      
076900     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
077000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
077100     PERFORM IMS-STATUSCHECK                                              
077200     .                                                                    
077300     EJECT                                                                
077400 IMS-GU-WDB201 SECTION.                                                   
077500                                                                          
077600     STRING 'WDB201  (IDGMT    =' W-WDB201KY-X ')'                        
077700          DELIMITED BY SIZE INTO SSA1                                     
077800     MOVE '  GE' TO GOOD-STATUSCODES                                      
077900     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
078000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
078100     PERFORM IMS-STATUSCHECK                                              
078200     .                                                                    
078300     EJECT                                                                
078400 IMS-GET-4738 SECTION.                                                    
078500                                                                          
078600     STRING 'WDR101  (WDGXKEY  =' W-KDEMBTYP-4738-X ')'                   
078700            DELIMITED BY SIZE INTO SSA1                                   
078800     MOVE 'WDGX4738' TO SSA2                                              
078900     MOVE '  GE' TO GOOD-STATUSCODES                                      
079000     CALL CBLTDLI USING GU 4738-PCB DLI-IO-AREA SSA1 SSA2                 
079100     MOVE 4738-STATUS-CODE TO STATUS-WS                                   
079200     PERFORM IMS-STATUSCHECK                                              
079300     .                                                                    
079400     EJECT                                                                
079500 IMS-STATUSCHECK SECTION.                                                 
079600                                                                          
079700     SET STATUS-IX TO 1                                                   
079800     SEARCH GOOD-STATUS                                                   
079900       AT END                                                             
080000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
080100           DELIMITED BY SIZE INTO ERRTEXT                                 
080200         DISPLAY ERRTEXT                                                  
080300         CALL FELLOG                                                      
080400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
080500         CONTINUE                                                         
080600     END-SEARCH                                                           
080700     .                                                                    
