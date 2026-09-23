000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011410.                                                
000300 AUTHOR.         LARS THELL.                                              
000400 DATE-WRITTEN.   92/02/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        MPP FÖR ATT RÄTTA/ÄNDRA FÖLJESEDELINFO                           
000900*                                                                         
001000*        PROGRAMMET          UPPDATERAR W6INLA (W6D1)                     
001100*                            LÄSER      WLLEVA (WDF1)                     
001200*                            LÄSER      WLARTC (WDK6)                     
001300*                            LÄSER      WLINLB (WDD9)                     
001400*    SUB PROGRAMMET W611REG  UPPDATERAR W6INLA (W6D1)                     
001500*                            LÄSER      WLLEVA (WDF1)                     
001600*                            LÄSER      WLARTC (WDK6)                     
001700*                            LÄSER      WLARTS (WDK7)                     
001800*                            LÄSER      WLBENA (WDD3)                     
001900*                            LÄSER      WLINLB (WDD9)                     
002000*    SUB PROGRAMMET W411SAP  LÄSER      WLSAPC (WDH3)                     
002100*    SUB PROGRAMMET W611LEVP UPPDATERAR WLINLB (WDD9)                     
002200*    INDATA.                                                              
002300*        TRANSAKTION: W6T114                                              
002400*        REQU         W60144I1                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        RESP         W60114O1                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400*    -COPY WY2000W1                                                       
003500     SKIP3                                                                
003600*    -COPY WY2000W9                                                       
003700     SKIP3                                                                
003800 77  IDPGM                       PIC X(08)   VALUE 'W6011410'.            
003900                                                                          
004000*    -- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG               
004100 77  FILLER                      PIC X(8)  VALUE 'ERRORTEX'.              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  YES                         PIC X       VALUE 'Y'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800                                                                          
004900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  SPAR-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  MAX-INDX                    PIC S9(4)  VALUE +8    COMP SYNC.        
005300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005400                                                                          
005500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005600 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
005700 77  WS-IDFS                     PIC X(8)    VALUE SPACE.                 
005800 77  WS-TIAVIDAT                 PIC X(6)    VALUE SPACE.                 
005900 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
006000 77  WS-FLKLIVIS                 PIC X(1)    VALUE SPACE.                 
006100 77  WS-FLBORT                   PIC X(1)    VALUE SPACE.                 
006200 77  WS-TIPRLIST-NYCKEL          PIC S9(9)   VALUE ZERO.                  
006300 77  WS-TIPRLIST-DEL             PIC 9(9).                                
006400 77  WS-TIPRLIST                 PIC 9(6).                                
006500 01  WS-IDDC-LOCAL.                                                       
006600     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
006700     03  WS-IDDC-LOCAL-DATE      PIC X(2).                                
006800     03  FILLER                  PIC X(1)   VALUE SPACE.                  
006900                                                                          
007000*    --- WORK FIELD FOR MOVING SPACE TO ANY KIND OF FIELD                 
007100 01  W-SPACE.                                                             
007200     03 FILLER                   PIC X(50)   VALUE SPACE.                 
007300                                                                          
007400*    --- WORK FIELD FOR MOVING ALL + TO ANY KIND OF FIELD                 
007500 01  W-PLUS.                                                              
007600     03 FILLER                   PIC X(50)   VALUE                        
007700        '++++++++++++++++++++++++++++++++++++++++++++++++++'.             
007800                                                                          
007900 01  DAGENS-DATUM                PIC 9(6).                                
008000 01  FILLER REDEFINES DAGENS-DATUM.                                       
008100  05 DATUM-AA                    PIC 9(2).                                
008200  05 DATUM-MM                    PIC 9(2).                                
008300  05 DATUM-DD                    PIC 9(2).                                
008400                                                                          
008500 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008600                                                                          
008700 01  DAGENS-DATUM-LOCAL          PIC 9(6).                                
008800 01  FILLER REDEFINES DAGENS-DATUM-LOCAL.                                 
008900  05 DATUM-AA-LOCAL              PIC 9(2).                                
009000  05 DATUM-MM-LOCAL              PIC 9(2).                                
009100  05 DATUM-DD-LOCAL              PIC 9(2).                                
009200                                                                          
009300*     -- DAGENS-DATUM MED SEKEL-SIFFRA                                    
009400 01      WS-DAGENS-DATUM         PIC 9(8)    VALUE ZERO.                  
009500 01      FILLER REDEFINES WS-DAGENS-DATUM.                                
009600   03    WS-DAGENS-SEKEL         PIC 9(2).                                
009700   03    WS-IDAG                 PIC 9(6).                                
009800 EJECT                                                                    
009900*      --- VALID IDDC CODES                                               
010000*                                                                         
010100*01    -COPY WWDC99                                                       
010200       EJECT                                                              
010300 77  W-KVKOLLI                   PIC S9(7)  VALUE +0    COMP-3.           
010400 77  W-KVAENDRING                PIC S9(1)  VALUE +0    COMP-3.           
010500 77  W-IDLEVNR-UPD               PIC  X(5)  VALUE SPACE.                  
010600 77  W-TIANKDAG-UPD              PIC  9(6)  VALUE ZERO.                   
010700 77  W-TIAVIDAT-UPD              PIC  9(6)  VALUE ZERO.                   
010800 77  W-TIAVIDAT                  PIC  9(6)  VALUE ZERO.                   
010900 77  W-ANT-ART                   PIC S9(3)  VALUE +0    COMP-3.           
011000 77  W-MAX-ART                   PIC S9(3)  VALUE +40   COMP-3.           
011100 77  SPAR-IDRADNR                PIC  9(3)  VALUE ZERO.                   
011200 77  SPAR-IDRADNR-INL            PIC  9(5)  VALUE ZERO.                   
011300 77  SPAR-IDARTNR-X              PIC  X(8)  VALUE ZERO.                   
011400 77  SPAR-KVAVIS                 PIC S9(7)  VALUE ZERO COMP-3.            
011500 77  SPAR-KVINLART               PIC S9(7)  VALUE ZERO COMP-3.            
011600 77  W-MOD-IDARTNR-RAD           PIC Z(7)9  VALUE ZERO.                   
011700 77  W-MOD-KVAVIS-RAD            PIC Z(5)9  VALUE ZERO.                   
011800 77  W-MOD-KDRT-RAD              PIC Z(1)9  VALUE ZERO.                   
011900 77  W-MOD-KVKOLLI-RAD           PIC Z(2)9  VALUE ZERO.                   
012000 77  W-MOD-IDOKOLLI-RAD          PIC Z(8)9  VALUE ZERO.                   
012100 77  W-MOD-KVINLART-RAD          PIC Z(5)9  VALUE ZERO.                   
012200                                                                          
012300 77  W-SPAR-IDLEVNR              PIC  X(5)   VALUE SPACE.                 
012400 77  W-SPAR-IDFS                 PIC  X(8)   VALUE SPACE.                 
012500 77  W-SPAR-TIAVIDAT             PIC S9(7)   VALUE ZERO COMP-3.           
012600                                                                          
012700 77  W-SPAR-INL-FLFEL            PIC  X(1)   VALUE SPACE.                 
012800 77  W-SPAR-TIANKDAG             PIC S9(7)   VALUE ZERO COMP-3.           
012900 77  W-FLFEL                     PIC  X(1)   VALUE SPACE.                 
013000 77  W-IDFTG                     PIC  9(2)   VALUE ZERO.                  
013100 77  W-INL-IDFTG                 PIC  9(2)   VALUE ZERO.                  
013200 77  WS-KVAVROP                  PIC S9(7)   COMP-3 VALUE ZERO.           
013300                                                                          
013400 EJECT                                                                    
013500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013600     88  INDATA-OK                           VALUE 'J'.                   
013700     88  INDATA-FEL                          VALUE 'N'.                   
013800                                                                          
013900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
014000     88  NYCKLAR-OK                          VALUE 'J'.                   
014100     88  NYCKLAR-FEL                         VALUE 'N'.                   
014200                                                                          
014300 77  AENDRING-NYCKEL-SW          PIC X       VALUE 'N'.                   
014400     88  AENDRING-NYCKEL                     VALUE 'J'.                   
014500                                                                          
014600 77  AENDRING-HUVUD-SW           PIC X       VALUE 'N'.                   
014700     88  AENDRING-HUVUD                      VALUE 'J'.                   
014800                                                                          
014900 77  BORTTAG-HUVUD-SW            PIC X       VALUE 'N'.                   
015000     88  BORTTAG-HUVUD                       VALUE 'J'.                   
015100                                                                          
015200 77  AENDRING-RAD-SW             PIC X       VALUE 'N'.                   
015300     88  AENDRING-RAD                        VALUE 'J'.                   
015400                                                                          
015500 77  BORTTAG-RAD-SW              PIC X       VALUE 'N'.                   
015600     88  BORTTAG-RAD                         VALUE 'J'.                   
015700                                                                          
015800 77  GODK-RAD-SW                 PIC X       VALUE 'N'.                   
015900     88  GODK-RAD                            VALUE 'J'.                   
016000                                                                          
016100 77  TILLAEGG-RAD-SW             PIC X       VALUE 'N'.                   
016200     88  TILLAEGG-RAD                        VALUE 'J'.                   
016300                                                                          
016400 77  FOER-MANGA-ART-SW           PIC X       VALUE 'N'.                   
016500     88  FOER-MANGA-ART                      VALUE 'J'.                   
016600                                                                          
016700 77  KONTO-LEV-SW                PIC X       VALUE 'J'.                   
016800     88  KONTO-LEV-FEL                       VALUE 'N'.                   
016900                                                                          
017000 77  KDRT-SW                     PIC X       VALUE 'J'.                   
017100     88  KDRT-FEL                            VALUE 'N'.                   
017200                                                                          
017300 77  RAD-OK-SW                   PIC X       VALUE 'J'.                   
017400     88  RAD-OK                              VALUE 'J'.                   
017500                                                                          
017600 77  LEVPLAN-SW                  PIC X       VALUE 'J'.                   
017700     88  LEVPLAN-OK                          VALUE 'J'.                   
017800     88  LEVPLAN-FEL                         VALUE 'N'.                   
017900                                                                          
018000 77  KONTO-UPPG-SW               PIC X       VALUE 'N'.                   
018100     88  KONTO-UPPG-AENDRAT                  VALUE 'J'.                   
018200                                                                          
018300 77  PRIS-FINNS-SW               PIC X       VALUE 'J'.                   
018400     88  PRIS-FINNS                          VALUE 'J'.                   
018500     88  PRIS-SAKNAS                         VALUE 'N'.                   
018600                                                                          
018700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
018800     88  EGEN-MID                            VALUE '6114'.                
018900     88  GODK-MID                            VALUE '6111' '6112'          
019000                                                   '6113' '6114'          
019100                                                   '6115' '6116'          
019200                                                   '6118' '6119'.         
019300     88  HELP-MID                            VALUE '0551'.                
019400                                                                          
019500 01  W-PRL-DADAT                 PIC 9(8)    VALUE ZERO.                  
019600     EJECT                                                                
019700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019800 01  GENERELLA-SUBPROGRAM.                                                
019900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
020000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
020100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
020400     03  W611REG                 PIC X(8)    VALUE 'W611REG '.            
020500     03  W611LEVP                PIC X(8)    VALUE 'W611LEVP'.            
020600     03  W411SAP                 PIC X(8)    VALUE 'W411SAP '.            
020700     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
020800     EJECT                                                                
020900*    --- PARAMETERS TO ABEND                                              
021000                                                                          
021100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
021200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
021300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
021400                                                                          
021500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
021600*01 -COPY WMEDAREA                                                        
021700     SKIP3                                                                
021800*01  -COPY WL01TIDZ  -PRE TIDZ-                                           
021900     EJECT                                                                
022000*01 -COPY WDATAREA                                                        
022100     EJECT                                                                
022200 01  MESSAGE-CODES.                                                       
022300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
022400     03  ERR-UPDATE-FORBIDD      PIC X(3)    VALUE '007'.                 
022500     03  MUST-BE-NUMERIC         PIC X(3)    VALUE '024'.                 
022600     03  ERR-MISSING-IN-REG      PIC X(3)    VALUE '027'.                 
022700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
022800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
022900     03  ERR-PART-SUPERSEDED     PIC X(3)    VALUE '223'.                 
023000     03  ERR-PRICE-IS-MISSING    PIC X(3)    VALUE '260'.                 
023100     03  ERR-WRONG-QUANTITY      PIC X(3)    VALUE '330'.                 
023200     03  ERR-WRONG-CMD-CODE      PIC X(3)    VALUE '345'.                 
023300     03  ERR-MESSAGE-MISSING     PIC X(3)    VALUE '999'.                 
023400     03  ERR-MISSING-PART        PIC X(3)    VALUE '025'.                 
023500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
023600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
023700     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
023800     03  INF-AVROP-SAKNAS        PIC X(3)    VALUE '336'.                 
023900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
024000*                                                                         
024100     EJECT                                                                
024200 01  FILLER                      PIC X(16)   VALUE 'W611REG-AREA'.        
024300     SKIP3                                                                
024400*01  -COPY W611REG0                                                       
024500*01  FILLER -COPY W611REG4          -RED LAENK-W611REG0.                  
024600     EJECT                                                                
024700*01 -COPY W611LEVP                                                        
024800     EJECT                                                                
024900*01 -COPY W411SAP                                                         
025000     EJECT                                                                
025100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025200     SKIP3                                                                
025300*01  -COPY WMFSAREA                                                       
025400     EJECT                                                                
025500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025600*                                                                         
025700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
025800     SKIP3                                                                
025900 01  NYCKLAR-TILL-DLI.                                                    
026000     03  W-W6D101KY-X.                                                    
026100         05  W-D101KY-IDDC       PIC  X(2)   VALUE SPACE.                 
026200         05  W-D101KY-IDLEVNR    PIC  X(5)   VALUE SPACE.                 
026300         05  W-D101KY-IDFS       PIC  X(8)   VALUE SPACE.                 
026400         05  W-D101KY-TIAVIDAT   PIC S9(7)   VALUE ZERO COMP-3.           
026500     03  W-W6D1A1KY-MIN-X.                                                
026600         05  W-D1A1KY-IDDC-MIN    PIC X(2)   VALUE SPACE.                 
026700         05  W-D1A1KY-IDLEVNR-MIN PIC  X(5)  VALUE SPACE.                 
026800         05  W-D1A1KY-IDFS-MIN    PIC X(8)   VALUE SPACE.                 
026900         05  FILLER               PIC X(16)  VALUE LOW-VALUE.             
027000     03  W-W6D1A1KY-MAX-X.                                                
027100         05  W-D1A1KY-IDDC-MAX    PIC X(2)   VALUE SPACE.                 
027200         05  W-D1A1KY-IDLEVNR-MAX PIC  X(5)  VALUE SPACE.                 
027300         05  W-D1A1KY-IDFS-MAX    PIC X(8)   VALUE SPACE.                 
027400         05  FILLER               PIC X(16)  VALUE HIGH-VALUE.            
027500     03  W-IDDC-B6-X.                                                     
027600         05 W-IDDC-B6                  PIC X(2).                          
027700     03  W-IDLEVNR-X.                                                     
027800         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
027900     03  W-IDRADNR-INL-X.                                                 
028000         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
028100     03  W-IDRADNR-X.                                                     
028200         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
028300     03  W-KDSEGKEY-X.                                                    
028400         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
028500     03  W-IDARTNR-X.                                                     
028600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
028610     03  W-WDD901KY-X.                                                    
028620         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
028630         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
028700     03  W-IDLEVNR-21-X.                                                  
028800         05  W-IDLEVNR-21        PIC X(5)    VALUE LOW-VALUE.             
028900     03  W-IDDC-X.                                                        
029000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
029100     03  W-KDAVROP-X.                                                     
029200         05  W-KDAVROP           PIC S9(1)    VALUE ZERO COMP-3.          
029300**       --- SÄTTS FRÅN IDDC                                              
029400     SKIP2                                                                
029500*    --- STATUS-KOD FRÅN IMS                                              
029600 01  STATUS-WS                   PIC XX.                                  
029700     88  SEGMENT-FINNS                       VALUE '  '.                  
029800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
029900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
030100     SKIP2                                                                
030200 01  GODK-STATUSKODER.                                                    
030300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030400     SKIP3                                                                
030500 01  SSA1                        PIC X(96).                               
030600 01  SSA2                        PIC X(64).                               
030700 01  SSA3                        PIC X(64).                               
030800     EJECT                                                                
030900*    --- IMS FUNKTIONSKODER                                               
031000*01  -COPY W0003                                                          
031100     EJECT                                                                
031200*    ---  DLI INPUT-OUTPUT AREA                                           
031300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
031400     SKIP3                                                                
031410 01  FILLER                      PIC X(16)   VALUE 'W6D1A1 AREA'.         
031420 01   DLI-IO-W6D1A1.                                                      
031430*     03  -COPY W6D1A1                                                    
031440                                                                          
031450 01  FILLER                      PIC X(16)   VALUE 'W6D101 AREA'.         
031460 01   DLI-IO-W6D101.                                                      
031470*     03  -COPY W6D101                                                    
031480                                                                          
031490 01  FILLER                      PIC X(16)   VALUE 'W6D111 AREA'.         
031500 01   DLI-IO-W6D111.                                                      
031600*     03  -COPY W6D111                                                    
031700                                                                          
031800 01  FILLER                      PIC X(16)   VALUE 'W6D121 AREA'.         
031900 01   DLI-IO-W6D121.                                                      
032000*     03  -COPY W6D121                                                    
032100                                                                          
033000 01  DLI-IO-AREA2.                                                        
033100     03  IO-AREA2                PIC X(100)  VALUE SPACE.                 
033200     SKIP3                                                                
033300     03  WLLEVA01 REDEFINES IO-AREA2.                                     
033400*        05  -COPY WDF101                                                 
033500     EJECT                                                                
033600 01  DLI-IO-AREA3.                                                        
033700     03  IO-AREA3                PIC X(900)  VALUE SPACE.                 
033800     SKIP3                                                                
033900     03  WLARTC01 REDEFINES IO-AREA3.                                     
034000*        05  -COPY WDK601 -PRE ARTC01-                                    
034100     EJECT                                                                
034200     03  WLARTC11 REDEFINES IO-AREA3.                                     
034300*        05  -COPY WDK611   -PRE ARTC11-                                  
034400     EJECT                                                                
034500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ARTC21'.         
034600*                                                                         
034700 01  DLI-IO-AREA-ARTC21.                                                  
034800     03  -COPY WDK621 -PRE ARTC21-                                        
034900     SKIP3                                                                
035000     EJECT                                                                
035100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-ARTS11'.         
035200*                                                                         
035300 01  DLI-IO-AREA-ARTS11.                                                  
035400     03  -COPY WDK711 -PRE ARTS11-                                        
035500     SKIP3                                                                
035600     EJECT                                                                
035700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDD9'.           
035800     SKIP3                                                                
035900 01  DLI-IO-WDD9.                                                         
036000     03  WDD9.                                                            
036100*        05  -COPY WDD905                                                 
036200     EJECT                                                                
036300 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
036400 01   DLI-IO-AREA-B601.                                                   
036500*     03  -COPY WDB601                                                    
036600                                                                          
036700 LINKAGE SECTION.                                                         
036800*                                                                         
036900 01  REQU-AREA.                                                           
037000*    03 -COPY WZ01REQU                                                    
037100*    03 -COPY W60114I1                                                    
037200*                                                                         
037300*                                                                         
037400 01  RESP-AREA.                                                           
037500*    03 -COPY WZ01RESP                                                    
037600*    03 -COPY W60114O1                                                    
037700                                                                          
037800 01  MAX-KVRADER                 PIC S9(4)  COMP.                         
037900                                                                          
038000*01  -COPY W0009   -PRE MSG-                                              
038100     EJECT                                                                
038200*01  -COPY W0008  -PRE W6INLB-                                            
038300     05  FILLER                  PIC X.                                   
038400     EJECT                                                                
038500*01  -COPY W0008  -PRE INLA1-                                             
038600     05  FILLER                  PIC X.                                   
038700     EJECT                                                                
038800*01  -COPY W0008  -PRE INLA2-                                             
038900     05  FILLER                  PIC X.                                   
039000     EJECT                                                                
039100*01  -COPY W0008  -PRE LEVA-                                              
039200     05  FILLER                  PIC X.                                   
039300     EJECT                                                                
039400*01  -COPY W0008  -PRE ARTC-                                              
039500     05  FILLER                  PIC X.                                   
039600     EJECT                                                                
039700*01  -COPY W0008  -PRE WDD9-                                              
039800     05  FILLER                  PIC X.                                   
039900*01  -COPY W0008  -PRE ARTS-                                              
040000     05  FILLER                  PIC X.                                   
040100*01  -COPY W0008  -PRE WDB6-                                              
040200     05  FILLER                  PIC X.                                   
040300     EJECT                                                                
040400*   PCB'ER FÖR SUB PGM W611REG                                            
040500                                                                          
040600 01  REG-INLA1-PCB               PIC X.                                   
040700                                                                          
040800 01  REG-INLA2-PCB               PIC X.                                   
040900                                                                          
041000 01  REG-INLA3-PCB               PIC X.                                   
041100                                                                          
041200 01  REG-LEVA-PCB                PIC X.                                   
041300                                                                          
041400 01  REG-ARTC-PCB                PIC X.                                   
041500                                                                          
041600 01  REG-BENA-PCB                PIC X.                                   
041700                                                                          
041800 01  REG-WDD9-PCB                PIC X.                                   
041900                                                                          
042000 01  REG-WDK7-PCB                PIC X.                                   
042100                                                                          
042110 01  REG-WDB6-PCB                PIC X.                                   
042120                                                                          
042200*   PCB'ER FÖR SUB PGM W411SAP                                            
042300                                                                          
042400 01  SAP-SAPC-PCB                PIC X.                                   
042500                                                                          
042600*   PCB FÖR SUB PGM W611LEVP                                              
042700                                                                          
042800 01  LEVP-INLB-PCB               PIC X.                                   
042900     EJECT                                                                
043000 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
043100                           MSG-PCB          W6INLB-PCB                    
043200                           INLA1-PCB INLA2-PCB LEVA-PCB                   
043300                           ARTC-PCB WDD9-PCB ARTS-PCB                     
043400                           WDB6-PCB                                       
043500                           REG-INLA1-PCB REG-INLA2-PCB                    
043600                           REG-INLA3-PCB REG-LEVA-PCB                     
043700                                         REG-ARTC-PCB                     
043800                           REG-BENA-PCB  REG-WDD9-PCB                     
043900                           REG-WDK7-PCB  REG-WDB6-PCB                     
044000                           SAP-SAPC-PCB                                   
044100                           LEVP-INLB-PCB.                                 
044200 MAIN SECTION.                                                            
044300                                                                          
044400       PERFORM A-INIT                                                     
044500       PERFORM B-VALIDATE-KEYS                                            
044600       IF NYCKLAR-OK                                                      
044700         IF REQU-UPDATE                                                   
044800           MOVE REQU-KVRADER TO RESP-KVRADER                              
044900                                MAX-INDX                                  
045000           IF SEGMENT-FINNS                                               
045100             PERFORM IMS-GU-INLA1-INLA01                                  
045200             IF INL-KDINL  = '310'                                        
045300               MOVE NEJ        TO INDATA-SW                               
045400               MOVE ERR-UPDATE-FORBIDD TO RESP-IDMSG-ERROR                
045500             ELSE                                                         
045600               PERFORM G-CONTROL-INPUT-UPDATE                             
045700             END-IF                                                       
045800           ELSE                                                           
045900             MOVE NEJ                TO INDATA-SW                         
046000             MOVE ERR-MISSING-IN-REG TO RESP-IDMSG-ERROR                  
046100           END-IF                                                         
046200         ELSE                                                             
046300           IF REQU-FIRST                                                  
046400             PERFORM C-FOERSTA-SIDA                                       
046500           ELSE                                                           
046600             IF REQU-NEXT                                                 
046700               PERFORM D-NAESTA-SIDA                                      
046800             ELSE                                                         
046900               PERFORM E-SAMMA-SIDA                                       
047000             END-IF                                                       
047100           END-IF                                                         
047200         END-IF                                                           
047300         IF INDATA-OK                                                     
047400           PERFORM F-LAES-VISA-INFO                                       
047500         END-IF                                                           
047600       END-IF                                                             
047700     MOVE ZERO TO RETURN-CODE                                             
047800     GOBACK                                                               
047900     .                                                                    
048000     EJECT                                                                
048100 A-INIT SECTION.                                                          
048200     IF REQU-KVRADER NOT NUMERIC                                          
048300       MOVE ZERO      TO REQU-KVRADER                                     
048400     END-IF                                                               
048500                                                                          
048600     MOVE ALL '+'     TO RESP-OUTPUT                                      
048700     MOVE 001         TO RESP-IDMSGVER                                    
048800     MOVE MAX-KVRADER TO RESP-KVRADER                                     
048900     MOVE SPACE       TO RESP-IDMSG-ERROR                                 
049000                         RESP-IDMSG-INFO                                  
049100                         RESP-IDELMT-ERROR                                
049200                                                                          
049300     MOVE ZERO        TO W-KVAENDRING                                     
049400     MOVE NEJ         TO AENDRING-HUVUD-SW                                
049500                         BORTTAG-HUVUD-SW                                 
049600                         AENDRING-RAD-SW                                  
049700                         BORTTAG-RAD-SW                                   
049800                         GODK-RAD-SW                                      
049900                         TILLAEGG-RAD-SW                                  
050000                         KONTO-UPPG-SW                                    
050100     MOVE JA          TO KONTO-LEV-SW                                     
050200                         KDRT-SW                                          
050300                                                                          
050400     ACCEPT DAGENS-DATUM       FROM DATE                                  
050500     ACCEPT DAGENS-TID         FROM TIME                                  
050600     PERFORM AA-INIT-REG4-AREA                                            
050700     .                                                                    
050800     EJECT                                                                
050900*----------------------------------------------------------------*        
051000 AA-INIT-REG4-AREA  SECTION.                                              
051100                                                                          
051200     MOVE '6114'               TO REG4-IDTRANS                            
051300     MOVE MAX-KVRADER          TO REG4-KVRADER-MAX                        
051400     MOVE ZERO                 TO REG4-TIAVIDAT                           
051500                                  REG4-IDFTG                              
051600                                  REG4-IDKONTO                            
051800     MOVE JA                   TO REG4-IDFTG-OK                           
052000                                  REG4-IDANALYS-OK                        
052100     MOVE SPACE                TO REG4-IDFS                               
052101                                  REG4-IDKST                              
052102                                  REG4-IDKST-OK                           
052200                                  REG4-IDDC                               
052300                                  REG4-IDANALYS                           
052400                                  REG4-IDLEVNR                            
052500                                                                          
052600     MOVE +1                   TO INDX                                    
052700     PERFORM UNTIL INDX        >  MAX-INDX                                
052800         MOVE ZERO             TO REG4-IDARTNR-NY    (INDX)               
052900                                  REG4-IDARTNR-GAMMAL(INDX)               
053000                                  REG4-IDRADNR-INL-GAMMAL(INDX)           
053100                                  REG4-KVAVIS        (INDX)               
053200                                  REG4-KDRT          (INDX)               
053300                                  REG4-KDBEH         (INDX)               
053400         MOVE JA               TO REG4-IDARTNR-OK    (INDX)               
053500                                  REG4-KDRT-OK       (INDX)               
053600         MOVE SPACE            TO REG4-IDMFSFEL      (INDX)               
053700         ADD +1                TO INDX                                    
053800     END-PERFORM                                                          
053900     .                                                                    
054000     EJECT                                                                
054100 B-VALIDATE-KEYS SECTION.                                                 
054200     MOVE JA TO NYCKLAR-SW                                                
054300                                                                          
054400     PERFORM BA-VALIDATE-IDDC                                             
054500     PERFORM BB-VALIDATE-IDLEVNR                                          
054600     PERFORM BC-VALIDATE-IDFS                                             
054700     PERFORM BD-VALIDATE-TIAVIDAT                                         
054800     PERFORM BE-VALIDATE-FLKLIVIS                                         
054900                                                                          
055000     MOVE REQU-ADINLOMR-PRT-KEY TO WS-ADINLOMR-PRT                        
055100                                                                          
055200**   PERFORM BF-FLYTTA-OEVRIGA-NYCKLAR                                    
055300     IF NYCKLAR-OK                                                        
055400       PERFORM BG-CALL-WL01TIDZ                                           
055500     END-IF                                                               
055600                                                                          
055700     IF NYCKLAR-FEL                                                       
055800       IF RESP-IDMSG-ERROR = SPACE                                        
055900         MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                           
056000       END-IF                                                             
056100     END-IF                                                               
056200     .                                                                    
056300     EJECT                                                                
056400 BA-VALIDATE-IDDC SECTION.                                                
056500     MOVE SPACE           TO WS-IDDC                                      
056600     MOVE REQU-IDDC-KEY   TO WS-IDDC                                      
056700                             W-IDDC-B6                                    
056800*                                                                         
056900     PERFORM IMS-GU-WDB601                                                
057000     IF SEGMENT-SAKNAS                                                    
057100       MOVE SPACE TO DCS-KDDC                                             
057200       MOVE 'IDDC'        TO RESP-IDELMT-ERROR                            
057300       MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                             
057400       MOVE ZERO           TO RESP-KVRADER                                
057500       MOVE NEJ           TO NYCKLAR-SW                                   
057600     ELSE                                                                 
057700       IF CDC OR NDC                                                      
057800         MOVE WS-IDDC     TO REG4-IDDC                                    
057900                             W-D101KY-IDDC                                
058000                             W-D1A1KY-IDDC-MIN                            
058100                             W-D1A1KY-IDDC-MAX                            
058110                             W-IDDC-D9                                    
058200       ELSE                                                               
058300         MOVE 'IDDC'        TO RESP-IDELMT-ERROR                          
058400         MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                           
058500         MOVE ZERO           TO RESP-KVRADER                              
058600         MOVE  NEJ          TO NYCKLAR-SW                                 
058700         MOVE SPACE         TO DCS-IDDC                                   
058800       END-IF                                                             
058900     END-IF                                                               
059000     .                                                                    
059100     EJECT                                                                
059200 BB-VALIDATE-IDLEVNR SECTION.                                             
059300     IF INDATA-OK                                                         
059400       MOVE REQU-IDLEVNR-KEY     TO WS-IDLEVNR                            
059500       IF WS-IDLEVNR  NOT = SPACE                                         
059600         MOVE WS-IDLEVNR         TO W-D101KY-IDLEVNR                      
059700       ELSE                                                               
059800         MOVE 'IDLEVNR'          TO RESP-IDELMT-ERROR                     
059900         MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                      
060000         MOVE ZERO               TO RESP-KVRADER                          
060100         MOVE NEJ                TO NYCKLAR-SW                            
060200       END-IF                                                             
060300     END-IF                                                               
060400     .                                                                    
060500     EJECT                                                                
060600 BC-VALIDATE-IDFS SECTION.                                                
060700     IF INDATA-OK                                                         
060800       MOVE REQU-IDFS-KEY      TO WS-IDFS                                 
060900       IF WS-IDFS = SPACE OR ALL '+'                                      
061000         MOVE 'IDFS'           TO RESP-IDELMT-ERROR                       
061100         MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                        
061200         MOVE ZERO             TO RESP-KVRADER                            
061300         MOVE NEJ              TO NYCKLAR-SW                              
061400       ELSE                                                               
061500         MOVE WS-IDFS          TO W-D101KY-IDFS                           
061600       END-IF                                                             
061700     END-IF                                                               
061800     .                                                                    
061900     EJECT                                                                
062000 BD-VALIDATE-TIAVIDAT SECTION.                                            
062100     IF INDATA-OK                                                         
062200       MOVE REQU-TIAVIDAT-KEY    TO WS-TIAVIDAT                           
062300       IF WS-TIAVIDAT NUMERIC                                             
062400         IF WS-TIAVIDAT > ZERO                                            
062500           MOVE WS-TIAVIDAT      TO W-D101KY-TIAVIDAT                     
062600         ELSE                                                             
062700           IF NYCKLAR-OK                                                  
062800             MOVE WS-IDDC        TO W-D1A1KY-IDDC-MIN                     
062900                                    W-D1A1KY-IDDC-MAX                     
063000             MOVE WS-IDLEVNR     TO W-D1A1KY-IDLEVNR-MIN                  
063100                                    W-D1A1KY-IDLEVNR-MAX                  
063200             MOVE WS-IDFS        TO W-D1A1KY-IDFS-MIN                     
063300                                    W-D1A1KY-IDFS-MAX                     
063400             PERFORM IMS-GU-INLB-INLB01                                   
063500             IF SEGMENT-FINNS                                             
063600               MOVE SEQA-TIAVIDAT   TO W-TIAVIDAT                         
063700               MOVE W-TIAVIDAT      TO WS-TIAVIDAT                        
063800               PERFORM IMS-GN-INLB-INLB01                                 
063900               IF SEGMENT-FINNS                                           
064000                 MOVE +0            TO WS-TIAVIDAT                        
064100                 MOVE ERR-WRONG-KEY TO RESP-IDMSG-ERROR                   
064200                 MOVE NEJ           TO NYCKLAR-SW                         
064300                 MOVE ZERO          TO RESP-KVRADER                       
064400               END-IF                                                     
064500               MOVE WS-TIAVIDAT     TO W-D101KY-TIAVIDAT                  
064600             ELSE                                                         
064700               MOVE +0              TO WS-TIAVIDAT                        
064800               MOVE ERR-UPDATE-FORBIDD TO RESP-IDMSG-ERROR                
064900               MOVE NEJ             TO NYCKLAR-SW                         
065000             END-IF                                                       
065100           END-IF                                                         
065200         END-IF                                                           
065300       ELSE                                                               
065400          MOVE 'TIAVIDAT'      TO RESP-IDELMT-ERROR                       
065500          MOVE ERR-WRONG-KEY   TO RESP-IDMSG-ERROR                        
065600          MOVE ZERO            TO RESP-KVRADER                            
065700          MOVE NEJ             TO NYCKLAR-SW                              
065800       END-IF                                                             
065900     END-IF                                                               
066000     .                                                                    
066100     EJECT                                                                
066200 BE-VALIDATE-FLKLIVIS SECTION.                                            
066300     IF INDATA-OK                                                         
066400       MOVE REQU-FLKLIVIS-KEY TO WS-FLKLIVIS                              
066500       IF WS-FLKLIVIS =  '+'                                              
066600         MOVE SPACE           TO WS-FLKLIVIS                              
066700       END-IF                                                             
066800                                                                          
066900       IF WS-FLKLIVIS  = SPACE OR JA OR NEJ OR YES                        
067000         CONTINUE                                                         
067100       ELSE                                                               
067200         MOVE 'FLKLIVIS'       TO RESP-IDELMT-ERROR                       
067300         MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                        
067400         MOVE ZERO             TO RESP-KVRADER                            
067500         MOVE NEJ              TO NYCKLAR-SW                              
067600       END-IF                                                             
067700     END-IF                                                               
067800     .                                                                    
067900     EJECT                                                                
068000 C-FOERSTA-SIDA SECTION.                                                  
068100     MOVE INF-FIRST-PAGE       TO RESP-IDMSG-INFO                         
068200     MOVE ZERO                 TO W-IDRADNR                               
068300     .                                                                    
068400     EJECT                                                                
068500 D-NAESTA-SIDA SECTION.                                                   
068510     IF REQU-IDRADNR-INL-START NOT NUMERIC                                
068520        MOVE ZERO                   TO W-IDRADNR-INL                      
068530     ELSE                                                                 
068600        MOVE REQU-IDRADNR-INL-START TO W-IDRADNR-INL                      
068610     END-IF                                                               
068620                                                                          
068630     IF REQU-IDRADNR-START NOT NUMERIC                                    
068640        MOVE ZERO                   TO W-IDRADNR                          
068650     ELSE                                                                 
068700        MOVE REQU-IDRADNR-START     TO W-IDRADNR                          
068710     END-IF                                                               
068800     .                                                                    
068900     EJECT                                                                
069000 E-SAMMA-SIDA SECTION.                                                    
069100     IF REQU-IDRADNR-INL-START NOT NUMERIC                                
069101        MOVE ZERO                   TO W-IDRADNR-INL                      
069102     ELSE                                                                 
069104        MOVE REQU-IDRADNR-INL-START TO W-IDRADNR-INL                      
069105     END-IF                                                               
069106     IF REQU-IDRADNR-START NOT NUMERIC                                    
069107        MOVE ZERO               TO W-IDRADNR                              
069108     ELSE                                                                 
069109        MOVE REQU-IDRADNR-START TO W-IDRADNR                              
069110     END-IF                                                               
069300                                                                          
069400     IF REQU-INPUT = ALL '+'                                              
069600       CONTINUE                                                           
069700     ELSE                                                                 
069800*      MOVE NEJ TO INDATA-SW                                              
069900       MOVE INF-PRESS-PF11       TO RESP-IDMSG-INFO                       
070000       PERFORM MFS-LAES-IN-IGEN                                           
070100       PERFORM MFS-STAENG-FAELT-RADER-IN                                  
070200       PERFORM EA-REQU-INDATA-TILL-RESP                                   
070300     END-IF                                                               
070400     .                                                                    
070500     EJECT                                                                
070600 EA-REQU-INDATA-TILL-RESP SECTION.                                        
070700     IF REQU-TIAVIDAT-UPD = ALL '+'                                       
070800       MOVE W-SPACE            TO RESP-TIAVIDAT-UPD                       
070900     ELSE                                                                 
071000       MOVE REQU-TIAVIDAT-UPD  TO RESP-TIAVIDAT-UPD                       
071100     END-IF                                                               
071200                                                                          
071300     IF REQU-IDLBBET-UPD  = ALL '+'                                       
071400       MOVE W-SPACE            TO RESP-IDLBBET-UPD                        
071500     ELSE                                                                 
071600       MOVE REQU-IDLBBET-UPD   TO RESP-IDLBBET-UPD                        
071700     END-IF                                                               
071800                                                                          
071900     IF REQU-TIANKDAG-UPD = ALL '+'                                       
072000       MOVE W-SPACE              TO RESP-TIANKDAG-UPD                     
072100     ELSE                                                                 
072200         MOVE REQU-TIANKDAG-UPD  TO RESP-TIANKDAG-UPD                     
072300     END-IF                                                               
072400                                                                          
072500     IF REQU-IDLEVNR-UPD  = ALL '+'                                       
072600       MOVE W-SPACE            TO RESP-IDLEVNR-UPD                        
072700     ELSE                                                                 
072800       MOVE REQU-IDLEVNR-UPD   TO RESP-IDLEVNR-UPD                        
072900     END-IF                                                               
073000                                                                          
073100     IF REQU-FLBORT-UPD   = ALL '+'                                       
073200       MOVE W-SPACE            TO RESP-FLBORT-UPD                         
073300     ELSE                                                                 
073400       MOVE REQU-FLBORT-UPD    TO RESP-FLBORT-UPD                         
073500     END-IF                                                               
073600                                                                          
073700     IF REQU-IDFTG-UPD = ALL '+'                                          
073800       MOVE W-SPACE            TO RESP-IDFTG-UPD                          
073900     ELSE                                                                 
074000       MOVE REQU-IDFTG-UPD     TO RESP-IDFTG-UPD                          
074100     END-IF                                                               
074200                                                                          
074300     IF REQU-IDKONTO-UPD = ALL '+'                                        
074400       MOVE W-SPACE            TO RESP-IDKONTO-UPD                        
074500     ELSE                                                                 
074600       MOVE REQU-IDKONTO-UPD   TO RESP-IDKONTO-UPD                        
074700     END-IF                                                               
074800                                                                          
074900     IF REQU-IDANALYS-UPD = ALL '+'                                       
075000       MOVE W-SPACE            TO RESP-IDANALYS-UPD                       
075100     ELSE                                                                 
075200       MOVE REQU-IDANALYS-UPD  TO RESP-IDANALYS-UPD                       
075300     END-IF                                                               
075400                                                                          
075500     IF REQU-IDKST-UPD = ALL '+'                                          
075600       MOVE W-SPACE          TO RESP-IDKST-UPD                            
075700     ELSE                                                                 
075800       MOVE REQU-IDKST-UPD   TO RESP-IDKST-UPD                            
075900     END-IF                                                               
076000                                                                          
076100     IF REQU-IDARTNR-UPD = ALL '+'                                        
076200       MOVE W-SPACE            TO RESP-IDARTNR-UPD                        
076300     ELSE                                                                 
076400       MOVE REQU-IDARTNR-UPD   TO RESP-IDARTNR-UPD                        
076500     END-IF                                                               
076600                                                                          
076700     IF REQU-KVAVIS-UPD = ALL '+'                                         
076800       MOVE W-SPACE            TO RESP-KVAVIS-UPD                         
076900     ELSE                                                                 
077000       MOVE REQU-KVAVIS-UPD    TO RESP-KVAVIS-UPD                         
077100     END-IF                                                               
077200                                                                          
077300     IF REQU-KDRT-UPD = ALL '+'                                           
077400       MOVE W-SPACE            TO RESP-KDRT-UPD                           
077500     ELSE                                                                 
077600       MOVE REQU-KDRT-UPD      TO RESP-KDRT-UPD                           
077700     END-IF                                                               
077800                                                                          
077900     .                                                                    
078000     EJECT                                                                
078100 F-LAES-VISA-INFO SECTION.                                                
078200     PERFORM FA-LAES-FLYTTA-GRUNDDATA                                     
078300     IF SEGMENT-SAKNAS OR                                                 
078400        INL-TIINLMOT > ZERO OR                                            
078500        INL-IDARTNR  > ZERO                                               
078600       MOVE ERR-MISSING-IN-REG TO RESP-IDMSG-ERROR                        
078700       PERFORM MFS-RENSA-FAELT-UT                                         
078800       PERFORM MFS-STAENG-FAELT-UPD-IN                                    
078900     ELSE                                                                 
079000       MOVE +1                 TO INDX                                    
079100       MOVE +0                 TO RESP-KVRADER                            
079200       IF REQU-FIRST                                                      
079300         PERFORM IMS-GNP-INLA1-INLA11                                     
079400       ELSE                                                               
079500         PERFORM IMS-GNP-INLA1-INLA11-KVAL                                
079600       END-IF                                                             
079700                                                                          
079800       PERFORM UNTIL INDX >  MAX-KVRADER OR SEGMENT-SAKNAS                
079900         MOVE ZERO                 TO W-KVKOLLI                           
080000         IF SEGMENT-FINNS                                                 
080100                                                                          
080200           PERFORM FB-FLYTTA-RADDATA                                      
080300           IF INDX                 >  MAX-INDX AND                        
080400              WS-FLKLIVIS          =  JA       AND                        
080500              SPAR-IDRADNR         > ZERO                                 
080600**             SIDAN ÄR FULL OCH DET FINNS FLER KOLLI                     
080700**             SEGMENT PÅ AKTUELL ARTIKEL                                 
080800             CONTINUE                                                     
080900           ELSE                                                           
081000             PERFORM IMS-GNP-INLA1-INLA11                                 
081100             IF SEGMENT-FINNS                                             
081200               MOVE ART-IDRADNR-INL TO SPAR-IDRADNR-INL                   
081300             ELSE                                                         
081400               MOVE ZERO                    TO SPAR-IDRADNR-INL           
081500             END-IF                                                       
081600           END-IF                                                         
081700         END-IF                                                           
081800                                                                          
081900         IF WS-FLKLIVIS             = JA                                  
082000           CONTINUE                                                       
082100         ELSE                                                             
082200           ADD 1                  TO INDX                                 
082300           ADD +1                 TO RESP-KVRADER                         
082400         END-IF                                                           
082500                                                                          
082600         IF SEGMENT-SAKNAS                                                
082700           MOVE NEJ               TO WS-FLKLIVIS                          
082800         END-IF                                                           
082900       END-PERFORM                                                        
083000                                                                          
083100       IF SPAR-IDRADNR-INL         >  ZERO                                
083200         MOVE SPAR-IDRADNR-INL     TO RESP-IDRADNR-INL-NEXT               
083300         MOVE SPAR-IDRADNR         TO RESP-IDRADNR-NEXT                   
083400         IF REQU-UPDATE                                                   
083500           CONTINUE                                                       
083600         ELSE                                                             
083700           MOVE INF-MORE-INFO-EXISTS  TO RESP-IDMSG-INFO                  
083800         END-IF                                                           
083900       ELSE                                                               
084000         MOVE ZERO                 TO RESP-IDRADNR-NEXT                   
084100       END-IF                                                             
084200     END-IF                                                               
084300     .                                                                    
084400     EJECT                                                                
084500 FA-LAES-FLYTTA-GRUNDDATA SECTION.                                        
084600                                                                          
084700     PERFORM IMS-GU-INLA1-INLA01                                          
084800     IF SEGMENT-FINNS AND INL-TIINLMOT = ZERO                             
084900       MOVE INL-TIAVIDAT       TO RESP-TIAVIDAT                           
085000       MOVE INL-IDLBBET        TO RESP-IDLBBET                            
085100**                              --- ÄVEN TILL BILDGRUPP-NYCKEL            
085200                                  RESP-IDLBBET                            
085300       IF INL-TIANKDAG > ZERO                                             
085400         MOVE INL-TIANKDAG     TO RESP-TIANKDAG                           
085500       ELSE                                                               
085600         MOVE W-SPACE          TO RESP-TIANKDAG                           
085700       END-IF                                                             
085800       MOVE INL-IDLEVNR        TO RESP-IDLEVNR                            
085900       MOVE INL-IDFTG          TO RESP-IDFTG                              
086000       MOVE INL-IDKONTO        TO RESP-IDKONTO                            
086100       MOVE INL-IDANALYS       TO RESP-IDANALYS                           
086200       MOVE INL-IDKST          TO RESP-IDKST                              
086300       INSPECT RESP-IDFTG REPLACING LEADING ZERO BY SPACE                 
086400     END-IF                                                               
086500     .                                                                    
086600     EJECT                                                                
086700 FB-FLYTTA-RADDATA SECTION.                                               
086800                                                                          
086900     IF INDX                   = 1                                        
087000       MOVE ART-IDRADNR-INL    TO RESP-IDRADNR-INL-START                  
087100     END-IF                                                               
087200                                                                          
087300     IF ART-FLFEL              =  JA                                      
087400       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
087500                              RESP-IDARTNR-LINE-ATTR(INDX)                
087600       IF RESP-IDMSG-ERROR = SPACE                                        
087700         MOVE ART-IDRADNR-INL    TO W-IDRADNR-INL                         
087800         PERFORM S05-CONTROL-OM-RAD-OK                                    
087900                                                                          
088000         IF RAD-OK                                                        
088100           IF ARTC11-CLAG-KDERS > 20                                      
088200             MOVE ERR-PART-SUPERSEDED    TO RESP-IDMSG-ERROR              
088300           ELSE                                                           
088400             MOVE WS-IDLEVNR      TO W-IDLEVNR                            
088500             IF W-IDLEVNR = '     ' OR '0    ' OR '9998 ' OR              
088600                            '9999 ' OR '8888 ' OR                         
088700                            'BW5JA' OR '16466' OR '6492 ' OR              
088800                            '1003 ' OR '1304 ' OR                         
088900                            'BWLAA' OR 'BZFFA' OR '12054' OR              
089000                            'BP2TH' OR 'BKMJA' OR 'S5S2A'                 
089100*              MOVE ERR-MESSAGE-MISSING TO MED-IDMFSFEL                   
089200               MOVE ERR-MESSAGE-MISSING TO RESP-IDMSG-ERROR               
089300             ELSE                                                         
089400               MOVE +2              TO W-KDAVROP                          
089500               MOVE ZERO            TO WS-KVAVROP                         
089510               MOVE ART-IDARTNR     TO W-IDARTNR-D9                       
089520               MOVE ART-IDDC        TO W-IDDC-D9                          
089600               PERFORM IMS-GET-WDD902                                     
089700               IF SEGMENT-FINNS                                           
089800                 PERFORM IMS-GNP-WDD905                                   
089900                 IF SEGMENT-FINNS                                         
090000                   IF KVAVROP < ART-KVAVIS                                
090100                     PERFORM UNTIL SEGMENT-SAKNAS OR                      
090200                     (WS-KVAVROP NOT < ART-KVAVIS)                        
090300                        ADD KVAVROP TO WS-KVAVROP                         
090400                        PERFORM IMS-GNP-WDD905                            
090500                     END-PERFORM                                          
090600                   ELSE                                                   
090700                     MOVE ART-KVAVIS TO WS-KVAVROP                        
090800                   END-IF                                                 
090900                 END-IF                                                   
091000               END-IF                                                     
091100                                                                          
091200               IF ART-KVAVIS > WS-KVAVROP                                 
091300                 MOVE INF-AVROP-SAKNAS      TO RESP-IDMSG-INFO            
091400               ELSE                                                       
091500*                MOVE ERR-MESSAGE-MISSING TO MED-IDMFSFEL                 
091600                 MOVE ERR-MESSAGE-MISSING TO RESP-IDMSG-ERROR             
091700               END-IF                                                     
091800             END-IF                                                       
091900           END-IF                                                         
092000         END-IF                                                           
092100                                                                          
092200         PERFORM IMS-GNP-INLA1-INLA11-KVAL-FST                            
092300       END-IF                                                             
092400     END-IF                                                               
092500     MOVE ART-IDARTNR          TO W-MOD-IDARTNR-RAD                       
092600                                  RESP-IDARTNR-SPAR-LINE(INDX)            
092700     MOVE ART-IDRADNR-INL      TO RESP-IDRADNR-INL-SPAR-LINE(INDX)        
092800     MOVE W-MOD-IDARTNR-RAD    TO RESP-IDARTNR-LINE (INDX)                
092900     MOVE ART-KVAVIS           TO W-MOD-KVAVIS-RAD                        
093000     MOVE W-MOD-KVAVIS-RAD     TO RESP-KVAVIS-LINE (INDX)                 
093100     MOVE ART-KDRT             TO W-MOD-KDRT-RAD                          
093200     MOVE W-MOD-KDRT-RAD       TO RESP-KDRT-LINE  (INDX)                  
093300     MOVE INDX                 TO SPAR-INDX                               
093400     PERFORM FBA-BEHANDLA-KOLLI                                           
093500     MOVE W-KVKOLLI            TO W-MOD-KVKOLLI-RAD                       
093600     MOVE W-MOD-KVKOLLI-RAD    TO RESP-KVKOLLI-LINE (SPAR-INDX)           
093700     .                                                                    
093800     EJECT                                                                
093900 FBA-BEHANDLA-KOLLI      SECTION.                                         
094000                                                                          
094100     MOVE ART-IDARTNR            TO W-IDARTNR                             
094200     MOVE ART-IDRADNR-INL        TO W-IDRADNR-INL                         
094300     IF (REQU-FIRST OR REQU-NEXT) AND                                     
094400        W-IDRADNR                >  ZERO AND                              
094500        INDX                     =  1                                     
094600       PERFORM IMS-GNP-INLA1-INLA21                                       
094700       PERFORM UNTIL W-IDRADNR = RAD-IDRADNR    OR                        
094800                     SEGMENT-SAKNAS                                       
094900         ADD +1                  TO W-KVKOLLI                             
095000         PERFORM IMS-GNP-INLA1-INLA21                                     
095100       END-PERFORM                                                        
095200     ELSE                                                                 
095300       PERFORM IMS-GNP-INLA1-INLA21                                       
095400     END-IF                                                               
095500                                                                          
095600     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
095700                  (WS-FLKLIVIS = JA AND INDX > MAX-INDX)                  
095800       ADD +1                  TO W-KVKOLLI                               
095900       IF INDX                 = 1                                        
096000           MOVE RAD-IDRADNR    TO RESP-IDRADNR-START                      
096100       END-IF                                                             
096200       IF WS-FLKLIVIS                  =  JA                              
096300         IF RESP-IDARTNR-LINE(INDX) = MFS-RENSA-FAELT OR                  
096400                                      LOW-VALUE                           
096500             MOVE W-SPACE         TO                                      
096600                                RESP-KDCMD-LINE          (INDX)           
096700                                RESP-IDARTNR-LINE        (INDX)           
096800                                RESP-KVAVIS-LINE         (INDX)           
096900                                RESP-KDRT-LINE           (INDX)           
097000             MOVE MFS-STAENG-FAELT TO                                     
097100                                RESP-KDCMD-LINE-ATTR    (INDX)            
097200                                RESP-IDARTNR-LINE-ATTR  (INDX)            
097300                                RESP-KVAVIS-LINE-ATTR   (INDX)            
097400                                RESP-KDRT-LINE-ATTR     (INDX)            
097500             MOVE ZERO         TO RESP-IDRADNR-INL-SPAR-LINE(INDX)        
097600                                   RESP-IDARTNR-SPAR-LINE(INDX)           
097700         END-IF                                                           
097800         MOVE RAD-IDRADNR        TO RESP-IDRADNR-SPAR-LINE(INDX)          
097900         MOVE RAD-IDOKOLLI           TO W-MOD-IDOKOLLI-RAD                
098000         MOVE W-MOD-IDOKOLLI-RAD TO RESP-IDOKOLLI-LINE(INDX)              
098100         MOVE RAD-KVINLART           TO W-MOD-KVINLART-RAD                
098200         MOVE W-MOD-KVINLART-RAD TO RESP-KVINLART-LINE(INDX)              
098300         ADD +1                      TO INDX                              
098310                                        RESP-KVRADER                      
098400       END-IF                                                             
098500       PERFORM IMS-GNP-INLA1-INLA21                                       
098600     END-PERFORM                                                          
098700                                                                          
098800     IF SEGMENT-FINNS                                                     
098900         MOVE RAD-IDRADNR      TO SPAR-IDRADNR                            
099000         MOVE W-IDRADNR-INL    TO SPAR-IDRADNR-INL                        
099100         PERFORM UNTIL             SEGMENT-SAKNAS                         
099200             ADD +1                TO W-KVKOLLI                           
099300             PERFORM IMS-GNP-INLA1-INLA21                                 
099400         END-PERFORM                                                      
099500     END-IF                                                               
099600     .                                                                    
099700     EJECT                                                                
099800 G-CONTROL-INPUT-UPDATE SECTION.                                          
099900                                                                          
100000     MOVE JA                   TO INDATA-SW                               
100100     MOVE NEJ                  TO AENDRING-HUVUD-SW                       
100200                                  AENDRING-RAD-SW                         
100300                                  BORTTAG-RAD-SW                          
100400                                  TILLAEGG-RAD-SW                         
100500                                                                          
100600     PERFORM GA-CONTROL-VAD-SOM-SKA-UPPDAT                                
100700                                                                          
100800     IF W-KVAENDRING  = ZERO                                              
100900       MOVE ERR-PF11-AND-NO-DATA  TO RESP-IDMSG-ERROR                     
101000       MOVE NEJ                   TO INDATA-SW                            
101100       PERFORM MFS-ROER-EJ-FAELT-IN                                       
101200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
101300       PERFORM MFS-STAENG-FAELT-RADER-IN                                  
101400     ELSE                                                                 
101500       MOVE SPACE                 TO RESP-IDMSG-ERROR                     
101600       IF W-KVAENDRING > 1                                                
101700         MOVE ERR-UPDATE-FORBIDD  TO RESP-IDMSG-ERROR                     
101800         MOVE NEJ                 TO INDATA-SW                            
101900       END-IF                                                             
102000**** KOLLAR ATT EJ FELAKTIG ÅTGÄRDSKOD ÄR ANGIVEN                         
102100                                                                          
102200       MOVE +1                     TO INDX                                
102300       PERFORM UNTIL INDX          >  MAX-INDX                            
102400         IF REQU-KDCMD-LINE(INDX) = 'G' OR 'A' OR 'B' OR 'D' OR           
102500                                   '+' OR ' '                             
102600            CONTINUE                                                      
102700         ELSE                                                             
102800           MOVE REQU-KDCMD-LINE(INDX) TO RESP-KDCMD-LINE(INDX)            
102900           MOVE ERR-WRONG-CMD-CODE TO RESP-IDMSG-ERROR                    
103000           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMD-LINE-ATTR(INDX)          
103100           MOVE NEJ                TO INDATA-SW                           
103200         END-IF                                                           
103300         ADD +1  TO INDX                                                  
103400       END-PERFORM                                                        
103500                                                                          
103600       IF INDATA-OK                                                       
103700         PERFORM GB-CONTROL-INDATA-EV-UPDATE                              
103800       END-IF                                                             
103900       IF INDATA-FEL                                                      
104000         IF SAP-BEFEL = SPACE                                             
104100           IF RESP-IDMSG-ERROR               =  SPACE                     
104200             MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                
104300           END-IF                                                         
104400         END-IF                                                           
104500         PERFORM MFS-STAENG-FAELT-RADER-IN                                
104600       ELSE                                                               
104700         MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                          
104800         PERFORM MFS-FORM-ATTR                                            
104900         PERFORM MFS-RENSA-FAELT-IN                                       
105000       END-IF                                                             
105100     END-IF                                                               
105200     .                                                                    
105300     EJECT                                                                
105400 GA-CONTROL-VAD-SOM-SKA-UPPDAT      SECTION.                              
105500     PERFORM GAA-CONTROL-OM-AENDRING-HUVUD                                
105600     PERFORM GAB-CONTROL-OM-BORTTAG-HUVUD                                 
105700     PERFORM GAC-CONTROL-OM-AENDRING-RAD                                  
105800     PERFORM GAD-CONTROL-BORTTAG-GODK-RAD                                 
105900     PERFORM GAE-CONTROL-OM-TILLAEGG-RAD                                  
106000     .                                                                    
106100     EJECT                                                                
106200 GAA-CONTROL-OM-AENDRING-HUVUD SECTION.                                   
106300     IF REQU-TIAVIDAT-UPD      NOT = ALL '+'                              
106400         MOVE JA               TO AENDRING-HUVUD-SW                       
106500                                  AENDRING-NYCKEL-SW                      
106600     END-IF                                                               
106700                                                                          
106800     IF REQU-IDLBBET-UPD       NOT = ALL '+'                              
106900         MOVE JA               TO AENDRING-HUVUD-SW                       
107000     END-IF                                                               
107100                                                                          
107200     IF REQU-TIANKDAG-UPD      NOT = ALL '+'                              
107300         MOVE JA               TO AENDRING-HUVUD-SW                       
107400     END-IF                                                               
107500                                                                          
107600     IF REQU-IDLEVNR-UPD       NOT = ALL '+'                              
107700         MOVE JA               TO AENDRING-HUVUD-SW                       
107800                                  AENDRING-NYCKEL-SW                      
107900     END-IF                                                               
108000                                                                          
108100     IF REQU-IDFTG-UPD         NOT = ALL '+'                              
108200         MOVE JA               TO AENDRING-HUVUD-SW                       
108300     END-IF                                                               
108400                                                                          
108500     IF NDC-NA                                                            
108600       IF REQU-IDKONTO-UPD NOT = ALL '+'                                  
108700         MOVE NEJ               TO INDATA-SW                              
108800         MOVE 'IDKONTO'         TO RESP-IDELMT-ERROR                      
108900         MOVE MFS-NUM-FAELT-FEL TO RESP-IDKONTO-UPD-ATTR                  
109000         MOVE REQU-IDKONTO-UPD  TO RESP-IDKONTO-UPD                       
109100       END-IF                                                             
109200       IF REQU-IDANALYS-UPD NOT = ALL '+'                                 
109300         MOVE NEJ               TO INDATA-SW                              
109400         MOVE 'IDANALYS'        TO RESP-IDELMT-ERROR                      
109500         MOVE MFS-NUM-FAELT-FEL TO RESP-IDANALYS-UPD-ATTR                 
109600         MOVE REQU-IDANALYS-UPD TO RESP-IDANALYS-UPD                      
109700       END-IF                                                             
109800       IF REQU-IDKST-UPD   NOT = ALL '+'                                  
109900         MOVE NEJ               TO INDATA-SW                              
110000         MOVE 'IDKST'           TO RESP-IDELMT-ERROR                      
110100         MOVE MFS-NUM-FAELT-FEL TO RESP-IDKST-UPD-ATTR                    
110200         MOVE REQU-IDKST-UPD    TO RESP-IDKST-UPD                         
110300       END-IF                                                             
110400     ELSE                                                                 
110500       IF REQU-IDKONTO-UPD  NOT = ALL '+' OR                              
110600          REQU-IDANALYS-UPD NOT = ALL '+' OR                              
110700          REQU-IDKST-UPD    NOT = ALL '+'                                 
110800         MOVE JA               TO AENDRING-HUVUD-SW                       
110900       END-IF                                                             
111000     END-IF                                                               
111100                                                                          
111200     IF AENDRING-HUVUD                                                    
111300       ADD +1                 TO W-KVAENDRING                             
111400     END-IF                                                               
111500     .                                                                    
111600     EJECT                                                                
111700 GAB-CONTROL-OM-BORTTAG-HUVUD SECTION.                                    
111800     IF REQU-FLBORT-UPD NOT = ALL '+'                                     
111900       MOVE JA                 TO BORTTAG-HUVUD-SW                        
112000       ADD +1                  TO W-KVAENDRING                            
112100     END-IF                                                               
112200     .                                                                    
112300     EJECT                                                                
112400 GAC-CONTROL-OM-AENDRING-RAD  SECTION.                                    
112500                                                                          
112600     MOVE +1                      TO INDX                                 
112700     PERFORM UNTIL INDX > MAX-INDX OR AENDRING-RAD                        
112800       IF REQU-IDARTNR-LINE(INDX) NOT = ALL '+'                           
112900         MOVE JA                  TO AENDRING-RAD-SW                      
113000                                     AENDRING-NYCKEL-SW                   
113100       END-IF                                                             
113200                                                                          
113300       IF REQU-KVAVIS-LINE(INDX)  NOT = ALL '+'                           
113400         MOVE JA                  TO AENDRING-RAD-SW                      
113500       END-IF                                                             
113600                                                                          
113700       IF REQU-KDRT-LINE(INDX)    NOT = ALL '+'                           
113800         MOVE JA                  TO AENDRING-RAD-SW                      
113900       END-IF                                                             
114000                                                                          
114100       IF AENDRING-RAD                                                    
114200         ADD +1                   TO W-KVAENDRING                         
114300       END-IF                                                             
114400                                                                          
114500       ADD +1                     TO INDX                                 
114600     END-PERFORM                                                          
114700     .                                                                    
114800     EJECT                                                                
114900 GAD-CONTROL-BORTTAG-GODK-RAD   SECTION.                                  
115000                                                                          
115100     MOVE +1                      TO INDX                                 
115200     PERFORM UNTIL INDX           >  MAX-INDX                             
115300       IF REQU-KDCMD-LINE(INDX)   NOT = ALL '+' AND NOT = SPACE           
115400         IF REQU-KDCMD-LINE(INDX) = 'G' OR 'A'                            
115500*          ---                     G=GODKÄNN  A=ACCEPT                    
115600           MOVE JA                TO GODK-RAD-SW                          
115700         ELSE                                                             
115800           IF REQU-KDCMD-LINE(INDX) = 'B' OR 'D'                          
115900*            ---                   B=BORTTAG  D=DELETE                    
116000             MOVE JA              TO BORTTAG-RAD-SW                       
116100           END-IF                                                         
116200         END-IF                                                           
116300       END-IF                                                             
116400                                                                          
116500       ADD +1                     TO INDX                                 
116600     END-PERFORM                                                          
116700                                                                          
116800     IF BORTTAG-RAD                                                       
116900       ADD +1                 TO W-KVAENDRING                             
117000     END-IF                                                               
117100                                                                          
117200     IF GODK-RAD                                                          
117300       ADD +1                 TO W-KVAENDRING                             
117400     END-IF                                                               
117500     .                                                                    
117600     EJECT                                                                
117700 GAE-CONTROL-OM-TILLAEGG-RAD  SECTION.                                    
117800     IF REQU-IDARTNR-UPD NOT = ALL '+'                                    
117900       MOVE JA           TO TILLAEGG-RAD-SW                               
118000     END-IF                                                               
118100                                                                          
118200     IF REQU-KVAVIS-UPD NOT = ALL '+'                                     
118300       MOVE JA           TO TILLAEGG-RAD-SW                               
118400     END-IF                                                               
118500                                                                          
118600     IF REQU-KDRT-UPD NOT = ALL '+'                                       
118700       MOVE JA           TO TILLAEGG-RAD-SW                               
118800     END-IF                                                               
118900                                                                          
119000     IF TILLAEGG-RAD                                                      
119100       ADD +1            TO W-KVAENDRING                                  
119200     END-IF                                                               
119300     .                                                                    
119400     EJECT                                                                
119500 GB-CONTROL-INDATA-EV-UPDATE       SECTION.                               
119600     IF AENDRING-HUVUD                                                    
119700       PERFORM GBA-AENDRING-HUVUD                                         
119800     END-IF                                                               
119900                                                                          
120000     IF BORTTAG-HUVUD                                                     
120100       PERFORM GBB-BORTTAG-HUVUD                                          
120200     END-IF                                                               
120300                                                                          
120400     IF AENDRING-RAD                                                      
120500       PERFORM GBC-AENDRING-RAD                                           
120600     END-IF                                                               
120700                                                                          
120800     IF BORTTAG-RAD                                                       
120900       PERFORM GBD-BORTTAG-RAD                                            
121000     END-IF                                                               
121100                                                                          
121200     IF TILLAEGG-RAD                                                      
121300       PERFORM GBE-TILLAEGG-RAD                                           
121400     END-IF                                                               
121500                                                                          
121600     IF GODK-RAD                                                          
121700       PERFORM GBF-GODK-RAD                                               
121800     END-IF                                                               
121900     .                                                                    
122000     EJECT                                                                
122100 GBA-AENDRING-HUVUD    SECTION.                                           
122200                                                                          
122300     PERFORM GBAA-CONTROL-AENDRING-HUVUD                                  
122400     IF INDATA-OK                                                         
122500       PERFORM GBAB-UPDATE-AENDRING-HUVUD                                 
122600       MOVE REQU-IDRADNR-INL-SPAR-LINE (1) TO W-IDRADNR-INL               
122700       MOVE REQU-IDARTNR-SPAR-LINE (1)     TO W-IDARTNR                   
122800     END-IF                                                               
122900     .                                                                    
123000     EJECT                                                                
123100 GBAA-CONTROL-AENDRING-HUVUD  SECTION.                                    
123200                                                                          
123300     PERFORM IMS-GU-INLA1-INLA01                                          
123400     IF SEGMENT-FINNS                                                     
123500       IF REQU-TIAVIDAT-UPD    NOT = ALL '+'                              
123600         PERFORM GBAAA-CONTROL-TIAVIDAT                                   
123700       END-IF                                                             
123800                                                                          
123900       IF REQU-IDLBBET-UPD     NOT = ALL '+'                              
124000         MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDLBBET-UPD-ATTR               
124100       END-IF                                                             
124200                                                                          
124300       IF REQU-TIANKDAG-UPD    NOT = ALL '+'                              
124400         PERFORM GBAAB-CONTROL-TIANKDAG                                   
124500       END-IF                                                             
124600                                                                          
124700       IF REQU-IDLEVNR-UPD     = ALL '+'                                  
124800         MOVE INL-IDLEVNR      TO W-IDLEVNR                               
124900       ELSE                                                               
125000                                                                          
125100         PERFORM GBAAC-CONTROL-IDLEVNR                                    
125200       END-IF                                                             
125300                                                                          
125400       IF REQU-IDFTG-UPD       NOT = ALL '+'                              
125500         PERFORM GBAAD-CONTROL-IDFTG                                      
125600       END-IF                                                             
125700                                                                          
125800       IF NDC-NA                                                          
125900         CONTINUE                                                         
126000       ELSE                                                               
126100         PERFORM GBAAH-CONTROL-KONTO-IDLEVNR                              
126200         PERFORM GBAAJ-CONTROL-MOT-SAP                                    
126300         MOVE JA                      TO KONTO-UPPG-SW                    
126400         IF SAP-BEFEL NOT = SPACE                                         
126500           MOVE NEJ                   TO INDATA-SW                        
126600           MOVE SAP-BEFEL             TO RESP-IDMSG-ERROR                 
126700           IF SAP-IDKONTO-OK = NEJ                                        
126800             MOVE MFS-NUM-FAELT-FEL TO RESP-IDKONTO-UPD-ATTR              
126900           ELSE                                                           
127000             IF SAP-IDANALYS-OK = NEJ                                     
127100               MOVE MFS-NUM-FAELT-FEL TO RESP-IDANALYS-UPD-ATTR           
127200             ELSE                                                         
127300               IF SAP-IDKST-OK = NEJ                                      
127400                 MOVE MFS-NUM-FAELT-FEL TO RESP-IDKST-UPD-ATTR            
127500               END-IF                                                     
127600             END-IF                                                       
127700           END-IF                                                         
127800         END-IF                                                           
127900       END-IF                                                             
128000     ELSE                                                                 
128100       MOVE NEJ                  TO INDATA-SW                             
128200       MOVE ERR-UPDATE-FORBIDD TO RESP-IDMSG-ERROR                        
128300     END-IF                                                               
128400     .                                                                    
128500     EJECT                                                                
128600 GBAAA-CONTROL-TIAVIDAT        SECTION.                                   
128700                                                                          
128800     MOVE REQU-TIAVIDAT-UPD    TO DAT-I-TIDATUM                           
128900     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
129000                                                                          
129100     CALL WDATKONV USING DAT-KDDATFORM,                                   
129200                         DAT-I-TIDATUM,                                   
129300                         DAT-O-TIDATUM,                                   
129400                         DAT-KDSVAR                                       
129500     IF DAT-KDSVAR-OK                                                     
129600       MOVE DAT-TIAAMMDD            TO TMP1-YYMMDD                        
129700       MOVE DAGENS-DATUM-LOCAL      TO TMP2-YYMMDD                        
129800       PERFORM WY2000P1                                                   
129900                                                                          
130000       MOVE DAT-TIAA          TO TMP1-YY                                  
130100       MOVE DATUM-AA          TO TMP2-YY                                  
130200       PERFORM WY2000P9                                                   
130300                                                                          
130400       IF (TMP1-YYMMDD > TMP2-YYMMDD) OR                                  
130500          (TMP1-YY       < TMP2-YY - 1)                                   
130600          MOVE MFS-NUM-FAELT-FEL TO RESP-TIAVIDAT-UPD-ATTR                
130700          MOVE NEJ               TO INDATA-SW                             
130800        ELSE                                                              
130900                                                                          
131000         IF REQU-TIANKDAG-UPD        NOT = ALL '+'                        
131100           MOVE REQU-TIANKDAG-UPD         TO TMP1-YYMMDD                  
131200           MOVE REQU-TIAVIDAT-UPD         TO TMP2-YYMMDD                  
131300           PERFORM WY2000P1                                               
131400                                                                          
131500           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
131600             MOVE MFS-NUM-FAELT-FEL TO RESP-TIAVIDAT-UPD-ATTR             
131700                                       RESP-TIANKDAG-UPD-ATTR             
131800                                                                          
131900             MOVE NEJ            TO INDATA-SW                             
132000             MOVE 'TIANKDAG-UPD' TO RESP-IDELMT-ERROR                     
132100                                                                          
132200           ELSE                                                           
132300             MOVE REQU-TIAVIDAT-UPD   TO W-TIAVIDAT-UPD                   
132400             MOVE MFS-NUM-FAELT-RAETT TO RESP-TIAVIDAT-UPD-ATTR           
132500                                         RESP-TIANKDAG-UPD-ATTR           
132600                                                                          
132700           END-IF                                                         
132800         ELSE                                                             
132900           MOVE REQU-TIAVIDAT-UPD TO W-TIAVIDAT-UPD                       
133000           MOVE INL-TIANKDAG      TO TMP1-YYMMDD                          
133100           MOVE W-TIAVIDAT-UPD    TO TMP2-YYMMDD                          
133200           PERFORM WY2000P1                                               
133300                                                                          
133400           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
133500             MOVE MFS-NUM-FAELT-FEL TO RESP-TIAVIDAT-UPD-ATTR             
133600                                       RESP-TIANKDAG-UPD-ATTR             
133700             MOVE NEJ               TO INDATA-SW                          
133800           ELSE                                                           
133900             MOVE MFS-NUM-FAELT-RAETT TO RESP-TIAVIDAT-UPD-ATTR           
134000                                         RESP-TIANKDAG-UPD-ATTR           
134100           END-IF                                                         
134200                                                                          
134300           IF INL-TIAVIDAT = W-TIAVIDAT-UPD                               
134400             MOVE MFS-NUM-FAELT-FEL TO RESP-TIAVIDAT-UPD-ATTR             
134500             MOVE NEJ            TO INDATA-SW                             
134600             MOVE 'TIANKDAG-INL' TO RESP-IDELMT-ERROR                     
134700           END-IF                                                         
134800         END-IF                                                           
134900       END-IF                                                             
135000     ELSE                                                                 
135100       MOVE MFS-NUM-FAELT-FEL TO RESP-TIAVIDAT-UPD-ATTR                   
135200       MOVE NEJ                 TO INDATA-SW                              
135300     END-IF                                                               
135400     .                                                                    
135500     EJECT                                                                
135600 GBAAB-CONTROL-TIANKDAG        SECTION.                                   
135700                                                                          
135800     MOVE REQU-TIANKDAG-UPD TO DAT-I-TIDATUM                              
135900     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
136000                                                                          
136100     CALL WDATKONV USING DAT-KDDATFORM,                                   
136200                         DAT-I-TIDATUM,                                   
136300                         DAT-O-TIDATUM,                                   
136400                         DAT-KDSVAR                                       
136500     IF DAT-KDSVAR-OK                                                     
136600       IF REQU-TIAVIDAT-UPD       NOT = ALL '+'                           
136700         MOVE REQU-TIANKDAG-UPD       TO TMP1-YYMMDD                      
136800         MOVE REQU-TIAVIDAT-UPD       TO TMP2-YYMMDD                      
136900         PERFORM WY2000P1                                                 
137000         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
137100           MOVE MFS-NUM-FAELT-FEL   TO RESP-TIANKDAG-UPD-ATTR             
137200           MOVE NEJ                 TO INDATA-SW                          
137300         ELSE                                                             
137400           MOVE MFS-NUM-FAELT-RAETT TO RESP-TIANKDAG-UPD-ATTR             
137500         END-IF                                                           
137600       ELSE                                                               
137700         MOVE REQU-TIANKDAG-UPD     TO W-TIANKDAG-UPD                     
137800         MOVE W-TIANKDAG-UPD        TO TMP1-YYMMDD                        
137900         MOVE INL-TIAVIDAT          TO TMP2-YYMMDD                        
138000         PERFORM WY2000P1                                                 
138100         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
138200           MOVE MFS-NUM-FAELT-FEL   TO RESP-TIANKDAG-UPD-ATTR             
138300           MOVE NEJ                 TO INDATA-SW                          
138400         ELSE                                                             
138500           MOVE MFS-NUM-FAELT-RAETT TO RESP-TIANKDAG-UPD-ATTR             
138600         END-IF                                                           
138700       END-IF                                                             
138800     ELSE                                                                 
138900       MOVE MFS-NUM-FAELT-FEL TO RESP-TIANKDAG-UPD-ATTR                   
139000       MOVE NEJ               TO INDATA-SW                                
139100     END-IF                                                               
139200     .                                                                    
139300     EJECT                                                                
139400 GBAAC-CONTROL-IDLEVNR         SECTION.                                   
139500                                                                          
139600     IF REQU-IDLEVNR-UPD       NOT = SPACE                                
139700       MOVE REQU-IDLEVNR-UPD   TO W-IDLEVNR                               
139800                                W-IDLEVNR-UPD                             
139900       PERFORM IMS-GU-LEVA01                                              
140000       IF SEGMENT-FINNS                                                   
140100         MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDLEVNR-UPD-ATTR               
140200       ELSE                                                               
140300         MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDLEVNR-UPD-ATTR               
140400         MOVE NEJ                  TO INDATA-SW                           
140500       END-IF                                                             
140600                                                                          
140700       IF W-IDLEVNR  = INL-IDLEVNR                                        
140800         MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDLEVNR-UPD-ATTR               
140900         MOVE NEJ                  TO INDATA-SW                           
141000       END-IF                                                             
141100     ELSE                                                                 
141200       MOVE MFS-ALFA-FAELT-FEL        TO RESP-IDLEVNR-UPD-ATTR            
141300       MOVE NEJ                       TO INDATA-SW                        
141400     END-IF                                                               
141500                                                                          
141600     IF INL-IDANALYS >  ZERO OR                                           
141700        INL-IDKONTO  >  ZERO OR                                           
141800        INL-IDKST    >  SPACE                                             
141900       MOVE MFS-ALFA-FAELT-FEL        TO RESP-IDLEVNR-UPD-ATTR            
142000       MOVE NEJ                       TO INDATA-SW                        
142100     END-IF                                                               
142200     .                                                                    
142300     EJECT                                                                
142400 GBAAD-CONTROL-IDFTG          SECTION.                                    
142500     IF REQU-IDFTG-UPD NUMERIC                                            
142600       MOVE MFS-NUM-FAELT-RAETT      TO RESP-IDFTG-UPD-ATTR               
142700     ELSE                                                                 
142800       MOVE MFS-NUM-FAELT-FEL        TO RESP-IDFTG-UPD-ATTR               
142900       MOVE NEJ                      TO INDATA-SW                         
143000       MOVE MUST-BE-NUMERIC          TO RESP-IDMSG-ERROR                  
143100       MOVE 'IDFTG-UPD'              TO RESP-IDELMT-ERROR                 
143200     END-IF                                                               
143300     .                                                                    
143400     EJECT                                                                
143500 GBAAH-CONTROL-KONTO-IDLEVNR  SECTION.                                    
143600     IF REQU-IDLEVNR-UPD NOT = ALL '+'                                    
143700       IF W-IDLEVNR-UPD = SPACE OR '1000 ' OR '1004 ' OR                  
143800                        '8265 ' OR '9999 ' OR 'BP2TD'                     
143900         CONTINUE                                                         
144000       ELSE                                                               
144100         MOVE NEJ              TO KONTO-LEV-SW                            
144200       END-IF                                                             
144300     ELSE                                                                 
144400       IF INL-IDLEVNR = SPACE OR '1000 ' OR '1004 ' OR                    
144500                      '8265 ' OR '9999 ' OR 'BP2TD'                       
144600         CONTINUE                                                         
144700       ELSE                                                               
144800         MOVE NEJ              TO KONTO-LEV-SW                            
144900       END-IF                                                             
145000     END-IF                                                               
145100                                                                          
145200     IF KONTO-LEV-FEL                                                     
145300       MOVE MFS-NUM-FAELT-FEL    TO RESP-IDFTG-UPD-ATTR                   
145400                                    RESP-IDKONTO-UPD-ATTR                 
145500                                    RESP-IDANALYS-UPD-ATTR                
145600                                    RESP-IDKST-UPD-ATTR                   
145700       MOVE NEJ                  TO INDATA-SW                             
145800       MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                      
145900     END-IF                                                               
146000     .                                                                    
146100     EJECT                                                                
146200 GBAAJ-CONTROL-MOT-SAP        SECTION.                                    
146300     MOVE 'SEPV'               TO SAP-KDTRADP                             
146400     MOVE ZERO                 TO SAP-IDDISTR                             
146500     MOVE SPACE                TO SAP-KDFAKTYP                            
146600     MOVE ZERO                 TO SAP-IDFTG                               
146700     IF REQU-IDKONTO-UPD NOT NUMERIC                                      
146800       MOVE ZERO               TO SAP-IDKONTO                             
146900     ELSE                                                                 
147000       MOVE REQU-IDKONTO-UPD   TO SAP-IDKONTO                             
147100     END-IF                                                               
147200     IF REQU-IDANALYS-UPD = ALL '+' OR                                    
147300        REQU-IDANALYS-UPD = ALL '0'                                       
147400       MOVE SPACE              TO SAP-IDANALYS                            
147500     ELSE                                                                 
147600       MOVE REQU-IDANALYS-UPD  TO SAP-IDANALYS                            
147700     END-IF                                                               
148100     MOVE REQU-IDKST-UPD       TO SAP-IDKST                               
148300     MOVE SPACE                TO SAP-IDPROFIT                            
148400     MOVE +2                   TO SAP-KDCALL                              
148500                                                                          
148600     CALL W411SAP USING SAP-W411SAP SAP-SAPC-PCB                          
148700     .                                                                    
148800     EJECT                                                                
148900 GBAB-UPDATE-AENDRING-HUVUD     SECTION.                                  
149000     MOVE NEJ                  TO W-FLFEL                                 
149100     IF AENDRING-NYCKEL                                                   
149200       PERFORM GBABA-CONTROL-ANTAL-ART                                    
149300       IF FOER-MANGA-ART                                                  
149400         MOVE NEJ                      TO INDATA-SW                       
149500         MOVE ERR-UPDATE-FORBIDD       TO RESP-IDMSG-ERROR                
149600       ELSE                                                               
149700         PERFORM IMS-GHU-INLA1-INLA01                                     
149800         PERFORM GBABB-BORTTAG-NYUPPLAEGG                                 
149900       END-IF                                                             
150000     ELSE                                                                 
150100       PERFORM IMS-GHU-INLA1-INLA01                                       
150200       MOVE INL-FLFEL          TO W-SPAR-INL-FLFEL                        
150300                                                                          
150400       PERFORM S02-FLYTTA-EJ-NYCKEL-FAELT                                 
150500       PERFORM IMS-REPL-INLA1-INLA01                                      
150600                                                                          
150700       MOVE INL-IDFTG          TO W-INL-IDFTG                             
150800       PERFORM IMS-GHNP-INLA1-INLA11                                      
150900                                                                          
151000       PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                       
151100         IF ART-FLFEL = JA                                                
151200           MOVE ART-FLFEL TO W-FLFEL                                      
151300         END-IF                                                           
151400         PERFORM IMS-GHNP-INLA1-INLA11                                    
151500       END-PERFORM                                                        
151600                                                                          
151700       PERFORM IMS-GHU-INLA1-INLA01                                       
151800       IF KONTO-UPPG-AENDRAT OR                                           
151900          REQU-IDFTG-UPD       NOT = ALL '+'                              
152000                                                                          
152100         MOVE INL-IDLEVNR      TO W-SPAR-IDLEVNR                          
152200         PERFORM IMS-GHNP-INLA1-INLA11                                    
152300                                                                          
152400         PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-SAKNAS                     
152500           MOVE ART-IDARTNR            TO W-IDARTNR                       
152600           MOVE ART-IDRADNR-INL        TO W-IDRADNR-INL                   
152700                                                                          
152800           PERFORM IMS-GU-ARTC-ARTC01                                     
152900           MOVE ARTC01-ART-IDFTG TO W-IDFTG                               
153000           MOVE JA                 TO KDRT-SW                             
153100                                LEVPLAN-SW                                
153200           IF KONTO-UPPG-AENDRAT                                          
153300               PERFORM S03-CONTROL-KDRT-IDLEVNR                           
153400           END-IF                                                         
153500                                                                          
153600           PERFORM S04-CONTROL-LEVPLAN                                    
153700           IF KDRT-FEL           OR                                       
153800              LEVPLAN-FEL                                                 
153900             MOVE JA             TO ART-FLFEL                             
154000             PERFORM IMS-REPL-INLA1-INLA11                                
154100           ELSE                                                           
154200                                                                          
154300             PERFORM IMS-GNP-ARTC-ARTC11                                  
154400             IF  ARTC11-CLAG-KDERS < 20                                   
154500             AND ART-PRARTSTD      > ZERO                                 
154600             AND ART-KVAVIS        > ZERO                                 
154700               MOVE +2                             TO W-KDAVROP           
154800               MOVE ZERO                           TO WS-KVAVROP          
154810               MOVE ART-IDARTNR    TO W-IDARTNR-D9                        
154820               MOVE ART-IDDC       TO W-IDDC-D9                           
154900                                                                          
155000               PERFORM IMS-GET-WDD902                                     
155100               IF SEGMENT-FINNS                                           
155200                                                                          
155300                 PERFORM IMS-GNP-WDD905                                   
155400                 IF SEGMENT-FINNS                                         
155500                   IF KVAVROP < ART-KVAVIS                                
155600                                                                          
155700                     PERFORM UNTIL SEGMENT-SAKNAS OR                      
155800                    (WS-KVAVROP NOT < ART-KVAVIS)                         
155900                                                                          
156000                       ADD KVAVROP TO WS-KVAVROP                          
156100                       PERFORM IMS-GNP-WDD905                             
156200                     END-PERFORM                                          
156300                                                                          
156400                   END-IF                                                 
156500                 END-IF                                                   
156600               END-IF                                                     
156700                                                                          
156800               IF ART-KVAVIS > WS-KVAVROP                                 
156900                 MOVE NEJ                 TO ART-FLFEL                    
157000                 PERFORM IMS-REPL-INLA1-INLA11                            
157100               END-IF                                                     
157200             END-IF                                                       
157300           END-IF                                                         
157400                                                                          
157500           IF ART-FLFEL                = JA                               
157600               MOVE JA                 TO W-FLFEL                         
157700           END-IF                                                         
157800           PERFORM IMS-GHNP-INLA1-INLA11                                  
157900                                                                          
158000         END-PERFORM                                                      
158100       END-IF                                                             
158200     END-IF                                                               
158300                                                                          
158400     IF W-FLFEL                NOT = W-SPAR-INL-FLFEL                     
158500       PERFORM IMS-GHU-INLA1-INLA01                                       
158600       MOVE W-FLFEL            TO  INL-FLFEL                              
158700       PERFORM IMS-REPL-INLA1-INLA01                                      
158800     END-IF                                                               
158900     .                                                                    
159000     EJECT                                                                
159100 GBABA-CONTROL-ANTAL-ART     SECTION.                                     
159200                                                                          
159300     MOVE +0                   TO W-ANT-ART                               
159400     MOVE NEJ                  TO FOER-MANGA-ART-SW                       
159500                                                                          
159600     PERFORM IMS-GU-INLA2-INLA01                                          
159700     IF SEGMENT-FINNS                                                     
159800       PERFORM IMS-GNP-INLA2-INLA11                                       
159900       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                    
160000                     FOER-MANGA-ART                                       
160100         ADD +1                  TO W-ANT-ART                             
160200         IF W-ANT-ART            >  W-MAX-ART                             
160300             MOVE JA             TO FOER-MANGA-ART-SW                     
160400         END-IF                                                           
160500                                                                          
160600         PERFORM IMS-GNP-INLA2-INLA11                                     
160700       END-PERFORM                                                        
160800     ELSE                                                                 
160900       MOVE NEJ                TO INDATA-SW                               
161000       MOVE ERR-MISSING-IN-REG TO RESP-IDMSG-ERROR                        
161100     END-IF                                                               
161200     .                                                                    
161300     EJECT                                                                
161400 GBABB-BORTTAG-NYUPPLAEGG    SECTION.                                     
161500                                                                          
161600     PERFORM GBABBA-SKAPA-NY-INLA01                                       
161700                                                                          
161800     MOVE INL-IDLEVNR          TO W-SPAR-IDLEVNR                          
161900     MOVE INL-FLFEL            TO W-SPAR-INL-FLFEL                        
162000     MOVE INL-TIANKDAG         TO W-SPAR-TIANKDAG                         
162100     MOVE INL-TIAVIDAT         TO W-SPAR-TIAVIDAT                         
162200     MOVE INL-IDFTG            TO W-INL-IDFTG                             
162300     PERFORM IMS-ISRT-INLA2-INLA01                                        
162400                                                                          
162500     PERFORM IMS-GNP-INLA1-INLA11                                         
162600                                                                          
162700     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
162800       MOVE ART-IDARTNR        TO W-IDARTNR                               
162900       MOVE ART-IDRADNR-INL    TO W-IDRADNR-INL                           
163000                                                                          
163100       IF KONTO-UPPG-AENDRAT                                              
163200       OR REQU-IDFTG-UPD  NOT = ALL '+'                                   
163300       OR REQU-IDLEVNR-UPD NOT = ALL '+'                                  
163400         MOVE JA                    TO KDRT-SW                            
163500                                   LEVPLAN-SW                             
163600         PERFORM IMS-GU-ARTC-ARTC01                                       
163700         MOVE ARTC01-ART-IDFTG TO W-IDFTG                                 
163800                                                                          
163900         IF KONTO-UPPG-AENDRAT                                            
164000           PERFORM S03-CONTROL-KDRT-IDLEVNR                               
164100         END-IF                                                           
164200                                                                          
164300         PERFORM S04-CONTROL-LEVPLAN                                      
164400         IF KDRT-FEL OR LEVPLAN-FEL                                       
164500           MOVE JA           TO ART-FLFEL                                 
164600         ELSE                                                             
164700                                                                          
164800           PERFORM IMS-GNP-ARTC-ARTC11                                    
164900           IF  ARTC11-CLAG-KDERS < 20 AND ART-PRARTSTD > ZERO             
165000           AND ART-KVAVIS > ZERO                                          
165100             MOVE +2                           TO W-KDAVROP               
165200             MOVE ZERO                         TO WS-KVAVROP              
165210             MOVE ART-IDARTNR    TO W-IDARTNR-D9                          
165220             MOVE ART-IDDC       TO W-IDDC-D9                             
165300                                                                          
165400             PERFORM IMS-GET-WDD902                                       
165500             IF SEGMENT-FINNS                                             
165600               PERFORM IMS-GNP-WDD905                                     
165700                                                                          
165800               IF SEGMENT-FINNS                                           
165900                 IF KVAVROP < ART-KVAVIS                                  
166000                                                                          
166100                    PERFORM UNTIL SEGMENT-SAKNAS OR                       
166200                     (WS-KVAVROP NOT < ART-KVAVIS)                        
166300                      ADD KVAVROP TO WS-KVAVROP                           
166400                      PERFORM IMS-GNP-WDD905                              
166500                    END-PERFORM                                           
166600                                                                          
166700                  END-IF                                                  
166800               END-IF                                                     
166900             END-IF                                                       
167000                                                                          
167100             IF ART-KVAVIS > WS-KVAVROP                                   
167200               MOVE NEJ               TO ART-FLFEL                        
167300             END-IF                                                       
167400           END-IF                                                         
167500         END-IF                                                           
167600** OM TIANKDAG ÄR 0 HAR LEVERANSPLANEN EJ BLIVIT UPPDATERAD               
167700** TIDIGARE OCH SKALL DÄRMED INTE UPPDATERAS NU HELLER                    
167800** FÖR NDC:ERNA SKAPAS DET HELLER INGA LEVERANSPLANER, ÄR DET             
167900** ETT NDC (DVS IDDC NOT = '11' OR '12') GÖRS INGEN UPPDAT.               
168000                                                                          
168100         IF  W-SPAR-TIANKDAG > ZERO                                       
168200         AND REQU-IDLEVNR-UPD NOT = ALL '+'                               
168300         AND CDC                                                          
168400           PERFORM GBABBB-TA-BORT-LEVBESKED                               
168500         END-IF                                                           
168600       END-IF                                                             
168700                                                                          
168800       IF ART-FLFEL                  =  JA                                
168900         MOVE JA                     TO W-FLFEL                           
169000       END-IF                                                             
169100                                                                          
169200       PERFORM IMS-ISRT-INLA2-INLA11                                      
169300       PERFORM IMS-GHNP-INLA1-INLA21                                      
169400                                                                          
169500       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
169600         IF REQU-IDLEVNR-UPD NOT = ALL '+' AND                            
169700            RAD-IDRADNR > 1                                               
169800           MOVE REQU-IDLEVNR-UPD TO RAD-IDLEVNR-KOLLI                     
169900         END-IF                                                           
170000                                                                          
170100         PERFORM IMS-ISRT-INLA2-INLA21                                    
170200         PERFORM IMS-DLET-INLA1-INLA21                                    
170300         PERFORM IMS-GHNP-INLA1-INLA21                                    
170400       END-PERFORM                                                        
170500                                                                          
170600       PERFORM IMS-GNP-INLA1-INLA11                                       
170700     END-PERFORM                                                          
170800                                                                          
170900     PERFORM IMS-GHU-INLA1-INLA01                                         
171000     PERFORM IMS-DLET-INLA1-INLA01                                        
171100                                                                          
171200     MOVE W-SPAR-IDLEVNR       TO W-D101KY-IDLEVNR                        
171300     MOVE W-SPAR-IDFS          TO W-D101KY-IDFS                           
171400     MOVE W-SPAR-TIAVIDAT      TO W-D101KY-TIAVIDAT                       
171500     .                                                                    
171600     EJECT                                                                
171700 GBABBA-SKAPA-NY-INLA01      SECTION.                                     
171800     IF REQU-TIAVIDAT-UPD           NOT = ALL '+'                         
171900       MOVE W-TIAVIDAT-UPD          TO INL-TIAVIDAT                       
172000                                       RESP-TIAVIDAT                      
172100     ELSE                                                                 
172200       MOVE W-PLUS                  TO RESP-TIAVIDAT-UPD                  
172300     END-IF                                                               
172400                                                                          
172500     IF REQU-IDLEVNR-UPD            NOT = ALL '+'                         
172600       MOVE REQU-IDLEVNR-UPD        TO INL-IDLEVNR                        
172700                                       RESP-IDLEVNR                       
172800     ELSE                                                                 
172900       MOVE W-PLUS                  TO RESP-IDLEVNR-UPD                   
173000     END-IF                                                               
173100                                                                          
173200     MOVE INL-IDLEVNR               TO W-SPAR-IDLEVNR                     
173300     MOVE INL-IDFS                  TO W-SPAR-IDFS                        
173400     MOVE INL-TIAVIDAT              TO W-SPAR-TIAVIDAT                    
173500                                                                          
173600     PERFORM S02-FLYTTA-EJ-NYCKEL-FAELT                                   
173700     .                                                                    
173800     EJECT                                                                
173900 GBABBB-TA-BORT-LEVBESKED  SECTION.                                       
174000                                                                          
174100     MOVE ART-IDARTNR          TO LEVP-IDARTNR                            
174110     MOVE WS-IDDC              TO LEVP-IDDC                               
174200     MOVE W-SPAR-IDLEVNR       TO LEVP-IDLEVNR                            
174300     MOVE W-SPAR-TIAVIDAT      TO LEVP-TILEVBSK-AVS                       
174400     MOVE 'B'                  TO LEVP-KDBEH                              
174500     MOVE ZERO                 TO LEVP-KVAVIS                             
174600                                  LEVP-KVDAGAR-INLEV                      
174700                                  LEVP-TILEVBSK-INL                       
174800                                                                          
174900     CALL W611LEVP USING LEVP-W611LEVP LEVP-INLB-PCB                      
175000     .                                                                    
175100     EJECT                                                                
175200 GBB-BORTTAG-HUVUD     SECTION.                                           
175300     IF REQU-FLBORT-UPD  =  JA OR NEJ OR YES                              
175400       MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLBORT-UPD-ATTR                  
175500       IF REQU-FLBORT-UPD = YES                                           
175600          MOVE JA TO REQU-FLBORT-UPD                                      
175700       END-IF                                                             
175800     ELSE                                                                 
175900       MOVE REQU-FLBORT-UPD        TO RESP-FLBORT-UPD                     
176000       MOVE MFS-ALFA-FAELT-FEL     TO RESP-FLBORT-UPD-ATTR                
176100       MOVE NEJ                    TO INDATA-SW                           
176200       MOVE ERR-CORR-HILITE-FLDS   TO RESP-IDMSG-ERROR                    
176300     END-IF                                                               
176400                                                                          
176500     IF INDATA-OK AND REQU-FLBORT-UPD = JA                                
176600       PERFORM IMS-GHU-INLA1-INLA01                                       
176700       MOVE INL-IDLEVNR        TO W-SPAR-IDLEVNR                          
176800       MOVE INL-TIAVIDAT       TO W-SPAR-TIAVIDAT                         
176900                                                                          
177000*NDC HAR INTE LEVERANSPLANER UPPLAGDA, DÄRFÖR SKA INGA TAS BORT.          
177100       IF CDC                                                             
177200         PERFORM IMS-GNP-INLA1-INLA11                                     
177300                                                                          
177400         PERFORM UNTIL SEGMENT-SAKNAS                                     
177500           PERFORM GBBA-TA-BORT-LEVBESKED                                 
177600           PERFORM IMS-GNP-INLA1-INLA11                                   
177700         END-PERFORM                                                      
177800                                                                          
177900         PERFORM IMS-GHU-INLA1-INLA01                                     
178000       END-IF                                                             
178100       PERFORM IMS-DLET-INLA1-INLA01                                      
178200     END-IF                                                               
178300     .                                                                    
178400     EJECT                                                                
178500 GBBA-TA-BORT-LEVBESKED  SECTION.                                         
178600                                                                          
178700     MOVE ART-IDARTNR             TO LEVP-IDARTNR                         
178800     MOVE W-SPAR-IDLEVNR          TO LEVP-IDLEVNR                         
178900     MOVE W-SPAR-TIAVIDAT         TO LEVP-TILEVBSK-AVS                    
179000     MOVE 'B'                     TO LEVP-KDBEH                           
179100     MOVE ZERO                    TO LEVP-KVAVIS                          
179200                                     LEVP-KVDAGAR-INLEV                   
179300                                     LEVP-TILEVBSK-INL                    
179400                                                                          
179500     CALL W611LEVP USING LEVP-W611LEVP LEVP-INLB-PCB                      
179600     .                                                                    
179700     EJECT                                                                
179800 GBC-AENDRING-RAD      SECTION.                                           
179900     PERFORM GBCA-CONTROL-AENDRING-RAD                                    
180000     IF INDATA-OK                                                         
180100                                                                          
180200       PERFORM GBCB-UPDATE-EV-AENDRING-RAD                                
180300     END-IF                                                               
180400     .                                                                    
180500     EJECT                                                                
180600 GBCA-CONTROL-AENDRING-RAD    SECTION.                                    
180700     MOVE +1                   TO INDX                                    
180800     PERFORM UNTIL INDX        >  MAX-INDX                                
180900                                                                          
181000       IF REQU-IDARTNR-LINE (INDX) = ALL '+' AND                          
181100          REQU-KVAVIS-LINE (INDX)  = ALL '+' AND                          
181200          REQU-KDRT-LINE   (INDX)  = ALL '+'                              
181300         CONTINUE                                                         
181400       ELSE                                                               
181500         IF REQU-IDARTNR-LINE (INDX) NOT = ALL '+'                        
181600           PERFORM GBCAA-CONTROL-IDARTNR                                  
181700         ELSE                                                             
181800           MOVE ZERO           TO REG4-IDARTNR-GAMMAL (INDX)              
181900                                  REG4-IDARTNR-NY(INDX)                   
182000         END-IF                                                           
182100                                                                          
182200         IF REQU-KVAVIS-LINE (INDX) NOT = ALL '+'                         
182300           PERFORM GBCAB-CONTROL-KVAVIS                                   
182400           MOVE REQU-IDRADNR-INL-SPAR-LINE(INDX) TO                       
182500                              REG4-IDRADNR-INL-GAMMAL(INDX)               
182600           MOVE REQU-IDARTNR-SPAR-LINE(INDX) TO                           
182700                              REG4-IDARTNR-GAMMAL(INDX)                   
182800           IF REG4-IDARTNR-NY (INDX) = ZERO                               
182900             MOVE REQU-IDARTNR-SPAR-LINE(INDX) TO                         
183000                                  REG4-IDARTNR-NY(INDX)                   
183100           END-IF                                                         
183200         END-IF                                                           
183300                                                                          
183400         IF REQU-KDRT-LINE (INDX) NOT = ALL '+'                           
183500           PERFORM GBCAC-CONTROL-KDRT                                     
183600           MOVE REQU-IDRADNR-INL-SPAR-LINE(INDX) TO                       
183700                              REG4-IDRADNR-INL-GAMMAL(INDX)               
183800           MOVE REQU-IDARTNR-SPAR-LINE(INDX) TO                           
183900                              REG4-IDARTNR-GAMMAL(INDX)                   
184000           IF REG4-IDARTNR-NY (INDX)         = ZERO                       
184100               MOVE REQU-IDARTNR-SPAR-LINE(INDX) TO                       
184200                                  REG4-IDARTNR-NY          (INDX)         
184300           END-IF                                                         
184400         ELSE                                                             
184500           MOVE 99             TO REG4-KDRT     (INDX)                    
184600         END-IF                                                           
184700       END-IF                                                             
184800                                                                          
184900       ADD +1                  TO INDX                                    
185000     END-PERFORM                                                          
185100     .                                                                    
185200     EJECT                                                                
185300 GBCAA-CONTROL-IDARTNR      SECTION.                                      
185400     IF REQU-IDARTNR-LINE (INDX) NUMERIC                                  
185500       IF REQU-IDARTNR-LINE (INDX) NOT =                                  
185600                                   REQU-IDARTNR-SPAR-LINE (INDX)          
185700         MOVE MFS-NUM-FAELT-RAETT TO                                      
185800                            RESP-IDARTNR-LINE-ATTR(INDX)                  
185900         MOVE REQU-IDRADNR-INL-SPAR-LINE(INDX) TO                         
186000                             REG4-IDRADNR-INL-GAMMAL(INDX)                
186100         MOVE REQU-IDARTNR-SPAR-LINE(INDX) TO                             
186200                             REG4-IDARTNR-GAMMAL(INDX)                    
186300         MOVE REQU-IDARTNR-LINE(INDX)  TO REG4-IDARTNR-NY   (INDX)        
186400         MOVE +3                       TO REG4-KDBEH        (INDX)        
186500       END-IF                                                             
186600     ELSE                                                                 
186700       MOVE REQU-IDARTNR-LINE (INDX) TO                                   
186800                                 RESP-IDARTNR-LINE(INDX)                  
186900       MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-LINE-ATTR(INDX)             
187000       MOVE NEJ               TO INDATA-SW                                
187100       MOVE MUST-BE-NUMERIC   TO RESP-IDMSG-ERROR                         
187200     END-IF                                                               
187300     .                                                                    
187400     EJECT                                                                
187500 GBCAB-CONTROL-KVAVIS         SECTION.                                    
187600     IF REQU-KVAVIS-LINE (INDX)      NUMERIC AND                          
187700        REQU-KVAVIS-LINE (INDX)      > ZERO                               
187800       MOVE MFS-NUM-FAELT-RAETT   TO RESP-KVAVIS-LINE-ATTR(INDX)          
187900       MOVE REQU-KVAVIS-LINE(INDX)   TO REG4-KVAVIS        (INDX)         
188000       MOVE REQU-IDRADNR-INL-SPAR-LINE(INDX) TO                           
188100                         REG4-IDRADNR-INL-GAMMAL(INDX)                    
188200       MOVE REQU-IDARTNR-SPAR-LINE(INDX) TO                               
188300                         REG4-IDARTNR-GAMMAL(INDX)                        
188400       MOVE +3                    TO REG4-KDBEH         (INDX)            
188500     ELSE                                                                 
188600       MOVE REQU-KVAVIS-LINE (INDX) TO RESP-KVAVIS-LINE(INDX)             
188700       MOVE MFS-NUM-FAELT-FEL   TO RESP-KVAVIS-LINE-ATTR(INDX)            
188800       MOVE NEJ                 TO INDATA-SW                              
188900       MOVE MUST-BE-NUMERIC     TO RESP-IDMSG-ERROR                       
189000     END-IF                                                               
189100     .                                                                    
189200     EJECT                                                                
189300 GBCAC-CONTROL-KDRT           SECTION.                                    
189400     IF REQU-KDRT-LINE (INDX)    NUMERIC   AND                            
189500        REQU-KDRT-LINE (INDX)    NOT = 99                                 
189600       MOVE MFS-NUM-FAELT-RAETT  TO RESP-KDRT-LINE-ATTR(INDX)             
189700       MOVE REQU-KDRT-LINE(INDX) TO REG4-KDRT        (INDX)               
189800       IF REG4-KDBEH (INDX)      =  ZERO                                  
189900           MOVE +2               TO REG4-KDBEH       (INDX)               
190000       END-IF                                                             
190100     ELSE                                                                 
190200       MOVE REQU-KDRT-LINE(INDX) TO RESP-KDRT-LINE(INDX)                  
190300       MOVE MFS-NUM-FAELT-FEL    TO RESP-KDRT-LINE-ATTR(INDX)             
190400       MOVE NEJ                  TO RESP-KDRT-LINE-ATTR(INDX)             
190500       MOVE NEJ                  TO INDATA-SW                             
190600       MOVE MUST-BE-NUMERIC      TO RESP-IDMSG-ERROR                      
190700                                                                          
190800     END-IF                                                               
190900     .                                                                    
191000     EJECT                                                                
191100 GBCB-UPDATE-EV-AENDRING-RAD              SECTION.                        
191200     PERFORM IMS-GU-INLA1-INLA01                                          
191300     IF SEGMENT-FINNS                                                     
191400       MOVE WS-IDFS              TO REG4-IDFS                             
191500       MOVE INL-IDLEVNR          TO REG4-IDLEVNR                          
191600       MOVE INL-TIAVIDAT         TO REG4-TIAVIDAT                         
191700       MOVE INL-IDFTG            TO REG4-IDFTG                            
191800       MOVE INL-IDKONTO          TO REG4-IDKONTO                          
191900       MOVE INL-IDANALYS         TO REG4-IDANALYS                         
192000       MOVE INL-IDKST            TO REG4-IDKST                            
192100       CALL W611REG USING LAENK-W611REG0                                  
192200                          REG-INLA1-PCB REG-INLA2-PCB                     
192300                          REG-INLA3-PCB REG-LEVA-PCB                      
192400                                        REG-ARTC-PCB REG-BENA-PCB         
192500                          REG-WDD9-PCB REG-WDK7-PCB REG-WDB6-PCB          
192600                                                                          
192700       MOVE +1                   TO INDX                                  
192800       MOVE ZERO                 TO W-IDARTNR                             
192900       PERFORM UNTIL INDX        >  MAX-INDX                              
193000         PERFORM GBCBA-CONTROL-FELFLAGGOR                                 
193100         IF REG4-IDARTNR-NY (INDX)          >  ZERO AND                   
193200            W-IDARTNR                       =  ZERO                       
193300             MOVE REG4-IDARTNR-NY(INDX)     TO W-IDARTNR                  
193400         END-IF                                                           
193500         ADD    +1               TO INDX                                  
193600       END-PERFORM                                                        
193700                                                                          
193800** OM TIANKDAG ÄR 0 HAR LEVERANSPLANEN EJ BLIVIT UPPDATERAD               
193900** TIDIGARE OCH SKALL DÄRMED EJ UPPDATERAS NU HELLER                      
194000** NDC  HAR INGA LEVERANSPLANER, DÄRFÖR SKA INTE DESSA UPP-               
194100** DATERAS                                                                
194200       IF INDATA-OK AND INL-TIANKDAG > ZERO AND                           
194300          CDC                                                             
194400         PERFORM GBCBB-UPD-EV-LEVERANSBESKED                              
194500       END-IF                                                             
194600     ELSE                                                                 
194700       MOVE NEJ                    TO INDATA-SW                           
194800     END-IF                                                               
194900     .                                                                    
195000     EJECT                                                                
195100 GBCBA-CONTROL-FELFLAGGOR    SECTION.                                     
195200     IF REG4-IDARTNR-OK (INDX) = NEJ                                      
195300       MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-LINE-ATTR(INDX)             
195400       MOVE NEJ               TO INDATA-SW                                
195500     END-IF                                                               
195600                                                                          
195700     IF REG4-KDRT-OK (INDX) = NEJ                                         
195800       MOVE MFS-NUM-FAELT-FEL        TO RESP-KDRT-LINE-ATTR(INDX)         
195900       MOVE NEJ                      TO INDATA-SW                         
196000     END-IF                                                               
196100                                                                          
196200     IF REG4-IDMFSFEL (INDX)   NOT = SPACE                                
196300*      IF MED-IDMFSFEL = SPACE                                            
196400       IF RESP-IDMSG-ERROR     = SPACE                                    
196500         MOVE REG4-IDMFSFEL (INDX)  TO RESP-IDMSG-ERROR                   
196600       END-IF                                                             
196700     END-IF                                                               
196800     .                                                                    
196900     EJECT                                                                
197000 GBCBB-UPD-EV-LEVERANSBESKED SECTION.                                     
197100     MOVE +1                      TO INDX                                 
197200     PERFORM UNTIL INDX           >  MAX-INDX                             
197300       IF REG4-IDARTNR-NY(INDX) > ZERO                                    
197400         PERFORM GBCBBA-UPD-LEVBESKED                                     
197500       END-IF                                                             
197600       ADD +1                     TO INDX                                 
197700     END-PERFORM                                                          
197800     .                                                                    
197900     EJECT                                                                
198000 GBCBBA-UPD-LEVBESKED  SECTION.                                           
198100                                                                          
198200     IF REG4-IDARTNR-GAMMAL(INDX)   =                                     
198300        REG4-IDARTNR-NY    (INDX)                                         
198400       MOVE 'R'                     TO LEVP-KDBEH                         
198500     ELSE                                                                 
198600       MOVE 'B'                     TO LEVP-KDBEH                         
198700     END-IF                                                               
198800     MOVE REG4-IDARTNR-GAMMAL(INDX) TO LEVP-IDARTNR                       
198900     MOVE REG4-IDLEVNR              TO LEVP-IDLEVNR                       
199000     MOVE REG4-TIAVIDAT             TO LEVP-TILEVBSK-AVS                  
199100     MOVE REG4-KVAVIS(INDX)         TO LEVP-KVAVIS                        
199200     MOVE ZERO                      TO LEVP-KVDAGAR-INLEV                 
199300                                       LEVP-TILEVBSK-INL                  
199400                                                                          
199500     CALL W611LEVP USING LEVP-W611LEVP LEVP-INLB-PCB                      
199600     .                                                                    
199700     EJECT                                                                
199800 GBD-BORTTAG-RAD       SECTION.                                           
199900                                                                          
200000     PERFORM GBDA-CONTROL-BORTTAG-RAD                                     
200100     IF INDATA-OK                                                         
200200       PERFORM GBDB-UPDATE-BORTTAG-RAD                                    
200300     END-IF                                                               
200400     .                                                                    
200500     EJECT                                                                
200600 GBDA-CONTROL-BORTTAG-RAD     SECTION.                                    
200700                                                                          
200800*      MOVE MUST-BE-NUMERIC          TO RESP-IDMSG-ERROR                  
200900                                                                          
201000     MOVE +1                   TO INDX                                    
201100     PERFORM UNTIL INDX        >  MAX-INDX                                
201200         IF REQU-KDCMD-LINE(INDX) NOT = ALL '+' AND NOT = SPACE           
201300             IF (REQU-KDCMD-LINE(INDX) = 'B' OR 'D' )                     
201400*                ---                 B=BORTTAG   D=DELETE                 
201500             AND REQU-IDRADNR-INL-SPAR-LINE (INDX) > ZERO                 
201600         MOVE REQU-IDRADNR-INL-SPAR-LINE (INDX) TO W-IDRADNR-INL          
201700               PERFORM IMS-GHU-INLA1-INLA11                               
201800               IF SEGMENT-FINNS                                           
201900                 MOVE MFS-ALFA-FAELT-RAETT        TO                      
202000                        RESP-KDCMD-LINE-ATTR      (INDX)                  
202100                 CONTINUE                                                 
202200               ELSE                                                       
202300                 MOVE MFS-ALFA-FAELT-FEL TO                               
202400                                     RESP-KDCMD-LINE-ATTR(INDX)           
202500                 MOVE MFS-NUM-FAELT-FEL TO                                
202600                                     RESP-IDARTNR-LINE-ATTR(INDX)         
202700                 MOVE NEJ           TO INDATA-SW                          
202800                 MOVE ERR-MISSING-IN-REG  TO RESP-IDMSG-ERROR             
202900               END-IF                                                     
203000             ELSE                                                         
203100                MOVE REQU-KDCMD-LINE (INDX) TO                            
203200                                     RESP-KDCMD-LINE-ATTR(INDX)           
203300                MOVE MFS-ALFA-FAELT-FEL     TO                            
203400                                     RESP-KDCMD-LINE-ATTR(INDX)           
203500                MOVE NEJ                    TO INDATA-SW                  
203600                MOVE ERR-WRONG-CMD-CODE     TO RESP-IDMSG-ERROR           
203700             END-IF                                                       
203800         END-IF                                                           
203900         ADD +1                TO INDX                                    
204000     END-PERFORM                                                          
204100     .                                                                    
204200     EJECT                                                                
204300 GBDB-UPDATE-BORTTAG-RAD       SECTION.                                   
204400     PERFORM IMS-GU-INLA1-INLA01                                          
204500     MOVE INL-IDLEVNR                        TO W-SPAR-IDLEVNR            
204600     MOVE INL-TIAVIDAT                       TO W-SPAR-TIAVIDAT           
204700     MOVE +1                                 TO INDX                      
204800     PERFORM UNTIL INDX                      >   MAX-INDX                 
204900         IF REQU-KDCMD-LINE(INDX)            NOT = ALL '+' AND            
205000                                             NOT = SPACE                  
205100           MOVE REQU-IDRADNR-INL-SPAR-LINE(INDX) TO W-IDRADNR-INL         
205200           PERFORM IMS-GHNP-INLA1-INLA11-KVAL                             
205300           IF ART-FLFEL                      =  JA                        
205400               MOVE ART-FLFEL                TO W-FLFEL                   
205500           END-IF                                                         
205600           PERFORM IMS-DLET-INLA1-INLA11                                  
205700* NDC  HAR INGA LEVERANSBESKED UPPLAGDA, DÄRMED SKA INGA TAS BORT.        
205800           IF CDC                                                         
205900             PERFORM GBDBA-TA-BORT-LEVBESKED                              
206000           END-IF                                                         
206100         END-IF                                                           
206200         ADD +1                TO INDX                                    
206300     END-PERFORM                                                          
206400                                                                          
206500     IF W-FLFEL                = JA                                       
206600       PERFORM S01-UPDATE-EV-INLA01                                       
206700     END-IF                                                               
206800     .                                                                    
206900     EJECT                                                                
207000 GBDBA-TA-BORT-LEVBESKED  SECTION.                                        
207100                                                                          
207200     MOVE ART-IDARTNR             TO LEVP-IDARTNR                         
207300     MOVE W-SPAR-IDLEVNR          TO LEVP-IDLEVNR                         
207400     MOVE W-SPAR-TIAVIDAT         TO LEVP-TILEVBSK-AVS                    
207500     MOVE 'B'                     TO LEVP-KDBEH                           
207600     MOVE ZERO                    TO LEVP-KVAVIS                          
207700                                     LEVP-KVDAGAR-INLEV                   
207800                                     LEVP-TILEVBSK-INL                    
207900                                                                          
208000     CALL W611LEVP USING LEVP-W611LEVP LEVP-INLB-PCB                      
208100     .                                                                    
208200     EJECT                                                                
208300 GBE-TILLAEGG-RAD            SECTION.                                     
208400                                                                          
208500     PERFORM GBEA-CONTROL-TILLAEGG-RAD                                    
208600     IF INDATA-OK                                                         
208700       PERFORM GBEB-UPDATE-EV-TILLAEGG-RAD                                
208800     END-IF                                                               
208900                                                                          
208910     IF REQU-IDARTNR-UPD NOT = ALL '+'                                    
209000        MOVE REQU-IDARTNR-UPD          TO W-IDARTNR                       
209010     END-IF                                                               
209100     .                                                                    
209200     EJECT                                                                
209300 GBEA-CONTROL-TILLAEGG-RAD    SECTION.                                    
209400                                                                          
209500     IF REQU-IDARTNR-UPD  NOT = ALL '+'                                   
209600       PERFORM GBEAA-CONTROL-IDARTNR                                      
209700     ELSE                                                                 
209800       MOVE REQU-IDARTNR-UPD  TO RESP-IDARTNR-UPD                         
209900       MOVE MFS-NUM-FAELT-FEL TO RESP-IDARTNR-UPD-ATTR                    
210000       MOVE NEJ                 TO INDATA-SW                              
210100     END-IF                                                               
210200                                                                          
210300     IF REQU-KVAVIS-UPD NOT = ALL '+'                                     
210400       PERFORM GBEAB-CONTROL-KVAVIS                                       
210500     ELSE                                                                 
210600*      MOVE REQU-KVAVIS-UPD   TO RESP-KVAVIS-UPD                          
210700       MOVE MFS-NUM-FAELT-FEL TO RESP-KVAVIS-UPD-ATTR                     
210800       MOVE NEJ                 TO INDATA-SW                              
210900     END-IF                                                               
211000                                                                          
211100     IF REQU-KDRT-UPD NOT = ALL '+'                                       
211200       PERFORM GBEAC-CONTROL-KDRT                                         
211300     END-IF                                                               
211400     .                                                                    
211500     EJECT                                                                
211600 GBEAA-CONTROL-IDARTNR      SECTION.                                      
211700     IF REQU-IDARTNR-UPD       NUMERIC                                    
211800       MOVE MFS-NUM-FAELT-RAETT      TO RESP-IDARTNR-UPD-ATTR             
211900       MOVE REQU-IDARTNR-UPD         TO REG4-IDARTNR-NY (1)               
212000     ELSE                                                                 
212100       MOVE REQU-IDARTNR-UPD         TO RESP-IDARTNR-UPD                  
212200       MOVE MFS-NUM-FAELT-FEL        TO RESP-IDARTNR-UPD-ATTR             
212300       MOVE NEJ                      TO INDATA-SW                         
212400     END-IF                                                               
212500     .                                                                    
212600     EJECT                                                                
212700 GBEAB-CONTROL-KVAVIS         SECTION.                                    
212800     IF REQU-KVAVIS-UPD              NUMERIC                              
212900       MOVE MFS-NUM-FAELT-RAETT      TO RESP-KVAVIS-UPD-ATTR              
213000       MOVE REQU-KVAVIS-UPD          TO REG4-KVAVIS (1)                   
213100     ELSE                                                                 
213200**     MOVE REQU-KVAVIS-UPD          TO RESP-KVAVIS-UPD                   
213300       MOVE MFS-NUM-FAELT-FEL        TO RESP-KVAVIS-UPD-ATTR              
213400       MOVE NEJ                      TO INDATA-SW                         
213500     END-IF                                                               
213600     .                                                                    
213700     EJECT                                                                
213800 GBEAC-CONTROL-KDRT           SECTION.                                    
213900     IF REQU-KDRT-UPD         NUMERIC AND REQU-KDRT-UPD NOT = 99          
214000       MOVE MFS-NUM-FAELT-RAETT      TO RESP-KDRT-UPD-ATTR                
214100       MOVE REQU-KDRT-UPD            TO REG4-KDRT (1)                     
214200*NDC                                                                      
214300       IF NDC                                                             
214400         IF REQU-KDRT-UPD = 00                                            
214500           MOVE MFS-NUM-FAELT-RAETT     TO RESP-KDRT-UPD-ATTR             
214600           MOVE REQU-KDRT-UPD           TO REG4-KDRT (1)                  
214700         ELSE                                                             
214800           MOVE REQU-KDRT-UPD             TO RESP-KDRT-UPD                
214900           MOVE MFS-NUM-FAELT-FEL         TO RESP-KDRT-UPD-ATTR           
215000           MOVE NEJ                       TO INDATA-SW                    
215100         END-IF                                                           
215200       END-IF                                                             
215300     ELSE                                                                 
215400       MOVE REQU-KDRT-UPD            TO RESP-KDRT-UPD                     
215500       MOVE MFS-NUM-FAELT-FEL        TO RESP-KDRT-UPD-ATTR                
215600       MOVE NEJ                      TO INDATA-SW                         
215700     END-IF                                                               
215800     .                                                                    
215900     EJECT                                                                
216000                                                                          
216100 GBEB-UPDATE-EV-TILLAEGG-RAD              SECTION.                        
216200     PERFORM IMS-GU-INLA1-INLA01                                          
216300     MOVE +1                   TO REG4-KDBEH (1)                          
216400     MOVE WS-IDFS              TO REG4-IDFS                               
216500     MOVE INL-IDLEVNR          TO REG4-IDLEVNR                            
216600     MOVE INL-TIAVIDAT         TO REG4-TIAVIDAT                           
216700     MOVE INL-IDFTG            TO REG4-IDFTG                              
216800     MOVE INL-IDKONTO          TO REG4-IDKONTO                            
216900     MOVE INL-IDANALYS         TO REG4-IDANALYS                           
217000     MOVE INL-IDKST            TO REG4-IDKST                              
217100                                                                          
217200     CALL W611REG USING LAENK-W611REG0                                    
217300                        REG-INLA1-PCB REG-INLA2-PCB                       
217400                        REG-INLA3-PCB REG-LEVA-PCB                        
217500                                      REG-ARTC-PCB REG-BENA-PCB           
217600                        REG-WDD9-PCB REG-WDK7-PCB REG-WDB6-PCB            
217700                                                                          
217800     IF REG4-IDARTNR-OK (1)  = NEJ                                        
217900       MOVE MFS-NUM-FAELT-FEL     TO RESP-IDARTNR-UPD-ATTR                
218000       MOVE NEJ                   TO INDATA-SW                            
218100     END-IF                                                               
218200                                                                          
218300     IF REG4-KDRT-OK (1)  = NEJ                                           
218400       MOVE MFS-NUM-FAELT-FEL      TO RESP-KDRT-UPD-ATTR                  
218500       MOVE NEJ                    TO INDATA-SW                           
218600     END-IF                                                               
218700                                                                          
218800     IF REG4-IDMFSFEL (1)   NOT = SPACE                                   
218900*      IF MED-IDMFSFEL = SPACE                                            
219000       IF RESP-IDMSG-ERROR = SPACE                                        
219100         MOVE REG4-IDMFSFEL (1)  TO RESP-IDMSG-ERROR                      
219200*        MOVE REG4-IDMFSFEL (1)  TO MED-IDMFSFEL                          
219300       END-IF                                                             
219400     END-IF                                                               
219500     .                                                                    
219600     EJECT                                                                
219700 GBF-GODK-RAD       SECTION.                                              
219800                                                                          
219900     PERFORM GBFA-CONTROL-GODK-RAD                                        
220000     IF INDATA-OK                                                         
220100       PERFORM GBFB-UPDATE-GODK-RAD                                       
220200     END-IF                                                               
220300     .                                                                    
220400     EJECT                                                                
220500 GBFA-CONTROL-GODK-RAD     SECTION.                                       
220600     MOVE +1                   TO INDX                                    
220700     PERFORM UNTIL INDX        >  MAX-INDX                                
220800       IF REQU-KDCMD-LINE(INDX) NOT = ALL '+' AND NOT = SPACE             
220900         IF (REQU-KDCMD-LINE(INDX) = 'G' OR 'A' )                         
221000*            ---                     G=GODKÄNN   A=ACCEPT                 
221100         AND REQU-IDRADNR-INL-SPAR-LINE (INDX) > ZERO                     
221200           MOVE REQU-IDRADNR-INL-SPAR-LINE (INDX) TO W-IDRADNR-INL        
221300           PERFORM IMS-GHU-INLA1-INLA11                                   
221400           IF SEGMENT-FINNS                                               
221500             MOVE REQU-KDCMD-LINE(INDX) TO                                
221600                                      RESP-KDCMD-LINE(INDX)               
221700             MOVE MFS-ALFA-FAELT-RAETT TO                                 
221800                                      RESP-KDCMD-LINE-ATTR(INDX)          
221900             MOVE ERR-WRONG-CMD-CODE   TO RESP-IDMSG-ERROR                
222000           ELSE                                                           
222100             MOVE MFS-ALFA-FAELT-FEL TO RESP-KDCMD-LINE-ATTR(INDX)        
222200             MOVE MFS-NUM-FAELT-FEL  TO                                   
222300                                    RESP-IDARTNR-LINE-ATTR(INDX)          
222400             MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                  
222500             MOVE NEJ                TO INDATA-SW                         
222600           END-IF                                                         
222700         ELSE                                                             
222800           MOVE REQU-KDCMD-LINE(INDX) TO RESP-KDCMD-LINE(INDX)            
222900           MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDCMD-LINE-ATTR(INDX)        
223000           MOVE NEJ                  TO INDATA-SW                         
223100           MOVE ERR-WRONG-CMD-CODE   TO RESP-IDMSG-ERROR                  
223200         END-IF                                                           
223300       END-IF                                                             
223400       ADD +1                  TO INDX                                    
223500     END-PERFORM                                                          
223600     .                                                                    
223700     EJECT                                                                
223800 GBFB-UPDATE-GODK-RAD       SECTION.                                      
223900     PERFORM IMS-GU-INLA1-INLA01                                          
224000     MOVE INL-IDLEVNR                        TO W-SPAR-IDLEVNR            
224100     MOVE INL-TIAVIDAT                       TO W-SPAR-TIAVIDAT           
224200     MOVE +1                                 TO INDX                      
224300     PERFORM UNTIL INDX                      >   MAX-INDX                 
224400       IF REQU-KDCMD-LINE(INDX) NOT = ALL '+' AND NOT = SPACE             
224500         MOVE REQU-IDRADNR-INL-SPAR-LINE(INDX)   TO W-IDRADNR-INL         
224600         PERFORM IMS-GHNP-INLA1-INLA11-KVAL                               
224700         IF ART-FLFEL                        =  JA                        
224800           PERFORM S05-CONTROL-OM-RAD-OK                                  
224900           IF RAD-OK                                                      
225000             PERFORM IMS-GHNP-INLA1-INLA11-KVAL-FST                       
225100             MOVE ART-FLFEL                    TO W-FLFEL                 
225200             MOVE NEJ                          TO ART-FLFEL               
225300             PERFORM IMS-REPL-INLA1-INLA11                                
225400           ELSE                                                           
225500             MOVE MFS-ALFA-FAELT-FEL TO                                   
225600                                    RESP-KDCMD-LINE-ATTR(INDX)            
225700             MOVE NEJ                TO INDATA-SW                         
225800*ERROR COD IS SET IN SECTION S05-CONTROL-OM-RAD-OK                        
225900           END-IF                                                         
226000         END-IF                                                           
226100       END-IF                                                             
226200       ADD +1                  TO INDX                                    
226300     END-PERFORM                                                          
226400                                                                          
226500     IF W-FLFEL                = JA                                       
226600       PERFORM S01-UPDATE-EV-INLA01                                       
226700     END-IF                                                               
226800     .                                                                    
226900     EJECT                                                                
227000 BG-CALL-WL01TIDZ       SECTION.                                          
227100*NDC  SKA ANVÄNDA SIG AV SITT LOKALA DATUM VID KONTROLL AV                
227200*     TIAVIDAT OCH ATT AKTUELLT PRIS-FINNS                                
227300                                                                          
227400     MOVE '011'                    TO TIDZ-MSGI-KDCALL                    
227500     MOVE DCS-IDTIDZON             TO TIDZ-MSGI-IDTIDZON                  
227500     MOVE DCS-IDDC                 TO TIDZ-MSGI-IDDC                      
227600     MOVE DAGENS-DATUM             TO TIDZ-MSGI-TILOKDAT                  
227700     MOVE DAGENS-TID               TO TIDZ-MSGI-TILOKTID                  
227800     CALL WL01TIDZ USING              TIDZ-MSGI-WL01TIDZ                  
227900     MOVE TIDZ-MSGI-TILOKDAT(1:6) TO DAGENS-DATUM                         
228000     MOVE TIDZ-MSGI-TILOKTID(1:4) TO DAGENS-TID(1:4)                      
228100     MOVE TIDZ-MSGI-TILOKDAT TO DAGENS-DATUM-LOCAL                        
228200     .                                                                    
228300     EJECT                                                                
228400 S01-UPDATE-EV-INLA01         SECTION.                                    
228500                                                                          
228600     PERFORM IMS-GU-INLA1-INLA11                                          
228700     PERFORM UNTIL SEGMENT-SAKNAS OR ART-FLFEL = JA                       
228800       PERFORM IMS-GNP-INLA1-INLA11                                       
228900     END-PERFORM                                                          
229000                                                                          
229100     IF ART-FLFEL              = NEJ                                      
229200       PERFORM IMS-GHU-INLA1-INLA01                                       
229300       MOVE NEJ                TO INL-FLFEL                               
229400       PERFORM IMS-REPL-INLA1-INLA01                                      
229500     END-IF                                                               
229600     .                                                                    
229700     EJECT                                                                
229800 S02-FLYTTA-EJ-NYCKEL-FAELT SECTION.                                      
229900                                                                          
230000     IF REQU-IDLBBET-UPD            NOT = ALL '+'                         
230100       MOVE REQU-IDLBBET-UPD        TO INL-IDLBBET                        
230200                                       RESP-IDLBBET                       
230300     END-IF                                                               
230400                                                                          
230500     IF REQU-TIANKDAG-UPD           NOT = ALL '+'                         
230600       MOVE REQU-TIANKDAG-UPD       TO INL-TIANKDAG                       
230700     ELSE                                                                 
230800       MOVE W-PLUS                  TO RESP-TIANKDAG-UPD                  
230900     END-IF                                                               
231000                                                                          
231100     IF REQU-IDFTG-UPD              NOT = ALL '+'                         
231200       MOVE REQU-IDFTG-UPD          TO INL-IDFTG                          
231300     ELSE                                                                 
231400       MOVE W-PLUS                  TO RESP-IDFTG-UPD                     
231500     END-IF                                                               
231600                                                                          
231700     IF REQU-IDKONTO-UPD            NOT = ALL '+'                         
231800       MOVE REQU-IDKONTO-UPD        TO INL-IDKONTO                        
231900     ELSE                                                                 
232000       MOVE W-PLUS                  TO RESP-IDKONTO-UPD                   
232100     END-IF                                                               
232200                                                                          
232300     IF REQU-IDANALYS-UPD           NOT = ALL '+'                         
232400       MOVE REQU-IDANALYS-UPD       TO INL-IDANALYS                       
232500     ELSE                                                                 
232600       MOVE W-PLUS                  TO RESP-IDANALYS-UPD                  
232700     END-IF                                                               
232800                                                                          
232900     IF REQU-IDKST-UPD              NOT = ALL '+'                         
233000       MOVE REQU-IDKST-UPD          TO INL-IDKST                          
233100     ELSE                                                                 
233200       MOVE W-PLUS                  TO RESP-IDKST-UPD                     
233300     END-IF                                                               
233400     .                                                                    
233500     EJECT                                                                
233600 S03-CONTROL-KDRT-IDLEVNR SECTION.                                        
233700                                                                          
233800     EVALUATE TRUE                                                        
233900       WHEN ART-KDRT           = 4                                        
234000         IF W-SPAR-IDLEVNR     = SPACE OR '1000 '                         
234100             CONTINUE                                                     
234200          ELSE                                                            
234300             MOVE NEJ          TO KDRT-SW                                 
234400         END-IF                                                           
234500                                                                          
234600       WHEN ART-KDRT           = 5                                        
234700         IF W-SPAR-IDLEVNR     = '1004 '                                  
234800            OR W-SPAR-IDLEVNR     = 'BP2TD'                               
234900             CONTINUE                                                     
235000          ELSE                                                            
235100             MOVE NEJ          TO KDRT-SW                                 
235200         END-IF                                                           
235300                                                                          
235400       WHEN ART-KDRT           = 6                                        
235500         IF W-SPAR-IDLEVNR     = SPACE OR '8265 ' OR '9999 '              
235600             CONTINUE                                                     
235700          ELSE                                                            
235800             MOVE NEJ          TO KDRT-SW                                 
235900         END-IF                                                           
236000                                                                          
236100       WHEN OTHER                                                         
236200             MOVE NEJ          TO KDRT-SW                                 
236300     END-EVALUATE                                                         
236400     .                                                                    
236500     EJECT                                                                
236600 S04-CONTROL-LEVPLAN      SECTION.                                        
236700                                                                          
236800     CONTINUE                                                             
236900     .                                                                    
237000     EJECT                                                                
237100 S05-CONTROL-OM-RAD-OK    SECTION.                                        
237200                                                                          
237300     MOVE JA                 TO RAD-OK-SW                                 
237400     MOVE ART-IDARTNR        TO W-IDARTNR                                 
237500                                                                          
237600     PERFORM IMS-GU-ARTC-ARTC11                                           
237700     IF NDC                                                               
237800       PERFORM IMS-GU-ARTS-ARTS11                                         
237900     END-IF                                                               
238000*                                                                         
238100     IF SEGMENT-SAKNAS                                                    
238200       MOVE NEJ                TO RAD-OK-SW                               
238300       MOVE 'IDARTNR'          TO RESP-IDELMT-ERROR                       
238400       MOVE ERR-MISSING-PART   TO RESP-IDMSG-ERROR                        
238500     ELSE                                                                 
238600       IF ART-PRARTSTD =  ZERO                                            
238700         MOVE NEJ              TO RAD-OK-SW                               
238800         MOVE ERR-PRICE-IS-MISSING           TO RESP-IDMSG-ERROR          
238900       ELSE                                                               
239000         MOVE JA TO PRIS-FINNS-SW                                         
239100         IF NDC                                                           
239200           PERFORM S05A-CONTROL-PRIS-FINNS                                
239300         END-IF                                                           
239400*NDC                                                                      
239500         IF NDC                                                           
239600         AND PRIS-SAKNAS                                                  
239700           MOVE NEJ             TO RAD-OK-SW                              
239800           MOVE ERR-PRICE-IS-MISSING         TO RESP-IDMSG-ERROR          
239900         ELSE                                                             
240000           MOVE ART-KVAVIS TO SPAR-KVAVIS                                 
240100           MOVE ZERO       TO SPAR-KVINLART                               
240200           PERFORM IMS-GNP-INLA1-INLA21                                   
240300                                                                          
240400           PERFORM UNTIL SEGMENT-SAKNAS                                   
240500             ADD RAD-KVINLART    TO SPAR-KVINLART                         
240600             PERFORM IMS-GNP-INLA1-INLA21                                 
240700           END-PERFORM                                                    
240800                                                                          
240900           IF SPAR-KVAVIS NOT = SPAR-KVINLART                             
241000             MOVE NEJ                 TO RAD-OK-SW                        
241100             MOVE ERR-WRONG-QUANTITY  TO RESP-IDMSG-ERROR                 
241200           ELSE                                                           
241300             IF SPAR-KVAVIS = +0                                          
241400               MOVE NEJ               TO RAD-OK-SW                        
241500               MOVE ERR-WRONG-QUANTITY     TO RESP-IDMSG-ERROR            
241600             END-IF                                                       
241700           END-IF                                                         
241800         END-IF                                                           
241900       END-IF                                                             
242000     END-IF                                                               
242100     .                                                                    
242200     EJECT                                                                
242300 S05A-CONTROL-PRIS-FINNS SECTION.                                         
242400                                                                          
242500*NYARE SÄTT ATT BERÄKNA PRIS-FINNS                                        
242600     MOVE DAGENS-DATUM-LOCAL    TO WS-IDAG                                
242700     MOVE WS-IDAG           TO DAT-I-TIDATUM                              
242800     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
242900     CALL WDATKONV USING       DAT-KDDATFORM                              
243000                               DAT-I-TIDATUM                              
243100                               DAT-O-TIDATUM                              
243200                               DAT-KDSVAR                                 
243300     IF DAT-KDSVAR-FEL                                                    
243400       MOVE 'FEL VID ANROP TILL DATKONV ' TO FELTEXT                      
243500       CALL FELLOG                                                        
243600     END-IF                                                               
243700                                                                          
243800     MOVE DAT-TISEKEL TO WS-DAGENS-SEKEL                                  
243900                                                                          
244000     MOVE WS-IDLEVNR    TO W-IDLEVNR                                      
244100                           W-IDLEVNR-21                                   
244200     MOVE NEJ TO PRIS-FINNS-SW                                            
244300                                                                          
244400     PERFORM IMS-GNP-ARTC-ARTC21                                          
244500     PERFORM UNTIL SEGMENT-SAKNAS OR PRIS-FINNS                           
244600      COMPUTE W-PRL-DADAT = 99999999 - ARTC21-PRL-DAPRLIST-9KOMPL         
244700       IF ARTC21-PRL-KDSTATUS-PR = +1 AND                                 
244800         W-PRL-DADAT <= WS-DAGENS-DATUM                                   
244900         MOVE JA TO PRIS-FINNS-SW                                         
245000       END-IF                                                             
245100       PERFORM IMS-GNP-ARTC-ARTC21                                        
245200     END-PERFORM                                                          
245300     .                                                                    
245400     EJECT                                                                
245500                                                                          
245600                                                                          
245700* --- IMS SEKTIONER ---                                                   
245800* --- IMS SEKTIONER ---                                                   
245900* --- IMS SEKTIONER ---                                                   
246000* --- IMS SEKTIONER ---                                                   
246100* --- IMS SEKTIONER ---                                                   
246200     SKIP3                                                                
246300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
246400                                                                          
246500     MOVE W-PLUS               TO RESP-TIAVIDAT-UPD                       
246600                                  RESP-IDLBBET-UPD                        
246700                                  RESP-TIANKDAG-UPD                       
246800                                  RESP-IDLEVNR-UPD                        
246900                                  RESP-FLBORT-UPD                         
247000                                  RESP-IDFTG-UPD                          
247100                                  RESP-IDKONTO-UPD                        
247200                                  RESP-IDANALYS-UPD                       
247300                                  RESP-IDKST-UPD                          
247400                                  RESP-IDARTNR-UPD                        
247500                                  RESP-KVAVIS-UPD                         
247600                                  RESP-KDRT-UPD                           
247700     .                                                                    
247800     SKIP2                                                                
247900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
248000                                                                          
248100     MOVE W-PLUS               TO RESP-TIAVIDAT                           
248200                                  RESP-IDLBBET                            
248300                                  RESP-TIANKDAG                           
248400                                  RESP-IDLEVNR                            
248500                                  RESP-FLBORT                             
248600                                  RESP-IDFTG                              
248700                                  RESP-IDKONTO                            
248800                                  RESP-IDANALYS                           
248900                                  RESP-IDKST                              
249000                                  RESP-IDRADNR-START                      
249100                                  RESP-IDRADNR-NEXT                       
249200                                  RESP-IDRADNR-INL-START                  
249300                                  RESP-IDRADNR-INL-NEXT                   
249400     MOVE +1                   TO INDX                                    
249500     PERFORM UNTIL INDX        >  MAX-INDX                                
249600         PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                 
249700         ADD +1                TO INDX                                    
249800     END-PERFORM                                                          
249900     .                                                                    
250000     SKIP2                                                                
250100 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
250200                                                                          
250300     MOVE W-PLUS       TO  RESP-KDCMD-LINE    (INDX)                      
250400                           RESP-IDARTNR-LINE  (INDX)                      
250500                           RESP-KVAVIS-LINE   (INDX)                      
250600                           RESP-KDRT-LINE     (INDX)                      
250700                           RESP-KVKOLLI-LINE  (INDX)                      
250800                           RESP-IDOKOLLI-LINE (INDX)                      
250900                           RESP-KVINLART-LINE (INDX)                      
251000                           RESP-IDARTNR-SPAR-LINE (INDX)                  
251100                           RESP-IDRADNR-INL-SPAR-LINE (INDX)              
251200                           RESP-IDRADNR-SPAR-LINE  (INDX)                 
251300     .                                                                    
251400     EJECT                                                                
251500 MFS-FORM-ATTR SECTION.                                                   
251600                                                                          
251700     MOVE MFS-FORMATETS-ATTR   TO RESP-TIAVIDAT-UPD-ATTR                  
251800                                  RESP-IDLBBET-UPD                        
251900                                  RESP-TIANKDAG-UPD-ATTR                  
252000                                  RESP-IDLEVNR-UPD-ATTR                   
252100                                  RESP-FLBORT-UPD-ATTR                    
252200                                  RESP-IDFTG-UPD-ATTR                     
252300                                  RESP-IDKONTO-UPD-ATTR                   
252400                                  RESP-IDANALYS-UPD-ATTR                  
252500                                  RESP-IDKST-UPD-ATTR                     
252600                                  RESP-IDARTNR-UPD-ATTR                   
252700                                  RESP-KVAVIS-UPD-ATTR                    
252800                                  RESP-KDRT-UPD-ATTR                      
252900                                                                          
253000     MOVE +1                   TO INDX                                    
253100     PERFORM UNTIL INDX        >  MAX-KVRADER                             
253200         PERFORM MFS-FORM-ATTR-RAD                                        
253300         ADD +1                TO INDX                                    
253400     END-PERFORM                                                          
253500     .                                                                    
253600     SKIP2                                                                
253700 MFS-FORM-ATTR-RAD    SECTION.                                            
253800                                                                          
253900     MOVE MFS-FORMATETS-ATTR   TO RESP-KDCMD-LINE-ATTR    (INDX)          
254000                                  RESP-IDARTNR-LINE-ATTR  (INDX)          
254100                                  RESP-KVAVIS-LINE-ATTR   (INDX)          
254200                                  RESP-KDRT-LINE-ATTR     (INDX)          
254300     .                                                                    
254400     SKIP2                                                                
254500 MFS-RENSA-FAELT-IN SECTION.                                              
254600                                                                          
254700*    --- ALLA INDATA-FÄLT                                                 
254800     MOVE W-SPACE              TO RESP-TIAVIDAT-UPD                       
254900                                  RESP-IDLBBET-UPD                        
255000                                  RESP-TIANKDAG-UPD                       
255100                                  RESP-IDLEVNR-UPD                        
255200                                  RESP-FLBORT-UPD                         
255300                                  RESP-IDFTG-UPD                          
255400                                  RESP-IDKONTO-UPD                        
255500                                  RESP-IDANALYS-UPD                       
255600                                  RESP-IDARTNR-UPD                        
255700                                  RESP-KVAVIS-UPD                         
255800                                  RESP-KDRT-UPD                           
255900                                                                          
256000     MOVE +1                   TO INDX                                    
256100     PERFORM UNTIL INDX        >  MAX-KVRADER                             
256200         MOVE MFS-RENSA-FAELT  TO RESP-KDCMD-LINE(INDX)                   
256300         ADD +1                TO INDX                                    
256400     END-PERFORM                                                          
256500     .                                                                    
256600     EJECT                                                                
256700 MFS-LAES-IN-IGEN SECTION.                                                
256800                                                                          
256900     MOVE MFS-ADD-LAES-IN-FAELT TO RESP-TIAVIDAT-UPD-ATTR                 
257000                                   RESP-IDLBBET-UPD-ATTR                  
257100                                   RESP-TIANKDAG-UPD-ATTR                 
257200                                   RESP-IDLEVNR-UPD-ATTR                  
257300                                   RESP-FLBORT-UPD-ATTR                   
257400                                   RESP-IDFTG-UPD-ATTR                    
257500                                   RESP-IDKONTO-UPD-ATTR                  
257600                                   RESP-IDANALYS-UPD-ATTR                 
257700                                   RESP-IDKST-UPD-ATTR                    
257800                                   RESP-IDARTNR-UPD-ATTR                  
257900                                   RESP-KVAVIS-UPD-ATTR                   
258000                                   RESP-KDRT-UPD-ATTR                     
258100     .                                                                    
258200     EJECT                                                                
258300 MFS-RENSA-FAELT-UT SECTION.                                              
258400                                                                          
258500     MOVE W-SPACE              TO RESP-TIAVIDAT                           
258600                                  RESP-IDLBBET                            
258700                                  RESP-TIANKDAG                           
258800                                  RESP-IDLEVNR                            
258900                                  RESP-FLBORT                             
259000                                  RESP-IDFTG                              
259100                                  RESP-IDKONTO                            
259200                                  RESP-IDANALYS                           
259300                                  RESP-IDKST                              
259400                                  RESP-IDRADNR-START                      
259500                                  RESP-IDRADNR-NEXT                       
259600                                  RESP-IDRADNR-INL-START                  
259700                                  RESP-IDRADNR-INL-NEXT                   
259800                                                                          
259900     MOVE +1                   TO INDX                                    
260000     PERFORM UNTIL INDX        >  MAX-INDX                                
260100         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
260200         ADD +1                TO INDX                                    
260300     END-PERFORM                                                          
260400     .                                                                    
260500     SKIP2                                                                
260600 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
260700                                                                          
260800     MOVE W-SPACE         TO RESP-KDCMD-LINE    (INDX)                    
260900                             RESP-IDARTNR-LINE  (INDX)                    
261000                             RESP-KVAVIS-LINE   (INDX)                    
261100                             RESP-KDRT-LINE     (INDX)                    
261200                             RESP-KVKOLLI-LINE  (INDX)                    
261300                             RESP-IDOKOLLI-LINE (INDX)                    
261400                             RESP-KVINLART-LINE (INDX)                    
261500                             RESP-IDARTNR-SPAR-LINE (INDX)                
261600                             RESP-IDRADNR-INL-SPAR-LINE (INDX)            
261700                             RESP-IDRADNR-SPAR-LINE  (INDX)               
261800     .                                                                    
261900     EJECT                                                                
262000 MFS-STAENG-FAELT-UPD-IN    SECTION.                                      
262100                                                                          
262200     MOVE MFS-STAENG-FAELT      TO RESP-TIAVIDAT-UPD-ATTR                 
262300                                   RESP-IDLBBET-UPD-ATTR                  
262400                                   RESP-TIANKDAG-UPD-ATTR                 
262500                                   RESP-IDLEVNR-UPD-ATTR                  
262600                                   RESP-FLBORT-UPD-ATTR                   
262700                                   RESP-IDFTG-UPD-ATTR                    
262800                                   RESP-IDKONTO-UPD-ATTR                  
262900                                   RESP-IDANALYS-UPD-ATTR                 
263000                                   RESP-IDKST-UPD-ATTR                    
263100                                   RESP-IDARTNR-UPD-ATTR                  
263200                                   RESP-KVAVIS-UPD-ATTR                   
263300                                   RESP-KDRT-UPD-ATTR                     
263400     MOVE +1 TO INDX                                                      
263500     PERFORM UNTIL INDX    >  MAX-KVRADER                                 
263600       MOVE MFS-STAENG-FAELT TO RESP-KDCMD-LINE-ATTR   (INDX)             
263700                                RESP-IDARTNR-LINE-ATTR (INDX)             
263800                                RESP-KVAVIS-LINE-ATTR  (INDX)             
263900                                RESP-KDRT-LINE-ATTR    (INDX)             
264000       MOVE ZERO          TO RESP-IDRADNR-INL-SPAR-LINE (INDX)            
264100                             RESP-IDARTNR-SPAR-LINE (INDX)                
264200       ADD +1                 TO INDX                                     
264300     END-PERFORM                                                          
264400     .                                                                    
264500     SKIP3                                                                
264600 MFS-STAENG-FAELT-RADER-IN   SECTION.                                     
264700                                                                          
264800     MOVE +1                   TO INDX                                    
264900     PERFORM UNTIL INDX            >  MAX-INDX                            
265000       IF REQU-IDRADNR-INL-SPAR-LINE(INDX) > ZERO                         
265100         CONTINUE                                                         
265200       ELSE                                                               
265300         MOVE MFS-STAENG-FAELT TO RESP-KDCMD-LINE-ATTR  (INDX)            
265400                                  RESP-IDARTNR-LINE-ATTR (INDX)           
265500                                  RESP-KVAVIS-LINE-ATTR (INDX)            
265600                                  RESP-KDRT-LINE-ATTR   (INDX)            
265700       END-IF                                                             
265800       ADD +1                    TO INDX                                  
265900     END-PERFORM                                                          
266000     .                                                                    
266100     EJECT                                                                
266200 IMS-GU-WDB601    SECTION.                                                
266300                                                                          
266400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
266500          DELIMITED BY SIZE INTO SSA1                                     
266600     MOVE '  GE' TO GODK-STATUSKODER                                      
266700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
266800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
266900     PERFORM IMS-STATUSKONTROLL                                           
267000     .                                                                    
267100     EJECT                                                                
267200 IMS-GU-INLB-INLB01       SECTION.                                        
267300                                                                          
267400     STRING 'W6INLB01(W6D1A1KY>=' W-W6D1A1KY-MIN-X                        
267500                    '&W6D1A1KY<=' W-W6D1A1KY-MAX-X ')'                    
267600          DELIMITED BY SIZE INTO SSA1                                     
267700     MOVE '  GE' TO GODK-STATUSKODER                                      
267800     CALL CBLTDLI USING GU W6INLB-PCB DLI-IO-W6D1A1 SSA1                  
267900     MOVE W6INLB-STATUS-CODE TO STATUS-WS                                 
268000     PERFORM IMS-STATUSKONTROLL                                           
268100     .                                                                    
268200     SKIP3                                                                
268300 IMS-GN-INLB-INLB01       SECTION.                                        
268400                                                                          
268500     STRING 'W6INLB01(W6D1A1KY>=' W-W6D1A1KY-MIN-X                        
268600                    '&W6D1A1KY<=' W-W6D1A1KY-MAX-X ')'                    
268700          DELIMITED BY SIZE INTO SSA1                                     
268800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
268900     CALL CBLTDLI USING GN W6INLB-PCB DLI-IO-W6D1A1 SSA1                  
269000     MOVE W6INLB-STATUS-CODE TO STATUS-WS                                 
269100     PERFORM IMS-STATUSKONTROLL                                           
269200     .                                                                    
269300     SKIP3                                                                
269400 IMS-GU-INLA1-INLA01    SECTION.                                          
269500                                                                          
269600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
269700          DELIMITED BY SIZE INTO SSA1                                     
269800     MOVE '  GE' TO GODK-STATUSKODER                                      
269900     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-W6D101 SSA1                   
270000     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
270100     PERFORM IMS-STATUSKONTROLL                                           
270200     .                                                                    
270300     EJECT                                                                
270400 IMS-GHU-INLA1-INLA01    SECTION.                                         
270500                                                                          
270600     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
270700          DELIMITED BY SIZE INTO SSA1                                     
270800     MOVE '  ' TO GODK-STATUSKODER                                        
270900     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-W6D101 SSA1                  
271000     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
271100     PERFORM IMS-STATUSKONTROLL                                           
271200     .                                                                    
271300     EJECT                                                                
271400 IMS-REPL-INLA1-INLA01    SECTION.                                        
271500                                                                          
271600     MOVE '  ' TO GODK-STATUSKODER                                        
271700     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-W6D101                      
271800     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
271900     PERFORM IMS-STATUSKONTROLL                                           
272000     .                                                                    
272100     EJECT                                                                
272200 IMS-GU-INLA1-INLA11   SECTION.                                           
272300                                                                          
272400     STRING 'W6INLA01*P(W6D101KY =' W-W6D101KY-X ')'                      
272500          DELIMITED BY SIZE INTO SSA1                                     
272600     MOVE 'W6INLA11' TO SSA2                                              
272700     MOVE '  GE' TO GODK-STATUSKODER                                      
272800     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-W6D111 SSA1 SSA2             
272900     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
273000     PERFORM IMS-STATUSKONTROLL                                           
273100     .                                                                    
273200     EJECT                                                                
273300 IMS-GHU-INLA1-INLA11   SECTION.                                          
273400                                                                          
273500     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
273600          DELIMITED BY SIZE INTO SSA1                                     
273700     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
273800          DELIMITED BY SIZE INTO SSA2                                     
273900     MOVE '  GE' TO GODK-STATUSKODER                                      
274000     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-W6D111 SSA1 SSA2             
274100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
274200     PERFORM IMS-STATUSKONTROLL                                           
274300     .                                                                    
274400     EJECT                                                                
274500 IMS-GNP-INLA1-INLA11-KVAL   SECTION.                                     
274600                                                                          
274700     STRING 'W6INLA11(IDRADNRI>=' W-IDRADNR-INL-X ')'                     
274800          DELIMITED BY SIZE INTO SSA1                                     
274900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
275000     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-W6D111 SSA1                  
275100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
275200     PERFORM IMS-STATUSKONTROLL                                           
275300     .                                                                    
275400     SKIP3                                                                
275500 IMS-GNP-INLA1-INLA11-KVAL-FST   SECTION.                                 
275600                                                                          
275700     STRING 'W6INLA11*F(IDRADNRI =' W-IDRADNR-INL-X ')'                   
275800          DELIMITED BY SIZE INTO SSA1                                     
275900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
276000     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-W6D111 SSA1                  
276100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
276200     PERFORM IMS-STATUSKONTROLL                                           
276300     .                                                                    
276400     SKIP3                                                                
276500 IMS-GHNP-INLA1-INLA11-KVAL   SECTION.                                    
276600                                                                          
276700     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
276800          DELIMITED BY SIZE INTO SSA1                                     
276900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
277000     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-W6D111 SSA1                 
277100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
277200     PERFORM IMS-STATUSKONTROLL                                           
277300     .                                                                    
277400     SKIP3                                                                
277500 IMS-GHNP-INLA1-INLA11-KVAL-FST   SECTION.                                
277600                                                                          
277700     STRING 'W6INLA11*F(IDRADNRI =' W-IDRADNR-INL-X ')'                   
277800          DELIMITED BY SIZE INTO SSA1                                     
277900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
278000     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-W6D111 SSA1                 
278100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
278200     PERFORM IMS-STATUSKONTROLL                                           
278300     .                                                                    
278400     SKIP3                                                                
278500 IMS-GHNP-INLA1-INLA11   SECTION.                                         
278600                                                                          
278700     MOVE 'W6INLA11' TO  SSA1                                             
278800     MOVE '  GEGB'   TO GODK-STATUSKODER                                  
278900     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-W6D111 SSA1                 
279000     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
279100     PERFORM IMS-STATUSKONTROLL                                           
279200     .                                                                    
279300     SKIP3                                                                
279400 IMS-REPL-INLA1-INLA11   SECTION.                                         
279500                                                                          
279600     MOVE '    ' TO GODK-STATUSKODER                                      
279700     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-W6D111                      
279800     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
279900     PERFORM IMS-STATUSKONTROLL                                           
280000     .                                                                    
280100     EJECT                                                                
280200 IMS-GNP-INLA1-INLA11   SECTION.                                          
280300                                                                          
280400     MOVE   'W6INLA11'         TO SSA1                                    
280500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
280600     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-W6D111 SSA1                  
280700     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
280800     PERFORM IMS-STATUSKONTROLL                                           
280900     .                                                                    
281000     EJECT                                                                
281100 IMS-GNP-INLA1-INLA21    SECTION.                                         
281200                                                                          
281300     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
281400          DELIMITED BY SIZE INTO SSA1                                     
281500     MOVE   'W6INLA21'         TO SSA2                                    
281600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
281700     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-W6D121 SSA1 SSA2             
281800     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
281900     PERFORM IMS-STATUSKONTROLL                                           
282000     .                                                                    
282100     SKIP3                                                                
282200 IMS-GHNP-INLA1-INLA21    SECTION.                                        
282300                                                                          
282400     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
282500          DELIMITED BY SIZE INTO SSA1                                     
282600     MOVE   'W6INLA21'         TO SSA2                                    
282700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
282800     CALL CBLTDLI USING GHNP INLA1-PCB DLI-IO-W6D121 SSA1 SSA2            
282900     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
283000     PERFORM IMS-STATUSKONTROLL                                           
283100     .                                                                    
283200     SKIP3                                                                
283300 IMS-DLET-INLA1-INLA01 SECTION.                                           
283400                                                                          
283500     MOVE '  ' TO GODK-STATUSKODER                                        
283600     CALL CBLTDLI USING DLET INLA1-PCB DLI-IO-W6D101                      
283700     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
283800     PERFORM IMS-STATUSKONTROLL                                           
283900     .                                                                    
284000     SKIP2                                                                
284100 IMS-DLET-INLA1-INLA11 SECTION.                                           
284200                                                                          
284300     MOVE '  ' TO GODK-STATUSKODER                                        
284400     CALL CBLTDLI USING DLET INLA1-PCB DLI-IO-W6D111                      
284500     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
284600     PERFORM IMS-STATUSKONTROLL                                           
284700     .                                                                    
284800     SKIP2                                                                
284900 IMS-DLET-INLA1-INLA21 SECTION.                                           
285000                                                                          
285100     MOVE '  ' TO GODK-STATUSKODER                                        
285200     CALL CBLTDLI USING DLET INLA1-PCB DLI-IO-W6D121                      
285300     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
285400     PERFORM IMS-STATUSKONTROLL                                           
285500     .                                                                    
285600     EJECT                                                                
285700 IMS-GU-INLA2-INLA01   SECTION.                                           
285800                                                                          
285900     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
286000          DELIMITED BY SIZE INTO SSA1                                     
286100     MOVE '  GE' TO GODK-STATUSKODER                                      
286200     CALL CBLTDLI USING GHU INLA2-PCB DLI-IO-W6D101 SSA1                  
286300     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
286400     PERFORM IMS-STATUSKONTROLL                                           
286500     .                                                                    
286600     EJECT                                                                
286700 IMS-GNP-INLA2-INLA11   SECTION.                                          
286800                                                                          
286900     MOVE   'W6INLA11'         TO SSA1                                    
287000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
287100     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-W6D111 SSA1                  
287200     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
287300     PERFORM IMS-STATUSKONTROLL                                           
287400     .                                                                    
287500     EJECT                                                                
287600 IMS-ISRT-INLA2-INLA01 SECTION.                                           
287700                                                                          
287800     MOVE 'W6INLA01'     TO SSA1                                          
287900     MOVE '  ' TO GODK-STATUSKODER                                        
288000     CALL CBLTDLI USING ISRT INLA2-PCB DLI-IO-W6D101 SSA1                 
288100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
288200     PERFORM IMS-STATUSKONTROLL                                           
288300     .                                                                    
288400     SKIP2                                                                
288500 IMS-ISRT-INLA2-INLA11 SECTION.                                           
288600                                                                          
288700     MOVE 'W6INLA11'     TO SSA1                                          
288800     MOVE '  ' TO GODK-STATUSKODER                                        
288900     CALL CBLTDLI USING ISRT INLA2-PCB DLI-IO-W6D111 SSA1                 
289000     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
289100     PERFORM IMS-STATUSKONTROLL                                           
289200     .                                                                    
289300     SKIP2                                                                
289400 IMS-ISRT-INLA2-INLA21 SECTION.                                           
289500                                                                          
289600     MOVE 'W6INLA21'     TO SSA1                                          
289700     MOVE '  ' TO GODK-STATUSKODER                                        
289800     CALL CBLTDLI USING ISRT INLA2-PCB DLI-IO-W6D121 SSA1                 
289900     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
290000     PERFORM IMS-STATUSKONTROLL                                           
290100     .                                                                    
290200     EJECT                                                                
290300 IMS-GU-LEVA01    SECTION.                                                
290400                                                                          
290500     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
290600          DELIMITED BY SIZE INTO SSA1                                     
290700     MOVE '  GE' TO GODK-STATUSKODER                                      
290800     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA2 SSA1                     
290900     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
291000     PERFORM IMS-STATUSKONTROLL                                           
291100     .                                                                    
291200     EJECT                                                                
291300 IMS-GU-ARTC-ARTC01 SECTION.                                              
291400                                                                          
291500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
291600          DELIMITED BY SIZE INTO SSA1                                     
291700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
291800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA3 SSA1                     
291900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
292000     PERFORM IMS-STATUSKONTROLL                                           
292100     .                                                                    
292200     SKIP3                                                                
292300 IMS-GNP-ARTC-ARTC11 SECTION.                                             
292400                                                                          
292500     MOVE 'WLARTC11 '      TO SSA1                                        
292600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
292700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA3 SSA1                    
292800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
292900     PERFORM IMS-STATUSKONTROLL                                           
293000     .                                                                    
293100     SKIP3                                                                
293200 IMS-GU-ARTC-ARTC11 SECTION.                                              
293300                                                                          
293400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
293500          DELIMITED BY SIZE INTO SSA1                                     
293600     MOVE 'WLARTC11 ' TO SSA2                                             
293700     MOVE '  GE' TO GODK-STATUSKODER                                      
293800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA3 SSA1 SSA2                
293900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
294000     PERFORM IMS-STATUSKONTROLL                                           
294100     .                                                                    
294200     EJECT                                                                
294300 IMS-GNP-ARTC-ARTC21 SECTION.                                             
294400                                                                          
294500     STRING 'WLARTC21(IDLEVNR  =' W-IDLEVNR-21-X ')'                      
294600          DELIMITED BY SIZE INTO SSA1                                     
294700     MOVE '  GE' TO GODK-STATUSKODER                                      
294800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-ARTC21 SSA1              
294900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
295000     PERFORM IMS-STATUSKONTROLL                                           
295100     .                                                                    
295200     SKIP3                                                                
295300 IMS-GU-ARTS-ARTS11 SECTION.                                              
295400                                                                          
295500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
295600          DELIMITED BY SIZE INTO SSA1                                     
295700     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
295800          DELIMITED BY SIZE INTO SSA2                                     
295900     MOVE '  GE' TO GODK-STATUSKODER                                      
296000     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-ARTS11 SSA1 SSA2          
296100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
296200     PERFORM IMS-STATUSKONTROLL                                           
296300     .                                                                    
296400     SKIP3                                                                
296500 IMS-GET-WDD902 SECTION.                                                  
296600                                                                          
296700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
296800          DELIMITED BY SIZE INTO SSA1                                     
296900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
297000          DELIMITED BY SIZE INTO SSA2                                     
297100     MOVE '  GE' TO GODK-STATUSKODER                                      
297200     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD9 SSA1 SSA2                 
297300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
297400     PERFORM IMS-STATUSKONTROLL                                           
297500     .                                                                    
297600     SKIP3                                                                
297700 IMS-GNP-WDD905 SECTION.                                                  
297800                                                                          
297900     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
298000          DELIMITED BY SIZE INTO SSA1                                     
298100     MOVE '  GE' TO GODK-STATUSKODER                                      
298200     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD9 SSA1                     
298300     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
298400     PERFORM IMS-STATUSKONTROLL                                           
298500     .                                                                    
298600     SKIP3                                                                
298700 IMS-STATUSKONTROLL SECTION.                                              
298800                                                                          
298900     SET STATUS-IX TO 1                                                   
299000     SEARCH GODK-STATUS                                                   
299100       AT END                                                             
299200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
299300         DELIMITED BY SIZE INTO FELTEXT                                   
299400         CALL FELLOG                                                      
299500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
299600         CONTINUE                                                         
299700     END-SEARCH                                                           
299800     .                                                                    
299900     EJECT                                                                
300000*    -COPY WY2000P9                                                       
300100     EJECT                                                                
300200*    -COPY WY2000P1                                                       
