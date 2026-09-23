000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W3017210.                                                
000500*AUTHOR.         THOMAS LARSSON.                                          
000600*DATE-WRITTEN.   92/05/19.                                                
000700*                12/10/XX SPLIT                                           
000800*    REMARKS. . .                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*            --- CORE TREATMENT, BYTES GODKÄNNANDE ---                    
001200*        SUBPROGRAM MED AFFÄRSLOGIK EFTER SPLIT AV GAMLA                  
001300*        W3017200 I TVÅ DELAR. KÖRS BÅDE FRÅN CLASSIC IMS        .        
001400*        OCH WEB.                                                         
001500*                                                                         
001600*        VISA VILKA OBJEKT SOM FINNS PÅ ETT RAPPORTNUMMER                 
001700*        SAMT GODKÄNNA OBJEKTEN ELLER JUSTERA PÅ BILDEN                   
001800*        SÅ ATT OBJEKTEN MOTSVARAR DET SOM FINNS I PALLEN.                
001900*        ETT RAPPORTNUMMER MOTSVARAR ETT KOLLI (PALL).                    
002000*                                                                         
002100*                                                                         
002200*        PROGRAMMET UPPDATERAR WLBYTF (WDM6)                              
002300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002400*        PROGRAMMET LÄSER      WDK6                                       
002500*        PROGRAMMET LÄSER      WDK7                                       
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W3T172                                              
002900*        MID:         W3I17201                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W3O17201                                            
003300*                                                                         
003400*    CHANGE LOG:                                                          
003500*                                                                         
003600*    DIGAMBAR/20020715                                                    
003700*    PUT IDBYTREP-9KOMPL IN WDM611                                        
003800*                                                                         
003900*    ETRACKER 1072007 060815/EÖ                                           
004000*    ADD SCRAP COMMAND                                                    
004100*                                                                         
004200*    ETRACKER 2816396 061004/EÖ                                           
004300*    AUTOMATIC UPDATING OF NEW CORES TO DC21, ET                          
004400*                                                                         
004500*    ETRACKER 2045675 061113/EÖ                                           
004600*    ADD INFO IFF LOCATION IS MISSING                                     
004700*                                                                         
004800*    ETRACKER 10206694 FEB 2015/RAHUL REDDY                               
004900*    PRINT CORE LABELS IN MAASTRICHT                                      
005000*                                                                         
005100*    ETRACKER 10251639 OCT 2015/RAHUL REDDY                               
005200*    ALLOW RETURN QUATITY UPTO 999                                        
005300*                                                                         
005400                                                                          
005500     SKIP3                                                                
005600 ENVIRONMENT DIVISION.                                                    
005700     EJECT                                                                
005800 DATA DIVISION.                                                           
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200 77  IDPGM                       PIC X(08)   VALUE 'W3017210'.            
006300                                                                          
006400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
006500 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
006600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
006700                                                                          
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000                                                                          
007100 01  ALL-SPACE.                                                           
007200     03  FILLER                  PIC X(80)  VALUE SPACE.                  
007300 01  ALL-SPACE-UTF8.                                                      
007400     03  FILLER                  PIC X(25)  VALUE ALL X'20'.              
007500 01  ALL-PLUS.                                                            
007600     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
007700 01  ALL-PLUS-UTF8.                                                       
007800     03  FILLER                  PIC X(25)  VALUE ALL X'2B'.              
007900                                                                          
008000 01  WS-IDSKYLT-SE               PIC X(3)   VALUE 'S  '.                  
008100 01  WS-IDSKYLT-GB               PIC X(3)   VALUE 'GB '.                  
008200 01  WS-IDSKYLT-CN               PIC X(3)   VALUE 'RCN'.                  
008300 01  WS-CP-UNICODE               PIC X(4)   VALUE 'UTF8'.                 
008400 01  WS-CP-EBCDIC                PIC X(3)   VALUE '278'.                  
008500                                                                          
008600 77  KDRC-DISPLAY                PIC Z(5).                                
008700                                                                          
008800 01  WS-CURRENT-DATE-TIME.                                                
008900     03  WS-YEAR                 PIC 9(4).                                
009000     03  WS-MONTH                PIC 9(2).                                
009100     03  WS-DAY                  PIC 9(2).                                
009200     03  WS-HOUR                 PIC 9(2).                                
009300     03  WS-MINUTE               PIC 9(2).                                
009400 01  FILLER REDEFINES WS-CURRENT-DATE-TIME.                               
009500     03  FILLER                  PIC X(2).                                
009600     03  WS-TIYYMMDDHHMM         PIC X(10).                               
009700                                                                          
009800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
009900 77  INDX                        PIC S9(5)  VALUE +0    COMP SYNC.        
010000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
010100                                                                          
010200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
010300                                                                          
010400 77  WS-RADNR                    PIC S9(1)   VALUE ZERO  COMP-3.          
010500 77  SPAR-RADNR                  PIC S9(1)   VALUE ZERO  COMP-3.          
010700                                                                          
010800 77  RED-INDX1                   PIC S9(1)   VALUE ZERO  COMP-3.          
010900 77  RED-INDX2                   PIC S9(1)   VALUE ZERO  COMP-3.          
011000                                                                          
011100 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
011200 77  WS-IDBYTRAP                 PIC X(7)    VALUE SPACE.                 
011300 77  WS-IDBYTRAD                 PIC S9(5)    VALUE ZERO.                 
011400 77  SPAR-IDBYTRAD               PIC S9(5)    VALUE ZERO.                 
011500 77  TEST-IDBYTRAD               PIC S9(5)    VALUE ZERO.                 
011600 77  VIPS-IDBYTRAD               PIC S9(5)    VALUE ZERO.                 
011700 77  WS-SPAR-IDBYTRAD            PIC S9(5)    VALUE ZERO.                 
011800 77  WS-SPAR-IDARTNR-OBJ         PIC S9(9)    VALUE ZERO COMP-3.          
011900 77  WS-FLBYTGAR                 PIC X        VALUE SPACE.                
012000                                                                          
012100 77  WS-KVRETUR-TOTURSP          PIC S9(4)   VALUE ZERO COMP-3.           
012200 77  WS-KVRETUR-TOTGODK          PIC S9(4)   VALUE ZERO COMP-3.           
012300 77  WS-KVRETUR                  PIC S9(7)   VALUE ZERO COMP-3.           
012400 77  WS-SPAR-KVRETUR             PIC S9(7)   VALUE ZERO COMP-3.           
012500 77  WS-SPAR-KVRETUR-TOT         PIC S9(7)   VALUE ZERO COMP-3.           
012600 77  WS-SPAR-OBJNR               PIC S9(9)   VALUE ZERO COMP-3.           
012700 77  WS-SPAR-OBJNR-SPAERR        PIC S9(9)   VALUE ZERO COMP-3.           
012800 77  WS-SPAR-KVRETUR-GODK        PIC 9(7)    VALUE ZERO.                  
012900 77  WS-SPAR-KVRETUR-URSP        PIC S9(7)   VALUE ZERO COMP-3.           
013000 77  WS-KONT-KVRETUR-GODK        PIC 9(7)    VALUE ZERO.                  
013100 77  WS-KONT-KVRETUR-URSP        PIC S9(7)   VALUE ZERO COMP-3.           
013200 77  WS-SPAR-KDBYTSTA-OBJ        PIC X       VALUE SPACE.                 
013300 77  SW-IDARTNR-OBJ              PIC S9(9)   VALUE ZERO COMP-3.           
013400 77  WS-ANMARK                   PIC X(3)    VALUE SPACE.                 
013410 77  WS-FLSKROT                  PIC X(1)    VALUE SPACE.                 
013500 77  WS-KVRETUR-GODK             PIC 9(3)    VALUE ZERO.                  
013600 77  WS-TEST-KVRETUR-GODK        PIC 9(7)    VALUE ZERO.                  
013700 77  WS-PRINT-ANTAL              PIC 9(7)    VALUE ZERO.                  
013800 77  W-9KOMPL                    PIC 9(07)   VALUE 9999999.               
013900 77  W-RAPP-IDBYTRAP             PIC 9(07)   VALUE ZERO.                  
013910 77  W-RAPP-IDDC                 PIC X(02)   VALUE SPACE.                 
014000                                                                          
014100 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
014200 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
014300 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
014400 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
014500 01  FILLER                  PIC X(16)   VALUE 'WS-DB2-SEKTION'.          
014600 01  WS-DB2-SEKTION              PIC X(30)   VALUE SPACE.                 
014700                                                                          
014800 01  WS-LOPNR                    PIC 9(5).                                
014900 01  XX-LOPNR REDEFINES WS-LOPNR.                                         
015000     03  WS-LOPNR-1              PIC 9(3).                                
015100     03  WS-LOPNR-2              PIC 9(2).                                
015200                                                                          
015300 01  WS-LOPNUMMER                PIC 9(5).                                
015400 01  XX-LOPNUMMER REDEFINES WS-LOPNUMMER.                                 
015500    03  WS-LOPNUMMER1            PIC 9(3).                                
015600    03  WS-LOPNUMMER2            PIC 9(2).                                
015700                                                                          
015800 01  WS-QTY-NEW                  PIC 9(7)    VALUE ZERO.                  
015900 01  FILLER REDEFINES WS-QTY-NEW.                                         
016000     03 FILLER                   PIC 9(4).                                
016100     03 WS-QTY-OK                PIC 9(3).                                
016200                                                                          
016300                                                                          
016400 01  WS9-IDARTNR                 PIC X(9)    VALUE ZERO.                  
016500 01  FILLER REDEFINES WS9-IDARTNR.                                        
016600     03 FILLER                   PIC  9(1).                               
016700     03 WS8-IDARTNR              PIC  9(8).                               
016800                                                                          
016900 01  WS-IDARTNR                  PIC  X(9)   VALUE ZERO.                  
017000 01  FILLER REDEFINES WS-IDARTNR.                                         
017100     03 FILLER                   PIC 9(5).                                
017200     03 WS-ARTSIFFRA             PIC 9(1).                                
017300        88 ART-0                 VALUE 6.                                 
017400        88 ART-1                 VALUE 4  7.                              
017500        88 ART-2                 VALUE 5  8.                              
017600        88 ART-3                 VALUE 9.                                 
017700     03 FILLER                   PIC  9(3).                               
017800                                                                          
017900 01  WS-IDARTNR-K7               PIC  9(9)   VALUE ZERO.                  
018000                                                                          
018100 01  RED-TAB1.                                                            
018200     03  WS-RED-TAB1 OCCURS 3 PIC X.                                      
018300                                                                          
018400 01  RED-TAB2.                                                            
018500     03  WS-RED-TAB2 OCCURS 3 PIC X.                                      
018600                                                                          
018700 01  KDBYTREF-TAB.                                                        
018800     03  WS-KDBYTREF-TAB OCCURS 300 PIC X(3).                             
018900                                                                          
019000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
019100     88  INDATA-OK                           VALUE 'J'.                   
019200     88  INDATA-FEL                          VALUE 'N'.                   
019300                                                                          
019400 77  KOLL-SW                     PIC X       VALUE 'J'.                   
019500     88  INMATNING-OK                        VALUE 'J'.                   
019600     88  INMATNING-EJ-OK                     VALUE 'N'.                   
019700                                                                          
019800 77  PF11-VARNING-SW             PIC X       VALUE 'J'.                   
019900     88  PF11-VARNING                        VALUE 'J'.                   
020000     88  PF11-VARNING-NEJ                    VALUE 'N'.                   
020100                                                                          
020200 77  BORTTAG-SW                  PIC X       VALUE 'J'.                   
020300     88  BORTTAG-OK                          VALUE 'J'.                   
020400     88  BORTTAG-EJ-OK                       VALUE 'N'.                   
020500                                                                          
020600 77  STATUS-SW                   PIC X       VALUE 'J'.                   
020700     88  STATUS-OK                           VALUE 'J'.                   
020800     88  STATUS-EJ-OK                        VALUE 'N'.                   
020900                                                                          
021000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
021100     88  NYCKLAR-OK                          VALUE 'J'.                   
021200     88  NYCKLAR-FEL                         VALUE 'N'.                   
021300                                                                          
021400 77  ARTIKEL-SW                  PIC X       VALUE 'J'.                   
021500     88  ARTIKEL-LIKA                        VALUE 'J'.                   
021600     88  ARTIKEL-OLIKA                       VALUE 'N'.                   
021700                                                                          
021800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
021900     88  ALLT-OK                             VALUE 'J'.                   
022000                                                                          
022100 77  TRAFF-SW                    PIC X       VALUE 'N'.                   
022200     88  TRAFF-OK                            VALUE 'J'.                   
022300     88  NO-TRAFF                            VALUE 'N'.                   
022400                                                                          
022500 77  FORSTA-SW                   PIC X       VALUE 'N'.                   
022600     88  FORSTA-TRAFF                        VALUE 'N'.                   
022700                                                                          
022800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
022900     88  EGEN-MID                            VALUE '3172'.                
023000     88  GODK-MID                            VALUE '3171' '3177'.         
023100     88  HELP-MID                            VALUE '0551'.                
023200     EJECT                                                                
023300*- - - - - - - - - - - - - - - - - - - - CORE LABEL LAYOUT                
023400 01  FILLER                      PIC X(16)   VALUE 'CORE LABEL'.          
023500 01  CORE-DATE.                                                           
023600     03  LBL-TIAAAA              PIC X(4).                                
023700     03  FILLER                  PIC X       VALUE '-'.                   
023800     03  LBL-TIMM                PIC X(2).                                
023900     03  FILLER                  PIC X       VALUE '-'.                   
024000     03  LBL-TIDD                PIC X(2).                                
024100                                                                          
024200 01  CORE-TIME.                                                           
024300     03  LBL-TIHH                PIC X(2).                                
024400     03  FILLER                  PIC X       VALUE ':'.                   
024500     03  LBL-TIMIN               PIC X(2).                                
024600                                                                          
024700*- - - - - - - - - - - - - - - - - - - - BYTES-ARTIKELTEST                
024800 01  FILLER                      PIC X(16)   VALUE 'BYTES-ART'.           
024900 01  TEST-IDARTNR                PIC 9(9)    COMP-3.                      
025000*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR                             
025100                                                                          
025200 01  TEST-KDBYTREF               PIC X(3).                                
025300*01  FILLER -COPY WWBYT15   -RED TEST-KDBYTREF                            
025400     EJECT                                                                
025500                                                                          
025600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
025700 01  GENERELLA-SUBPROGRAM.                                                
025800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
025900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
026000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
026100     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
026200     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
026300     03  WZ04CRUL                PIC X(8)    VALUE 'WZ04CRUL'.            
026400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
026500     EJECT                                                                
026600*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
026700 01  FILLER                      PIC X(8)    VALUE 'W005WDK7'.            
026800*   -COPY W005WDK7                                                        
026900     EJECT                                                                
027000*    --- PARAMETRAR TILL SUBPROGRAM WTRAUTF8                              
027100*                                                                         
027200 01  FILLER                 PIC X(16)   VALUE 'WTRAUTF8-AREA   '.         
027300*01 -COPY WTRAUTF8                                                        
027400     SKIP3                                                                
027500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
027600*01 -COPY WMSGINIT                                                        
027700                                                                          
027800 01  FILLER                      PIC X(16)   VALUE 'WZ04CRUL'.            
027900*01 -COPY WZ04CRUL                                                        
028000                                                                          
028100 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
028200*01 -COPY WZ01SEND                                                        
028300 01  FILLER                      PIC X(08)  VALUE 'W3017201'.             
028400 01  SEND-AREA-TO-CORE-LABEL.                                             
028500*03  -COPY W3017201                                                       
028600*                                                                         
028700                                                                          
028800 01  HDR-AREA.                                                            
028900*    03 -COPY WZ01REQU -PRE HDR-                                          
029000*    03 -COPY WZ04HDR                                                     
029100                                                                          
029200     EJECT                                                                
029300*    --- VALID IDDC CODES                                                 
029400*                                                                         
029500 01  FILLER                      PIC X(16)   VALUE 'IDDC CODES'.          
029600*01 -COPY WWDC99                                                          
029700*01 -COPY WWDCKONS                                                        
029800     SKIP3                                                                
029900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
030000                                                                          
030100*01  -COPY WMFSAREA                                                       
030200                                                                          
030300     EJECT                                                                
030400 01  MESSAGE-CODES.                                                       
030500*                                                                         
030600     03  ERR-CORR-HILITE-FLDS   PIC X(3)    VALUE '020'.                  
030700     03  INF-PRESS-PF11         PIC X(3)    VALUE '013'.                  
030800     03  UPDATE-NOT-ALLOWED     PIC X(3)    VALUE '007'.                  
030900     03  ERR-PF11-AND-NO-DATA   PIC X(3)    VALUE '014'.                  
031000     03  INF-PRINT-REQUESTED    PIC X(3)    VALUE '376'.                  
031100     03  WRONG-PRINTER          PIC X(3)    VALUE '347'.                  
031200     03  WRONG-CODE             PIC X(3)    VALUE '023'.                  
031300*                                                 +KDBYTREF               
031400     03  MISSING-PARTNO         PIC X(3)    VALUE '025'.                  
031500*                                                 +IDARTNR                
031600     03  INF-UPDATE-DONE        PIC X(3)    VALUE '001'.                  
031700     03  INF-FIRST-PAGE         PIC X(3)    VALUE '010'.                  
031800     03  INF-MORE-INFO-EXISTS   PIC X(3)    VALUE '011'.                  
031900     03  ERR-WRONG-KEY          PIC X(3)    VALUE '043'.                  
032000     03  ERR-NO-UPPDATE-DONE    PIC X(3)    VALUE '004'.                  
032100     03  QTY-NOT-SAME-ORIGIN    PIC X(3)    VALUE '387'.                  
032200     03  REPORT-NOT-REGISTERED  PIC X(3)    VALUE '389'.                  
032300     03  ERR-NOT-EXCHANGE-NO    PIC X(3)    VALUE '023'.                  
032400*                                                 +IDARTNR-OBJ            
032500     03  REPORT-WRONG-STATUS    PIC X(3)    VALUE '390'.                  
032600*                                                 +REPORT                 
032700     03  WRONG-STATUS           PIC X(3)    VALUE '390'.                  
032800*                                                 +LINE                   
032900     03  QTY-MORE-THAN-ALLOWED  PIC X(3)    VALUE '298'.                  
033000     03  QTY-NOT-LESS-ORIGIN    PIC X(3)    VALUE '393'.                  
033100     03  CORE-ALREADY-EXISTS    PIC X(3)    VALUE '388'.                  
033200     03  USER-NOT-ALLOWED       PIC X(3)    VALUE '00A'.                  
033300     03  OLD-VIPS-USER          PIC X(3)    VALUE '394'.                  
033400     03  INF-REP-NOT-COMP       PIC X(3)    VALUE '404'.                  
033500     EJECT                                                                
033600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
033700*                                                                         
033800     EJECT                                                                
033900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
034000     SKIP3                                                                
034100 01  NYCKLAR-TILL-DLI.                                                    
034200     03  W-WDM601KY-X.                                                    
034300         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
034400         05  W-IDBYTRAP          PIC S9(7)    VALUE ZERO COMP-3.          
034500                                                                          
034600     03  W-IDBYTRAD-X.                                                    
034700         05  W-IDBYTRAD          PIC S9(5)   VALUE ZERO COMP-3.           
034800                                                                          
034900     03  W-IDBYTRAD-MIN-X.                                                
035000         05  W-IDBYTRAD-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
035100                                                                          
035200     03  W-IDBYTRAD-MAX-X.                                                
035300         05  W-IDBYTRAD-MAX      PIC S9(5)   VALUE ZERO COMP-3.           
035400                                                                          
035500                                                                          
035600     03  W-WDM611KY-X.                                                    
035700         05  W-IDARTNR-OBJ       PIC S9(9)   VALUE ZERO COMP-3.           
035800         05  W-IDTABNR           PIC S9(3)   VALUE ZERO COMP-3.           
035900                                                                          
036000     03  W-WDGX30-X.                                                      
036100         05  FILLER              PIC X(4)    VALUE '3139'.                
036200         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
036300                                                                          
036400     03  W-WDGXKEY-LOW-X.                                                 
036500         05  W-IDARTNR-LOW-KVITT PIC S9(9)   VALUE ZERO COMP-3.           
036600         05  W-IDDISTR-LOW-KVITT PIC S9(5)   VALUE ZERO COMP-3.           
036700         05  W-IDTABNR-LOW-KVITT PIC S9(3)   VALUE ZERO COMP-3.           
036800                                                                          
036900     03  W-WDGXKEY-HIGH-X.                                                
037000       05  W-IDARTNR-HIGH-KVITT PIC S9(9)                                 
037100                                VALUE +999999999 COMP-3.                  
037200       05  W-IDDISTR-HIGH-KVITT PIC S9(5)   VALUE ZERO COMP-3.            
037300       05  W-IDTABNR-HIGH-KVITT PIC S9(3)   VALUE ZERO COMP-3.            
037400                                                                          
037500     03  W-IDTABNR-X.                                                     
037600         05  W-IDTABNR-KVITT     PIC S9(3)   VALUE ZERO COMP-3.           
037700                                                                          
037800     03  W-IDARTNR-KVITT-X.                                               
037900         05  W-IDARTNR-KVITT     PIC S9(9)   VALUE ZERO COMP-3.           
038000                                                                          
038100                                                                          
038200     03  W-IDARTNR-X.                                                     
038300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
038400                                                                          
038500     03  W-IDSKYLT-X.                                                     
038600         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
038700                                                                          
038800     03  W-IDDC-X.                                                        
038900         05 W-IDDC               PIC X(2)    VALUE SPACE.                 
039000                                                                          
039100     03  W-IDARTNR-K6-X.                                                  
039200         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
039300                                                                          
039400     03  W-KDSEGKEY-K6-X.                                                 
039500         05  W-KDSEGKEY-K6       PIC X(1)    VALUE '1'.                   
039600                                                                          
039700     03  W-IDARTNR-K7-X.                                                  
039800         05  W-IDARTNR-K7        PIC S9(9)  VALUE ZERO COMP-3.            
039900                                                                          
040000     03  W-IDDC-K7-X.                                                     
040100         05  W-IDDC-K7            PIC X(2).                               
040110     03  W-IDDC-B6-X.                                                     
040120         05 W-IDDC-B6                  PIC X(2).                          
040200                                                                          
040300 01  FILLER                  PIC X(16) VALUE 'IMS-WS STATUS-WS'.          
040400     SKIP2                                                                
040500*    --- STATUS-KOD FRÅN IMS                                              
040600 01  STATUS-WS                   PIC XX.                                  
040700     88  SEGMENT-FINNS                       VALUE '  '.                  
040800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
040900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041000     SKIP2                                                                
041100 01  GODK-STATUSKODER.                                                    
041200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
041300     SKIP3                                                                
041400                                                                          
041500 01  FILLER                  PIC X(08) VALUE 'SSA-AREA'.                  
041600 01  SSA1                        PIC X(156).                              
041700 01  SSA2                        PIC X(124).                              
041800     EJECT                                                                
041900*    --- IMS FUNKTIONSKODER                                               
042000*01  -COPY W0003                                                          
042100     EJECT                                                                
042200*    ---  DLI INPUT-OUTPUT AREA                                           
042300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
042400     SKIP3                                                                
042500 01  DLI-IO-AREA.                                                         
042600     03  IO-AREA                 PIC X(120)  VALUE SPACE.                 
042700     SKIP3                                                                
042800     03  WLBYTF01 REDEFINES IO-AREA.                                      
042900*        05  -COPY WDM601  -PRE BYTF-                                     
043000     EJECT                                                                
043100     03  WLBYTF11 REDEFINES IO-AREA.                                      
043200*        05  -COPY WDM611  -PRE BYTF-                                     
043300     EJECT                                                                
043400     03  WLBENA11 REDEFINES IO-AREA.                                      
043500*        05  -COPY WDD311  -PRE BENA-                                     
043600     EJECT                                                                
043700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA2'.          
043800 01  DLI-IO-AREA2.                                                        
043900     03  IO-AREA2                PIC X(100)  VALUE SPACE.                 
044000     SKIP3                                                                
044100     03  WLXXCP11 REDEFINES IO-AREA2.                                     
044200*        05  -COPY WDGX3140 -PRE KVITT-                                   
044300 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK601'.        
044400     SKIP3                                                                
044500 01  DLI-IO-WDK601.                                                       
044600*    03  -COPY WDK601                                                     
044700     SKIP3                                                                
044800 01  DLI-IO-WDK611.                                                       
044900*    03  -COPY WDK611                                                     
045000     EJECT                                                                
045100 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDM601'.        
045200 01  DLI-IO-AREA4.                                                        
045300     03  IO-AREA4                PIC X(150)  VALUE SPACE.                 
045400     03  WLBYTF01 REDEFINES IO-AREA4.                                     
045500*        05  -COPY WDM601  -PRE BYTF2-                                    
045600     EJECT                                                                
045700     03  WLBYTF11 REDEFINES IO-AREA4.                                     
045800*        05  -COPY WDM611  -PRE BYTF2-                                    
045900 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK711'.        
046000     SKIP3                                                                
046100 01  DLI-IO-WDK711.                                                       
046200*    03  -COPY WDK711                                                     
046300     EJECT                                                                
046310 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
046320 01   DLI-IO-AREA-B601.                                                   
046330*     03  -COPY WDB601                                                    
046340     EJECT                                                                
046400*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
046500*                                                                         
046600 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
046700 01  FILLER                  PIC X(16)   VALUE 'BYART-COPYTEXT'.          
046800*01  -COPY BYART -PRE BYART-                                              
046900     EJECT                                                                
047000 01  FILLER                  PIC X(16)   VALUE 'BYART-AREA'.              
047100       EXEC SQL INCLUDE BYART END-EXEC.                                   
047200     SKIP3                                                                
047300 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
047400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
047500*                        **** STATUS-KOD FRÅN DB2                         
047600 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
047700 01  DB2-WS.                                                              
047800   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
047900     88  RADER-FINNS                         VALUE 000.                   
048000     88  RADER-SAKNAS                        VALUE 100.                   
048100     88  904-KOD                             VALUE 904.                   
048200     SKIP1                                                                
048300   03  GODK-SQLCODESKODER.                                                
048400     05  GODK-SQLCODE OCCURS 5                                            
048500         INDEXED BY SQLCODE-IX PIC 999.                                   
048600     EJECT                                                                
048700     SKIP3                                                                
048800 LINKAGE SECTION.                                                         
048900                                                                          
049000 01  REQU-AREA.                                                           
049100*    03 -COPY WZ01REQU                                                    
049200*    03 -COPY W30172I1                                                    
049300                                                                          
049400     EJECT                                                                
049500 01  RESP-AREA.                                                           
049600*    03 -COPY WZ01RESP                                                    
049700*    03 -COPY W30172O1                                                    
049800                                                                          
049900 01  MAX-KVRADER                 PIC S9(4)  COMP.                         
050000                                                                          
050100*01  -COPY W0009  -PRE MSG-                                               
050200     EJECT                                                                
050300*01  -COPY W0009  -PRE DISTRDOC-                                          
050400     EJECT                                                                
050500*01  -COPY W0008  -PRE USEA-                                              
050600     05  FILLER                  PIC X.                                   
050700     EJECT                                                                
050800*01  -COPY W0008  -PRE BYTF-                                              
050900     05  FILLER                  PIC X.                                   
051000     EJECT                                                                
051100*01  -COPY W0008  -PRE WDR2-                                              
051200     05  FILLER                  PIC X.                                   
051300     EJECT                                                                
051400*01  -COPY W0008  -PRE BENA-                                              
051500     05  FILLER                  PIC X.                                   
051600*01  -COPY W0008  -PRE WDK6-                                              
051700     05  FILLER                  PIC X.                                   
051800     EJECT                                                                
051900*01  -COPY W0008  -PRE BYTF2-                                             
052000     05  FILLER                  PIC X.                                   
052100     EJECT                                                                
052200*01  -COPY W0008  -PRE WDK7-                                              
052300     05  FILLER                  PIC X.                                   
052400     EJECT                                                                
052500*01  -COPY W0008  -PRE WDB6-                                              
052600     05  FILLER                  PIC X.                                   
052700     EJECT                                                                
052800 PROCEDURE DIVISION USING REQU-AREA RESP-AREA MAX-KVRADER MSG-PCB         
052900             DISTRDOC-PCB USEA-PCB  BYTF-PCB  WDR2-PCB BENA-PCB           
053000                 WDK6-PCB BYTF2-PCB WDK7-PCB  WDB6-PCB.                   
053100                                                                          
053200                                                                          
053310     PERFORM A-INIT                                                       
053400     PERFORM B-KOLLA-NYCKLAR                                              
053500     IF NYCKLAR-OK                                                        
053600       IF REQU-UPDATE                                                     
053700         PERFORM MFS-FORM-ATTR                                            
053800         PERFORM G-KOLLA-INPUT                                            
053900         IF INDATA-OK                                                     
054000           IF PF11-VARNING-NEJ                                            
054100             PERFORM H-UPPDATERA                                          
054200           END-IF                                                         
054300         END-IF                                                           
054400       ELSE                                                               
054500         IF REQU-FIRST                                                    
054600          PERFORM C-FOERSTA-SIDA                                          
054700         ELSE                                                             
054800           IF REQU-NEXT                                                   
054900             PERFORM D-NAESTA-SIDA                                        
054901           ELSE                                                           
054902               IF REQU-PREVIOUS                                           
054904                 PERFORM I-PREV-PAGE                                      
055000               ELSE                                                       
055100                   PERFORM E-SAMMA-SIDA                                   
055200               END-IF                                                     
055210           END-IF                                                         
055300         END-IF                                                           
055400       END-IF                                                             
055500       IF ALLT-OK                                                         
055600         PERFORM F-LAES-VISA-INFO                                         
055700       END-IF                                                             
055800     END-IF                                                               
055900                                                                          
056000     MOVE ZERO TO RETURN-CODE                                             
056100     GOBACK                                                               
056200     .                                                                    
056300     EJECT                                                                
056400 A-INIT SECTION.                                                          
056500                                                                          
056600     MOVE 'A-INIT '    TO WS-SEKTION                                      
056700                                                                          
056800     MOVE ALL '+'                TO RESP-W30172O1                         
056900     PERFORM MFS-FORM-ATTR                                                
057000                                                                          
057100     IF REQU-IDMSGVER = '001'                                             
057200*      -- CALL FROM WEB                                                   
057300       MOVE 1 TO INDX                                                     
057400       PERFORM UNTIL INDX > MAX-KVRADER                                   
057500         MOVE ALL-PLUS-UTF8      TO RESP-BEART-LINE (INDX)                
057600         ADD 1 TO INDX                                                    
057700       END-PERFORM                                                        
057800     END-IF                                                               
057900                                                                          
058000     MOVE 001                    TO RESP-IDMSGVER                         
058100     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
058200                                    RESP-IDMSG-INFO                       
058300                                    RESP-IDMSG-VIPS                       
058400                                    RESP-IDELMT-ERROR                     
058500                                                                          
058600     MOVE REQU-IDTRANS-FROM      TO W-IDTRANS                             
058700     MOVE REQU-KVRADER           TO RESP-KVRADER                          
058800                                                                          
058900     EVALUATE REQU-IDSPRAK                                                
059000       WHEN 'SV'                                                          
059100        MOVE WS-IDSKYLT-SE       TO W-IDSKYLT                             
059200        MOVE WS-CP-EBCDIC        TO TRAUTF8-KDCP                          
059300                                                                          
059400       WHEN 'ZH'                                                          
059500        MOVE WS-IDSKYLT-CN       TO W-IDSKYLT                             
059600        MOVE WS-CP-UNICODE       TO TRAUTF8-KDCP                          
059700                                                                          
059800       WHEN OTHER                                                         
059900        MOVE WS-IDSKYLT-GB       TO W-IDSKYLT                             
060000        MOVE WS-CP-EBCDIC        TO TRAUTF8-KDCP                          
060100     END-EVALUATE                                                         
060200                                                                          
060300     MOVE ZERO                   TO WS-KVRETUR-TOTURSP                    
060400                                    WS-KVRETUR-TOTGODK                    
060500                                    WS-KVRETUR                            
060600                                    WS-KONT-KVRETUR-GODK                  
060700                                    WS-KONT-KVRETUR-URSP                  
060800                                    WS-LOPNUMMER                          
060900                                    WS-LOPNR                              
061000                                    SPAR-RADNR                            
061100                                    WS-RADNR                              
061200     MOVE ZERO                   TO VIPS-IDBYTRAD                         
061300     MOVE ZERO                   TO SPAR-IDBYTRAD                         
061310     IF REQU-IDBYTRAD-3173 = ALL '+'                                      
061320        MOVE ZERO TO REQU-IDBYTRAD-3173                                   
061330     END-IF                                                               
061340     IF REQU-IDBYTRAD-START = ALL '+' OR                                  
061341        REQU-IDBYTRAD-START NOT NUMERIC                                   
061342        MOVE ZERO TO REQU-IDBYTRAD-START                                  
061343     END-IF                                                               
061400     .                                                                    
061500     EJECT                                                                
061600 B-KOLLA-NYCKLAR SECTION.                                                 
061700     MOVE 'B-KOLLA-NYCKLAR'      TO WS-SEKTION                            
061800                                                                          
061900     MOVE ALL '+'                TO MSGI-WMSGINIT                         
062000     MOVE '001'                  TO MSGI-KDCALL                           
062100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
062200     MOVE '3172'                 TO MSGI-IDTRANS                          
062300     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
062400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062500                                                                          
062600     MOVE JA                     TO NYCKLAR-SW                            
062700     MOVE NEJ                    TO BORTTAG-SW                            
062800                                                                          
062900                                                                          
063000     MOVE REQU-IDDC-KEY          TO W-IDDC                                
063100                                    WS-IDDC                               
063200                                    W-IDDC-K7                             
063300                                    W-IDDC-B6                             
063400*    -- KONTROLL AV IDDISTR OCH IDBYTRAP                                  
063500                                                                          
063600     IF REQU-IDDISTR-KEY NUMERIC                                          
063700       IF REQU-IDDISTR-KEY > ZERO                                         
063800         MOVE REQU-IDDISTR-KEY   TO W-IDDISTR                             
063900                                    W-IDDISTR-LOW-KVITT                   
064000                                    W-IDDISTR-HIGH-KVITT                  
064100                                    WS-IDDISTR                            
064200       ELSE                                                               
064300         MOVE NEJ                TO NYCKLAR-SW                            
064400       END-IF                                                             
064500     ELSE                                                                 
064600       MOVE NEJ                  TO NYCKLAR-SW                            
064700     END-IF                                                               
064800                                                                          
064900     INSPECT REQU-IDBYTRAP-KEY REPLACING LEADING SPACE BY ZERO            
065000                                                                          
065100     IF REQU-IDBYTRAP-KEY NUMERIC                                         
065200       IF REQU-IDBYTRAP-KEY > ZERO                                        
065300         MOVE REQU-IDBYTRAP-KEY  TO WS-IDBYTRAP                           
065400                                    W-IDBYTRAP                            
065500       ELSE                                                               
065600         MOVE NEJ                TO NYCKLAR-SW                            
065700       END-IF                                                             
065800     ELSE                                                                 
065900       MOVE NEJ                  TO NYCKLAR-SW                            
066000     END-IF                                                               
066100                                                                          
066200     IF NYCKLAR-FEL                                                       
066300       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
066400       MOVE ZERO                 TO RESP-KVRADER                          
066500       PERFORM MFS-RENSA-FAELT-IN                                         
066600       PERFORM MFS-RENSA-FAELT-UT                                         
066700     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
067000 C-FOERSTA-SIDA SECTION.                                                  
067110     MOVE 'C-FOERSTA-SIDA'       TO WS-SEKTION                            
067200                                                                          
067300     MOVE INF-FIRST-PAGE         TO RESP-IDMSG-ERROR                      
067400                                                                          
067500*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
067600     MOVE ZERO                   TO W-IDBYTRAD                            
067700     MOVE JA                     TO ALLT-SW                               
067800                                                                          
067900     PERFORM MFS-RENSA-FAELT-IN                                           
068000                                                                          
068100*KA WHY?                                                                  
068200*    DO BELOW TO READ THE LINE INPUTS AGAIN                               
068300     IF REQU-IDBYTRAD-START > 0                                           
068400       PERFORM MFS-LAES-IN-IGEN                                           
068500     END-IF                                                               
068600                                                                          
068700     .                                                                    
068800     EJECT                                                                
068900 D-NAESTA-SIDA SECTION.                                                   
069100     MOVE 'D-NAESTA-SIDA'        TO WS-SEKTION                            
069200                                                                          
069220     MOVE REQU-IDBYTRAD-START    TO W-IDBYTRAD                            
069300     MOVE JA                     TO ALLT-SW                               
069400     PERFORM MFS-RENSA-FAELT-IN                                           
069500     .                                                                    
069600     EJECT                                                                
069700 E-SAMMA-SIDA SECTION.                                                    
069800     MOVE 'E-SAMMA-SIDA'         TO WS-SEKTION                            
069900                                                                          
070000     MOVE JA                     TO KOLL-SW                               
070100     MOVE +1                     TO INDX                                  
070200     PERFORM UNTIL INDX > REQU-KVRADER                                    
070300       IF REQU-KVRETUR-GODK-LINE(INDX) = ALL '+' AND                      
070400          REQU-KDCMD-LINE (INDX) = ALL '+' AND                            
070500          REQU-KDBYTREF-LINE (INDX) = ALL '+'                             
070600         MOVE JA                 TO KOLL-SW                               
070700         ADD +1                  TO INDX                                  
070800       ELSE                                                               
070900         MOVE NEJ                TO KOLL-SW                               
071000         MOVE +9999              TO INDX                                  
071100       END-IF                                                             
071200     END-PERFORM                                                          
071300                                                                          
071400     IF INMATNING-OK                                                      
071500       IF REQU-INPUT = ALL '+'                                            
071600       AND REQU-IDARTNR-OBJ-SPAERR = ALL '+'                              
071700         MOVE REQU-IDBYTRAD-START TO W-IDBYTRAD                           
071800         MOVE JA TO ALLT-SW                                               
071900         PERFORM MFS-RENSA-FAELT-IN                                       
072000       ELSE                                                               
072100* * * * INMATNING FINNS PÅ BILDENS REQU-INPUT * * * *                     
072200         IF EGEN-MID OR HELP-MID                                          
072300           MOVE NEJ TO ALLT-SW                                            
072400           MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                         
072500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
072600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
072700           PERFORM MFS-LAES-IN-IGEN                                       
072800           PERFORM EA-REQU-INDATA-TILL-MOD                                
072900         ELSE                                                             
073000           MOVE JA TO ALLT-SW                                             
073100           PERFORM MFS-LAES-IN-IGEN                                       
073200         END-IF                                                           
073300       END-IF                                                             
073400     ELSE                                                                 
073500* * * * INMATNING FINNS PÅ BILDENS REQU-RADINFO * * * *                   
073600       IF EGEN-MID OR HELP-MID                                            
073700         MOVE NEJ TO ALLT-SW                                              
073800         MOVE INF-PRESS-PF11 TO RESP-IDMSG-INFO                           
073900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
074000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
074100         PERFORM MFS-LAES-IN-IGEN                                         
074200         PERFORM EA-REQU-INDATA-TILL-MOD                                  
074300       ELSE                                                               
074400         MOVE JA TO ALLT-SW                                               
074500         PERFORM MFS-LAES-IN-IGEN                                         
074600       END-IF                                                             
074700     END-IF                                                               
074800     .                                                                    
074900     EJECT                                                                
075000 EA-REQU-INDATA-TILL-MOD SECTION.                                         
075100     MOVE 'EA-REQU-INDATA-TILL' TO WS-SEKTION                             
075200                                                                          
075300     MOVE +1 TO INDX                                                      
075400     PERFORM UNTIL INDX > REQU-KVRADER                                    
075500       IF REQU-KDCMD-LINE (INDX) NOT = ALL '+'                            
075600         MOVE REQU-KDCMD-LINE (INDX) TO RESP-KDCMD-LINE     (INDX)        
075700       END-IF                                                             
075800       IF REQU-KVRETUR-GODK-LINE(INDX) NOT = ALL '+'                      
075900         MOVE ALL-PLUS            TO RESP-KVRETUR-GODK-LINE (INDX)        
076000         MOVE MFS-ADD-LAES-IN-FAELT   TO                                  
076100                               RESP-KVRETUR-GODK-LINE-ATTR (INDX)         
076200*        MOVE REQU-KVRETUR-GODK-LINE (INDX)                               
076300*        TO RESP-KVRETUR-GODK-LINE (INDX)                                 
076400       END-IF                                                             
076500       IF REQU-KDBYTREF-LINE (INDX) NOT = ALL '+'                         
076600         MOVE REQU-KDBYTREF-LINE (INDX)                                   
076700           TO RESP-KDBYTREF-LINE (INDX)                                   
076800       END-IF                                                             
076900       IF REQU-IDBYTRAD-LINE (INDX) > ZERO                                
077000         MOVE REQU-IDBYTRAD-LINE (INDX)                                   
077100           TO RESP-IDBYTRAD-LINE (INDX)                                   
077200       END-IF                                                             
077300       ADD +1 TO INDX                                                     
077400     END-PERFORM                                                          
077500                                                                          
077600     IF W-IDTRANS = '3172'                                                
077700       IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                           
077800         INSPECT REQU-IDARTNR-OBJ-SPAERR                                  
077900                 REPLACING LEADING SPACE BY ZERO                          
078000         MOVE REQU-IDARTNR-OBJ-SPAERR TO RESP-IDARTNR-OBJ-SPAERR          
078100         IF REQU-IDBYTRAD-3173 > ZERO                                     
078200           MOVE REQU-IDBYTRAD-3173 TO RESP-IDBYTRAD-3173                  
078300           MOVE MFS-STAENG-FAELT TO RESP-IDARTNR-OBJ-SPAERR-ATTR          
078400         END-IF                                                           
078500       END-IF                                                             
078600     ELSE                                                                 
078700       IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                           
078800         INSPECT REQU-IDARTNR-OBJ-SPAERR                                  
078900                 REPLACING LEADING SPACE BY ZERO                          
079000         MOVE REQU-IDARTNR-OBJ-SPAERR TO RESP-IDARTNR-OBJ-SPAERR          
079100         MOVE REQU-IDBYTRAD-3173 TO RESP-IDBYTRAD-3173                    
079200       END-IF                                                             
079300       MOVE MFS-STAENG-FAELT  TO RESP-IDARTNR-OBJ-SPAERR-ATTR             
079400     END-IF                                                               
079500                                                                          
079600     IF REQU-KVRETUR-IN NOT = ALL '+'                                     
079700*      MOVE REQU-KVRETUR-IN TO RESP-KVRETUR-IN                            
079800       MOVE ALL-PLUS                TO RESP-KVRETUR-IN                    
079900       MOVE MFS-ADD-LAES-IN-FAELT   TO RESP-KVRETUR-IN-ATTR               
080000     END-IF                                                               
080100     IF REQU-KDBYTREF-IN NOT = ALL '+'                                    
080200       MOVE REQU-KDBYTREF-IN TO RESP-KDBYTREF-IN                          
080300     END-IF                                                               
080400     IF REQU-FLSKROT-IN NOT = ALL '+'                                     
080500       MOVE REQU-FLSKROT-IN TO RESP-FLSKROT-IN                            
080600     END-IF                                                               
080601     IF REQU-FLGODK-IN NOT = ALL '+'                                      
080602       MOVE REQU-FLGODK-IN TO RESP-FLGODK-IN                              
080603     END-IF                                                               
080700     .                                                                    
080800     EJECT                                                                
080900                                                                          
080901 I-PREV-PAGE SECTION.                                                     
080903     MOVE 'I-PREV-PAGE'          TO WS-SEKTION                            
080904                                                                          
080906     MOVE REQU-IDBYTRAD-START    TO W-IDBYTRAD                            
080907     MOVE JA                     TO ALLT-SW                               
080908     PERFORM MFS-RENSA-FAELT-IN                                           
080909     .                                                                    
080910     EJECT                                                                
081000 F-LAES-VISA-INFO SECTION.                                                
081110     MOVE 'F-LAES-VISA-INFO'   TO WS-SEKTION                              
081210     MOVE REQU-KDPRT             TO RESP-KDPRT                            
081300     MOVE ZERO                   TO RESP-KVRADER                          
081400     PERFORM FA-LAES-GRUNDDATA                                            
081500                                                                          
081600     IF SEGMENT-SAKNAS                                                    
081700        MOVE REPORT-NOT-REGISTERED  TO RESP-IDMSG-ERROR                   
081800        MOVE 'IDBYTRAP'             TO RESP-IDELMT-ERROR                  
081900        PERFORM MFS-RENSA-FAELT-UT                                        
082000     ELSE                                                                 
082100       MOVE +1 TO INDX                                                    
082200       PERFORM IMS-GNP-BYTF11                                             
082300       PERFORM FB-SKAPA-ENTER-NYCKLAR                                     
082400                                                                          
082500       PERFORM UNTIL INDX > MAX-KVRADER                                   
082600         IF SEGMENT-FINNS                                                 
082700           MOVE MFS-OEPPNA-ALFA-FAELT TO                                  
082800                                     RESP-KDCMD-LINE-ATTR   (INDX)        
082900           MOVE BYTF-OBJ-IDARTNR-OBJ                                      
083000           TO RESP-IDARTNR-OBJ-LINE  (INDX)                               
083100                                          WS-IDARTNR-K7                   
083200                                          W-IDARTNR-K6                    
083300           PERFORM IMS-GU-WDK611                                          
083400           IF SEGMENT-FINNS                                               
083500              MOVE CLAG-KVPOINT     TO RESP-KVPOINT-LINE (INDX)           
083600           ELSE                                                           
083700              MOVE ZERO             TO RESP-KVPOINT-LINE (INDX)           
083800           END-IF                                                         
083900                                                                          
084000           IF NDC-NA                                                      
084100              MOVE ZERO             TO RESP-IDTABNR-LINE (INDX)           
084200           ELSE                                                           
084300              MOVE BYTF-OBJ-IDTABNR TO RESP-IDTABNR-LINE (INDX)           
084400           END-IF                                                         
084500           IF BYTF-OBJ-KVRETUR-URSP > ZERO                                
084600            MOVE BYTF-OBJ-KVRETUR-URSP                                    
084700            TO RESP-KVRETUR-URSP-LINE (INDX)                              
084800           ELSE                                                           
084900              MOVE ZERO         TO RESP-KVRETUR-URSP-LINE (INDX)          
085000           END-IF                                                         
085100           MOVE BYTF-OBJ-IDBYTRAD     TO RESP-IDBYTRAD-LINE (INDX)        
085200                                                                          
085300           MOVE ZERO                   TO WS-IDBYTRAD                     
085400           MOVE BYTF-OBJ-IDBYTRAD      TO WS-IDBYTRAD                     
085500           IF WS-IDBYTRAD < 100                                           
085600             MOVE OLD-VIPS-USER   TO RESP-IDMSG-VIPS                      
085700           END-IF                                                         
085800                                                                          
085900                                                                          
086000           IF BYTF-OBJ-KVRETUR-GODK  > ZERO                               
086100             MOVE BYTF-OBJ-KVRETUR-GODK TO WS-QTY-NEW                     
086200             MOVE WS-QTY-OK       TO RESP-KVRETUR-GODK-LINE (INDX)        
086300           ELSE                                                           
086400             IF BYTF-OBJ-KDBYTSTA-OBJ = 'C'                               
086500               MOVE ZERO TO RESP-KVRETUR-GODK-LINE (INDX)                 
086600             ELSE                                                         
086700               MOVE ALL-SPACE                                             
086800               TO RESP-KVRETUR-GODK-LINE (INDX)                           
086900             END-IF                                                       
087000           END-IF                                                         
087100                                                                          
087200           MOVE BYTF-OBJ-BERADREF     TO RESP-BERADREF-LINE (INDX)        
087300           MOVE BYTF-OBJ-KDBYTSTA-OBJ  TO                                 
087400                                    RESP-KDBYTSTA-LINE      (INDX)        
087500           MOVE BYTF-OBJ-KDBYTREF   TO RESP-KDBYTREF-LINE (INDX)          
087600           IF BYTF-OBJ-FLSKROT = JA                                       
087700             IF W-IDSKYLT = 'GB' OR 'RCN'                                 
087800               MOVE 'Y'               TO RESP-FLSKROT-LINE  (INDX)        
087900             ELSE                                                         
088000               MOVE BYTF-OBJ-FLSKROT  TO RESP-FLSKROT-LINE  (INDX)        
088100             END-IF                                                       
088200           ELSE                                                           
088300             MOVE BYTF-OBJ-FLSKROT    TO RESP-FLSKROT-LINE  (INDX)        
088400           END-IF                                                         
088500                                                                          
088610           IF BYTF-OBJ-IDARTNR-OBJ = ZERO                                 
088700             MOVE BYTF-OBJ-IDTABNR TO W-IDTABNR-KVITT                     
088800             PERFORM IMS-GET-XXCP-ROT                                     
088900             IF SEGMENT-FINNS                                             
089000               PERFORM IMS-GET-XXCP-GNP                                   
089100               IF SEGMENT-FINNS                                           
089200                 MOVE KVITT-3140-IDARTNR-BYT TO WS8-IDARTNR               
089300                 MOVE WS9-IDARTNR            TO WS-IDARTNR                
089400                                                TEST-IDARTNR              
089500                 IF BYT16-RADIO                                           
089600                    MOVE 4                   TO WS-ARTSIFFRA              
089700                 ELSE                                                     
089800                    IF WS-ARTSIFFRA = 0                                   
089900                       MOVE 6                  TO WS-ARTSIFFRA            
090000                    ELSE                                                  
090100                      IF WS-ARTSIFFRA = 1                                 
090200                         MOVE 7                TO WS-ARTSIFFRA            
090300                      ELSE                                                
090400                         IF WS-ARTSIFFRA = 2                              
090500                            MOVE 8             TO WS-ARTSIFFRA            
090600                         ELSE                                             
090700                            MOVE 9             TO WS-ARTSIFFRA            
090800                         END-IF                                           
090900                      END-IF                                              
091000                    END-IF                                                
091100                 END-IF                                                   
091200                 MOVE WS-IDARTNR          TO W-IDARTNR                    
091210                 PERFORM IMS-GU-WDB601                                    
091220                                                                          
091230                 MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                  
091240                 IF DCS-UNICODE-IDSKYLT                                   
091250                    MOVE 'UTF8'             TO TRAUTF8-KDCP               
091260                 ELSE                                                     
091270                    MOVE '278 '             TO TRAUTF8-KDCP               
091280                 END-IF                                                   
091300                 PERFORM IMS-GET-BENA-GU                                  
091400                 IF SEGMENT-FINNS                                         
091500                    MOVE BENA-TEXT-BEART                                  
091600                      TO TRAUTF8-TECONV-FROM                              
091700                 ELSE                                                     
091800                    MOVE SPACE        TO TRAUTF8-TECONV-FROM              
091900                    MOVE WS-CP-EBCDIC TO TRAUTF8-KDCP                     
092000                 END-IF                                                   
092010                 IF TRAUTF8-TECONV-FROM = SPACES                          
092020                  MOVE 'GB'  TO W-IDSKYLT                                 
092030                  MOVE '278' TO TRAUTF8-KDCP                              
092040                  PERFORM IMS-GET-BENA-GU                                 
092050                  MOVE BENA-TEXT-BEART TO TRAUTF8-TECONV-FROM             
092060                 END-IF                                                   
092100               ELSE                                                       
092200                 MOVE SPACE        TO TRAUTF8-TECONV-FROM                 
092300                 MOVE WS-CP-EBCDIC TO TRAUTF8-KDCP                        
092400               END-IF                                                     
092500             ELSE                                                         
092600               MOVE SPACE        TO TRAUTF8-TECONV-FROM                   
092700               MOVE WS-CP-EBCDIC TO TRAUTF8-KDCP                          
092800             END-IF                                                       
092900             IF REQU-IDMSGVER = '101'                                     
093000               MOVE BENA-TEXT-BEART   TO RESP-BEART-LINE (INDX)           
093100             ELSE                                                         
093200               CALL WTRAUTF8 USING TRAUTF8-AREA                           
093300               MOVE TRAUTF8-TECONV-TO TO RESP-BEART-LINE (INDX)           
093400             END-IF                                                       
093500           ELSE                                                           
093600*                                                                         
093700* --- --- MAN KAN INTE SÖKA MED OBJEKTNR I KVITTNINGSTABELLEN             
093800* --- --- HÄR GÖRS OBJNUMMRET OM TILL BYTES ARTIKELNUMMER                 
093900* --- --- OBSERVERA SKILLNADEN MED KONVERTERINGEN OVAN                    
094000* --- --- DÄR MAN GÖR OM ARTNR TILL OBJEKTNR. VICE VERSA ALLTSÅ!!         
094100             MOVE BYTF-OBJ-IDARTNR-OBJ TO WS-IDARTNR                      
094200                                          TEST-IDARTNR                    
094300             IF BYT16-RADIO                                               
094400                MOVE 3                 TO WS-ARTSIFFRA                    
094500             ELSE                                                         
094600               IF WS-ARTSIFFRA = 6                                        
094700                  MOVE 0                 TO WS-ARTSIFFRA                  
094800               ELSE                                                       
094900                 IF WS-ARTSIFFRA = 7                                      
095000                    MOVE 1               TO WS-ARTSIFFRA                  
095100                 ELSE                                                     
095200                   IF WS-ARTSIFFRA = 8                                    
095300                      MOVE 2             TO WS-ARTSIFFRA                  
095400                   ELSE                                                   
095500                      MOVE 3             TO WS-ARTSIFFRA                  
095600                   END-IF                                                 
095700                 END-IF                                                   
095800               END-IF                                                     
095900             END-IF                                                       
096000* --- --- *                                                               
096100             MOVE WS-IDARTNR           TO W-IDARTNR-KVITT                 
096200             MOVE ZERO                 TO W-IDDISTR-LOW-KVITT             
096300                                          W-IDARTNR-LOW-KVITT             
096400                                          W-IDTABNR-LOW-KVITT             
096500                                                                          
096600             MOVE +999999999           TO W-IDARTNR-HIGH-KVITT            
096700             MOVE +99999               TO W-IDDISTR-HIGH-KVITT            
096800             MOVE +999                 TO W-IDTABNR-HIGH-KVITT            
096900             PERFORM IMS-GET-XXCP-ROT                                     
097000             IF SEGMENT-FINNS                                             
097100               PERFORM IMS-GET-XXCP-ARTNR                                 
097200               IF SEGMENT-FINNS                                           
097300                  IF NDC-NA                                               
097400                     MOVE ZERO   TO RESP-IDTABNR-LINE (INDX)              
097500                  ELSE                                                    
097600                     MOVE KVITT-3140-IDTABNR                              
097700                     TO RESP-IDTABNR-LINE (INDX)                          
097800                  END-IF                                                  
097900               ELSE                                                       
098000                 MOVE ZERO TO RESP-IDTABNR-LINE (INDX)                    
098100               END-IF                                                     
098200             END-IF                                                       
098300             PERFORM IMS-GU-WDB601                                        
098310                                                                          
098320             MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                      
098330             IF DCS-UNICODE-IDSKYLT                                       
098340                MOVE 'UTF8'             TO TRAUTF8-KDCP                   
098350             ELSE                                                         
098360                MOVE '278 '             TO TRAUTF8-KDCP                   
098370             END-IF                                                       
098400             MOVE BYTF-OBJ-IDARTNR-OBJ TO W-IDARTNR                       
098500             PERFORM IMS-GET-BENA-GU                                      
098600             IF SEGMENT-FINNS                                             
098700               MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM             
098800             ELSE                                                         
098900               MOVE SPACE              TO TRAUTF8-TECONV-FROM             
099000               MOVE WS-CP-EBCDIC       TO TRAUTF8-KDCP                    
099100             END-IF                                                       
099110             IF TRAUTF8-TECONV-FROM = SPACES                              
099120              MOVE 'GB'  TO W-IDSKYLT                                     
099130              MOVE '278' TO TRAUTF8-KDCP                                  
099140              PERFORM IMS-GET-BENA-GU                                     
099150              MOVE BENA-TEXT-BEART TO TRAUTF8-TECONV-FROM                 
099160             END-IF                                                       
099200             IF REQU-IDMSGVER = '101'                                     
099300               MOVE BENA-TEXT-BEART    TO RESP-BEART-LINE   (INDX)        
099400             ELSE                                                         
099500               CALL WTRAUTF8 USING TRAUTF8-AREA                           
099600               MOVE TRAUTF8-TECONV-TO  TO RESP-BEART-LINE   (INDX)        
099700             END-IF                                                       
099800           END-IF                                                         
099900                                                                          
100000           IF BYTF-OBJ-KDBYTREF = '032'                                   
100100*            MOVE SPACE                TO RESP-BEART-LINE (INDX)          
100200             IF W-IDSKYLT = 'S  '                                         
100300               MOVE 'ARTIKELNR SAKNAS' TO TRAUTF8-TECONV-FROM             
100400             ELSE                                                         
100500               MOVE 'MISSING PARTNO'   TO TRAUTF8-TECONV-FROM             
100600             END-IF                                                       
100700             MOVE WS-CP-EBCDIC         TO TRAUTF8-KDCP                    
100800             CALL WTRAUTF8 USING TRAUTF8-AREA                             
100900             MOVE TRAUTF8-TECONV-TO    TO RESP-BEART-LINE (INDX)          
101000                                                                          
101100             MOVE SPACE            TO RESP-FLAGGA-LOC-LINE (INDX)         
101200           ELSE                                                           
101210             IF BYTF-RAPP-IDDC = WC-CDC-SE OR WC-NDC-US-BAT OR            
101211                WC-NDC-TH-93                                              
101220               MOVE SPACE           TO RESP-FLAGGA-LOC-LINE (INDX)        
101230             ELSE                                                         
101300*NY ÄNDRING SCR 2045675 FLAGGA OM LOKALITET FATTAS                        
101400               MOVE WS-IDARTNR-K7   TO W-IDARTNR-K7                       
101500               MOVE WC-SDC-NL-ET    TO W-IDDC-K7                          
101600               PERFORM IMS-GHU-WDK711                                     
101700               IF SEGMENT-FINNS                                           
101800                 IF SLAG-ADLAGOMR > ZERO                                  
101900                   MOVE SPACE       TO RESP-FLAGGA-LOC-LINE (INDX)        
102000                 ELSE                                                     
102100                   IF W-IDSKYLT = 'S  '                                   
102200                     MOVE 'J'       TO RESP-FLAGGA-LOC-LINE (INDX)        
102300                   ELSE                                                   
102400                     MOVE 'Y'       TO RESP-FLAGGA-LOC-LINE (INDX)        
102500                   END-IF                                                 
102600                 END-IF                                                   
102700               ELSE                                                       
102800                 IF W-IDSKYLT = 'S  '                                     
102900                   MOVE 'J'         TO RESP-FLAGGA-LOC-LINE (INDX)        
103000                 ELSE                                                     
103100                   MOVE 'Y'         TO RESP-FLAGGA-LOC-LINE (INDX)        
103200                 END-IF                                                   
103300               END-IF                                                     
103310             END-IF                                                       
103400           END-IF                                                         
103500                                                                          
103600           IF RESP-KDBYTSTA-RAPP NOT = '3'                                
103700              MOVE MFS-STAENG-FAELT TO                                    
103800                                   RESP-KDCMD-LINE-ATTR    (INDX)         
103900                               RESP-KVRETUR-GODK-LINE-ATTR (INDX)         
104000                                   RESP-KDBYTREF-LINE-ATTR (INDX)         
104100                                                                          
104200           END-IF                                                         
104300           PERFORM IMS-GNP-BYTF11                                         
104400           ADD +1                TO RESP-KVRADER                          
104500         ELSE                                                             
104600           MOVE ALL-SPACE       TO RESP-KDCMD-LINE          (INDX)        
104700                                   RESP-IDARTNR-OBJ-LINE    (INDX)        
104800                                   RESP-KVPOINT-LINE        (INDX)        
104900                                   RESP-IDTABNR-LINE        (INDX)        
105000                                   RESP-KVRETUR-URSP-LINE   (INDX)        
105100                                   RESP-KVRETUR-GODK-LINE   (INDX)        
105200                                   RESP-BERADREF-LINE       (INDX)        
105300                                   RESP-KDBYTSTA-LINE       (INDX)        
105400                                   RESP-KDBYTREF-LINE       (INDX)        
105500                                   RESP-FLSKROT-LINE        (INDX)        
105600                                   RESP-BEART-LINE          (INDX)        
105700                                   RESP-IDBYTRAD-LINE       (INDX)        
105800                                   RESP-FLAGGA-LOC-LINE     (INDX)        
105900           MOVE MFS-STAENG-FAELT TO RESP-KDCMD-LINE-ATTR    (INDX)        
106000                               RESP-KVRETUR-GODK-LINE-ATTR  (INDX)        
106100                                    RESP-KDBYTREF-LINE-ATTR (INDX)        
106200         END-IF                                                           
106300         ADD 1 TO INDX                                                    
106400       END-PERFORM                                                        
106500                                                                          
106600       PERFORM FC-SKAPA-NEXT-NYCKLAR                                      
106700                                                                          
106800     END-IF                                                               
106900                                                                          
107000     MOVE ZERO TO W-IDBYTRAD                                              
107100     PERFORM IMS-GHU-BYTF01                                               
107200     IF SEGMENT-FINNS                                                     
107300       PERFORM IMS-GNP-BYTF11                                             
107400       PERFORM UNTIL SEGMENT-SAKNAS                                       
107500         IF SEGMENT-FINNS                                                 
107600           ADD BYTF-OBJ-KVRETUR-URSP TO WS-KVRETUR-TOTURSP                
107700           ADD BYTF-OBJ-KVRETUR-GODK TO WS-KVRETUR-TOTGODK                
107800         END-IF                                                           
107900         PERFORM IMS-GNP-BYTF11                                           
108000       END-PERFORM                                                        
108100       MOVE WS-KVRETUR-TOTURSP TO RESP-KVRETUR-TOTU                       
108200       MOVE WS-KVRETUR-TOTGODK TO RESP-KVRETUR-TOTG                       
108300     END-IF                                                               
108400     .                                                                    
108500     EJECT                                                                
108600 FA-LAES-GRUNDDATA SECTION.                                               
108700     MOVE 'FA-LAES-GRUNDDATA'   TO WS-SEKTION                             
108800                                                                          
108900     PERFORM IMS-GHU-BYTF01                                               
109000     IF SEGMENT-FINNS                                                     
109100       MOVE BYTF-RAPP-IDKUNDNR      TO RESP-IDKUNDNR                      
109200       MOVE BYTF-RAPP-IDFAKT        TO RESP-IDFAKT                        
109300       MOVE BYTF-RAPP-KDBYTSTA-RAPP TO RESP-KDBYTSTA-RAPP                 
109310       MOVE BYTF-RAPP-IDUSER        TO RESP-IDUSER-GODK                   
109400     END-IF                                                               
109500     .                                                                    
109600     EJECT                                                                
109700 FB-SKAPA-ENTER-NYCKLAR SECTION.                                          
109800     MOVE 'FB-SKAPA-ENTER-NYCKLAR'   TO WS-SEKTION                        
109900                                                                          
110000     IF SEGMENT-FINNS                                                     
110100        MOVE BYTF-OBJ-IDBYTRAD    TO RESP-IDBYTRAD-START                  
110200     ELSE                                                                 
110300        MOVE ZERO                 TO RESP-IDBYTRAD-START                  
110400     END-IF                                                               
110500     .                                                                    
110600     EJECT                                                                
110700 FC-SKAPA-NEXT-NYCKLAR SECTION.                                           
110800     MOVE 'FC-SKAPA-NEXT-NYCKLAR'   TO WS-SEKTION                         
110900                                                                          
111000     IF SEGMENT-FINNS                                                     
111100        MOVE BYTF-OBJ-IDBYTRAD    TO RESP-IDBYTRAD-NEXT                   
111200        IF NOT REQU-UPDATE                                                
111300           MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                   
111400        END-IF                                                            
111500     ELSE                                                                 
111600         MOVE RESP-IDBYTRAD-START TO RESP-IDBYTRAD-NEXT                   
111700     END-IF                                                               
111800     .                                                                    
111900     EJECT                                                                
112000 G-KOLLA-INPUT SECTION.                                                   
112110     MOVE 'G-KOLLA-INPUT'           TO WS-SEKTION                         
112200******************************************************************        
112300*** REQU-FLAGGA SÄTTS NÄR DU TRYCKER PF11 OCH UTRSPRUNGLIGT    ***        
112400*** ANTAL INTE STÄMMER ÖVERENS MED GODKÄNT ANTAL DÄREFTER GÅR  ***        
112500*** DET ATT TRYCK PF11 IGEN OCH DÅ GODKÄNNS RAPPORTEN          ***        
112600*** (DET GÄLLER ENDAST GAMLA VIPSANVÄNDARE )                   ***        
112700*** FÖLJANDE SWITCHAR ANVÄNDS PF11-VARNING-SW,                            
112800******************************************************************        
112900                                                                          
113000     MOVE JA  TO INDATA-SW                                                
113100                 STATUS-SW                                                
113200                 KOLL-SW                                                  
113300     MOVE NEJ TO PF11-VARNING-SW                                          
113400     MOVE SPACE TO KDBYTREF-TAB                                           
113500                                                                          
113600     MOVE +1 TO INDX                                                      
113700     PERFORM UNTIL INDX > REQU-KVRADER                                    
113800       IF REQU-IDBYTRAD-LINE (INDX) > ZERO AND                            
113810          REQU-IDBYTRAD-LINE (INDX) NUMERIC                               
113900          MOVE REQU-IDBYTRAD-LINE (INDX)                                  
114000            TO RESP-IDBYTRAD-LINE (INDX)                                  
114100          MOVE REQU-IDBYTRAD-LINE (INDX) TO WS-IDBYTRAD                   
114200          IF WS-IDBYTRAD < 100                                            
114300             MOVE OLD-VIPS-USER   TO RESP-IDMSG-VIPS                      
114400          END-IF                                                          
114410       ELSE                                                               
114420          MOVE ZERO TO REQU-IDBYTRAD-LINE (INDX)                          
114500       END-IF                                                             
114600       ADD +1 TO INDX                                                     
114700     END-PERFORM                                                          
114800                                                                          
114900     MOVE +1 TO INDX                                                      
115000     PERFORM UNTIL INDX > REQU-KVRADER                                    
115100                                                                          
115200       IF REQU-KVRETUR-GODK-LINE(INDX) = ALL '+' AND                      
115300          REQU-KDBYTREF-LINE (INDX) = ALL '+' AND                         
115400          REQU-KDCMD-LINE (INDX) = ALL '+'                                
115500          MOVE JA TO KOLL-SW                                              
115600          ADD +1 TO INDX                                                  
115700       ELSE                                                               
115800          MOVE NEJ TO KOLL-SW                                             
115900          ADD +9999 TO INDX                                               
116000       END-IF                                                             
116100     END-PERFORM                                                          
116200                                                                          
116300     IF REQU-INPUT = ALL '+'                                              
116400******************************************************************        
116500**** VALIDERA REQU-RADINFO                                                
116600******************************************************************        
116700       IF INMATNING-OK                                                    
116800         MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                    
116900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
117000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
117100         MOVE NEJ TO INDATA-SW                                            
117200       ELSE                                                               
117300         MOVE +1 TO INDX                                                  
117400         PERFORM UNTIL INDX > REQU-KVRADER                                
117500           IF REQU-KVRETUR-GODK-LINE(INDX) NOT = ALL '+' OR               
117600              REQU-KDBYTREF-LINE (INDX) NOT = ALL '+' OR                  
117700              REQU-KDCMD-LINE (INDX) NOT = ALL '+'                        
117800              PERFORM IMS-GHU-BYTF01                                      
117900              IF SEGMENT-FINNS                                            
118000                IF BYTF-RAPP-KDBYTSTA-RAPP = '3'                          
118100                   MOVE BYTF-RAPP-FLBYTGAR TO WS-FLBYTGAR                 
118200                  IF REQU-KDCMD-LINE (INDX) NOT = ALL '+'                 
118300******************************************************************        
118400**** VALIDERA REQU-KDCMD-LINE (INDX)                                      
118500******************************************************************        
118600                    IF REQU-KDCMD-LINE (INDX) = 'D'                       
118700                      MOVE REQU-IDBYTRAD-LINE (INDX) TO W-IDBYTRAD        
118800                      IF W-IDBYTRAD   NOT = ZERO                          
118900                        PERFORM IMS-GU-BYTF11                             
119000                        IF SEGMENT-FINNS                                  
119100                          IF BYTF-OBJ-KDBYTSTA-OBJ = 'N'                  
119200                          OR BYTF-OBJ-KDBYTSTA-OBJ = 'E'                  
119300                            MOVE MFS-ALFA-FAELT-RAETT TO                  
119400                                 RESP-KDCMD-LINE-ATTR (INDX)              
119500                          ELSE                                            
119600                            MOVE MFS-ALFA-FAELT-FEL TO                    
119700                                 RESP-KDCMD-LINE-ATTR (INDX)              
119800                            MOVE NEJ TO INDATA-SW                         
119900                            MOVE WRONG-STATUS TO RESP-IDMSG-INFO          
120000                            MOVE 'LINE'     TO RESP-IDELMT-ERROR          
120100                          END-IF                                          
120200                        ELSE                                              
120300                          MOVE MFS-ALFA-FAELT-FEL TO                      
120400                               RESP-KDCMD-LINE-ATTR (INDX)                
120500                          MOVE NEJ TO INDATA-SW                           
120600                        END-IF                                            
120700                      ELSE                                                
120800                        MOVE MFS-ALFA-FAELT-FEL TO                        
120900                             RESP-KDCMD-LINE-ATTR (INDX)                  
121000                        MOVE NEJ TO INDATA-SW                             
121100                        MOVE MISSING-PARTNO TO RESP-IDMSG-INFO            
121200                        MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR          
121300                      END-IF                                              
121400                    ELSE                                                  
121500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
121600* * * VALIDERING AV DIREKT GODKÄNNANDE                      * * *         
121700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
121800                      IF REQU-KDCMD-LINE (INDX) = 'G'                     
121900                      OR REQU-KDCMD-LINE (INDX) = 'A'                     
122000                         PERFORM GD-VALIDERA-DIREKT-GODKN                 
122100                      ELSE                                                
122200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
122300* * * VALIDERING AV SKROTNING                               * * *         
122400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
122500                        IF REQU-KDCMD-LINE (INDX) = 'S'                   
122600                          PERFORM GH-VALIDERA-SKROTNING                   
122700                        ELSE                                              
122800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
122900* * * VALIDERING AV BORTTAG SKROTNING                       * * *         
123000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
123100                          IF REQU-KDCMD-LINE (INDX) = 'C'                 
123200                            MOVE REQU-IDBYTRAD-LINE (INDX)                
123300                            TO W-IDBYTRAD                                 
123400                            IF W-IDBYTRAD   NOT = ZERO                    
123500                              PERFORM IMS-GU-BYTF11                       
123600                              IF SEGMENT-FINNS                            
123700                                IF BYTF-OBJ-FLSKROT = JA                  
123800                                  MOVE MFS-ALFA-FAELT-RAETT TO            
123900                                  RESP-KDCMD-LINE-ATTR (INDX)             
124000                                ELSE                                      
124100                                  MOVE MFS-ALFA-FAELT-FEL TO              
124200                                  RESP-KDCMD-LINE-ATTR (INDX)             
124300                                  MOVE NEJ TO INDATA-SW                   
124400                                  MOVE UPDATE-NOT-ALLOWED                 
124500                                  TO RESP-IDMSG-INFO                      
124600                                END-IF                                    
124700                              ELSE                                        
124800                                 MOVE MFS-ALFA-FAELT-FEL TO               
124900                                 RESP-KDCMD-LINE-ATTR (INDX)              
125000                                MOVE NEJ TO INDATA-SW                     
125100                              END-IF                                      
125200                            ELSE                                          
125300                              MOVE MFS-ALFA-FAELT-FEL TO                  
125400                               RESP-KDCMD-LINE-ATTR (INDX)                
125500                              MOVE NEJ TO INDATA-SW                       
125600                              MOVE MISSING-PARTNO                         
125700                                             TO RESP-IDMSG-INFO           
125800                              MOVE 'IDARTNR' TO RESP-IDELMT-ERROR         
125900                            END-IF                                        
126000                          ELSE                                            
126100                             MOVE MFS-ALFA-FAELT-FEL TO                   
126200                             RESP-KDCMD-LINE-ATTR (INDX)                  
126300                           MOVE NEJ TO INDATA-SW                          
126400                          END-IF                                          
126500                        END-IF                                            
126600                      END-IF                                              
126700                    END-IF                                                
126800                  END-IF                                                  
126900                                                                          
127000******************************************************************        
127100**** VALIDERA REQU-KVRETUR-GODK-LINE(INDX)                                
127200******************************************************************        
127300                                                                          
127400                  IF REQU-KVRETUR-GODK-LINE(INDX) NOT = ALL '+'           
127500                    MOVE REQU-IDBYTRAD-LINE (INDX) TO W-IDBYTRAD          
127600                     PERFORM IMS-GU-BYTF11                                
127700                     IF SEGMENT-FINNS                                     
127800                        IF BYTF-OBJ-IDARTNR-OBJ NOT = ZERO                
127900                          INSPECT REQU-KVRETUR-GODK-LINE(INDX)            
128000                          REPLACING LEADING SPACE BY ZERO                 
128100*                         PERFORM GG-RED-KVRETUR-GODK                     
128200                          IF REQU-KVRETUR-GODK-LINE(INDX) NUMERIC         
128300****************************************************************          
128400                             IF REQU-KVRETUR-GODK-LINE(INDX) > 999        
128500                                MOVE MFS-NUM-FAELT-FEL TO                 
128600                                RESP-KVRETUR-GODK-LINE-ATTR (INDX)        
128700                                MOVE NEJ TO INDATA-SW                     
128800                             ELSE                                         
128900****************************************************************          
129000                               MOVE MFS-NUM-FAELT-RAETT TO                
129100                                RESP-KVRETUR-GODK-LINE-ATTR (INDX)        
129200                             END-IF                                       
129300                          ELSE                                            
129400                            MOVE MFS-NUM-FAELT-FEL TO                     
129500                                RESP-KVRETUR-GODK-LINE-ATTR (INDX)        
129600                            MOVE NEJ TO INDATA-SW                         
129700                          END-IF                                          
129800                        ELSE                                              
129900                          IF BYTF-OBJ-IDBYTRAD < 100                      
130000***************************************************************           
130100***** GODKÄNNA TABELL FÖR GAMMAL VIPSANVÄNDARE ****************           
130200***** OM KVRETUR-GODK LIKA MED NOLL            ****************           
130300***************************************************************           
130400                            INSPECT REQU-KVRETUR-GODK-LINE(INDX)          
130500                             REPLACING LEADING SPACE BY ZERO              
130600*                            PERFORM GG-RED-KVRETUR-GODK                  
130700                             MOVE REQU-KVRETUR-GODK-LINE(INDX) TO         
130800                                  WS-TEST-KVRETUR-GODK                    
130900                             IF WS-TEST-KVRETUR-GODK = ZERO               
131000                                MOVE MFS-NUM-FAELT-RAETT TO               
131100                                RESP-KVRETUR-GODK-LINE-ATTR (INDX)        
131200                             ELSE                                         
131300                                MOVE MFS-NUM-FAELT-FEL TO                 
131400                                RESP-KVRETUR-GODK-LINE-ATTR (INDX)        
131500                                MOVE NEJ TO INDATA-SW                     
131600                             END-IF                                       
131700                          ELSE                                            
131800                             MOVE MFS-NUM-FAELT-FEL TO                    
131900                                RESP-KVRETUR-GODK-LINE-ATTR (INDX)        
132000                             MOVE NEJ TO INDATA-SW                        
132100                          END-IF                                          
132200                        END-IF                                            
132300                     ELSE                                                 
132400                        MOVE MFS-NUM-FAELT-FEL TO                         
132500                             RESP-KVRETUR-GODK-LINE-ATTR (INDX)           
132600                        MOVE NEJ TO INDATA-SW                             
132700                     END-IF                                               
132800                  END-IF                                                  
132900                                                                          
133000******************************************************************        
133100**** VALIDERA REQU-KDBYTREF-LINE (INDX)                                   
133200******************************************************************        
133300                                                                          
133400                  IF REQU-KDBYTREF-LINE (INDX) NOT = ALL '+'              
133500                     IF REQU-KDBYTREF-LINE (INDX) = ALL ' '               
133600                        MOVE SPACE     TO WS-ANMARK                       
133700                     ELSE                                                 
133800                       MOVE REQU-KDBYTREF-LINE (INDX) TO                  
133900                             WS-ANMARK                                    
134000                       MOVE WS-ANMARK  TO                                 
134100                             TEST-KDBYTREF                                
134200                     END-IF                                               
134300                     IF BYT15-KDBYTREF-OK                                 
134400                     OR BYT15-KDBYTREF-REMOVE                             
134500                     OR BYT15-KDBYTREF-GAR-SALD                           
134600                     OR BYT15-KDBYTREF-GAR-EJ-SALD                        
134700                     OR WS-ANMARK = SPACE                                 
134800                        PERFORM GE-VALIDERA-ANMARK                        
134900                     ELSE                                                 
135000                        MOVE MFS-ALFA-FAELT-FEL TO                        
135100                        RESP-KDBYTREF-LINE-ATTR (INDX)                    
135200                        MOVE WRONG-CODE TO  RESP-IDMSG-INFO               
135300                        MOVE 'KDBYTREF' TO  RESP-IDELMT-ERROR             
135400                        MOVE NEJ TO INDATA-SW                             
135500                     END-IF                                               
135600                  END-IF                                                  
135700                                                                          
135800                ELSE                                                      
135900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
136000* * * * * * *     FEL STATUSKOD PÅ BYTESRAPPORTEN   * * * * * * *         
136100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
136200                  IF REQU-KVRETUR-GODK-LINE(INDX) NOT = ALL '+'           
136300                    MOVE MFS-NUM-FAELT-FEL TO                             
136400                         RESP-KVRETUR-GODK-LINE-ATTR (INDX)               
136500                    MOVE NEJ TO INDATA-SW                                 
136600                  END-IF                                                  
136700                  IF REQU-KDCMD-LINE (INDX) NOT = ALL '+'                 
136800                    MOVE MFS-ALFA-FAELT-FEL TO                            
136900                         RESP-KDCMD-LINE-ATTR (INDX)                      
137000                    MOVE NEJ TO INDATA-SW                                 
137100                  END-IF                                                  
137200                  IF REQU-KDBYTREF-LINE (INDX) NOT = ALL '+'              
137300                    MOVE MFS-ALFA-FAELT-FEL TO                            
137400                         RESP-KDBYTREF-LINE-ATTR (INDX)                   
137500                    MOVE NEJ TO INDATA-SW                                 
137600                  END-IF                                                  
137700                  MOVE REPORT-WRONG-STATUS TO RESP-IDMSG-INFO             
137800                  MOVE 'REPORT'            TO RESP-IDELMT-ERROR           
137900                END-IF                                                    
138000              END-IF                                                      
138100           END-IF                                                         
138200           ADD +1 TO INDX                                                 
138300         END-PERFORM                                                      
138400                                                                          
138500******************************************************************        
138600**** VALIDERA REQU-IDARTNR-OBJ-SPAERR                                     
138700******************************************************************        
138800         IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                         
138900            IF INDATA-FEL                                                 
139000               MOVE MFS-ALFA-FAELT-RAETT TO                               
139100                         RESP-IDARTNR-OBJ-SPAERR-ATTR                     
139200            ELSE                                                          
139300               MOVE MFS-ALFA-FAELT-FEL TO                                 
139400                         RESP-IDARTNR-OBJ-SPAERR-ATTR                     
139500               MOVE NEJ TO INDATA-SW                                      
139600            END-IF                                                        
139700         END-IF                                                           
139800                                                                          
139900         IF INDATA-FEL                                                    
140000           MOVE NEJ TO ALLT-SW                                            
140100           MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                  
140200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
140300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
140400         END-IF                                                           
140500       END-IF                                                             
140600     ELSE                                                                 
140700******************************************************************        
140800**** VALIDERA RADEN LÄNGST NER PÅ SIDAN REQU-INPUT                        
140900******************************************************************        
141000       PERFORM GA-VALIDERA-REQU-INPUT                                     
141100     END-IF                                                               
141200     .                                                                    
141300     EJECT                                                                
141400 GA-VALIDERA-REQU-INPUT SECTION.                                          
141510     MOVE 'GA-VALIDERA-REQU-INPUT'   TO WS-SEKTION                        
141600                                                                          
141700     PERFORM IMS-GHU-BYTF01                                               
141800     IF SEGMENT-FINNS                                                     
141900        IF BYTF-RAPP-KDBYTSTA-RAPP = '3'                                  
142000           IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                       
142100             PERFORM GB-KOLL-OM-OBJNR                                     
142200           END-IF                                                         
142300                                                                          
142400           IF INDATA-OK                                                   
142500             IF REQU-KVRETUR-IN NOT = ALL '+'                             
142600                INSPECT REQU-KVRETUR-IN                                   
142700                REPLACING LEADING SPACE BY ZERO                           
142800               IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                   
142900                 IF REQU-KVRETUR-IN NUMERIC                               
143000                  IF REQU-IDARTNR-OBJ-SPAERR NUMERIC                      
143100                   IF REQU-IDBYTRAD-3173 > ZERO                           
143200                      MOVE REQU-IDBYTRAD-3173 TO W-IDBYTRAD               
143300                       PERFORM IMS-GHU-BYTF11                             
143400                       IF SEGMENT-FINNS                                   
143500                         MOVE REQU-KVRETUR-IN                             
143600                         TO WS-SPAR-KVRETUR-GODK                          
143700                         ADD BYTF-OBJ-KVRETUR-GODK TO                     
143800                           WS-SPAR-KVRETUR-GODK                           
143900                         IF WS-SPAR-KVRETUR-GODK > +999                   
144000                           MOVE MFS-NUM-FAELT-FEL TO                      
144100                                RESP-KVRETUR-IN-ATTR                      
144200                           MOVE MFS-ADD-LAES-IN-FAELT TO                  
144300                                RESP-IDARTNR-OBJ-SPAERR-ATTR              
144400                           MOVE NEJ TO INDATA-SW                          
144500                           MOVE QTY-MORE-THAN-ALLOWED                     
144600                                            TO RESP-IDMSG-INFO            
144700                         ELSE                                             
144800                           MOVE MFS-NUM-FAELT-RAETT TO                    
144900                                RESP-KVRETUR-IN-ATTR                      
145000                         END-IF                                           
145100                         PERFORM GF-VALIDERA-ANMARK                       
145200                       ELSE                                               
145300                         IF REQU-KVRETUR-IN NOT = ZERO                    
145400                           MOVE MFS-NUM-FAELT-RAETT TO                    
145500                                RESP-KVRETUR-IN-ATTR                      
145600                         ELSE                                             
145700                           MOVE MFS-NUM-FAELT-FEL TO                      
145800                                RESP-KVRETUR-IN-ATTR                      
145900                           MOVE NEJ TO INDATA-SW                          
146000                         END-IF                                           
146100                         PERFORM GF-VALIDERA-ANMARK                       
146200                       END-IF                                             
146300                   ELSE                                                   
146400****************************************************************          
146500**** NY OKOPPLAD RAD SKA MATAS IN I WDM611                    **          
146600**** VALIDERING AV ARTIKEL LADES IN FREDAGEN DEN 5/1          **          
146700****************************************************************          
146800                            MOVE REQU-IDARTNR-OBJ-SPAERR TO               
146900                                W-IDARTNR-K6                              
147000                      PERFORM IMS-GU-WDK6                                 
147100                      IF SEGMENT-FINNS                                    
147200                         IF REQU-KVRETUR-IN NOT = ZERO AND                
147210                            REQU-KVRETUR-IN <= +999                       
147300                            MOVE MFS-NUM-FAELT-RAETT TO                   
147400                                RESP-KVRETUR-IN-ATTR                      
147500                            MOVE MFS-ALFA-FAELT-RAETT TO                  
147600                                RESP-IDARTNR-OBJ-SPAERR-ATTR              
147700                         ELSE                                             
147800                            MOVE MFS-NUM-FAELT-FEL TO                     
147900                                RESP-KVRETUR-IN-ATTR                      
148000                            MOVE MFS-ALFA-FAELT-FEL TO                    
148100                                RESP-IDARTNR-OBJ-SPAERR-ATTR              
148200                            MOVE NEJ TO INDATA-SW                         
148300                         END-IF                                           
148400                      ELSE                                                
148500                         MOVE MFS-NUM-FAELT-FEL TO                        
148600                                RESP-KVRETUR-IN-ATTR                      
148700                         MOVE MFS-ALFA-FAELT-FEL TO                       
148800                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
148900                         MOVE NEJ TO INDATA-SW                            
149000                         MOVE MISSING-PARTNO TO RESP-IDMSG-INFO           
149100                         MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR         
149200                      END-IF                                              
149300                   END-IF                                                 
149400                  ELSE                                                    
149500                    MOVE MFS-NUM-FAELT-FEL TO RESP-KVRETUR-IN-ATTR        
149600                    MOVE MFS-ALFA-FAELT-FEL TO                            
149700                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
149800                    MOVE NEJ TO INDATA-SW                                 
149900                  END-IF                                                  
150000                 ELSE                                                     
150100                  MOVE MFS-NUM-FAELT-FEL TO RESP-KVRETUR-IN-ATTR          
150200                  MOVE MFS-ALFA-FAELT-FEL                                 
150300                  TO RESP-IDARTNR-OBJ-SPAERR-ATTR                         
150400                  MOVE NEJ TO INDATA-SW                                   
150500                 END-IF                                                   
150600               ELSE                                                       
150700                 MOVE MFS-NUM-FAELT-FEL TO RESP-KVRETUR-IN-ATTR           
150800                 MOVE MFS-ALFA-FAELT-FEL                                  
150900                 TO RESP-IDARTNR-OBJ-SPAERR-ATTR                          
151000                 MOVE NEJ TO INDATA-SW                                    
151100                 MOVE MISSING-PARTNO TO RESP-IDMSG-INFO                   
151200                 MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR                 
151300               END-IF                                                     
151400               IF INDATA-OK                                               
151500                 MOVE REQU-KDBYTREF-IN TO TEST-KDBYTREF                   
151600                 IF REQU-IDMSGVER = '101' AND                             
151700                    REQU-IDDC-KEY = WC-SDC-NL-ET AND                      
151800                    NOT BYT15-KDBYTREF-REMOVE AND                         
151900                    NOT BYT15-KDBYTREF-GAR-EJ-SALD AND                    
151910                    (REQU-FLSKROT-IN = '+' OR SPACE)                      
152000                   IF (REQU-KDPRT = ALL '+' OR                            
152100                       REQU-KDPRT = SPACE)                                
152200                     MOVE MFS-ALFA-FAELT-FEL                              
152300                                 TO RESP-KDPRT-ATTR                       
152400                     MOVE NEJ    TO INDATA-SW                             
152500                     MOVE WRONG-PRINTER                                   
152600                                 TO RESP-IDMSG-INFO                       
152700                   ELSE                                                   
152800                     PERFORM GI-KONTROLLERA-PRINTER                       
152900                     IF CRUL-KDRC = ZERO                                  
153000                       MOVE MFS-ALFA-FAELT-RAETT                          
153100                                 TO RESP-KDPRT-ATTR                       
153200                     ELSE                                                 
153300                       MOVE MFS-ALFA-FAELT-FEL                            
153400                                 TO RESP-KDPRT-ATTR                       
153500                       MOVE NEJ  TO INDATA-SW                             
153600                       MOVE WRONG-PRINTER                                 
153700                                 TO RESP-IDMSG-INFO                       
153800                     END-IF                                               
153900                   END-IF                                                 
154000                 END-IF                                                   
154100               END-IF                                                     
154200             END-IF                                                       
154300                                                                          
154500             IF REQU-KDBYTREF-IN NOT = ALL '+'                            
154600               PERFORM GF-VALIDERA-ANMARK                                 
154700               IF REQU-KVRETUR-IN NOT = ALL '+'                           
154800                 IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                 
154900                   MOVE MFS-ALFA-FAELT-RAETT TO                           
155000                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
155100                 ELSE                                                     
155200                   MOVE MFS-NUM-FAELT-FEL                                 
155300                   TO RESP-IDARTNR-OBJ-SPAERR-ATTR                        
155400                   MOVE MFS-NUM-FAELT-FEL TO RESP-KVRETUR-IN-ATTR         
155500                   MOVE NEJ TO INDATA-SW                                  
155600                   MOVE MISSING-PARTNO TO RESP-IDMSG-INFO                 
155700                   MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR               
155800                 END-IF                                                   
155900               ELSE                                                       
156000                 MOVE MFS-ALFA-FAELT-RAETT                                
156100                 TO RESP-KDBYTREF-IN-ATTR                                 
156200                 IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                 
156300                   MOVE MFS-ALFA-FAELT-RAETT TO                           
156400                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
156500                 ELSE                                                     
156600                   MOVE MFS-NUM-FAELT-FEL                                 
156700                   TO RESP-IDARTNR-OBJ-SPAERR-ATTR                        
156800                 END-IF                                                   
156900                 MOVE MFS-NUM-FAELT-FEL TO RESP-KVRETUR-IN-ATTR           
157000                 MOVE NEJ TO INDATA-SW                                    
157100               END-IF                                                     
157200             END-IF                                                       
157201                                                                          
157202             IF REQU-FLSKROT-IN NOT = ALL '+'                             
157203               PERFORM GJ-VALIDERA-SKROT                                  
157204               IF REQU-KVRETUR-IN NOT = ALL '+'                           
157205                 IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                 
157206                   MOVE MFS-ALFA-FAELT-RAETT TO                           
157207                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
157208                 ELSE                                                     
157209                   MOVE MFS-NUM-FAELT-FEL                                 
157210                   TO RESP-IDARTNR-OBJ-SPAERR-ATTR                        
157211                   MOVE MFS-NUM-FAELT-FEL TO RESP-KVRETUR-IN-ATTR         
157212                   MOVE NEJ TO INDATA-SW                                  
157213                   MOVE MISSING-PARTNO TO RESP-IDMSG-INFO                 
157214                   MOVE 'IDARTNR'      TO RESP-IDELMT-ERROR               
157215                 END-IF                                                   
157216               ELSE                                                       
157217**               MOVE MFS-ALFA-FAELT-RAETT                                
157218**               TO RESP-FLSKROT-IN-ATTR                                  
157219                 IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                 
157220                   MOVE MFS-ALFA-FAELT-RAETT TO                           
157221                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
157222                 ELSE                                                     
157223                   MOVE MFS-NUM-FAELT-FEL                                 
157224                   TO RESP-IDARTNR-OBJ-SPAERR-ATTR                        
157225                 END-IF                                                   
157226                 MOVE MFS-NUM-FAELT-FEL TO RESP-KVRETUR-IN-ATTR           
157227                 MOVE NEJ TO INDATA-SW                                    
157228               END-IF                                                     
157229             END-IF                                                       
157300           END-IF                                                         
157400                                                                          
157500           IF REQU-IDARTNR-OBJ-SPAERR = ALL '+' AND                       
157600              REQU-KVRETUR-IN = ALL '+' AND                               
157700              REQU-KDBYTREF-IN = ALL '+' AND                              
157710              REQU-FLSKROT-IN = ALL '+'                                   
157800                                                                          
157900* INGEN INPUT UTOM EV. FLGODK-IN = JA                                     
158000* FLAGGA "FLAGGA" ANVÄNDS FÖR ATT SKILJA PÅ 1:A OCH 2:A GÅNGEN            
158100* 1:A GÅNGEN ÄR "FLAGGA" INTE SATT - KONTROLLERA ATT DET ÄR               
158200* OK ATT GODKÄNNA RAPPORTEN.                                              
158300* 2:A GÅNGEN PF11/EXECUTE TRYCKS ÄR "FLAGGA" = JA OCH DÅ                  
158400* GODKÄNNS RAPPORTEN UTAN KONTROLL.                                       
158500              IF REQU-FLAGGA = NEJ OR REQU-FLAGGA = SPACE                 
158600              OR REQU-FLAGGA = '+'                                        
158700                                                                          
158800                IF REQU-FLGODK-IN NOT = ALL '+'                           
158900                  IF REQU-FLGODK-IN = 'J' OR 'Y'                          
159000* KONTROLLERA OM OK ATT GODKÄNNA RAPPORTEN                                
159100                    MOVE ZERO TO WS-SPAR-KVRETUR                          
159200                                 WS-SPAR-KVRETUR-URSP                     
159300                    MOVE ZERO TO W-IDBYTRAD                               
159400                    PERFORM IMS-GNP-BYTF11                                
159500                    IF SEGMENT-FINNS                                      
159600                       MOVE BYTF-OBJ-IDBYTRAD                             
159700                                  TO VIPS-IDBYTRAD                        
159800                    END-IF                                                
159900                    PERFORM UNTIL (SEGMENT-SAKNAS) OR                     
160000                            STATUS-EJ-OK                                  
160100                      IF SEGMENT-FINNS                                    
160200                        IF BYTF-OBJ-KDBYTSTA-OBJ =                        
160300                           '4' OR 'C' OR 'N' OR 'E'                       
160400                          ADD BYTF-OBJ-KVRETUR-GODK TO                    
160500                                    WS-SPAR-KVRETUR                       
160600                          ADD BYTF-OBJ-KVRETUR-URSP TO                    
160700                                    WS-SPAR-KVRETUR-URSP                  
160800                          IF BYTF-OBJ-KDBYTSTA-OBJ =                      
160900                           '4' OR 'C' OR 'N'                              
161000                             PERFORM GC-BERAK-URSP-KVRETUR                
161100                          END-IF                                          
161200                        ELSE                                              
161300                          MOVE NEJ TO STATUS-SW                           
161400                        END-IF                                            
161500                      END-IF                                              
161600                      PERFORM IMS-GNP-BYTF11                              
161700                      IF SEGMENT-FINNS                                    
161800                         MOVE BYTF-OBJ-IDBYTRAD                           
161900                                  TO VIPS-IDBYTRAD                        
162000                      END-IF                                              
162100                    END-PERFORM                                           
162200                    PERFORM GCC-KONT-URSP-KVRETUR                         
162300                    IF STATUS-OK                                          
162400                      MOVE MFS-ALFA-FAELT-RAETT TO                        
162500                           RESP-FLGODK-IN-ATTR                            
162600                      IF WS-SPAR-KVRETUR-URSP = WS-SPAR-KVRETUR           
162700                      OR VIPS-IDBYTRAD > 99                               
162800                        MOVE NEJ TO RESP-FLAGGA                           
162900                      ELSE                                                
163000* SÄTT FLAGGA FÖR ATT HOPPA ÖVER  KONTROLLER NÄSTA GÅNG                   
163100                        MOVE JA TO RESP-FLAGGA                            
163200                        MOVE JA TO PF11-VARNING-SW                        
163300                        MOVE NEJ TO INDATA-SW                             
163400                      END-IF                                              
163500                    ELSE                                                  
163600                      MOVE MFS-ALFA-FAELT-FEL TO                          
163700                           RESP-FLGODK-IN-ATTR                            
163800                      MOVE NEJ TO INDATA-SW                               
163900                    END-IF                                                
164000                  ELSE                                                    
164100*****************************************************************         
164200******** TAR TILLBAKA GODKÄNNANDE AV RAPPORTEN ******************         
164300*****************************************************************         
164400                    IF REQU-FLGODK-IN = 'N'                               
164500                      MOVE REPORT-WRONG-STATUS TO RESP-IDMSG-INFO         
164600                      MOVE 'REPORT'          TO RESP-IDELMT-ERROR         
164700                    END-IF                                                
164800                    MOVE MFS-ALFA-FAELT-FEL TO RESP-FLGODK-IN-ATTR        
164900                    MOVE NEJ TO INDATA-SW                                 
165000                  END-IF                                                  
165100                ELSE                                                      
165200                  MOVE MFS-ALFA-FAELT-FEL TO RESP-FLGODK-IN-ATTR          
165300                  MOVE NEJ TO INDATA-SW                                   
165400                END-IF                                                    
165500              ELSE                                                        
165600* SLÅ AV FLAGGAN SOM GÖR ATT KONTROLL SKIPPAS                             
165700                IF REQU-FLAGGA = JA                                       
165800                  MOVE NEJ TO RESP-FLAGGA                                 
165900                              PF11-VARNING-SW                             
166000                END-IF                                                    
166100              END-IF                                                      
166200           ELSE                                                           
166300             IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+' AND                 
166400                REQU-KVRETUR-IN = ALL '+' AND                             
166500                REQU-KDBYTREF-IN = ALL '+' AND                            
166510                REQU-FLSKROT-IN = ALL '+'                                 
166600                MOVE MFS-ALFA-FAELT-FEL TO                                
166700                                    RESP-IDARTNR-OBJ-SPAERR-ATTR          
166800                MOVE NEJ TO INDATA-SW                                     
166900                IF REQU-FLGODK-IN NOT = ALL '+'                           
167000                   MOVE MFS-ALFA-FAELT-FEL TO RESP-FLGODK-IN-ATTR         
167100                END-IF                                                    
167200             ELSE                                                         
167300               IF REQU-FLGODK-IN NOT = ALL '+'                            
167400                 MOVE MFS-ALFA-FAELT-FEL TO RESP-FLGODK-IN-ATTR           
167500                 MOVE NEJ TO INDATA-SW                                    
167600               END-IF                                                     
167700             END-IF                                                       
167800           END-IF                                                         
167900        ELSE                                                              
168000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
168100* * * * * * *   FEL STATUSKOD PÅ BYTESOBJEKT      * * * * * * *           
168200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
168310          IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                        
168400             MOVE MFS-NUM-FAELT-FEL                                       
168500             TO RESP-IDARTNR-OBJ-SPAERR-ATTR                              
168600             MOVE NEJ TO INDATA-SW                                        
168700          END-IF                                                          
168800          IF REQU-KVRETUR-IN NOT = ALL '+'                                
168900             MOVE MFS-NUM-FAELT-FEL TO RESP-KVRETUR-IN-ATTR               
169000             MOVE NEJ TO INDATA-SW                                        
169100          END-IF                                                          
169200          IF REQU-KDBYTREF-IN NOT = ALL '+'                               
169300             MOVE MFS-ALFA-FAELT-FEL TO RESP-KDBYTREF-IN-ATTR             
169400             MOVE NEJ TO INDATA-SW                                        
169500          END-IF                                                          
169501          IF REQU-FLSKROT-IN NOT = ALL '+'                                
169502             MOVE MFS-ALFA-FAELT-FEL TO RESP-FLSKROT-IN-ATTR              
169503             MOVE NEJ TO INDATA-SW                                        
169504          END-IF                                                          
169600                                                                          
169700          MOVE REPORT-WRONG-STATUS TO RESP-IDMSG-INFO                     
169800          MOVE 'REPORT'            TO RESP-IDELMT-ERROR                   
169900          IF BYTF-RAPP-KDBYTSTA-RAPP = '4'                                
170000***** ÄNDRAT FÖR ATT KUNNA TA TILLBAKA ETT GODKÄNNADE 960617******        
170100             MOVE SPACE      TO RESP-IDMSG-INFO                           
170200***          MOVE MFS-ALFA-FAELT-FEL TO RESP-FLGODK-IN-ATTR               
170300***          MOVE NEJ TO INDATA-SW                                        
170400          ELSE                                                            
170500            IF REQU-FLGODK-IN NOT = ALL '+'                               
170600               MOVE MFS-ALFA-FAELT-FEL TO RESP-FLGODK-IN-ATTR             
170700               MOVE NEJ TO INDATA-SW                                      
170800            END-IF                                                        
170900          END-IF                                                          
171000        END-IF                                                            
171100     END-IF                                                               
171200     IF INDATA-FEL                                                        
171300       IF PF11-VARNING                                                    
171400          MOVE NEJ TO INDATA-SW                                           
171500          MOVE QTY-NOT-SAME-ORIGIN  TO RESP-IDMSG-INFO                    
171600          PERFORM MFS-ROER-EJ-FAELT-UT                                    
171700          PERFORM MFS-ROER-EJ-FAELT-IN                                    
171800       ELSE                                                               
171900          MOVE NEJ TO ALLT-SW                                             
172000          MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                   
172100          PERFORM MFS-ROER-EJ-FAELT-UT                                    
172200          PERFORM MFS-ROER-EJ-FAELT-IN                                    
172300          IF SPAR-RADNR > ZERO                                            
172400             MOVE MFS-ADD-SAETT-CURSOR TO                                 
172500                          RESP-KDCMD-LINE-ATTR (SPAR-RADNR)               
172600          END-IF                                                          
172700          IF STATUS-EJ-OK                                                 
172800            MOVE INF-REP-NOT-COMP    TO RESP-IDMSG-INFO                   
172900          END-IF                                                          
173000       END-IF                                                             
173100     END-IF                                                               
173200                                                                          
173300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
173400* * *  KOMMER FRÅN TRANSEN 3173 OCH DÅ SKRIV SKYDDAS FÄLTET * * *         
173500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
173600     IF REQU-IDBYTRAD-3173 > ZERO                                         
173700        MOVE REQU-IDBYTRAD-3173 TO RESP-IDBYTRAD-3173                     
173800        MOVE MFS-STAENG-FAELT  TO RESP-IDARTNR-OBJ-SPAERR-ATTR            
173900     END-IF                                                               
174000     .                                                                    
174100     EJECT                                                                
174200                                                                          
174300 GB-KOLL-OM-OBJNR SECTION.                                                
174410     MOVE 'GB-KOLL-OM-OBJNR'         TO WS-SEKTION                        
174500                                                                          
174600     INSPECT REQU-IDARTNR-OBJ-SPAERR                                      
174700     REPLACING LEADING SPACE BY ZERO                                      
174800     MOVE REQU-IDARTNR-OBJ-SPAERR TO WS-IDARTNR                           
174900     IF WS-IDARTNR NUMERIC                                                
175000       MOVE WS-IDARTNR TO TEST-IDARTNR                                    
175100       IF BYT16-BYTES                                                     
175200          IF WS-ARTSIFFRA > 5                                             
175300             CONTINUE                                                     
175400* --- ---* ARTIKELN HAR ETT GODKÄNT BRA OBJEKTNUMMER                      
175500             MOVE MFS-NUM-FAELT-RAETT                                     
175600             TO RESP-IDARTNR-OBJ-SPAERR-ATTR                              
175700          ELSE                                                            
175800             MOVE ERR-NOT-EXCHANGE-NO TO RESP-IDMSG-INFO                  
175900             MOVE 'IDARTNR-OBJ'       TO RESP-IDELMT-ERROR                
176000             MOVE ALL-PLUS          TO RESP-IDARTNR-OBJ-SPAERR            
176100                                        RESP-KVRETUR-IN                   
176200                                        RESP-KDBYTREF-IN                  
176210                                        RESP-FLSKROT-IN                   
176300             MOVE MFS-ADD-LAES-IN-FAELT TO                                
176400                                     RESP-IDARTNR-OBJ-SPAERR-ATTR         
176500                                        RESP-KVRETUR-IN-ATTR              
176600                                        RESP-KDBYTREF-IN-ATTR             
176610                                        RESP-FLSKROT-IN-ATTR              
176700             MOVE MFS-ALFA-FAELT-FEL    TO                                
176800                                     RESP-IDARTNR-OBJ-SPAERR-ATTR         
176900             MOVE NEJ TO INDATA-SW                                        
177000          END-IF                                                          
177100       ELSE                                                               
177200          IF BYT16-RADIO                                                  
177300             IF WS-ARTSIFFRA = 4                                          
177400* --- ---* ARTIKELN HAR ETT GODKÄNT OBJEKTNUMMER                          
177500               CONTINUE                                                   
177600               MOVE MFS-NUM-FAELT-RAETT                                   
177700               TO RESP-IDARTNR-OBJ-SPAERR-ATTR                            
177800             ELSE                                                         
177900               MOVE ERR-NOT-EXCHANGE-NO TO RESP-IDMSG-INFO                
178000               MOVE 'IDARTNR-OBJ'       TO RESP-IDELMT-ERROR              
178100               MOVE ALL-PLUS          TO RESP-IDARTNR-OBJ-SPAERR          
178200                                         RESP-KVRETUR-IN                  
178300                                         RESP-KDBYTREF-IN                 
178310                                         RESP-FLSKROT-IN                  
178400               MOVE MFS-ADD-LAES-IN-FAELT TO                              
178500                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
178600                                         RESP-KVRETUR-IN-ATTR             
178700                                         RESP-KDBYTREF-IN-ATTR            
178710                                         RESP-FLSKROT-IN-ATTR             
178800               MOVE MFS-ALFA-FAELT-FEL    TO                              
178900                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
179000               MOVE NEJ TO INDATA-SW                                      
179100             END-IF                                                       
179200          ELSE                                                            
179300            MOVE ERR-NOT-EXCHANGE-NO TO RESP-IDMSG-INFO                   
179400            MOVE 'IDARTNR-OBJ'       TO RESP-IDELMT-ERROR                 
179500            MOVE ALL-PLUS          TO RESP-IDARTNR-OBJ-SPAERR             
179600                                      RESP-KVRETUR-IN                     
179700                                      RESP-KDBYTREF-IN                    
179710                                      RESP-FLSKROT-IN                     
179800            MOVE MFS-ADD-LAES-IN-FAELT TO                                 
179900                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
180000                                      RESP-KVRETUR-IN-ATTR                
180100                                      RESP-KDBYTREF-IN-ATTR               
180110                                      RESP-FLSKROT-IN-ATTR                
180200            MOVE MFS-ALFA-FAELT-FEL    TO                                 
180300                                      RESP-IDARTNR-OBJ-SPAERR-ATTR        
180400            MOVE NEJ TO INDATA-SW                                         
180500          END-IF                                                          
180600       END-IF                                                             
180700     END-IF                                                               
180800     .                                                                    
180900     EJECT                                                                
181000                                                                          
181100 GC-BERAK-URSP-KVRETUR SECTION.                                           
181200     MOVE 'GC-BERAK-URSP-KVRETUR'    TO WS-SEKTION                        
181300                                                                          
181400     MOVE ZERO                        TO WS-LOPNR                         
181500     ADD +1                           TO WS-RADNR                         
181600     MOVE BYTF-OBJ-IDBYTRAD           TO WS-LOPNR                         
181700     IF WS-LOPNR  > 99                                                    
181800     AND WS-LOPNR < 9000                                                  
181900       IF INDATA-OK                                                       
182000        IF WS-LOPNR-2 = ZERO                                              
182100           IF SPAR-IDBYTRAD > ZERO                                        
182200              IF WS-KONT-KVRETUR-URSP = WS-KONT-KVRETUR-GODK              
182300              OR WS-KONT-KVRETUR-URSP < WS-KONT-KVRETUR-GODK              
182400                 MOVE ZERO           TO WS-KONT-KVRETUR-URSP              
182500                 MOVE ZERO           TO WS-KONT-KVRETUR-GODK              
182600                 MOVE BYTF-OBJ-IDBYTRAD  TO SPAR-IDBYTRAD                 
182700                 MOVE WS-RADNR           TO SPAR-RADNR                    
182800                 MOVE BYTF-OBJ-KVRETUR-URSP TO                            
182900                                        WS-KONT-KVRETUR-URSP              
183000                 MOVE BYTF-OBJ-KVRETUR-GODK TO                            
183100                                        WS-KONT-KVRETUR-GODK              
183200              ELSE                                                        
183300* * * * * ************************************************** * *          
183400* * * * * OBS !!! URSPRUNGLIGT ANTAL STÖRRE ÄN INREGISTRERAT * *          
183500* * * * * ************************************************** * *          
183600                MOVE QTY-NOT-LESS-ORIGIN TO RESP-IDMSG-INFO               
183700                PERFORM MFS-ROER-EJ-FAELT-UT                              
183800                PERFORM MFS-ROER-EJ-FAELT-IN                              
183900                MOVE NEJ TO INDATA-SW                                     
184000                MOVE SPAR-IDBYTRAD    TO W-IDBYTRAD                       
184100              END-IF                                                      
184200           ELSE                                                           
184300* * * * * FÖRSTA GÅNGEN * *                                               
184400              MOVE ZERO               TO WS-KONT-KVRETUR-URSP             
184500                                         WS-KONT-KVRETUR-GODK             
184600              MOVE BYTF-OBJ-IDBYTRAD  TO SPAR-IDBYTRAD                    
184700              MOVE WS-RADNR           TO SPAR-RADNR                       
184800              MOVE BYTF-OBJ-KVRETUR-URSP TO WS-KONT-KVRETUR-URSP          
184900              MOVE BYTF-OBJ-KVRETUR-GODK TO WS-KONT-KVRETUR-GODK          
185000           END-IF                                                         
185100        ELSE                                                              
185200           ADD BYTF-OBJ-KVRETUR-GODK  TO WS-KONT-KVRETUR-GODK             
185300           ADD BYTF-OBJ-KVRETUR-URSP  TO WS-KONT-KVRETUR-URSP             
185400        END-IF                                                            
185500       END-IF                                                             
185600     END-IF                                                               
185700     .                                                                    
185800     EJECT                                                                
185900                                                                          
186000 GCC-KONT-URSP-KVRETUR SECTION.                                           
186100     MOVE 'GCC-KONT-URSP-KVRETUR'    TO WS-SEKTION                        
186200                                                                          
186300     IF WS-KONT-KVRETUR-URSP = WS-KONT-KVRETUR-GODK                       
186400     OR WS-KONT-KVRETUR-URSP < WS-KONT-KVRETUR-GODK                       
186500        CONTINUE                                                          
186600     ELSE                                                                 
186700* * * * * ************************************************** * *          
186800* * * * * OBS !!! URSPRUNGLIGT ANTAL STÖRRE ÄN INREGISTRERAT * *          
186900* * * * * ************************************************** * *          
187000        MOVE QTY-NOT-LESS-ORIGIN TO RESP-IDMSG-INFO                       
187100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
187200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
187300        MOVE NEJ TO INDATA-SW                                             
187400        MOVE SPAR-IDBYTRAD    TO W-IDBYTRAD                               
187500     END-IF                                                               
187600     .                                                                    
187700     EJECT                                                                
187800                                                                          
187900 GD-VALIDERA-DIREKT-GODKN SECTION.                                        
188000     MOVE 'GD-VALIDER-DIREKT-GODKN'  TO WS-SEKTION                        
188100                                                                          
188200     IF (REQU-KVRETUR-GODK-LINE(INDX) = ALL '+'                           
188300     OR REQU-KVRETUR-GODK-LINE(INDX) = SPACE)                             
188400                                                                          
188500         MOVE REQU-IDBYTRAD-LINE (INDX) TO W-IDBYTRAD                     
188600         PERFORM IMS-GU-BYTF11                                            
188700         IF SEGMENT-FINNS                                                 
188800           IF BYTF-OBJ-IDARTNR-OBJ NOT = ZERO                             
188900              IF BYTF-OBJ-KDBYTSTA-OBJ = 'N'                              
189000              OR BYTF-OBJ-KDBYTSTA-OBJ = 'E'                              
189100              OR BYTF-OBJ-KDBYTSTA-OBJ = '4'                              
189200                 MOVE MFS-ALFA-FAELT-FEL TO                               
189300                    RESP-KDCMD-LINE-ATTR (INDX)                           
189400                 MOVE NEJ TO INDATA-SW                                    
189500                 MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO               
189600              ELSE                                                        
189700                IF REQU-KDBYTREF-LINE (INDX) = ALL '+'                    
189800                  MOVE BYTF-OBJ-KDBYTREF TO TEST-KDBYTREF                 
189900                                         WS-KDBYTREF-TAB (INDX)           
190000                ELSE                                                      
190100                  MOVE REQU-KDBYTREF-LINE (INDX) TO TEST-KDBYTREF         
190200                                           WS-KDBYTREF-TAB (INDX)         
190300                END-IF                                                    
190400                IF REQU-IDMSGVER = '101' AND                              
190500                   REQU-IDDC-KEY = WC-SDC-NL-ET AND                       
190600                   NOT BYT15-KDBYTREF-REMOVE AND                          
190700                   NOT BYT15-KDBYTREF-GAR-EJ-SALD                         
190800                  PERFORM GI-KONTROLLERA-PRINTER                          
190900                  IF CRUL-KDRC = ZERO                                     
191000                    MOVE MFS-ALFA-FAELT-RAETT                             
191100                                  TO RESP-KDPRT-ATTR                      
191200                                     RESP-KDCMD-LINE-ATTR (INDX)          
191300                  ELSE                                                    
191400                    MOVE MFS-ALFA-FAELT-FEL                               
191500                                  TO RESP-KDPRT-ATTR                      
191600                                     RESP-KDCMD-LINE-ATTR (INDX)          
191700                    MOVE NEJ    TO INDATA-SW                              
191800                    MOVE WRONG-PRINTER                                    
191900                                  TO RESP-IDMSG-INFO                      
192000                  END-IF                                                  
192100                ELSE                                                      
192200                  MOVE MFS-ALFA-FAELT-RAETT                               
192300                                  TO RESP-KDCMD-LINE-ATTR (INDX)          
192400                END-IF                                                    
192500              END-IF                                                      
192600           ELSE                                                           
192700             MOVE MFS-ALFA-FAELT-FEL TO                                   
192800                    RESP-KDCMD-LINE-ATTR (INDX)                           
192900             MOVE NEJ TO INDATA-SW                                        
193000             MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO                   
193100           END-IF                                                         
193200         ELSE                                                             
193300           MOVE MFS-ALFA-FAELT-FEL TO                                     
193400                     RESP-KDCMD-LINE-ATTR (INDX)                          
193500           MOVE NEJ TO INDATA-SW                                          
193600           MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO                     
193700         END-IF                                                           
193800     ELSE                                                                 
193900        MOVE MFS-ALFA-FAELT-FEL TO                                        
194000             RESP-KDCMD-LINE-ATTR (INDX)                                  
194100        MOVE NEJ TO INDATA-SW                                             
194200        MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO                        
194300     END-IF                                                               
194400     MOVE ZERO                     TO WS-IDBYTRAD                         
194500                                                                          
194600     .                                                                    
194700     EJECT                                                                
194800                                                                          
194900 GE-VALIDERA-ANMARK SECTION.                                              
195000     MOVE 'GE-VALIDER-ANMARK'  TO WS-SEKTION                              
195100                                                                          
195200     MOVE REQU-IDARTNR-OBJ-LINE (INDX) TO W-IDARTNR-OBJ                   
195300     IF REQU-KDBYTREF-LINE (INDX) NOT = ALL '+'                           
195400        MOVE REQU-KDBYTREF-LINE (INDX) TO WS-ANMARK                       
195500     ELSE                                                                 
195600        MOVE SPACE                    TO WS-ANMARK                        
195700     END-IF                                                               
195800******************************************************************        
195900** VARNING GARANTI FÅR ENDAST ÄNDRAS AV EN ANNAN TYP AV GARANTI **        
196000******************************************************************        
196100     MOVE REQU-IDBYTRAD-LINE (INDX) TO W-IDBYTRAD                         
196200     PERFORM IMS-GU-BYTF11                                                
196300     IF SEGMENT-FINNS                                                     
196400****************                                                          
196500** OM GARANTI **                                                          
196600****************                                                          
196700        IF BYTF-OBJ-KDBYTREF = '200'                                      
196800        OR BYTF-OBJ-KDBYTREF = '210'                                      
196900        OR BYTF-OBJ-KDBYTREF = '220'                                      
197000           IF WS-FLBYTGAR = 'J'                                           
197100             IF WS-ANMARK = SPACE                                         
197200                MOVE MFS-ALFA-FAELT-FEL TO                                
197300                RESP-KDBYTREF-LINE-ATTR (INDX)                            
197400                MOVE WRONG-CODE TO RESP-IDMSG-INFO                        
197500                MOVE 'KDBYTREF' TO  RESP-IDELMT-ERROR                     
197600                MOVE NEJ TO INDATA-SW                                     
197700             ELSE                                                         
197800                IF BYT15-KDBYTREF-GAR-SALD                                
197900                OR BYT15-KDBYTREF-GAR-EJ-SALD                             
198000                   PERFORM GEE-VALIDERA-ANMARK                            
198100                ELSE                                                      
198200                   MOVE MFS-ALFA-FAELT-FEL TO                             
198300                   RESP-KDBYTREF-LINE-ATTR (INDX)                         
198400                   MOVE WRONG-CODE TO RESP-IDMSG-INFO                     
198500                   MOVE 'KDBYTREF' TO  RESP-IDELMT-ERROR                  
198600                   MOVE NEJ TO INDATA-SW                                  
198700                END-IF                                                    
198800             END-IF                                                       
198900           ELSE                                                           
199000              PERFORM GEE-VALIDERA-ANMARK                                 
199100           END-IF                                                         
199200        ELSE                                                              
199300           PERFORM GEE-VALIDERA-ANMARK                                    
199400        END-IF                                                            
199500     ELSE                                                                 
199600        PERFORM GEE-VALIDERA-ANMARK                                       
199700     END-IF                                                               
199800                                                                          
199900     .                                                                    
200000     EJECT                                                                
200100                                                                          
200200 GEE-VALIDERA-ANMARK SECTION.                                             
200300     MOVE 'GEE-VALIDERA-ANMARK'  TO WS-SEKTION                            
200400                                                                          
200500     MOVE ZERO                     TO W-IDTABNR                           
200600     MOVE NEJ                      TO ARTIKEL-SW                          
200700     MOVE REQU-IDBYTRAD-LINE (INDX) TO WS-IDBYTRAD                        
200800     MOVE WS-IDBYTRAD              TO WS-LOPNUMMER                        
200900     IF WS-LOPNUMMER2 = ZERO                                              
201000        MOVE WS-LOPNUMMER          TO W-IDBYTRAD-MIN                      
201100        MOVE +99                   TO WS-LOPNUMMER2                       
201200        MOVE WS-LOPNUMMER          TO W-IDBYTRAD-MAX                      
201300     ELSE                                                                 
201400        MOVE ZERO                  TO WS-LOPNUMMER2                       
201500        MOVE WS-LOPNUMMER          TO W-IDBYTRAD-MIN                      
201600        MOVE +99                   TO WS-LOPNUMMER2                       
201700        MOVE WS-LOPNUMMER          TO W-IDBYTRAD-MAX                      
201800     END-IF                                                               
201900     MOVE ZERO                     TO WS-IDBYTRAD                         
202000     PERFORM IMS-GU-BYTF01                                                
202100     IF SEGMENT-FINNS                                                     
202200        PERFORM UNTIL SEGMENT-SAKNAS                                      
202300        OR   ARTIKEL-LIKA                                                 
202400             PERFORM IMS-GHNP-BYTF11-IMS                                  
202500             IF SEGMENT-FINNS                                             
202600                IF W-IDARTNR-OBJ = BYTF-OBJ-IDARTNR-OBJ AND               
202700                   W-IDTABNR = BYTF-OBJ-IDTABNR AND                       
202800                   WS-ANMARK = BYTF-OBJ-KDBYTREF                          
202900                    MOVE JA    TO ARTIKEL-SW                              
203000                END-IF                                                    
203100             END-IF                                                       
203200                                                                          
203300        END-PERFORM                                                       
203400                                                                          
203500        MOVE MFS-ALFA-FAELT-RAETT TO                                      
203600             RESP-KDBYTREF-LINE-ATTR (INDX)                               
204500     END-IF                                                               
204600                                                                          
204700     .                                                                    
204800     EJECT                                                                
204900                                                                          
205000 GF-VALIDERA-ANMARK SECTION.                                              
205100     MOVE 'GF-VALIDERA-ANMARK'  TO WS-SEKTION                             
205200                                                                          
205300     IF REQU-KDBYTREF-IN NOT = ALL '+'                                    
205400        IF REQU-KDBYTREF-IN = ALL ' '                                     
205500           MOVE SPACE TO WS-ANMARK                                        
205600        ELSE                                                              
205700           MOVE REQU-KDBYTREF-IN TO                                       
205800                   WS-ANMARK                                              
205900           MOVE WS-ANMARK  TO                                             
206000                   TEST-KDBYTREF                                          
206100        END-IF                                                            
206200        IF BYT15-KDBYTREF-OK                                              
206300        OR BYT15-KDBYTREF-REMOVE                                          
206310        OR BYT15-KDBYTREF-GAR-EJ-SALD                                     
206400        OR WS-ANMARK = SPACE                                              
206500           MOVE MFS-ALFA-FAELT-RAETT TO                                   
206600                   RESP-KDBYTREF-IN-ATTR                                  
206700        ELSE                                                              
206800           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDBYTREF-IN-ATTR               
207000           MOVE NEJ TO INDATA-SW                                          
207100           MOVE WRONG-CODE TO  RESP-IDMSG-INFO                            
207200           MOVE 'KDBYTREF' TO  RESP-IDELMT-ERROR                          
207300        END-IF                                                            
207400     END-IF                                                               
207500                                                                          
207600     .                                                                    
207700     EJECT                                                                
207800                                                                          
207900* -- GAMMAL EJ FÖRSTÅDD KOD FÖR REDIGERING AV 3-STÄLLIGT ANTAL.           
208000* -- KVRETUR-GODK ÄR NUMERA 5-STÄLLIGT OCH REDAN REDIGERAD.               
208100*                                                                         
208200*GG-RED-KVRETUR-GODK SECTION.                                             
208300*    MOVE 'GG-RED-KVRETUR'  TO WS-SEKTION                                 
208400*                                                                         
208500*    MOVE SPACE                    TO RED-TAB1                            
208600*    MOVE SPACE                    TO RED-TAB2                            
208700*    MOVE 3                        TO RED-INDX1                           
208800*    MOVE 3                        TO RED-INDX2                           
208900*                                                                         
209000*    MOVE REQU-KVRETUR-GODK-LINE(INDX) TO RED-TAB1                        
209100*    IF WS-RED-TAB1(1) NUMERIC                                            
209200*    AND WS-RED-TAB1(2) = SPACE                                           
209300*    AND WS-RED-TAB1(3) NUMERIC                                           
209400*        CONTINUE                                                         
209500*    ELSE                                                                 
209600*                                                                         
209700*       PERFORM UNTIL  RED-INDX1 < 1                                      
209800*           IF WS-RED-TAB1(RED-INDX1) = SPACE                             
209900*              SUBTRACT +1 FROM RED-INDX1                                 
210000*           ELSE                                                          
210100*              IF WS-RED-TAB1(RED-INDX1) NUMERIC                          
210200*                 MOVE WS-RED-TAB1(RED-INDX1) TO                          
210300*                   WS-RED-TAB2(RED-INDX2)                                
210400*                 SUBTRACT +1 FROM RED-INDX1                              
210500*                 SUBTRACT +1 FROM RED-INDX2                              
210600*              ELSE                                                       
210700*                 MOVE WS-RED-TAB1(RED-INDX1) TO                          
210800*                   WS-RED-TAB2(RED-INDX2)                                
210900*                 SUBTRACT +1 FROM RED-INDX1                              
211000*                 SUBTRACT +1 FROM RED-INDX2                              
211100*              END-IF                                                     
211200*           END-IF                                                        
211300*                                                                         
211400*       END-PERFORM                                                       
211500*                                                                         
211600*       MOVE RED-TAB2 TO REQU-KVRETUR-GODK-LINE(INDX)                     
211700*       INSPECT REQU-KVRETUR-GODK-LINE(INDX)                              
211800*                     REPLACING LEADING SPACE BY ZERO                     
211900*    END-IF                                                               
212000*    .                                                                    
212100*    EJECT                                                                
212200                                                                          
212300 GH-VALIDERA-SKROTNING SECTION.                                           
212400     MOVE 'GH-VALIDERA-SKROTNING'  TO WS-SEKTION                          
212500                                                                          
212600     IF (REQU-KVRETUR-GODK-LINE(INDX) = ALL '+'                           
212700     OR REQU-KVRETUR-GODK-LINE(INDX) = SPACE)                             
212800                                                                          
212900       MOVE REQU-IDBYTRAD-LINE (INDX) TO W-IDBYTRAD                       
213000       PERFORM IMS-GU-BYTF11                                              
213100       IF SEGMENT-FINNS                                                   
213200          IF BYTF-OBJ-IDARTNR-OBJ NOT = ZERO                              
213300*/EJ KUNNA SKROTA STATUS 4, I SÅ FALL FÅR MAN GÖRA PÅ GAMMALT VIS         
213400*/GENOM ATT ANVÄNDA BILD 4231.                                            
213500             IF BYTF-OBJ-KDBYTSTA-OBJ = '4'                               
213600                MOVE MFS-ALFA-FAELT-FEL TO                                
213700                   RESP-KDCMD-LINE-ATTR (INDX)                            
213800                MOVE NEJ TO INDATA-SW                                     
213900                MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO                
214000             ELSE                                                         
214100               IF BYTF-OBJ-KVRETUR-GODK = 0 AND                           
214200                 (BYTF-OBJ-KDBYTSTA-OBJ = 'N' OR                          
214300                  BYTF-OBJ-KDBYTSTA-OBJ = 'E' OR                          
214400                  BYTF-OBJ-KDBYTSTA-OBJ = 'C')                            
214500                 MOVE MFS-ALFA-FAELT-FEL TO                               
214600                    RESP-KDCMD-LINE-ATTR (INDX)                           
214700                 MOVE NEJ TO INDATA-SW                                    
214800                 MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO               
214900               ELSE                                                       
215000                 MOVE MFS-ALFA-FAELT-RAETT TO                             
215100                 RESP-KDCMD-LINE-ATTR (INDX)                              
215200               END-IF                                                     
215300             END-IF                                                       
215400          ELSE                                                            
215500            MOVE MFS-ALFA-FAELT-FEL TO                                    
215600                   RESP-KDCMD-LINE-ATTR (INDX)                            
215700            MOVE NEJ TO INDATA-SW                                         
215800            MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO                    
215900          END-IF                                                          
216000       ELSE                                                               
216100         MOVE MFS-ALFA-FAELT-FEL TO                                       
216200                   RESP-KDCMD-LINE-ATTR (INDX)                            
216300         MOVE NEJ TO INDATA-SW                                            
216400         MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO                       
216500       END-IF                                                             
216600     ELSE                                                                 
216700        MOVE MFS-ALFA-FAELT-FEL TO                                        
216800             RESP-KDCMD-LINE-ATTR (INDX)                                  
216900        MOVE NEJ TO INDATA-SW                                             
217000        MOVE UPDATE-NOT-ALLOWED TO RESP-IDMSG-INFO                        
217100     END-IF                                                               
217200     MOVE ZERO                     TO WS-IDBYTRAD                         
217300                                                                          
217400     .                                                                    
217500     EJECT                                                                
217600                                                                          
217700 GI-KONTROLLERA-PRINTER SECTION.                                          
217800                                                                          
217900     MOVE 'GI-KONTROLLERA-PRINTER'                                        
218000                                 TO WS-SEKTION                            
218100     MOVE 'CORE-LABEL'           TO CRUL-IDOUTTYPE                        
218200     MOVE REQU-KDPRT             TO CRUL-IDOUTREC                         
218300     CALL WZ04CRUL USING CRUL-WZ04CRUL                                    
218400                                                                          
218500     .                                                                    
218501     EJECT                                                                
218502                                                                          
218503 GJ-VALIDERA-SKROT SECTION.                                               
218504                                                                          
218505     MOVE 'GJ-VALIDERA-SKROT'                                             
218506                                 TO WS-SEKTION                            
218507                                                                          
218508     IF REQU-FLSKROT-IN = 'Y' OR 'J' OR SPACE                             
218509        IF REQU-FLSKROT-IN = 'Y'                                          
218510           MOVE JA TO  REQU-FLSKROT-IN                                    
218511        END-IF                                                            
218512        MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLSKROT-IN-ATTR                 
218513     ELSE                                                                 
218514        MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLSKROT-IN-ATTR                 
218515        MOVE NEJ TO INDATA-SW                                             
218516     END-IF                                                               
218517     .                                                                    
218600     EJECT                                                                
218700                                                                          
218800 H-UPPDATERA SECTION.                                                     
218900     MOVE 'H-UPPDATERA'  TO WS-SEKTION                                    
219000                                                                          
219100     PERFORM IMS-GU-BYTF01                                                
219200     IF BYTF-RAPP-IDDC = W-IDDC                                           
219210        MOVE BYTF-RAPP-IDDC TO W-RAPP-IDDC                                
219300        PERFORM HA-UPPDATERA                                              
219400     ELSE                                                                 
219500        MOVE NEJ TO INDATA-SW                                             
219600        MOVE USER-NOT-ALLOWED TO RESP-IDMSG-INFO                          
219700        PERFORM MFS-FORM-ATTR                                             
219800        PERFORM MFS-RENSA-FAELT-IN                                        
219900     END-IF                                                               
220000     .                                                                    
220100     EJECT                                                                
220200 HA-UPPDATERA SECTION.                                                    
220310     MOVE 'HA-UPPDATERA'  TO WS-SEKTION                                   
220400                                                                          
220500     IF PF11-VARNING-NEJ                                                  
220600       IF REQU-INPUT = ALL '+'                                            
220700         MOVE +1 TO INDX                                                  
220800         PERFORM UNTIL INDX > REQU-KVRADER                                
220900           IF REQU-KVRETUR-GODK-LINE(INDX) NOT = ALL '+' OR               
221000              REQU-KDCMD-LINE (INDX) NOT = ALL '+' OR                     
221100              REQU-KDBYTREF-LINE (INDX) NOT = ALL '+'                     
221200                                                                          
221300              MOVE REQU-IDBYTRAD-LINE (INDX) TO W-IDBYTRAD                
221400                                                                          
221500              PERFORM IMS-GHU-BYTF11                                      
221600              IF SEGMENT-FINNS                                            
221700                MOVE BYTF-OBJ-IDARTNR-OBJ    TO                           
221800                     WS-SPAR-IDARTNR-OBJ                                  
221900                IF REQU-KVRETUR-GODK-LINE(INDX) NOT = ALL '+'             
222000                  MOVE BYTF-OBJ-KDBYTSTA-OBJ TO                           
222100                       WS-SPAR-KDBYTSTA-OBJ                               
222200                  MOVE BYTF-OBJ-KVRETUR-URSP TO                           
222300                       WS-SPAR-KVRETUR-URSP                               
222400                  MOVE REQU-KVRETUR-GODK-LINE(INDX) TO                    
222500                       BYTF-OBJ-KVRETUR-GODK                              
222600                  MOVE REQU-KVRETUR-GODK-LINE(INDX) TO WS-KVRETUR         
222700                  IF WS-KVRETUR = BYTF-OBJ-KVRETUR-URSP                   
222800                    IF BYTF-OBJ-KDBYTSTA-OBJ = 'N'                        
222900                    OR BYTF-OBJ-KDBYTSTA-OBJ = 'E'                        
223000                       CONTINUE                                           
223100                    ELSE                                                  
223200                      MOVE BYTF-OBJ-IDARTNR-OBJ TO                        
223300                                W-IDARTNR-K6                              
223400                      PERFORM IMS-GU-WDK6                                 
223500                      IF SEGMENT-FINNS                                    
223600                        IF BYTF-OBJ-IDBYTRAD < 100                        
223700                           MOVE BYTF-OBJ-KDBYTREF TO                      
223800                                    TEST-KDBYTREF                         
223900                           IF BYT15-KDBYTREF-REMOVE                       
224000                              MOVE ZERO TO BYTF-OBJ-KVRETUR-GODK          
224100                           END-IF                                         
224200                        END-IF                                            
224300                      ELSE                                                
224400                        MOVE '032'     TO BYTF-OBJ-KDBYTREF               
224500                        IF BYTF-OBJ-IDBYTRAD < 100                        
224600                           MOVE ZERO    TO BYTF-OBJ-KVRETUR-GODK          
224700                        END-IF                                            
224800                      END-IF                                              
224900                      PERFORM HAB-AVGOR-STATUS-RADEN                      
225000*                     IF BYTF-OBJ-KVRETUR-URSP =                          
225100*                     BYTF-OBJ-KVRETUR-GODK                               
225200*                       MOVE '4'       TO BYTF-OBJ-KDBYTSTA-OBJ           
225300*                     ELSE                                                
225400*                       MOVE 'C'       TO BYTF-OBJ-KDBYTSTA-OBJ           
225500*                     END-IF                                              
225600                    END-IF                                                
225700                  ELSE                                                    
225800                    IF BYTF-OBJ-KDBYTSTA-OBJ = 'N'                        
225900                    OR BYTF-OBJ-KDBYTSTA-OBJ = 'E'                        
226000                       CONTINUE                                           
226100                    ELSE                                                  
226200                      MOVE BYTF-OBJ-IDARTNR-OBJ TO                        
226300                                W-IDARTNR-K6                              
226400                      PERFORM IMS-GU-WDK6                                 
226500                      IF SEGMENT-FINNS                                    
226600                        IF BYTF-OBJ-IDBYTRAD < 100                        
226700                           MOVE BYTF-OBJ-KDBYTREF TO                      
226800                                    TEST-KDBYTREF                         
226900                           IF BYT15-KDBYTREF-REMOVE                       
227000                              MOVE ZERO TO BYTF-OBJ-KVRETUR-GODK          
227100                           END-IF                                         
227200                        END-IF                                            
227300                      ELSE                                                
227400******************************************************************        
227500***** TABELL FÖR GAMMAL VIPSANVÄNDARE GÅR ATT GODKÄNNA ***********        
227600******************************************************************        
227700                        IF BYTF-OBJ-IDARTNR-OBJ  = ZERO                   
227800                        AND BYTF-OBJ-IDBYTRAD < 100                       
227900                           CONTINUE                                       
228000                        ELSE                                              
228100                           MOVE '032'     TO BYTF-OBJ-KDBYTREF            
228200                        END-IF                                            
228300                        IF BYTF-OBJ-IDBYTRAD < 100                        
228400                           MOVE ZERO   TO BYTF-OBJ-KVRETUR-GODK           
228500                        END-IF                                            
228600                      END-IF                                              
228700                      MOVE 'C'         TO BYTF-OBJ-KDBYTSTA-OBJ           
228800                    END-IF                                                
228900                  END-IF                                                  
229000                ELSE                                                      
229100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
229200* * * DIREKT GODKÄNNANDE MED G ELLER A  * * * * * * * * * * * * *         
229300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
229400                  IF REQU-KDCMD-LINE (INDX) = 'G'                         
229500                  OR REQU-KDCMD-LINE (INDX) = 'A'                         
229600                     MOVE BYTF-OBJ-IDARTNR-OBJ TO                         
229700                                W-IDARTNR-K6                              
229800                     PERFORM IMS-GU-WDK6                                  
229900                     MOVE BYTF-OBJ-KVRETUR-URSP TO                        
230000                                          BYTF-OBJ-KVRETUR-GODK           
230100                     IF SEGMENT-FINNS                                     
230200                        IF BYTF-OBJ-IDBYTRAD < 100                        
230300                           MOVE BYTF-OBJ-KDBYTREF TO                      
230400                                    TEST-KDBYTREF                         
230500                           IF BYT15-KDBYTREF-REMOVE                       
230600                              MOVE ZERO TO BYTF-OBJ-KVRETUR-GODK          
230700                           END-IF                                         
230800                        END-IF                                            
230900                     ELSE                                                 
231000                        MOVE '032'     TO BYTF-OBJ-KDBYTREF               
231100                        IF BYTF-OBJ-IDBYTRAD < 100                        
231200                           MOVE ZERO   TO BYTF-OBJ-KVRETUR-GODK           
231300                        END-IF                                            
231400                     END-IF                                               
231500                     MOVE BYTF-OBJ-KDBYTSTA-OBJ TO                        
231600                                         WS-SPAR-KDBYTSTA-OBJ             
231700                     MOVE BYTF-OBJ-KVRETUR-URSP TO                        
231800                                         WS-SPAR-KVRETUR-URSP             
231900                     IF REQU-KDBYTREF-LINE (INDX) NOT = ALL '+'           
232000                        PERFORM HAB-AVGOR-STATUS-RADEN                    
232100*                       MOVE 'C'       TO BYTF-OBJ-KDBYTSTA-OBJ           
232200                     ELSE                                                 
232300                        PERFORM HAB-AVGOR-STATUS-RADEN                    
232400*                       IF BYTF-OBJ-KVRETUR-URSP =                        
232500*                       BYTF-OBJ-KVRETUR-GODK                             
232600*                       MOVE '4'       TO BYTF-OBJ-KDBYTSTA-OBJ           
232700*                       ELSE                                              
232800*                       MOVE 'C'       TO BYTF-OBJ-KDBYTSTA-OBJ           
232900*                       END-IF                                            
233000                     END-IF                                               
233100                  ELSE                                                    
233200                    IF REQU-KDCMD-LINE (INDX) = 'S'                       
233300                      MOVE BYTF-OBJ-IDARTNR-OBJ TO                        
233400                                 W-IDARTNR-K6                             
233500                      PERFORM IMS-GU-WDK6                                 
233600                      IF BYTF-OBJ-KVRETUR-URSP > 0 AND                    
233700                         BYTF-OBJ-KDBYTSTA-OBJ = ' '                      
233800                          MOVE BYTF-OBJ-KVRETUR-URSP TO                   
233900                                BYTF-OBJ-KVRETUR-GODK                     
234000                          MOVE JA TO BYTF-OBJ-FLSKROT                     
234100                      ELSE                                                
234200                        IF BYTF-OBJ-KVRETUR-GODK > 0 AND                  
234300                          (BYTF-OBJ-KDBYTSTA-OBJ = 'N' OR                 
234400                          BYTF-OBJ-KDBYTSTA-OBJ = 'C' OR                  
234500                          BYTF-OBJ-KDBYTSTA-OBJ = 'E')                    
234600                          MOVE JA TO BYTF-OBJ-FLSKROT                     
234700                        END-IF                                            
234800                      END-IF                                              
234900                                                                          
235000                      IF SEGMENT-FINNS                                    
235100                         IF BYTF-OBJ-IDBYTRAD < 100                       
235200                            MOVE BYTF-OBJ-KDBYTREF TO                     
235300                                     TEST-KDBYTREF                        
235400                            IF BYT15-KDBYTREF-REMOVE                      
235500                               MOVE ZERO TO BYTF-OBJ-KVRETUR-GODK         
235600                            END-IF                                        
235700                         END-IF                                           
235800                      ELSE                                                
235900                         MOVE '032'     TO BYTF-OBJ-KDBYTREF              
236000                         IF BYTF-OBJ-IDBYTRAD < 100                       
236100                            MOVE ZERO   TO BYTF-OBJ-KVRETUR-GODK          
236200                         END-IF                                           
236300                      END-IF                                              
236400                      MOVE BYTF-OBJ-KDBYTSTA-OBJ TO                       
236500                                          WS-SPAR-KDBYTSTA-OBJ            
236600                      MOVE BYTF-OBJ-KVRETUR-URSP TO                       
236700                                          WS-SPAR-KVRETUR-URSP            
236800                      IF REQU-KDBYTREF-LINE (INDX) NOT = ALL '+'          
236900                         PERFORM HAB-AVGOR-STATUS-RADEN                   
237000*                        MOVE 'C'       TO BYTF-OBJ-KDBYTSTA-OBJ          
237100                      ELSE                                                
237200                         PERFORM HAB-AVGOR-STATUS-RADEN                   
237300*                        IF BYTF-OBJ-KVRETUR-URSP =                       
237400*                        BYTF-OBJ-KVRETUR-GODK                            
237500*                        MOVE '4'       TO BYTF-OBJ-KDBYTSTA-OBJ          
237600*                        ELSE                                             
237700*                        MOVE 'C'       TO BYTF-OBJ-KDBYTSTA-OBJ          
237800*                        END-IF                                           
237900                      END-IF                                              
238000                    ELSE                                                  
238100                     MOVE ALL-PLUS          TO                            
238200                       RESP-KVRETUR-GODK-LINE-ATTR (INDX)                 
238300                    END-IF                                                
238400                  END-IF                                                  
238500                END-IF                                                    
238600                                                                          
238700                IF REQU-KDBYTREF-LINE (INDX) NOT = ALL '+'                
238800                  IF BYTF-OBJ-KDBYTREF = '032'                            
238900                     CONTINUE                                             
239000                  ELSE                                                    
239100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
239200* * * *  960320 NY ÄNDRING  * * * * * * * * * * * * * * * * * * *         
239300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
239400                     IF BYTF-OBJ-IDBYTRAD < 100                           
239500                        MOVE REQU-KDBYTREF-LINE (INDX) TO                 
239600                                    TEST-KDBYTREF                         
239700                        IF BYT15-KDBYTREF-REMOVE                          
239800                           MOVE ZERO TO BYTF-OBJ-KVRETUR-GODK             
239900                           IF BYTF-OBJ-KDBYTSTA-OBJ = '4'                 
240000                              MOVE 'C' TO BYTF-OBJ-KDBYTSTA-OBJ           
240100                           END-IF                                         
240200                        END-IF                                            
240300                     ELSE                                                 
240400                        PERFORM HAB-AVGOR-STATUS-RADEN                    
240500                     END-IF                                               
240600* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
240700* * * *  960320 NY ÄNDRING  * * * * * * * * * * * * * * * * * * *         
240800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
240900                     MOVE REQU-KDBYTREF-LINE (INDX) TO                    
241000                       BYTF-OBJ-KDBYTREF                                  
241100                  END-IF                                                  
241200                ELSE                                                      
241300                  IF REQU-KDCMD-LINE (INDX) = 'G'                         
241400                  OR REQU-KDCMD-LINE (INDX) = 'A'                         
241500                  OR REQU-KDCMD-LINE (INDX) = 'S'                         
241600                     CONTINUE                                             
241700                  ELSE                                                    
241800                     MOVE ALL-PLUS          TO                            
241900                       RESP-KDBYTREF-LINE-ATTR (INDX)                     
242000                  END-IF                                                  
242100                END-IF                                                    
242200*                                                                         
242300                PERFORM IMS-REPL-BYTF                                     
242400                IF REQU-KDCMD-LINE (INDX) NOT = ALL '+'                   
242500                   IF REQU-KDCMD-LINE (INDX) = 'D'                        
242600                       PERFORM IMS-DLET-BYTF                              
242700                       PERFORM HAC-AVGOR-STATUS-I-DLT                     
242800                   END-IF                                                 
242900                END-IF                                                    
243000                IF REQU-KDCMD-LINE (INDX) NOT = ALL '+'                   
243100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
243200**KUNNA ÅNGRA SKROTNING. TAG BORT SCR OCH ÅTERSTÄLL GODK-KVRETUR          
243300                   IF REQU-KDCMD-LINE (INDX) = 'C'                        
243400                     IF BYTF-OBJ-FLSKROT = 'J'                            
243500                       IF BYTF-OBJ-KDBYTSTA-OBJ = 4                       
243600                         MOVE ZERO  TO BYTF-OBJ-KVRETUR-GODK              
243700                         MOVE SPACE TO BYTF-OBJ-FLSKROT                   
243800                         MOVE SPACE TO BYTF-OBJ-KDBYTSTA-OBJ              
243900                         PERFORM IMS-REPL-BYTF                            
244000                       ELSE                                               
244100                         MOVE SPACE TO BYTF-OBJ-FLSKROT                   
244200                         PERFORM IMS-REPL-BYTF                            
244300                       END-IF                                             
244400                     ELSE                                                 
244500                       MOVE MFS-ALFA-FAELT-FEL TO                         
244600                       RESP-KDCMD-LINE-ATTR (INDX)                        
244700                       MOVE NEJ TO INDATA-SW                              
244800                       MOVE UPDATE-NOT-ALLOWED                            
244900                       TO RESP-IDMSG-INFO                                 
245000                     END-IF                                               
245100                   END-IF                                                 
245200                END-IF                                                    
245300                IF REQU-KDCMD-LINE (INDX) = ALL '+'                       
245400                   IF BYTF-OBJ-KDBYTSTA-OBJ = 'N'                         
245500                   OR BYTF-OBJ-KDBYTSTA-OBJ = 'E'                         
245600                      IF REQU-KVRETUR-GODK-LINE(INDX) = ALL '+'           
245700                         IF BYTF-OBJ-KVRETUR-GODK < 1                     
245800                             PERFORM IMS-DLET-BYTF                        
245900                             PERFORM HAC-AVGOR-STATUS-I-DLT               
246000                         END-IF                                           
246100                      ELSE                                                
246200                         INSPECT REQU-KVRETUR-GODK-LINE(INDX)             
246300                         REPLACING LEADING SPACE BY ZERO                  
246400                         MOVE REQU-KVRETUR-GODK-LINE(INDX) TO             
246500                             WS-KVRETUR-GODK                              
246600                         IF WS-KVRETUR-GODK < 1                           
246700                            PERFORM IMS-DLET-BYTF                         
246800                            PERFORM HAC-AVGOR-STATUS-I-DLT                
246900                         END-IF                                           
247000                      END-IF                                              
247100                   END-IF                                                 
247200                END-IF                                                    
247300                IF WS-SPAR-KDBYTSTA-OBJ = SPACE                           
247400                   PERFORM IMS-GHU-BYTF01                                 
247500                   IF SEGMENT-FINNS                                       
247600                      SUBTRACT WS-SPAR-KVRETUR-URSP FROM                  
247700                      BYTF-RAPP-KVRETUR-TOT                               
247800                      PERFORM IMS-REPL-BYTF                               
247900                   END-IF                                                 
248000                END-IF                                                    
248100              END-IF                                                      
248200           END-IF                                                         
248300           IF INDATA-OK AND ALLT-OK                                       
248400             MOVE WS-KDBYTREF-TAB (INDX) TO TEST-KDBYTREF                 
248500             IF REQU-IDMSGVER = '101' AND                                 
248600                REQU-IDDC-KEY = WC-SDC-NL-ET AND                          
248700                NOT BYT15-KDBYTREF-REMOVE AND                             
248800                NOT BYT15-KDBYTREF-GAR-EJ-SALD AND                        
248900               (REQU-KDCMD-LINE (INDX) = 'G' OR                           
249000                REQU-KDCMD-LINE (INDX) = 'A')                             
249100                                                                          
249200               MOVE FUNCTION CURRENT-DATE(1:12)                           
249300                                 TO WS-CURRENT-DATE-TIME                  
249400               MOVE '1        '  TO CORE-IDAFPRCD                         
249500               MOVE WS-YEAR      TO LBL-TIAAAA                            
249600               MOVE WS-MONTH     TO LBL-TIMM                              
249700               MOVE WS-DAY       TO LBL-TIDD                              
249800               MOVE WS-HOUR      TO LBL-TIHH                              
249900               MOVE WS-MINUTE    TO LBL-TIMIN                             
250000               MOVE CORE-DATE    TO CORE-PRINT-DATE                       
250100               MOVE CORE-TIME    TO CORE-TIHHMM                           
250200               MOVE REQU-IDUSER  TO CORE-IDUSER                           
250201               MOVE WS-SPAR-IDARTNR-OBJ                                   
250202                                 TO CORE-IDARTNR-OBJ                      
250203                                    WS-IDARTNR                            
250204                                    TEST-IDARTNR                          
250205                                    W-IDARTNR                             
250208               PERFORM IMS-GET-BENA-GU                                    
250209               IF SEGMENT-FINNS                                           
250214                 MOVE  BENA-TEXT-BEART                                    
250220                                 TO CORE-BEART                            
250230               END-IF                                                     
250600               IF BYT16-RADIO                                             
250700                 IF BYT16-RADIO-EXTRA                                     
250800                   MOVE 0        TO WS-ARTSIFFRA                          
250900                 ELSE                                                     
251000                   MOVE 3        TO WS-ARTSIFFRA                          
251100                 END-IF                                                   
251200               ELSE                                                       
251300                 IF ART-0                                                 
251400                   MOVE 0        TO WS-ARTSIFFRA                          
251500                 ELSE                                                     
251600                   IF ART-1                                               
251700                     MOVE 1      TO WS-ARTSIFFRA                          
251800                   ELSE                                                   
251900                     IF ART-2                                             
252000                       MOVE 2    TO WS-ARTSIFFRA                          
252100                     ELSE                                                 
252200                       IF ART-3                                           
252300                         MOVE 3  TO WS-ARTSIFFRA                          
252400                       END-IF                                             
252500                     END-IF                                               
252600                   END-IF                                                 
252700                 END-IF                                                   
252800               END-IF                                                     
252900               MOVE WS-IDARTNR   TO BYART-IDARTNR-BYT                     
253000               PERFORM DB2-SELECT-BYART                                   
253100               IF RADER-FINNS                                             
253200                 MOVE BYART-ADLAGOMR TO CORE-ADLAGOMR                     
253300                 MOVE BYART-ADGANG   TO CORE-ADGANG                       
253400                 MOVE BYART-ADPLATS  TO CORE-ADPLATS                      
253500               ELSE                                                       
253600                 MOVE ZERO           TO CORE-ADLAGOMR                     
253700                 MOVE ZERO           TO CORE-ADGANG                       
253800                 MOVE ZERO           TO CORE-ADPLATS                      
253900               END-IF                                                     
254000               IF FORSTA-TRAFF                                            
254110                  PERFORM S21-SEND-OPEN                                   
254200                  PERFORM S22-PUT-HEADER                                  
254300                  MOVE JA TO FORSTA-SW                                    
254400               END-IF                                                     
254500               MOVE 1 TO WS-PRINT-ANTAL                                   
254600               PERFORM UNTIL WS-PRINT-ANTAL > WS-SPAR-KVRETUR-URSP        
254700                  PERFORM S25-PUT-LINE                                    
254800                  ADD 1 TO WS-PRINT-ANTAL                                 
254900               END-PERFORM                                                
255000               MOVE INF-PRINT-REQUESTED                                   
255100                                TO RESP-IDMSG-ERROR                       
255200             END-IF                                                       
255300           END-IF                                                         
255400           ADD +1 TO INDX                                                 
255500         END-PERFORM                                                      
255600         IF FORSTA-TRAFF                                                  
255700            CONTINUE                                                      
255800         ELSE                                                             
255900            PERFORM S29-SEND-CLOSE                                        
256000         END-IF                                                           
256100         MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                          
256200         PERFORM MFS-FORM-ATTR                                            
256300         PERFORM MFS-RENSA-FAELT-IN                                       
256400*   * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                
256500       ELSE                                                               
256600         IF REQU-FLGODK-IN = ALL '+'                                      
256700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
256800* * * * * MATA IN NYTT OBJEKT ELLER ÄNDAR BEFINTLIG OBJEKT  * * *         
256900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
257000            PERFORM HAA-KONTR-UPPDAT-OBJ                                  
257100         ELSE                                                             
257200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
257300* * * * * GODKÄNN RAPPORTEN ELLER TA TILLBAKA GODKÄNNANDET * * *          
257400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
257500           PERFORM IMS-GHU-BYTF01                                         
257600           IF SEGMENT-FINNS                                               
257700                                                                          
257800             IF REQU-FLGODK-IN = 'Y' OR 'J' OR 'N'                        
257900               IF REQU-FLGODK-IN = 'Y' OR 'J'                             
258000                 MOVE REQU-IDUSER     TO BYTF-RAPP-IDUSER                 
258100                 MOVE '4'             TO BYTF-RAPP-KDBYTSTA-RAPP          
258200*******  HÄR SÄTTS DATUM FÖR GODKÄNNADE I LOKAL TID                       
258300                 MOVE MSGI-TILOKDAT  TO BYTF-RAPP-DAREGDAT-GODK           
258400                 IF MSGI-TILOKDAT NOT = ZERO                              
258500                   IF MSGI-TILOKDAT < 500000                              
258600                      MOVE 20   TO BYTF-RAPP-DAREGDAT-GODK (1:2)          
258700                   ELSE                                                   
258800                     IF MSGI-TILOKDAT < 999999                            
258900                       MOVE 19  TO BYTF-RAPP-DAREGDAT-GODK (1:2)          
259000                     ELSE                                                 
259100                       MOVE 99999999 TO BYTF-RAPP-DAREGDAT-GODK           
259200                     END-IF                                               
259300                   END-IF                                                 
259400                 END-IF                                                   
259500                 MOVE SPACE           TO BYTF-RAPP-ADBYTANK               
259600                 PERFORM IMS-REPL-BYTF                                    
259700               ELSE                                                       
259800* --- --- HÄR TAS GODKÄNNANDET TILLBAKA!!!!!                              
259900                 MOVE SPACE           TO BYTF-RAPP-IDUSER                 
260000                 MOVE '3'             TO BYTF-RAPP-KDBYTSTA-RAPP          
260100                 MOVE ZERO            TO BYTF-RAPP-DAREGDAT-GODK          
260200                 PERFORM IMS-REPL-BYTF                                    
260300               END-IF                                                     
260400             ELSE                                                         
260500               MOVE ERR-NO-UPPDATE-DONE TO RESP-IDMSG-INFO                
260600               PERFORM MFS-ROER-EJ-FAELT-UT                               
260700               PERFORM MFS-ROER-EJ-FAELT-IN                               
260800               MOVE NEJ TO INDATA-SW                                      
260900             END-IF                                                       
261000           END-IF                                                         
261100         END-IF                                                           
261200         IF INDATA-OK                                                     
261300           MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                        
261400           PERFORM MFS-FORM-ATTR                                          
261500           PERFORM MFS-RENSA-FAELT-IN                                     
261600         END-IF                                                           
261700* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
261800       END-IF                                                             
261900     END-IF                                                               
262000     .                                                                    
262100     EJECT                                                                
262200                                                                          
262300 HAA-KONTR-UPPDAT-OBJ SECTION.                                            
262400     MOVE 'HAA-KONTR-UPPDAT-OBJ'  TO WS-SEKTION                           
262500                                                                          
262600     IF REQU-IDBYTRAD-3173 > ZERO                                         
262700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
262800* * * LÖPNUMMER ÄR IFYLLT RADEN KOMMER FRÅN 3173            * * *         
262900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
263000        MOVE REQU-IDBYTRAD-3173 TO W-IDBYTRAD                             
263100        MOVE W-IDBYTRAD   TO WS-IDBYTRAD                                  
263200        IF WS-IDBYTRAD < 100                                              
263300           PERFORM HD-GM-VIPS-ANV-IDBYTRAD                                
263400        ELSE                                                              
263500           MOVE ZERO     TO WS-SPAR-KVRETUR-URSP                          
263600           PERFORM IMS-GHU-BYTF11                                         
263700           IF SEGMENT-FINNS                                               
263800              INSPECT REQU-IDARTNR-OBJ-SPAERR                             
263900                      REPLACING LEADING SPACE BY ZERO                     
264000              MOVE REQU-IDARTNR-OBJ-SPAERR                                
264100              TO WS-SPAR-OBJNR-SPAERR                                     
264200              PERFORM HF-UPPDAT-NY-RAD                                    
264300           END-IF                                                         
264400        END-IF                                                            
264500     ELSE                                                                 
264600********************************************************                  
264700****** LÄGGER UPP EN NY OKOPPLAD RAD PÅ WDM611  ********                  
264800****** DIREKT FRÅN 3172                         ********                  
264900********************************************************                  
265000       PERFORM HC-DIREKT-INMATNING                                        
265100     END-IF                                                               
265200     IF INDATA-OK AND ALLT-OK                                             
265300       MOVE REQU-KDBYTREF-IN TO TEST-KDBYTREF                             
265400       IF REQU-IDMSGVER = '101' AND                                       
265500          REQU-IDDC-KEY = WC-SDC-NL-ET AND                                
265600          NOT BYT15-KDBYTREF-REMOVE AND                                   
265700          NOT BYT15-KDBYTREF-GAR-EJ-SALD AND                              
265710          (REQU-FLSKROT-IN = '+' OR SPACE)                                
265800         MOVE FUNCTION CURRENT-DATE(1:12)                                 
265900                                       TO WS-CURRENT-DATE-TIME            
266000         MOVE '1        '            TO CORE-IDAFPRCD                     
266100         MOVE WS-YEAR                TO LBL-TIAAAA                        
266200         MOVE WS-MONTH               TO LBL-TIMM                          
266300         MOVE WS-DAY                 TO LBL-TIDD                          
266400         MOVE WS-HOUR                TO LBL-TIHH                          
266500         MOVE WS-MINUTE              TO LBL-TIMIN                         
266600         MOVE CORE-DATE              TO CORE-PRINT-DATE                   
266700         MOVE CORE-TIME              TO CORE-TIHHMM                       
266800         MOVE REQU-IDUSER            TO CORE-IDUSER                       
266803         MOVE REQU-IDARTNR-OBJ-SPAERR                                     
266804                                     TO CORE-IDARTNR-OBJ                  
266805                                        WS-IDARTNR                        
266806                                        TEST-IDARTNR                      
266807                                        W-IDARTNR                         
266808         PERFORM IMS-GET-BENA-GU                                          
266809         IF SEGMENT-FINNS                                                 
266811           MOVE BENA-TEXT-BEART                                           
266820                                 TO CORE-BEART                            
266830         END-IF                                                           
267210         IF BYT16-RADIO                                                   
267300           IF BYT16-RADIO-EXTRA                                           
267400             MOVE 0              TO WS-ARTSIFFRA                          
267500           ELSE                                                           
267600             MOVE 3              TO WS-ARTSIFFRA                          
267700           END-IF                                                         
267800         ELSE                                                             
267900           IF ART-0                                                       
268000             MOVE 0              TO WS-ARTSIFFRA                          
268100           ELSE                                                           
268200             IF ART-1                                                     
268300               MOVE 1            TO WS-ARTSIFFRA                          
268400             ELSE                                                         
268500               IF ART-2                                                   
268600                 MOVE 2          TO WS-ARTSIFFRA                          
268700               ELSE                                                       
268800                 IF ART-3                                                 
268900                   MOVE 3        TO WS-ARTSIFFRA                          
269000                 END-IF                                                   
269100               END-IF                                                     
269200             END-IF                                                       
269300           END-IF                                                         
269400         END-IF                                                           
269500         MOVE WS-IDARTNR         TO BYART-IDARTNR-BYT                     
269600         PERFORM DB2-SELECT-BYART                                         
269700         IF RADER-FINNS                                                   
269800           MOVE BYART-ADLAGOMR   TO CORE-ADLAGOMR                         
269900           MOVE BYART-ADGANG     TO CORE-ADGANG                           
270000           MOVE BYART-ADPLATS    TO CORE-ADPLATS                          
270100         ELSE                                                             
270200           MOVE ZERO             TO CORE-ADLAGOMR                         
270300                                      CORE-ADGANG                         
270400                                      CORE-ADPLATS                        
270500         END-IF                                                           
270600         PERFORM S21-SEND-OPEN                                            
270700         PERFORM S22-PUT-HEADER                                           
270800         MOVE 1 TO WS-PRINT-ANTAL                                         
270900         PERFORM UNTIL WS-PRINT-ANTAL > REQU-KVRETUR-IN                   
271000           PERFORM S25-PUT-LINE                                           
271100           ADD 1 TO WS-PRINT-ANTAL                                        
271200         END-PERFORM                                                      
271300         PERFORM S29-SEND-CLOSE                                           
271400         MOVE INF-PRINT-REQUESTED TO RESP-IDMSG-ERROR                     
271500                                                                          
271600       END-IF                                                             
271700     END-IF                                                               
271800     .                                                                    
271900     EJECT                                                                
272000                                                                          
272100 HAB-AVGOR-STATUS-RADEN SECTION.                                          
272200     MOVE 'HAB-AVGOR-STATUS-RADEN'  TO WS-SEKTION                         
272300                                                                          
272400     IF BYTF-OBJ-KDBYTSTA-OBJ = 'N'                                       
272500     OR BYTF-OBJ-KDBYTSTA-OBJ = 'E'                                       
272600        CONTINUE                                                          
272700     ELSE                                                                 
272800        IF BYTF-OBJ-KVRETUR-URSP =                                        
272900           BYTF-OBJ-KVRETUR-GODK                                          
273000           MOVE REQU-IDBYTRAD-LINE (INDX) TO WS-LOPNUMMER                 
273100           MOVE 99                  TO WS-LOPNUMMER2                      
273200           MOVE WS-LOPNUMMER        TO  W-IDBYTRAD-MAX                    
273300           MOVE 00                  TO WS-LOPNUMMER2                      
273400           MOVE WS-LOPNUMMER        TO  W-IDBYTRAD-MIN                    
273500           PERFORM IMS-GU-BYTF01-SPEC                                     
273600           PERFORM IMS-GNP-SPEC-BYTF11                                    
273700           IF SEGMENT-SAKNAS                                              
273800             MOVE '4'  TO BYTF-OBJ-KDBYTSTA-OBJ                           
273900           ELSE                                                           
274000             MOVE 'C'  TO BYTF-OBJ-KDBYTSTA-OBJ                           
274100           END-IF                                                         
274200        ELSE                                                              
274300           MOVE 'C'  TO BYTF-OBJ-KDBYTSTA-OBJ                             
274400        END-IF                                                            
274500     END-IF                                                               
274600                                                                          
274700     .                                                                    
274800     EJECT                                                                
274900                                                                          
275000 HAC-AVGOR-STATUS-I-DLT SECTION.                                          
275100     MOVE 'HAC-AVGOR-STATUS-I-DLT'  TO WS-SEKTION                         
275200                                                                          
275300     IF BYTF-OBJ-IDBYTRAD < 100                                           
275400        CONTINUE                                                          
275500     ELSE                                                                 
275600        IF BYTF-OBJ-KDBYTSTA-OBJ = 'N'                                    
275700           MOVE REQU-IDBYTRAD-LINE (INDX) TO WS-LOPNUMMER                 
275800           MOVE 99                  TO WS-LOPNUMMER2                      
275900           MOVE WS-LOPNUMMER        TO W-IDBYTRAD-MAX                     
276000           MOVE 00                  TO WS-LOPNUMMER2                      
276100           MOVE WS-LOPNUMMER        TO W-IDBYTRAD-MIN                     
276200                                       W-IDBYTRAD                         
276300           PERFORM IMS-GU-BYTF01-SPEC                                     
276400           PERFORM IMS-GNP-SPEC-BYTF11                                    
276500           IF SEGMENT-FINNS                                               
276600              PERFORM IMS-GHU-BYTF11                                      
276700              MOVE 'C'  TO BYTF-OBJ-KDBYTSTA-OBJ                          
276800           ELSE                                                           
276900              PERFORM IMS-GHU-BYTF11                                      
277000              IF BYTF-OBJ-KVRETUR-URSP =                                  
277100                 BYTF-OBJ-KVRETUR-GODK                                    
277200                 MOVE '4'  TO BYTF-OBJ-KDBYTSTA-OBJ                       
277300              ELSE                                                        
277400                 MOVE 'C'  TO BYTF-OBJ-KDBYTSTA-OBJ                       
277500              END-IF                                                      
277600           END-IF                                                         
277700           PERFORM IMS-REPL-BYTF                                          
277800        END-IF                                                            
277900     END-IF                                                               
278000**************************************************************            
278100     IF BYTF-OBJ-KDBYTSTA-OBJ = 'N'                                       
278200     OR BYTF-OBJ-KDBYTSTA-OBJ = 'E'                                       
278300        CONTINUE                                                          
278400     ELSE                                                                 
278500        IF BYTF-OBJ-KVRETUR-URSP =                                        
278600           BYTF-OBJ-KVRETUR-GODK                                          
278700           MOVE REQU-IDBYTRAD-LINE (INDX) TO WS-LOPNUMMER                 
278800           MOVE 99                  TO WS-LOPNUMMER2                      
278900           MOVE WS-LOPNUMMER        TO  W-IDBYTRAD-MAX                    
279000           MOVE 00                  TO WS-LOPNUMMER2                      
279100           MOVE WS-LOPNUMMER        TO  W-IDBYTRAD-MIN                    
279200           PERFORM IMS-GU-BYTF01-SPEC                                     
279300           PERFORM IMS-GNP-SPEC-BYTF11                                    
279400           IF SEGMENT-SAKNAS                                              
279500             MOVE '4'  TO BYTF-OBJ-KDBYTSTA-OBJ                           
279600           ELSE                                                           
279700             MOVE 'C'  TO BYTF-OBJ-KDBYTSTA-OBJ                           
279800           END-IF                                                         
279900        ELSE                                                              
280000           MOVE 'C'  TO BYTF-OBJ-KDBYTSTA-OBJ                             
280100        END-IF                                                            
280200     END-IF                                                               
280300                                                                          
280400                                                                          
280500     .                                                                    
280600     EJECT                                                                
280700 HB-GAMMAL-VIPS-ANVANDARE SECTION.                                        
280800     MOVE 'HB-GAMMAL-VIPS'  TO WS-SEKTION                                 
280900*******************************************************                   
281000**** GAMMAL VIPSANVÄNDARE LÄGGER UPP EN NY RAD MEN   **                   
281100**** DET FÅR ENDAST FINNAS EN RAD PER OBJEKTNR       **                   
281200*******************************************************                   
281300                                                                          
281400     MOVE REQU-IDARTNR-OBJ-SPAERR TO W-IDARTNR-OBJ                        
281500     MOVE NEJ                      TO ARTIKEL-SW                          
281600     IF REQU-KDBYTREF-IN = ALL '+'                                        
281700        MOVE SPACE                 TO WS-ANMARK                           
281800     ELSE                                                                 
281900        MOVE REQU-KDBYTREF-IN      TO WS-ANMARK                           
282000     END-IF                                                               
282001     IF REQU-FLSKROT-IN = ALL '+'                                         
282002        MOVE SPACE                 TO WS-FLSKROT                          
282003     ELSE                                                                 
282004        MOVE REQU-FLSKROT-IN       TO WS-FLSKROT                          
282005     END-IF                                                               
282100     MOVE ZERO                     TO W-IDBYTRAD-MIN                      
282200     MOVE 00099                    TO W-IDBYTRAD-MAX                      
282300     MOVE ZERO                     TO WS-IDBYTRAD                         
282400     PERFORM IMS-GU-BYTF01                                                
282500     IF SEGMENT-FINNS                                                     
282600        MOVE  BYTF-RAPP-IDBYTRAP  TO W-RAPP-IDBYTRAP                      
282700        PERFORM UNTIL SEGMENT-SAKNAS                                      
282800        OR   ARTIKEL-LIKA                                                 
282900             PERFORM IMS-GHNP-BYTF11-IMS                                  
283000             IF SEGMENT-FINNS                                             
283100                IF W-IDARTNR-OBJ = BYTF-OBJ-IDARTNR-OBJ                   
283200                    MOVE JA    TO ARTIKEL-SW                              
283300                END-IF                                                    
283400                MOVE BYTF-OBJ-IDBYTRAD TO WS-IDBYTRAD                     
283500             END-IF                                                       
283600                                                                          
283700        END-PERFORM                                                       
283800                                                                          
283900        IF ARTIKEL-LIKA                                                   
284000           MOVE ZERO                TO WS-SPAR-KVRETUR-GODK               
284100           INSPECT REQU-KVRETUR-IN                                        
284200                      REPLACING LEADING SPACE BY ZERO                     
284300           MOVE REQU-KVRETUR-IN TO WS-SPAR-KVRETUR-GODK                   
284400           ADD WS-SPAR-KVRETUR-GODK TO BYTF-OBJ-KVRETUR-GODK              
284500           IF WS-ANMARK  = BYTF-OBJ-KDBYTREF AND                          
284510              WS-FLSKROT = BYTF-OBJ-FLSKROT                               
284600              PERFORM IMS-REPL-BYTF                                       
284700              MOVE BYTF-OBJ-IDBYTRAD TO W-IDBYTRAD                        
284800           ELSE                                                           
284900               MOVE CORE-ALREADY-EXISTS TO RESP-IDMSG-ERROR               
285000               PERFORM MFS-ROER-EJ-FAELT-UT                               
285100               PERFORM MFS-ROER-EJ-FAELT-IN                               
285200               MOVE NEJ TO INDATA-SW                                      
285300               MOVE NEJ TO ALLT-SW                                        
285400               MOVE MFS-ADD-SAETT-CURSOR TO RESP-KDBYTREF-IN-ATTR         
285500           END-IF                                                         
285600        ELSE                                                              
285700           ADD +1             TO WS-IDBYTRAD                              
285800           MOVE WS-IDBYTRAD TO BYTF-OBJ-IDBYTRAD                          
285900           PERFORM HBB-FLYTTA-TILL-FAELT                                  
286000           IF WS-IDBYTRAD < 100                                           
286100              PERFORM IMS-ISRT-BYTF11                                     
286200              MOVE BYTF-OBJ-IDBYTRAD TO W-IDBYTRAD                        
286300           END-IF                                                         
286310           IF W-RAPP-IDDC = WC-CDC-SE OR WC-NDC-US-BAT OR                 
286311              WC-NDC-TH-93                                                
286320             CONTINUE                                                     
286330           ELSE                                                           
286400             MOVE REQU-IDARTNR-OBJ-SPAERR TO W-IDARTNR-K7                 
286500                                      W-IDARTNR-K6                        
286600             MOVE WC-SDC-NL-ET TO W-IDDC-K7                               
286700             PERFORM IMS-GHU-WDK711                                       
286800             IF SEGMENT-SAKNAS                                            
286900               PERFORM S11-NYA-SEGMENT-WDK7                               
287000             END-IF                                                       
287010           END-IF                                                         
287100        END-IF                                                            
287200     END-IF                                                               
287300                                                                          
287400     .                                                                    
287500     EJECT                                                                
287600                                                                          
287700 HBB-FLYTTA-TILL-FAELT SECTION.                                           
287800     MOVE 'HBB-FLYTT-TILL-FAELT'  TO WS-SEKTION                           
287900                                                                          
288000     IF REQU-KDBYTREF-IN NOT = ALL '+'                                    
288100        MOVE REQU-KDBYTREF-IN TO BYTF-OBJ-KDBYTREF                        
288200     ELSE                                                                 
288300        MOVE SPACE            TO BYTF-OBJ-KDBYTREF                        
288400     END-IF                                                               
288401     IF REQU-FLSKROT-IN NOT = ALL '+'                                     
288402        MOVE REQU-FLSKROT-IN TO BYTF-OBJ-FLSKROT                          
288403     ELSE                                                                 
288404        MOVE SPACE           TO BYTF-OBJ-FLSKROT                          
288405     END-IF                                                               
288500     INSPECT REQU-IDARTNR-OBJ-SPAERR                                      
288600             REPLACING LEADING SPACE BY ZERO                              
288700     MOVE REQU-IDARTNR-OBJ-SPAERR TO BYTF-OBJ-IDARTNR-OBJ                 
288800     MOVE 'E'              TO BYTF-OBJ-KDBYTSTA-OBJ                       
288900     INSPECT REQU-KVRETUR-IN                                              
289000             REPLACING LEADING SPACE BY ZERO                              
289100     MOVE REQU-KVRETUR-IN TO BYTF-OBJ-KVRETUR-GODK                        
289200     MOVE ZERO             TO BYTF-OBJ-KVRETUR-URSP                       
289300     MOVE ZERO             TO BYTF-OBJ-IDTABNR                            
289400     MOVE ZERO             TO BYTF-OBJ-IDORDER                            
289500     MOVE SPACE            TO BYTF-OBJ-BERADREF                           
289600     MOVE SPACE            TO BYTF-OBJ-KDBYTSTA-AVL                       
289800     COMPUTE  BYTF-OBJ-IDBYTRAP-9KOMPL =                                  
289900                              W-9KOMPL - W-RAPP-IDBYTRAP                  
290000                                                                          
290100     .                                                                    
290200     EJECT                                                                
290300                                                                          
290400 HC-DIREKT-INMATNING SECTION.                                             
290500     MOVE 'HC-DIREKT-INMATNING'  TO WS-SEKTION                            
290600******************************************************************        
290700**** DIREKT INMATNING AV EN BILD PÅ 3172 OKOPPLAD RAD SKAPAS *****        
290800******************************************************************        
290900        MOVE ZERO             TO WS-IDBYTRAD                              
291000        MOVE REQU-IDBYTRAD-LINE (1) TO WS-IDBYTRAD                        
291100        IF WS-IDBYTRAD < 100                                              
291200           IF WS-IDBYTRAD > ZERO                                          
291300              PERFORM HB-GAMMAL-VIPS-ANVANDARE                            
291400           ELSE                                                           
291500              PERFORM HH-IDBYTRAD-SAKNAS                                  
291600           END-IF                                                         
291700        ELSE                                                              
291800           PERFORM HCA-SKAPA-OKOPLAD-RAD                                  
291900        END-IF                                                            
292000                                                                          
292100     .                                                                    
292200     EJECT                                                                
292300                                                                          
292400 HCA-SKAPA-OKOPLAD-RAD SECTION.                                           
292500     MOVE 'HCA-SKAPA-OKOPLAD-RAD'  TO WS-SEKTION                          
292600                                                                          
292700     INSPECT REQU-IDARTNR-OBJ-SPAERR                                      
292800                  REPLACING LEADING SPACE BY ZERO                         
292900     MOVE REQU-IDARTNR-OBJ-SPAERR TO SW-IDARTNR-OBJ                       
293000     IF REQU-KDBYTREF-IN = ALL '+'                                        
293100     OR REQU-KDBYTREF-IN = ALL ' '                                        
293200        MOVE SPACE         TO WS-ANMARK                                   
293300     ELSE                                                                 
293400        MOVE REQU-KDBYTREF-IN TO WS-ANMARK                                
293500     END-IF                                                               
293501     IF REQU-FLSKROT-IN = ALL '+'                                         
293502     OR REQU-FLSKROT-IN = ALL ' '                                         
293503        MOVE SPACE           TO WS-FLSKROT                                
293504     ELSE                                                                 
293505        MOVE REQU-FLSKROT-IN TO WS-FLSKROT                                
293506     END-IF                                                               
293600     MOVE NEJ              TO ARTIKEL-SW                                  
293700     MOVE 09000            TO W-IDBYTRAD-MIN                              
293800     MOVE 09999            TO W-IDBYTRAD-MAX                              
293900     MOVE ZERO             TO WS-IDBYTRAD                                 
294000     PERFORM IMS-GU-BYTF01                                                
294100     IF SEGMENT-FINNS                                                     
294200        MOVE BYTF-RAPP-IDBYTRAP  TO W-RAPP-IDBYTRAP                       
294300        PERFORM UNTIL SEGMENT-SAKNAS                                      
294400        OR ARTIKEL-LIKA                                                   
294500           PERFORM IMS-GHNP-BYTF11-IMS                                    
294600           IF SEGMENT-FINNS                                               
294700              IF SW-IDARTNR-OBJ = BYTF-OBJ-IDARTNR-OBJ                    
294800              AND WS-ANMARK  = BYTF-OBJ-KDBYTREF                          
294810              AND WS-FLSKROT = BYTF-OBJ-FLSKROT                           
294900                 MOVE JA    TO ARTIKEL-SW                                 
295000              END-IF                                                      
295100              MOVE BYTF-OBJ-IDBYTRAD TO WS-IDBYTRAD                       
295200           END-IF                                                         
295300                                                                          
295400        END-PERFORM                                                       
295500        IF ARTIKEL-LIKA                                                   
295600           MOVE ZERO         TO WS-SPAR-KVRETUR-GODK                      
295700           INSPECT REQU-KVRETUR-IN                                        
295800                     REPLACING LEADING SPACE BY ZERO                      
295900           MOVE REQU-KVRETUR-IN TO WS-SPAR-KVRETUR-GODK                   
296000           ADD WS-SPAR-KVRETUR-GODK TO                                    
296100                                     BYTF-OBJ-KVRETUR-GODK                
296200           PERFORM IMS-REPL-BYTF                                          
296300           MOVE BYTF-OBJ-IDBYTRAD TO W-IDBYTRAD                           
296400        ELSE                                                              
296500          IF WS-IDBYTRAD = ZERO                                           
296600             MOVE 09000      TO WS-IDBYTRAD                               
296700          ELSE                                                            
296800             ADD +1          TO WS-IDBYTRAD                               
296900          END-IF                                                          
297000          MOVE WS-IDBYTRAD TO BYTF-OBJ-IDBYTRAD                           
297100          PERFORM HCB-FLYTTA-TILL-FAELT                                   
297110          IF W-RAPP-IDDC = WC-CDC-SE OR WC-NDC-US-BAT OR                  
297111             WC-NDC-TH-93                                                 
297120            CONTINUE                                                      
297130          ELSE                                                            
297200            MOVE WC-SDC-NL-ET TO W-IDDC-K7                                
297300            PERFORM IMS-GHU-WDK711                                        
297400            IF SEGMENT-SAKNAS                                             
297500              PERFORM S11-NYA-SEGMENT-WDK7                                
297600            END-IF                                                        
297610          END-IF                                                          
297700          PERFORM IMS-ISRT-BYTF11                                         
297800          MOVE BYTF-OBJ-IDBYTRAD TO W-IDBYTRAD                            
297900        END-IF                                                            
298000     END-IF                                                               
298100     .                                                                    
298200     EJECT                                                                
298300                                                                          
298400 HCB-FLYTTA-TILL-FAELT SECTION.                                           
298500     MOVE 'HCB-FLYTTA-TILL-FAELT'  TO WS-SEKTION                          
298600                                                                          
298700     IF REQU-KDBYTREF-IN NOT = ALL '+'                                    
298800        MOVE REQU-KDBYTREF-IN TO BYTF-OBJ-KDBYTREF                        
298900     ELSE                                                                 
299000        MOVE SPACE         TO BYTF-OBJ-KDBYTREF                           
299100     END-IF                                                               
299101     IF REQU-FLSKROT-IN NOT = ALL '+'                                     
299102        MOVE REQU-FLSKROT-IN TO BYTF-OBJ-FLSKROT                          
299103     ELSE                                                                 
299104        MOVE SPACE         TO BYTF-OBJ-FLSKROT                            
299105     END-IF                                                               
299200     INSPECT REQU-IDARTNR-OBJ-SPAERR                                      
299300             REPLACING LEADING SPACE BY ZERO                              
299400     MOVE REQU-IDARTNR-OBJ-SPAERR TO BYTF-OBJ-IDARTNR-OBJ                 
299500                              W-IDARTNR-K7                                
299600                              W-IDARTNR-K6                                
299700     MOVE 'E'              TO BYTF-OBJ-KDBYTSTA-OBJ                       
299800     INSPECT REQU-KVRETUR-IN                                              
299900             REPLACING LEADING SPACE BY ZERO                              
300000     MOVE REQU-KVRETUR-IN TO BYTF-OBJ-KVRETUR-GODK                        
300100     MOVE ZERO             TO BYTF-OBJ-KVRETUR-URSP                       
300200     MOVE ZERO             TO BYTF-OBJ-IDTABNR                            
300300     MOVE ZERO             TO BYTF-OBJ-IDORDER                            
300400     MOVE SPACE            TO BYTF-OBJ-BERADREF                           
300500     MOVE SPACE            TO BYTF-OBJ-KDBYTSTA-AVL                       
300700     COMPUTE  BYTF-OBJ-IDBYTRAP-9KOMPL =                                  
300800                                W-9KOMPL - W-RAPP-IDBYTRAP                
300900     .                                                                    
301000     EJECT                                                                
301100                                                                          
301200 HD-GM-VIPS-ANV-IDBYTRAD SECTION.                                         
301300     MOVE 'HD-GM-VIPS-ANV-IDBYTRAD' TO WS-SEKTION                         
301400                                                                          
301500     MOVE NEJ                      TO ARTIKEL-SW                          
301600     IF REQU-KDBYTREF-IN = ALL '+'                                        
301700        MOVE SPACE         TO WS-ANMARK                                   
301800     ELSE                                                                 
301900        MOVE REQU-KDBYTREF-IN TO WS-ANMARK                                
302000     END-IF                                                               
302001     IF REQU-FLSKROT-IN = ALL '+'                                         
302002        MOVE SPACE           TO WS-FLSKROT                                
302003     ELSE                                                                 
302004        MOVE REQU-FLSKROT-IN TO WS-FLSKROT                                
302005     END-IF                                                               
302100     MOVE ZERO                     TO W-IDBYTRAD-MIN                      
302200     MOVE ZERO                     TO WS-IDBYTRAD                         
302300     MOVE 00099                    TO W-IDBYTRAD-MAX                      
302400     INSPECT REQU-IDARTNR-OBJ-SPAERR                                      
302500             REPLACING LEADING SPACE BY ZERO                              
302600     MOVE REQU-IDARTNR-OBJ-SPAERR TO SW-IDARTNR-OBJ                       
302700     MOVE ZERO                     TO WS-IDBYTRAD                         
302800     PERFORM IMS-GU-BYTF01                                                
302900     IF SEGMENT-FINNS                                                     
303000        MOVE BYTF-RAPP-IDBYTRAP    TO W-RAPP-IDBYTRAP                     
303100        PERFORM UNTIL SEGMENT-SAKNAS                                      
303200                 OR   ARTIKEL-LIKA                                        
303300          PERFORM IMS-GHNP-BYTF11-IMS                                     
303400          IF SEGMENT-FINNS                                                
303500             IF SW-IDARTNR-OBJ = BYTF-OBJ-IDARTNR-OBJ                     
303600                MOVE JA    TO ARTIKEL-SW                                  
303700             END-IF                                                       
303800             MOVE BYTF-OBJ-IDBYTRAD TO WS-IDBYTRAD                        
303900          END-IF                                                          
304000                                                                          
304100        END-PERFORM                                                       
304200        IF ARTIKEL-LIKA                                                   
304300           MOVE BYTF-OBJ-KDBYTSTA-OBJ TO WS-SPAR-KDBYTSTA-OBJ             
304400           MOVE BYTF-OBJ-KVRETUR-URSP TO WS-SPAR-KVRETUR-URSP             
304500           MOVE REQU-KVRETUR-IN TO WS-SPAR-KVRETUR-GODK                   
304600           ADD WS-SPAR-KVRETUR-GODK TO BYTF-OBJ-KVRETUR-GODK              
304700           IF BYTF-OBJ-KVRETUR-GODK = BYTF-OBJ-KVRETUR-URSP               
304800              IF BYTF-OBJ-KDBYTSTA-OBJ NOT = 'N'                          
304900                 MOVE '4'              TO BYTF-OBJ-KDBYTSTA-OBJ           
305000              END-IF                                                      
305100           ELSE                                                           
305200             IF BYTF-OBJ-KDBYTSTA-OBJ NOT = 'N'                           
305300                MOVE 'C'              TO BYTF-OBJ-KDBYTSTA-OBJ            
305400             END-IF                                                       
305500           END-IF                                                         
305600           IF WS-ANMARK = BYTF-OBJ-KDBYTREF AND                           
305700              WS-FLSKROT = BYTF-OBJ-FLSKROT                               
306000              PERFORM IMS-REPL-BYTF                                       
306100              IF WS-SPAR-KDBYTSTA-OBJ = SPACE                             
306200                 PERFORM IMS-GHU-BYTF01                                   
306300                 IF SEGMENT-FINNS                                         
306400                    SUBTRACT WS-SPAR-KVRETUR-URSP FROM                    
306500                    BYTF-RAPP-KVRETUR-TOT                                 
306600                    PERFORM IMS-REPL-BYTF                                 
306700                 END-IF                                                   
306800              END-IF                                                      
306900              MOVE ZERO            TO RESP-IDBYTRAD-3173                  
307000              MOVE BYTF-OBJ-IDBYTRAD TO W-IDBYTRAD                        
307100           ELSE                                                           
307200              MOVE CORE-ALREADY-EXISTS TO RESP-IDMSG-ERROR                
307300              PERFORM MFS-ROER-EJ-FAELT-UT                                
307400              PERFORM MFS-ROER-EJ-FAELT-IN                                
307500              MOVE MFS-ADD-SAETT-CURSOR TO RESP-KDBYTREF-IN-ATTR          
307600              MOVE NEJ TO INDATA-SW                                       
307700              MOVE NEJ TO ALLT-SW                                         
307800           END-IF                                                         
307900        ELSE                                                              
308000******************************************************************        
308100** HAR ÄNDRAT OBJEKTNR I TRANS 3173, DÄRFÖR SAKNAS RADEN I WDM611*        
308200******************************************************************        
308300           ADD +1                TO WS-IDBYTRAD                           
308400           MOVE WS-IDBYTRAD      TO BYTF-OBJ-IDBYTRAD                     
308500           PERFORM HBB-FLYTTA-TILL-FAELT                                  
308600           IF WS-IDBYTRAD < 100                                           
308610              IF W-RAPP-IDDC = WC-CDC-SE OR WC-NDC-US-BAT OR              
308611                 WC-NDC-TH-93                                             
308620                CONTINUE                                                  
308630              ELSE                                                        
308700                MOVE REQU-IDARTNR-OBJ-SPAERR TO W-IDARTNR-K7              
308800                                                W-IDARTNR-K6              
308900                MOVE WC-SDC-NL-ET TO W-IDDC-K7                            
309000                PERFORM IMS-GHU-WDK711                                    
309100                IF SEGMENT-SAKNAS                                         
309200                   PERFORM S11-NYA-SEGMENT-WDK7                           
309300                END-IF                                                    
309310              END-IF                                                      
309400              PERFORM IMS-ISRT-BYTF11                                     
309500           END-IF                                                         
309600           MOVE BYTF-OBJ-IDBYTRAD TO W-IDBYTRAD                           
309700           MOVE ZERO            TO RESP-IDBYTRAD-3173                     
309800        END-IF                                                            
309900     END-IF                                                               
310000     .                                                                    
310100     EJECT                                                                
310200                                                                          
310300 HF-UPPDAT-NY-RAD SECTION.                                                
310400     MOVE 'HF-UPPDAT-NY-RAD' TO WS-SEKTION                                
310500******************************************************************        
310600**** INMATAD ANMARK SKILJER SIG FRÅN TABELLENS ANNMÄRKNING  ******        
310700**** DÄRFÖR LÄGGS EN HELT NY RAD UPP                        ******        
310800******************************************************************        
310900     IF BYTF-OBJ-KDBYTSTA-OBJ = 'E'                                       
311000        PERFORM HC-DIREKT-INMATNING                                       
311100     ELSE                                                                 
311200        MOVE NEJ                   TO ARTIKEL-SW                          
311300        MOVE WS-IDBYTRAD           TO WS-LOPNUMMER                        
311400        IF REQU-KDBYTREF-IN = ALL '+'                                     
311500           MOVE SPACE              TO WS-ANMARK                           
311600        ELSE                                                              
311700           MOVE REQU-KDBYTREF-IN   TO WS-ANMARK                           
311800        END-IF                                                            
311801        IF REQU-FLSKROT-IN = ALL '+'                                      
311802           MOVE SPACE              TO WS-FLSKROT                          
311803        ELSE                                                              
311804           MOVE REQU-FLSKROT-IN    TO WS-FLSKROT                          
311805        END-IF                                                            
311900        IF WS-LOPNUMMER2 = ZERO                                           
312000           MOVE WS-LOPNUMMER       TO W-IDBYTRAD-MIN                      
312100           MOVE +99                TO WS-LOPNUMMER2                       
312200           MOVE WS-LOPNUMMER       TO W-IDBYTRAD-MAX                      
312300        ELSE                                                              
312400           MOVE ZERO               TO WS-LOPNUMMER2                       
312500           MOVE WS-LOPNUMMER       TO W-IDBYTRAD-MIN                      
312600           MOVE +99                TO WS-LOPNUMMER2                       
312700           MOVE WS-LOPNUMMER       TO W-IDBYTRAD-MAX                      
312800        END-IF                                                            
312900        MOVE ZERO                  TO WS-IDBYTRAD                         
313000                                                                          
313100        PERFORM IMS-GU-BYTF01                                             
313200        IF SEGMENT-FINNS                                                  
313300           MOVE BYTF-RAPP-IDBYTRAP  TO W-RAPP-IDBYTRAP                    
313400           PERFORM UNTIL SEGMENT-SAKNAS                                   
313500           OR ARTIKEL-LIKA                                                
313600              PERFORM IMS-GHNP-BYTF11-IMS                                 
313700              IF SEGMENT-FINNS                                            
313800                 IF WS-SPAR-OBJNR-SPAERR = BYTF-OBJ-IDARTNR-OBJ           
313900                 AND WS-ANMARK  = BYTF-OBJ-KDBYTREF                       
313910                 AND WS-FLSKROT = BYTF-OBJ-FLSKROT                        
314000                    MOVE JA        TO ARTIKEL-SW                          
314100                 END-IF                                                   
314200                 MOVE BYTF-OBJ-IDBYTRAD TO WS-IDBYTRAD                    
314300              END-IF                                                      
314400           END-PERFORM                                                    
314500           IF ARTIKEL-LIKA                                                
314600              MOVE BYTF-OBJ-KDBYTSTA-OBJ TO WS-SPAR-KDBYTSTA-OBJ          
314700              MOVE BYTF-OBJ-KVRETUR-URSP TO WS-SPAR-KVRETUR-URSP          
314800              MOVE REQU-KVRETUR-IN TO WS-SPAR-KVRETUR-GODK                
314900              ADD WS-SPAR-KVRETUR-GODK TO BYTF-OBJ-KVRETUR-GODK           
315000              IF BYTF-OBJ-KVRETUR-GODK = BYTF-OBJ-KVRETUR-URSP            
315100                 IF BYTF-OBJ-KDBYTSTA-OBJ NOT = 'N'                       
315200                    MOVE '4'       TO BYTF-OBJ-KDBYTSTA-OBJ               
315300                 END-IF                                                   
315400              ELSE                                                        
315500                IF BYTF-OBJ-KDBYTSTA-OBJ NOT = 'N'                        
315600                   MOVE 'C'       TO BYTF-OBJ-KDBYTSTA-OBJ                
315700                END-IF                                                    
315800              END-IF                                                      
316200              PERFORM IMS-REPL-BYTF                                       
316300              MOVE BYTF-OBJ-IDBYTRAD TO W-IDBYTRAD                        
316400              MOVE ZERO              TO RESP-IDBYTRAD-3173                
316500              IF WS-SPAR-KDBYTSTA-OBJ = SPACE                             
316600                 PERFORM IMS-GHU-BYTF01                                   
316700                 IF SEGMENT-FINNS                                         
316800                    SUBTRACT WS-SPAR-KVRETUR-URSP FROM                    
316900                    BYTF-RAPP-KVRETUR-TOT                                 
317000                    PERFORM IMS-REPL-BYTF                                 
317100                 END-IF                                                   
317200              END-IF                                                      
317300           ELSE                                                           
317400******************************************************************        
317500**** LÄGGER UPP EN NY KOPPLAD RAD MED STATUS N              ******        
317600**** KONTROLLERA STATUS PÅ HUVUDRADEN STATUS SKA VARA C     ******        
317700******************************************************************        
317800             ADD +1          TO WS-IDBYTRAD                               
317900             MOVE WS-IDBYTRAD TO BYTF-OBJ-IDBYTRAD                        
318000             PERFORM HFF-FLYTTA-TILL-FAELT                                
318010             IF W-RAPP-IDDC = WC-CDC-SE OR WC-NDC-US-BAT OR               
318011                WC-NDC-TH-93                                              
318020               CONTINUE                                                   
318030             ELSE                                                         
318100               MOVE REQU-IDARTNR-OBJ-SPAERR TO W-IDARTNR-K7               
318200                                               W-IDARTNR-K6               
318300               MOVE WC-SDC-NL-ET TO W-IDDC-K7                             
318400               PERFORM IMS-GHU-WDK711                                     
318500               IF SEGMENT-SAKNAS                                          
318600                 PERFORM S11-NYA-SEGMENT-WDK7                             
318700               END-IF                                                     
318710             END-IF                                                       
318800             PERFORM IMS-ISRT-BYTF11                                      
318900             MOVE BYTF-OBJ-IDBYTRAD TO W-IDBYTRAD                         
319000             MOVE ZERO       TO RESP-IDBYTRAD-3173                        
319100             PERFORM HFG-KONTR-HUVUD-RAD-STATUS                           
319200           END-IF                                                         
319300        END-IF                                                            
319400     END-IF                                                               
319500     .                                                                    
319600     EJECT                                                                
319700                                                                          
319800 HFF-FLYTTA-TILL-FAELT SECTION.                                           
319900     MOVE 'HFF-FLYTTA-TILL-FAELT' TO WS-SEKTION                           
320000                                                                          
320100     IF REQU-KDBYTREF-IN NOT = ALL '+'                                    
320200        MOVE REQU-KDBYTREF-IN TO BYTF-OBJ-KDBYTREF                        
320300     ELSE                                                                 
320400        MOVE SPACE         TO BYTF-OBJ-KDBYTREF                           
320500     END-IF                                                               
320501     IF REQU-FLSKROT-IN NOT = ALL '+'                                     
320502        MOVE REQU-FLSKROT-IN TO BYTF-OBJ-FLSKROT                          
320503     ELSE                                                                 
320504        MOVE SPACE           TO BYTF-OBJ-FLSKROT                          
320505     END-IF                                                               
320600     INSPECT REQU-IDARTNR-OBJ-SPAERR                                      
320700             REPLACING LEADING SPACE BY ZERO                              
320800     MOVE REQU-IDARTNR-OBJ-SPAERR TO BYTF-OBJ-IDARTNR-OBJ                 
320900     MOVE 'N'              TO BYTF-OBJ-KDBYTSTA-OBJ                       
321000     INSPECT REQU-KVRETUR-IN                                              
321100             REPLACING LEADING SPACE BY ZERO                              
321200     MOVE REQU-KVRETUR-IN TO BYTF-OBJ-KVRETUR-GODK                        
321300     MOVE ZERO             TO BYTF-OBJ-KVRETUR-URSP                       
321400     MOVE ZERO             TO BYTF-OBJ-IDTABNR                            
321500     MOVE ZERO             TO BYTF-OBJ-IDORDER                            
321600     MOVE SPACE            TO BYTF-OBJ-BERADREF                           
321700     MOVE SPACE            TO BYTF-OBJ-KDBYTSTA-AVL                       
321900     COMPUTE  BYTF-OBJ-IDBYTRAP-9KOMPL =                                  
322000                                W-9KOMPL - W-RAPP-IDBYTRAP                
322100                                                                          
322200                                                                          
322300     .                                                                    
322400     EJECT                                                                
322500                                                                          
322600 HFG-KONTR-HUVUD-RAD-STATUS SECTION.                                      
322700     MOVE 'HFG-KONTR-HUVUD-RAD-STATUS' TO WS-SEKTION                      
322800                                                                          
322900                                                                          
323000     MOVE W-IDBYTRAD TO WS-SPAR-IDBYTRAD                                  
323100     MOVE BYTF-OBJ-IDBYTRAD TO WS-LOPNUMMER                               
323200     MOVE ZERO              TO WS-LOPNUMMER2                              
323300     MOVE WS-LOPNUMMER      TO  W-IDBYTRAD                                
323400                                                                          
323500     PERFORM IMS-GHU-BYTF11                                               
323600     IF SEGMENT-FINNS                                                     
323700        IF BYTF-OBJ-KDBYTSTA-OBJ = '4'                                    
323800           MOVE 'C'       TO BYTF-OBJ-KDBYTSTA-OBJ                        
323900           PERFORM IMS-REPL-BYTF                                          
324000        END-IF                                                            
324100     END-IF                                                               
324200     MOVE WS-SPAR-IDBYTRAD TO W-IDBYTRAD                                  
324300     .                                                                    
324400     EJECT                                                                
324500                                                                          
324600 HH-IDBYTRAD-SAKNAS SECTION.                                              
324700     MOVE 'HH-IDBYTRAD-SAKNAS' TO WS-SEKTION                              
324800*******************************************************                   
324900**** IDBYTRAD SAKNAS PÅ BILDEN, BILD SIDAN ÄR TOM    **                   
325000**** UPPDATERING FRÅN TOM BILD                       **                   
325100*******************************************************                   
325200     MOVE REQU-IDARTNR-OBJ-SPAERR TO W-IDARTNR-OBJ                        
325300     MOVE ZERO             TO W-IDTABNR                                   
325400     MOVE +1               TO INDX                                        
325500                                                                          
325600     PERFORM IMS-GHU-BYTF01                                               
325700     IF SEGMENT-FINNS                                                     
325800        PERFORM IMS-GNP-BYTF11                                            
325900        IF SEGMENT-FINNS                                                  
326000           MOVE BYTF-OBJ-IDBYTRAD TO WS-IDBYTRAD                          
326100           IF WS-IDBYTRAD < 100                                           
326200              PERFORM HB-GAMMAL-VIPS-ANVANDARE                            
326300           ELSE                                                           
326400              PERFORM HCA-SKAPA-OKOPLAD-RAD                               
326500           END-IF                                                         
326600        END-IF                                                            
326700     END-IF                                                               
326800     .                                                                    
326900     EJECT                                                                
327000                                                                          
327100 S11-NYA-SEGMENT-WDK7 SECTION.                                            
327200                                                                          
327300     MOVE ALL '+'          TO WDK7-W005WDK7                               
327400     MOVE 'WDK711'         TO WDK7-IDSEGM                                 
327500     MOVE REQU-IDARTNR-OBJ-SPAERR                                         
327600                           TO WDK7-IDARTNR-KFB                            
327700     MOVE WC-SDC-NL-ET     TO WDK7-IDDC-KFB                               
327800                              WDK7-IDDC                                   
327900                                                                          
328000     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB WDK7-PCB         
328100     .                                                                    
328200     EJECT                                                                
328300                                                                          
328400 S21-SEND-OPEN SECTION.                                                   
328500     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
328600     MOVE 'OPEN'                  TO SEND-KDFUNC                          
328700     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
328800                                     SEND-OPEN-AREA                       
328900     IF SEND-KDRC > ZERO                                                  
329000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
329100       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
329200       DELIMITED BY SIZE INTO FELTEXT                                     
329300       DISPLAY FELTEXT                                                    
329400       CALL FELLOG                                                        
329500     END-IF                                                               
329600     .                                                                    
329700     EJECT                                                                
329800 S22-PUT-HEADER SECTION.                                                  
329900     MOVE 1                       TO HDR-REQU-IDMSGVER                    
330000     MOVE SPACE                   TO HDR-REQU-KDPGMACT                    
330100     MOVE IDPGM                   TO HDR-REQU-IDUSER                      
330200     MOVE 'CORE-LABEL'            TO HDR-IDOUTTYPE                        
330300     MOVE REQU-KDPRT              TO HDR-IDOUTREC                         
330400     MOVE WS-TIYYMMDDHHMM         TO HDR-IDLIST                           
330500     MOVE 'PUT'                   TO SEND-KDFUNC                          
330600     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
330700     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
330800                                     SEND-KVDLEN                          
330900                                     HDR-AREA                             
331000     IF SEND-KDRC > ZERO                                                  
331100       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
331200       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
331300       DELIMITED BY SIZE       INTO FELTEXT                               
331400       DISPLAY FELTEXT                                                    
331500       CALL FELLOG                                                        
331600     END-IF                                                               
331700     .                                                                    
331800     EJECT                                                                
331900 S25-PUT-LINE SECTION.                                                    
332000     MOVE 'PUT'                   TO SEND-KDFUNC                          
332100     MOVE LENGTH OF SEND-AREA-TO-CORE-LABEL TO SEND-KVDLEN                
332200     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
332300                                     SEND-KVDLEN                          
332400                                     SEND-AREA-TO-CORE-LABEL              
332500     IF SEND-KDRC > ZERO                                                  
332600       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
332700       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
332800       DELIMITED BY SIZE       INTO FELTEXT                               
332900       DISPLAY FELTEXT                                                    
333000       CALL FELLOG                                                        
333100     END-IF                                                               
333200     .                                                                    
333300     SKIP2                                                                
333400 S29-SEND-CLOSE SECTION.                                                  
333500     MOVE 'CLOSE'                TO SEND-KDFUNC                           
333600     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
333700     IF SEND-KDRC > 0                                                     
333800       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
333900       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
334000       DELIMITED BY SIZE       INTO FELTEXT                               
334100       DISPLAY FELTEXT                                                    
334200       CALL FELLOG                                                        
334300     END-IF                                                               
334400     .                                                                    
334500     EJECT                                                                
334600 MFS-RENSA-FAELT-UT SECTION.                                              
334700                                                                          
334800*    --- ALLA UTDATA-FÄLT                                                 
334900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
335000     MOVE ALL-SPACE       TO RESP-IDBYTRAD-START                          
335100                             RESP-IDBYTRAD-NEXT                           
335200                             RESP-FLAGGA                                  
335300                             RESP-KVRETUR-TOTU                            
335400                             RESP-KVRETUR-TOTG                            
335500                             RESP-IDKUNDNR                                
335600                             RESP-IDFAKT                                  
335700                             RESP-KDBYTSTA-RAPP                           
335710                             RESP-IDUSER-GODK                             
335800     MOVE +1 TO INDX                                                      
335900     PERFORM UNTIL INDX > MAX-KVRADER                                     
336000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
336100       ADD +1 TO INDX                                                     
336200     END-PERFORM                                                          
336300     .                                                                    
336400     SKIP2                                                                
336500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
336600                                                                          
336700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
336800     MOVE ALL-SPACE       TO RESP-KDCMD-LINE        (INDX)                
336900                             RESP-IDARTNR-OBJ-LINE  (INDX)                
337000                             RESP-KVPOINT-LINE      (INDX)                
337100                             RESP-IDTABNR-LINE      (INDX)                
337200                             RESP-KVRETUR-URSP-LINE (INDX)                
337300                             RESP-KVRETUR-GODK-LINE (INDX)                
337400                             RESP-BERADREF-LINE     (INDX)                
337500                             RESP-KDBYTSTA-LINE     (INDX)                
337600                             RESP-KDBYTREF-LINE     (INDX)                
337700                             RESP-BEART-LINE        (INDX)                
337800     IF REQU-IDMSGVER = '001'                                             
337900       MOVE ALL-SPACE-UTF8 TO RESP-BEART-LINE       (INDX)                
338000     END-IF                                                               
338100     .                                                                    
338200     SKIP2                                                                
338300 MFS-RENSA-FAELT-IN SECTION.                                              
338400                                                                          
338500*    --- ALLA INDATA-FÄLT                                                 
338600     IF W-IDTRANS = '3173'                                                
338700       IF REQU-KDBYTREF-IN = 'XXX'                                        
338800          MOVE REQU-KDBYTREF-IN      TO RESP-KDBYTREF-IN                  
338900       END-IF                                                             
339000       MOVE MFS-ADD-LAES-IN-FAELT    TO RESP-KDBYTREF-IN-ATTR             
339100       IF REQU-IDARTNR-OBJ-SPAERR NUMERIC                                 
339200         MOVE REQU-IDARTNR-OBJ-SPAERR TO RESP-IDARTNR-OBJ-SPAERR          
339300       END-IF                                                             
339400       IF REQU-IDBYTRAD-LINE (1) NUMERIC                                  
339500         MOVE REQU-IDBYTRAD-LINE (1) TO W-IDBYTRAD                        
339600       END-IF                                                             
339700       MOVE REQU-IDBYTRAD-3173       TO RESP-IDBYTRAD-3173                
339800       IF W-IDBYTRAD = ZERO                                               
339900         MOVE MFS-RENSA-FAELT        TO RESP-IDARTNR-OBJ-SPAERR           
340000       END-IF                                                             
340100       MOVE MFS-STAENG-FAELT    TO RESP-IDARTNR-OBJ-SPAERR-ATTR           
340200     ELSE                                                                 
340300       MOVE ALL-SPACE       TO RESP-IDARTNR-OBJ-SPAERR                    
340400                               RESP-KVRETUR-IN                            
340500                               RESP-KDBYTREF-IN                           
340510                               RESP-FLSKROT-IN                            
340600                               RESP-FLGODK-IN                             
340700     END-IF                                                               
340800                                                                          
340900     MOVE +1 TO INDX                                                      
341000     PERFORM UNTIL INDX > MAX-KVRADER                                     
341100       MOVE ALL-SPACE       TO RESP-KDCMD-LINE        (INDX)              
341200                               RESP-KVRETUR-GODK-LINE (INDX)              
341300                               RESP-KDBYTREF-LINE     (INDX)              
341400                               RESP-FLSKROT-LINE      (INDX)              
341500                               ADD +1 TO INDX                             
341600     END-PERFORM                                                          
341700     .                                                                    
341800     EJECT                                                                
341900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
342000                                                                          
342100*    --- ALLA UTDATA-FÄLT                                                 
342200*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
342300     MOVE ALL-PLUS          TO RESP-KVRETUR-TOTU                          
342400                               RESP-KVRETUR-TOTG                          
342500                               RESP-IDBYTRAD-START                        
342600                               RESP-IDBYTRAD-NEXT                         
342700                               RESP-IDKUNDNR                              
342800                               RESP-IDFAKT                                
342900                               RESP-KDBYTSTA-RAPP                         
342910                               RESP-IDUSER-GODK                           
343000                                                                          
343100     MOVE +1 TO INDX                                                      
343200     PERFORM UNTIL INDX > MAX-KVRADER                                     
343300       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
343400       ADD +1 TO INDX                                                     
343500     END-PERFORM                                                          
343600     .                                                                    
343700     SKIP2                                                                
343800 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
343900                                                                          
344000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
344100     MOVE ALL-PLUS          TO RESP-KDCMD-LINE        (INDX)              
344200                               RESP-IDARTNR-OBJ-LINE  (INDX)              
344300                               RESP-KVPOINT-LINE      (INDX)              
344400                               RESP-IDTABNR-LINE      (INDX)              
344500                               RESP-KVRETUR-URSP-LINE (INDX)              
344600                               RESP-KVRETUR-GODK-LINE (INDX)              
344700                               RESP-BERADREF-LINE     (INDX)              
344800                               RESP-KDBYTSTA-LINE     (INDX)              
344900                               RESP-KDBYTREF-LINE     (INDX)              
345000                               RESP-FLSKROT-LINE      (INDX)              
345100                               RESP-BEART-LINE        (INDX)              
345200                               RESP-FLAGGA-LOC-LINE   (INDX)              
345300     IF REQU-IDMSGVER = '001'                                             
345400        MOVE ALL-PLUS-UTF8  TO RESP-BEART-LINE        (INDX)              
345500     END-IF                                                               
345600     .                                                                    
345700     SKIP2                                                                
345800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
345900                                                                          
346000*    --- ALLA INDATA-FÄLT                                                 
346100     MOVE ALL-PLUS          TO RESP-IDARTNR-OBJ-SPAERR                    
346200                               RESP-KVRETUR-IN                            
346300                               RESP-KDBYTREF-IN                           
346400                               RESP-FLSKROT-IN                            
346410                               RESP-FLGODK-IN                             
346500                                                                          
346600     MOVE +1 TO INDX                                                      
346700     PERFORM UNTIL INDX > MAX-KVRADER                                     
346800       MOVE ALL-PLUS          TO RESP-KDCMD-LINE (INDX)                   
346900                                 RESP-KVRETUR-GODK-LINE (INDX)            
347000                                 RESP-KDBYTREF-LINE (INDX)                
347100                                 RESP-FLSKROT-LINE (INDX)                 
347200                                 ADD +1 TO INDX                           
347300     END-PERFORM                                                          
347400     .                                                                    
347500     EJECT                                                                
347600 MFS-FORM-ATTR SECTION.                                                   
347700                                                                          
347800*    --- ALLA INDATA-FÄLT                                                 
347900     MOVE +1 TO INDX                                                      
348000     PERFORM UNTIL INDX > MAX-KVRADER                                     
348100       MOVE MFS-FORMATETS-ATTR TO RESP-KDCMD-LINE-ATTR (INDX)             
348200                                RESP-KVRETUR-GODK-LINE-ATTR (INDX)        
348300                                RESP-KDBYTREF-LINE-ATTR (INDX)            
348400       ADD +1 TO INDX                                                     
348500     END-PERFORM                                                          
348600                                                                          
348700     MOVE MFS-FORMATETS-ATTR TO RESP-IDARTNR-OBJ-SPAERR-ATTR              
348800     MOVE MFS-FORMATETS-ATTR TO RESP-KVRETUR-IN-ATTR                      
348900     MOVE MFS-FORMATETS-ATTR TO RESP-KDBYTREF-IN-ATTR                     
349000     MOVE MFS-FORMATETS-ATTR TO RESP-FLSKROT-IN-ATTR                      
349010     MOVE MFS-FORMATETS-ATTR TO RESP-FLGODK-IN-ATTR                       
349100     .                                                                    
349200     SKIP2                                                                
349300 MFS-LAES-IN-IGEN SECTION.                                                
349400                                                                          
349500*    --- ALLA INDATA-FÄLT                                                 
349600     MOVE +1 TO INDX                                                      
349700     PERFORM UNTIL INDX > MAX-KVRADER                                     
349800       IF REQU-KDCMD-LINE (INDX) NOT = ALL '+'                            
349900         MOVE MFS-ADD-LAES-IN-FAELT                                       
350000         TO RESP-KDCMD-LINE-ATTR(INDX)                                    
350100       END-IF                                                             
350200       IF REQU-KVRETUR-GODK-LINE(INDX) NOT = ALL '+'                      
350300         MOVE MFS-ADD-LAES-IN-FAELT                                       
350400         TO RESP-KVRETUR-GODK-LINE-ATTR(INDX)                             
350500       END-IF                                                             
350600       IF REQU-KDBYTREF-LINE (INDX) NOT = ALL '+'                         
350700         MOVE MFS-ADD-LAES-IN-FAELT                                       
350800         TO RESP-KDBYTREF-LINE-ATTR(INDX)                                 
350900       END-IF                                                             
351000*      IF REQU-FLSKROT-LINE (INDX) NOT = ALL '+'                          
351100*       MOVE MFS-ADD-LAES-IN-FAELT                                        
351200*       TO RESP-FLSKROT-LINE-ATTR(INDX)                                   
351300*      END-IF                                                             
351400       ADD +1 TO INDX                                                     
351500     END-PERFORM                                                          
351600                                                                          
351700     IF REQU-IDARTNR-OBJ-SPAERR NOT = ALL '+'                             
351800       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDARTNR-OBJ-SPAERR-ATTR         
351900     END-IF                                                               
352000     IF REQU-KVRETUR-IN NOT = ALL '+'                                     
352100       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KVRETUR-IN-ATTR                 
352200     END-IF                                                               
352300     IF REQU-KDBYTREF-IN NOT = ALL '+'                                    
352400       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KDBYTREF-IN-ATTR                
352500     END-IF                                                               
352600     IF REQU-FLSKROT-IN NOT = ALL '+'                                     
352700       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLSKROT-IN-ATTR                 
352800     END-IF                                                               
352801     IF REQU-FLGODK-IN NOT = ALL '+'                                      
352802       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLGODK-IN-ATTR                  
352803     END-IF                                                               
352900     .                                                                    
353000     EJECT                                                                
353100* --- IMS SEKTIONER ---                                                   
353200     SKIP3                                                                
353300 IMS-GU-BYTF01 SECTION.                                                   
353400     MOVE 'IMS-GU-BYTF01' TO WS-IMS-SEKTION                               
353500                                                                          
353600     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
353700          DELIMITED BY SIZE INTO SSA1                                     
353800     MOVE '  GE' TO GODK-STATUSKODER                                      
353900     CALL CBLTDLI USING GU BYTF-PCB DLI-IO-AREA SSA1                      
354000     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
354100     PERFORM IMS-STATUSKONTROLL                                           
354200     .                                                                    
354300     SKIP3                                                                
354400 IMS-GHU-BYTF01 SECTION.                                                  
354500     MOVE 'IMS-GHU-BYTF01' TO WS-IMS-SEKTION                              
354600                                                                          
354700     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
354800          DELIMITED BY SIZE INTO SSA1                                     
354900     MOVE '  GE' TO GODK-STATUSKODER                                      
355000     CALL CBLTDLI USING GHU BYTF-PCB DLI-IO-AREA SSA1                     
355100     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
355200     PERFORM IMS-STATUSKONTROLL                                           
355300     .                                                                    
355400     SKIP3                                                                
355500 IMS-GNP-BYTF11 SECTION.                                                  
355600     MOVE 'IMS-GNP-BYTF11' TO WS-IMS-SEKTION                              
355700                                                                          
355800     STRING 'WLBYTF11(IDBYTRAD>=' W-IDBYTRAD-X ')'                        
355900          DELIMITED BY SIZE INTO SSA1                                     
356000     MOVE '  GE' TO GODK-STATUSKODER                                      
356100     CALL CBLTDLI USING GNP BYTF-PCB DLI-IO-AREA SSA1                     
356200     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
356300     PERFORM IMS-STATUSKONTROLL                                           
356400     .                                                                    
356500     SKIP3                                                                
356600 IMS-GU-BYTF01-SPEC SECTION.                                              
356700     MOVE 'IMS-GU-BYTF01-SPEC' TO WS-IMS-SEKTION                          
356800                                                                          
356900     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
357000          DELIMITED BY SIZE INTO SSA1                                     
357100     MOVE '  GE' TO GODK-STATUSKODER                                      
357200     CALL CBLTDLI USING GU BYTF2-PCB DLI-IO-AREA4 SSA1                    
357300     MOVE BYTF2-STATUS-CODE TO STATUS-WS                                  
357400     PERFORM IMS-STATUSKONTROLL                                           
357500     .                                                                    
357600     SKIP3                                                                
357700 IMS-GNP-SPEC-BYTF11 SECTION.                                             
357800     MOVE 'IMS-GNP-SPEC-BYTF11' TO WS-IMS-SEKTION                         
357900                                                                          
358000     STRING 'WLBYTF11(IDBYTRAD >' W-IDBYTRAD-MIN-X                        
358100                    '&IDBYTRAD <' W-IDBYTRAD-MAX-X ')'                    
358200          DELIMITED BY SIZE INTO SSA1                                     
358300     MOVE '  GE' TO GODK-STATUSKODER                                      
358400     CALL CBLTDLI USING GNP BYTF2-PCB DLI-IO-AREA4 SSA1                   
358500     MOVE BYTF2-STATUS-CODE TO STATUS-WS                                  
358600     PERFORM IMS-STATUSKONTROLL                                           
358700     .                                                                    
358800     SKIP3                                                                
358900 IMS-GHNP-BYTF11-IMS SECTION.                                             
359000     MOVE 'IMS-GHNP-BYTF11-11' TO WS-IMS-SEKTION                          
359100                                                                          
359200     STRING 'WLBYTF11(IDBYTRAD>=' W-IDBYTRAD-MIN-X                        
359300                    '&IDBYTRAD<=' W-IDBYTRAD-MAX-X ')'                    
359400          DELIMITED BY SIZE INTO SSA1                                     
359500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
359600     CALL CBLTDLI USING GHNP BYTF-PCB DLI-IO-AREA SSA1                    
359700     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
359800     PERFORM IMS-STATUSKONTROLL                                           
359900     .                                                                    
360000     SKIP3                                                                
360100 IMS-GHU-BYTF11 SECTION.                                                  
360200     MOVE 'IMS-GHU-BYTF11' TO WS-IMS-SEKTION                              
360300                                                                          
360400     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
360500          DELIMITED BY SIZE INTO SSA1                                     
360600     STRING 'WLBYTF11(IDBYTRAD =' W-IDBYTRAD-X ')'                        
360700          DELIMITED BY SIZE INTO SSA2                                     
360800     MOVE '  GE' TO GODK-STATUSKODER                                      
360900     CALL CBLTDLI USING GHU BYTF-PCB DLI-IO-AREA SSA1 SSA2                
361000     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
361100     PERFORM IMS-STATUSKONTROLL                                           
361200     .                                                                    
361300     EJECT                                                                
361400 IMS-GU-BYTF11 SECTION.                                                   
361500     MOVE 'IMS-GU-BYTF11' TO WS-IMS-SEKTION                               
361600                                                                          
361700     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
361800          DELIMITED BY SIZE INTO SSA1                                     
361900     STRING 'WLBYTF11(IDBYTRAD =' W-IDBYTRAD-X ')'                        
362000          DELIMITED BY SIZE INTO SSA2                                     
362100     MOVE '  GE' TO GODK-STATUSKODER                                      
362200     CALL CBLTDLI USING GU BYTF-PCB DLI-IO-AREA SSA1 SSA2                 
362300     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
362400     PERFORM IMS-STATUSKONTROLL                                           
362500     .                                                                    
362600     EJECT                                                                
362700 IMS-ISRT-BYTF11 SECTION.                                                 
362800     MOVE 'IMS-ISRT-BYTF11' TO WS-IMS-SEKTION                             
362900                                                                          
363000     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
363100          DELIMITED BY SIZE INTO SSA1                                     
363200     MOVE 'WLBYTF11 ' TO SSA2                                             
363300     MOVE '  II' TO GODK-STATUSKODER                                      
363400     CALL CBLTDLI USING ISRT BYTF-PCB DLI-IO-AREA SSA1 SSA2               
363500     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
363600     PERFORM IMS-STATUSKONTROLL                                           
363700     .                                                                    
363800     SKIP3                                                                
363900 IMS-REPL-BYTF SECTION.                                                   
364000     MOVE 'IMS-REPL-BYTF' TO WS-IMS-SEKTION                               
364100                                                                          
364200     MOVE '  ' TO GODK-STATUSKODER                                        
364300     CALL CBLTDLI USING REPL BYTF-PCB DLI-IO-AREA                         
364400     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
364500     PERFORM IMS-STATUSKONTROLL                                           
364600     .                                                                    
364700     SKIP3                                                                
364800 IMS-DLET-BYTF SECTION.                                                   
364900     MOVE 'IMS-DLET-BYTF' TO WS-IMS-SEKTION                               
365000                                                                          
365100     MOVE '  ' TO GODK-STATUSKODER                                        
365200     CALL CBLTDLI USING DLET BYTF-PCB DLI-IO-AREA                         
365300     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
365400     PERFORM IMS-STATUSKONTROLL                                           
365500     .                                                                    
365600     EJECT                                                                
365700 IMS-GET-BENA-GU SECTION.                                                 
365800     MOVE 'IMS-GET-BENA-GU' TO WS-IMS-SEKTION                             
365900                                                                          
366000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
366100          DELIMITED BY SIZE INTO SSA1                                     
366200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
366300          DELIMITED BY SIZE INTO SSA2                                     
366400     MOVE '  GE' TO GODK-STATUSKODER                                      
366500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
366600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
366700     PERFORM IMS-STATUSKONTROLL                                           
366800     .                                                                    
366900     SKIP2                                                                
367000 IMS-GET-XXCP-ROT SECTION.                                                
367100     MOVE 'IMS-GET-XXCP-ROT' TO WS-IMS-SEKTION                            
367200                                                                          
367300     STRING 'WLXXCP01(WDGXKEY  =' W-WDGX30-X ')'                          
367400          DELIMITED BY SIZE INTO SSA1                                     
367500     MOVE '  GE' TO GODK-STATUSKODER                                      
367600     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA2 SSA1                     
367700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
367800     PERFORM IMS-STATUSKONTROLL                                           
367900     .                                                                    
368000     SKIP2                                                                
368100 IMS-GET-XXCP-GNP SECTION.                                                
368200     MOVE 'IMS-GET-XXCP-GNP' TO WS-IMS-SEKTION                            
368300                                                                          
368400     STRING 'WLXXCP01(WDGXKEY  =' W-WDGX30-X ')'                          
368500          DELIMITED BY SIZE INTO SSA1                                     
368600     STRING 'WLXXCP11(WDGXKEY >=' W-WDGXKEY-LOW-X                         
368700                    '&IDTABNR  =' W-IDTABNR-X ')'                         
368800          DELIMITED BY SIZE INTO SSA2                                     
368900     MOVE '  GE' TO GODK-STATUSKODER                                      
369000     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA2 SSA1 SSA2               
369100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
369200     PERFORM IMS-STATUSKONTROLL                                           
369300     .                                                                    
369400     SKIP2                                                                
369500 IMS-GET-XXCP-ARTNR SECTION.                                              
369600     MOVE 'IMS-GET-XXCP-ARTNR' TO WS-IMS-SEKTION                          
369700                                                                          
369800     STRING 'WLXXCP01(WDGXKEY  =' W-WDGX30-X ')'                          
369900          DELIMITED BY SIZE INTO SSA1                                     
370000     STRING 'WLXXCP11(WDGXKEY >=' W-WDGXKEY-LOW-X                         
370100                    '&WDGXKEY <=' W-WDGXKEY-HIGH-X                        
370200                    '&IDARTNR  =' W-IDARTNR-KVITT-X ')'                   
370300          DELIMITED BY SIZE INTO SSA2                                     
370400     MOVE '  GE' TO GODK-STATUSKODER                                      
370500     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA2 SSA1 SSA2               
370600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
370700     PERFORM IMS-STATUSKONTROLL                                           
370800     .                                                                    
370900     EJECT                                                                
371000 IMS-GU-WDK6 SECTION.                                                     
371100     MOVE 'IMS-GU-WDK6' TO WS-IMS-SEKTION                                 
371200                                                                          
371300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
371400          DELIMITED BY SIZE INTO SSA1                                     
371500     MOVE '  GE' TO GODK-STATUSKODER                                      
371600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
371700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
371800     PERFORM IMS-STATUSKONTROLL                                           
371900     .                                                                    
372000     EJECT                                                                
372100 IMS-GU-WDK611 SECTION.                                                   
372200     MOVE 'IMS-GU-WDK611' TO WS-IMS-SEKTION                               
372300                                                                          
372400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
372500          DELIMITED BY SIZE INTO SSA1                                     
372600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-X ')'                     
372700          DELIMITED BY SIZE INTO SSA2                                     
372800     MOVE '  GE' TO GODK-STATUSKODER                                      
372900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
373000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
373100     PERFORM IMS-STATUSKONTROLL                                           
373200     .                                                                    
373300     EJECT                                                                
373400 IMS-GHU-WDK711 SECTION.                                                  
373500     MOVE 'IMS-GHU-WDK711'   TO WS-IMS-SEKTION                            
373600                                                                          
373700     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-K7-X ')'                      
373800          DELIMITED BY SIZE INTO SSA1                                     
373900     STRING 'WDK711  (IDDC    = ' W-IDDC-K7-X ')'                         
374000          DELIMITED BY SIZE INTO SSA2                                     
374100     MOVE '  GE' TO GODK-STATUSKODER                                      
374200     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
374300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
374400     PERFORM IMS-STATUSKONTROLL                                           
374500     .                                                                    
374600     EJECT                                                                
374610 IMS-GU-WDB601    SECTION.                                                
374620     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
374630          DELIMITED BY SIZE INTO SSA1                                     
374640     MOVE '  GE' TO GODK-STATUSKODER                                      
374650     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
374660     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
374670     PERFORM IMS-STATUSKONTROLL                                           
374680     .                                                                    
374690     EJECT                                                                
374700 IMS-STATUSKONTROLL SECTION.                                              
374800                                                                          
374900     SET STATUS-IX TO 1                                                   
375000     SEARCH GODK-STATUS                                                   
375100       AT END                                                             
375200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
375300         DELIMITED BY SIZE INTO FELTEXT                                   
375400         CALL FELLOG                                                      
375500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
375600         CONTINUE                                                         
375700     END-SEARCH                                                           
375800     .                                                                    
375900 DB2-SELECT-BYART SECTION.                                                
376000     MOVE 'DB2-SELECT-BYART' TO  WS-DB2-SEKTION                           
376100                                                                          
376200     MOVE 000100904         TO GODK-SQLCODESKODER                         
376300     EXEC SQL SELECT                                                      
376400                  ADLAGOMR,                                               
376500                  ADGANG,                                                 
376600                  ADPLATS                                                 
376700              INTO                                                        
376800                  :BYART-ADLAGOMR,                                        
376900                  :BYART-ADGANG,                                          
377000                  :BYART-ADPLATS                                          
377100            FROM BYART                                                    
377200            WHERE IDARTNR_BYT = :BYART-IDARTNR-BYT                        
377300     END-EXEC                                                             
377400     MOVE SQLCODE           TO SQLCODE-WS                                 
377500     PERFORM DB2-STATUSKONTROLL                                           
377600     .                                                                    
377700 DB2-STATUSKONTROLL SECTION.                                              
377800     SKIP2                                                                
377900     SET SQLCODE-IX          TO 1                                         
378000     SEARCH GODK-SQLCODE                                                  
378100       AT END                                                             
378200         CALL FELLOG                                                      
378300        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
378400           CONTINUE                                                       
378500     END-SEARCH                                                           
378600     .                                                                    
