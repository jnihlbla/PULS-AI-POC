000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4070400.                                                
000300 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000400 DATE-WRITTEN.   95/02/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        REGISTRERING AV LEVERANSANMÄRKNINGAR                             
001000*        PROGRAMMET LÄSER FÖLJANDE BASER VIA SUBPROGRAM W418KTL1          
001100*        OCH W418KTL3                                                     
001200*                                                                         
001300*        PROGRAMMET LÄSER              WDL5                               
001400*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
001500*        PROGRAMMET LÄSER      WDG2 HTYP 9305,9306,9308                   
001600*        PROGRAMMET LÄSER              WDB2                               
001700*        PROGRAMMET LÄSER              WDB6                               
001800*        PROGRAMMET LÄSER              WDK7                               
001900*        PROGRAMMET LÄSER              WDK6                               
002000*                                                                         
002100*        PROGRAMMET LÄSER      WDA8 MATRIX-BASEN VIA W418KTL3             
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T704                                              
002500*        MID:         W4I70401                                            
002600*        MID:         W4I79101 TILL 4791 VIA DISPATCEN                    
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O70401                                            
003000*                                                                         
003100* CHANGE LOG: E'TRACKER 2913019    20051207                               
003200*             E'TRACKER 1658417    20060404                               
003300*             E'TRACKER 850114     20070404                               
003400*             E'TRACKER 4996031    20070507                               
003500*             E'TRACKER 5935191    20071119                               
003600*             E'TRACKER 5838822    20071121                               
003700*             E'TRACKER 4823800    20071127                               
003800*                                                                         
003900*             E-TRACKER 8635407  20091021 RETURN CODES MATRIX             
004000*             E-TRACKER 8735608  20091215 MINIMUM VALUE CONTROL           
004100*             E-TRACKER 9822116  20101014 DISCR/RETURNS HAZ.MAT.          
004200*             E-TRACKER 10143271 20111018 CHINA WAREHOUSE PROJ.1          
004300*             STORY 2375089 ADD IDSYSTEM VOUI, ECOM                       
004400*                                                                         
004500                                                                          
004600                                                                          
004700 ENVIRONMENT DIVISION.                                                    
004800 DATA DIVISION.                                                           
004900    EJECT                                                                 
005000 WORKING-STORAGE SECTION.                                                 
005100*    -- CHECKED BY WY2000                                                 
005200     SKIP3                                                                
005300 77  IDPGM                       PIC X(08)   VALUE 'W4070400'.            
005400                                                                          
005500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL FELLOG                    
005600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005700                                                                          
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  YES                         PIC X       VALUE 'Y'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 77  FEL                         PIC X       VALUE 'N'.                   
006200 77  SPAR-FEL                    PIC X       VALUE 'N'.                   
006300*77  MATRIX-FEL                  PIC X       VALUE 'N'.                   
006400 77  MATRIX-Q                    PIC X       VALUE 'N'.                   
006500 77  WS-IDDC-INPUT               PIC X(2)    VALUE SPACE.                 
006600 77  WS-NDC-AU                   PIC X(2)    VALUE '62'.                  
006700 77  WS-CDC-SE                   PIC X(2)    VALUE '11'.                  
006800 77  WS-KDVALISO                 PIC X(3)    VALUE SPACE.                 
006900 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
007000 77  WS-IDDISTR-N                PIC 9(4)    VALUE ZERO.                  
007100 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
007200 77  WS-IDKUNDNR-N               PIC 9(6)    VALUE ZERO.                  
007300 77  WS-IDRAPPNR                 PIC X(7)    VALUE SPACE.                 
007400 77  WS-IDRAPPNR-N               PIC 9(7)    VALUE ZERO.                  
007500 77  WS-IDORDNR-N                PIC 9(5)    VALUE ZERO.                  
007600 77  WS-IDFAKT-N                 PIC 9(7)    VALUE ZERO.                  
007700 77  WS-IDKOLLI-N                PIC 9(5)    VALUE ZERO.                  
007800 77  WS-IDARTNR-N                PIC 9(9)    VALUE ZERO.                  
007900 77  WS-KVLEVANM-N               PIC 9(6)    VALUE ZERO.                  
008000 77  WS-KDANMORS-N               PIC 9(2)    VALUE ZERO.                  
008100 77  WS-PRARTBTO-JFR             PIC S9(7)V9(2) VALUE +0 COMP-3.          
008200 77  WS-KVRADER                  PIC S9(5)   VALUE +0    COMP-3.          
008300 77  W-LINENO                    PIC 9(4)    VALUE ZERO.                  
008400 77  W-IX                        PIC 9(8)   VALUE ZERO COMP-4.            
008500 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
008600 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
008700 77  IX2                         PIC S9(9)  VALUE +0    COMP SYNC.        
008800 77  L-IX                        PIC S9(9)  VALUE +0    COMP SYNC.        
008900 77  F-IX                        PIC S9(9)  VALUE +0    COMP SYNC.        
009000 77  MAX-F-IX                    PIC S9(9)  VALUE +4    COMP SYNC.        
009100 77  MAX-IX                      PIC S9(9)  VALUE +13   COMP SYNC.        
009200 77  LINK-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
009300 77  MAX-LINK-IX                 PIC S9(9)  VALUE +50   COMP SYNC.        
009400 77  DAGENS-DATUM                PIC  9(6)  VALUE ZERO.                   
009500 77  DAGENS-TID                  PIC  9(8)  VALUE ZERO.                   
009600 77  KDRC-DISPLAY                PIC Z(5).                                
009700 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
009800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +100  COMP SYNC.        
009900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010000 77  WC-KDANMORS                 PIC X(8)   VALUE 'KDANMORS'.             
010100 77  WC-IDFKNGRP                 PIC X(8)   VALUE 'IDFKNGRP'.             
010200 77  WC-IDARTNR                  PIC X(8)   VALUE 'IDARTNR '.             
010300 77  WC-IDDC-EXCP                PIC X(8)   VALUE 'IDDC    '.             
010400 77  DUMMY-IDARTNR               PIC S9(9)   VALUE +100  COMP-3.          
010500                                                                          
010600*                                                                         
010700 77  HELP-IDANALYS               PIC  X(12)  VALUE SPACE.                 
010800 77  HELP-IDKONTO                PIC  9(10)  VALUE ZERO.                  
010900 77  HELP-IDARTNR                PIC  9(9)  VALUE ZERO.                   
011000 77  HELP-IDDISTR                PIC  9(4)  VALUE ZERO.                   
011100 77  HELP-IDKUNDNR               PIC  9(6)  VALUE ZERO.                   
011200 77  HELP-KDFRAKT                PIC  9(2)  VALUE ZERO.                   
011300 77  HELP-IDFTG                  PIC  9(2)  VALUE ZERO.                   
011400 77  HELP-KVLEVANM               PIC  9(6)  VALUE ZERO.                   
011500 77  HELP-TIFAKT                 PIC  9(6)  VALUE ZERO.                   
011600*    --- DET RÄTTA VÄRDET PÅ NEDANSTÅENDE FÄLT SÄTTS I A-INIT             
011700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
011800                                                                          
011900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
012000                                                                          
012100 77  API-SW                      PIC X       VALUE 'N'.                   
012200                                                                          
012300 77    OK-SW                     PIC X       VALUE 'Y'.                   
012400   88  EVERYTHING-OK                         VALUE 'Y'.                   
012500   88  SOMETHING-WRONG                       VALUE 'N'.                   
012600                                                                          
012700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
012800     88  NYCKLAR-OK                          VALUE 'J'.                   
012900     88  NYCKLAR-FEL                         VALUE 'N'.                   
013000                                                                          
013100 77  FORTSAETT-SW                PIC X       VALUE 'J'.                   
013200     88  NYCKLAR-GAMLA                       VALUE 'J'.                   
013300     88  NYCKLAR-NYA                         VALUE 'N'.                   
013400                                                                          
013500 77  RADER-SW                    PIC X       VALUE 'N'.                   
013600     88  RADER-FINNS                         VALUE 'J'.                   
013700                                                                          
013800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
013900     88  ALLT-OK                             VALUE 'J'.                   
014000                                                                          
014100 77  TILLAEGGSKOST-SW            PIC X       VALUE 'N'.                   
014200     88  TILLAEGGSKOST-FINNS                 VALUE 'J'.                   
014300                                                                          
014400 77  GODK-KDFAKTYP-SW            PIC X.                                   
014500     88  GODK-KDFAKTYP                       VALUE 'R' 'K' 'N' 'G'        
014600                                                   'F'.                   
014700                                                                          
014800 77  LDC-SW                      PIC X       VALUE 'N'.                   
014900     88  EJ-KOD-72-LDC                       VALUE 'J'.                   
015000                                                                          
015100 77  KOD-72-LDC-SW               PIC X       VALUE 'N'.                   
015200     88  KOD-72-LDC                          VALUE 'J'.                   
015300                                                                          
015400 77  KOD-74-FINNS-SW             PIC X       VALUE 'N'.                   
015500     88  KOD-74-FINNS                        VALUE 'J'.                   
015600                                                                          
015700 77  KOD-74-SAKNAS-SW            PIC X       VALUE 'N'.                   
015800     88  KOD-74-SAKNAS                       VALUE 'J'.                   
015900                                                                          
016000 77  GODK-KOD-SW                 PIC X.                                   
016100     88  GODK-KOD                            VALUE 'J'.                   
016200     88  EJ-GODK-KOD                         VALUE 'N'.                   
016300     EJECT                                                                
016400                                                                          
016500 77  GODK-ARTIKEL-SW             PIC X.                                   
016600     88  GODK-ARTIKEL                        VALUE 'J'.                   
016700     88  EJ-GODK-ARTIKEL                     VALUE 'N'.                   
016800     EJECT                                                                
016900                                                                          
017000 77  GODK-IDFKNGRP-SW            PIC X.                                   
017100     88  GODK-IDFKNGRP                       VALUE 'J'.                   
017200     88  EJ-GODK-IDFKNGRP                    VALUE 'N'.                   
017300     EJECT                                                                
017400                                                                          
017500 77  GODK-DC-ARTIKEL-SW          PIC X.                                   
017600     88  GODK-DC-ARTIKEL                     VALUE 'J'.                   
017700     88  EJ-GODK-DC-ARTIKEL                  VALUE 'N'.                   
017800     EJECT                                                                
017900                                                                          
018000 77  GODK-DC-LEV-SW              PIC X.                                   
018100     88  GODK-DC-LEV                         VALUE 'J'.                   
018200     88  EJ-GODK-DC-LEV                      VALUE 'N'.                   
018300     EJECT                                                                
018400                                                                          
018500                                                                          
018600*      --- HÄMTA MARKNADSBOLAGSVALUTA                                     
018700       EJECT                                                              
018800 77  BETALARE-SW                 PIC X       VALUE 'J'.                   
018900     88  BETALARE-FINNS                      VALUE 'J'.                   
019000     88  BETALARE-SAKNAS                     VALUE 'N'.                   
019100                                                                          
019200 77  MATRIX-SW                   PIC X       VALUE 'N'.                   
019300     88  MATRIX-FEL                          VALUE 'J'.                   
019400                                                                          
019500 77  OBEHORIG-SW                 PIC X       VALUE 'N'.                   
019600     88  OBEHORIG                            VALUE 'J'.                   
019700                                                                          
019800 77  TOM-RAD-1-SW                PIC X       VALUE 'N'.                   
019900     88  TOM-RAD-1                           VALUE 'J'.                   
020000                                                                          
020100 77  STATUS-3-SW                 PIC X       VALUE 'N'.                   
020200     88  STATUS-3                            VALUE 'J'.                   
020300                                                                          
020400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
020500     88  EGEN-MID                            VALUE '4704'.                
020600     88  HELP-MID                            VALUE '0551'.                
020700                                                                          
020800 01  WS-PRFOERS.                                                          
020900    03 WS-PRFOERS-ALFA           PIC X(10).                               
021000 01  FILLER REDEFINES WS-PRFOERS.                                         
021100   03 WS-PRFOERS-NUM             PIC  9(7).9(2).                          
021200                                                                          
021300 01  WS-PRFRAKT.                                                          
021400    03 WS-PRFRAKT-ALFA           PIC X(10).                               
021500 01  FILLER REDEFINES WS-PRFRAKT.                                         
021600   03 WS-PRFRAKT-NUM             PIC  9(7).9(2).                          
021700                                                                          
021800 01  WS-PRLEGKST.                                                         
021900    03 WS-PRLEGKST-ALFA          PIC X(10).                               
022000 01  FILLER REDEFINES WS-PRLEGKST.                                        
022100   03 WS-PRLEGKST-NUM            PIC  9(7).9(2).                          
022200                                                                          
022300 01  WS-PRARTBTO.                                                         
022400    03 WS-PRARTBTO-ALFA          PIC X(10).                               
022500 01  FILLER REDEFINES WS-PRARTBTO.                                        
022600   03 WS-PRARTBTO-NUM            PIC  9(7).9(2).                          
022700                                                                          
022800 01  WS-PRARTBTO-LOC.                                                     
022900   03 WS-PRARTBTO-LOC-ALFA       PIC X(10).                               
023000 01  FILLER REDEFINES WS-PRARTBTO-LOC.                                    
023100   03 WS-PRARTBTO-LOC-NUM        PIC  9(7).9(2).                          
023200                                                                          
023300 01  WS-PRARTSTD.                                                         
023400   03 WS-PRARTSTD-ALFA           PIC X(10).                               
023500 01  FILLER REDEFINES WS-PRARTSTD.                                        
023600   03 WS-PRARTSTD-NUM            PIC  9(7).9(2).                          
023700                                                                          
023800 01  WS-PRARTSJK.                                                         
023900   03 WS-PRARTSJK-ALFA           PIC X(10).                               
024000 01  FILLER REDEFINES WS-PRARTSJK.                                        
024100   03 WS-PRARTSJK-NUM            PIC  9(7).9(2).                          
024200                                                                          
024300 01  WS-RELANDCO.                                                         
024400    03 WS-RELANDCO-ALFA          PIC X(6).                                
024500 01  FILLER REDEFINES WS-RELANDCO.                                        
024600   03 WS-RELANDCO-NUM            PIC  9(3).9(2).                          
024700     EJECT                                                                
024800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
024900 01  GENERELLA-SUBPROGRAM.                                                
025000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
025100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
025200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
025300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
025400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
025500     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
025600     03  W418KTL1                PIC X(8)    VALUE 'W418KTL1'.            
025700     03  W418KTL2                PIC X(8)    VALUE 'W418KTL2'.            
025800     03  W418KTL3                PIC X(8)    VALUE 'W418KTL3'.            
025900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
026000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
026100     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
026200     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
026300     EJECT                                                                
026400*    --- PARAMETRAR TILL SUBPROGRAM WZ01SUB                               
026500*01  -COPY WZ01SUB                                                        
026600 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH   '.         
026700                                                                          
026800*01  -COPY WZ01AUTH                                                       
026900*                                                                         
027000 01  SUB-DATA                    PIC X(4000000).                          
027100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
027200*01 -COPY WMEDAREA                                                        
027300*                                                                         
027400*    --- PARAMETRAR TILL SUBPROGRAM WSECURIT                              
027500*   -COPY WSECAREA                                                        
027600     EJECT                                                                
027700*    ---  LÄNKAREA TILL W418OKOD                                          
027800 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
027900                                                                          
028000*01  -COPY W418OKOD           -PRE OKOD-.                                 
028100     EJECT                                                                
028200                                                                          
028300 01  FILLER              PIC X(16) VALUE 'TEST-IDDISTRIKT '.              
028400                                                                          
028500 01  TEST-IDDISTR           PIC  9(5) COMP-3 VALUE ZERO.                  
028600*01  FILLER  -COPY WWDIST34    -RED TEST-IDDISTR.                         
028700     EJECT                                                                
028800*01  FILLER  -COPY WWDIST79    -RED TEST-IDDISTR.                         
028900     SKIP2                                                                
029000                                                                          
029100*01  -COPY WWIDFTG                                                        
029200     EJECT                                                                
029300                                                                          
029400*01    -COPY WWDC99                                                       
029500       EJECT                                                              
029600 01  MESSAGE-CODES.                                                       
029700     03  ERR-WRONG-INPUT         PIC X(3)    VALUE '001'.                 
029800     03  ERR-CUST-MISSING        PIC X(3)    VALUE '063'.                 
029900     03  ERR-NO-UPDATE           PIC X(3)    VALUE '034'.                 
030000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
030100     03  ERR-ALREADY-EXISTS      PIC X(3)    VALUE '245'.                 
030200     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
030300     03  ERR-ON-LINE-1           PIC X(3)    VALUE '184'.                 
030400     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
030500     03  ERR-PRICE-MISSING       PIC X(3)    VALUE '301'.                 
030600     03  ERR-INVOICE-MISSING     PIC X(3)    VALUE '320'.                 
030700     03  ERR-UPPDAT-EJ-TILLATEN  PIC X(3)    VALUE '777'.                 
030800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
030900     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
031000     03  BAD-REQUEST             PIC X(3)    VALUE '400'.                 
031100     03  DISCP-CREATED           PIC X(3)    VALUE '201'.                 
031200     03  NOT-FOUND               PIC X(3)    VALUE '404'.                 
031300     03  DUPLICATE-DR            PIC X(3)    VALUE '403'.                 
031400     EJECT                                                                
031500 01  FILLER                   PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.         
031600 01  NYCKLAR.                                                             
031700                                                                          
031800     03 W-IDGMT-MIN-X.                                                    
031900        05 W-IDDISTR-WDB2-MIN    PIC S9(5)    COMP-3.                     
032000        05 W-IDKUNDNR-WDB2-MIN   PIC S9(7)    COMP-3.                     
032100                                                                          
032200     03 W-IDGMT-MAX-X.                                                    
032300        05 W-IDDISTR-WDB2-MAX    PIC S9(5)    COMP-3.                     
032400        05 W-IDKUNDNR-WDB2-MAX   PIC S9(7)    COMP-3.                     
032500                                                                          
032600     03 W-WDB101KY-X.                                                     
032700        05 W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
032800        05 W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
032900                                                                          
033000     03 W-IDLEVANM-X.                                                     
033100        05 W-IDDISTR-WDA2        PIC S9(5)    COMP-3.                     
033200        05 W-IDKUNDNR-WDA2       PIC S9(7)    COMP-3.                     
033300        05 W-IDRAPPNR-WDA2       PIC  X(7).                               
033400                                                                          
033500     03 W-WDA2A1KY-MIN-X.                                                 
033600        05  W-KDLEVANM-MIN       PIC 9       VALUE ZERO.                  
033700        05  FILLER               PIC X(16)   VALUE LOW-VALUE.             
033800                                                                          
033900     03 W-WDA2A1KY-MAX-X.                                                 
034000        05  W-KDLEVANM-MAX       PIC 9       VALUE 7.                     
034100        05  FILLER               PIC X(16)   VALUE HIGH-VALUE.            
034200                                                                          
034300     03 W-IDDISTR-X.                                                      
034400        05 W-IDDISTR             PIC S9(5)    COMP-3 VALUE ZERO.          
034500                                                                          
034600     03 W-IDRAPPNR-X.                                                     
034700        05  W-IDRAPPNR           PIC 9(7)     VALUE ZERO.                 
034800                                                                          
034900     03 W-IDGMT-X.                                                        
035000        05  W-WDB2-IDDISTR       PIC S9(5)   VALUE ZERO COMP-3.           
035100        05  W-WDB2-IDKUNDNR      PIC S9(7)   VALUE ZERO COMP-3.           
035200                                                                          
035300     03  W-IDDC-B6-X.                                                     
035400         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
035500                                                                          
035600     03  W-WDB611KY-X.                                                    
035700       05  W-URV-TEELMT            PIC X(16)  VALUE SPACE.                
035800       05  W-URV-FILLER            PIC X(20)  VALUE SPACE.                
035900       05  W-URV-IDARTNR-EXCP-FILLER REDEFINES W-URV-FILLER.              
036000         07  W-URV-IDARTNR-EXCP    PIC 9(9).                              
036100         07  FILLER                PIC X(11).                             
036200       05  W-URV-IDFKNGRP-EXCP-FILLER REDEFINES W-URV-FILLER.             
036300         07  W-URV-IDFKNGRP-EXCP   PIC 9(4).                              
036400         07  FILLER                PIC X(16).                             
036500       05  W-URV-KDANMORS-RET-FILLER REDEFINES W-URV-FILLER.              
036600         07  W-URV-KDANMORS-RET    PIC X(2).                              
036700         07  FILLER                PIC X(18).                             
036800       05  W-URV-IDDC-EXCP-FILLER REDEFINES W-URV-FILLER.                 
036900         07  W-URV-IDDC-EXCP       PIC X(2).                              
037000         07  FILLER                PIC X(18).                             
037100                                                                          
037200     03  W-IDDC-K7-X.                                                     
037300       05  W-IDDC-K7             PIC X(2)     VALUE SPACE.                
037400                                                                          
037500     03 W-IDARTNR-X.                                                      
037600       05  W-IDARTNR             PIC S9(9)    COMP-3 VALUE ZERO.          
037700                                                                          
037800     03 W-IDARTNR-K6-X.                                                   
037900       05  W-IDARTNR-K6          PIC S9(9)    COMP-3 VALUE ZERO.          
038000                                                                          
038100     EJECT                                                                
038200 01  FILLER                      PIC X(16)   VALUE 'DECAREA '.            
038300                                                                          
038400 01  DECAREA.                                                             
038500* 03  WDECAREA   -COPY WDECAREA                                           
038600     EJECT                                                                
038700 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
038800*01  -COPY WMSGKOM                                                        
038900     EJECT                                                                
039000 01  FILLER                      PIC X(80)   VALUE ALL 'A'.               
039100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
039200*                                                                         
039300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
039400                                                                          
039500*01  MID -COPY W4I70401                                                   
039600     EJECT                                                                
039700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
039800                                                                          
039900*01  -COPY WMSGAREA                                                       
040000     EJECT                                                                
040100 01  MOD.                                                                 
040200   03    MOD-KVLL                PIC S9(4)         COMP SYNC.             
040300*                                    * LENGTH OF MESSAGE                  
040400   03    MOD-KDZ1                PIC X.                                   
040500*                                    * FLAG FOR MOD                       
040600   03    MOD-KDZ2                PIC X.                                   
040700*                                    * FLAG FOR MOD                       
040800*  03  -COPY W4O70401                                                     
040900     EJECT                                                                
041000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
041100                                                                          
041200 01  REQU-AREA.                                                           
041300*    03  -COPY WZ01REQ2                                                   
041400*    03  -COPY W40704I1                                                   
041500                                                                          
041600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
041700                                                                          
041800 01  RESP-AREA.                                                           
041900*    03  -COPY WZ01RESP                                                   
042000*    03  -COPY W40704O1                                                   
042100                                                                          
042200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
042300                                                                          
042400*01  -COPY WMFSAREA                                                       
042500     EJECT                                                                
042600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
042700*                                                                         
042800     EJECT                                                                
042900 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
043000                                                                          
043100 01  SPAR-AREA.                                                           
043200*  05 -COPY W4I79102    -PRE SPAR-.                                       
043300                                                                          
043400     EJECT                                                                
043500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
043600                                                                          
043700*    --- STATUS-KOD FRÅN IMS                                              
043800 01  STATUS-WS                   PIC XX.                                  
043900     88  SEGMENT-FINNS                       VALUE '  '.                  
044000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
044100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
044200     88  BASEN-SLUT                          VALUE 'GB'.                  
044300                                                                          
044400 01  GODK-STATUSKODER.                                                    
044500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
044600                                                                          
044700 01  FILLER                      PIC X(16) VALUE 'SSA'.                   
044800                                                                          
044900 01  SSA1                        PIC X(128).                              
045000 01  SSA2                        PIC X(64).                               
045100     EJECT                                                                
045200*    --- IMS FUNKTIONSKODER                                               
045300*01  -COPY W0003                                                          
045400     EJECT                                                                
045500 01  FILLER                      PIC X(16) VALUE 'W418KTL1'.              
045600*---LÄNKAREA TILL KONTROLL-SUBPROGRAMMEN                                  
045700*01  -COPY W418KTL1           -PRE LINK-.                                 
045800     EJECT                                                                
045900 01  FILLER                      PIC X(16) VALUE 'W418KTL3'.              
046000*---LÄNKAREA TILL KONTROLL-SUBPROGRAM FÖR RETUR-MATRIXEN                  
046100*01  -COPY W418KTL3                                                       
046200     EJECT                                                                
046300 01  FILLER                      PIC X(16) VALUE 'P-TO-P-SW'.             
046400                                                                          
046500 01  4791-MSG-IO-AREA.                                                    
046600     03  4791-LL                 PIC S9(4) VALUE +0 COMP SYNC.            
046700     03  4791-Z1                 PIC X.                                   
046800     03  4791-Z2                 PIC X.                                   
046900     03  4791-TRANSKOD           PIC X(8)  VALUE 'W4T791X '.              
047000     03  4791-IDTRANS            PIC X(4)  VALUE '4791'.                  
047100     03  4791-SPRAK              PIC X.                                   
047200     03  FILLER.                                                          
047300*       05 -COPY W4I79101    -PRE 4791-.                                  
047400     03  FILLER                  PIC X(950) VALUE SPACE.                  
047500     EJECT                                                                
047600*                            DLI INPUT-OUTPUT AREA                        
047700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
047800                                                                          
047900 01  DLI-IO-AREA-WDB1.                                                    
048000*    03  WDB101    -COPY WDB101                                           
048100     EJECT                                                                
048200 01  DLI-IO-AREA-WDB2.                                                    
048300*    03  WDB201    -COPY WDB201                                           
048400     EJECT                                                                
048500 01  FILLER                      PIC X(16)   VALUE 'WDA201-AREA'.         
048600 01  DLI-IO-AREA-WDA201.                                                  
048700*    03 -COPY WDA201.                                                     
048800     EJECT                                                                
048900 01  FILLER                      PIC X(16)   VALUE 'WDA211-AREA'.         
049000 01  DLI-IO-AREA-WDA211.                                                  
049100*    03 -COPY WDA211.                                                     
049200     EJECT                                                                
049300 01  FILLER                      PIC X(16)   VALUE 'WDA2A1-AREA'.         
049400 01  DLI-IO-AREA-WDA2A1.                                                  
049500*    03  -COPY WDA2A1                                                     
049600                                                                          
049700 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDB601'.        
049800 01   DLI-IO-WDB601.                                                      
049900*     03  -COPY WDB601                                                    
050000     EJECT                                                                
050100 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDB611'.        
050200 01  DLI-IO-WDB611.                                                       
050300*    03  -COPY WDB611                                                     
050400     EJECT                                                                
050500 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK711'.        
050600 01  DLI-IO-WDK711.                                                       
050700*    03  -COPY WDK711                                                     
050800     EJECT                                                                
050900 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK601'.        
051000 01  DLI-IO-WDK601.                                                       
051100*    03  -COPY WDK601                                                     
051200                                                                          
051300     EJECT                                                                
051400 LINKAGE SECTION.                                                         
051500                                                                          
051600*01  -COPY W0009   -PRE MSG-                                              
051700     EJECT                                                                
051800*01  -COPY W0008  -PRE DISP-                                              
051900     05  FILLER                  PIC X.                                   
052000     EJECT                                                                
052100 01  ATAB-PCB                    PIC X.                                   
052200                                                                          
052300*01  -COPY W0008  -PRE KREE-                                              
052400     05  FILLER                  PIC X.                                   
052500     EJECT                                                                
052600*01  -COPY W0008  -PRE WDL5-                                              
052700     05  FILLER                  PIC X.                                   
052800     EJECT                                                                
052900*01  -COPY W0008  -PRE ARTC-                                              
053000     05  FILLER                  PIC X.                                   
053100     EJECT                                                                
053200*01  -COPY W0008  -PRE KOMA-                                              
053300     05  FILLER                  PIC X.                                   
053400     EJECT                                                                
053500*01  -COPY W0008  -PRE XXMI-                                              
053600     05  FILLER                  PIC X.                                   
053700     EJECT                                                                
053800*01  -COPY W0008  -PRE WDB1-                                              
053900     05  FILLER                  PIC X.                                   
054000     EJECT                                                                
054100*01  -COPY W0008  -PRE WDB2-                                              
054200     05  FILLER                  PIC X.                                   
054300     EJECT                                                                
054400*01  -COPY W0008  -PRE PARTC-                                             
054500     05  FILLER                  PIC X.                                   
054600     EJECT                                                                
054700*01  -COPY W0008  -PRE PWDK7-                                             
054800     05  FILLER                  PIC X.                                   
054900     EJECT                                                                
055000*01  -COPY W0008  -PRE PGMTA-                                             
055100     05  FILLER                  PIC X.                                   
055200     EJECT                                                                
055300*01  -COPY W0008  -PRE BETA-                                              
055400     05  FILLER                  PIC X.                                   
055500     EJECT                                                                
055600*01  -COPY W0008  -PRE PRIA-                                              
055700     05  FILLER                  PIC X.                                   
055800     EJECT                                                                
055900*01  -COPY W0008  -PRE GPRIB-                                             
056000     05  FILLER                  PIC X.                                   
056100     EJECT                                                                
056200*01  -COPY W0008  -PRE GMTB-                                              
056300     05  FILLER                  PIC X.                                   
056400     EJECT                                                                
056500*01  -COPY W0008  -PRE 9305-                                              
056600     05  FILLER                  PIC X.                                   
056700     EJECT                                                                
056800*01  -COPY W0008  -PRE WDA2A-                                             
056900     05  FILLER                  PIC X.                                   
057000     EJECT                                                                
057100*01  -COPY W0008  -PRE WDB1A-                                             
057200     05  FILLER                  PIC X.                                   
057300     EJECT                                                                
057400 01  4128-PCB                    PIC X.                                   
057500     EJECT                                                                
057600*01  -COPY W0008  -PRE WDB6-                                              
057700     05  FILLER                  PIC X.                                   
057800     EJECT                                                                
057900*01  -COPY W0008  -PRE WDK7-                                              
058000     05  FILLER                  PIC X.                                   
058100     EJECT                                                                
058200*01  -COPY W0008  -PRE WDK6-                                              
058300     05  FILLER                  PIC X.                                   
058400     EJECT                                                                
058500*01  -COPY W0008  -PRE WDA2-                                              
058600     05  FILLER                  PIC X.                                   
058700     EJECT                                                                
058800 01  COST-WDK6-PCB               PIC X.                                   
058900 01  COST-WDK7-PCB               PIC X.                                   
059000 01  COST-WDF1-PCB               PIC X.                                   
059100 01  COST-9305-PCB               PIC X.                                   
059200 01  COST-WDK72-PCB              PIC X.                                   
059300 01  COST-WDB6-PCB               PIC X.                                   
059400 01  PRIS-COST-WDK6-PCB          PIC X.                                   
059500 01  PRIS-COST-WDK7-PCB          PIC X.                                   
059600 01  PRIS-COST-WDF1-PCB          PIC X.                                   
059700 01  PRIS-COST-9305-PCB          PIC X.                                   
059800 01  PRIS-COST-WDK72-PCB         PIC X.                                   
059900 01  PRIS-COST-WDB6-PCB          PIC X.                                   
060000 01  KTL3-WDA8-PCB               PIC X.                                   
060100 01  KTL3-WDB2-PCB               PIC X.                                   
060200 01  KTL3-WDK6-PCB               PIC X.                                   
060300 01  KTL3-WDK7-PCB               PIC X.                                   
060400 01  KTL3-WDB6-PCB               PIC X.                                   
060500 01  KTL3-1165-PCB               PIC X.                                   
060600     EJECT                                                                
060700 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB ATAB-PCB                      
060800                          KREE-PCB WDL5-PCB ARTC-PCB                      
060900                          KOMA-PCB XXMI-PCB WDB1-PCB WDB2-PCB             
061000                          PARTC-PCB PWDK7-PCB PGMTA-PCB                   
061100                          BETA-PCB PRIA-PCB GPRIB-PCB                     
061200                          GMTB-PCB 9305-PCB WDA2A-PCB                     
061300                          WDB1A-PCB 4128-PCB WDB6-PCB                     
061400                          WDK7-PCB WDK6-PCB WDA2-PCB                      
061500                          COST-WDK6-PCB                                   
061600                          COST-WDK7-PCB                                   
061700                          COST-WDF1-PCB                                   
061800                          COST-9305-PCB                                   
061900                          COST-WDK72-PCB                                  
062000                          COST-WDB6-PCB                                   
062100                          PRIS-COST-WDK6-PCB                              
062200                          PRIS-COST-WDK7-PCB                              
062300                          PRIS-COST-WDF1-PCB                              
062400                          PRIS-COST-9305-PCB                              
062500                          PRIS-COST-WDK72-PCB                             
062600                          PRIS-COST-WDB6-PCB                              
062700                          KTL3-WDA8-PCB                                   
062800                          KTL3-WDB2-PCB                                   
062900                          KTL3-WDK6-PCB                                   
063000                          KTL3-WDK7-PCB                                   
063100                          KTL3-WDB6-PCB                                   
063200                          KTL3-1165-PCB.                                  
063300 MAIN SECTION.                                                            
063400     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB ATAB-PCB                      
063500                          KREE-PCB WDL5-PCB ARTC-PCB                      
063600                          KOMA-PCB XXMI-PCB WDB1-PCB WDB2-PCB             
063700                          PARTC-PCB PWDK7-PCB PGMTA-PCB                   
063800                          BETA-PCB PRIA-PCB GPRIB-PCB                     
063900                          GMTB-PCB 9305-PCB WDA2A-PCB                     
064000                          WDB1A-PCB 4128-PCB WDB6-PCB                     
064100                          WDK7-PCB WDK6-PCB WDA2-PCB                      
064200                          COST-WDK6-PCB                                   
064300                          COST-WDK7-PCB                                   
064400                          COST-WDF1-PCB                                   
064500                          COST-9305-PCB                                   
064600                          COST-WDK72-PCB                                  
064700                          COST-WDB6-PCB                                   
064800                          PRIS-COST-WDK6-PCB                              
064900                          PRIS-COST-WDK7-PCB                              
065000                          PRIS-COST-WDF1-PCB                              
065100                          PRIS-COST-9305-PCB                              
065200                          PRIS-COST-WDK72-PCB                             
065300                          PRIS-COST-WDB6-PCB                              
065400                          KTL3-WDA8-PCB                                   
065500                          KTL3-WDB2-PCB                                   
065600                          KTL3-WDK6-PCB                                   
065700                          KTL3-WDK7-PCB                                   
065800                          KTL3-WDB6-PCB                                   
065900                          KTL3-1165-PCB.                                  
066000                                                                          
066100     PERFORM S11-FETCH-REQUEST-ARGUMENT                                   
066200     IF SUB-KDRC = 0                                                      
066300       PERFORM A-INIT                                                     
066400       PERFORM B-KOLLA-NYCKLAR                                            
066500       IF  NYCKLAR-OK AND EGEN-MID                                        
066600         PERFORM S01-DIST-KUND-LDC                                        
066700         PERFORM C-KOLLA-INPUT-FAELT                                      
066800         IF ALLT-OK                                                       
066900            PERFORM D-KALLA-PA-KONTROLLPROGRAM                            
067000            IF FEL = NEJ AND SPAR-FEL = NEJ AND EVERYTHING-OK             
067100               PERFORM E-UPPDATERA-DISPATCHEN                             
067200               MOVE INF-UPDATE-DONE               TO MED-IDMFSFEL         
067300               CALL WMEDKONV USING MED-WMEDAREA                           
067400               MOVE MED-MFSFEL                    TO MOD-TEMFSFEL         
067500               MOVE DISCP-CREATED                 TO RESP-IDMFSINF        
067600               MOVE 'DISCREPANCY CREATED '                                
067700                                                  TO RESP-TEMFSINF        
067800               PERFORM MFS-RENSA-FAELT-UT                                 
067900            ELSE                                                          
068000             IF API-SW = 'Y'                                              
068100               PERFORM DF-KOLLA-API-FELKODER                              
068200             ELSE                                                         
068300               PERFORM MFS-ROER-EJ-FAELT-UT                               
068400             END-IF                                                       
068500            END-IF                                                        
068600         END-IF                                                           
068700       END-IF                                                             
068800*      MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
068900       MOVE LENGTH OF MOD  TO MOD-KVLL                                    
069000       IF API-SW ='N'                                                     
069100         PERFORM IMS-INSERT-MSG                                           
069200       ELSE                                                               
069300         PERFORM S12-RETURN-RESPONSE                                      
069400       END-IF                                                             
069500     END-IF                                                               
069600                                                                          
069700     MOVE ZERO TO RETURN-CODE                                             
069800     GOBACK                                                               
069900     .                                                                    
070000     EJECT                                                                
070100 A-INIT SECTION.                                                          
070200                                                                          
070300     MOVE SPACE                         TO 4791-MID-W4I79101              
070400                                           SPAR-W4I79102                  
070500*  IF CALL IS FROM CLASSIC SCREEN                                         
070600     IF SUB-KDTRANS(1:6) = 'W4T704'                                       
070700        MOVE SUB-KDTRANS                  TO MSG-KDTRANS-1                
070800        MOVE SUB-DATA                     TO MSG-AREA(9:1925)             
070900        IF MSG-DUBBLA-TRANSKODER                                          
071000          MOVE MSG-INDATA-MINUS-2-TRANSKODER                              
071100                                          TO MID-W4I70401                 
071200          MOVE MSG-IDTRANS-2              TO MFS-IDTRANS                  
071300          MOVE MSG-KDMFSFOR-2             TO MFS-KDMFSFOR                 
071400        ELSE                                                              
071500          MOVE MSG-INDATA-MINUS-1-TRANSKOD                                
071600                                          TO MID-W4I70401                 
071700          MOVE MSG-IDTRANS-1              TO MFS-IDTRANS                  
071800          MOVE MSG-KDMFSFOR-1             TO MFS-KDMFSFOR                 
071900        END-IF                                                            
072000        MOVE MSG-KDTRTYP                  TO MFS-KDTRTYP                  
072100        MOVE MSG-IDPFK                    TO MFS-IDPFK                    
072200        MOVE MFS-IDTRANS                  TO W-IDTRANS                    
072300                                                                          
072400     ELSE                                                                 
072500*  IF CALL IS FROM LYNK OR ECOM OR VOUI API                               
072600        MOVE SUB-DATA(1:SUB-KVDLEN)       TO REQU-AREA                    
072700        MOVE 001                          TO AUTH-KDCALL                  
072800        CALL WZ01AUTH                  USING AUTH-WZ01AUTH                
072900                                             REQU-WZ01REQ2                
073000        IF AUTH-KDRC = 0                                                  
073100          IF REQU-KDPGMACT = 'E'                                          
073200            MOVE SPACE                     TO MID-W4I70401                
073300            MOVE REQU-IDDISTR              TO MID-IDDISTR-IN              
073400            MOVE REQU-IDKUNDNR             TO MID-IDKUNDNR-IN             
073500            MOVE REQU-IDRAPPNR             TO MID-IDRAPPNR-IN             
073600            MOVE 'Y'                       TO API-SW                      
073700            MOVE '4704'                    TO W-IDTRANS                   
073800            MOVE AUTH-IDSYSTEM             TO 4791-MID-IDSYSTEM           
073900          ELSE                                                            
074000            MOVE SYS-ERROR                 TO RESP-IDMSG-ERROR            
074100          END-IF                                                          
074200        ELSE                                                              
074300          IF AUTH-KDRC = 4                                                
074400             MOVE BAD-REQUEST TO RESP-IDMSG-ERROR                         
074500                                                                          
074600             MOVE AUTH-KDRC TO KDRC-DISPLAY                               
074700             STRING 'WZ01AUTH GETARG ERROR RC=' KDRC-DISPLAY              
074800             DELIMITED BY SIZE INTO ERROR-TEXT                            
074900             CALL ABEND USING RKOD-ABEND-WITH-DUMP                        
075000          END-IF                                                          
075100        END-IF                                                            
075200     END-IF                                                               
075300                                                                          
075400                                                                          
075500     MOVE LOW-VALUE                       TO MSG-AREA                     
075600     MOVE 'W4O70401'                      TO MFS-IDMOD                    
075700     MOVE '4704'                          TO MOD-IDTRANS                  
075800     PERFORM MFS-RENSA-FAELT-UT                                           
075900     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
076000                                             MOD-TEMFSINF                 
076100                                                                          
076200     MOVE SPACE                           TO MED-IDMFSFEL                 
076300                                                                          
076400*    --- OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4               
076500*    --- OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17              
076600     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O70401 + 4                  
076700     IF EGEN-MID OR HELP-MID                                              
076800       CONTINUE                                                           
076900     ELSE                                                                 
077000       MOVE SPACE                         TO MFS-KDTRTYP                  
077100       MOVE '7'                           TO MFS-IDPFK                    
077200       PERFORM MFS-RENSA-NYCKELFAELT-UT                                   
077300     END-IF                                                               
077400                                                                          
077500     IF ENGLISH-TEXT                                                      
077600       MOVE 'GB '                         TO MED-IDSKYLT                  
077700     ELSE                                                                 
077800       MOVE 'S  '                         TO MED-IDSKYLT                  
077900     END-IF                                                               
078000                                                                          
078100     ACCEPT DAGENS-DATUM               FROM DATE                          
078200     ACCEPT DAGENS-TID                 FROM TIME                          
078300                                                                          
078400     MOVE +0                           TO WS-KVRADER                      
078500     MOVE LOW-VALUE                    TO W-IDGMT-MIN-X                   
078600                                          W-IDLEVANM-X                    
078700     MOVE HIGH-VALUE                   TO W-IDGMT-MAX-X                   
078800                                                                          
078900     .                                                                    
079000     EJECT                                                                
079100 B-KOLLA-NYCKLAR SECTION.                                                 
079200                                                                          
079300     MOVE JA                           TO NYCKLAR-SW                      
079400                                                                          
079500     PERFORM BA-KONTROLLERA-IDDISTR                                       
079600     IF NYCKLAR-OK                                                        
079700      PERFORM BB-KONTROLLERA-IDKUNDNR                                     
079800     END-IF                                                               
079900     IF NYCKLAR-OK                                                        
080000      PERFORM BC-KONTROLLERA-IDRAPPNR                                     
080100     END-IF                                                               
080200     IF NYCKLAR-OK                                                        
080300        PERFORM BD-KOLLA-ATT-KUNDEN-FINNS                                 
080400        IF API-SW = 'N'                                                   
080500          PERFORM BE-KOLLA-SECURIT                                        
080600          PERFORM BF-KOLLA-OM-UPPDAT-OK                                   
080700        END-IF                                                            
080800     END-IF                                                               
080900                                                                          
081000     PERFORM MFS-RENSA-FAELT-IN                                           
081100     .                                                                    
081200     EJECT                                                                
081300 BA-KONTROLLERA-IDDISTR SECTION.                                          
081400                                                                          
081500     IF MID-IDDISTR-IN NOT = ALL '+'                                      
081600        MOVE MID-IDDISTR-IN               TO WS-IDDISTR                   
081700        MOVE SPACE                        TO MFS-KDTRTYP                  
081800        MOVE '7'                          TO MFS-IDPFK                    
081900        MOVE NEJ                          TO FORTSAETT-SW                 
082000     ELSE                                                                 
082100        MOVE JA                           TO FORTSAETT-SW                 
082200        MOVE MID-IDDISTR-UT               TO WS-IDDISTR                   
082300        INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                
082400     END-IF                                                               
082500                                                                          
082600     MOVE FUNCTION TRIM(WS-IDDISTR)      TO WS-IDDISTR-N                  
082700     MOVE WS-IDDISTR-N                   TO WS-IDDISTR                    
082800     IF WS-IDDISTR  NUMERIC AND WS-IDDISTR  > ZERO                        
082900        MOVE WS-IDDISTR                   TO MOD-IDDISTR-UT               
083000                                             RESP-IDDISTR                 
083100        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
083200     ELSE                                                                 
083300        MOVE NEJ                          TO NYCKLAR-SW                   
083400* ERROR CODE FOR API                                                      
083500        MOVE BAD-REQUEST                  TO RESP-IDMFSINF                
083600        MOVE 'DISTRICT NOT CORRECT'                                       
083700                                          TO RESP-TEMFSINF                
083800     END-IF                                                               
083900     .                                                                    
084000     EJECT                                                                
084100 BB-KONTROLLERA-IDKUNDNR SECTION.                                         
084200                                                                          
084300     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
084400        MOVE MID-IDKUNDNR-IN              TO WS-IDKUNDNR                  
084500        MOVE SPACE                        TO MFS-KDTRTYP                  
084600        MOVE '7'                          TO MFS-IDPFK                    
084700        MOVE NEJ                          TO FORTSAETT-SW                 
084800     ELSE                                                                 
084900        MOVE JA                           TO FORTSAETT-SW                 
085000        MOVE MID-IDKUNDNR-UT              TO WS-IDKUNDNR                  
085100        INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO               
085200     END-IF                                                               
085300                                                                          
085400     MOVE FUNCTION TRIM(WS-IDKUNDNR)     TO WS-IDKUNDNR-N                 
085500     MOVE WS-IDKUNDNR-N                  TO WS-IDKUNDNR                   
085600     IF WS-IDKUNDNR NUMERIC                                               
085700       IF WS-IDKUNDNR = ZERO                                              
085800        MOVE '     0'                     TO MOD-IDKUNDNR-UT              
085900        MOVE '0'                          TO RESP-IDKUNDNR                
086000       ELSE                                                               
086100        MOVE WS-IDKUNDNR                  TO MOD-IDKUNDNR-UT              
086200                                             RESP-IDKUNDNR                
086300        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
086400       END-IF                                                             
086500     ELSE                                                                 
086600        MOVE NEJ                          TO NYCKLAR-SW                   
086700* ERROR CODE FOR API                                                      
086800        MOVE BAD-REQUEST                  TO RESP-IDMFSINF                
086900        MOVE 'CUSTOMER NOT CORRECT '                                      
087000                                          TO RESP-TEMFSINF                
087100     END-IF                                                               
087200     .                                                                    
087300     EJECT                                                                
087400 BC-KONTROLLERA-IDRAPPNR SECTION.                                         
087500                                                                          
087600     IF MID-IDRAPPNR-IN NOT = ALL '+'                                     
087700        MOVE MID-IDRAPPNR-IN              TO WS-IDRAPPNR                  
087800        MOVE SPACE                        TO MFS-KDTRTYP                  
087900        MOVE '7'                          TO MFS-IDPFK                    
088000        MOVE NEJ                          TO FORTSAETT-SW                 
088100     ELSE                                                                 
088200        MOVE JA                           TO FORTSAETT-SW                 
088300        MOVE MID-IDRAPPNR-UT              TO WS-IDRAPPNR                  
088400        INSPECT WS-IDRAPPNR REPLACING LEADING SPACE BY ZERO               
088500     END-IF                                                               
088600                                                                          
088700     MOVE FUNCTION TRIM(WS-IDRAPPNR)      TO WS-IDRAPPNR-N                
088800     MOVE WS-IDRAPPNR-N                   TO WS-IDRAPPNR                  
088900     IF WS-IDRAPPNR NUMERIC AND WS-IDRAPPNR > ZERO                        
089000       MOVE WS-IDRAPPNR                   TO MOD-IDRAPPNR-UT              
089100                                             RESP-IDRAPPNR                
089200       INSPECT MOD-IDRAPPNR-UT REPLACING LEADING ZERO BY SPACE            
089300     ELSE                                                                 
089400       MOVE NEJ                           TO NYCKLAR-SW                   
089500* ERROR CODE FOR API                                                      
089600        MOVE BAD-REQUEST                  TO RESP-IDMFSINF                
089700        MOVE 'DISCREPANCY NO NOT CORRECT'                                 
089800                                          TO RESP-TEMFSINF                
089900     END-IF                                                               
090000     .                                                                    
090100     EJECT                                                                
090200                                                                          
090300 BD-KOLLA-ATT-KUNDEN-FINNS SECTION.                                       
090400                                                                          
090500     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
090600       MOVE WS-IDDISTR                    TO W-IDDISTR-WDB2-MIN           
090700                                             W-IDDISTR-WDB2-MAX           
090800                                             TEST-IDDISTR                 
090900     ELSE                                                                 
091000       MOVE ZERO                          TO W-IDDISTR-WDB2-MIN           
091100                                             W-IDDISTR-WDB2-MAX           
091200                                             TEST-IDDISTR                 
091300     END-IF                                                               
091400                                                                          
091500     MOVE WS-IDKUNDNR                     TO W-IDKUNDNR-WDB2-MIN          
091600     IF API-SW = 'Y'                                                      
091700       MOVE WS-IDKUNDNR                   TO W-IDKUNDNR-WDB2-MAX          
091800     END-IF                                                               
091900     PERFORM IMS-GET-WDB201                                               
092000                                                                          
092100                                                                          
092200     IF SEGMENT-FINNS                                                     
092300        IF NYCKLAR-FEL                                                    
092400          MOVE ERR-WRONG-KEY              TO MED-IDMFSFEL                 
092500          CALL WMEDKONV USING MED-WMEDAREA                                
092600          MOVE MED-MFSFEL                 TO MOD-TEMFSFEL                 
092700          PERFORM MFS-RENSA-FAELT-UT                                      
092800* ERROR CODE FOR API                                                      
092900          MOVE BAD-REQUEST                TO RESP-IDMFSINF                
093000          MOVE 'WRONG VALUE IN DISTRICT OR CUSTOMER'                      
093100                                          TO RESP-TEMFSINF                
093200        END-IF                                                            
093300                                                                          
093400        IF DIST79-DEALER-PRICE                                            
093600          MOVE GMT-IDPARTNR               TO W-WDB1-IDPARTNR              
093700          MOVE GMT-IDFTG                  TO W-WDB1-IDFTG                 
093800          PERFORM IMS-GET-WDB101                                          
093900          IF SEGMENT-FINNS                                                
094000            MOVE BET-KDVALISO             TO MOD-KDVALISO                 
094100                                             WS-KDVALISO                  
094200          ELSE                                                            
094300            MOVE SPACE                    TO MOD-KDVALISO                 
094400                                             WS-KDVALISO                  
094500          END-IF                                                          
094600        ELSE                                                              
094700          IF DIST79-ECOM-PRICE                                            
094800            MOVE SPACE                    TO MOD-KDVALISO                 
094900                                             WS-KDVALISO                  
095000          ELSE                                                            
095100            MOVE 'SEK'                    TO MOD-KDVALISO                 
095200          END-IF                                                          
095300        END-IF                                                            
095400     ELSE                                                                 
095500        IF NYCKLAR-FEL                                                    
095600           MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                 
095700           CALL WMEDKONV USING MED-WMEDAREA                               
095800           MOVE MED-MFSFEL                TO MOD-TEMFSFEL                 
095900* ERROR CODE FOR API                                                      
096000           PERFORM MFS-RENSA-FAELT-UT                                     
096100           MOVE BAD-REQUEST               TO RESP-IDMFSINF                
096200           MOVE 'ERROR IN DISTRICT/CUSTOMER'                              
096300                                          TO RESP-TEMFSINF                
096400        ELSE                                                              
096500* ERROR CODE FOR API                                                      
096600           MOVE NOT-FOUND                 TO RESP-IDMFSINF                
096700           MOVE 'DISTRICT OR CUSTOMER NOT FOUND '                         
096800                                          TO RESP-TEMFSINF                
096900           MOVE NEJ                       TO NYCKLAR-SW                   
097000           MOVE ERR-CUST-MISSING          TO MED-IDMFSFEL                 
097100           CALL WMEDKONV USING MED-WMEDAREA                               
097200           MOVE MED-MFSFEL                TO MOD-TEMFSFEL                 
097300        END-IF                                                            
097400     END-IF                                                               
097500     .                                                                    
097600     EJECT                                                                
097700 BE-KOLLA-SECURIT SECTION.                                                
097800                                                                          
097900     MOVE MSG-SIGNON-USERID       TO SEC-IDUSER                           
098000     MOVE '4704'                  TO SEC-IDTRANS                          
098100     MOVE WS-IDDISTR              TO SEC-IDKEY                            
098200                                                                          
098300     CALL WSECURIT USING SEC-IDUSER                                       
098400                         SEC-IDTRANS                                      
098500                         SEC-IDKEY                                        
098600                         SEC-KDSVAR                                       
098700                                                                          
098800     IF SEC-KDSVAR = 'F'                                                  
098900        MOVE ERR-OBEHORIG         TO MED-IDMFSFEL                         
099000        MOVE NEJ                  TO NYCKLAR-SW                           
099100        CALL WMEDKONV USING MED-WMEDAREA                                  
099200        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
099300        PERFORM MFS-RENSA-FAELT-UT                                        
099400     END-IF                                                               
099500     .                                                                    
099600     EJECT                                                                
099700 BF-KOLLA-OM-UPPDAT-OK SECTION.                                           
099800                                                                          
099900     IF WS-IDDISTR = '2640' OR '2699' OR '2602' OR '2697'                 
100000        MOVE ERR-UPPDAT-EJ-TILLATEN TO MED-IDMFSFEL                       
100100        MOVE NEJ                    TO NYCKLAR-SW                         
100200        CALL WMEDKONV USING MED-WMEDAREA                                  
100300        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
100400        PERFORM MFS-RENSA-FAELT-UT                                        
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800 C-KOLLA-INPUT-FAELT SECTION.                                             
100900                                                                          
101000     MOVE JA                              TO ALLT-SW                      
101100     PERFORM CA-FORMELL-KONTROLL                                          
101200                                                                          
101300     IF ALLT-OK                                                           
101400        CONTINUE                                                          
101500     ELSE                                                                 
101600        IF OBEHORIG                                                       
101700           MOVE ERR-OBEHORIG              TO MED-IDMFSFEL                 
101800           CALL WMEDKONV USING MED-WMEDAREA                               
101900           MOVE MED-MFSFEL                TO MOD-TEMFSFEL                 
102000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
102100        ELSE                                                              
102200           IF TOM-RAD-1                                                   
102300             MOVE ERR-ON-LINE-1           TO MED-IDMFSFEL                 
102400* ERROR CODE FOR API                                                      
102500             MOVE BAD-REQUEST             TO RESP-IDMFSINF                
102600             MOVE 'DATA ON LINE 1 MISSING'                                
102700                                          TO RESP-TEMFSINF                
102800           ELSE                                                           
102900             IF DIST79-DEALER-PRICE                                       
103000                AND TILLAEGGSKOST-FINNS                                   
103100               MOVE ERR-UPD-NOT-ALLOWED   TO MED-IDMFSFEL                 
103200             ELSE                                                         
103300               IF MED-IDMFSFEL  = SPACE                                   
103400                 MOVE ERR-WRONG-INPUT     TO MED-IDMFSFEL                 
103500               END-IF                                                     
103600             END-IF                                                       
103700           END-IF                                                         
103800           CALL WMEDKONV USING MED-WMEDAREA                               
103900           MOVE MED-MFSFEL                TO MOD-TEMFSFEL                 
104000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
104100        END-IF                                                            
104200     END-IF                                                               
104300     IF TILLAEGGSKOST-FINNS OR RADER-FINNS                                
104400                            OR MID-LEVANM-KLAR = 'J'                      
104500                            OR MID-LEVANM-KLAR = 'Y'                      
104600                                                                          
104700        IF API-SW = 'Y'                                                   
104800            MOVE JA                       TO MID-LEVANM-KLAR              
104900        END-IF                                                            
105000        MOVE WS-IDDISTR                   TO W-IDDISTR-WDA2               
105100                                             W-IDDISTR                    
105200        MOVE WS-IDKUNDNR                  TO W-IDKUNDNR-WDA2              
105300        MOVE WS-IDRAPPNR                  TO W-IDRAPPNR-WDA2              
105400                                             W-IDRAPPNR                   
105500                                                                          
105600        PERFORM IMS-GET-WDA201                                            
105700        IF SEGMENT-FINNS                                                  
105800           IF ANM-KDLEVANM = '0'                                          
105900             PERFORM IMS-GNP-WDA211                                       
106000             IF SEGMENT-FINNS                                             
106100               PERFORM CB-KOLLA-UPPLAGDA-RADER                            
106200             END-IF                                                       
106300           ELSE                                                           
106400               MOVE ERR-ALREADY-EXISTS    TO MED-IDMFSFEL                 
106500               CALL WMEDKONV USING MED-WMEDAREA                           
106600               MOVE MED-MFSFEL            TO MOD-TEMFSFEL                 
106700               PERFORM MFS-ROER-EJ-FAELT-UT                               
106800               MOVE NEJ                   TO ALLT-SW                      
106900**API ERROR CODE                                                          
107000               MOVE DUPLICATE-DR    TO RESP-IDMFSINF                      
107100               MOVE 'DISCREPANCY NO ALREADY EXISTS '                      
107200                                    TO RESP-TEMFSINF                      
107300           END-IF                                                         
107400        ELSE                                                              
107500******                                                                    
107600* LAGT TILL EN LÄSNING FÖR ATT KOLLA SÅ ATT INTE SAMMA RAPPNR OCH         
107700* DISTRIKT FAST OLIKA KUNDNR ÄR LAGDA SAMMA DAG. BILL-IT OCH SAP          
107800* KLARAR INTE DETTA.                                                      
107900*****                                                                     
108000           PERFORM IMS-GN-WDA2A1                                          
108100           IF SEGMENT-FINNS                                               
108200             MOVE ERR-ALREADY-EXISTS    TO MED-IDMFSFEL                   
108300             CALL WMEDKONV USING MED-WMEDAREA                             
108400             MOVE MED-MFSFEL            TO MOD-TEMFSFEL                   
108500             PERFORM MFS-ROER-EJ-FAELT-UT                                 
108600             MOVE NEJ                   TO ALLT-SW                        
108700**API ERROR CODE                                                          
108800             MOVE DUPLICATE-DR          TO RESP-IDMFSINF                  
108900             MOVE 'DISCREPANCY NO ALREADY EXISTS '                        
109000                                        TO RESP-TEMFSINF                  
109100           ELSE                                                           
109200             IF RADER-FINNS                                               
109300               CONTINUE                                                   
109400             ELSE                                                         
109500               MOVE ERR-NO-UPDATE                TO MED-IDMFSFEL          
109600               CALL WMEDKONV USING MED-WMEDAREA                           
109700               MOVE MED-MFSFEL                   TO MOD-TEMFSFEL          
109800               MOVE NEJ                          TO ALLT-SW               
109900               PERFORM MFS-RENSA-FAELT-UT                                 
110000**API ERROR CODE                                                          
110100               MOVE BAD-REQUEST                  TO RESP-IDMFSINF         
110200               MOVE 'NO INPUT LINE CREATED'                               
110300                                                 TO RESP-TEMFSINF         
110400             END-IF                                                       
110500           END-IF                                                         
110600        END-IF                                                            
110700     ELSE                                                                 
110800        MOVE ERR-NO-UPDATE                TO MED-IDMFSFEL                 
110900        CALL WMEDKONV USING MED-WMEDAREA                                  
111000        MOVE MED-MFSFEL                   TO MOD-TEMFSFEL                 
111100        MOVE NEJ                          TO ALLT-SW                      
111200        PERFORM MFS-RENSA-FAELT-UT                                        
111300**API ERROR CODE                                                          
111400        MOVE BAD-REQUEST                  TO RESP-IDMFSINF                
111500        MOVE 'NO INPUT LINE CREATED'                                      
111600                                          TO RESP-TEMFSINF                
111700     END-IF                                                               
111800     .                                                                    
111900     EJECT                                                                
112000 CA-FORMELL-KONTROLL SECTION.                                             
112100                                                                          
112200     PERFORM CAA-KOLLA-RADER                                              
112300     IF API-SW = 'N'                                                      
112400        PERFORM CAB-KOLLA-TILLAEGGSKOST                                   
112500     END-IF                                                               
112600     .                                                                    
112700     EJECT                                                                
112800 CAA-KOLLA-RADER SECTION.                                                 
112900                                                                          
113000     MOVE NEJ                             TO RADER-SW                     
113100     MOVE NEJ                             TO LDC-SW                       
113200     MOVE NEJ                             TO KOD-72-LDC-SW                
113300     MOVE NEJ                             TO KOD-74-FINNS-SW              
113400     MOVE NEJ                             TO KOD-74-SAKNAS-SW             
113500     MOVE NEJ                             TO TOM-RAD-1-SW                 
113600     MOVE WS-IDDISTR                      TO TEST-IDDISTR                 
113700     MOVE +1                              TO IX                           
113800                                                                          
113900     IF SUB-KDTRANS(1:6)='W4T704'                                         
114000        CONTINUE                                                          
114100     ELSE                                                                 
114200        MOVE REQU-KVRADER                 TO MAX-IX                       
114300        MOVE REQU-KDANMORS(IX)            TO MID-KDANMORS(IX)             
114400     END-IF                                                               
114500     IF GMT-FLLDCKND = JA OR                                              
114600        GMT-FLRETUR  = JA                                                 
114700       IF MID-KDANMORS (IX) NOT = ALL '+'                                 
114800         IF MID-KDANMORS (IX) = '72'                                      
114900            MOVE JA TO KOD-72-LDC-SW                                      
115000         ELSE                                                             
115100            MOVE JA TO LDC-SW                                             
115200         END-IF                                                           
115300       END-IF                                                             
115400     END-IF                                                               
115500     IF MID-KDANMORS (IX) NOT = ALL '+'                                   
115600       IF MID-KDANMORS (IX) = '74'                                        
115700          MOVE JA TO KOD-74-FINNS-SW                                      
115800       ELSE                                                               
115900          MOVE JA TO KOD-74-SAKNAS-SW                                     
116000       END-IF                                                             
116100     END-IF                                                               
116200                                                                          
116300     PERFORM UNTIL IX > MAX-IX OR  SOMETHING-WRONG                        
116400       MOVE IX                           TO MOD-KVRADER                   
116500       IF API-SW = 'Y'                                                    
116600          MOVE REQU-INFO-RAD(IX)         TO MID-INFO-RAD(IX)              
116700       END-IF                                                             
116800       IF MID-INFO-RAD (IX) NOT = ALL '+'                                 
116900          IF API-SW = 'Y'                                                 
117000            MOVE FUNCTION TRIM(REQU-IDORDNR(IX))                          
117100                                          TO WS-IDORDNR-N                 
117200            MOVE WS-IDORDNR-N             TO MID-IDORDNR5 (IX)            
117300          END-IF                                                          
117400                                                                          
117500         IF MID-IDORDNR5 (IX)            NOT = ALL '+'                    
117600            MOVE JA                       TO RADER-SW                     
117700            IF MID-IDORDNR5 (IX) NUMERIC AND                              
117800               MID-IDORDNR5 (IX)  > ZERO                                  
117900               MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDORDNR5-ATTR(IX)        
118000            ELSE                                                          
118100               MOVE NEJ                   TO ALLT-SW                      
118200               MOVE NEJ                   TO OK-SW                        
118300               MOVE MFS-NUM-FAELT-FEL     TO MOD-IDORDNR5-ATTR(IX)        
118400* ERROR CODE FOR API                                                      
118500               MOVE BAD-REQUEST           TO RESP-IDMFSINF                
118600               MOVE IX                    TO W-LINENO                     
118700               STRING 'ORDERNO NOT CORRECT ON ' W-LINENO                  
118800               DELIMITED BY SIZE INTO RESP-TEMFSINF                       
118900            END-IF                                                        
119000         END-IF                                                           
119100                                                                          
119200         IF API-SW = 'Y'                                                  
119300            MOVE FUNCTION TRIM(REQU-IDKOLLI(IX))                          
119400                                          TO WS-IDKOLLI-N                 
119500            MOVE WS-IDKOLLI-N             TO MID-IDKOLLI  (IX)            
119600         END-IF                                                           
119700         IF MID-IDKOLLI (IX)             NOT = ALL '+'                    
119800            MOVE JA                       TO RADER-SW                     
119900            IF MID-IDKOLLI (IX) NUMERIC AND                               
120000               MID-IDKOLLI (IX)   > ZERO                                  
120100               MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDKOLLI-ATTR(IX)         
120200            ELSE                                                          
120300               MOVE NEJ                   TO ALLT-SW                      
120400               MOVE NEJ                   TO OK-SW                        
120500               MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKOLLI-ATTR(IX)         
120600               MOVE BAD-REQUEST           TO RESP-IDMFSINF                
120700* ERROR CODE FOR API                                                      
120800               MOVE IX                    TO W-LINENO                     
120900               STRING 'CASENO NOT CORRECT ON ' W-LINENO                   
121000               DELIMITED BY SIZE INTO RESP-TEMFSINF                       
121100            END-IF                                                        
121200         END-IF                                                           
121300                                                                          
121400         IF API-SW = 'Y'                                                  
121500            MOVE FUNCTION TRIM(REQU-IDARTNR(IX))                          
121600                                          TO WS-IDARTNR-N                 
121700            MOVE WS-IDARTNR-N             TO MID-IDARTNR  (IX)            
121800         END-IF                                                           
121900         IF MID-IDARTNR (IX)             NOT = ALL '+'                    
122000            MOVE JA                       TO RADER-SW                     
122100            IF MID-IDARTNR (IX) NUMERIC AND                               
122200               MID-IDARTNR (IX)   > ZERO                                  
122300               MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDARTNR-ATTR(IX)         
122400            ELSE                                                          
122500               MOVE NEJ                   TO ALLT-SW                      
122600               MOVE NEJ                   TO OK-SW                        
122700               MOVE MFS-NUM-FAELT-FEL     TO MOD-IDARTNR-ATTR(IX)         
122800               MOVE BAD-REQUEST           TO RESP-IDMFSINF                
122900               MOVE IX                    TO W-LINENO                     
123000               STRING 'PARTNO NOT CORRECT ON ' W-LINENO                   
123100               DELIMITED BY SIZE INTO RESP-TEMFSINF                       
123200            END-IF                                                        
123300         ELSE                                                             
123400            MOVE NEJ                      TO ALLT-SW                      
123500            MOVE MFS-NUM-FAELT-FEL        TO MOD-IDARTNR-ATTR(IX)         
123600         END-IF                                                           
123700                                                                          
123800         IF API-SW = 'Y'                                                  
123900            MOVE FUNCTION TRIM(REQU-KVLEVANM (IX))                        
124000                                          TO WS-KVLEVANM-N                
124100            MOVE WS-KVLEVANM-N            TO MID-KVLEVANM (IX)            
124200         END-IF                                                           
124300         IF MID-KVLEVANM (IX)            NOT = ALL '+'                    
124400            MOVE JA                       TO RADER-SW                     
124500            IF MID-KVLEVANM (IX) NUMERIC AND                              
124600               MID-KVLEVANM (IX)  > ZERO                                  
124700               MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVLEVANM-ATTR(IX)        
124800            ELSE                                                          
124900               MOVE NEJ                   TO ALLT-SW                      
125000               MOVE NEJ                   TO OK-SW                        
125100               MOVE MFS-NUM-FAELT-FEL     TO MOD-KVLEVANM-ATTR(IX)        
125200* ERROR CODE FOR API                                                      
125300               MOVE BAD-REQUEST           TO RESP-IDMFSINF                
125400               MOVE IX                    TO W-LINENO                     
125500               STRING 'QTY NOT CORRECT ON  ' W-LINENO                     
125600               DELIMITED BY SIZE INTO RESP-TEMFSINF                       
125700            END-IF                                                        
125800         ELSE                                                             
125900            MOVE NEJ                      TO ALLT-SW                      
126000            MOVE NEJ                      TO OK-SW                        
126100            MOVE MFS-NUM-FAELT-FEL        TO MOD-KVLEVANM-ATTR(IX)        
126200* ERROR CODE FOR API                                                      
126300            MOVE BAD-REQUEST              TO RESP-IDMFSINF                
126400            MOVE IX                       TO W-LINENO                     
126500            STRING 'QTY NOT PRESENT ON ' W-LINENO                         
126600            DELIMITED BY SIZE INTO RESP-TEMFSINF                          
126700         END-IF                                                           
126800         IF API-SW = 'Y'                                                  
126900            MOVE FUNCTION TRIM(REQU-KDANMORS (IX))                        
127000                                          TO WS-KDANMORS-N                
127100            MOVE WS-KDANMORS-N            TO MID-KDANMORS (IX)            
127200         END-IF                                                           
127300                                                                          
127400         IF MID-KDANMORS (IX)            NOT = ALL '+'                    
127500            MOVE JA                       TO RADER-SW                     
127600            IF MID-KDANMORS (IX) NUMERIC                                  
127700****************************************************************          
127800***-- FOR API ONLY CODE 72,20,22,42,43,12,00,70 IS ALLOWED                
127900****************************************************************          
128000               IF API-SW ='Y'                                             
128100                  IF WS-KDANMORS-N = '72' OR '20' OR '22'                 
128200                          OR '42' OR '43' OR '12' OR '00' OR '70'         
128300                          OR '60' OR '13' OR '62' OR '63'                 
128400                          OR '82' OR '83' OR '73' OR '20' OR '23'         
128500                     CONTINUE                                             
128600                  ELSE                                                    
128700                      MOVE NEJ             TO ALLT-SW                     
128800                      MOVE NEJ             TO OK-SW                       
128900* ERROR CODE FOR API                                                      
129000                      MOVE BAD-REQUEST     TO RESP-IDMFSINF               
129100                      MOVE IX              TO W-LINENO                    
129200                      STRING 'DR CODE NOT ALLOWED ON LINE '               
129300                       W-LINENO                                           
129400                      DELIMITED BY SIZE INTO RESP-TEMFSINF                
129500                  END-IF                                                  
129600               END-IF                                                     
129700****************************************************************          
129800***-- BARA ADMINISTRATIONEN I GÖTEBORG FÅR LÄGGA KOD 40                   
129900****************************************************************          
130000                 IF MID-KDANMORS (IX) = '40'                              
130100                    IF MSG-SIGNON-USERID (1:2) = 'PC'                     
130200                       MOVE MFS-NUM-FAELT-RAETT                           
130300                                         TO MOD-KDANMORS-ATTR(IX)         
130400                    ELSE                                                  
130500                      MOVE NEJ           TO ALLT-SW                       
130600                      MOVE MFS-NUM-FAELT-FEL                              
130700                                         TO MOD-KDANMORS-ATTR(IX)         
130800                    END-IF                                                
130900                 ELSE                                                     
131000                    MOVE MFS-NUM-FAELT-RAETT                              
131100                                         TO MOD-KDANMORS-ATTR(IX)         
131200                 END-IF                                                   
131300****************************************************************          
131400                                                                          
131500*- LDC-SE/GB FÅR INTE LOV ATT BLANDA KOD 72 MED ANDRA KODER.              
131600*- GÄLLER ÄVEN SDC-23, SDC-21, SDC-25 (?? LASSI)                          
131700                                                                          
131800               IF GMT-FLLDCKND = JA OR                                    
131900                  GMT-FLRETUR  = JA                                       
132000                 IF MID-KDANMORS (IX) = '72'                              
132100                   IF EJ-KOD-72-LDC                                       
132200                     MOVE NEJ                   TO ALLT-SW                
132300                     MOVE NEJ                   TO OK-SW                  
132400                     MOVE MFS-NUM-FAELT-FEL     TO                        
132500                                         MOD-KDANMORS-ATTR(IX)            
132600* ERROR CODE FOR API                                                      
132700                      MOVE BAD-REQUEST          TO RESP-IDMFSINF          
132800                      MOVE IX                   TO W-LINENO               
132900                      STRING 'CANT MIX 72 WITH OTHER CODE ON LINE'        
133000                      W-LINENO                                            
133100                      DELIMITED BY SIZE INTO RESP-TEMFSINF                
133200                   END-IF                                                 
133300                 ELSE                                                     
133400                   IF KOD-72-LDC                                          
133500                     MOVE NEJ                   TO ALLT-SW                
133600                     MOVE NEJ                   TO OK-SW                  
133700                     MOVE MFS-NUM-FAELT-FEL     TO                        
133800                                         MOD-KDANMORS-ATTR(IX)            
133900                      MOVE BAD-REQUEST          TO RESP-IDMFSINF          
134000                      MOVE IX                   TO W-LINENO               
134100                      STRING 'CANT MIX 72 WITH OTHER'                     
134200                       'CODE ON LINE 'W-LINENO                            
134300                      DELIMITED BY SIZE INTO RESP-TEMFSINF                
134400                   END-IF                                                 
134500                 END-IF                                                   
134600               END-IF                                                     
134700                                                                          
134800*- 2007-04-04 E'TRACKER 850114                                            
134900*- N-FAKTUROR FÅR INTE LOV ATT BLANDA KOD 74 MED ANDRA KODER.             
135000                                                                          
135100                 IF MID-KDANMORS (IX) = '74'                              
135200                   IF KOD-74-SAKNAS                                       
135300                     MOVE NEJ                   TO ALLT-SW                
135400                     MOVE MFS-NUM-FAELT-FEL     TO                        
135500                                       MOD-KDANMORS-ATTR(IX)              
135600                   END-IF                                                 
135700                 ELSE                                                     
135800                   IF KOD-74-FINNS                                        
135900                     MOVE NEJ                   TO ALLT-SW                
136000                     MOVE MFS-NUM-FAELT-FEL     TO                        
136100                                       MOD-KDANMORS-ATTR(IX)              
136200                   END-IF                                                 
136300                 END-IF                                                   
136400                                                                          
136500*- PGA KOD 74 KRÄVER FAKTURA OCH ÄR FLAGGAD HÄMTA-PRIS-FAKT OCH           
136600*- FAKTURAN MÅSTE FINNAS SÅ ÄR DET EJ TILLÅTET ATT SÄTTA VALFRITT         
136700*- PRIS PÅ RADEN ENLIGT SUSSI/KARIN 070425.                               
136800                 IF MID-KDANMORS (IX) = '74'                              
136900                    IF MID-PRARTBTO (IX) NOT = ALL '+'                    
137000                       MOVE NEJ             TO ALLT-SW                    
137100                       MOVE MFS-NUM-FAELT-FEL                             
137200                                         TO MOD-KDANMORS-ATTR(IX)         
137300                       MOVE ERR-UPD-NOT-ALLOWED TO MED-IDMFSFEL           
137400                    END-IF                                                
137500                 END-IF                                                   
137600                                                                          
137700*- PGA KOD 61 INTE KRÄVER FAKTURA MEN ÄR FLAGGAD HÄMTA-PRIS-FAKT O        
137800*- FLAGGAD NEJ PÅ PRISSÄTTNING SÅ MÅSTE MAN GE PRIS HÄR. ÖNSKEMÅL         
137900*- FRÅN SUSSI/KARIN N ATT VARNINGSTEXT LÄGGS UT HÄR.                      
138000                 IF MID-KDANMORS (IX) = '61'                              
138100                    IF MID-PRARTBTO (IX)  = ALL '+'                       
138200                      MOVE NEJ             TO ALLT-SW                     
138300                      MOVE MFS-NUM-FAELT-FEL                              
138400                                          TO MOD-KDANMORS-ATTR(IX)        
138500                      MOVE ERR-PRICE-MISSING   TO MED-IDMFSFEL            
138600                    END-IF                                                
138700                 END-IF                                                   
138800*HUNGARY DISTRICT CAN PLACE DR ONLY FOR CODE 99.                          
138900                 IF API-SW ='N'                                           
139000                     IF WS-IDDISTR = '2364' AND                           
139100                       (MID-KDANMORS (IX) NOT = 99)                       
139200                        MOVE NEJ          TO ALLT-SW                      
139300                        MOVE MFS-NUM-FAELT-FEL                            
139400                                          TO MOD-KDANMORS-ATTR(IX)        
139500                        MOVE ERR-UPPDAT-EJ-TILLATEN TO                    
139600                                             MED-IDMFSFEL                 
139700                        CALL WMEDKONV USING MED-WMEDAREA                  
139800                        MOVE MED-MFSFEL   TO MOD-TEMFSFEL                 
139900                        PERFORM MFS-RENSA-FAELT-UT                        
140000                     END-IF                                               
140100                 END-IF                                                   
140200            ELSE                                                          
140300               MOVE NEJ                   TO ALLT-SW                      
140400               MOVE MFS-NUM-FAELT-FEL     TO MOD-KDANMORS-ATTR(IX)        
140500* ERROR CODE FOR API                                                      
140600               MOVE BAD-REQUEST           TO RESP-IDMFSINF                
140700               MOVE IX                    TO W-LINENO                     
140800               STRING 'WRONG DISCREPANCY CODE ON LINE 'W-LINENO           
140900                      DELIMITED BY SIZE INTO RESP-TEMFSINF                
141000            END-IF                                                        
141100         ELSE                                                             
141200            MOVE NEJ                      TO ALLT-SW                      
141300            MOVE NEJ                      TO OK-SW                        
141400            MOVE MFS-NUM-FAELT-FEL        TO MOD-KDANMORS-ATTR(IX)        
141500* ERROR CODE FOR API                                                      
141600            MOVE BAD-REQUEST              TO RESP-IDMFSINF                
141700            MOVE IX                       TO W-LINENO                     
141800            STRING 'WRONG DISCREPANCY CODE ON LINE ' W-LINENO             
141900                      DELIMITED BY SIZE INTO RESP-TEMFSINF                
142000         END-IF                                                           
142100* FIELDS NOT PRESENT IN API HENCE ONLY CHECK IN CLASSIC                   
142200                                                                          
142300     IF API-SW = 'N'                                                      
142400         IF MID-KDEMBLEV (IX)            NOT = ALL '+'                    
142500            MOVE JA                       TO RADER-SW                     
142600            IF MID-KDEMBLEV (IX) NUMERIC AND                              
142700               MID-KDEMBLEV (IX)  > ZERO                                  
142800               MOVE MFS-NUM-FAELT-RAETT   TO MOD-KDEMBLEV-ATTR(IX)        
142900            ELSE                                                          
143000               MOVE NEJ                   TO ALLT-SW                      
143100               MOVE MFS-NUM-FAELT-FEL     TO MOD-KDEMBLEV-ATTR(IX)        
143200            END-IF                                                        
143300         END-IF                                                           
143400                                                                          
143500         MOVE ZERO                         TO WS-PRARTBTO-JFR             
143600         IF MID-PRARTBTO (IX)            NOT = ALL '+'                    
143700            MOVE JA                        TO RADER-SW                    
143800            MOVE MID-PRARTBTO (IX)         TO DEC-IDFRIDATA               
143900            MOVE 7                         TO DEC-KVHELTAL                
144000            MOVE 2                         TO DEC-KVDECIMAL               
144100            CALL WDECEDIT USING WDECAREA                                  
144200            IF DEC-KDSVAR-OK                                              
144300               MOVE DEC-IDEDITDATA         TO WS-PRARTBTO-JFR             
144400               IF WS-PRARTBTO-JFR  > ZERO                                 
144500                  MOVE MFS-NUM-FAELT-RAETT TO                             
144600                                           MOD-PRARTBTO-ATTR(IX)          
144700               ELSE                                                       
144800                  MOVE NEJ                 TO ALLT-SW                     
144900                  MOVE MFS-NUM-FAELT-FEL   TO                             
145000                                          MOD-PRARTBTO-ATTR(IX)           
145100               END-IF                                                     
145200            ELSE                                                          
145300               MOVE NEJ                    TO ALLT-SW                     
145400               MOVE MFS-NUM-FAELT-FEL      TO                             
145500                                          MOD-PRARTBTO-ATTR(IX)           
145600            END-IF                                                        
145700         END-IF                                                           
145800                                                                          
145900         IF MID-KDFAKTYP (IX)            NOT = ALL '+'                    
146000            MOVE JA                       TO RADER-SW                     
146100            MOVE MID-KDFAKTYP (IX)        TO GODK-KDFAKTYP-SW             
146200            IF GODK-KDFAKTYP                                              
146300               MOVE MFS-ALFA-FAELT-RAETT  TO MOD-KDFAKTYP-ATTR(IX)        
146400            ELSE                                                          
146500               MOVE NEJ                   TO ALLT-SW                      
146600               MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDFAKTYP-ATTR(IX)        
146700            END-IF                                                        
146800         END-IF                                                           
146900     END-IF                                                               
147000                                                                          
147100     IF API-SW = 'Y'                                                      
147200            MOVE FUNCTION TRIM(REQU-IDFAKT(IX))                           
147300                                          TO WS-IDFAKT-N                  
147400            MOVE WS-IDFAKT-N              TO MID-IDFAKT(IX)               
147500     END-IF                                                               
147600         IF MID-IDFAKT (IX)              NOT = ALL '+'                    
147700            MOVE JA                       TO    RADER-SW                  
147800            IF MID-IDFAKT (IX) NUMERIC AND                                
147900               MID-IDFAKT (IX)    > ZERO                                  
148000               MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDFAKT-ATTR(IX)          
148100            ELSE                                                          
148200               MOVE NEJ                   TO ALLT-SW                      
148300               MOVE NEJ                   TO OK-SW                        
148400               MOVE MFS-NUM-FAELT-FEL     TO MOD-IDFAKT-ATTR(IX)          
148500* ERROR CODE FOR API                                                      
148600               MOVE BAD-REQUEST           TO RESP-IDMFSINF                
148700               MOVE IX                    TO W-LINENO                     
148800               STRING 'INVOICE NOT CORRECT ON LINE 'W-LINENO              
148900                      DELIMITED BY SIZE INTO RESP-TEMFSINF                
149000            END-IF                                                        
149100         END-IF                                                           
149200                                                                          
149300       IF API-SW = 'N'                                                    
149400         IF MID-IDDC (IX)                NOT = ALL '+'                    
149500            MOVE JA                       TO    RADER-SW                  
149600            MOVE MID-IDDC (IX)            TO WS-IDDC-INPUT                
149700            IF WS-IDDC-INPUT NOT = W-IDDC-B6                              
149800               MOVE WS-IDDC-INPUT         TO W-IDDC-B6                    
149900               PERFORM IMS-GU-WDB601                                      
150000            END-IF                                                        
150100            IF DCS-CDC OR DCS-SDC OR DCS-NDC-PF OR DCS-NDC-CN OR          
150200              (DCS-DDC AND                                                
150300              (DCS-SWEDEN  OR DCS-NORWAY OR DCS-FINLAND OR                
150400               DCS-BELGIUM OR DCS-FRANCE OR DCS-GERMANY OR                
150500               DCS-KOREA   OR DCS-TURKEY OR DCS-HUNGARY OR                
150600               DCS-MEXICO  OR DCS-BRASIL OR DCS-SOUTH-AFRICA OR           
150700               DCS-MALAYSIA OR DCS-THAILAND OR DCS-TAIWAN OR              
150800               DCS-POLAND  OR DCS-MAROCKO OR DCS-ENGLAND OR               
150900               DCS-AUSTRALIA))                                            
150900                                                                          
151000               MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDDC-ATTR(IX)            
151100            ELSE                                                          
151200               MOVE NEJ                   TO ALLT-SW                      
151300               MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDDC-ATTR(IX)            
151400            END-IF                                                        
151500            IF DCS-FTG-CN                                                 
151600              IF MID-KDANMORS (IX) = '99'                                 
151700                MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDDC-ATTR(IX)           
151800              ELSE                                                        
151900                MOVE NEJ                   TO ALLT-SW                     
152000                MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDDC-ATTR(IX)           
152100              END-IF                                                      
152200            END-IF                                                        
152300         ELSE                                                             
152400            MOVE NEJ                      TO ALLT-SW                      
152500            MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDDC-ATTR(IX)            
152600         END-IF                                                           
152700       END-IF                                                             
152800     END-IF                                                               
152900       ADD +1                             TO IX                           
153000     END-PERFORM                                                          
153100                                                                          
153200     IF MID-INFO-RAD (1) = ALL '+'  AND RADER-FINNS                       
153300       MOVE JA                            TO TOM-RAD-1-SW                 
153400       MOVE NEJ                           TO ALLT-SW                      
153500* ERROR CODE FOR API                                                      
153600       MOVE MFS-NUM-FAELT-FEL     TO MOD-IDORDNR5-ATTR(1)                 
153700       MOVE BAD-REQUEST           TO RESP-IDMFSINF                        
153800       MOVE 'DATA ON LINE 1 MISSING' TO RESP-TEMFSINF                     
153900                                                                          
154000     END-IF                                                               
154100                                                                          
154200     IF API-SW = 'N'                                                      
154300       IF MID-LEVANM-KLAR  NOT =  '+'                                     
154400         IF MID-LEVANM-KLAR = 'J' OR 'Y' OR ' ' OR 'N'                    
154500           MOVE MFS-ALFA-FAELT-RAETT      TO MOD-LEVANM-KLAR-ATTR         
154600         ELSE                                                             
154700           MOVE NEJ                       TO ALLT-SW                      
154800           MOVE MFS-ALFA-FAELT-FEL        TO MOD-LEVANM-KLAR-ATTR         
154900         END-IF                                                           
155000       END-IF                                                             
155100     END-IF                                                               
155200     .                                                                    
155300     EJECT                                                                
155400 CAB-KOLLA-TILLAEGGSKOST SECTION.                                         
155500                                                                          
155600     MOVE NEJ                             TO TILLAEGGSKOST-SW             
155700                                                                          
155800     PERFORM CABA-KOLLA-RELANDCO                                          
155900     PERFORM CABB-KOLLA-PRFRAKT                                           
156000     PERFORM CABC-KOLLA-PRFOERS                                           
156100     PERFORM CABD-KOLLA-PRLEGKST                                          
156200     .                                                                    
156300     EJECT                                                                
156400 CABA-KOLLA-RELANDCO SECTION.                                             
156500                                                                          
156600     MOVE JA                              TO BETALARE-SW                  
156700     MOVE NEJ                             TO OBEHORIG-SW                  
156800     IF MID-RELANDCO NOT = ALL '+'                                        
156900       IF DIST79-DEALER-PRICE                                             
157100         MOVE NEJ                         TO ALLT-SW                      
157200         MOVE JA                          TO TILLAEGGSKOST-SW             
157300         MOVE MFS-NUM-FAELT-FEL           TO MOD-RELANDCO-ATTR            
157400       ELSE                                                               
157500         IF MSG-SIGNON-USERID (1:2) = 'PC'                                
157600            MOVE JA                         TO TILLAEGGSKOST-SW           
157700            MOVE MID-RELANDCO               TO DEC-IDFRIDATA              
157800            MOVE 3                          TO DEC-KVHELTAL               
157900            MOVE 2                          TO DEC-KVDECIMAL              
158000            CALL WDECEDIT USING WDECAREA                                  
158100            IF DEC-KDSVAR-OK                                              
158200               MOVE DEC-IDEDITDATA          TO WS-RELANDCO-NUM            
158300               MOVE MFS-NUM-FAELT-RAETT     TO MOD-RELANDCO-ATTR          
158400            ELSE                                                          
158500               MOVE NEJ                     TO ALLT-SW                    
158600               MOVE MFS-NUM-FAELT-FEL       TO MOD-RELANDCO-ATTR          
158700            END-IF                                                        
158800         ELSE                                                             
158900             MOVE NEJ                       TO ALLT-SW                    
159000             MOVE JA                        TO OBEHORIG-SW                
159100             MOVE MFS-NUM-FAELT-FEL         TO MOD-RELANDCO-ATTR          
159200         END-IF                                                           
159300       END-IF                                                             
159400     ELSE                                                                 
159500       IF DIST79-DEALER-PRICE                                             
159700         MOVE ZERO                          TO WS-RELANDCO-NUM            
159800       ELSE                                                               
159900         MOVE WS-IDDISTR                    TO W-IDDISTR-WDB2-MIN         
160000                                               W-IDDISTR-WDB2-MAX         
160100         MOVE WS-IDKUNDNR                   TO W-IDKUNDNR-WDB2-MIN        
160200         PERFORM IMS-GET-WDB201                                           
160300                                                                          
160400         IF SEGMENT-FINNS                                                 
160500           MOVE GMT-IDPARTNR                TO W-WDB1-IDPARTNR            
160600           MOVE GMT-IDFTG                   TO W-WDB1-IDFTG               
160700           PERFORM IMS-GET-WDB101                                         
160800           IF SEGMENT-FINNS                                               
160900              MOVE BET-RELANDCO              TO WS-RELANDCO-NUM           
161000           ELSE                                                           
161100              MOVE ZERO                      TO WS-RELANDCO-NUM           
161200              MOVE NEJ                       TO BETALARE-SW               
161300           END-IF                                                         
161400         END-IF                                                           
161500       END-IF                                                             
161600     END-IF                                                               
161700     .                                                                    
161800     EJECT                                                                
161900 CABB-KOLLA-PRFRAKT SECTION.                                              
162000                                                                          
162100     IF MID-PRFRAKT NOT = ALL '+'                                         
162200       MOVE JA                             TO TILLAEGGSKOST-SW            
162300       IF DIST79-DEALER-PRICE                                             
162500         MOVE NEJ                          TO ALLT-SW                     
162600         MOVE MFS-NUM-FAELT-FEL            TO MOD-PRFRAKT-ATTR            
162700       ELSE                                                               
162800         MOVE MID-PRFRAKT                  TO DEC-IDFRIDATA               
162900         MOVE 7                            TO DEC-KVHELTAL                
163000         MOVE 2                            TO DEC-KVDECIMAL               
163100         CALL WDECEDIT USING WDECAREA                                     
163200         IF DEC-KDSVAR-OK                                                 
163300            MOVE DEC-IDEDITDATA            TO WS-PRFRAKT-NUM              
163400            IF WS-PRFRAKT-NUM       > ZERO                                
163500               MOVE MFS-NUM-FAELT-RAETT    TO MOD-PRFRAKT-ATTR            
163600            ELSE                                                          
163700               MOVE NEJ                    TO ALLT-SW                     
163800               MOVE MFS-NUM-FAELT-FEL      TO MOD-PRFRAKT-ATTR            
163900            END-IF                                                        
164000         ELSE                                                             
164100            MOVE NEJ                       TO ALLT-SW                     
164200            MOVE MFS-NUM-FAELT-FEL         TO MOD-PRFRAKT-ATTR            
164300         END-IF                                                           
164400       END-IF                                                             
164500     END-IF                                                               
164600     .                                                                    
164700     EJECT                                                                
164800 CABC-KOLLA-PRFOERS SECTION.                                              
164900                                                                          
165000     IF MID-PRFOERS NOT = ALL '+'                                         
165100       MOVE JA                             TO TILLAEGGSKOST-SW            
165200       IF DIST79-DEALER-PRICE                                             
165400         MOVE NEJ                          TO ALLT-SW                     
165500         MOVE MFS-NUM-FAELT-FEL            TO MOD-PRFOERS-ATTR            
165600       ELSE                                                               
165700         MOVE MID-PRFOERS                  TO DEC-IDFRIDATA               
165800         MOVE 7                            TO DEC-KVHELTAL                
165900         MOVE 2                            TO DEC-KVDECIMAL               
166000         CALL WDECEDIT USING WDECAREA                                     
166100         IF DEC-KDSVAR-OK                                                 
166200            MOVE DEC-IDEDITDATA            TO WS-PRFOERS-NUM              
166300            IF WS-PRFOERS-NUM       > ZERO                                
166400               MOVE MFS-NUM-FAELT-RAETT    TO MOD-PRFOERS-ATTR            
166500            ELSE                                                          
166600               MOVE NEJ                    TO ALLT-SW                     
166700               MOVE MFS-NUM-FAELT-FEL      TO MOD-PRFOERS-ATTR            
166800            END-IF                                                        
166900         ELSE                                                             
167000            MOVE NEJ                       TO ALLT-SW                     
167100            MOVE MFS-NUM-FAELT-FEL         TO MOD-PRFOERS-ATTR            
167200         END-IF                                                           
167300       END-IF                                                             
167400     END-IF                                                               
167500     .                                                                    
167600     EJECT                                                                
167700 CABD-KOLLA-PRLEGKST SECTION.                                             
167800                                                                          
167900     IF MID-PRLEGKST NOT = ALL '+'                                        
168000       MOVE JA                             TO TILLAEGGSKOST-SW            
168100       IF DIST79-DEALER-PRICE                                             
168300         MOVE NEJ                          TO ALLT-SW                     
168400         MOVE MFS-NUM-FAELT-FEL            TO MOD-PRLEGKST-ATTR           
168500       ELSE                                                               
168600         MOVE MID-PRLEGKST                 TO DEC-IDFRIDATA               
168700         MOVE 7                            TO DEC-KVHELTAL                
168800         MOVE 2                            TO DEC-KVDECIMAL               
168900         CALL WDECEDIT USING WDECAREA                                     
169000         IF DEC-KDSVAR-OK                                                 
169100            MOVE DEC-IDEDITDATA            TO WS-PRLEGKST-NUM             
169200            IF WS-PRLEGKST-NUM      > ZERO                                
169300               MOVE MFS-NUM-FAELT-RAETT    TO MOD-PRLEGKST-ATTR           
169400            ELSE                                                          
169500               MOVE NEJ                    TO ALLT-SW                     
169600               MOVE MFS-NUM-FAELT-FEL      TO MOD-PRLEGKST-ATTR           
169700            END-IF                                                        
169800         ELSE                                                             
169900            MOVE NEJ                       TO ALLT-SW                     
170000            MOVE MFS-NUM-FAELT-FEL         TO MOD-PRLEGKST-ATTR           
170100         END-IF                                                           
170200       END-IF                                                             
170300     END-IF                                                               
170400     .                                                                    
170500     EJECT                                                                
170600 CB-KOLLA-UPPLAGDA-RADER SECTION.                                         
170700                                                                          
170800*-KOLLA SÅ ATT TIDIGARE UPPLAGDA RADER STÄMMER MED NYA,DÄR KOD            
170900*-72 OCH 74 INTE FÅR BLANDAS MED ANDRA KODER.                             
171000                                                                          
171100       IF LEV-KDANMORS = '74'                                             
171200         IF KOD-74-SAKNAS                                                 
171300           MOVE ERR-UPD-NOT-ALLOWED   TO MED-IDMFSFEL                     
171400           CALL WMEDKONV USING MED-WMEDAREA                               
171500           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
171600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
171700           MOVE NEJ                   TO ALLT-SW                          
171800           MOVE BAD-REQUEST           TO RESP-IDMFSINF                    
171900           MOVE 'UPDATE NOT ALLOWED '  TO RESP-TEMFSINF                   
172000         END-IF                                                           
172100       ELSE                                                               
172200         IF KOD-74-FINNS                                                  
172300           MOVE ERR-UPD-NOT-ALLOWED   TO MED-IDMFSFEL                     
172400           CALL WMEDKONV USING MED-WMEDAREA                               
172500           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
172600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
172700           MOVE NEJ                   TO ALLT-SW                          
172800           MOVE BAD-REQUEST           TO RESP-IDMFSINF                    
172900           MOVE 'UPDATE NOT ALLOWED '  TO RESP-TEMFSINF                   
173000         END-IF                                                           
173100       END-IF                                                             
173200                                                                          
173300     IF GMT-FLLDCKND = JA OR                                              
173400        GMT-FLRETUR  = JA                                                 
173500       IF LEV-KDANMORS = '72'                                             
173600         IF EJ-KOD-72-LDC                                                 
173700           MOVE ERR-UPD-NOT-ALLOWED   TO MED-IDMFSFEL                     
173800           CALL WMEDKONV USING MED-WMEDAREA                               
173900           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
174000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
174100           MOVE NEJ                   TO ALLT-SW                          
174200           MOVE BAD-REQUEST           TO RESP-IDMFSINF                    
174300           MOVE 'UPDATE NOT ALLOWED '  TO RESP-TEMFSINF                   
174400                                                                          
174500         END-IF                                                           
174600       ELSE                                                               
174700         IF KOD-72-LDC                                                    
174800           MOVE ERR-UPD-NOT-ALLOWED   TO MED-IDMFSFEL                     
174900           CALL WMEDKONV USING MED-WMEDAREA                               
175000           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
175100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
175200           MOVE NEJ                   TO ALLT-SW                          
175300           MOVE BAD-REQUEST           TO RESP-IDMFSINF                    
175400           MOVE 'UPDATE NOT ALLOWED ' TO RESP-TEMFSINF                    
175500                                                                          
175600         END-IF                                                           
175700       END-IF                                                             
175800     END-IF                                                               
175900     .                                                                    
176000     EJECT                                                                
176100 D-KALLA-PA-KONTROLLPROGRAM SECTION.                                      
176200                                                                          
176300     MOVE +1                              TO IX                           
176400     MOVE NEJ                             TO FEL                          
176500                                             SPAR-FEL                     
176600                                                                          
176700     IF SUB-KDTRANS (1:6)='W4T704'                                        
176800        CONTINUE                                                          
176900     ELSE                                                                 
177000        MOVE REQU-KVRADER                 TO MAX-IX                       
177100     END-IF                                                               
177200     PERFORM UNTIL IX > MAX-IX OR SOMETHING-WRONG                         
177300        MOVE IX                           TO MOD-KVRADER                  
177400                                             SPAR-KVRADER                 
177500        IF MID-INFO-RAD (IX) = ALL '+'                                    
177600           CONTINUE                                                       
177700        ELSE                                                              
177800           MOVE NEJ                       TO MATRIX-SW                    
177900           MOVE NEJ                       TO MATRIX-Q                     
178000                                                                          
178100           PERFORM DA-FLYTTA-DATA                                         
178200                                                                          
178300           CALL W418KTL1 USING LINK-W418KTL1  KREE-PCB                    
178400                                              WDL5-PCB                    
178500                                              WDB2-PCB                    
178600                                              ARTC-PCB                    
178700                                              XXMI-PCB                    
178800                                             PARTC-PCB                    
178900                                             PWDK7-PCB                    
179000                                             PGMTA-PCB                    
179100                                              BETA-PCB                    
179200                                              PRIA-PCB                    
179300                                             GPRIB-PCB                    
179400                                              GMTB-PCB                    
179500                                              9305-PCB                    
179600                                              WDB1A-PCB                   
179700                                              4128-PCB                    
179800                                         COST-WDK6-PCB                    
179900                                         COST-WDK7-PCB                    
180000                                         COST-WDF1-PCB                    
180100                                         COST-9305-PCB                    
180200                                         COST-WDK72-PCB                   
180300                                         COST-WDB6-PCB                    
180400                                    PRIS-COST-WDK6-PCB                    
180500                                    PRIS-COST-WDK7-PCB                    
180600                                    PRIS-COST-WDF1-PCB                    
180700                                    PRIS-COST-9305-PCB                    
180800                                    PRIS-COST-WDK72-PCB                   
180900                                    PRIS-COST-WDB6-PCB                    
181000                                                                          
181100              PERFORM DE-KOLLA-RETUR-MATRIX                               
181200                                                                          
181300           IF LINK-KDSVAR = SPACE                                         
181400              IF BETALARE-SAKNAS OR MATRIX-FEL                            
181500                IF BETALARE-SAKNAS                                        
181600                   MOVE '799'               TO MOD-IDFELKOD-1 (IX)        
181700                   MOVE JA                  TO FEL                        
181800                   MOVE MFS-RENSA-FAELT     TO MOD-IDFELKOD-2 (IX)        
181900                                               MOD-IDFELKOD-3 (IX)        
182000                   PERFORM DC-KOLLA-OM-TVANGSGODK                         
182100                END-IF                                                    
182200                IF MATRIX-FEL                                             
182300                  IF MOD-IDFELKOD-1 (IX) = '799'                          
182400                    MOVE '712'              TO MOD-IDFELKOD-2 (IX)        
182500                    MOVE MFS-RENSA-FAELT    TO MOD-IDFELKOD-3 (IX)        
182600                  ELSE                                                    
182700                   MOVE '712'               TO MOD-IDFELKOD-1 (IX)        
182800                   MOVE MFS-RENSA-FAELT     TO MOD-IDFELKOD-2 (IX)        
182900                                               MOD-IDFELKOD-3 (IX)        
183000                  END-IF                                                  
183100                                                                          
183200                  MOVE JA                  TO FEL                         
183300                END-IF                                                    
183400              ELSE                                                        
183500                MOVE MFS-RENSA-FAELT        TO MOD-IDFELKOD-1 (IX)        
183600                                               MOD-IDFELKOD-2 (IX)        
183700                                               MOD-IDFELKOD-3 (IX)        
183800              END-IF                                                      
183900           ELSE                                                           
184000              IF API-SW = 'N'                                             
184100                PERFORM DB-KOLLA-FELKODER                                 
184200              END-IF                                                      
184300              IF MATRIX-FEL                                               
184400                CONTINUE                                                  
184500              ELSE                                                        
184600* TRUE ONLY IN CASE OF LYNK API                                           
184700                IF LINK-TEMFSINF NOT =  SPACE AND API-SW = 'Y'            
184800                  MOVE NEJ               TO OK-SW                         
184900                  MOVE IX                TO W-LINENO                      
185000                ELSE                                                      
185100                  IF API-SW = 'N'                                         
185200                    PERFORM DC-KOLLA-OM-TVANGSGODK                        
185300                  END-IF                                                  
185400                END-IF                                                    
185500              END-IF                                                      
185600           END-IF                                                         
185700           PERFORM DD-SPARA-DATA                                          
185800        END-IF                                                            
185900        ADD +1                            TO IX                           
186000     END-PERFORM                                                          
186100     .                                                                    
186200     EJECT                                                                
186300 DA-FLYTTA-DATA SECTION.                                                  
186400                                                                          
186500     MOVE 'N'                             TO LINK-FLDIRLEV                
186600     MOVE SPACE                           TO LINK-IDANALYS                
186700     MOVE +0                              TO LINK-IDKONTO                 
186800     MOVE SPACE                           TO LINK-IDKST                   
186900     IF MID-IDARTNR (IX) = ALL '+'                                        
187000        MOVE ZERO                         TO LINK-IDARTNR                 
187100     ELSE                                                                 
187200        MOVE MID-IDARTNR (IX)             TO LINK-IDARTNR                 
187300     END-IF                                                               
187400     IF API-SW = 'Y'                                                      
187500        MOVE SPACE                        TO LINK-IDDC                    
187600     ELSE                                                                 
187700        MOVE MID-IDDC       (IX)          TO LINK-IDDC                    
187800     END-IF                                                               
187900     MOVE WS-IDDISTR                      TO LINK-IDDISTR                 
188000     IF MID-IDFAKT (IX) NOT = ALL '+'                                     
188100        MOVE MID-IDFAKT  (IX)             TO LINK-IDFAKT                  
188200     ELSE                                                                 
188300        MOVE +0                           TO LINK-IDFAKT                  
188400     END-IF                                                               
188500                                                                          
188600     MOVE +1                              TO L-IX                         
188700     PERFORM 50 TIMES                                                     
188800        MOVE SPACE                        TO LINK-IDFELKOD  (L-IX)        
188900        ADD +1                            TO L-IX                         
189000     END-PERFORM                                                          
189100                                                                          
189200     IF MID-KDANMORS (IX) = '99'                                          
189300       MOVE MID-IDDC (IX)                 TO WS-IDDC-INPUT                
189400       IF WS-IDDC-INPUT NOT = W-IDDC-B6                                   
189500          MOVE WS-IDDC-INPUT              TO W-IDDC-B6                    
189600          PERFORM IMS-GU-WDB601                                           
189700       END-IF                                                             
189800       EVALUATE TRUE                                                      
189900       WHEN DCS-FTG-CN                                                    
190000           MOVE WC-IDFTG-CN               TO LINK-IDFTG                   
190100       WHEN DCS-FTG-IN                                                    
190200           MOVE WC-IDFTG-IN               TO LINK-IDFTG                   
190300       WHEN DCS-FTG-KR                                                    
190400           MOVE WC-IDFTG-KR               TO LINK-IDFTG                   
190500       WHEN DCS-FTG-MY                                                    
190600           MOVE WC-IDFTG-MY               TO LINK-IDFTG                   
190700       WHEN DCS-FTG-TH                                                    
190800           MOVE WC-IDFTG-TH               TO LINK-IDFTG                   
190900       WHEN DCS-FTG-TW                                                    
191000           MOVE WC-IDFTG-TW               TO LINK-IDFTG                   
191010       WHEN DCS-FTG-ZA                                                    
191020           MOVE WC-IDFTG-ZA               TO LINK-IDFTG                   
191100       WHEN OTHER                                                         
191200           MOVE WC-IDFTG-PV               TO LINK-IDFTG                   
191300       END-EVALUATE                                                       
191400     ELSE                                                                 
191500       MOVE WC-IDFTG-PV                   TO LINK-IDFTG                   
191600     END-IF                                                               
191700                                                                          
191800     IF MID-IDKOLLI (IX) = ALL '+'                                        
191900        MOVE ZERO                         TO LINK-IDKOLLI                 
192000     ELSE                                                                 
192100        MOVE MID-IDKOLLI  (IX)            TO LINK-IDKOLLI                 
192200     END-IF                                                               
192300     MOVE WS-IDKUNDNR                     TO LINK-IDKUNDNR                
192400     IF MID-IDORDNR5 (IX) = ALL '+'                                       
192500        MOVE ZERO                         TO LINK-IDORDNR5                
192600     ELSE                                                                 
192700        MOVE MID-IDORDNR5 (IX)            TO LINK-IDORDNR5                
192800     END-IF                                                               
192900     MOVE 'STA'                           TO LINK-IDPTYP                  
193000     MOVE WS-IDRAPPNR                     TO LINK-IDRAPPNR                
193100     MOVE SPACE                           TO LINK-IDUSER-PACK             
193200     MOVE MID-KDANMORS    (IX)            TO LINK-KDANMORS                
193300     IF MID-KDEMBLEV (IX) NOT =  '+'                                      
193400        MOVE MID-KDEMBLEV(IX)             TO LINK-KDEMBLEV                
193500     ELSE                                                                 
193600        MOVE +0                           TO LINK-KDEMBLEV                
193700     END-IF                                                               
193800     IF API-SW = 'Y' AND MID-KDANMORS (IX) = ('42' OR '43')               
193900        MOVE '2'                          TO LINK-KDEMBLEV                
194000     END-IF                                                               
194100     IF MID-KDFAKTYP (IX) = ALL '+'                                       
194200          MOVE SPACE                      TO LINK-KDFAKTYP                
194300     ELSE                                                                 
194400        MOVE MID-KDFAKTYP (IX)            TO LINK-KDFAKTYP                
194500     END-IF                                                               
194600     IF API-SW = 'Y'                                                      
194700        MOVE 'R'                          TO LINK-KDFAKTYP                
194800     END-IF                                                               
194900     MOVE +0                              TO LINK-KDFRAKT                 
195000     MOVE 'R  '                           TO LINK-KDKREBEH                
195100     MOVE SPACE                           TO LINK-KDSVAR                  
195200     IF MID-KVLEVANM (IX) = ALL '+'                                       
195300        MOVE ZERO                         TO LINK-KVLEVANM                
195400     ELSE                                                                 
195500        MOVE MID-KVLEVANM (IX)            TO LINK-KVLEVANM                
195600     END-IF                                                               
195700     IF MID-PRARTBTO (IX) NOT = ALL '+' AND API-SW = 'N'                  
195800        MOVE MID-PRARTBTO (IX)            TO DEC-IDFRIDATA                
195900        MOVE 7                            TO DEC-KVHELTAL                 
196000        MOVE 2                            TO DEC-KVDECIMAL                
196100        CALL WDECEDIT USING WDECAREA                                      
196200        IF DEC-KDSVAR-OK                                                  
196300          IF DIST79-DEALER-PRICE OR                                       
196500             DIST79-ECOM-PRICE                                            
196600            MOVE DEC-IDEDITDATA            TO LINK-PRARTBTO-LOC           
196700            MOVE ZERO                      TO LINK-PRARTBTO               
196800          ELSE                                                            
196900            MOVE DEC-IDEDITDATA            TO LINK-PRARTBTO               
197000            MOVE ZERO                      TO LINK-PRARTBTO-LOC           
197100          END-IF                                                          
197200        END-IF                                                            
197300     ELSE                                                                 
197400        MOVE +0                           TO LINK-PRARTBTO                
197500        MOVE +0                           TO LINK-PRARTBTO-LOC            
197600     END-IF                                                               
197700     MOVE +0                              TO LINK-TIFAKT                  
197800     MOVE DAGENS-DATUM                    TO LINK-TILEVANM                
197900     MOVE SPACE                           TO LINK-KDVAT                   
198000                                                                          
198100*-- FIX FÖR ATT KLARA LEV.ANM. SOM KOMMER IN EFTER INSTALLATION           
198200*-- AV EN DEALER-NET/DDI-MARKNAD PÅ 'GAMLA' FAKTUROR.                     
198300*-- FIX BÖRJAR. 2003-01-14                                                
198400     IF DIST79-DEALER-PRICE                                               
198600       MOVE WS-KDVALISO                   TO LINK-KDVALISO                
198700     ELSE                                                                 
198800       MOVE SPACE                         TO LINK-KDVALISO                
198900     END-IF                                                               
199000                                                                          
199100     MOVE SPACE                           TO LINK-BEART-VIPS              
199200     MOVE 'N'                             TO LINK-FLPRQUES                
199300     MOVE +0                              TO LINK-PRARTSTD                
199400     MOVE +0                              TO LINK-PRARTSJK                
199500     .                                                                    
199600     EJECT                                                                
199700 DB-KOLLA-FELKODER SECTION.                                               
199800                                                                          
199900     MOVE +1                              TO F-IX                         
200000     MOVE JA                              TO FEL                          
200100     PERFORM UNTIL F-IX > MAX-F-IX                                        
200200        IF LINK-IDFELKOD(F-IX) = SPACE                                    
200300           IF BETALARE-SAKNAS                                             
200400              IF F-IX = +2                                                
200500                 MOVE '799'               TO MOD-IDFELKOD-2 (IX)          
200600              ELSE                                                        
200700                 MOVE '799'               TO MOD-IDFELKOD-3 (IX)          
200800              END-IF                                                      
200900           END-IF                                                         
201000           IF MATRIX-FEL                                                  
201100             MOVE '712'                   TO MOD-IDFELKOD-3 (IX)          
201200           END-IF                                                         
201300           MOVE +4                        TO F-IX                         
201400        ELSE                                                              
201500           EVALUATE F-IX                                                  
201600              WHEN +1                                                     
201700                MOVE LINK-IDFELKOD(F-IX) TO MOD-IDFELKOD-1    (IX)        
201800              WHEN +2                                                     
201900                MOVE LINK-IDFELKOD(F-IX) TO MOD-IDFELKOD-2    (IX)        
202000              WHEN +3                                                     
202100                MOVE LINK-IDFELKOD(F-IX) TO MOD-IDFELKOD-3    (IX)        
202200              WHEN OTHER                                                  
202300                CONTINUE                                                  
202400           END-EVALUATE                                                   
202500        END-IF                                                            
202600        ADD +1                            TO F-IX                         
202700     END-PERFORM                                                          
202800     .                                                                    
202900     EJECT                                                                
203000 DF-KOLLA-API-FELKODER SECTION.                                           
203100* SEND RESPONSE TO API IF THER IS ANY ERROR                               
203200     MOVE NEJ               TO OK-SW                                      
203300     MOVE BAD-REQUEST       TO RESP-IDMFSINF                              
203400     STRING LINK-TEMFSINF  DELIMITED BY '  '                              
203500            ' ON LINE  '      DELIMITED BY SIZE                           
203600             W-LINENO      DELIMITED BY SIZE                              
203700     INTO  RESP-TEMFSINF                                                  
203800     .                                                                    
203900     EJECT                                                                
204000 DC-KOLLA-OM-TVANGSGODK SECTION.                                          
204100                                                                          
204200     IF NYCKLAR-GAMLA AND MFS-UPDATE                                      
204300        MOVE +1                        TO LINK-IX                         
204400        MOVE NEJ                       TO FEL                             
204500        PERFORM UNTIL LINK-IX > MAX-LINK-IX OR FEL = JA                   
204600           EVALUATE LINK-IDFELKOD(LINK-IX)                                
204700              WHEN '   '                                                  
204800                CONTINUE                                                  
204900              WHEN '730'                                                  
205000*- LINJEN GLÖMMER OFTA GE PRIS VID PF23 - GODKÄNNER SEDAN KREDIT-         
205100*- ERING MED 0 I PRIS - STOPPAS I BILL-IT OCH SAP.                        
205200                IF LINK-KDANMORS = '00' OR '20' OR '42' OR '43' OR        
205300                                   '60' OR '62' OR '63' OR '70' OR        
205400                                   '72' OR '73' OR '75' OR '82' OR        
205500                                   '83' OR '11' OR '25'                   
205600                  IF LINK-PRARTBTO = ZERO AND                             
205700                     LINK-PRARTBTO-LOC = ZERO                             
205800                                                                          
205900                    MOVE ERR-PRICE-MISSING  TO MED-IDMFSFEL               
206000                    CALL WMEDKONV USING MED-WMEDAREA                      
206100                    MOVE MED-MFSFEL         TO MOD-TEMFSFEL               
206200                    MOVE JA                 TO FEL                        
206300                                               SPAR-FEL                   
206400                  ELSE                                                    
206500                    CONTINUE                                              
206600                  END-IF                                                  
206700                ELSE                                                      
206800                  IF LINK-KDANMORS = '74'                                 
206900                    MOVE ERR-INVOICE-MISSING  TO MED-IDMFSFEL             
207000                    CALL WMEDKONV USING MED-WMEDAREA                      
207100                    MOVE MED-MFSFEL         TO MOD-TEMFSFEL               
207200                    MOVE JA                 TO FEL                        
207300                                               SPAR-FEL                   
207400                  ELSE                                                    
207500                    CONTINUE                                              
207600                  END-IF                                                  
207700                END-IF                                                    
207800              WHEN '733'                                                  
207900                CONTINUE                                                  
208000              WHEN '734'                                                  
208100                CONTINUE                                                  
208200              WHEN '773'                                                  
208300                CONTINUE                                                  
208400              WHEN '799'                                                  
208500                CONTINUE                                                  
208600              WHEN OTHER                                                  
208700                MOVE JA                TO FEL                             
208800                                          SPAR-FEL                        
208900           END-EVALUATE                                                   
209000           ADD +1                      TO LINK-IX                         
209100       END-PERFORM                                                        
209200     END-IF                                                               
209300     .                                                                    
209400     EJECT                                                                
209500 DD-SPARA-DATA SECTION.                                                   
209600                                                                          
209700     MOVE LINK-FLDIRLEV                TO SPAR-FLDIRLEV  (IX)             
209800     MOVE LINK-IDANALYS                TO HELP-IDANALYS                   
209900     MOVE HELP-IDANALYS                TO SPAR-IDANALYS  (IX)             
210000     MOVE LINK-IDKONTO                 TO HELP-IDKONTO                    
210100     MOVE HELP-IDKONTO                 TO SPAR-IDKONTO   (IX)             
210200     MOVE LINK-IDKST                   TO SPAR-IDKST     (IX)             
210300     MOVE LINK-IDARTNR                 TO HELP-IDARTNR                    
210400     MOVE HELP-IDARTNR                 TO SPAR-IDARTNR   (IX)             
210500     MOVE LINK-IDDC                    TO SPAR-IDDC      (IX)             
210600     MOVE LINK-IDFAKT                  TO SPAR-IDFAKT    (IX)             
210700*SO? MOVE 57                           TO SPAR-IDFTG     (IX)             
210800     MOVE LINK-IDFTG                   TO SPAR-IDFTG     (IX)             
210900     MOVE LINK-IDKOLLI                 TO SPAR-IDKOLLI   (IX)             
211000     MOVE SPACE                        TO SPAR-IDKUNDRF  (IX)             
211100     MOVE LINK-IDORDNR5                TO SPAR-IDORDNR7  (IX)             
211200     MOVE ZERO                         TO SPAR-IDRADNR   (IX)             
211300     MOVE LINK-KDANMORS                TO SPAR-KDANMORS  (IX)             
211400     MOVE LINK-KDEMBLEV                TO SPAR-KDEMBLEV  (IX)             
211500     MOVE LINK-KDFAKTYP                TO SPAR-KDFAKTYP  (IX)             
211600     MOVE LINK-KDFRAKT                 TO HELP-KDFRAKT                    
211700     MOVE HELP-KDFRAKT                 TO SPAR-KDFRAKT   (IX)             
211800     MOVE LINK-KDKREBEH                TO SPAR-KDKREBEH  (IX)             
211900     MOVE LINK-KVLEVANM                TO HELP-KVLEVANM                   
212000     MOVE HELP-KVLEVANM                TO SPAR-KVLEVANM  (IX)             
212100*    MOVE ZERO                         TO SPAR-KVRADER                    
212200     ADD +1                            TO WS-KVRADER                      
212300     MOVE LINK-PRARTBTO                TO WS-PRARTBTO-NUM                 
212400     MOVE WS-PRARTBTO-ALFA             TO SPAR-PRARTBTO  (IX)             
212500     MOVE LINK-PRARTBTO-LOC            TO WS-PRARTBTO-LOC-NUM             
212600     MOVE WS-PRARTBTO-LOC-ALFA         TO SPAR-PRARTBTO-LOC (IX)          
212700     MOVE LINK-TIFAKT                  TO HELP-TIFAKT                     
212800     MOVE HELP-TIFAKT                  TO SPAR-TIFAKT    (IX)             
212900     MOVE DAGENS-DATUM                 TO                                 
213000                                       SPAR-TILEVANM-RAD (IX)             
213100     MOVE LINK-IDUSER-PACK             TO SPAR-IDUSER-PACK (IX)           
213200     MOVE LINK-KDORDKL                 TO SPAR-KDORDKL   (IX)             
213300     MOVE LINK-KDVAT                   TO SPAR-KDVAT     (IX)             
213400     MOVE LINK-KDVALISO                TO SPAR-KDVALISO  (IX)             
213500     MOVE LINK-BEART-VIPS              TO SPAR-BEART-VIPS (IX)            
213600     MOVE LINK-FLPRQUES                TO SPAR-FLPRQUES  (IX)             
213700     MOVE LINK-PRARTSTD                TO WS-PRARTSTD-NUM                 
213800     MOVE WS-PRARTSTD-ALFA             TO SPAR-PRARTSTD  (IX)             
213900     MOVE LINK-PRARTSJK                TO WS-PRARTSJK-NUM                 
214000     MOVE WS-PRARTSJK-ALFA             TO SPAR-PRARTSJK  (IX)             
214100                                                                          
214200     .                                                                    
214300     EJECT                                                                
214400 DE-KOLLA-RETUR-MATRIX  SECTION.                                          
214500                                                                          
214600*-- TILLAGG FOR KONTROLL MOT RETUR-MATRIXEN W418KTL3                      
214700                                                                          
214800     IF LINK-IDARTNR = DUMMY-IDARTNR                                      
214900       CONTINUE                                                           
215000     ELSE                                                                 
215100       MOVE LINK-KDANMORS        TO OKOD-KDANMORS                         
215200       CALL W418OKOD USING OKOD-W418OKOD                                  
215300* FOR ECOM VOUI AND CLASIC WE SHOULD CALL 4751/4752                       
215400* FOR NOW POLESTAR IS NOT USING IT HENCE ADDED A CONDITION                
215500* FOR NOT LYNK SO IT WILL PROCESS CLASSIC AND ECOM/VOUI                   
215600       IF  OKOD-FL-RETILL = 'J'                                           
215800                                                                          
215900         MOVE IDPGM            TO KTL3-IDPGM                              
216000         MOVE LINK-IDARTNR     TO KTL3-IDARTNR                            
216100         MOVE LINK-KDANMORS    TO KTL3-KDANMORS                           
216200         MOVE LINK-IDDC        TO KTL3-IDDC                               
216300         MOVE LINK-IDDISTR     TO KTL3-IDDISTR                            
216400         MOVE LINK-IDKUNDNR    TO KTL3-IDKUNDNR                           
216500         CALL W418KTL3 USING KTL3-W418KTL3  KTL3-WDA8-PCB                 
216600                                            KTL3-WDB2-PCB                 
216700                                            KTL3-WDK6-PCB                 
216800                                            KTL3-WDK7-PCB                 
216900                                            KTL3-WDB6-PCB                 
217000                                            KTL3-1165-PCB                 
217100                                                                          
217200         IF KTL3-KDSVAR = YES                                             
217300           IF KTL3-KDRETBEH = 'S'                                         
217400             MOVE JA               TO FEL                                 
217500             MOVE JA               TO MATRIX-SW                           
217600             MOVE BAD-REQUEST      TO RESP-IDMFSINF                       
217700             STRING 'STOPPED FOR RETURN '                                 
217800             DELIMITED BY SIZE INTO LINK-TEMFSINF                         
217900           END-IF                                                         
218000           IF KTL3-KDRETBEH = 'Q'                                         
218100             MOVE JA               TO MATRIX-Q                            
218200             MOVE 'Q  '            TO LINK-KDKREBEH                       
218300             MOVE NEJ              TO LINK-FLAUTKRE                       
218400           END-IF                                                         
218500           IF KTL3-KDRETBEH = 'P'                                         
218600             MOVE JA               TO MATRIX-Q                            
218700             MOVE 'P  '            TO LINK-KDKREBEH                       
218800             MOVE NEJ              TO LINK-FLAUTKRE                       
218900           END-IF                                                         
219000         END-IF                                                           
219100       END-IF                                                             
219200     END-IF                                                               
219300     .                                                                    
219400     EJECT                                                                
219500 E-UPPDATERA-DISPATCHEN SECTION.                                          
219600                                                                          
219700     PERFORM EA-SKAPA-BUNTHUVUD                                           
219800     PERFORM EB-SKAPA-MID-TILL-DISPATCH                                   
219900     .                                                                    
220000     EJECT                                                                
220100 EA-SKAPA-BUNTHUVUD SECTION.                                              
220200                                                                          
220300     MOVE +54                             TO MSG-KOM-KVLL                 
220400     MOVE LOW-VALUE                       TO MSG-KOM-KDZ1                 
220500     MOVE LOW-VALUE                       TO MSG-KOM-KDZ2                 
220600     MOVE SPACE                           TO MSG-KOM-KDTRANS              
220700     MOVE 'W4I79101'                      TO MSG-KOM-IDCPYTXT             
220800     MOVE 'KREDIT  '                      TO MSG-KOM-IDSNDNOD             
220900     MOVE 'W4070400'                      TO MSG-KOM-IDSNDJOB             
221000     MOVE DAGENS-DATUM                    TO MSG-KOM-TIREGDAT             
221100     MOVE DAGENS-TID                      TO MSG-KOM-TIKLOCK              
221200     MOVE SPACE                           TO MSG-KOM-IDMFSMED             
221300                                             MSG-KOM-KDSVAR               
221400     .                                                                    
221500     EJECT                                                                
221600 EB-SKAPA-MID-TILL-DISPATCH SECTION.                                      
221700                                                                          
221800     MOVE JA                           TO STATUS-3-SW                     
221900     MOVE MFS-KDMFSFOR                 TO 4791-SPRAK                      
222000     IF (4791-MID-IDSYSTEM = 'LYNK' OR 'ECOM' OR 'VOUI' OR 'TAD '         
222100                                    OR 'ACC ' OR 'APA'  OR 'APB'          
222200                                    OR 'APC ' OR 'APD'  OR 'APE'          
222300                                    OR 'APF ' OR 'APG'  OR 'APH'          
222400                                    OR 'API ' OR 'APJ' )                  
222500       CONTINUE                                                           
222600     ELSE                                                                 
222700       MOVE 'STA '                     TO 4791-MID-IDSYSTEM               
222800     END-IF                                                               
222900     MOVE WS-IDDISTR                   TO 4791-MID-IDDISTR                
223000     MOVE WS-IDKUNDNR                  TO 4791-MID-IDKUNDNR               
223100     MOVE WS-IDRAPPNR                  TO 4791-MID-IDRAPPNR               
223200* NO CHECK FOR API BECAUSE ALL ARE AUTO AND NO RETURN MATRIX              
223300     IF MID-LEVANM-KLAR  = 'J' OR 'Y' AND API-SW = 'N'                    
223400        MOVE +1                        TO IX                              
223500        PERFORM UNTIL IX > MAX-IX                                         
223600           MOVE IX                     TO MOD-KVRADER                     
223700           MOVE   SPAR-IDDC(IX)        TO WS-IDDC                         
223800           IF MID-KDANMORS (IX)  = ALL '+' OR                             
223900              SPAR-KDANMORS (IX) = '72'  OR '52' OR '53'  OR              
224000              GOOD-DDC                   OR                               
224100            ((SPAR-KDANMORS (IX) = '70'  OR                               
224200                                   '92'  OR '20' OR '21' ) AND            
224300             (SPAR-IDFTG(IX)     = '57'  AND                              
224400              SPAR-IDDC(IX) NOT  = '61'  AND '6A' AND '62'))              
224500             IF SPAR-KDKREBEH (IX) = 'Q  ' OR 'P  '                       
224600               MOVE NEJ                TO STATUS-3-SW                     
224700             END-IF                                                       
224800           ELSE                                                           
224900              MOVE NEJ                 TO STATUS-3-SW                     
225000           END-IF                                                         
225100           ADD +1                      TO IX                              
225200        END-PERFORM                                                       
225300        IF STATUS-3                                                       
225400           MOVE '3'                    TO 4791-MID-KDLEVANM               
225500        ELSE                                                              
225600           MOVE '1'                    TO 4791-MID-KDLEVANM               
225700        END-IF                                                            
225800     ELSE                                                                 
225900       MOVE '0'                        TO 4791-MID-KDLEVANM               
226000     END-IF                                                               
226100     IF 4791-MID-IDSYSTEM =    'ECOM' OR 'VOUI' OR 'LYNK' OR 'ACC'        
226200                            OR 'APA'  OR 'APB'  OR 'APC'  OR 'APD'        
226300                            OR 'APE'  OR 'APF'  OR 'APG'  OR 'APH'        
226400                            OR 'API'  OR 'APJ'                            
226500        MOVE  JA                       TO MID-LEVANM-KLAR                 
226600*IF ECOM/VOUI AND IT IS Q OR P ON 4572 THEN WE MOVE STATUS 1              
226700       IF MATRIX-Q =  JA                                                  
226800           MOVE '1'                    TO 4791-MID-KDLEVANM               
226900       ELSE                                                               
227000          MOVE  '3'                    TO 4791-MID-KDLEVANM               
227100       END-IF                                                             
227200     END-IF                                                               
227300     MOVE MID-LEVANM-KLAR              TO 4791-MID-LEVANM-KLAR            
227400     IF MID-PRFOERS NOT = ALL '+'                                         
227500        MOVE WS-PRFOERS                TO 4791-MID-PRFOERS                
227600     ELSE                                                                 
227700        MOVE MID-PRFOERS               TO 4791-MID-PRFOERS                
227800     END-IF                                                               
227900                                                                          
228000     IF MID-PRFRAKT NOT = ALL '+'                                         
228100        MOVE WS-PRFRAKT                TO 4791-MID-PRFRAKT                
228200     ELSE                                                                 
228300        MOVE MID-PRFRAKT               TO 4791-MID-PRFRAKT                
228400     END-IF                                                               
228500                                                                          
228600     MOVE WS-RELANDCO                  TO 4791-MID-RELANDCO               
228700                                                                          
228800     IF MID-PRLEGKST NOT = ALL '+'                                        
228900        MOVE WS-PRLEGKST               TO 4791-MID-PRLEGKST               
229000     ELSE                                                                 
229100        MOVE MID-PRLEGKST              TO 4791-MID-PRLEGKST               
229200     END-IF                                                               
229300                                                                          
229400     MOVE ZERO                         TO 4791-MID-REEMBHNT               
229500                                                                          
229600     MOVE DAGENS-DATUM                 TO 4791-MID-TILEVANM               
229700                                                                          
229800     PERFORM EBA-FLYTTA-SPAR-DATA-RAD                                     
229900     .                                                                    
230000     EJECT                                                                
230100 EBA-FLYTTA-SPAR-DATA-RAD SECTION.                                        
230200                                                                          
230300     MOVE +1                           TO IX1                             
230400                                          IX2                             
230500     PERFORM UNTIL IX1 > WS-KVRADER                                       
230600                                                                          
230700        IF IX2 > 4                                                        
230800           SUBTRACT 1                FROM IX2                             
230900           MOVE IX2                    TO 4791-MID-KVRADER                
231000           PERFORM EC-UPPDATERA-DISPATCHEN                                
231100           MOVE +1                     TO IX2                             
231200           PERFORM 4 TIMES                                                
231300              MOVE SPACE               TO 4791-MID-INFO-RAD(IX2)          
231400              ADD +1                   TO IX2                             
231500           END-PERFORM                                                    
231600           MOVE +1                     TO IX2                             
231700        END-IF                                                            
231800                                                                          
231900        MOVE SPAR-FLDIRLEV     (IX1) TO 4791-MID-FLDIRLEV    (IX2)        
232000        MOVE SPAR-IDANALYS     (IX1) TO 4791-MID-IDANALYS    (IX2)        
232100        MOVE SPAR-IDKONTO      (IX1) TO 4791-MID-IDKONTO     (IX2)        
232200        MOVE SPAR-IDKST        (IX1) TO 4791-MID-IDKST       (IX2)        
232300        MOVE SPAR-IDARTNR      (IX1) TO 4791-MID-IDARTNR     (IX2)        
232400        MOVE SPAR-IDDC         (IX1) TO 4791-MID-IDDC        (IX2)        
232500        MOVE SPAR-IDFAKT       (IX1) TO 4791-MID-IDFAKT      (IX2)        
232600        MOVE ZERO                    TO 4791-MID-IDFAKT-LOC  (IX2)        
232700        MOVE SPAR-TIFAKT       (IX1) TO 4791-MID-TIFAKT      (IX2)        
232800        MOVE ZERO                    TO 4791-MID-TIFAKT-LOC  (IX2)        
232900        MOVE SPAR-IDFTG        (IX1) TO 4791-MID-IDFTG       (IX2)        
233000        MOVE SPAR-IDKOLLI      (IX1) TO 4791-MID-IDKOLLI     (IX2)        
233100        MOVE SPAR-IDKUNDRF     (IX1) TO 4791-MID-IDKUNDRF    (IX2)        
233200        MOVE SPAR-IDRADNR      (IX1) TO 4791-MID-IDRADNR     (IX2)        
233300        MOVE SPAR-KDANMORS     (IX1) TO 4791-MID-KDANMORS    (IX2)        
233400        MOVE SPAR-KDEMBLEV     (IX1) TO 4791-MID-KDEMBLEV    (IX2)        
233500        MOVE SPAR-KDFAKTYP     (IX1) TO 4791-MID-KDFAKTYP    (IX2)        
233600        MOVE SPAR-KDFRAKT      (IX1) TO 4791-MID-KDFRAKT     (IX2)        
233700        MOVE SPAR-KDKREBEH     (IX1) TO 4791-MID-KDKREBEH    (IX2)        
233800        MOVE SPAR-KVLEVANM     (IX1) TO 4791-MID-KVLEVANM    (IX2)        
233900        MOVE SPAR-PRARTBTO     (IX1) TO 4791-MID-PRARTBTO    (IX2)        
234000        MOVE SPAR-PRARTBTO-LOC (IX1) TO 4791-MID-PRARTBTO-LOC(IX2)        
234100        MOVE ZERO                TO 4791-MID-PRARTBTO-LOCINV (IX2)        
234200        MOVE DAGENS-DATUM            TO 4791-MID-TILEVANM-RAD(IX2)        
234300        MOVE SPAR-IDUSER-PACK  (IX1) TO 4791-MID-IDUSER-PACK (IX2)        
234400        MOVE SPAR-KDORDKL      (IX1) TO 4791-MID-KDORDKL     (IX2)        
234500        MOVE NEJ                     TO 4791-MID-FLANLYSF    (IX2)        
234600                                                                          
234700                                                                          
234800        IF API-SW = 'N'                                                   
234900           IF SPAR-KDKREBEH   (IX1)  =  'Q  ' OR 'P  '                    
235000             CONTINUE                                                     
235100           ELSE                                                           
235200              MOVE SPAR-IDDC(IX1)         TO WS-IDDC                      
235300              IF  SPAR-KDANMORS (IX1) = '72'  OR '52' OR '53' OR          
235400                   GOOD-DDC                   OR                          
235500                 ((SPAR-KDANMORS (IX1) = '70' OR                          
235600                                         '92' OR '20' OR '21') AND        
235700                 ( SPAR-IDFTG(IX1)     = '57' AND                         
235800                   SPAR-IDDC(IX1) NOT  = '61' AND '6A' AND '62'))         
235900                   MOVE JA         TO 4791-MID-FLAUTKRE    (IX2)          
236000                   MOVE 'J'        TO 4791-MID-KDKREBEH    (IX2)          
236100              ELSE                                                        
236200                   MOVE NEJ                                               
236300                                   TO 4791-MID-FLAUTKRE    (IX2)          
236400              END-IF                                                      
236500           END-IF                                                         
236600        END-IF                                                            
236700                                                                          
236800*API IS AUTO APPROVE BUT IF Q AND P THEN SHOULD BE STOPPED                
236900*POLESTAR IS NOT USING DISCP HENCE NO CHANGES FOR IT                      
237000     IF 4791-MID-IDSYSTEM =    'ECOM' OR 'VOUI' OR 'LYNK' OR 'ACC'        
237100                            OR 'APA'  OR 'APB'  OR 'APC'  OR 'APD'        
237200                            OR 'APE'  OR 'APF'  OR 'APG'  OR 'APH'        
237300                            OR 'API'  OR 'APJ'                            
237400*IF ECOM/VOUI AND IT IS Q OR P ON 4572 THEN WE MOVE STATUS 1              
237500       IF MATRIX-Q =  JA                                                  
237600           CONTINUE                                                       
237700       ELSE                                                               
237800          MOVE JA                 TO 4791-MID-FLAUTKRE    (IX2)           
237900          MOVE 'J'                TO 4791-MID-KDKREBEH    (IX2)           
238000       END-IF                                                             
238100     END-IF                                                               
238200        MOVE SPAR-IDDC (IX1)      TO WS-IDDC-INPUT                        
238300        MOVE WS-IDDISTR           TO TEST-IDDISTR                         
238400                                                                          
238500        IF WS-IDDC-INPUT NOT = W-IDDC-B6                                  
238600           MOVE WS-IDDC-INPUT        TO W-IDDC-B6                         
238700           PERFORM IMS-GU-WDB601                                          
238800        END-IF                                                            
238900        EVALUATE  TRUE                                                    
239000          WHEN DCS-NDC-PF AND DCS-AUSTRALIA                               
239100            MOVE WS-NDC-AU           TO 4791-MID-IDDC-RET    (IX2)        
239200          WHEN OTHER                                                      
239300              IF DIST34-AUSTRALIA-NDC                                     
239400                MOVE WS-NDC-AU       TO 4791-MID-IDDC-RET    (IX2)        
239500              ELSE                                                        
239600                MOVE SPAR-KDANMORS (IX1) TO OKOD-KDANMORS                 
239700                CALL W418OKOD USING OKOD-W418OKOD                         
239800                                                                          
239900                IF (GMT-FLLDCKND = JA  OR                                 
240000                    GMT-FLRETUR  = JA) AND                                
240100                   OKOD-FL-RETILL = JA                                    
240200                                                                          
240300                  IF GMT-IDDC-RET72(1) = SPACE                            
240400                    IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR            
240500                       DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR           
240600                       DIST34-MALAYSIA-NDC OR DIST34-JAPAN-NDC OR         
240700                       DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR        
240800                       DIST34-MEXICO-NDC   OR DIST34-BRAZIL-NDC OR        
240810                       DIST34-SOUTH-AFRICA-NDC                            
240900                      MOVE GMT-IDDC-RET   TO                              
241000                                          4791-MID-IDDC-RET (IX2)         
241100                    ELSE                                                  
241200                      MOVE WS-CDC-SE   TO 4791-MID-IDDC-RET (IX2)         
241300                    END-IF                                                
241400                  ELSE                                                    
241500                    IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR            
241600                       DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR           
241700                       DIST34-MALAYSIA-NDC OR DIST34-JAPAN-NDC OR         
241800                       DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR        
241900                       DIST34-MEXICO-NDC   OR DIST34-BRAZIL-NDC OR        
241910                       DIST34-SOUTH-AFRICA-NDC                            
242000                      IF GMT-IDDC-RET72(1) = GMT-IDDC-RET                 
242100                        MOVE GMT-IDDC-RET TO                              
242200                                           4791-MID-IDDC-RET (IX2)        
242300                      ELSE                                                
242400                        PERFORM S03-KOLLA-RETUR-DC                        
242500                      END-IF                                              
242600                    ELSE                                                  
242700                      IF GMT-IDDC-RET72(1) = WS-CDC-SE                    
242800                        MOVE WS-CDC-SE  TO 4791-MID-IDDC-RET (IX2)        
242900                      ELSE                                                
243000                        PERFORM S03-KOLLA-RETUR-DC                        
243100                      END-IF                                              
243200                    END-IF                                                
243300                  END-IF                                                  
243400                ELSE                                                      
243500                  IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR              
243600                     DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR             
243700                     DIST34-MALAYSIA-NDC OR DIST34-JAPAN-NDC OR           
243800                     DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR          
243900                     DIST34-MEXICO-NDC   OR DIST34-BRAZIL-NDC OR          
243910                     DIST34-SOUTH-AFRICA-NDC                              
244000                    MOVE GMT-IDDC-RET   TO 4791-MID-IDDC-RET (IX2)        
244100                  ELSE                                                    
244200                    MOVE WS-CDC-SE      TO 4791-MID-IDDC-RET (IX2)        
244300                  END-IF                                                  
244400                END-IF                                                    
244500              END-IF                                                      
244600        END-EVALUATE                                                      
244700                                                                          
244800        MOVE ZERO                    TO 4791-MID-PRFRAKT-RAD (IX2)        
244900        MOVE ZERO                    TO 4791-MID-IDLOPNRM    (IX2)        
245000        IF 4791-MID-KDVALISO = SPACE                                      
245100          IF SPAR-KDVALISO (IX1) NOT = SPACE                              
245200            MOVE SPAR-KDVALISO  (IX1)  TO 4791-MID-KDVALISO               
245300          END-IF                                                          
245400        END-IF                                                            
245500        MOVE SPAR-FLPRQUES     (IX1) TO 4791-MID-FLPRQUES    (IX2)        
245600        MOVE SPAR-KDVAT        (IX1) TO 4791-MID-KDVAT       (IX2)        
245700        MOVE SPAR-BEART-VIPS   (IX1) TO 4791-MID-BEART-VIPS  (IX2)        
245800        MOVE SPAR-PRARTSTD     (IX1) TO 4791-MID-PRARTSTD    (IX2)        
245900        MOVE SPAR-PRARTSJK     (IX1) TO 4791-MID-PRARTSJK    (IX2)        
246000                                                                          
246100        ADD +1                       TO IX1                               
246200                                        IX2                               
246300     END-PERFORM                                                          
246400                                                                          
246500     IF IX2 > 1                                                           
246600        SUBTRACT 1                   FROM IX2                             
246700     END-IF                                                               
246800                                                                          
246900     MOVE IX2                            TO 4791-MID-KVRADER              
247000     PERFORM EC-UPPDATERA-DISPATCHEN                                      
247100     .                                                                    
247200     EJECT                                                                
247300 EC-UPPDATERA-DISPATCHEN SECTION.                                         
247400                                                                          
247500*4791-LL = ((ANTAL-RADER * RAD-LÄNGDEN) + ÖVRIGT DATA) + 17 FÖR           
247600*          P-TO-P-SW                                                      
247700*                                                                         
247800     COMPUTE 4791-LL        = ((4791-MID-KVRADER * 221) + 77) + 17        
247900                                                                          
248000     CALL W006KOM USING MSG-PCB                                           
248100                        DISP-PCB                                          
248200                        KOMA-PCB                                          
248300                        MSG-KOM-WMSGKOM                                   
248400                        4791-MSG-IO-AREA                                  
248500                                                                          
248600     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
248700*       FELAKTIG UPPDATERING AV LEVERANSANM PÅ                            
248800*       KOMMUNIKATIONS DB                                                 
248900        MOVE                                                              
249000        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
249100                                     TO FELTEXT                           
249200        CALL FELLOG USING RKOD-ABEND-MED-DUMP                             
249300     END-IF                                                               
249400     .                                                                    
249500     EJECT                                                                
249600 MFS-RENSA-FAELT-UT SECTION.                                              
249700                                                                          
249800*    --- ALLA UTDATA-FÄLT                                                 
249900     MOVE MFS-RENSA-FAELT                 TO MOD-RELANDCO                 
250000                                             MOD-PRFRAKT                  
250100                                             MOD-PRFOERS                  
250200                                             MOD-PRLEGKST                 
250300     MOVE 'N'                             TO MOD-LEVANM-KLAR              
250400     MOVE MFS-ADD-LAES-IN-FAELT           TO MOD-LEVANM-KLAR-ATTR         
250500                                                                          
250600     MOVE +1                              TO IX                           
250700     PERFORM UNTIL IX > MAX-IX                                            
250800       MOVE IX                           TO MOD-KVRADER                   
250900       MOVE MFS-RENSA-FAELT               TO MOD-IDORDNR5 (IX)            
251000                                             MOD-IDKOLLI  (IX)            
251100                                             MOD-IDARTNR  (IX)            
251200                                             MOD-KVLEVANM (IX)            
251300                                             MOD-KDANMORS (IX)            
251400                                             MOD-KDEMBLEV (IX)            
251500                                             MOD-PRARTBTO (IX)            
251600                                             MOD-KDFAKTYP (IX)            
251700                                             MOD-IDFAKT   (IX)            
251800                                             MOD-IDDC     (IX)            
251900                                             MOD-IDFELKOD-1 (IX)          
252000                                             MOD-IDFELKOD-2 (IX)          
252100                                             MOD-IDFELKOD-3 (IX)          
252200       ADD +1                                TO IX                        
252300     END-PERFORM                                                          
252400     .                                                                    
252500                                                                          
252600 MFS-RENSA-FAELT-IN SECTION.                                              
252700                                                                          
252800*    --- ALLA INDATA-FÄLT                                                 
252900     MOVE MFS-RENSA-FAELT                 TO MOD-IDDISTR-IN               
253000                                             MOD-IDKUNDNR-IN              
253100                                             MOD-IDRAPPNR-IN              
253200     .                                                                    
253300     EJECT                                                                
253400 MFS-RENSA-NYCKELFAELT-UT SECTION.                                        
253500                                                                          
253600*    --- ALLA INDATA-FÄLT                                                 
253700     MOVE MFS-RENSA-FAELT                 TO MOD-IDDISTR-UT               
253800                                             MOD-IDKUNDNR-UT              
253900                                             MOD-IDRAPPNR-UT              
254000     .                                                                    
254100     EJECT                                                                
254200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
254300                                                                          
254400*    --- ALLA UTDATA-FÄLT                                                 
254500     MOVE MFS-ROER-EJ-FAELT               TO MOD-RELANDCO                 
254600                                             MOD-PRFRAKT                  
254700                                             MOD-PRFOERS                  
254800                                             MOD-PRLEGKST                 
254900                                             MOD-LEVANM-KLAR              
255000                                                                          
255100     MOVE +1                              TO IX                           
255200     PERFORM UNTIL IX > MAX-IX                                            
255300       MOVE IX                           TO MOD-KVRADER                   
255400       MOVE MFS-ROER-EJ-FAELT             TO MOD-IDORDNR5 (IX)            
255500                                             MOD-IDKOLLI  (IX)            
255600                                             MOD-IDARTNR  (IX)            
255700                                             MOD-KVLEVANM (IX)            
255800                                             MOD-KDANMORS (IX)            
255900                                             MOD-KDEMBLEV (IX)            
256000                                             MOD-PRARTBTO (IX)            
256100                                             MOD-KDFAKTYP (IX)            
256200                                             MOD-IDFAKT   (IX)            
256300                                             MOD-IDDC     (IX)            
256400       ADD +1                                TO IX                        
256500     END-PERFORM                                                          
256600     .                                                                    
256700     EJECT                                                                
256800 S01-DIST-KUND-LDC SECTION.                                               
256900                                                                          
257000     MOVE WS-IDDISTR          TO  W-WDB2-IDDISTR                          
257100     MOVE WS-IDKUNDNR         TO  W-WDB2-IDKUNDNR                         
257200                                                                          
257300     PERFORM IMS-GU-WDB201                                                
257400                                                                          
257500     IF SEGMENT-SAKNAS                                                    
257600        MOVE NEJ              TO  GMT-FLLDCKND                            
257700                                  GMT-FLRETUR                             
257800     END-IF                                                               
257900     .                                                                    
258000     EJECT                                                                
258100 S03-KOLLA-RETUR-DC  SECTION.                                             
258200                                                                          
258300     MOVE NEJ                       TO  GODK-KOD-SW                       
258400     MOVE JA                        TO  GODK-ARTIKEL-SW                   
258500                                        GODK-IDFKNGRP-SW                  
258600                                        GODK-DC-ARTIKEL-SW                
258700                                        GODK-DC-LEV-SW                    
258800                                                                          
258900     MOVE GMT-IDDC-RET72(1)   TO W-IDDC-B6                                
259000                                 W-IDDC-K7                                
259100     PERFORM IMS-GU-WDB601                                                
259200     IF SEGMENT-SAKNAS                                                    
259300       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
259400          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
259500          DIST34-MALAYSIA-NDC OR DIST34-JAPAN-NDC OR                      
259600          DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                     
259700          DIST34-MEXICO-NDC   OR DIST34-BRAZIL-NDC OR                     
259710          DIST34-SOUTH-AFRICA-NDC                                         
259800         MOVE GMT-IDDC-RET      TO 4791-MID-IDDC-RET (IX2)                
259900       ELSE                                                               
260000         MOVE WS-CDC-SE         TO 4791-MID-IDDC-RET (IX2)                
260100       END-IF                                                             
260200     ELSE                                                                 
260300       IF DCS-FLARTDC = JA                                                
260400*- KOLLA OM ARTIKELN FINNS PÅ DC'T. KRAV FÖR ATT TA EMOT RETUR.           
260500         MOVE SPAR-IDARTNR (IX1) TO W-IDARTNR                             
260600         PERFORM IMS-GU-WDK711                                            
260700         IF SEGMENT-SAKNAS                                                
260800           MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                               
260900         END-IF                                                           
261000       END-IF                                                             
261100                                                                          
261200       IF EJ-GODK-DC-ARTIKEL                                              
261300         CONTINUE                                                         
261400       ELSE                                                               
261500         MOVE SPACE           TO W-URV-TEELMT                             
261600         MOVE WC-IDARTNR      TO W-URV-TEELMT                             
261700         MOVE SPACE           TO W-URV-FILLER                             
261800         MOVE SPAR-IDARTNR (IX1)  TO W-URV-IDARTNR-EXCP                   
261900                                                                          
262000         PERFORM IMS-GNP-WDB611-FIRST                                     
262100         IF SEGMENT-FINNS                                                 
262200           MOVE NEJ           TO GODK-ARTIKEL-SW                          
262300         ELSE                                                             
262400           MOVE SPAR-IDARTNR (IX1) TO W-IDARTNR-K6                        
262500           PERFORM IMS-GU-WDK601                                          
262600           IF SEGMENT-SAKNAS                                              
262700             MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                             
262800           ELSE                                                           
262900             MOVE SPACE           TO W-URV-TEELMT                         
263000             MOVE WC-IDFKNGRP     TO W-URV-TEELMT                         
263100             MOVE SPACE           TO W-URV-FILLER                         
263200             MOVE ART-IDFKNGRP    TO W-URV-IDFKNGRP-EXCP                  
263300                                                                          
263400             PERFORM IMS-GNP-WDB611-FIRST                                 
263500             IF SEGMENT-FINNS                                             
263600               MOVE NEJ           TO GODK-IDFKNGRP-SW                     
263700             ELSE                                                         
263800               MOVE SPACE           TO W-URV-TEELMT                       
263900               MOVE WC-IDDC-EXCP    TO W-URV-TEELMT                       
264000               MOVE SPACE           TO W-URV-FILLER                       
264100               MOVE SPAR-IDDC (IX1) TO W-URV-IDDC-EXCP                    
264200                                                                          
264300               PERFORM IMS-GNP-WDB611-FIRST                               
264400               IF SEGMENT-FINNS                                           
264500                 MOVE NEJ           TO GODK-DC-LEV-SW                     
264600               ELSE                                                       
264700                 MOVE SPACE           TO W-URV-TEELMT                     
264800                 MOVE WC-KDANMORS     TO W-URV-TEELMT                     
264900                 MOVE SPACE           TO W-URV-FILLER                     
265000                 MOVE SPAR-KDANMORS (IX1) TO W-URV-KDANMORS-RET           
265100                                                                          
265200                 PERFORM IMS-GNP-WDB611-FIRST                             
265300                 IF SEGMENT-FINNS                                         
265400                   MOVE JA            TO GODK-KOD-SW                      
265500                 END-IF                                                   
265600               END-IF                                                     
265700             END-IF                                                       
265800           END-IF                                                         
265900         END-IF                                                           
266000       END-IF                                                             
266100                                                                          
266200       IF EJ-GODK-DC-ARTIKEL OR                                           
266300          EJ-GODK-ARTIKEL OR                                              
266400          EJ-GODK-IDFKNGRP OR                                             
266500          EJ-GODK-DC-LEV   OR                                             
266600          EJ-GODK-KOD                                                     
266700                                                                          
266800          IF GMT-IDDC-RET72(2) = SPACE                                    
266900            MOVE GMT-IDDC-RET72(3)  TO 4791-MID-IDDC-RET (IX2)            
267000            IF 4791-MID-IDDC-RET (IX2) = SPACE                            
267100              IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                  
267200                 DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                 
267300                 DIST34-MALAYSIA-NDC OR DIST34-JAPAN-NDC OR               
267400                 DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR              
267500                 DIST34-MEXICO-NDC   OR DIST34-BRAZIL-NDC OR              
267510                 DIST34-SOUTH-AFRICA-NDC                                  
267600                MOVE GMT-IDDC-RET     TO 4791-MID-IDDC-RET (IX2)          
267700              ELSE                                                        
267800                MOVE WS-CDC-SE        TO 4791-MID-IDDC-RET (IX2)          
267900              END-IF                                                      
268000            END-IF                                                        
268100          ELSE                                                            
268200            PERFORM S04-KOLLA-RETUR-DC-2                                  
268300          END-IF                                                          
268400       ELSE                                                               
268500         MOVE GMT-IDDC-RET72(1)     TO 4791-MID-IDDC-RET (IX2)            
268600       END-IF                                                             
268700     END-IF                                                               
268800     .                                                                    
268900     EJECT                                                                
269000 S04-KOLLA-RETUR-DC-2  SECTION.                                           
269100                                                                          
269200     MOVE NEJ                       TO  GODK-KOD-SW                       
269300     MOVE JA                        TO  GODK-ARTIKEL-SW                   
269400                                        GODK-IDFKNGRP-SW                  
269500                                        GODK-DC-ARTIKEL-SW                
269600                                        GODK-DC-LEV-SW                    
269700                                                                          
269800     MOVE GMT-IDDC-RET72(2)   TO W-IDDC-B6                                
269900                                 W-IDDC-K7                                
270000     PERFORM IMS-GU-WDB601                                                
270100     IF SEGMENT-SAKNAS                                                    
270200       IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                         
270300          DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                        
270400          DIST34-MALAYSIA-NDC OR DIST34-JAPAN-NDC OR                      
270500          DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                     
270600          DIST34-MEXICO-NDC   OR DIST34-BRAZIL-NDC OR                     
270610          DIST34-SOUTH-AFRICA-NDC                                         
270700         MOVE GMT-IDDC-RET    TO 4791-MID-IDDC-RET (IX2)                  
270800       ELSE                                                               
270900         MOVE WS-CDC-SE       TO 4791-MID-IDDC-RET (IX2)                  
271000       END-IF                                                             
271100     ELSE                                                                 
271200       IF DCS-FLARTDC = JA                                                
271300*- KOLLA OM ARTIKELN FINNS PÅ DC'T. KRAV FÖR ATT TA EMOT RETUR.           
271400         MOVE SPAR-IDARTNR (IX1) TO W-IDARTNR                             
271500         PERFORM IMS-GU-WDK711                                            
271600         IF SEGMENT-SAKNAS                                                
271700           MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                               
271800         END-IF                                                           
271900       END-IF                                                             
272000                                                                          
272100       IF EJ-GODK-DC-ARTIKEL                                              
272200         CONTINUE                                                         
272300       ELSE                                                               
272400         MOVE SPACE           TO W-URV-TEELMT                             
272500         MOVE WC-IDARTNR      TO W-URV-TEELMT                             
272600         MOVE SPACE           TO W-URV-FILLER                             
272700         MOVE SPAR-IDARTNR (IX1)  TO W-URV-IDARTNR-EXCP                   
272800                                                                          
272900         PERFORM IMS-GNP-WDB611-FIRST                                     
273000         IF SEGMENT-FINNS                                                 
273100           MOVE NEJ           TO GODK-ARTIKEL-SW                          
273200         ELSE                                                             
273300           MOVE SPAR-IDARTNR (IX1) TO W-IDARTNR-K6                        
273400           PERFORM IMS-GU-WDK601                                          
273500           IF SEGMENT-SAKNAS                                              
273600             MOVE NEJ  TO  GODK-DC-ARTIKEL-SW                             
273700           ELSE                                                           
273800             MOVE SPACE            TO W-URV-TEELMT                        
273900             MOVE WC-IDFKNGRP      TO W-URV-TEELMT                        
274000             MOVE SPACE            TO W-URV-FILLER                        
274100             MOVE ART-IDFKNGRP     TO W-URV-IDFKNGRP-EXCP                 
274200                                                                          
274300             PERFORM IMS-GNP-WDB611-FIRST                                 
274400             IF SEGMENT-FINNS                                             
274500               MOVE NEJ            TO GODK-IDFKNGRP-SW                    
274600             ELSE                                                         
274700               MOVE SPACE            TO W-URV-TEELMT                      
274800               MOVE WC-IDDC-EXCP     TO W-URV-TEELMT                      
274900               MOVE SPACE            TO W-URV-FILLER                      
275000               MOVE SPAR-IDDC (IX1)  TO W-URV-IDDC-EXCP                   
275100                                                                          
275200               PERFORM IMS-GNP-WDB611-FIRST                               
275300               IF SEGMENT-FINNS                                           
275400                 MOVE NEJ            TO GODK-DC-LEV-SW                    
275500               ELSE                                                       
275600                 MOVE SPACE          TO W-URV-TEELMT                      
275700                 MOVE WC-KDANMORS    TO W-URV-TEELMT                      
275800                 MOVE SPACE          TO W-URV-FILLER                      
275900                 MOVE SPAR-KDANMORS (IX1) TO W-URV-KDANMORS-RET           
276000                                                                          
276100                 PERFORM IMS-GNP-WDB611-FIRST                             
276200                 IF SEGMENT-FINNS                                         
276300                   MOVE JA           TO GODK-KOD-SW                       
276400                 END-IF                                                   
276500               END-IF                                                     
276600             END-IF                                                       
276700           END-IF                                                         
276800         END-IF                                                           
276900       END-IF                                                             
277000                                                                          
277100       IF EJ-GODK-DC-ARTIKEL OR                                           
277200          EJ-GODK-ARTIKEL OR                                              
277300          EJ-GODK-IDFKNGRP OR                                             
277400          EJ-GODK-DC-LEV OR                                               
277500          EJ-GODK-KOD                                                     
277600                                                                          
277700          MOVE GMT-IDDC-RET72(3)   TO 4791-MID-IDDC-RET (IX2)             
277800          IF 4791-MID-IDDC-RET (IX2) = SPACE                              
277900            IF DIST34-KINA-NDC  OR DIST34-INDIA-NDC OR                    
278000               DIST34-KOREA-NDC OR DIST34-TURKEY-NDC OR                   
278100               DIST34-MALAYSIA-NDC OR DIST34-JAPAN-NDC OR                 
278200               DIST34-THAILAND-NDC OR DIST34-TAIWAN-NDC OR                
278300               DIST34-MEXICO-NDC   OR DIST34-BRAZIL-NDC OR                
278310               DIST34-SOUTH-AFRICA-NDC                                    
278400              MOVE GMT-IDDC-RET    TO 4791-MID-IDDC-RET (IX2)             
278500            ELSE                                                          
278600              MOVE WS-CDC-SE       TO 4791-MID-IDDC-RET (IX2)             
278700            END-IF                                                        
278800          END-IF                                                          
278900       ELSE                                                               
279000         MOVE GMT-IDDC-RET72(2)    TO 4791-MID-IDDC-RET (IX2)             
279100       END-IF                                                             
279200     END-IF                                                               
279300     .                                                                    
279400     EJECT                                                                
279500                                                                          
279600*    --- DISPATCHER SECTIONS                                              
279700 S11-FETCH-REQUEST-ARGUMENT SECTION.                                      
279800                                                                          
279900     MOVE 'GETARG'                    TO SUB-KDFUNC                       
280000     MOVE 'CARPARTS.PULS.APIDISCREG'        TO SUB-ADDISPABS              
280100     MOVE SPACE                       TO SUB-DATA                         
280200     MOVE 999                         TO REQU-KVRADER                     
280300     MOVE LENGTH OF SUB-DATA          TO SUB-KVDLEN                       
280400                                                                          
280500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN SUB-DATA              
280600                                                                          
280700     IF SUB-KDRC > 0                                                      
280800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
280900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
281000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
281100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
281200     END-IF                                                               
281300     .                                                                    
281400     SKIP3                                                                
281500 S12-RETURN-RESPONSE SECTION.                                             
281600                                                                          
281700     MOVE 'RETURN'                    TO SUB-KDFUNC                       
281800     MOVE LENGTH OF RESP-AREA         TO SUB-KVDLEN                       
281900                                                                          
282000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
282100                                                                          
282200     IF SUB-KDRC > 0                                                      
282300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
282400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
282500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
282600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
282700     END-IF                                                               
282800     .                                                                    
282900                                                                          
283000* --- IMS SEKTIONER ---                                                   
283100                                                                          
283200 IMS-GET-MSG SECTION.                                                     
283300                                                                          
283400     MOVE '  QC' TO GODK-STATUSKODER                                      
283500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
283600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
283700     PERFORM IMS-STATUSKONTROLL                                           
283800     .                                                                    
283900                                                                          
284000 IMS-INSERT-MSG SECTION.                                                  
284100                                                                          
284200     IF ENGLISH-TEXT                                                      
284300       MOVE 'N' TO MFS-KDHUVOMR                                           
284400     END-IF                                                               
284500     MOVE LOW-VALUE TO MOD-KDZ1 MOD-KDZ2                                  
284600     MOVE SPACE TO GODK-STATUSKODER                                       
284700     CALL CBLTDLI USING ISRT MSG-PCB MOD MFS-IDMOD                        
284800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
284900     PERFORM IMS-STATUSKONTROLL                                           
285000     .                                                                    
285100     EJECT                                                                
285200 IMS-GET-WDB201                 SECTION.                                  
285300                                                                          
285400     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
285500                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
285600            DELIMITED BY SIZE INTO SSA1                                   
285700                                                                          
285800     MOVE '  GE' TO GODK-STATUSKODER                                      
285900     CALL CBLTDLI USING GU WDB2-PCB GMT-WDB201 SSA1                       
286000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
286100     PERFORM IMS-STATUSKONTROLL                                           
286200     .                                                                    
286300     EJECT                                                                
286400 IMS-GET-WDB101                 SECTION.                                  
286500                                                                          
286600     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
286700            DELIMITED BY SIZE INTO SSA1                                   
286800     MOVE '  GE' TO GODK-STATUSKODER                                      
286900     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB1 SSA1                 
287000     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
287100     PERFORM IMS-STATUSKONTROLL                                           
287200     .                                                                    
287300     EJECT                                                                
287400 IMS-GET-WDA201 SECTION.                                                  
287500     MOVE '*** IMS-GET-WDA201 *** '                                       
287600                              TO FELTEXT                                  
287700                                                                          
287800     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
287900            DELIMITED BY SIZE INTO SSA1                                   
288000                                                                          
288100     MOVE '  GE' TO GODK-STATUSKODER                                      
288200     CALL CBLTDLI USING GU WDA2-PCB DLI-IO-AREA-WDA201 SSA1               
288300     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
288400     PERFORM IMS-STATUSKONTROLL                                           
288500     .                                                                    
288600     EJECT                                                                
288700 IMS-GNP-WDA211 SECTION.                                                  
288800                                                                          
288900     MOVE 'WDA211   ' TO SSA1                                             
289000     MOVE '  GE' TO GODK-STATUSKODER                                      
289100     CALL CBLTDLI USING GNP WDA2-PCB DLI-IO-AREA-WDA211 SSA1              
289200     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
289300     PERFORM IMS-STATUSKONTROLL                                           
289400     .                                                                    
289500     EJECT                                                                
289600 IMS-GN-WDA2A1   SECTION.                                                 
289700                                                                          
289800     STRING 'WDA2A1  (WDA2A1KY>=' W-WDA2A1KY-MIN-X                        
289900                    '&WDA2A1KY<=' W-WDA2A1KY-MAX-X                        
290000                    '&IDDISTR  =' W-IDDISTR-X                             
290100                    '&IDRAPPNR =' W-IDRAPPNR-X ')'                        
290200          DELIMITED BY SIZE INTO SSA1                                     
290300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
290400     CALL CBLTDLI USING GN WDA2A-PCB DLI-IO-AREA-WDA2A1 SSA1              
290500     MOVE WDA2A-STATUS-CODE TO STATUS-WS                                  
290600     PERFORM IMS-STATUSKONTROLL                                           
290700     .                                                                    
290800     EJECT                                                                
290900 IMS-GU-WDB201      SECTION.                                              
291000                                                                          
291100     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
291200          DELIMITED BY SIZE INTO SSA1                                     
291300     MOVE '  GE' TO GODK-STATUSKODER                                      
291400     CALL CBLTDLI USING GU WDB2-PCB GMT-WDB201 SSA1                       
291500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
291600     PERFORM IMS-STATUSKONTROLL                                           
291700     .                                                                    
291800     EJECT                                                                
291900 IMS-GU-WDB601    SECTION.                                                
292000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
292100          DELIMITED BY SIZE INTO SSA1                                     
292200     MOVE '  GE' TO GODK-STATUSKODER                                      
292300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
292400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
292500     PERFORM IMS-STATUSKONTROLL                                           
292600     IF SEGMENT-SAKNAS                                                    
292700        MOVE SPACE TO DCS-KDDC                                            
292800     END-IF                                                               
292900     .                                                                    
293000     EJECT                                                                
293100 IMS-GNP-WDB611-FIRST    SECTION.                                         
293200                                                                          
293300     STRING 'WDB611  *F(WDB611KY =' W-WDB611KY-X ')'                      
293400          DELIMITED BY SIZE INTO SSA1                                     
293500     MOVE '  GE' TO GODK-STATUSKODER                                      
293600     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB611 SSA1                   
293700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
293800     PERFORM IMS-STATUSKONTROLL                                           
293900     .                                                                    
294000     EJECT                                                                
294100 IMS-GU-WDK711    SECTION.                                                
294200                                                                          
294300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
294400     DELIMITED BY SIZE INTO SSA1                                          
294500     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
294600     DELIMITED BY SIZE INTO SSA2                                          
294700     MOVE '  GE' TO GODK-STATUSKODER                                      
294800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
294900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
295000     PERFORM IMS-STATUSKONTROLL                                           
295100     .                                                                    
295200     EJECT                                                                
295300 IMS-GU-WDK601    SECTION.                                                
295400                                                                          
295500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
295600          DELIMITED BY SIZE INTO SSA1                                     
295700     MOVE '  GE'           TO GODK-STATUSKODER                            
295800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
295900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
296000     PERFORM IMS-STATUSKONTROLL                                           
296100     .                                                                    
296200     EJECT                                                                
296300 IMS-STATUSKONTROLL SECTION.                                              
296400                                                                          
296500     SET STATUS-IX TO 1                                                   
296600     SEARCH GODK-STATUS                                                   
296700       AT END                                                             
296800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
296900         DELIMITED BY SIZE INTO FELTEXT                                   
297000         CALL FELLOG                                                      
297100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
297200         CONTINUE                                                         
297300     END-SEARCH                                                           
297400     .                                                                    
