000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6032300.                                                
000300 AUTHOR.         JOHAN NIHLBLAD                                           
000400 DATE-WRITTEN.   05/04/13.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RELEASE SCRAP ORDERS                                             
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001300*                              WLARTS (WDK7)                              
001400*                                6321 (WDR5)                              
001500*                                6327 (WDR5)                              
001600*                                2402 (WDR5)                              
001700*                                WDG9                                     
001800*                                WDF2                                     
001900*                   STARTAR RUTIN W216S1 I SOP                            
002000*                   (SKAPAR SKROTORDER).                                  
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W6T323                                              
002400*        MID:         W6I32301                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         W6O32301                                            
002800*                                                                         
002900*    E'TRACKER 4823800 20071128 NEW LDC                                   
003000*    E'TRACKER 9399577 20110810 DESTOCKING SCRAP                          
003100*    E'TRACKER 10143271 DATE 2011-10-19 CHINA WAREHOUSE PROJECT-1         
003200*    E'TRACKER 10143273 DATE 2012-08-21 LOCAL SOURCING                    
003300*                                                                         
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(08)   VALUE 'W6032300'.            
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400 77  WS-BODIL                    PIC X     VALUE SPACE.                   
004500 77  LAES-SW                     PIC X     VALUE SPACE.                   
004600 77  WS-SKROTDATUM-SLUT          PIC X     VALUE SPACE.                   
004700 77  WS-DATUM-HITTAD             PIC X     VALUE SPACE.                   
004800 77  WS-ART-SLUT                 PIC X     VALUE SPACE.                   
004900 77  PROGSW-IX                   PIC S9(3)  VALUE ZERO  COMP-3.           
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  WS-FLLAS-6326               PIC X       VALUE 'N'.                   
005300 77  RAD-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
005400 77  WS-IX-NEXT                  PIC S9(3)  VALUE ZERO  COMP-3.           
005500 77  RAD-IX-MAX                  PIC S9(3)  VALUE +12   COMP-3.           
005600 77  TAB-IX-MAX                  PIC S9(3)  VALUE +100  COMP-3.           
005700 77  URV-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
005800 77  PERS-IX                     PIC S9(3)  VALUE ZERO  COMP-3.           
005900 77  IX-BEEMB                    PIC S9(4)   VALUE +0  COMP SYNC.         
006000 77  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
006100 77  MAX-IX                      PIC S9(4)   VALUE +11 COMP SYNC.         
006200 77  MAX-PERS-IX                 PIC S9(3)  VALUE +10  COMP-3.            
006300 77  SPAR-IDDC                   PIC X(2)   VALUE SPACE.                  
006400 77  SPAR-IDARTNR                PIC S9(9)  VALUE ZERO COMP-3.            
006500 77  DAGENS-DATUM-Y2K            PIC 9(8)   VALUE ZERO.                   
006600 77  WS-ANTAL-X                  PIC 9(3)   VALUE ZERO COMP-3.            
006700 77  W-TID                       PIC 9(8)   VALUE ZERO.                   
006800 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
006900 77  SW-6326-OK                  PIC X      VALUE SPACE.                  
007000 77  TEST-IDINK                  PIC 9(3)    VALUE ZERO.                  
007100 77  WS-IDLOGLOP                 PIC 9(01) COMP-3 VALUE ZERO.             
007200 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
007300                                                                          
007400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007500     88  INDATA-OK                           VALUE 'J'.                   
007600     88  INDATA-FEL                          VALUE 'N'.                   
007700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007800     88  NYCKLAR-OK                          VALUE 'J'.                   
007900     88  NYCKLAR-FEL                         VALUE 'N'.                   
008000 77  JUMP-6325-SW                PIC X       VALUE 'N'.                   
008100     88  JUMP-6325                           VALUE 'J'.                   
008200     88  NO-JUMP                             VALUE 'N'.                   
008300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008400     88  EGEN-MID                            VALUE '6323'.                
008500     88  GODK-MID                            VALUE '6323' '6325'.         
008600 77  WS-CMD                      PIC X       VALUE SPACE.                 
008700 77  WS-FL6326                   PIC X       VALUE 'N'.                   
008800                                                                          
008900 77  KDARBTYP-SOEKNING           PIC X       VALUE 'N'.                   
009000 77  IDDC-SOEKNING               PIC X       VALUE 'N'.                   
009100 77  IDARTNR-SOEKNING            PIC X       VALUE 'N'.                   
009200 77  WS-DASKROT9                 PIC 9(8)    VALUE ZERO.                  
009300 77  WS-SPAR-DASKROT9            PIC 9(8)    VALUE ZERO.                  
009400 77  WS-SUARTSTD                 PIC 9(7)V9(2) VALUE ZERO.                
009500 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
009600 77  WS-DASKROT9-BEORD           PIC 9(8)    VALUE ZERO.                  
009700 77  WS-6322-DASKROT9            PIC 9(8)    VALUE ZERO.                  
009800 77  W-KVSKROT-REST              PIC S9(7)  VALUE ZERO.                   
009900 77  W-ADBUFFPL                  PIC 9(5)  VALUE ZERO.                    
010000 77  TEST-NYCKEL-IDARTNR         PIC X(9)    VALUE SPACE.                 
010100 77  TEST-NYCKEL-TIDATUM         PIC X(6)    VALUE SPACE.                 
010200 77  TEST-NYCKEL-IDANSK          PIC X(3)    VALUE SPACE.                 
010300 77  WS-FLHOGRE                  PIC X       VALUE 'N'.                   
010400 77  WS-FLURVAL                  PIC X       VALUE 'N'.                   
010500 77  WS-HIGHLEV                  PIC X       VALUE 'N'.                   
010600 77  WS-SUBEL                    PIC 9(7)    VALUE ZERO.                  
010700 77  W-SUBEL                     PIC 9(7)    VALUE ZERO.                  
010800 77  WS-IDUSER                   PIC X(8)    VALUE SPACE.                 
010900 77  WS-KDERS-UTG                PIC Z(2)    VALUE ZERO.                  
011000 77  WS-SUTPO-TOT                PIC Z(6)9   VALUE ZERO.                  
011100 77  WS-KVSKROT-BEORD            PIC Z(6)9   VALUE ZERO.                  
011200 77  WS-KVSKROT-KVAR             PIC Z(6)9   VALUE ZERO.                  
011300 77  WS-KVTILLG-CDC              PIC Z(6)9   VALUE ZERO.                  
011400 77  WS-KVTILLG-SDC              PIC Z(6)9   VALUE ZERO.                  
011500 77  WS-KVAKS-CDC                PIC Z(6)9   VALUE ZERO.                  
011600 77  WS-KVAKS-SDC                PIC Z(6)9   VALUE ZERO.                  
011700 77  W-ANNUL-IDUSER              PIC X(8)    VALUE SPACE.                 
011800 77  W-ANNUL-IDMAIL              PIC X(60)   VALUE SPACE.                 
011900 77  WS-IDLEVNR-NUM              PIC 9(5)    VALUE ZERO.                  
012000 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
012100 77  WS-IDARTNR-8                PIC 9(08)   VALUE ZERO.                  
012200 01  ANUL-BEANST-GODK            PIC X(25) VALUE SPACE.                   
012300 01  ANUL-IDMAIL                 PIC X(60) VALUE SPACE.                   
012400 01  WSM-IDARTNR                 PIC Z(8)9 VALUE ZERO.                    
012500     EJECT                                                                
012600*      --- VALID IDDC CODES                                               
012700*                                                                         
012800*01    -COPY WWDC99                                                       
012900*01    -COPY WWDC99 -PRE SW-                                              
013000*                                                                         
013100*01    -COPY WWPRODSL                                                     
013200*                                                                         
013300       EJECT                                                              
013400*01  FILLER.                                                              
013500*    03 IDDC-TABELL.                                                      
013600*       05  IDDC-TABELL1-20             PIC X(40) VALUE                   
013700*           '112122232425264142435161621A1B2A3A2B2C2D'.                   
013800*       05  IDDC-TABELL21-30            PIC X(20) VALUE                   
013900*           '2E2F2G2H2I2J3B3C3D3E'.                                       
014000*       05  IDDC-TABELL31-41            PIC X(22) VALUE                   
014100*           '1C1K2K2L2M2N2O3F3G3H3I'.                                     
014200*    03 TAB-IDDC REDEFINES IDDC-TABELL  PIC X(2) OCCURS 41.               
014300                                                                          
014400 01  IDDC-TABELL.                                                         
014500     03 WS-IDDC-ARRAY  OCCURS 100 INDEXED BY TAB-IX.                      
014600       05 TAB-IDDC             PIC X(2).                                  
014700                                                                          
014800 01  WS-SEKEL-KOLL               PIC 9(6).                                
014900 01  FILLER REDEFINES WS-SEKEL-KOLL.                                      
015000     03  WS-SEKEL                PIC 9(1).                                
015100     03  FILLER                  PIC 9(5).                                
015200                                                                          
015300 01  WS-TIDATETIME               PIC X(14).                               
015400 01  FILLER REDEFINES WS-TIDATETIME.                                      
015500     03  WS-DATUM                PIC 9(8).                                
015600     03  WS-TIDHHMMSS            PIC 9(6).                                
015700                                                                          
015800 01  WS-TID                      PIC 9(8) VALUE ZERO.                     
015900 01  WS-DAREGDAT                 PIC 9(8).                                
016000                                                                          
016100 01  WS-DASKROT.                                                          
016200     03  WS-DASKROT-SS                PIC 9(2).                           
016300     03  WS-DASKROT-AAMMDD            PIC 9(6).                           
016400 01  WS-AAAAMMDD REDEFINES WS-DASKROT PIC 9(8).                           
016500 01  W-IDAVTAL-RED               PIC 9(13).                               
016600 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
016700     03  FILLER                  PIC X.                                   
016800     03  W-PREFIX                PIC X(3).                                
016900     03  W-AVTALSNR              PIC X(6).                                
017000     03  W-SUFFIX                PIC X(3).                                
017100     SKIP2                                                                
017200                                                                          
017300                                                                          
017400 01  WS-BC-PARAMETRAR.                                                    
017500     03  WS-URVAL.                                                        
017600         05  URV-FLKLAR       PIC X     VALUE SPACE.                      
017700         05  URV-KDARBTYP     PIC X(8)  VALUE SPACE.                      
017800     03  URV-TABELL.                                                      
017900         05 URV-TAB-RAD OCCURS 12.                                        
018000            07  URV-IDDC             PIC X(2).                            
018100            07  URV-IDARTNR          PIC 9(9).                            
018200            07  URV-DASKROT9-BEORD   PIC 9(8).                            
018300 01  BAS-R22-REGPOST.                                                     
018400*    03      -COPY W212R22   -PRE BAS-R22-                                
018500     03 BAS-R22-REST            PIC X(41).                                
018600     EJECT                                                                
018700 01  BAS-R23-REGPOST.                                                     
018800*    03      -COPY W212R23   -PRE BAS-R23-                                
018900     03 BAS-R23-REST            PIC X(41).                                
019000     EJECT                                                                
019100 77  WS-IX                      PIC S9(3)  VALUE ZERO  COMP-3.            
019200 77  WO-IX                      PIC S9(3)  VALUE ZERO  COMP-3.            
019300 77  O-IX                       PIC S9(3)  VALUE ZERO  COMP-3.            
019400 77  SW-VISA-6325               PIC X      VALUE 'N'.                     
019500 77  WO-IDARTNR                 PIC S9(9)  VALUE ZERO COMP-3.             
019600 77  WO-DASKROT9                PIC 9(8)   VALUE ZERO.                    
019700 77  WO-IDDC-6325               PIC X(2)   VALUE SPACE.                   
019800     SKIP3                                                                
019900 01  WO-TEMFSINF-TAB.                                                     
020000     03  WO-TEMFSINF-ORS  OCCURS 6 PIC X(9).                              
020100                                                                          
020200 01  WS-TEMEMO-TAB.                                                       
020300     03  FILLER      PIC X(66)  VALUE 'SPÄRRAD KVANT       '.             
020400     03  FILLER      PIC X(66)  VALUE 'INGÅR I SATS        '.             
020500     03  FILLER      PIC X(66)  VALUE '300-/400-SERIEN     '.             
020600     03  FILLER      PIC X(66)  VALUE 'TILLBEHÖR           '.             
020700     03  FILLER      PIC X(66)  VALUE 'BESTÄLLNINGSREST    '.             
020800     03  FILLER      PIC X(66)  VALUE 'KAMPANJ             '.             
020900     03  FILLER      PIC X(66)  VALUE 'STANDARD            '.             
021000     03  FILLER      PIC X(66)  VALUE 'SÄKERHETSPRODUKT    '.             
021100     03  FILLER      PIC X(66)  VALUE 'BYTES               '.             
021200     03  FILLER      PIC X(66)  VALUE 'DEKAL               '.             
021300     03  FILLER      PIC X(66)  VALUE '01-MÄRKT            '.             
021400 01  FILLER   REDEFINES  WS-TEMEMO-TAB.                                   
021500     03  WS-TEMEMO-ORS  OCCURS 11 PIC X(66).                              
021600                                                                          
021700 01  WS-TEMEMO-KORT-TAB.                                                  
021800     03  FILLER      PIC X(09)  VALUE 'SPÄRR.KV '.                        
021900     03  FILLER      PIC X(09)  VALUE 'ING.SATS '.                        
022000     03  FILLER      PIC X(09)  VALUE '300-/400 '.                        
022100     03  FILLER      PIC X(09)  VALUE 'TILLBEH  '.                        
022200     03  FILLER      PIC X(09)  VALUE 'BESTREST '.                        
022300     03  FILLER      PIC X(09)  VALUE 'KAMPANJ  '.                        
022400     03  FILLER      PIC X(09)  VALUE 'STANDARD '.                        
022500     03  FILLER      PIC X(09)  VALUE 'SÄKERHET '.                        
022600     03  FILLER      PIC X(09)  VALUE 'BYTES    '.                        
022700     03  FILLER      PIC X(09)  VALUE 'DEKAL    '.                        
022800     03  FILLER      PIC X(09)  VALUE '01-MÄRKT '.                        
022900 01  FILLER   REDEFINES  WS-TEMEMO-KORT-TAB.                              
023000     03  WS-TEMEMO-KORT OCCURS 11 PIC X(09).                              
023100     EJECT                                                                
023200 01  WS-TEMEMO-ESC-TAB.                                                   
023300     03  FILLER      PIC X(66)  VALUE 'REPLACED            '.             
023400     03  FILLER      PIC X(66)  VALUE '98 SCRAP            '.             
023500 01  FILLER   REDEFINES  WS-TEMEMO-ESC-TAB.                               
023600     03  WS-TEMEMO-ESC-ORS  OCCURS 11 PIC X(66).                          
023700                                                                          
023800 01  WS-TEMEMO-ESC-KORT-TAB.                                              
023900     03  FILLER      PIC X(09)  VALUE ' REPL    '.                        
024000     03  FILLER      PIC X(09)  VALUE ' 98 SCRAP'.                        
024100 01  FILLER   REDEFINES  WS-TEMEMO-ESC-KORT-TAB.                          
024200     03  WS-TEMEMO-ESC-KORT OCCURS 11 PIC X(09).                          
024300     EJECT                                                                
024400*    --- CLASSIC TRANS                                                    
024500*01 -COPY W21632            -PRE FILC-                                    
024600     EJECT                                                                
024700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
024800 01  GENERELLA-SUBPROGRAM.                                                
024900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
025000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
025100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
025200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
025300     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
025400     EJECT                                                                
025500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
025600*01 -COPY WMEDAREA                                                        
025700     SKIP3                                                                
025800 01  ERROR-TEXT.                                                          
025900     03  FILLER                  PIC X(8)    VALUE 'ERR-TXT'.             
026000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
026100                                                                          
026200 01  MESSAGE-CODES.                                                       
026300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
026400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
026500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
026600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
026700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
026800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
026900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
027000     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
027100     03  NO-AUTHOR-TO-SCRAP      PIC X(3)    VALUE '601'.                 
027200     03  NO-AUTHOR-TO-ATT        PIC X(3)    VALUE '602'.                 
027300     03  INF-HIGHER-LEV          PIC X(3)    VALUE '603'.                 
027400     03  ERR-RAD-FINNS-REDAN     PIC X(3)    VALUE '245'.                 
027500     EJECT                                                                
027600 01  PROG-TO-PROG-SW.                                                     
027700*    03  -COPY WMSGSOP                                                    
027800     EJECT                                                                
027900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
028000*                                                                         
028100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
028200     SKIP3                                                                
028300*01 -COPY WMSGINIT                                                        
028400     EJECT                                                                
028500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
028600*                                                                         
028700 01  SPAR-AREA.                                                           
028800     03  SPAR-IDTRANS                PIC X(4)  VALUE '6323'.              
028900     03  SPAR-IDDC-ENTER             PIC X(2)  VALUE SPACE.               
029000     03  SPAR-IDANSK-ENTER           PIC S9(3) VALUE ZERO COMP-3.         
029100     03  SPAR-IDARTNR-ENTER          PIC S9(9) VALUE ZERO COMP-3.         
029200     03  SPAR-DASKROT9-BEORD-ENTER   PIC 9(8)  VALUE ZERO.                
029300     03  SPAR-IDDC-NEXT              PIC X(2)  VALUE SPACE.               
029400     03  SPAR-IDANSK-NEXT            PIC S9(3) VALUE ZERO COMP-3.         
029500     03  SPAR-IDARTNR-NEXT           PIC S9(9) VALUE ZERO COMP-3.         
029600     03  SPAR-DASKROT9-BEORD-NEXT    PIC 9(8)  VALUE ZERO.                
029700     03  SPAR-KDARBTYP-ENTER         PIC X(8)  VALUE SPACE.               
029800     03  SPAR-KDARBTYP-NEXT          PIC X(8)  VALUE SPACE.               
029900     03  NEXT-IX                     PIC S9(3) VALUE ZERO COMP-3.         
030000     03  FILLER                      PIC X(1000) VALUE SPACE.             
030100     EJECT                                                                
030200                                                                          
030300 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
030400 01  P-TO-P-SW.                                                           
030500     03  P-TO-P-KVLL                PIC S9(4)           COMP SYNC.        
030600     03  P-TO-P-KDZ1                PIC X(1)  VALUE LOW-VALUE.            
030700     03  P-TO-P-KDZ2                PIC X(1)  VALUE LOW-VALUE.            
030800     03  P-TO-P-KDTRANS             PIC X(8).                             
030900     03  P-TO-P-IDTRANS             PIC X(4).                             
031000     03  P-TO-P-KDMFSFOR            PIC X(1).                             
031100     03  P-TO-P-DATA.                                                     
031200        05 FILLER                  PIC X(1000).                           
031300                                                                          
031400**********************************************************                
031500***   I N K Ö P S - P O S T   P V                                         
031600**********************************************************                
031700*                                                                         
031800*01  -COPY A310TB65                -PRE A310-                             
031900     EJECT                                                                
032000*01  AREA   -COPY W092W001     -PRE W092-.                                
032100     EJECT                                                                
032200******************************************************************        
032300*01  -COPY W6I32501   -PRE MOD6325-                                       
032400     EJECT                                                                
032500                                                                          
032600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
032700*                                                                         
032800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
032900     SKIP3                                                                
033000*01  MID -COPY W6I32301                                                   
033100     EJECT                                                                
033200*   -COPY WMSGMAIL                                                        
033300     EJECT                                                                
033400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
033500     SKIP3                                                                
033600*01  -COPY WMSGAREA                                                       
033700     EJECT                                                                
033800     03  MOD REDEFINES MSG-AREA.                                          
033900*      05  -COPY W6O32301                                                 
034000     EJECT                                                                
034100 01  FILLER              PIC X(16)  VALUE 'PROG-TO-PROG-SW'.              
034200                                                                          
034300 01  W-PROG-TO-PROG-SW.                                                   
034400     05  P-WS-LL         PIC S9(4)  VALUE +469 COMP SYNC.                 
034500     05  P-WS-Z1-Z2      PIC  X(2)  VALUE LOW-VALUE.                      
034600     05  KDTRANS-WS      PIC  X(8)  VALUE 'W1T113X '.                     
034700     05  FILLER          PIC  X(5)  VALUE '21293'.                        
034800     05  MID    -COPY W1I11301     -PRE PROGSW-                           
034900     EJECT                                                                
035000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
035100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
035200     SKIP3                                                                
035300*01  -COPY WMFSAREA                                                       
035400     EJECT                                                                
035500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
035600*                                                                         
035700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
035800     SKIP3                                                                
035900 01  NYCKLAR-TILL-DLI.                                                    
036000*                                                                         
036100     03  W-IDARTNR-MIN-X.                                                 
036200         05  W-IDARTNR-MIN   PIC S9(9)  VALUE ZERO COMP-3.                
036300     03  W-IDARTNR-MAX-X.                                                 
036400         05  W-IDARTNR-MAX   PIC S9(9)  VALUE +999999999 COMP-3.          
036500     03  W-IDDC-MIN-X.                                                    
036600         05  W-IDDC-MIN      PIC X(2)   VALUE LOW-VALUE.                  
036700     03  W-IDDC-MAX-X.                                                    
036800         05  W-IDDC-MAX      PIC X(2)   VALUE HIGH-VALUE.                 
036900     03  W-DASKROT9-MIN-X.                                                
037000         05  W-DASKROT9-MIN  PIC 9(8)   VALUE ZERO.                       
037100     03  W-DASKROT9-MAX-X.                                                
037200         05  W-DASKROT9-MAX  PIC 9(8)   VALUE 99999999.                   
037300     03  W-IDARTNR-X.                                                     
037400         05  W-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.                
037500     03  W-IDDC-X.                                                        
037600         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
037700     03  W-KDARBTYP-X.                                                    
037800         05  W-KDARBTYP      PIC X(8)   VALUE SPACE.                      
037900     03  W-DASKROT9-X.                                                    
038000         05  W-DASKROT9      PIC 9(8)   VALUE ZERO.                       
038100     03  W-IDDC-6324-X.                                                   
038200         05  W-IDDC-6324      PIC X(2)   VALUE SPACE.                     
038300     03  W-KDSTASKR-X.                                                    
038400         05  W-KDSTASKR       PIC S9     VALUE 2 COMP-3.                  
038500     03  W-WDGXKEY-6321.                                                  
038600         05  W-6321-IDHTYP    PIC X(4)   VALUE '6321'.                    
038700         05  W-6321-KDARBTYP  PIC X(8)   VALUE SPACE.                     
038800         05  W-6321-LOWVALUE  PIC X(18)  VALUE LOW-VALUE.                 
038900     03  W-KY6324-X.                                                      
039000         05  W-6324-IDARTNR   PIC S9(9)  VALUE ZERO COMP-3.               
039100         05  W-6324-IDDC      PIC X(2)   VALUE SPACE.                     
039200         05  W-6324-KDSTASKR  PIC S9     VALUE 2 COMP-3.                  
039300     03  W-WDGXKEY-6327.                                                  
039400         05  W-6327-IDHTYP    PIC X(4)   VALUE '6327'.                    
039500         05  W-6327-KDARBTYP  PIC X(8)   VALUE SPACE.                     
039600         05  W-6327-IDDC      PIC X(2)   VALUE SPACE.                     
039700         05  W-6327-LOWVALUE  PIC X(16)  VALUE LOW-VALUE.                 
039800     03  W-IDUSER-GODK-X.                                                 
039900         05  W-IDUSER-GODK    PIC X(8)   VALUE SPACE.                     
040000     03  W-SUBEL-MIN-X.                                                   
040100         05  W-SUBEL-MIN      PIC 9(7)  VALUE ZERO.                       
040200     03  W-SUBEL-MAX-X.                                                   
040300         05  W-SUBEL-MAX      PIC 9(7)  VALUE 9999999.                    
040400     03  W-TIDATETIME-MIN-X.                                              
040500         05  W-TIDATETIME-MIN PIC 9(14) VALUE ZERO.                       
040600     03  W-TIDATETIME-MAX-X.                                              
040700         05  W-TIDATETIME-MAX PIC 9(14) VALUE 99999999999999.             
040800     03 W-2401-KEY-X.                                                     
040900         05 FILLER               PIC X(4)    VALUE '2401'.                
041000         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
041100     03  W-KY6324-KVAL-X.                                                 
041200         05  W-IDARTNR-KVAL  PIC S9(9)  VALUE ZERO COMP-3.                
041300         05  W-IDDC-KVAL     PIC X(2)   VALUE SPACE.                      
041400         05  W-KDSTASKR-KVAL PIC S9     VALUE ZERO COMP-3.                
041500     03  W-1141-KEY-X.                                                    
041600         05 FILLER         PIC X(04)  VALUE '1141'.                       
041700         05 FILLER         PIC X(26)  VALUE LOW-VALUE.                    
041800     03  W-IDLEVNR-X.                                                     
041900         05 W-IDLEVNR      PIC X(5)   VALUE SPACE.                        
042000     03  W-WDG901KY-X.                                                    
042100         05  W-TIREGDAT          PIC S9(07)   VALUE ZERO COMP-3.          
042200         05  W-TIKLOCK           PIC S9(09)   VALUE ZERO COMP-3.          
042300                                                                          
042400     03  W-WDF201KY-X.                                                    
042500         05  W-IDLEVNR-WDF2      PIC  X(5)   VALUE SPACE.                 
042600         05  W-IDDIRGRP          PIC X(10)   VALUE SPACE.                 
042700                                                                          
042800     EJECT                                                                
042900*    --- STATUS-KOD FRÅN IMS                                              
043000 01  STATUS-WS                   PIC XX.                                  
043100     88  SEGMENT-FINNS                       VALUE '  '.                  
043200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
043300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
043400     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
043500     SKIP2                                                                
043600 01  GODK-STATUSKODER.                                                    
043700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
043800     SKIP3                                                                
043900 01  SSA1                        PIC X(256).                              
044000 01  SSA2                        PIC X(256).                              
044100 01  SSA3                        PIC X(256).                              
044200 01  SSA4                        PIC X(256).                              
044300     EJECT                                                                
044400*    --- IMS FUNKTIONSKODER                                               
044500*01  -COPY W0003                                                          
044600     EJECT                                                                
044700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
044800 01  DLI-IO-WLARTC01.                                                     
044900*    03  -COPY WDK601                                                     
045000     EJECT                                                                
045100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
045200 01  DLI-IO-WLARTC11.                                                     
045300*    03  -COPY WDK611                                                     
045400     EJECT                                                                
045500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC23'.                    
045600 01  DLI-IO-WLARTC23.                                                     
045700*    03  -COPY WDK623                                                     
045800     EJECT                                                                
045900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301  '.                    
046000 01  DLI-IO-WDT301.                                                       
046100*    03  -COPY WDT301                                                     
046200     EJECT                                                                
046300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311  '.                    
046400 01  DLI-IO-WDT311.                                                       
046500*    03  -COPY WDT311                                                     
046600     EJECT                                                                
046700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
046800 01  DLI-IO-WLARTS11.                                                     
046900*    03  -COPY WDK711                                                     
047000     EJECT                                                                
047100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
047200 01  DLI-IO-WDR501-6321.                                                  
047300*    03  -COPY WDGX6321                                                   
047400     EJECT                                                                
047500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
047600 01  DLI-IO-WDGX6322.                                                     
047700*    03  -COPY WDGX6322                                                   
047800     EJECT                                                                
047900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
048000 01  DLI-IO-WDGX6324.                                                     
048100*    03  -COPY WDGX6324                                                   
048200     EJECT                                                                
048300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6325'.                    
048400 01  DLI-IO-WDGX6325.                                                     
048500*    03  -COPY WDGX6325                                                   
048600     EJECT                                                                
048700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6326'.                    
048800 01  DLI-IO-WDGX6326.                                                     
048900*    03  -COPY WDGX6326                                                   
049000     EJECT                                                                
049100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6327'.                    
049200 01  DLI-IO-WDGX6327.                                                     
049300*    03  -COPY WDGX6327                                                   
049400     EJECT                                                                
049500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
049600 01  DLI-IO-WDGX6328.                                                     
049700*    03  -COPY WDGX6328                                                   
049800     EJECT                                                                
049900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-2401'.                 
050000 01  DLI-IO-WDR501-2401.                                                  
050100*    03  -COPY WDGX2402                                                   
050200     EJECT                                                                
050300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLFILC'.                      
050400 01  DLI-IO-AREA-FILC.                                                    
050500*    03  -COPY WDR301       -PRE FILC-                                    
050600     EJECT                                                                
050700                                                                          
050800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD801'.                      
050900 01  DLI-IO-WDD801.                                                       
051000*    03  -COPY WDD801                                                     
051100     EJECT                                                                
051200                                                                          
051300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD811'.                      
051400 01  DLI-IO-WDD811.                                                       
051500*    03  -COPY WDD811                                                     
051600     EJECT                                                                
051700                                                                          
051800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDJ901'.                      
051900 01  DLI-IO-WDJ901.                                                       
052000*    03  -COPY WDJ901                                                     
052100     EJECT                                                                
052200                                                                          
052300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDJ911'.                      
052400 01  DLI-IO-WDJ911.                                                       
052500*    03  -COPY WDJ911                                                     
052600     EJECT                                                                
052700 01  DLI-IO-AREA5.                                                        
052800     03  IO-AREA5              PIC X(150) VALUE SPACE.                    
052900                                                                          
053000*    03  ZZAC -COPY WDGZ01     -PRE ZZAC-  -RED IO-AREA5.                 
053100     EJECT                                                                
053200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDG901'.                      
053300 01  DLI-IO-WDG901.                                                       
053400*    03  -COPY WDG901                                                     
053500     EJECT                                                                
053600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF201'.                      
053700 01  DLI-IO-WDF201.                                                       
053800*    03  -COPY WDF201 -PRE WDF2-                                          
053900     EJECT                                                                
054000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF212'.                      
054100 01  DLI-IO-WDF212.                                                       
054200*    03  -COPY WDF212  -PRE  WDF2-                                        
054300     EJECT                                                                
054400                                                                          
054500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
054600 01  DLI-IO-WDB601.                                                       
054700*    03  -COPY WDB601                                                     
054800     EJECT                                                                
054900                                                                          
055000                                                                          
055100 LINKAGE SECTION.                                                         
055200*01  -COPY W0009   -PRE MSG-                                              
055300     EJECT                                                                
055400*01  -COPY W0009   -PRE ALT-                                              
055500     EJECT                                                                
055600*01  -COPY W0009   -PRE ALTMAIL-                                          
055700     EJECT                                                                
055800*01  -COPY W0009   -PRE ALT1-                                             
055900     EJECT                                                                
056000*01  -COPY W0009   -PRE ALT4-                                             
056100     EJECT                                                                
056200*01  -COPY W0008   -PRE USEA-                                             
056300     05  FILLER                  PIC X.                                   
056400     EJECT                                                                
056500*01  -COPY W0008   -PRE ARTC-                                             
056600     05  FILLER                  PIC X.                                   
056700     EJECT                                                                
056800*01  -COPY W0008   -PRE ARTS-                                             
056900     05  FILLER                  PIC X.                                   
057000     EJECT                                                                
057100*01  -COPY W0008   -PRE 6321-                                             
057200     05  FILLER                  PIC X.                                   
057300     EJECT                                                                
057400*01  -COPY W0008   -PRE 6327-                                             
057500     05  FILLER                  PIC X.                                   
057600     EJECT                                                                
057700*01  -COPY W0008   -PRE 2401-                                             
057800     05  FILLER                  PIC X.                                   
057900     EJECT                                                                
058000*01  -COPY W0008   -PRE FILC-                                             
058100     05  FILLER                  PIC X.                                   
058200     EJECT                                                                
058300*01  -COPY W0008  -PRE WDD8-                                              
058400     05  FILLER                  PIC X.                                   
058500     EJECT                                                                
058600*01  -COPY W0008  -PRE WDJ9-                                              
058700     05  FILLER                  PIC X.                                   
058800     EJECT                                                                
058900*01  -COPY W0008   -PRE 6326-                                             
059000     05  FILLER                  PIC X.                                   
059100     EJECT                                                                
059200*01  -COPY W0008  -PRE 6325-                                              
059300     05  FILLER                  PIC X.                                   
059400     EJECT                                                                
059500*01  -COPY W0008     -PRE ZZAC-                                           
059600         05  FILLER           PIC X.                                      
059700     EJECT                                                                
059800*01  -COPY W0008  -PRE ZZAD-                                              
059900     05  FILLER                  PIC X.                                   
060000     EJECT                                                                
060100*01  -COPY W0008  -PRE WDF2-                                              
060200     05  FILLER                  PIC X.                                   
060300     EJECT                                                                
060400*01  -COPY W0008  -PRE WDB6-                                              
060500     05  FILLER                  PIC X.                                   
060600     EJECT                                                                
060700*01  -COPY W0008  -PRE WDT3-                                              
060800     05  FILLER                  PIC X.                                   
060900     EJECT                                                                
061000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ALTMAIL-PCB                    
061100                           ALT1-PCB ALT4-PCB USEA-PCB                     
061200                           ARTC-PCB ARTS-PCB 6321-PCB 6327-PCB            
061300                           2401-PCB FILC-PCB WDD8-PCB WDJ9-PCB            
061400                           6326-PCB 6325-PCB ZZAC-PCB                     
061500                           ZZAD-PCB WDF2-PCB WDB6-PCB WDT3-PCB.           
061600                                                                          
061700 MAIN SECTION.                                                            
061800     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALTMAIL-PCB                    
061900                           ALT1-PCB ALT4-PCB USEA-PCB                     
062000                           ARTC-PCB ARTS-PCB 6321-PCB 6327-PCB            
062100                           2401-PCB FILC-PCB WDD8-PCB WDJ9-PCB            
062200                           6326-PCB 6325-PCB ZZAC-PCB                     
062300                           ZZAD-PCB WDF2-PCB WDB6-PCB WDT3-PCB.           
062400                                                                          
062500     PERFORM IMS-GET-MSG                                                  
062600     IF SEGMENT-FINNS                                                     
062700                                                                          
062800        PERFORM A-INIT                                                    
062900        PERFORM B-KOLLA-NYCKLAR                                           
063000                                                                          
063100        IF NYCKLAR-OK                                                     
063200          IF MFS-UPDATE                                                   
063300            PERFORM G-KOLLA-INPUT                                         
063400            IF INDATA-OK                                                  
063500              PERFORM H-UPPDATERA                                         
063600            END-IF                                                        
063700          ELSE                                                            
063800            IF MFS-FIRST                                                  
063900              PERFORM C-FOERSTA-SIDA                                      
064000            ELSE                                                          
064100              IF MFS-NEXT                                                 
064200                PERFORM D-NAESTA-SIDA                                     
064300              ELSE                                                        
064400                PERFORM E-SAMMA-SIDA                                      
064500              END-IF                                                      
064600            END-IF                                                        
064700         END-IF                                                           
064800         IF INDATA-OK                                                     
064900           PERFORM F-LAES-VISA-INFO                                       
065000         END-IF                                                           
065100       END-IF                                                             
065200                                                                          
065300*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
065400*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
065500       IF MFS-ENTER AND JUMP-6325                                         
065600         PERFORM S95-JUMP-6325                                            
065700       ELSE                                                               
065800         COMPUTE MSG-KVLL = LENGTH OF MOD-W6O32301 + 4                    
065900         PERFORM IMS-INSERT-MSG                                           
066000       END-IF                                                             
066100     END-IF                                                               
066200                                                                          
066300     MOVE ZERO TO RETURN-CODE                                             
066400     GOBACK                                                               
066500     .                                                                    
066600     EJECT                                                                
066700                                                                          
066800 A-INIT SECTION.                                                          
066900     IF MSG-DUBBLA-TRANSKODER                                             
067000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I32301                 
067100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
067200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
067300     ELSE                                                                 
067400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I32301                  
067500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
067600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
067700     END-IF                                                               
067800                                                                          
067900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
068000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
068100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
068200                                                                          
068300     MOVE LOW-VALUE TO MSG-AREA                                           
068400     MOVE 'W6O323N1' TO MFS-IDMOD                                         
068500     MOVE '6323' TO MOD-IDTRANS                                           
068600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
068700                                                                          
068800     IF EGEN-MID                                                          
068900       CONTINUE                                                           
069000     ELSE                                                                 
069100       MOVE SPACE TO MFS-KDTRTYP                                          
069200       MOVE '7'   TO MFS-IDPFK                                            
069300     END-IF                                                               
069400                                                                          
069500     ACCEPT DAGENS-DATUM FROM DATE                                        
069600     MOVE 'W6032300'      TO FILC-FIL-IDPGM                               
069700     MOVE DAGENS-DATUM    TO FILC-FIL-TIREGDAT                            
069800     MOVE 'W21632  '      TO FILC-FIL-IDCPYTXT                            
069900     MOVE ZERO            TO FILC-FIL-TIKLOCK                             
070000     MOVE NEJ             TO JUMP-6325-SW                                 
070100     ACCEPT POST-TIKLOCK FROM TIME                                        
070200     MOVE POST-TIKLOCK TO W-TIKLOCK                                       
070300                                                                          
070400*** READ WHOLE OF WDB6 AND FILL IN DC TABLE FOR KDDC = 'S' OR 'C'         
070500     INITIALIZE IDDC-TABELL                                               
070600     SET TAB-IX TO +1                                                     
070700     PERFORM IMS-GN-WDB601                                                
070800     PERFORM UNTIL SEGMENT-NOMORE                                         
070900        IF DCS-KDDC = 'S' OR 'C' OR 'NC' OR 'NA'                          
071000          MOVE DCS-IDDC     TO TAB-IDDC(TAB-IX)                           
071100          SET TAB-IX UP BY +1                                             
071200          IF TAB-IX > 100                                                 
071300            MOVE 'DC-TABELLEN FULL' TO ERROR-TEXT-STR                     
071400            DISPLAY ERROR-TEXT                                            
071500            CALL FELLOG                                                   
071600          END-IF                                                          
071700        END-IF                                                            
071800        PERFORM IMS-GN-WDB601                                             
071900     END-PERFORM                                                          
072000                                                                          
072100     SET TAB-IX TO +1                                                     
072200     .                                                                    
072300     EJECT                                                                
072400                                                                          
072500 B-KOLLA-NYCKLAR SECTION.                                                 
072600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
072700     MOVE '001'             TO MSGI-KDCALL                                
072800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
072900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
073000     MOVE '6323'            TO MSGI-IDTRANS                               
073100                                                                          
073200     IF GODK-MID                                                          
073300       MOVE NEJ TO KDARBTYP-SOEKNING                                      
073400                   IDDC-SOEKNING                                          
073500                   IDARTNR-SOEKNING                                       
073600                                                                          
073700       IF MID-KDARBTYP-IN  NOT = ALL '+'                                  
073800         MOVE JA TO KDARBTYP-SOEKNING                                     
073900         MOVE MID-KDARBTYP-IN TO MSGI-KDARBTYP                            
074000       ELSE                                                               
074100         IF MID-KDARBTYP-UT = SPACE OR LOW-VALUE                          
074200           CONTINUE                                                       
074300         ELSE                                                             
074400           MOVE JA TO KDARBTYP-SOEKNING                                   
074500           MOVE MID-KDARBTYP-UT TO MSGI-KDARBTYP                          
074600         END-IF                                                           
074700       END-IF                                                             
074800                                                                          
074900       IF MID-IDDC-IN NOT = ALL '+'                                       
075000         MOVE JA TO IDDC-SOEKNING                                         
075100         MOVE MID-IDDC-IN TO MSGI-IDDC-KEY                                
075200       ELSE                                                               
075300         MOVE MID-IDDC-UT    TO WS-IDDC                                   
075400         IF GOOD-DC OR MID-IDDC-UT = SPACE                                
075500         OR MID-IDDC-UT = ZERO                                            
075600           MOVE JA TO IDDC-SOEKNING                                       
075700           MOVE MID-IDDC-UT TO MSGI-IDDC-KEY                              
075800         END-IF                                                           
075900       END-IF                                                             
076000                                                                          
076100       IF MID-IDARTNR-IN NOT = ALL '+'                                    
076200         IF MID-IDARTNR-IN = SPACE OR LOW-VALUE                           
076300           MOVE NEJ TO IDARTNR-SOEKNING                                   
076400           MOVE ZERO           TO MSGI-IDARTNR                            
076500         ELSE                                                             
076600           INSPECT MID-IDARTNR-IN                                         
076700                     REPLACING LEADING SPACE BY ZERO                      
076800           IF MID-IDARTNR-IN = ZERO                                       
076900           OR MID-IDARTNR-IN NOT NUMERIC                                  
077000             MOVE NEJ TO IDARTNR-SOEKNING                                 
077100             MOVE ZERO           TO MSGI-IDARTNR                          
077200           ELSE                                                           
077300             MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                          
077400             MOVE JA TO IDARTNR-SOEKNING                                  
077500           END-IF                                                         
077600         END-IF                                                           
077700       ELSE                                                               
077800         IF MID-IDARTNR-UT = SPACE OR LOW-VALUE                           
077900         OR MID-IDARTNR-UT = ALL '+'                                      
078000           MOVE NEJ TO IDARTNR-SOEKNING                                   
078100           MOVE ZERO           TO MSGI-IDARTNR                            
078200         ELSE                                                             
078300           INSPECT MID-IDARTNR-UT                                         
078400                     REPLACING LEADING SPACE BY ZERO                      
078500           IF MID-IDARTNR-UT = ZERO                                       
078600           OR MID-IDARTNR-UT NOT NUMERIC                                  
078700             MOVE NEJ TO IDARTNR-SOEKNING                                 
078800             MOVE ZERO           TO MSGI-IDARTNR                          
078900           ELSE                                                           
079000             MOVE JA TO IDARTNR-SOEKNING                                  
079100             MOVE MID-IDARTNR-UT TO MSGI-IDARTNR                          
079200           END-IF                                                         
079300         END-IF                                                           
079400       END-IF                                                             
079500     END-IF                                                               
079600                                                                          
079700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
079800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
079900     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
080000                                                                          
080100     MOVE JA TO NYCKLAR-SW                                                
080200                                                                          
080300     MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-IN                              
080400     IF MID-KDARBTYP-IN NOT = ALL '+'                                     
080500       MOVE '7'         TO MFS-IDPFK                                      
080600       MOVE SPACE       TO MFS-KDTRTYP                                    
080700       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
080800     END-IF                                                               
080900                                                                          
081000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
081100     IF MID-IDDC-IN NOT = ALL '+'                                         
081200       MOVE '7'           TO MFS-IDPFK                                    
081300       MOVE SPACE         TO MFS-KDTRTYP                                  
081400       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
081500     END-IF                                                               
081600                                                                          
081700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
081800     IF MID-IDARTNR-IN NOT = ALL '+'                                      
081900       MOVE '7'         TO MFS-IDPFK                                      
082000       MOVE SPACE       TO MFS-KDTRTYP                                    
082100       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
082200     END-IF                                                               
082300                                                                          
082400     IF IDDC-SOEKNING = JA                                                
082500       MOVE MSGI-IDDC-KEY TO WS-IDDC                                      
082600       IF WS-IDDC = SPACE                                                 
082700         MOVE ZERO TO WS-IDDC                                             
082800       END-IF                                                             
082900       IF GOOD-DC                                                         
083000       OR WS-IDDC = ZERO                                                  
083100         MOVE WS-IDDC TO W-IDDC                                           
083200       ELSE                                                               
083300         MOVE NEJ TO NYCKLAR-SW                                           
083400       END-IF                                                             
083500     ELSE                                                                 
083600       MOVE MSGI-IDDC TO W-IDDC                                           
083700                         WS-IDDC                                          
083800     END-IF                                                               
083900                                                                          
084000     IF KDARBTYP-SOEKNING = JA                                            
084100       IF MSGI-KDARBTYP = 'ANSK' OR 'ESC' OR 'QUAL'                       
084200         IF MSGI-KDARBTYP = 'ANSK'                                        
084300           MOVE W-IDDC   TO SW-WS-IDDC                                    
084400           IF SW-CDC-SE OR SW-NDC-CN                                      
084500             CONTINUE                                                     
084600           ELSE                                                           
084700             MOVE NEJ TO NYCKLAR-SW                                       
084800           END-IF                                                         
084900         ELSE                                                             
085000           CONTINUE                                                       
085100         END-IF                                                           
085200         MOVE MSGI-KDARBTYP TO W-KDARBTYP                                 
085300                               W-6327-KDARBTYP                            
085400                               W-6321-KDARBTYP                            
085500       ELSE                                                               
085600         MOVE NEJ TO NYCKLAR-SW                                           
085700       END-IF                                                             
085800     ELSE                                                                 
085900       MOVE NEJ TO NYCKLAR-SW                                             
086000     END-IF                                                               
086100                                                                          
086200     IF IDARTNR-SOEKNING = JA                                             
086300       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
086400     END-IF                                                               
086500                                                                          
086600     IF GODK-MID OR NYCKLAR-OK                                            
086700       MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-UT                            
086800                               MOD-IDDC-UT                                
086900       IF KDARBTYP-SOEKNING = JA                                          
087000         MOVE MSGI-KDARBTYP TO MOD-KDARBTYP-UT                            
087100       END-IF                                                             
087200       IF IDARTNR-SOEKNING = JA                                           
087300         IF MSGI-IDARTNR = ZERO                                           
087400           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                         
087500           MOVE NEJ TO IDARTNR-SOEKNING                                   
087600         ELSE                                                             
087700           MOVE MSGI-IDARTNR  TO MOD-IDARTNR-UT                           
087800         END-IF                                                           
087900       ELSE                                                               
088000         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                           
088100       END-IF                                                             
088200       MOVE WS-IDDC         TO MOD-IDDC-UT                                
088300     ELSE                                                                 
088400       MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-UT                            
088500                               MOD-IDDC-UT                                
088600                               MOD-IDARTNR-UT                             
088700     END-IF                                                               
088800                                                                          
088900     IF NYCKLAR-FEL                                                       
089000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
089100       CALL WMEDKONV USING MED-WMEDAREA                                   
089200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
089300       PERFORM MFS-RENSA-FAELT-IN                                         
089400       PERFORM MFS-RENSA-FAELT-UT                                         
089500       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
089600     END-IF                                                               
089700     .                                                                    
089800     EJECT                                                                
089900                                                                          
090000 C-FOERSTA-SIDA SECTION.                                                  
090100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
090200     CALL WMEDKONV USING MED-WMEDAREA                                     
090300     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
090400     PERFORM MFS-RENSA-FAELT-IN                                           
090500     .                                                                    
090600     EJECT                                                                
090700                                                                          
090800 D-NAESTA-SIDA SECTION.                                                   
090900     IF SPAR-IDTRANS = '6323'                                             
091000       MOVE SPAR-IDDC-NEXT   TO SW-WS-IDDC                                
091100       IF SPAR-IDARTNR-NEXT NUMERIC                                       
091200       AND SPAR-IDANSK-NEXT NUMERIC                                       
091300       AND SPAR-DASKROT9-BEORD-NEXT NUMERIC                               
091400       AND SW-GOOD-DC                                                     
091500         IF SPAR-KDARBTYP-NEXT = 'ANSK' OR 'ESC' OR 'QUAL'                
091600           MOVE SPAR-IDARTNR-NEXT        TO W-IDARTNR-MIN                 
091700                                            W-IDARTNR                     
091800           MOVE SPAR-DASKROT9-BEORD-NEXT TO W-DASKROT9-MIN                
091900           MOVE SPAR-IDDC-NEXT           TO W-IDDC-MIN                    
092000                                            W-IDDC                        
092100           MOVE SPAR-KDARBTYP-NEXT       TO W-KDARBTYP                    
092200                                            W-6327-KDARBTYP               
092300           SET  TAB-IX                   TO NEXT-IX                       
092400         ELSE                                                             
092500           PERFORM MFS-RENSA-FAELT-IN                                     
092600         END-IF                                                           
092700       ELSE                                                               
092800         PERFORM MFS-RENSA-FAELT-IN                                       
092900       END-IF                                                             
093000     ELSE                                                                 
093100       PERFORM MFS-RENSA-FAELT-IN                                         
093200     END-IF                                                               
093300     .                                                                    
093400     EJECT                                                                
093500                                                                          
093600 E-SAMMA-SIDA SECTION.                                                    
093700     IF SPAR-IDTRANS = '6323'                                             
093800       MOVE SPAR-IDDC-ENTER  TO SW-WS-IDDC                                
093900       IF SPAR-IDARTNR-ENTER NUMERIC                                      
094000       AND SPAR-IDANSK-ENTER NUMERIC                                      
094100       AND SPAR-DASKROT9-BEORD-ENTER NUMERIC                              
094200       AND GOOD-DC                                                        
094300         IF SPAR-KDARBTYP-ENTER = 'ANSK' OR 'ESC' OR 'QUAL'               
094400           MOVE SPAR-IDARTNR-ENTER        TO W-IDARTNR-MIN                
094500                                             W-IDARTNR                    
094600           MOVE SPAR-IDDC-ENTER           TO W-IDDC-MIN                   
094700                                             W-IDDC                       
094800           MOVE SPAR-DASKROT9-BEORD-ENTER TO W-DASKROT9-MIN               
094900           MOVE SPAR-KDARBTYP-ENTER       TO W-KDARBTYP                   
095000                                             W-6327-KDARBTYP              
095100         ELSE                                                             
095200           PERFORM MFS-RENSA-FAELT-IN                                     
095300         END-IF                                                           
095400       ELSE                                                               
095500         PERFORM MFS-RENSA-FAELT-IN                                       
095600       END-IF                                                             
095700     ELSE                                                                 
095800       PERFORM MFS-RENSA-FAELT-IN                                         
095900     END-IF                                                               
096000                                                                          
096100     IF SPAR-IDTRANS = '6323'                                             
096200       IF MID-CMD(1)  = ALL '+'                                           
096300       AND MID-CMD(2) = ALL '+'                                           
096400       AND MID-CMD(3) = ALL '+'                                           
096500       AND MID-CMD(4) = ALL '+'                                           
096600       AND MID-CMD(5) = ALL '+'                                           
096700       AND MID-CMD(6) = ALL '+'                                           
096800       AND MID-CMD(7) = ALL '+'                                           
096900       AND MID-CMD(8) = ALL '+'                                           
097000       AND MID-CMD(9) = ALL '+'                                           
097100       AND MID-CMD(10)= ALL '+'                                           
097200       AND MID-CMD(11)= ALL '+'                                           
097300       AND MID-CMD(12)= ALL '+'                                           
097400       AND MID-FLKLAR = ALL '+'                                           
097500         PERFORM MFS-RENSA-FAELT-IN                                       
097600       ELSE                                                               
097700         MOVE +1 TO RAD-IX                                                
097800         PERFORM UNTIL RAD-IX > 12                                        
097900           IF MID-CMD(RAD-IX) = 'T' OR 'O'                                
098000             IF MID-CMD(RAD-IX) = 'T'                                     
098100                MOVE MID-IDARTNR(RAD-IX) TO                               
098200                                   MOD6325-MID-IDARTNR-IN                 
098300                MOVE MID-IDDC(RAD-IX)    TO                               
098400                                   MOD6325-MID-IDDC-IN                    
098500                IF JUMP-6325                                              
098600                  MOVE MFS-ALFA-FAELT-FEL                                 
098700                                         TO MOD-CMD-ATTR(RAD-IX)          
098800                  MOVE NEJ               TO JUMP-6325-SW                  
098900                ELSE                                                      
099000                  MOVE JA                TO JUMP-6325-SW                  
099100                END-IF                                                    
099200             ELSE                                                         
099300                MOVE RAD-IX          TO O-IX                              
099400                INSPECT MID-IDARTNR(O-IX)                                 
099500                   REPLACING LEADING SPACE BY ZERO                        
099600                IF MID-IDARTNR(O-IX) NUMERIC                              
099700                   MOVE JA                TO SW-VISA-6325                 
099800                   MOVE MID-IDDC(O-IX)    TO WO-IDDC-6325                 
099900                   MOVE MID-IDARTNR(O-IX) TO WO-IDARTNR                   
100000                   MOVE MID-TIDATUM(O-IX) TO WS-DASKROT-AAMMDD            
100100                   MOVE 20                TO WS-DASKROT-SS                
100200                   COMPUTE WO-DASKROT9 =                                  
100300                      99999999 - WS-AAAAMMDD                              
100400                END-IF                                                    
100500             END-IF                                                       
100600           ELSE                                                           
100700             IF MID-CMD(RAD-IX) = ALL '+'                                 
100800               CONTINUE                                                   
100900             ELSE                                                         
101000               MOVE NEJ           TO INDATA-SW                            
101100             END-IF                                                       
101200           END-IF                                                         
101300           ADD +1 TO RAD-IX                                               
101400         END-PERFORM                                                      
101500         IF NO-JUMP AND INDATA-FEL                                        
101600           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
101700           CALL WMEDKONV USING MED-WMEDAREA                               
101800           MOVE MED-MFSINF TO MOD-TEMFSFEL                                
101900           PERFORM EA-MID-INDATA-TILL-MOD                                 
102000         END-IF                                                           
102100       END-IF                                                             
102200     ELSE                                                                 
102300        PERFORM MFS-RENSA-FAELT-IN                                        
102400     END-IF                                                               
102500     .                                                                    
102600     EJECT                                                                
102700                                                                          
102800 EA-MID-INDATA-TILL-MOD SECTION.                                          
102900     IF MID-FLKLAR NOT = ALL '+'                                          
103000       MOVE MFS-ROER-EJ-FAELT TO MOD-FLKLAR                               
103100     ELSE                                                                 
103200       MOVE MFS-RENSA-FAELT   TO MOD-FLKLAR                               
103300     END-IF                                                               
103400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKLAR-ATTR                        
103500                                                                          
103600     MOVE +1 TO RAD-IX                                                    
103700     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
103800       IF MID-CMD(RAD-IX) NOT = ALL '+'                                   
103900         MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-IN(RAD-IX)                     
104000       ELSE                                                               
104100         MOVE MFS-RENSA-FAELT   TO MOD-CMD-IN(RAD-IX)                     
104200       END-IF                                                             
104300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR(RAD-IX)                 
104400       ADD +1 TO RAD-IX                                                   
104500     END-PERFORM                                                          
104600     .                                                                    
104700     EJECT                                                                
104800                                                                          
104900 F-LAES-VISA-INFO SECTION.                                                
105000     IF WS-IDDC = ZERO                                                    
105100       PERFORM FD-KOLLA-IDDC                                              
105200     END-IF                                                               
105300                                                                          
105400     PERFORM S11-LAES-GRUNDDATA                                           
105500     MOVE +1 TO RAD-IX                                                    
105600                                                                          
105700     IF SEGMENT-SAKNAS                                                    
105800       IF MED-IDMFSINF = INF-UPDATE-DONE                                  
105900         CONTINUE                                                         
106000       ELSE                                                               
106100         MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                            
106200       END-IF                                                             
106300       CALL WMEDKONV USING MED-WMEDAREA                                   
106400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
106500       PERFORM MFS-RENSA-FAELT-UT                                         
106600     ELSE                                                                 
106700       MOVE 6321-KDARBTYP TO SPAR-KDARBTYP-ENTER                          
106800                                                                          
106900       PERFORM S12-LAES-SKROTDATUM                                        
107000       IF SEGMENT-FINNS                                                   
107100         COMPUTE WS-DASKROT9 = 99999999 - 6322-DASKROT9-BEORD             
107200         MOVE 6322-DASKROT9-BEORD                                         
107300                          TO SPAR-DASKROT9-BEORD-ENTER                    
107400         PERFORM S13-LAES-RADDATA                                         
107500         MOVE 6324-IDDC TO SPAR-IDDC-ENTER                                
107600         IF SEGMENT-SAKNAS                                                
107700           PERFORM FE-SOEK-FORSTA-ART                                     
107800           IF SEGMENT-SAKNAS                                              
107900             MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                        
108000             CALL WMEDKONV USING MED-WMEDAREA                             
108100             MOVE MED-MFSINF TO MOD-TEMFSINF                              
108200             PERFORM MFS-RENSA-FAELT-UT                                   
108300           END-IF                                                         
108400         END-IF                                                           
108500       ELSE                                                               
108600         IF WS-IDDC = ZERO                                                
108700           PERFORM FG-SOEK-FORSTA-DATUM                                   
108800           IF SEGMENT-SAKNAS                                              
108900             MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                        
109000             CALL WMEDKONV USING MED-WMEDAREA                             
109100             MOVE MED-MFSINF TO MOD-TEMFSINF                              
109200             PERFORM MFS-RENSA-FAELT-UT                                   
109300           END-IF                                                         
109400         ELSE                                                             
109500           MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                          
109600           CALL WMEDKONV USING MED-WMEDAREA                               
109700           MOVE MED-MFSINF TO MOD-TEMFSINF                                
109800           PERFORM MFS-RENSA-FAELT-UT                                     
109900         END-IF                                                           
110000       END-IF                                                             
110100     END-IF                                                               
110200                                                                          
110300     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
110400       IF SEGMENT-FINNS                                                   
110500         IF 6324-FLSKROT-GODK = 'J'                                       
110600           ADD -1 TO RAD-IX                                               
110700           MOVE NEJ TO WS-FLLAS-6326                                      
110800         ELSE                                                             
110900           MOVE JA  TO WS-FLLAS-6326                                      
111000**** SAVE KEYS ****                                                       
111100           IF RAD-IX = 1                                                  
111200             MOVE 6322-DASKROT9-BEORD                                     
111300                      TO SPAR-DASKROT9-BEORD-ENTER                        
111400             MOVE 6324-IDDC     TO SPAR-IDDC-ENTER                        
111500             MOVE 6324-IDARTNR  TO SPAR-IDARTNR-ENTER                     
111600           END-IF                                                         
111700           MOVE 6324-IDARTNR       TO MOD-IDARTNR(RAD-IX)                 
111800           MOVE WS-DASKROT9        TO MOD-TIDATUM(RAD-IX)                 
111900           MOVE 6324-IDARTNR       TO MOD-IDARTNR(RAD-IX)                 
112000                                      W-IDARTNR                           
112100                                      W-IDARTNR-KVAL                      
112200           MOVE 6324-IDDC          TO MOD-IDDC(RAD-IX)                    
112300                                      W-IDDC-KVAL                         
112400           MOVE 6324-IDUSER        TO MOD-IDUSER(RAD-IX)                  
112500           MOVE 6324-BEANST        TO MOD-BEANST(RAD-IX)                  
112600           MOVE 6324-KVSKROT-BEORD TO MOD-KVANTAL(RAD-IX)                 
112700                                                                          
112800           IF 6324-IDDISTR = 81                                           
112900           AND 6324-IDKUNDNR = 111                                        
113000             MOVE 'Y'              TO MOD-FLCLASS(RAD-IX)                 
113100           END-IF                                                         
113200                                                                          
113300           MOVE 2                  TO W-KDSTASKR-KVAL                     
113400                                                                          
113500           MOVE 6322-DASKROT9-BEORD TO W-DASKROT9                         
113600                                                                          
113700           PERFORM IMS-GU-WDGX6324-6325                                   
113800           IF SEGMENT-FINNS                                               
113900             PERFORM IMS-GNP-WDGX6325                                     
114000             IF SEGMENT-FINNS                                             
114100               MOVE 'Y'            TO MOD-FLTEXT(RAD-IX)                  
114200             END-IF                                                       
114300           END-IF                                                         
114400                                                                          
114500           MOVE W-IDDC    TO SPAR-IDDC                                    
114600           MOVE 6324-IDDC TO  SW-WS-IDDC                                  
114700           IF SW-NDC-NA OR SW-NDC-CN OR SW-LDC-CN                         
114800             PERFORM IMS-GHU-ARTS11                                       
114900             IF SEGMENT-FINNS                                             
115000               COMPUTE WS-SUARTSTD                                        
115100                    = 6324-KVSKROT-BEORD * SLAG-PRAVCOST                  
115200               MOVE WS-SUARTSTD     TO MOD-SUBEL(RAD-IX)                  
115300             ELSE                                                         
115400               MOVE MFS-RENSA-FAELT TO MOD-SUBEL(RAD-IX)                  
115500             END-IF                                                       
115600             MOVE SPAR-IDDC TO W-IDDC                                     
115700           ELSE                                                           
115800             PERFORM IMS-GHU-ARTC11                                       
115900             IF SEGMENT-FINNS                                             
116000               COMPUTE WS-SUARTSTD                                        
116100                    = 6324-KVSKROT-BEORD * CLAG-PRARTSTD                  
116200               MOVE WS-SUARTSTD    TO MOD-SUBEL(RAD-IX)                   
116300             ELSE                                                         
116400               MOVE MFS-RENSA-FAELT TO MOD-SUBEL(RAD-IX)                  
116500             END-IF                                                       
116600           END-IF                                                         
116700         END-IF                                                           
116800                                                                          
116900         IF WS-FLLAS-6326 = JA                                            
117000           MOVE NEJ TO SW-6326-OK                                         
117100           PERFORM IMS-GU-WDGX6324-2                                      
117200           IF SEGMENT-FINNS                                               
117300             PERFORM IMS-GNP-WDGX6326-2                                   
117400             IF SEGMENT-FINNS                                             
117500               MOVE +1 TO WS-IX-NEXT                                      
117600               ADD  +1 TO RAD-IX                                          
117700             END-IF                                                       
117800           END-IF                                                         
117900                                                                          
118000           PERFORM UNTIL SEGMENT-SAKNAS OR RAD-IX > RAD-IX-MAX            
118100             MOVE 6326-IDUSER-GODK     TO MOD-IDUSER(RAD-IX)              
118200             MOVE 6326-IDUSER-GODK     TO WS-IDUSER                       
118300             MOVE 6326-BEANST-GODK     TO MOD-BEANST(RAD-IX)              
118400             MOVE 6326-TIDATETIME(3:6) TO MOD-TIDATUM(RAD-IX)             
118500             MOVE MFS-STAENG-FAELT     TO MOD-CMD-ATTR(RAD-IX)            
118600             MOVE MFS-RENSA-FAELT      TO MOD-CMD-IN(RAD-IX)              
118700             PERFORM IMS-GNP-WDGX6326-2                                   
118800             IF SEGMENT-FINNS                                             
118900               ADD +1 TO RAD-IX                                           
119000               ADD +1 TO WS-IX-NEXT                                       
119100             END-IF                                                       
119200           END-PERFORM                                                    
119300                                                                          
119400           IF SEGMENT-FINNS AND RAD-IX > RAD-IX-MAX                       
119500             PERFORM UNTIL WS-IX-NEXT = ZERO                              
119600             OR WS-IX-NEXT < ZERO                                         
119700               ADD -1 TO RAD-IX                                           
119800               MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR(RAD-IX)           
119900               MOVE MFS-RENSA-FAELT      TO MOD-KVANTAL(RAD-IX)           
120000               MOVE MFS-RENSA-FAELT      TO MOD-SUBEL(RAD-IX)             
120100               MOVE MFS-RENSA-FAELT      TO MOD-IDDC(RAD-IX)              
120200               MOVE MFS-RENSA-FAELT      TO MOD-SUBEL(RAD-IX)             
120300               MOVE MFS-RENSA-FAELT      TO MOD-FLCLASS(RAD-IX)           
120400               MOVE MFS-RENSA-FAELT      TO MOD-FLTEXT(RAD-IX)            
120500               MOVE MFS-RENSA-FAELT      TO MOD-IDUSER(RAD-IX)            
120600               MOVE MFS-RENSA-FAELT      TO MOD-BEANST(RAD-IX)            
120700               MOVE MFS-RENSA-FAELT      TO MOD-TIDATUM(RAD-IX)           
120800               MOVE MFS-RENSA-FAELT      TO MOD-CMD-IN(RAD-IX)            
120900               MOVE MFS-STAENG-FAELT     TO MOD-CMD-ATTR(RAD-IX)          
121000               ADD -1 TO WS-IX-NEXT                                       
121100             END-PERFORM                                                  
121200             ADD +13 TO RAD-IX                                            
121300           END-IF                                                         
121400         END-IF                                                           
121500                                                                          
121600         IF SEGMENT-SAKNAS OR WS-FLLAS-6326 = NEJ                         
121700           PERFORM S13-LAES-RADDATA                                       
121800           IF SEGMENT-FINNS                                               
121900             CONTINUE                                                     
122000           ELSE                                                           
122100             PERFORM FF-SOEK-NASTA-ART                                    
122200           END-IF                                                         
122300         END-IF                                                           
122400       ELSE                                                               
122500         IF RAD-IX = 1                                                    
122600           MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                          
122700           CALL WMEDKONV USING MED-WMEDAREA                               
122800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
122900         END-IF                                                           
123000         MOVE MFS-RENSA-FAELT  TO MOD-CMD-IN(RAD-IX)                      
123100                                  MOD-IDARTNR(RAD-IX)                     
123200                                  MOD-KVANTAL(RAD-IX)                     
123300                                  MOD-SUBEL(RAD-IX)                       
123400                                  MOD-TIDATUM(RAD-IX)                     
123500                                  MOD-IDDC(RAD-IX)                        
123600                                  MOD-IDUSER(RAD-IX)                      
123700                                  MOD-BEANST(RAD-IX)                      
123800         MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR(RAD-IX)                    
123900         MOVE MFS-RENSA-FAELT  TO MOD-CMD-IN(RAD-IX)                      
124000       END-IF                                                             
124100       ADD 1 TO RAD-IX                                                    
124200     END-PERFORM                                                          
124300                                                                          
124400     IF SEGMENT-FINNS AND 6324-FLSKROT-GODK = 'N'                         
124500        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSFEL                         
124600        CALL WMEDKONV USING MED-WMEDAREA                                  
124700        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
124800        MOVE 6324-IDDC           TO SPAR-IDDC-NEXT                        
124900        MOVE 6321-KDARBTYP       TO SPAR-KDARBTYP-NEXT                    
125000        MOVE 6322-DASKROT9-BEORD TO SPAR-DASKROT9-BEORD-NEXT              
125100        MOVE 6324-IDARTNR        TO SPAR-IDARTNR-NEXT                     
125200        SET  NEXT-IX             TO TAB-IX                                
125300     ELSE                                                                 
125400        MOVE MFS-RENSA-FAELT TO  SPAR-IDDC-NEXT                           
125500                                 SPAR-KDARBTYP-NEXT                       
125600                                 SPAR-DASKROT9-BEORD-NEXT                 
125700                                 SPAR-IDARTNR-NEXT                        
125800                                 SPAR-IDANSK-NEXT                         
125900     END-IF                                                               
126000                                                                          
126100     IF SW-VISA-6325 = JA                                                 
126200       MOVE WO-IDDC-6325    TO SW-WS-IDDC                                 
126300       IF SW-NDC-CN AND MSGI-KDARBTYP = 'ANSK'                            
126400         CONTINUE                                                         
126500       ELSE                                                               
126600         IF MSGI-KDARBTYP = 'ANSK'                                        
126700           MOVE 'ANSK'      TO W-6321-KDARBTYP                            
126800           MOVE '11'        TO W-IDDC-KVAL                                
126900         ELSE                                                             
127000           IF MSGI-KDARBTYP = 'ESC'                                       
127100             MOVE 'ESC'     TO W-6321-KDARBTYP                            
127200             MOVE MSGI-IDDC-KEY                                           
127300                            TO W-IDDC-KVAL                                
127400           END-IF                                                         
127500         END-IF                                                           
127600*                                                                         
127700         MOVE WO-DASKROT9   TO W-DASKROT9                                 
127800         MOVE WO-IDARTNR    TO W-IDARTNR-KVAL                             
127900         MOVE 2             TO W-KDSTASKR-KVAL                            
128000         PERFORM IMS-GU-WDGX6324-6325                                     
128100         IF SEGMENT-FINNS                                                 
128200            MOVE SPACE TO WO-TEMFSINF-TAB                                 
128300            MOVE +1    TO WO-IX                                           
128400            PERFORM IMS-GNP-WDGX6325                                      
128500            PERFORM UNTIL SEGMENT-SAKNAS                                  
128600               IF MSGI-KDARBTYP   = 'ANSK'                                
128700                 PERFORM FH-ORSAKSTEXTER                                  
128800               ELSE                                                       
128900                 IF MSGI-KDARBTYP = 'ESC'                                 
129000                   PERFORM FI-ORSAKSTEXTER                                
129100                 END-IF                                                   
129200               END-IF                                                     
129300               PERFORM IMS-GNP-WDGX6325                                   
129400            END-PERFORM                                                   
129500            MOVE WO-TEMFSINF-TAB TO MOD-TEMFSINF                          
129600         END-IF                                                           
129700       END-IF                                                             
129800     END-IF                                                               
129900                                                                          
130000     MOVE '002'      TO MSGI-KDCALL                                       
130100     MOVE '6323'     TO SPAR-IDTRANS                                      
130200     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
130300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
130400     .                                                                    
130500     EJECT                                                                
130600                                                                          
130700 FD-KOLLA-IDDC SECTION.                                                   
130800                                                                          
130900     MOVE W-IDDC    TO SW-WS-IDDC                                         
131000     .                                                                    
131100     EJECT                                                                
131200                                                                          
131300 FE-SOEK-FORSTA-ART SECTION.                                              
131400     IF WS-IDDC = ZERO                                                    
131500        MOVE NEJ TO WS-SKROTDATUM-SLUT                                    
131600        PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                             
131700           PERFORM S12-LAES-SKROTDATUM                                    
131800           IF SEGMENT-FINNS                                               
131900              COMPUTE WS-DASKROT9 =                                       
132000                  99999999 - 6322-DASKROT9-BEORD                          
132100              END-COMPUTE                                                 
132200              MOVE 6322-DASKROT9-BEORD                                    
132300                  TO SPAR-DASKROT9-BEORD-ENTER                            
132400              MOVE ZERO TO W-IDARTNR-MIN                                  
132500              PERFORM S13-LAES-RADDATA                                    
132600              IF SEGMENT-FINNS                                            
132700                 MOVE JA TO WS-SKROTDATUM-SLUT                            
132800              END-IF                                                      
132900           ELSE                                                           
133000              SET TAB-IX UP BY +1                                         
133100              PERFORM S11-LAES-GRUNDDATA                                  
133200              IF SEGMENT-FINNS                                            
133300                MOVE ZERO TO W-DASKROT9                                   
133400                             W-DASKROT9-MIN                               
133500              ELSE                                                        
133600                MOVE JA TO WS-SKROTDATUM-SLUT                             
133700              END-IF                                                      
133800           END-IF                                                         
133900        END-PERFORM                                                       
134000     ELSE                                                                 
134100        MOVE NEJ TO WS-SKROTDATUM-SLUT                                    
134200        PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                             
134300           PERFORM S12-LAES-SKROTDATUM                                    
134400           IF SEGMENT-FINNS                                               
134500              COMPUTE WS-DASKROT9 =                                       
134600                  99999999 - 6322-DASKROT9-BEORD                          
134700              END-COMPUTE                                                 
134800              MOVE 6322-DASKROT9-BEORD                                    
134900                  TO SPAR-DASKROT9-BEORD-ENTER                            
135000              MOVE ZERO TO W-IDARTNR-MIN                                  
135100              PERFORM S13-LAES-RADDATA                                    
135200              IF SEGMENT-FINNS                                            
135300                 MOVE JA TO WS-SKROTDATUM-SLUT                            
135400              END-IF                                                      
135500           ELSE                                                           
135600              MOVE JA TO WS-SKROTDATUM-SLUT                               
135700           END-IF                                                         
135800       END-PERFORM                                                        
135900     END-IF                                                               
136000     .                                                                    
136100     EJECT                                                                
136200                                                                          
136300 FF-SOEK-NASTA-ART SECTION.                                               
136400     IF WS-IDDC = ZERO                                                    
136500        MOVE NEJ TO WS-SKROTDATUM-SLUT                                    
136600                    WS-ART-SLUT                                           
136700        PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                             
136800           PERFORM S12-LAES-SKROTDATUM                                    
136900           IF SEGMENT-FINNS                                               
137000              COMPUTE WS-DASKROT9 =                                       
137100                  99999999 - 6322-DASKROT9-BEORD                          
137200              END-COMPUTE                                                 
137300              MOVE ZERO TO W-IDARTNR-MIN                                  
137400              PERFORM S13-LAES-RADDATA                                    
137500              PERFORM UNTIL SEGMENT-SAKNAS OR (WS-ART-SLUT = JA)          
137600                 IF SEGMENT-FINNS                                         
137700                    IF 6324-FLSKROT-GODK = 'N'                            
137800                       MOVE JA TO WS-SKROTDATUM-SLUT                      
137900                                  WS-ART-SLUT                             
138000                    ELSE                                                  
138100                       PERFORM S13-LAES-RADDATA                           
138200                    END-IF                                                
138300                 ELSE                                                     
138400                    MOVE JA TO WS-ART-SLUT                                
138500                 END-IF                                                   
138600              END-PERFORM                                                 
138700           ELSE                                                           
138800              SET TAB-IX UP BY +1                                         
138900              PERFORM S11-LAES-GRUNDDATA                                  
139000              IF SEGMENT-FINNS                                            
139100                  MOVE ZERO TO W-DASKROT9                                 
139200                               W-DASKROT9-MIN                             
139300              ELSE                                                        
139400                  MOVE JA TO WS-SKROTDATUM-SLUT                           
139500              END-IF                                                      
139600           END-IF                                                         
139700       END-PERFORM                                                        
139800     ELSE                                                                 
139900        MOVE NEJ TO WS-SKROTDATUM-SLUT                                    
140000                    WS-ART-SLUT                                           
140100        PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                             
140200           PERFORM S12-LAES-SKROTDATUM                                    
140300           IF SEGMENT-FINNS                                               
140400              COMPUTE WS-DASKROT9 =                                       
140500                  99999999 - 6322-DASKROT9-BEORD                          
140600              END-COMPUTE                                                 
140700              MOVE ZERO TO W-IDARTNR-MIN                                  
140800              PERFORM S13-LAES-RADDATA                                    
140900              PERFORM UNTIL SEGMENT-SAKNAS OR (WS-ART-SLUT = JA)          
141000                 IF SEGMENT-FINNS                                         
141100                    IF 6324-FLSKROT-GODK = 'N'                            
141200                       MOVE JA TO WS-SKROTDATUM-SLUT                      
141300                                  WS-ART-SLUT                             
141400                    ELSE                                                  
141500                       PERFORM S13-LAES-RADDATA                           
141600                    END-IF                                                
141700                 ELSE                                                     
141800                    MOVE JA TO WS-ART-SLUT                                
141900                 END-IF                                                   
142000              END-PERFORM                                                 
142100           ELSE                                                           
142200              MOVE JA TO WS-SKROTDATUM-SLUT                               
142300           END-IF                                                         
142400       END-PERFORM                                                        
142500     END-IF                                                               
142600     .                                                                    
142700     EJECT                                                                
142800                                                                          
142900 FG-SOEK-FORSTA-DATUM SECTION.                                            
143000     SET TAB-IX UP BY +1                                                  
143100     MOVE NEJ TO WS-DATUM-HITTAD                                          
143200     PERFORM UNTIL WS-DATUM-HITTAD = JA  OR TAB-IX > TAB-IX-MAX           
143300                   OR TAB-IDDC(TAB-IX) = SPACE                            
143400        PERFORM S11-LAES-GRUNDDATA                                        
143500        IF SEGMENT-FINNS                                                  
143600           MOVE NEJ TO WS-SKROTDATUM-SLUT                                 
143700           PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                          
143800              MOVE ZERO TO W-DASKROT9-MIN                                 
143900                           W-DASKROT9                                     
144000              PERFORM S12-LAES-SKROTDATUM                                 
144100              IF SEGMENT-FINNS                                            
144200                 COMPUTE WS-DASKROT9 =                                    
144300                     99999999 - 6322-DASKROT9-BEORD                       
144400                 END-COMPUTE                                              
144500                 MOVE 6322-DASKROT9-BEORD                                 
144600                     TO SPAR-DASKROT9-BEORD-ENTER                         
144700                 MOVE ZERO       TO W-IDARTNR-MIN                         
144800                 MOVE +999999999 TO W-IDARTNR-MAX                         
144900                 PERFORM S13-LAES-RADDATA                                 
145000                 IF SEGMENT-FINNS                                         
145100                    MOVE JA TO WS-SKROTDATUM-SLUT                         
145200                               WS-DATUM-HITTAD                            
145300                 ELSE                                                     
145400                    CONTINUE                                              
145500                 END-IF                                                   
145600              ELSE                                                        
145700                 MOVE JA TO WS-SKROTDATUM-SLUT                            
145800                 SET TAB-IX UP BY +1                                      
145900              END-IF                                                      
146000           END-PERFORM                                                    
146100        ELSE                                                              
146200           SET TAB-IX UP BY +1                                            
146300        END-IF                                                            
146400     END-PERFORM                                                          
146500     .                                                                    
146600     EJECT                                                                
146700 FH-ORSAKSTEXTER      SECTION.                                            
146800                                                                          
146900     MOVE +1      TO WS-IX                                                
147000     PERFORM UNTIL WS-IX > 11                                             
147100        IF 6325-TEMEMO (3:10) = WS-TEMEMO-ORS(WS-IX) (3:10)               
147200         IF WO-IX < 5                                                     
147300           MOVE WS-TEMEMO-KORT(WS-IX) TO WO-TEMFSINF-ORS(WO-IX)           
147400           IF WS-IX = 11                                                  
147500              MOVE 6325-TEMEMO (1:9)  TO WO-TEMFSINF-ORS(WO-IX)           
147600           END-IF                                                         
147700         ELSE                                                             
147800           MOVE 'FLER ORS.'           TO WO-TEMFSINF-ORS(5)               
147900           MOVE '- SE 6325'           TO WO-TEMFSINF-ORS(6)               
148000         END-IF                                                           
148100           ADD +1 TO WO-IX                                                
148200        END-IF                                                            
148300        ADD +1    TO WS-IX                                                
148400     END-PERFORM                                                          
148500     .                                                                    
148600     EJECT                                                                
148700 FI-ORSAKSTEXTER      SECTION.                                            
148800                                                                          
148900     MOVE +1      TO WS-IX                                                
149000     PERFORM UNTIL WS-IX > 11                                             
149100        IF 6325-TEMEMO (3:10) = WS-TEMEMO-ESC-ORS(WS-IX) (3:10)           
149200         IF WO-IX < 5                                                     
149300           MOVE WS-TEMEMO-ESC-KORT(WS-IX)                                 
149400                                      TO WO-TEMFSINF-ORS(WO-IX)           
149500           IF WS-IX = 11                                                  
149600              MOVE 6325-TEMEMO (1:9)  TO WO-TEMFSINF-ORS(WO-IX)           
149700           END-IF                                                         
149800         ELSE                                                             
149900           MOVE 'FLER ORS.'           TO WO-TEMFSINF-ORS(5)               
150000           MOVE '- SE 6325'           TO WO-TEMFSINF-ORS(6)               
150100         END-IF                                                           
150200           ADD +1 TO WO-IX                                                
150300        END-IF                                                            
150400        ADD +1    TO WS-IX                                                
150500     END-PERFORM                                                          
150600     .                                                                    
150700     EJECT                                                                
150800                                                                          
150900 G-KOLLA-INPUT SECTION.                                                   
151000     MOVE JA TO INDATA-SW                                                 
151100     MOVE SPACE TO WS-CMD                                                 
151200                                                                          
151300     IF MID-CMD(1)  = ALL '+'                                             
151400     AND MID-CMD(2) = ALL '+'                                             
151500     AND MID-CMD(3) = ALL '+'                                             
151600     AND MID-CMD(4) = ALL '+'                                             
151700     AND MID-CMD(5) = ALL '+'                                             
151800     AND MID-CMD(6) = ALL '+'                                             
151900     AND MID-CMD(7) = ALL '+'                                             
152000     AND MID-CMD(8) = ALL '+'                                             
152100     AND MID-CMD(9) = ALL '+'                                             
152200     AND MID-CMD(10)= ALL '+'                                             
152300     AND MID-CMD(11)= ALL '+'                                             
152400     AND MID-CMD(12)= ALL '+'                                             
152500     AND MID-FLKLAR = ALL '+'                                             
152600        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
152700        CALL WMEDKONV USING MED-WMEDAREA                                  
152800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
152900        PERFORM MFS-ROER-EJ-FAELT-IN                                      
153000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
153100        MOVE NEJ TO INDATA-SW                                             
153200     ELSE                                                                 
153300                                                                          
153400        IF MID-FLKLAR NOT = ALL '+'                                       
153500           IF MID-FLKLAR = 'J' OR 'Y'                                     
153600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR                
153700              PERFORM GA-KOLLA-FLKLAR                                     
153800           ELSE                                                           
153900              MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR                
154000              MOVE NEJ TO INDATA-SW                                       
154100           END-IF                                                         
154200        END-IF                                                            
154300                                                                          
154400        MOVE +1 TO RAD-IX                                                 
154500        PERFORM UNTIL RAD-IX > RAD-IX-MAX                                 
154600           IF MID-CMD(RAD-IX) NOT = ALL '+'                               
154700              IF MID-FLKLAR NOT = ALL '+'                                 
154800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR(RAD-IX)          
154900                 MOVE NEJ TO INDATA-SW                                    
155000              ELSE                                                        
155100                 IF MID-CMD(RAD-IX) = 'B' OR 'D'                          
155200                    IF WS-CMD = 'B' OR 'D' OR SPACE                       
155300                       MOVE MID-CMD(RAD-IX) TO WS-CMD                     
155400                       MOVE MFS-ALFA-FAELT-RAETT                          
155500                                         TO MOD-CMD-ATTR(RAD-IX)          
155600                       PERFORM GC-KOLLA-IDUSER                            
155700                       MOVE 6328-BEANST-GODK TO ANUL-BEANST-GODK          
155800                       MOVE 6328-IDMAIL      TO ANUL-IDMAIL               
155900                    ELSE                                                  
156000                       MOVE MFS-ALFA-FAELT-FEL                            
156100                                        TO MOD-CMD-ATTR(RAD-IX)           
156200                       MOVE NEJ TO INDATA-SW                              
156300                    END-IF                                                
156400                 ELSE                                                     
156500                    IF MID-CMD(RAD-IX) = 'X' OR 'A'                       
156600                       IF WS-CMD = 'X' OR 'A' OR SPACE                    
156700                          MOVE MID-CMD(RAD-IX) TO WS-CMD                  
156800                          MOVE MFS-ALFA-FAELT-RAETT                       
156900                                         TO MOD-CMD-ATTR(RAD-IX)          
157000                          PERFORM GB-KOLLA-SKROTBEORD                     
157100                          PERFORM GC-KOLLA-IDUSER                         
157200                       ELSE                                               
157300                          MOVE MFS-ALFA-FAELT-FEL                         
157400                                         TO MOD-CMD-ATTR(RAD-IX)          
157500                          MOVE NEJ TO INDATA-SW                           
157600                       END-IF                                             
157700                    END-IF                                                
157800                 END-IF                                                   
157900              END-IF                                                      
158000           END-IF                                                         
158100           ADD +1 TO RAD-IX                                               
158200        END-PERFORM                                                       
158300                                                                          
158400        IF INDATA-FEL                                                     
158500           IF MED-IDMFSFEL = NO-AUTHOR-TO-ATT                             
158600             CONTINUE                                                     
158700           ELSE                                                           
158800             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
158900           END-IF                                                         
159000           CALL WMEDKONV USING MED-WMEDAREA                               
159100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
159200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
159300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
159400        END-IF                                                            
159500     END-IF                                                               
159600                                                                          
159700     .                                                                    
159800     EJECT                                                                
159900                                                                          
160000 GA-KOLLA-FLKLAR SECTION.                                                 
160100     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
160200     MOVE W-IDDC    TO SPAR-IDDC                                          
160300     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-Y2K                  
160400                                                                          
160500     MOVE +1 TO RAD-IX                                                    
160600     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
160700        INSPECT MID-IDARTNR(RAD-IX)                                       
160800                     REPLACING LEADING SPACE BY ZERO                      
160900        IF MID-IDARTNR(RAD-IX) NUMERIC                                    
161000           IF MID-IDARTNR(RAD-IX) > ZERO                                  
161100              MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                   
161200                                          WS-DASKROT-AAMMDD               
161300              IF WS-SEKEL = 9                                             
161400                 MOVE 19 TO WS-DASKROT-SS                                 
161500              ELSE                                                        
161600                 MOVE 20 TO WS-DASKROT-SS                                 
161700              END-IF                                                      
161800              MOVE MID-IDARTNR(RAD-IX)        TO W-IDARTNR                
161900                                                 W-6324-IDARTNR           
162000              MOVE MID-IDDC(RAD-IX)           TO W-IDDC-6324              
162100                                                 W-6324-IDDC              
162200              MOVE MID-TIDATUM(RAD-IX)        TO WS-DASKROT-AAMMDD        
162300              MOVE MSGI-KDARBTYP              TO W-6321-KDARBTYP          
162400              MOVE 20                         TO WS-DASKROT-SS            
162500              COMPUTE W-DASKROT9 = 99999999 -                             
162600                                   WS-AAAAMMDD                            
162700              PERFORM IMS-GU-WDGX6324-3                                   
162800              IF MSGI-IDUSER = 6324-IDUSER                                
162900                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR             
163000                 MOVE NEJ TO INDATA-SW                                    
163100              END-IF                                                      
163200              PERFORM S03-KOLLA-IDDC-ART                                  
163300              PERFORM GC-KOLLA-IDUSER                                     
163400           END-IF                                                         
163500        END-IF                                                            
163600        ADD +1 TO RAD-IX                                                  
163700     END-PERFORM                                                          
163800     MOVE SPAR-IDARTNR TO W-IDARTNR                                       
163900     MOVE SPAR-IDDC    TO W-IDDC                                          
164000*                         W-6327-IDDC                                     
164100     .                                                                    
164200     EJECT                                                                
164300                                                                          
164400 GB-KOLLA-SKROTBEORD SECTION.                                             
164500     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
164600     MOVE W-IDDC    TO SPAR-IDDC                                          
164700     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-Y2K                  
164800                                                                          
164900     MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                            
165000                                 WS-DASKROT-AAMMDD                        
165100     IF WS-SEKEL = 9                                                      
165200        MOVE 19 TO WS-DASKROT-SS                                          
165300     ELSE                                                                 
165400        MOVE 20 TO WS-DASKROT-SS                                          
165500     END-IF                                                               
165600     PERFORM S03-KOLLA-IDDC-ART                                           
165700     MOVE SPAR-IDARTNR TO W-IDARTNR                                       
165800     MOVE SPAR-IDDC    TO W-IDDC                                          
165900*                         W-6327-IDDC                                     
166000     .                                                                    
166100     EJECT                                                                
166200                                                                          
166300 GC-KOLLA-IDUSER SECTION.                                                 
166400     MOVE MSGI-IDUSER      TO W-IDUSER-GODK-X                             
166500     MOVE MID-IDDC(RAD-IX)            TO W-IDDC-6324                      
166600                                         W-6327-IDDC                      
166700                                         W-6324-IDDC                      
166800     PERFORM IMS-GU-WDGX6328                                              
166900     IF SEGMENT-SAKNAS                                                    
167000       MOVE NO-AUTHOR-TO-ATT                     TO MED-IDMFSINF          
167100       CALL WMEDKONV USING MED-WMEDAREA                                   
167200       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
167300       MOVE NEJ TO INDATA-SW                                              
167400******************************************************                    
167500     ELSE                                                                 
167600       MOVE 6328-SUBEL       TO W-SUBEL                                   
167700     END-IF                                                               
167800     MOVE MID-IDARTNR(RAD-IX)         TO W-IDARTNR                        
167900                                         W-6324-IDARTNR                   
168000     MOVE MID-TIDATUM(RAD-IX)         TO WS-DASKROT-AAMMDD                
168100     MOVE MSGI-KDARBTYP               TO W-6321-KDARBTYP                  
168200     MOVE 20                          TO WS-DASKROT-SS                    
168300     COMPUTE W-DASKROT9 = 99999999 -                                      
168400                          WS-AAAAMMDD                                     
168500     PERFORM IMS-GU-WDGX6324-3                                            
168600     IF MID-CMD(RAD-IX) = 'X' OR 'A'                                      
168700     OR MID-FLKLAR = 'J'                                                  
168800       IF MSGI-IDUSER = 6324-IDUSER                                       
168900         MOVE NO-AUTHOR-TO-ATT                   TO MED-IDMFSINF          
169000         CALL WMEDKONV USING MED-WMEDAREA                                 
169100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
169200         MOVE NEJ TO INDATA-SW                                            
169300       END-IF                                                             
169400     END-IF                                                               
169500                                                                          
169600     IF MID-CMD(RAD-IX) = 'B' OR 'D'                                      
169700       CONTINUE                                                           
169800     ELSE                                                                 
169900       MOVE +1  TO PERS-IX                                                
170000       PERFORM IMS-GU-WDGX6324-2                                          
170100       IF SEGMENT-FINNS                                                   
170200         PERFORM IMS-GNP-WDGX6326-2                                       
170300         PERFORM UNTIL SEGMENT-SAKNAS OR PERS-IX > MAX-PERS-IX            
170400           IF MSGI-IDUSER = 6326-IDUSER-GODK                              
170500             MOVE ERR-RAD-FINNS-REDAN            TO MED-IDMFSINF          
170600             CALL WMEDKONV USING MED-WMEDAREA                             
170700             MOVE MED-MFSINF TO MOD-TEMFSINF                              
170800             MOVE NEJ TO INDATA-SW                                        
170900           END-IF                                                         
171000           MOVE 6326-IDUSER-GODK  TO W-IDUSER-GODK-X                      
171100           PERFORM IMS-GNP-WDGX6326-2                                     
171200           ADD +1 TO PERS-IX                                              
171300         END-PERFORM                                                      
171400                                                                          
171500         IF MID-SUBEL(RAD-IX) >= W-SUBEL                                  
171600           MOVE INF-HIGHER-LEV                   TO MED-IDMFSINF          
171700           CALL WMEDKONV USING MED-WMEDAREA                               
171800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
171900         END-IF                                                           
172000         MOVE MSGI-IDUSER  TO W-IDUSER-GODK-X                             
172100         PERFORM IMS-GU-WDGX6328                                          
172200       ELSE                                                               
172300         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
172400         CALL WMEDKONV USING MED-WMEDAREA                                 
172500         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
172600         PERFORM MFS-ROER-EJ-FAELT-UT                                     
172700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
172800       END-IF                                                             
172900     END-IF                                                               
173000     .                                                                    
173100     EJECT                                                                
173200                                                                          
173300 H-UPPDATERA SECTION.                                                     
173400     MOVE +1 TO RAD-IX                                                    
173500     MOVE ZERO TO WS-ANTAL-X                                              
173600     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
173700       IF MID-CMD(RAD-IX) = 'X' OR 'A'                                    
173800          ADD +1 TO WS-ANTAL-X                                            
173900       END-IF                                                             
174000       ADD +1 TO RAD-IX                                                   
174100     END-PERFORM                                                          
174200                                                                          
174300     MOVE +1 TO URV-IX                                                    
174400                                                                          
174500     IF MID-FLKLAR = 'J' OR 'Y'                                           
174600       PERFORM HA-SCRAP-ALL                                               
174700     ELSE                                                                 
174800       IF WS-ANTAL-X > +1                                                 
174900         PERFORM HD-SKAPA-SKROTORDER-URVAL                                
175000       ELSE                                                               
175100         MOVE +1 TO RAD-IX                                                
175200         PERFORM UNTIL RAD-IX > RAD-IX-MAX                                
175300           IF MID-CMD(RAD-IX) = 'B' OR 'D'                                
175400             PERFORM HB-UPPDAT-BORTTAG                                    
175500           ELSE                                                           
175600             IF MID-CMD(RAD-IX) = 'X' OR 'A'                              
175700               PERFORM HC-SKAPA-EN-SKROTORDER                             
175800               ADD +1 TO URV-IX                                           
175900             END-IF                                                       
176000           END-IF                                                         
176100           ADD +1 TO RAD-IX                                               
176200         END-PERFORM                                                      
176300       END-IF                                                             
176400     END-IF                                                               
176500                                                                          
176600     IF WS-HIGHLEV = JA                                                   
176700       MOVE INF-HIGHER-LEV TO MED-IDMFSINF                                
176800       CALL WMEDKONV USING MED-WMEDAREA                                   
176900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
177000     END-IF                                                               
177100     MOVE INF-UPDATE-DONE TO MED-IDMFSFEL                                 
177200     CALL WMEDKONV USING MED-WMEDAREA                                     
177300     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
177400     PERFORM MFS-FORM-ATTR                                                
177500     PERFORM MFS-RENSA-FAELT-IN                                           
177600     .                                                                    
177700     EJECT                                                                
177800                                                                          
177900 HA-SCRAP-ALL SECTION.                                                    
178000     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
178100     MOVE W-IDDC    TO SPAR-IDDC                                          
178200     MOVE NEJ       TO WS-FLURVAL                                         
178300                                                                          
178400     MOVE +1 TO RAD-IX                                                    
178500     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
178600        INSPECT MID-IDARTNR(RAD-IX)                                       
178700                     REPLACING LEADING SPACE BY ZERO                      
178800        MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                             
178900        MOVE W-IDARTNR           TO IDARTNR-WS                            
179000        MOVE MID-IDDC(RAD-IX)    TO W-IDDC                                
179100                                    W-IDDC-6324                           
179200                                    W-6327-IDDC                           
179300        MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                         
179400                                    WS-DASKROT-AAMMDD                     
179500        IF WS-SEKEL = 9                                                   
179600           MOVE 19 TO WS-DASKROT-SS                                       
179700        ELSE                                                              
179800           MOVE 20 TO WS-DASKROT-SS                                       
179900        END-IF                                                            
180000        COMPUTE WS-DASKROT9-BEORD = 99999999 - WS-AAAAMMDD                
180100        MOVE WS-DASKROT9-BEORD TO W-DASKROT9                              
180200        MOVE ZERO            TO 6324-KVSKROT-BEORD                        
180300        IF (MID-SUBEL(RAD-IX) <= 6328-SUBEL)                              
180400        OR (6328-SUBEL = 9999999)                                         
180500          PERFORM IMS-GHU-WDR501-6321                                     
180600          IF SEGMENT-FINNS                                                
180700             PERFORM IMS-GU-WDGX6324                                      
180800             IF SEGMENT-FINNS                                             
180900                MOVE 'J' TO 6324-FLSKROT-GODK                             
181000                PERFORM IMS-REPL-WDGX6324                                 
181100                IF 6324-IDKUNDNR = 111                                    
181200                   PERFORM S14-SKAPA-CLASSIC-TRANS                        
181300                END-IF                                                    
181400             END-IF                                                       
181500          END-IF                                                          
181600          MOVE 'J' TO URV-FLKLAR                                          
181700          PERFORM S01-SKAPA-URV-TRANS                                     
181800          MOVE 'J' TO WS-FLURVAL                                          
181900          ADD +1 TO URV-IX                                                
182000                                                                          
182100          MOVE W-IDDC  TO SW-WS-IDDC                                      
182200          IF SW-CDC-SE                                                    
182300             PERFORM IMS-GHU-ARTC01                                       
182400             PERFORM IMS-GHU-ARTC11                                       
182500             IF SEGMENT-FINNS                                             
182600                COMPUTE CLAG-KVSPARR-KVAL =                               
182700                        CLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD            
182800                IF CLAG-KVSPARR-KVAL < ZERO                               
182900                   MOVE ZERO TO CLAG-KVSPARR-KVAL                         
183000                END-IF                                                    
183100                MOVE MSGI-IDUSER                                          
183200                               TO CLAG-IDUSER-SPKVAL                      
183300                MOVE DAGENS-DATUM                                         
183400                               TO CLAG-TISPARR-KVAL                       
183500                                                                          
183600                MOVE 'N' TO CLAG-FLSKROT-BEORD                            
183700                MOVE DAGENS-DATUM                                         
183800                         TO CLAG-TISKROT                                  
183900                IF 6324-IDKUNDNR = 11         AND                         
184000                   6324-IDUSER   = 'W2616800' AND                         
184100                   CLAG-KDERS    = 00         AND                         
184200                  (ART-IDLEVNR NOT = 'BQ8VA')                             
184300                   MOVE 09 TO PROGSW-MID-KDERS                            
184400                   MOVE 1  TO PROGSW-MID-DIERS-ERS                        
184500                   PERFORM S10-SKAPA-TRANS-TILL-1113                      
184600                END-IF                                                    
184700                PERFORM IMS-REPL-ARTC11                                   
184800                PERFORM S04-BOKA-NER-BUFFERT                              
184900             END-IF                                                       
185000          ELSE                                                            
185100             PERFORM IMS-GHU-ARTS11                                       
185200             IF SEGMENT-FINNS                                             
185300                COMPUTE SLAG-KVSPARR-KVAL =                               
185400                        SLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD            
185500                                                                          
185600                IF SLAG-KVSPARR-KVAL < ZERO                               
185700                   MOVE ZERO TO SLAG-KVSPARR-KVAL                         
185800                END-IF                                                    
185900                                                                          
186000                MOVE MSGI-IDUSER                                          
186100                               TO SLAG-IDUSER-SPKVAL                      
186200                MOVE DAGENS-DATUM                                         
186300                               TO SLAG-TISPARR-KVAL                       
186400                                                                          
186500                MOVE 'N' TO SLAG-FLSKROT-BEORD                            
186600                PERFORM IMS-REPL-ARTS11                                   
186700             END-IF                                                       
186800          END-IF                                                          
186900        ELSE                                                              
187000          MOVE JA TO WS-HIGHLEV                                           
187100          PERFORM HF-HAMTA-HOGRE-NIVA                                     
187200          PERFORM S90-SKICKA-MAIL                                         
187300          MOVE NEJ  TO WS-FLURVAL                                         
187400        END-IF                                                            
187500        MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                             
187600        MOVE MID-IDDC(RAD-IX)    TO W-IDDC                                
187700                                    W-IDDC-6324                           
187800                                    W-6327-IDDC                           
187900        MOVE WS-DASKROT9-BEORD TO W-DASKROT9                              
188000                                  WS-6322-DASKROT9                        
188100        INSPECT MID-IDARTNR(RAD-IX)                                       
188200                     REPLACING LEADING SPACE BY ZERO                      
188300        IF MID-IDARTNR(RAD-IX) NUMERIC                                    
188400           IF MID-IDARTNR(RAD-IX) > ZERO                                  
188500              PERFORM HE-UPDATERA-6326                                    
188600              IF WS-FLURVAL = JA                                          
188700                PERFORM S15-SKAPA-WDGX2402                                
188800              END-IF                                                      
188900           END-IF                                                         
189000        END-IF                                                            
189100       ADD +1 TO RAD-IX                                                   
189200     END-PERFORM                                                          
189300     IF WS-FLURVAL = JA                                                   
189400       PERFORM S02-STARTA-URV-TRANS                                       
189500     END-IF                                                               
189600                                                                          
189700**************** ÅTERSTÄLL NYCKLAR                                        
189800     MOVE SPAR-IDARTNR        TO W-IDARTNR                                
189900     MOVE SPAR-IDDC           TO W-IDDC                                   
190000                                 W-IDDC-6324                              
190100*                                W-6327-IDDC                              
190200     .                                                                    
190300     EJECT                                                                
190400 HB-UPPDAT-BORTTAG SECTION.                                               
190500******************************************************************        
190600* FLSKROT-BEORD SÄTTS TILL NEJ PÅ ARTC ELLER ARTS                         
190700* RADEN TAS BORT FRÅN WL632121                                            
190800******************************************************************        
190900                                                                          
191000     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
191100     MOVE W-IDDC    TO SPAR-IDDC                                          
191200                                                                          
191300     INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING SPACE BY ZERO          
191400     MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                                
191500     MOVE MID-IDDC(RAD-IX)    TO W-IDDC                                   
191600                                 W-IDDC-6324                              
191700                                 W-6327-IDDC                              
191800     MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                            
191900                                 WS-DASKROT-AAMMDD                        
192000*    MOVE W-KDARBTYP          TO W-6321-KDARBTYP                          
192100     IF WS-SEKEL = 9                                                      
192200        MOVE 19 TO WS-DASKROT-SS                                          
192300     ELSE                                                                 
192400        MOVE 20 TO WS-DASKROT-SS                                          
192500     END-IF                                                               
192600     COMPUTE WS-DASKROT9-BEORD = 99999999 - WS-AAAAMMDD                   
192700     MOVE WS-DASKROT9-BEORD TO W-DASKROT9                                 
192800     PERFORM IMS-GHU-WDR501-6321                                          
192900     IF SEGMENT-FINNS                                                     
193000        PERFORM IMS-GU-WDGX6324                                           
193100        IF SEGMENT-FINNS                                                  
193200           MOVE 6324-IDUSER TO W-ANNUL-IDUSER                             
193300                               W-IDUSER-GODK-X                            
193400           PERFORM IMS-GU-WDGX6328                                        
193500           IF SEGMENT-FINNS                                               
193600              MOVE 6328-IDMAIL TO W-ANNUL-IDMAIL                          
193700           END-IF                                                         
193800           IF W-ANNUL-IDMAIL NOT = SPACE                                  
193900           AND W-ANNUL-IDUSER NOT = MSGI-IDUSER                           
194000             PERFORM S91-MAIL-ANNUL                                       
194100           END-IF                                                         
194200           PERFORM IMS-DLET-WDGX6324                                      
194300        END-IF                                                            
194400     END-IF                                                               
194500                                                                          
194600     PERFORM IMS-GHU-WDGX6322                                             
194700     IF SEGMENT-FINNS                                                     
194800        PERFORM IMS-GNP-WDGX6324                                          
194900        IF SEGMENT-SAKNAS                                                 
195000           PERFORM IMS-GHU-WDGX6322                                       
195100           PERFORM IMS-DLET-WDGX6322                                      
195200        END-IF                                                            
195300     END-IF                                                               
195400                                                                          
195500     MOVE W-IDDC    TO SW-WS-IDDC                                         
195600     IF SW-CDC-SE                                                         
195700        PERFORM IMS-GHU-ARTC11                                            
195800        IF SEGMENT-FINNS                                                  
195900           MOVE 'N' TO CLAG-FLSKROT-BEORD                                 
196000           PERFORM IMS-REPL-ARTC11                                        
196100        END-IF                                                            
196200     ELSE                                                                 
196300        PERFORM IMS-GHU-ARTS11                                            
196400        IF SEGMENT-FINNS                                                  
196500           MOVE 'N' TO SLAG-FLSKROT-BEORD                                 
196600           PERFORM IMS-REPL-ARTS11                                        
196700        END-IF                                                            
196800     END-IF                                                               
196900                                                                          
197000**************** ÅTERSTÄLL NYCKLAR                                        
197100     MOVE SPAR-IDARTNR        TO W-IDARTNR                                
197200     MOVE SPAR-IDDC           TO W-IDDC                                   
197300                                 W-IDDC-6324                              
197400*                                W-6327-IDDC                              
197500     .                                                                    
197600     EJECT                                                                
197700                                                                          
197800 HC-SKAPA-EN-SKROTORDER SECTION.                                          
197900     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
198000     MOVE W-IDDC    TO SPAR-IDDC                                          
198100                                                                          
198200     INSPECT MID-IDARTNR(RAD-IX)                                          
198300                       REPLACING LEADING SPACE BY ZERO.                   
198400     MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                                
198500     MOVE W-IDARTNR           TO IDARTNR-WS                               
198600     MOVE MID-IDDC(RAD-IX)    TO W-IDDC                                   
198700                                 W-IDDC-6324                              
198800                                 W-6327-IDDC                              
198900                                 WS-IDDC                                  
199000     MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                            
199100                                 WS-DASKROT-AAMMDD                        
199200     IF WS-SEKEL = 9                                                      
199300       MOVE 19 TO WS-DASKROT-SS                                           
199400     ELSE                                                                 
199500       MOVE 20 TO WS-DASKROT-SS                                           
199600     END-IF                                                               
199700     COMPUTE WS-DASKROT9-BEORD = 99999999 - WS-AAAAMMDD                   
199800     MOVE WS-DASKROT9-BEORD TO W-DASKROT9                                 
199900                               WS-6322-DASKROT9                           
200000     MOVE ZERO               TO 6324-KVSKROT-BEORD                        
200100                                                                          
200200     IF (MID-SUBEL(RAD-IX) <= 6328-SUBEL)                                 
200300     OR (6328-SUBEL = 9999999)                                            
200400       PERFORM IMS-GHU-WDR501-6321                                        
200500       IF SEGMENT-FINNS                                                   
200600         PERFORM IMS-GU-WDGX6324                                          
200700         IF SEGMENT-FINNS                                                 
200800           MOVE 'J' TO 6324-FLSKROT-GODK                                  
200900           PERFORM IMS-REPL-WDGX6324                                      
201000           IF 6324-IDKUNDNR = 111                                         
201100             PERFORM S14-SKAPA-CLASSIC-TRANS                              
201200           END-IF                                                         
201300         END-IF                                                           
201400       END-IF                                                             
201500       INSPECT MID-IDARTNR(RAD-IX)                                        
201600                     REPLACING LEADING SPACE BY ZERO                      
201700       IF MID-IDARTNR(RAD-IX) NUMERIC                                     
201800         IF MID-IDARTNR(RAD-IX) > ZERO                                    
201900           PERFORM HE-UPDATERA-6326                                       
202000           MOVE 'N' TO URV-FLKLAR                                         
202100           PERFORM S01-SKAPA-URV-TRANS                                    
202200           PERFORM S15-SKAPA-WDGX2402                                     
202300           PERFORM S02-STARTA-URV-TRANS                                   
202400         END-IF                                                           
202500       END-IF                                                             
202600                                                                          
202700       MOVE W-IDDC  TO SW-WS-IDDC                                         
202800       IF SW-CDC-SE                                                       
202900         PERFORM IMS-GHU-ARTC01                                           
203000         PERFORM IMS-GHU-ARTC11                                           
203100         IF SEGMENT-FINNS                                                 
203200           COMPUTE CLAG-KVSPARR-KVAL =                                    
203300                   CLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD                 
203400           IF CLAG-KVSPARR-KVAL < ZERO                                    
203500             MOVE ZERO    TO CLAG-KVSPARR-KVAL                            
203600           END-IF                                                         
203700           MOVE MSGI-IDUSER TO CLAG-IDUSER-SPKVAL                         
203800           MOVE DAGENS-DATUM TO CLAG-TISPARR-KVAL                         
203900                                                                          
204000           MOVE 'N' TO CLAG-FLSKROT-BEORD                                 
204100           MOVE DAGENS-DATUM TO CLAG-TISKROT                              
204200           IF 6324-IDKUNDNR = 11         AND                              
204300              6324-IDUSER   = 'W2616800' AND                              
204400              CLAG-KDERS    = 00         AND                              
204500             (ART-IDLEVNR NOT = 'BQ8VA')                                  
204600              MOVE 09 TO PROGSW-MID-KDERS                                 
204700              MOVE 1  TO PROGSW-MID-DIERS-ERS                             
204800              PERFORM S10-SKAPA-TRANS-TILL-1113                           
204900           END-IF                                                         
205000           PERFORM IMS-REPL-ARTC11                                        
205100           PERFORM S04-BOKA-NER-BUFFERT                                   
205200         END-IF                                                           
205300       ELSE                                                               
205400         PERFORM IMS-GHU-ARTS11                                           
205500         IF SEGMENT-FINNS                                                 
205600           COMPUTE SLAG-KVSPARR-KVAL =                                    
205700                   SLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD                 
205800                                                                          
205900           IF SLAG-KVSPARR-KVAL < ZERO                                    
206000             MOVE ZERO    TO SLAG-KVSPARR-KVAL                            
206100           END-IF                                                         
206200                                                                          
206300           MOVE MSGI-IDUSER TO SLAG-IDUSER-SPKVAL                         
206400           MOVE DAGENS-DATUM TO SLAG-TISPARR-KVAL                         
206500                                                                          
206600           MOVE 'N' TO SLAG-FLSKROT-BEORD                                 
206700           PERFORM IMS-REPL-ARTS11                                        
206800         END-IF                                                           
206900       END-IF                                                             
207000     ELSE                                                                 
207100       MOVE JA TO WS-HIGHLEV                                              
207200*      CALL FELLOG                                                        
207300       PERFORM HF-HAMTA-HOGRE-NIVA                                        
207400       PERFORM S90-SKICKA-MAIL                                            
207500       INSPECT MID-IDARTNR(RAD-IX)                                        
207600                     REPLACING LEADING SPACE BY ZERO                      
207700       IF MID-IDARTNR(RAD-IX) NUMERIC                                     
207800         IF MID-IDARTNR(RAD-IX) > ZERO                                    
207900           PERFORM HE-UPDATERA-6326                                       
208000         END-IF                                                           
208100       END-IF                                                             
208200     END-IF                                                               
208300                                                                          
208400**************** ÅTERSTÄLL NYCKLAR                                        
208500     MOVE SPAR-IDARTNR        TO W-IDARTNR                                
208600     MOVE SPAR-IDDC           TO W-IDDC                                   
208700                                 W-IDDC-6324                              
208800*                                W-6327-IDDC                              
208900     .                                                                    
209000     EJECT                                                                
209100                                                                          
209200 HD-SKAPA-SKROTORDER-URVAL SECTION.                                       
209300     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
209400     MOVE W-IDDC    TO SPAR-IDDC                                          
209500     MOVE NEJ       TO WS-FLURVAL                                         
209600                                                                          
209700     MOVE +1 TO RAD-IX                                                    
209800     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
209900        IF MID-CMD(RAD-IX) = 'X' OR 'A'                                   
210000           INSPECT MID-IDARTNR(RAD-IX)                                    
210100                        REPLACING LEADING SPACE BY ZERO                   
210200           MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                          
210300           MOVE W-IDARTNR           TO IDARTNR-WS                         
210400           MOVE MID-IDDC(RAD-IX)    TO W-IDDC                             
210500                                       W-IDDC-6324                        
210600                                       W-6327-IDDC                        
210700           MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                      
210800                                             WS-DASKROT-AAMMDD            
210900           IF WS-SEKEL = 9                                                
211000              MOVE 19 TO WS-DASKROT-SS                                    
211100           ELSE                                                           
211200              MOVE 20 TO WS-DASKROT-SS                                    
211300           END-IF                                                         
211400           COMPUTE WS-DASKROT9-BEORD = 99999999 - WS-AAAAMMDD             
211500           MOVE WS-DASKROT9-BEORD   TO W-DASKROT9                         
211600                                       WS-6322-DASKROT9                   
211700           MOVE ZERO                TO 6324-KVSKROT-BEORD                 
211800           IF (MID-SUBEL(RAD-IX) <= 6328-SUBEL)                           
211900           OR (6328-SUBEL = 9999999)                                      
212000             PERFORM IMS-GHU-WDR501-6321                                  
212100             IF SEGMENT-FINNS                                             
212200                PERFORM IMS-GU-WDGX6324                                   
212300                IF SEGMENT-FINNS                                          
212400                   MOVE 'J' TO 6324-FLSKROT-GODK                          
212500                   PERFORM IMS-REPL-WDGX6324                              
212600                   IF 6324-IDKUNDNR = 111                                 
212700                     PERFORM S14-SKAPA-CLASSIC-TRANS                      
212800                   END-IF                                                 
212900                END-IF                                                    
213000             END-IF                                                       
213100                                                                          
213200             MOVE W-IDDC  TO SW-WS-IDDC                                   
213300             IF SW-CDC-SE                                                 
213400                PERFORM IMS-GHU-ARTC01                                    
213500                PERFORM IMS-GHU-ARTC11                                    
213600                IF SEGMENT-FINNS                                          
213700                   COMPUTE CLAG-KVSPARR-KVAL =                            
213800                           CLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD         
213900                   IF CLAG-KVSPARR-KVAL < ZERO                            
214000                      MOVE ZERO TO CLAG-KVSPARR-KVAL                      
214100                   END-IF                                                 
214200                   MOVE MSGI-IDUSER                                       
214300                               TO CLAG-IDUSER-SPKVAL                      
214400                   MOVE DAGENS-DATUM                                      
214500                               TO CLAG-TISPARR-KVAL                       
214600                                                                          
214700                   MOVE 'N' TO CLAG-FLSKROT-BEORD                         
214800                   MOVE DAGENS-DATUM                                      
214900                         TO CLAG-TISKROT                                  
215000                   IF 6324-IDKUNDNR = 11         AND                      
215100                      6324-IDUSER   = 'W2616800' AND                      
215200                      CLAG-KDERS    = 00         AND                      
215300                     (ART-IDLEVNR NOT = 'BQ8VA')                          
215400                      MOVE 09 TO PROGSW-MID-KDERS                         
215500                      MOVE 1  TO PROGSW-MID-DIERS-ERS                     
215600                      PERFORM S10-SKAPA-TRANS-TILL-1113                   
215700                   END-IF                                                 
215800                   PERFORM IMS-REPL-ARTC11                                
215900                   PERFORM S04-BOKA-NER-BUFFERT                           
216000                END-IF                                                    
216100             ELSE                                                         
216200                PERFORM IMS-GHU-ARTS11                                    
216300                IF SEGMENT-FINNS                                          
216400                   COMPUTE SLAG-KVSPARR-KVAL =                            
216500                           SLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD         
216600                                                                          
216700                   IF SLAG-KVSPARR-KVAL < ZERO                            
216800                      MOVE ZERO                                           
216900                               TO SLAG-KVSPARR-KVAL                       
217000                   END-IF                                                 
217100                                                                          
217200                   MOVE MSGI-IDUSER                                       
217300                               TO SLAG-IDUSER-SPKVAL                      
217400                   MOVE DAGENS-DATUM                                      
217500                               TO SLAG-TISPARR-KVAL                       
217600                                                                          
217700                   MOVE 'N' TO SLAG-FLSKROT-BEORD                         
217800                   PERFORM IMS-REPL-ARTS11                                
217900                END-IF                                                    
218000             END-IF                                                       
218100                                                                          
218200             MOVE 'J' TO URV-FLKLAR                                       
218300             PERFORM S01-SKAPA-URV-TRANS                                  
218400             ADD +1 TO URV-IX                                             
218500             MOVE 'J' TO WS-FLURVAL                                       
218600             INSPECT MID-IDARTNR(RAD-IX)                                  
218700                     REPLACING LEADING SPACE BY ZERO                      
218800             IF MID-IDARTNR(RAD-IX) NUMERIC                               
218900               IF MID-IDARTNR(RAD-IX) > ZERO                              
219000                 PERFORM HE-UPDATERA-6326                                 
219100                 PERFORM S15-SKAPA-WDGX2402                               
219200               END-IF                                                     
219300             END-IF                                                       
219400           ELSE                                                           
219500              MOVE JA TO WS-HIGHLEV                                       
219600              PERFORM HF-HAMTA-HOGRE-NIVA                                 
219700              PERFORM S90-SKICKA-MAIL                                     
219800              INSPECT MID-IDARTNR(RAD-IX)                                 
219900                     REPLACING LEADING SPACE BY ZERO                      
220000              IF MID-IDARTNR(RAD-IX) NUMERIC                              
220100                IF MID-IDARTNR(RAD-IX) > ZERO                             
220200                  PERFORM HE-UPDATERA-6326                                
220300                END-IF                                                    
220400              END-IF                                                      
220500              MOVE NEJ TO WS-FLURVAL                                      
220600           END-IF                                                         
220700        END-IF                                                            
220800        ADD +1 TO RAD-IX                                                  
220900     END-PERFORM                                                          
221000                                                                          
221100     IF WS-FLURVAL = JA                                                   
221200       PERFORM S02-STARTA-URV-TRANS                                       
221300     END-IF                                                               
221400                                                                          
221500******************* ÅTERSTÄLL NYCKLAR                                     
221600           MOVE SPAR-IDARTNR        TO W-IDARTNR                          
221700           MOVE SPAR-IDDC           TO W-IDDC                             
221800                                       W-IDDC-6324                        
221900     .                                                                    
222000     EJECT                                                                
222100                                                                          
222200 HE-UPDATERA-6326 SECTION.                                                
222300     ACCEPT WS-TID FROM TIME                                              
222400     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATUM                          
222500     MOVE WS-TID (1:6)               TO WS-TIDHHMMSS                      
222600     MOVE WS-TIDATETIME              TO 6326-TIDATETIME                   
222700     MOVE MSGI-IDUSER                TO 6326-IDUSER-GODK                  
222800     MOVE MSGI-IDUSER                TO W-IDUSER-GODK-X                   
222900     PERFORM IMS-GU-WDGX6328                                              
223000     MOVE 6328-BEANST-GODK           TO 6326-BEANST-GODK                  
223100     PERFORM IMS-ISRT-WDGX6326                                            
223200     .                                                                    
223300     EJECT                                                                
223400                                                                          
223500 HF-HAMTA-HOGRE-NIVA SECTION.                                             
223600     MOVE NEJ TO WS-FLHOGRE                                               
223700     IF 6328-IDUSER-PRI = SPACE                                           
223800        MOVE 6328-SUBEL              TO WS-SUBEL                          
223900        PERFORM IMS-GU-WDGX6327                                           
224000        PERFORM IMS-GNP-WDGX6328                                          
224100        PERFORM UNTIL WS-FLHOGRE = JA                                     
224200                   OR SEGMENT-SAKNAS                                      
224300           IF 6328-SUBEL > WS-SUBEL                                       
224400              MOVE JA                TO WS-FLHOGRE                        
224500           ELSE                                                           
224600              PERFORM IMS-GNP-WDGX6328                                    
224700           END-IF                                                         
224800        END-PERFORM                                                       
224900     ELSE                                                                 
225000        MOVE 6328-IDUSER-PRI         TO W-IDUSER-GODK                     
225100        MOVE 6328-SUBEL              TO WS-SUBEL                          
225200        PERFORM IMS-GU-WDGX6328                                           
225300        IF SEGMENT-FINNS                                                  
225400           CONTINUE                                                       
225500        ELSE                                                              
225600           PERFORM IMS-GU-WDGX6327                                        
225700           PERFORM IMS-GNP-WDGX6328                                       
225800           PERFORM UNTIL WS-FLHOGRE = JA                                  
225900                   OR SEGMENT-SAKNAS                                      
226000              IF 6328-SUBEL > WS-SUBEL                                    
226100                 MOVE JA             TO WS-FLHOGRE                        
226200              ELSE                                                        
226300                 PERFORM IMS-GNP-WDGX6328                                 
226400              END-IF                                                      
226500           END-PERFORM                                                    
226600        END-IF                                                            
226700     END-IF                                                               
226800     .                                                                    
226900     EJECT                                                                
227000                                                                          
227100 S01-SKAPA-URV-TRANS SECTION.                                             
227200     MOVE W-KDARBTYP           TO URV-KDARBTYP                            
227300     INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING SPACE BY ZERO          
227400                                                                          
227500     MOVE MID-IDARTNR(RAD-IX)  TO URV-IDARTNR(URV-IX)                     
227600     MOVE MID-IDDC(RAD-IX)     TO URV-IDDC(URV-IX)                        
227700                                                                          
227800     MOVE MID-TIDATUM(RAD-IX)  TO WS-SEKEL-KOLL                           
227900                                  WS-DASKROT-AAMMDD                       
228000     IF WS-SEKEL = 9                                                      
228100        MOVE 19 TO WS-DASKROT-SS                                          
228200     ELSE                                                                 
228300        MOVE 20 TO WS-DASKROT-SS                                          
228400     END-IF                                                               
228500     COMPUTE WS-DASKROT9-BEORD = 999999999 - WS-AAAAMMDD                  
228600     MOVE WS-DASKROT9-BEORD    TO URV-DASKROT9-BEORD(URV-IX)              
228700     .                                                                    
228800     EJECT                                                                
228900                                                                          
229000 S02-STARTA-URV-TRANS SECTION.                                            
229100     MOVE '6323'   TO MSGSOP-IDTRANS                                      
229200     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
229300     MOVE 'W216S1' TO MSGSOP-IDPROCESS                                    
229400     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
229500                                                                          
229600     STRING 'URVAL1(' WS-URVAL ') '                                       
229700            'URVAL2(' URV-TAB-RAD (1) ') '                                
229800            'URVAL3(' URV-TAB-RAD (2) ') '                                
229900            'URVAL4(' URV-TAB-RAD (3) ') '                                
230000            'URVAL5(' URV-TAB-RAD (4) ') '                                
230100            'URVAL6(' URV-TAB-RAD (5) ') '                                
230200            'URVAL7(' URV-TAB-RAD (6) ') '                                
230300            'URVAL8(' URV-TAB-RAD (7) ') '                                
230400            'URVAL9(' URV-TAB-RAD (8) ') '                                
230500            'URVAL10(' URV-TAB-RAD (9) ') '                               
230600            'URVAL11(' URV-TAB-RAD (10) ') '                              
230700            'URVAL12(' URV-TAB-RAD (11) ') '                              
230800            'URVAL13(' URV-TAB-RAD (12) ')'                               
230900              DELIMITED BY SIZE INTO MSGSOP-TESYMBV                       
231000                                                                          
231100     PERFORM IMS-INSERT-ALTMSG                                            
231200     .                                                                    
231300     EJECT                                                                
231400                                                                          
231500 S03-KOLLA-IDDC-ART SECTION.                                              
231600     IF W-KDARBTYP = 'ESC'                                                
231700        MOVE MID-IDDC(RAD-IX)   TO SW-WS-IDDC                             
231800        IF SW-NDC OR SW-LDC-CN                                            
231900           IF MID-IDDC(RAD-IX) = MSGI-IDDC                                
232000              CONTINUE                                                    
232100           ELSE                                                           
232200              MOVE MSGI-IDDC     TO SW-WS-IDDC                            
232300              IF SW-CDC-SE                                                
232400                 CONTINUE                                                 
232500              ELSE                                                        
232600                IF MID-CMD(RAD-IX) = 'X' OR 'A'                           
232700                   MOVE MFS-ALFA-FAELT-FEL                                
232800                                   TO MOD-CMD-ATTR(RAD-IX)                
232900                ELSE                                                      
233000                   MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKLAR-ATTR             
233100                END-IF                                                    
233200                MOVE NEJ TO INDATA-SW                                     
233300             END-IF                                                       
233400           END-IF                                                         
233500           IF INDATA-OK                                                   
233600**-- ENLIGT PATRIK SKALL KINA KOLLA LEV.NR ISTÄLLET FÖR PRODSL.           
233700             MOVE MSGI-IDDC   TO SW-WS-IDDC                               
233800             IF SW-CDC-SE                                                 
233900               CONTINUE                                                   
234000             ELSE                                                         
234100               IF SW-NDC-CN OR SW-LDC-CN                                  
234200                 INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING            
234300                             SPACE BY ZERO                                
234400                 MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                    
234500                 MOVE MID-IDDC(RAD-IX)    TO W-IDDC                       
234600                 PERFORM IMS-GHU-ARTS11                                   
234700                 IF SEGMENT-FINNS                                         
234800                   IF SLAG-IDLEVNR = '1441 '                              
234900                     IF MID-CMD(RAD-IX) = 'X' OR 'A'                      
235000                        MOVE MFS-ALFA-FAELT-FEL                           
235100                                  TO MOD-CMD-ATTR(RAD-IX)                 
235200                     ELSE                                                 
235300                        MOVE MFS-ALFA-FAELT-FEL                           
235400                                  TO MOD-FLKLAR-ATTR                      
235500                     END-IF                                               
235600                     MOVE NEJ TO INDATA-SW                                
235700                   END-IF                                                 
235800                 END-IF                                                   
235900               ELSE                                                       
236000                 INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING            
236100                             SPACE BY ZERO                                
236200                 MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                    
236300                 PERFORM IMS-GHU-ARTC01                                   
236400                 IF SEGMENT-FINNS                                         
236500                    MOVE ART-KDPRODSL TO TEST-KDPRODSL                    
236600                    IF KDPRODSL-LOCAL                                     
236700                       CONTINUE                                           
236800                    ELSE                                                  
236900                       IF MID-CMD(RAD-IX) = 'X' OR 'A'                    
237000                          MOVE MFS-ALFA-FAELT-FEL                         
237100                                    TO MOD-CMD-ATTR(RAD-IX)               
237200                       ELSE                                               
237300                        MOVE MFS-ALFA-FAELT-FEL                           
237400                                    TO MOD-FLKLAR-ATTR                    
237500                       END-IF                                             
237600                       MOVE NEJ TO INDATA-SW                              
237700                    END-IF                                                
237800                 END-IF                                                   
237900               END-IF                                                     
238000             END-IF                                                       
238100           END-IF                                                         
238200        END-IF                                                            
238300     END-IF                                                               
238400     .                                                                    
238500     EJECT                                                                
238600                                                                          
238700 S04-BOKA-NER-BUFFERT SECTION.                                            
238800*      --- BOKAR NER WDD8 VID SKROTNING                                   
238900     MOVE 6324-KVSKROT-BEORD TO W-KVSKROT-REST                            
239000     PERFORM IMS-GU-WDD801                                                
239100     IF SEGMENT-FINNS                                                     
239200       PERFORM IMS-GHNP-WDD811                                            
239300       PERFORM UNTIL SEGMENT-SAKNAS                                       
239400                  OR W-KVSKROT-REST <= +0                                 
239500         MOVE SALDO-ADBUFFPL TO W-ADBUFFPL                                
239600         IF (6324-FLJUSTBUFF = 'J' OR 'Y')                                
239700            AND SALDO-ADBUFFOMR = 59                                      
239800            AND W-ADBUFFPL (1:2) = 97                                     
239900           IF W-KVSKROT-REST > SALDO-KVBUFF-F                             
240000             COMPUTE W-KVSKROT-REST = W-KVSKROT-REST -                    
240100                                      SALDO-KVBUFF-F                      
240200                     END-COMPUTE                                          
240300             MOVE ZERO TO SALDO-KVBUFF-F                                  
240400           ELSE                                                           
240500             COMPUTE SALDO-KVBUFF-F = SALDO-KVBUFF-F -                    
240600                                      W-KVSKROT-REST                      
240700                     END-COMPUTE                                          
240800             MOVE ZERO TO W-KVSKROT-REST                                  
240900           END-IF                                                         
241000           IF SALDO-KVBUFF-F <= ZERO                                      
241100             PERFORM IMS-DLET-WDD811                                      
241200             PERFORM IMS-GU-WDJ901                                        
241300             PERFORM UNTIL SEGMENT-SAKNAS OR (HIST-KDLOC = 'B'            
241400                              AND HIST-IDDC = 11                          
241500                              AND SALDO-ADBUFFOMR  = HIST-ADLAGOMR        
241600                              AND SALDO-ADBUFFGANG = HIST-ADGANG          
241700                              AND SALDO-ADBUFFPL   = HIST-ADPLATS)        
241800               PERFORM IMS-GHNP-WDJ911                                    
241900               IF SEGMENT-FINNS AND HIST-KDLOC = 'B'                      
242000                              AND HIST-IDDC = 11                          
242100                              AND SALDO-ADBUFFOMR  = HIST-ADLAGOMR        
242200                              AND SALDO-ADBUFFGANG = HIST-ADGANG          
242300                              AND SALDO-ADBUFFPL   = HIST-ADPLATS         
242400                 MOVE FUNCTION CURRENT-DATE(1:8) TO                       
242500                                       HIST-DASTODAT                      
242600                 MOVE MSGI-IDUSER TO HIST-IDUSER-STO                      
242700                 PERFORM IMS-REPL-WDJ911                                  
242800               END-IF                                                     
242900             END-PERFORM                                                  
243000           ELSE                                                           
243100             PERFORM IMS-REPL-WDD811                                      
243200           END-IF                                                         
243300         END-IF                                                           
243400         PERFORM IMS-GHNP-WDD811                                          
243500       END-PERFORM                                                        
243600     END-IF                                                               
243700     .                                                                    
243800     EJECT                                                                
243900 S10-SKAPA-TRANS-TILL-1113 SECTION.                                       
244000                                                                          
244100     MOVE IDARTNR-WS     TO PROGSW-MID-IDARTNR-UT                         
244200*    MOVE 1              TO PROGSW-MID-DIERS-ERS                          
244300*    MOVE 09             TO PROGSW-MID-KDERS                              
244400     MOVE JA             TO PROGSW-MID-FLKLAR                             
244500                                                                          
244600     MOVE ALL '+'        TO PROGSW-MID-IDARTNR-IN                         
244700                            PROGSW-MID-IDAO                               
244800                            PROGSW-MID-TIERSDAT-PREL                      
244900                            PROGSW-MID-TEARTNOT                           
245000                                                                          
245100     MOVE  1                TO PROGSW-IX                                  
245200     PERFORM UNTIL PROGSW-IX > 09                                         
245300        MOVE ALL '+'        TO PROGSW-MID-IDKORTNR                        
245400                                            (PROGSW-IX)                   
245500                               PROGSW-MID-IDARTNR-TILLK                   
245600                                            (PROGSW-IX)                   
245700                               PROGSW-MID-DIERS-TILLK                     
245800                                            (PROGSW-IX)                   
245900                               PROGSW-MID-BEERS                           
246000                                            (PROGSW-IX)                   
246100        ADD 1               TO PROGSW-IX                                  
246200     END-PERFORM                                                          
246300                                                                          
246400     PERFORM IMS-ISRT-ALT-PCB                                             
246500     .                                                                    
246600     EJECT                                                                
246700                                                                          
246800 S11-LAES-GRUNDDATA SECTION.                                              
246900     MOVE NEJ TO LAES-SW                                                  
247000     IF WS-IDDC = ZERO                                                    
247100       PERFORM UNTIL TAB-IX > TAB-IX-MAX OR (LAES-SW = JA)                
247200                     OR TAB-IDDC(TAB-IX) = SPACE                          
247300         MOVE TAB-IDDC(TAB-IX) TO W-IDDC                                  
247400         MOVE W-IDDC           TO W-IDDC-6324                             
247500         PERFORM IMS-GHU-WDR501-6321                                      
247600         IF SEGMENT-FINNS                                                 
247700           MOVE JA TO LAES-SW                                             
247800         ELSE                                                             
247900           SET TAB-IX UP BY +1                                            
248000         END-IF                                                           
248100       END-PERFORM                                                        
248200     ELSE                                                                 
248300       MOVE W-IDDC TO W-IDDC-6324                                         
248400       PERFORM IMS-GHU-WDR501-6321                                        
248500     END-IF                                                               
248600     .                                                                    
248700     EJECT                                                                
248800                                                                          
248900 S12-LAES-SKROTDATUM SECTION.                                             
249000     IF KDARBTYP-SOEKNING = JA                                            
249100       PERFORM IMS-GNP-WDGX6322                                           
249200       IF SEGMENT-FINNS                                                   
249300         MOVE 6322-DASKROT9-BEORD TO W-DASKROT9                           
249400       END-IF                                                             
249500     END-IF                                                               
249600     .                                                                    
249700     EJECT                                                                
249800                                                                          
249900 S13-LAES-RADDATA SECTION.                                                
250000     IF KDARBTYP-SOEKNING = JA                                            
250100       IF IDARTNR-SOEKNING = JA                                           
250200         MOVE W-IDARTNR TO W-IDARTNR-MIN                                  
250300                           W-IDARTNR-MAX                                  
250400       END-IF                                                             
250500       PERFORM IMS-GNP-WDGX6324-ART                                       
250600     END-IF                                                               
250700     .                                                                    
250800     EJECT                                                                
250900                                                                          
251000 S14-SKAPA-CLASSIC-TRANS SECTION.                                         
251100     PERFORM IMS-GHU-ARTC01                                               
251200     IF SEGMENT-FINNS                                                     
251300        MOVE W-IDARTNR      TO IDARTNR-WS                                 
251400        MOVE ART-IDLEVNR    TO FILC-IDLEVNR                               
251500        PERFORM IMS-GHU-ARTC11                                            
251600        IF SEGMENT-FINNS                                                  
251700           MOVE 495         TO CLAG-IDANSK                                
251800           MOVE 049         TO CLAG-IDBERED                               
251900           MOVE 99          TO CLAG-BEFT                                  
252000           MOVE 9           TO CLAG-IDPLANGR-AG                           
252100           MOVE 'GCP'       TO CLAG-IDPROJ                                
252200           MOVE ZERO        TO CLAG-KDFORPPL                              
252300           MOVE ZERO        TO CLAG-KDFORPGP                              
252400           MOVE ZERO        TO CLAG-KDFORPUF                              
252500                                                                          
252600           IF CLAG-IDINK (1:3) NUMERIC                                    
252700              MOVE CLAG-IDINK (1:3) TO FILC-IDINK                         
252800                                       TEST-IDINK                         
252900           ELSE                                                           
253000              IF CLAG-IDINK (2:3) NUMERIC                                 
253100                 MOVE CLAG-IDINK (2:3) TO FILC-IDINK                      
253200              ELSE                                                        
253300                 MOVE ZERO TO FILC-IDINK                                  
253400              END-IF                                                      
253500           END-IF                                                         
253600           MOVE '987'       TO CLAG-IDINK                                 
253700           IF CLAG-KDERS = 09    AND                                      
253800             (ART-IDLEVNR NOT = 'BQ8VA')                                  
253900             MOVE 00      TO PROGSW-MID-KDERS                             
254000             MOVE ALL '+' TO PROGSW-MID-DIERS-ERS                         
254100             PERFORM S10-SKAPA-TRANS-TILL-1113                            
254200           END-IF                                                         
254300                                                                          
254400           PERFORM IMS-REPL-ARTC11                                        
254500           PERFORM IMS-GNP-ARTC23                                         
254600           PERFORM UNTIL SEGMENT-SAKNAS                                   
254700             MOVE AVT-IDAVTAL TO W-IDAVTAL-RED                            
254800***             TAR ÄVEN MED NAP-AVTAL, PREFIX = 004                      
254900             IF (TEST-IDINK > 99 AND                                      
255000                 TEST-IDINK < 790) OR                                     
255100                (TEST-IDINK > 799 AND                                     
255200                 TEST-IDINK < 987) OR                                     
255300                (TEST-IDINK > 987 AND                                     
255400                 TEST-IDINK < 1000) OR                                    
255500                (W-PREFIX = '004')                                        
255600                PERFORM S16-SKAPA-B65                                     
255700             ELSE                                                         
255800                PERFORM S18-SKAPA-R22POST                                 
255900             END-IF                                                       
256000             PERFORM IMS-GNP-ARTC23                                       
256100           END-PERFORM                                                    
256200           PERFORM S14A-SKAPA-FORP-HIST                                   
256300           PERFORM S19-SKAPA-R23POST                                      
256400                                                                          
256500                                                                          
256600           MOVE W-IDARTNR   TO FILC-IDARTNR                               
256700           MOVE SPACE       TO FILC-BEART                                 
256800           MOVE FILC-W21632 TO FILC-FIL-WDR301-DATA                       
256900           ACCEPT W-TID FROM TIME                                         
257000           IF W-TID = FILC-FIL-TIKLOCK                                    
257100              ADD +1        TO FILC-FIL-IDSEKVNR                          
257200           ELSE                                                           
257300              MOVE W-TID    TO FILC-FIL-TIKLOCK                           
257400              MOVE +1       TO FILC-FIL-IDSEKVNR                          
257500           END-IF                                                         
257600           PERFORM IMS-ISRT-WLFILC                                        
257700                                                                          
257800           MOVE 'BQ8VA'    TO W-IDLEVNR-WDF2                              
257900           MOVE 'CLASSIC'  TO W-IDDIRGRP                                  
258000           MOVE W-IDARTNR  TO WDF2-ART-IDARTNR                            
258100           MOVE ZERO       TO WDF2-ART-DASTADAT                           
258200***********MOVE DAGENS-DATUM-Y2K TO WDF2-ART-DASTADAT                     
258300           MOVE -99        TO WDF2-ART-KVLS-DLEV                          
258400           MOVE ZERO       TO WDF2-ART-TIINLMOT                           
258500                              WDF2-ART-TIREGDAT                           
258600*      -99 BETYDER ATT SALDOT INTE UPPDATERAS AV LEVERANTÖR               
258700*      DET BETYDER ALLTSÅ INTE ATT VI HAR ETT NEGATIVT SALDO :-)          
258800*      FÖR LEVERANTÖRER SOM SKICKAR SALDOUPPGIFTER ÄR VÄRDET >= 0         
258900*      VID NYUPPLÄGG AV ARTIKLAR PÅ LEVERANTÖRER SOM REDOVISAR            
259000*      SALDO, KOMMER ARTIKELN SÅLEDES HA VÄRDET -99 TILL FÖRSTA           
259100*      UPPDATERINGEN AV SALDOT                                            
259200                                                                          
259300           PERFORM IMS-ISRT-WDF212                                        
259400        END-IF                                                            
259500     END-IF                                                               
259600     .                                                                    
259700     EJECT                                                                
259800 S14A-SKAPA-FORP-HIST SECTION.                                            
259900                                                                          
260000     PERFORM IMS-GU-WDT301                                                
260100     IF SEGMENT-SAKNAS                                                    
260200        MOVE W-IDARTNR TO FART-IDARTNR                                    
260300        PERFORM IMS-ISRT-WDT301                                           
260400     END-IF                                                               
260500**** INSERT WDT311                                                        
260600**** MOVE 'W60323'    TO FPCK-IDUSER                                      
260700     MOVE MSGI-IDUSER TO FPCK-IDUSER                                      
260800     MOVE 99          TO FPCK-BEFT                                        
260900     MOVE ZERO        TO FPCK-KDFORPPL                                    
261000     MOVE ZERO        TO FPCK-KDFORPGP                                    
261100     MOVE ZERO        TO FPCK-KDFORPUF                                    
261200     MOVE 'ÖVERGÅTT TILL CLASSIC DIREKTLEVERANS'                          
261300                      TO FPCK-TEBEFT(1)                                   
261400     MOVE SPACE       TO FPCK-TEBEFT(2)                                   
261500     MOVE SPACE       TO FPCK-TEBEFT(3)                                   
261600     MOVE SPACE       TO FPCK-TEBEFT(4)                                   
261700     MOVE SPACE       TO FPCK-TEBEFT(5)                                   
261800     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                       
261900     COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WS-DAREGDAT               
262000     ACCEPT WS-TID FROM TIME                                              
262100     COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WS-TID                     
262200     MOVE 'SE'        TO FPCK-IDLANDX2                                    
262300     PERFORM IMS-ISRT-WDT311                                              
262400     .                                                                    
262500     EJECT                                                                
262600                                                                          
262700 S15-SKAPA-WDGX2402 SECTION.                                              
262800     MOVE 6324-IDARTNR        TO 2402-IDARTNR                             
262900     MOVE 6324-IDANALYS       TO 2402-IDANALYS                            
263000     MOVE 6324-IDDC           TO 2402-IDDC                                
263100     MOVE 6324-IDDISTR        TO 2402-IDDISTR                             
263200     MOVE 6324-IDKONTO        TO 2402-IDKONTO                             
263300     MOVE 6324-IDKST          TO 2402-IDKST                               
263400     MOVE 6324-IDPERSON       TO 2402-IDPERSON                            
263500     MOVE 6321-KDARBTYP       TO 2402-KDARBTYP                            
263600     MOVE 6324-KVSKROT-BEORD  TO 2402-KVSKROT-BEORD                       
263700     IF 6324-KVSKROT-ONDEM NOT NUMERIC                                    
263800       MOVE ZERO               TO 2402-KVSKROT-KVAR                       
263900     ELSE                                                                 
264000       MOVE 6324-KVSKROT-ONDEM TO 2402-KVSKROT-KVAR                       
264100     END-IF                                                               
264200     MOVE 6324-IDUSER         TO 2402-IDUSER                              
264300     MOVE 6324-BEANST         TO 2402-BEANST                              
264400     MOVE WS-6322-DASKROT9    TO 2402-DASKROT9-BEORD                      
264500     MOVE 1 TO PERS-IX                                                    
264600     PERFORM UNTIL PERS-IX > MAX-PERS-IX                                  
264700       MOVE SPACE             TO 2402-TIDATETIME(PERS-IX)                 
264800                                 2402-IDUSER-GODK(PERS-IX)                
264900                                 2402-BEANST-GODK(PERS-IX)                
265000       ADD +1 TO PERS-IX                                                  
265100     END-PERFORM                                                          
265200     MOVE 6324-IDKUNDNR       TO 2402-IDKUNDNR                            
265300     MOVE 6324-KDERS-UTG      TO 2402-KDERS-UTG                           
265400     MOVE 6324-KVTILLG-CDC    TO 2402-KVTILLG-CDC                         
265500     MOVE 6324-KVTILLG-SDC    TO 2402-KVTILLG-SDC                         
265600     MOVE 6324-KVAKS-CDC      TO 2402-KVAKS-CDC                           
265700     MOVE 6324-KVAKS-SDC      TO 2402-KVAKS-SDC                           
265800     MOVE +1       TO IX-BEEMB                                            
265900     PERFORM UNTIL IX-BEEMB > 20                                          
266000       MOVE 6324-BEEMBLEM(IX-BEEMB) TO 2402-BEEMBLEM(IX-BEEMB)            
266100       ADD +1 TO IX-BEEMB                                                 
266200     END-PERFORM                                                          
266300     MOVE 6324-SUTPO-TOT      TO 2402-SUTPO-TOT                           
266400     MOVE 1 TO PERS-IX                                                    
266500     PERFORM IMS-GU-WDGX6324-2                                            
266600     IF SEGMENT-FINNS                                                     
266700       PERFORM IMS-GNP-WDGX6326-2                                         
266800     END-IF                                                               
266900     PERFORM UNTIL SEGMENT-SAKNAS OR PERS-IX > MAX-PERS-IX                
267000       MOVE 6326-TIDATETIME   TO 2402-TIDATETIME(PERS-IX)                 
267100       MOVE 6326-IDUSER-GODK  TO 2402-IDUSER-GODK(PERS-IX)                
267200       MOVE 6326-BEANST-GODK  TO 2402-BEANST-GODK(PERS-IX)                
267300       ADD +1 TO PERS-IX                                                  
267400       PERFORM IMS-GNP-WDGX6326-2                                         
267500     END-PERFORM                                                          
267600     PERFORM IMS-ISRT-WDGX2402                                            
267700     .                                                                    
267800     EJECT                                                                
267900  S16-SKAPA-B65 SECTION.                                                  
268000                                                                          
268100     ACCEPT ZZAC-TIKLOCK             FROM  TIME                           
268200     ACCEPT ZZAC-TIAAMMDD FROM             DATE                           
268300     ADD 1                          TO WS-IDLOGLOP                        
268400     MOVE WS-IDLOGLOP               TO ZZAC-IDLOGLOP                      
268500     MOVE SPACE              TO A310-LEVNUM-GODSM                         
268600                              A310-ANT-BESTANN                            
268700     MOVE 'RY2'              TO A310-KT                                   
268800     MOVE DAGENS-DATUM       TO A310-DATUM-UTSKR                          
268900     MOVE ART-IDLEVNR        TO W-IDLEVNR                                 
269000     IF W-IDLEVNR (5:1) = SPACE                                           
269100*      LEVNUM SKALL TILLS VIDARE VARA NUMERISKT I X(5)                    
269200       MOVE ZERO             TO TALLY                                     
269300       INSPECT W-IDLEVNR TALLYING TALLY                                   
269400                          FOR CHARACTERS BEFORE INITIAL SPACE             
269500       IF TALLY = ZERO                                                    
269600          MOVE ZERO          TO WS-IDLEVNR-NUM                            
269700       ELSE                                                               
269800          MOVE W-IDLEVNR (1:TALLY)                                        
269900                             TO WS-IDLEVNR-NUM                            
270000       END-IF                                                             
270100       MOVE WS-IDLEVNR-NUM TO A310-LEVNUM                                 
270200     ELSE                                                                 
270300       MOVE W-IDLEVNR        TO A310-LEVNUM                               
270400     END-IF                                                               
270500     MOVE W-IDARTNR          TO WS-IDARTNR                                
270600     MOVE WS-IDARTNR         TO WS-IDARTNR-8                              
270700     MOVE WS-IDARTNR-8       TO A310-ARTNR                                
270800                              W092-SORTBGP                                
270900                                                                          
271000     MOVE AVT-IDAVTAL        TO W-IDAVTAL-RED                             
271100     MOVE W-PREFIX           TO A310-BESTPREF                             
271200     MOVE W-AVTALSNR         TO A310-BESTLNR                              
271300     MOVE W-SUFFIX           TO A310-BESTSUFF                             
271400     MOVE A310-A310B65       TO ZZAC-LOGGPOST                             
271500     MOVE W092-AREA          TO ZZAC-SORTPOST                             
271600     PERFORM IMS-ISRT-ZZAC                                                
271700     .                                                                    
271800     EJECT                                                                
271900*                                                                         
272000 S18-SKAPA-R22POST SECTION.                                               
272100                                                                          
272200     MOVE 'R22'                TO BAS-R22-IDPTYP                          
272300     MOVE W-IDARTNR            TO BAS-R22-IDARTNR                         
272400     MOVE AVT-IDAVTAL          TO BAS-R22-IDBEST                          
272500     MOVE AVT-IDLEVNR-AVT      TO BAS-R22-IDLEVNR-BEST                    
272600     MOVE SPACE                TO BAS-R22-IDLEVNR-SHIP                    
272700*    MOVE AVT-TIAVTAL          TO BAS-R22-TIBEST                          
272800     MOVE DAGENS-DATUM         TO BAS-R22-TIBEST                          
272900     MOVE AVT-KVAVTANT         TO BAS-R22-KVBEST                          
273000     MOVE 5                    TO BAS-R22-KDBEH-BEST                      
273100     MOVE SPACE                TO BAS-R22-TENOT-BESTPRIS                  
273200     MOVE SPACE                TO BAS-R22-REST                            
273300                                                                          
273400                                                                          
273500     MOVE MSG-JULIAN-DATE      TO POST-TIREGDAT W-TIREGDAT                
273600     ADD +1                    TO POST-TIKLOCK W-TIKLOCK                  
273700     MOVE MSG-SIGNON-USERID    TO POST-IDUSER                             
273800     MOVE MSG-LTERM-NAME       TO POST-IDLTERM                            
273900     MOVE ZERO                 TO POST-TIBORT                             
274000                                                                          
274100     MOVE BAS-R22-REGPOST      TO POST-REGPOST                            
274200     PERFORM IMS-ISRT-WDG901                                              
274300     IF SEGMENT-FINNS-REDAN                                               
274400       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
274500          ADD +1 TO POST-TIKLOCK W-TIKLOCK                                
274600          PERFORM IMS-ISRT-WDG901                                         
274700       END-PERFORM                                                        
274800     END-IF                                                               
274900     .                                                                    
275000     EJECT                                                                
275100*                                                                         
275200 S19-SKAPA-R23POST SECTION.                                               
275300                                                                          
275400     MOVE 'R23'                TO BAS-R23-IDPTYP                          
275500     MOVE W-IDARTNR            TO BAS-R23-IDARTNR                         
275600     MOVE 987910987094         TO BAS-R23-IDAVTAL                         
275700     MOVE 'BQ8VA'              TO BAS-R23-IDLEVNR-AVT                     
275800                                  BAS-R23-IDLEVNR-SHIP                    
275900     MOVE DAGENS-DATUM         TO BAS-R23-TIAVTAL                         
276000     MOVE ZERO                 TO BAS-R23-KVAVTANT                        
276100     MOVE +1                   TO BAS-R23-KDBEH-AVT                       
276200     MOVE SPACE                TO BAS-R23-TENOT-AVTPRIS                   
276300     MOVE SPACE                TO BAS-R23-REST                            
276400                                                                          
276500                                                                          
276600     MOVE MSG-JULIAN-DATE      TO POST-TIREGDAT W-TIREGDAT                
276700     ADD +1                    TO POST-TIKLOCK W-TIKLOCK                  
276800     MOVE MSG-SIGNON-USERID    TO POST-IDUSER                             
276900     MOVE MSG-LTERM-NAME       TO POST-IDLTERM                            
277000     MOVE ZERO                 TO POST-TIBORT                             
277100                                                                          
277200     MOVE BAS-R23-REGPOST      TO POST-REGPOST                            
277300     PERFORM IMS-ISRT-WDG901                                              
277400     IF SEGMENT-FINNS-REDAN                                               
277500       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
277600          ADD +1 TO POST-TIKLOCK W-TIKLOCK                                
277700          PERFORM IMS-ISRT-WDG901                                         
277800       END-PERFORM                                                        
277900     END-IF                                                               
278000     .                                                                    
278100     EJECT                                                                
278200                                                                          
278300                                                                          
278400                                                                          
278500 S90-SKICKA-MAIL SECTION.                                                 
278600     MOVE W-IDARTNR         TO WSM-IDARTNR                                
278700     MOVE '6323'            TO MAIL-IDTRANS                               
278800     MOVE '1'               TO MAIL-KDMFSFOR                              
278900     MOVE 6328-IDMAIL       TO MAIL-IDMAIL                                
279000     MOVE 'SCRAPORDER'      TO MAIL-IDMAILTTL                             
279100     MOVE +23               TO MAIL-KVMAILLN                              
279200     MOVE 'GO TO SCREEN 6323 TO APPROVE SCRAPORDER'                       
279300                            TO MAIL-TEMAIL (1)                            
279400     MOVE 'JOB ROLE:      DC:        PART NO:'                            
279500                            TO MAIL-TEMAIL (2)                            
279600     MOVE SPACE             TO MAIL-TEMAIL (3)                            
279700     MOVE MSGI-KDARBTYP     TO MAIL-TEMAIL (3)(1:8)                       
279800     MOVE W-IDDC            TO MAIL-TEMAIL (3)(16:2)                      
279900                               W-IDDC-KVAL                                
280000     MOVE WSM-IDARTNR       TO MAIL-TEMAIL (3)(27:9)                      
280100                               W-IDARTNR-KVAL                             
280200     MOVE ' '                                                             
280300                            TO MAIL-TEMAIL (4)                            
280400     MOVE 'REASON FOR SCRAPPING:'                                         
280500                            TO MAIL-TEMAIL (5)                            
280600     MOVE 2                 TO W-KDSTASKR-KVAL                            
280700                                                                          
280800     MOVE SPACE             TO MAIL-TEMAIL (6)                            
280900     MOVE SPACE             TO MAIL-TEMAIL (7)                            
281000     MOVE SPACE             TO MAIL-TEMAIL (8)                            
281100     MOVE SPACE             TO MAIL-TEMAIL (9)                            
281200     MOVE SPACE             TO MAIL-TEMAIL (10)                           
281300     MOVE SPACE             TO MAIL-TEMAIL (11)                           
281400     MOVE SPACE             TO MAIL-TEMAIL (12)                           
281500     MOVE SPACE             TO MAIL-TEMAIL (13)                           
281600     MOVE SPACE             TO MAIL-TEMAIL (14)                           
281700     MOVE SPACE             TO MAIL-TEMAIL (15)                           
281800     MOVE SPACE             TO MAIL-TEMAIL (16)                           
281900     PERFORM IMS-GU-WDGX6324-6325                                         
282000     IF SEGMENT-FINNS                                                     
282100       PERFORM IMS-GNP-WDGX6325                                           
282200       MOVE ZERO          TO IX                                           
282300       PERFORM UNTIL SEGMENT-SAKNAS                                       
282400         MOVE 6325-IDRADNR TO IX                                          
282500         IF IX = 1                                                        
282600           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (6)                           
282700         END-IF                                                           
282800         IF IX = 2                                                        
282900           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (7)                           
283000         END-IF                                                           
283100         IF IX = 3                                                        
283200           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (8)                           
283300         END-IF                                                           
283400         IF IX = 4                                                        
283500           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (9)                           
283600         END-IF                                                           
283700         IF IX = 5                                                        
283800           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (10)                          
283900         END-IF                                                           
284000         IF IX = 6                                                        
284100           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (11)                          
284200         END-IF                                                           
284300         IF IX = 7                                                        
284400           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (12)                          
284500         END-IF                                                           
284600         IF IX = 8                                                        
284700           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (13)                          
284800         END-IF                                                           
284900         IF IX = 9                                                        
285000           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (14)                          
285100         END-IF                                                           
285200         IF IX = 10                                                       
285300           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (15)                          
285400         END-IF                                                           
285500         IF IX = 11                                                       
285600           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (16)                          
285700         END-IF                                                           
285800                                                                          
285900         PERFORM IMS-GNP-WDGX6325                                         
286000       END-PERFORM                                                        
286100                                                                          
286200       MOVE ' '                                                           
286300                            TO MAIL-TEMAIL (17)                           
286400       MOVE 'STOCK SITUATION WHEN SCRAP WAS ISSUED:'                      
286500                            TO MAIL-TEMAIL (18)                           
286600                                                                          
286700       MOVE 6324-IDDC            TO MAIL-TEMAIL (21)(4:2)                 
286800       MOVE 6324-KDERS-UTG       TO WS-KDERS-UTG                          
286900       MOVE WS-KDERS-UTG         TO MAIL-TEMAIL (21)(60:2)                
287000       MOVE 6324-SUTPO-TOT       TO WS-SUTPO-TOT                          
287100       MOVE WS-SUTPO-TOT         TO MAIL-TEMAIL (20)(51:7)                
287200       MOVE 6324-KVSKROT-BEORD   TO WS-KVSKROT-BEORD                      
287300       MOVE WS-KVSKROT-BEORD     TO MAIL-TEMAIL (21)(27:7)                
287400       MOVE 6324-KVSKROT-KVAR    TO WS-KVSKROT-KVAR                       
287500       MOVE WS-KVSKROT-KVAR      TO MAIL-TEMAIL (21)(41:7)                
287600       MOVE 6324-KVTILLG-CDC     TO WS-KVTILLG-CDC                        
287700       MOVE WS-KVTILLG-CDC       TO MAIL-TEMAIL (20)(7:7)                 
287800       MOVE 6324-KVTILLG-SDC     TO WS-KVTILLG-SDC                        
287900       MOVE WS-KVTILLG-SDC       TO MAIL-TEMAIL (21)(7:7)                 
288000       MOVE 6324-KVAKS-CDC       TO WS-KVAKS-CDC                          
288100       MOVE WS-KVAKS-CDC         TO MAIL-TEMAIL (20)(15:7)                
288200       MOVE 6324-KVAKS-SDC       TO WS-KVAKS-SDC                          
288300       MOVE WS-KVAKS-SDC         TO MAIL-TEMAIL (21)(15:7)                
288400                                                                          
288500       MOVE SPACE                TO MAIL-TEMAIL (22)                      
288600       MOVE SPACE                TO MAIL-TEMAIL (23)                      
288700       MOVE +1    TO IX-BEEMB                                             
288800       PERFORM UNTIL IX-BEEMB > 20                                        
288900         IF IX-BEEMB = 1                                                  
289000          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(17:5)          
289100         END-IF                                                           
289200         IF IX-BEEMB = 2                                                  
289300          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(22:5)          
289400         END-IF                                                           
289500         IF IX-BEEMB = 3                                                  
289600          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(27:5)          
289700         END-IF                                                           
289800         IF IX-BEEMB = 4                                                  
289900          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(32:5)          
290000         END-IF                                                           
290100         IF IX-BEEMB = 5                                                  
290200          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(37:5)          
290300         END-IF                                                           
290400         IF IX-BEEMB = 6                                                  
290500          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(42:5)          
290600         END-IF                                                           
290700         IF IX-BEEMB = 7                                                  
290800          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(47:5)          
290900         END-IF                                                           
291000         IF IX-BEEMB = 8                                                  
291100          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(52:5)          
291200         END-IF                                                           
291300         IF IX-BEEMB = 9                                                  
291400          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(1:5)           
291500         END-IF                                                           
291600         IF IX-BEEMB = 10                                                 
291700          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(6:5)           
291800         END-IF                                                           
291900         IF IX-BEEMB = 11                                                 
292000          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(11:5)          
292100         END-IF                                                           
292200         IF IX-BEEMB = 12                                                 
292300          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(16:5)          
292400         END-IF                                                           
292500         IF IX-BEEMB = 13                                                 
292600          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(21:5)          
292700         END-IF                                                           
292800         IF IX-BEEMB = 14                                                 
292900          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(26:5)          
293000         END-IF                                                           
293100         IF IX-BEEMB = 15                                                 
293200          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(31:5)          
293300         END-IF                                                           
293400         IF IX-BEEMB = 16                                                 
293500          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(36:5)          
293600         END-IF                                                           
293700         IF IX-BEEMB = 17                                                 
293800          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(41:5)          
293900         END-IF                                                           
294000         IF IX-BEEMB = 18                                                 
294100          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(46:5)          
294200         END-IF                                                           
294300         IF IX-BEEMB = 19                                                 
294400          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(51:5)          
294500         END-IF                                                           
294600         IF IX-BEEMB = 20                                                 
294700          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(56:5)          
294800         END-IF                                                           
294900         ADD +1 TO IX-BEEMB                                               
295000       END-PERFORM                                                        
295100     END-IF                                                               
295200                                                                          
295300     MOVE 'STOCK BALANCE'   TO MAIL-TEMAIL (19)(1:13)                     
295400     MOVE 'AK-BALANCE'      TO MAIL-TEMAIL (19)(15:10)                    
295500     MOVE 'QANT TO SCRAP'   TO MAIL-TEMAIL (19)(27:13)                    
295600     MOVE 'REM STOCK'       TO MAIL-TEMAIL (19)(41:9)                     
295700     MOVE 'TPO-TOTAL'       TO MAIL-TEMAIL (19)(51:9)                     
295800                                                                          
295900     MOVE 'CDC'             TO MAIL-TEMAIL (20)(1:3)                      
296000                                                                          
296100     MOVE 'DC'              TO MAIL-TEMAIL (21)(1:2)                      
296200     MOVE 'SS CODE'         TO MAIL-TEMAIL (21)(51:7)                     
296300                                                                          
296400     MOVE 'PART IN VEHICLE' TO MAIL-TEMAIL (22)(1:15)                     
296500                                                                          
296600     IF MAIL-IDMAIL = SPACE                                               
296700       CONTINUE                                                           
296800     ELSE                                                                 
296900       PERFORM IMS-PURG-MAIL                                              
297000     END-IF                                                               
297100     .                                                                    
297200     EJECT                                                                
297300                                                                          
297400 S91-MAIL-ANNUL SECTION.                                                  
297500     MOVE W-IDARTNR         TO WSM-IDARTNR                                
297600     MOVE '6323'            TO MAIL-IDTRANS                               
297700     MOVE '1'               TO MAIL-KDMFSFOR                              
297800     MOVE W-ANNUL-IDMAIL    TO MAIL-IDMAIL                                
297900     MOVE 'ANNUL. SCRORD'   TO MAIL-IDMAILTTL                             
298000     MOVE +26               TO MAIL-KVMAILLN                              
298100     MOVE 'SCRAPORDER REJECTED BY APPROVER'                               
298200                            TO MAIL-TEMAIL (1)                            
298300     MOVE 'JOB ROLE:      DC:        PART NO:'                            
298400                            TO MAIL-TEMAIL (2)                            
298500     MOVE SPACE             TO MAIL-TEMAIL (3)                            
298600     MOVE MSGI-KDARBTYP     TO MAIL-TEMAIL (3)(1:8)                       
298700     MOVE W-IDDC            TO MAIL-TEMAIL (3)(16:2)                      
298800                               W-IDDC-KVAL                                
298900     MOVE WSM-IDARTNR       TO MAIL-TEMAIL (3)(27:9)                      
299000                               W-IDARTNR-KVAL                             
299100     MOVE ' '                                                             
299200                            TO MAIL-TEMAIL (4)                            
299300     MOVE 'REJECTED BY:                  E-MAIL:'                         
299400                            TO MAIL-TEMAIL (5)                            
299500     MOVE SPACE             TO MAIL-TEMAIL (6)                            
299600     MOVE ANUL-BEANST-GODK  TO MAIL-TEMAIL (6)(1:25)                      
299700     MOVE ANUL-IDMAIL(1:34) TO MAIL-TEMAIL (6)(31:34)                     
299800     MOVE ' '                                                             
299900                            TO MAIL-TEMAIL (7)                            
300000                                                                          
300100     MOVE 'REASON FOR SCRAPPING:'                                         
300200                            TO MAIL-TEMAIL (8)                            
300300     MOVE 2                 TO W-KDSTASKR-KVAL                            
300400                                                                          
300500     MOVE SPACE        TO MAIL-TEMAIL (9)                                 
300600     MOVE SPACE        TO MAIL-TEMAIL (10)                                
300700     MOVE SPACE        TO MAIL-TEMAIL (11)                                
300800     MOVE SPACE        TO MAIL-TEMAIL (12)                                
300900     MOVE SPACE        TO MAIL-TEMAIL (13)                                
301000     MOVE SPACE        TO MAIL-TEMAIL (14)                                
301100     MOVE SPACE        TO MAIL-TEMAIL (15)                                
301200     MOVE SPACE        TO MAIL-TEMAIL (16)                                
301300     MOVE SPACE        TO MAIL-TEMAIL (17)                                
301400     MOVE SPACE        TO MAIL-TEMAIL (18)                                
301500     MOVE SPACE        TO MAIL-TEMAIL (19)                                
301600                                                                          
301700     PERFORM IMS-GU-WDGX6324-6325                                         
301800     IF SEGMENT-FINNS                                                     
301900       PERFORM IMS-GNP-WDGX6325                                           
302000       MOVE ZERO           TO IX                                          
302100       PERFORM UNTIL SEGMENT-SAKNAS                                       
302200         MOVE 6325-IDRADNR TO IX                                          
302300         IF IX = 1                                                        
302400           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (9)                           
302500         END-IF                                                           
302600         IF IX = 2                                                        
302700           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (10)                          
302800         END-IF                                                           
302900         IF IX = 3                                                        
303000           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (11)                          
303100         END-IF                                                           
303200         IF IX = 4                                                        
303300           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (12)                          
303400         END-IF                                                           
303500         IF IX = 5                                                        
303600           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (13)                          
303700         END-IF                                                           
303800         IF IX = 6                                                        
303900           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (14)                          
304000         END-IF                                                           
304100         IF IX = 7                                                        
304200           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (15)                          
304300         END-IF                                                           
304400         IF IX = 8                                                        
304500           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (16)                          
304600         END-IF                                                           
304700         IF IX = 9                                                        
304800           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (17)                          
304900         END-IF                                                           
305000         IF IX = 10                                                       
305100           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (18)                          
305200         END-IF                                                           
305300         IF IX = 11                                                       
305400           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (19)                          
305500         END-IF                                                           
305600                                                                          
305700         PERFORM IMS-GNP-WDGX6325                                         
305800       END-PERFORM                                                        
305900                                                                          
306000       MOVE ' '                                                           
306100                            TO MAIL-TEMAIL (20)                           
306200       MOVE 'STOCK SITUATION WHEN SCRAP WAS ISSUED:'                      
306300                            TO MAIL-TEMAIL (21)                           
306400                                                                          
306500       MOVE 6324-IDDC            TO MAIL-TEMAIL (24)(4:2)                 
306600       MOVE 6324-KDERS-UTG       TO WS-KDERS-UTG                          
306700       MOVE WS-KDERS-UTG         TO MAIL-TEMAIL (24)(60:2)                
306800       MOVE 6324-SUTPO-TOT       TO WS-SUTPO-TOT                          
306900       MOVE WS-SUTPO-TOT         TO MAIL-TEMAIL (23)(51:7)                
307000       MOVE 6324-KVSKROT-BEORD   TO WS-KVSKROT-BEORD                      
307100       MOVE WS-KVSKROT-BEORD     TO MAIL-TEMAIL (24)(27:7)                
307200       MOVE 6324-KVSKROT-KVAR    TO WS-KVSKROT-KVAR                       
307300       MOVE WS-KVSKROT-KVAR      TO MAIL-TEMAIL (24)(41:7)                
307400       MOVE 6324-KVTILLG-CDC     TO WS-KVTILLG-CDC                        
307500       MOVE WS-KVTILLG-CDC       TO MAIL-TEMAIL (23)(7:7)                 
307600       MOVE 6324-KVTILLG-SDC     TO WS-KVTILLG-SDC                        
307700       MOVE WS-KVTILLG-SDC       TO MAIL-TEMAIL (24)(7:7)                 
307800       MOVE 6324-KVAKS-CDC       TO WS-KVAKS-CDC                          
307900       MOVE WS-KVAKS-CDC         TO MAIL-TEMAIL (23)(15:7)                
308000       MOVE 6324-KVAKS-SDC       TO WS-KVAKS-SDC                          
308100       MOVE WS-KVAKS-SDC         TO MAIL-TEMAIL (24)(15:7)                
308200                                                                          
308300       MOVE SPACE                TO MAIL-TEMAIL (25)                      
308400       MOVE SPACE                TO MAIL-TEMAIL (26)                      
308500       MOVE +1    TO IX-BEEMB                                             
308600       PERFORM UNTIL IX-BEEMB > 20                                        
308700         IF IX-BEEMB = 1                                                  
308800          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(17:5)          
308900         END-IF                                                           
309000         IF IX-BEEMB = 2                                                  
309100          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(22:5)          
309200         END-IF                                                           
309300         IF IX-BEEMB = 3                                                  
309400          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(27:5)          
309500         END-IF                                                           
309600         IF IX-BEEMB = 4                                                  
309700          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(32:5)          
309800         END-IF                                                           
309900         IF IX-BEEMB = 5                                                  
310000          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(37:5)          
310100         END-IF                                                           
310200         IF IX-BEEMB = 6                                                  
310300          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(42:5)          
310400         END-IF                                                           
310500         IF IX-BEEMB = 7                                                  
310600          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(47:5)          
310700         END-IF                                                           
310800         IF IX-BEEMB = 8                                                  
310900          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(52:5)          
311000         END-IF                                                           
311100         IF IX-BEEMB = 9                                                  
311200          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(1:5)           
311300         END-IF                                                           
311400         IF IX-BEEMB = 10                                                 
311500          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(6:5)           
311600         END-IF                                                           
311700         IF IX-BEEMB = 11                                                 
311800          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(11:5)          
311900         END-IF                                                           
312000         IF IX-BEEMB = 12                                                 
312100          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(16:5)          
312200         END-IF                                                           
312300         IF IX-BEEMB = 13                                                 
312400          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(21:5)          
312500         END-IF                                                           
312600         IF IX-BEEMB = 14                                                 
312700          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(26:5)          
312800         END-IF                                                           
312900         IF IX-BEEMB = 15                                                 
313000          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(31:5)          
313100         END-IF                                                           
313200         IF IX-BEEMB = 16                                                 
313300          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(36:5)          
313400         END-IF                                                           
313500         IF IX-BEEMB = 17                                                 
313600          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(41:5)          
313700         END-IF                                                           
313800         IF IX-BEEMB = 18                                                 
313900          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(46:5)          
314000         END-IF                                                           
314100         IF IX-BEEMB = 19                                                 
314200          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(51:5)          
314300         END-IF                                                           
314400         IF IX-BEEMB = 20                                                 
314500          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(56:5)          
314600         END-IF                                                           
314700         ADD +1 TO IX-BEEMB                                               
314800       END-PERFORM                                                        
314900     END-IF                                                               
315000                                                                          
315100     MOVE 'STOCK BALANCE'   TO MAIL-TEMAIL (22)(1:13)                     
315200     MOVE 'AK-BALANCE'      TO MAIL-TEMAIL (22)(15:10)                    
315300     MOVE 'QANT TO SCRAP'   TO MAIL-TEMAIL (22)(27:13)                    
315400     MOVE 'REM STOCK'       TO MAIL-TEMAIL (22)(41:9)                     
315500     MOVE 'TPO-TOTAL'       TO MAIL-TEMAIL (22)(51:9)                     
315600                                                                          
315700     MOVE 'CDC'             TO MAIL-TEMAIL (23)(1:3)                      
315800                                                                          
315900     MOVE 'DC'              TO MAIL-TEMAIL (24)(1:2)                      
316000     MOVE 'SS CODE'         TO MAIL-TEMAIL (24)(51:7)                     
316100                                                                          
316200     MOVE 'PART IN VEHICLE' TO MAIL-TEMAIL (25)(1:15)                     
316300                                                                          
316400     IF MAIL-IDMAIL = SPACE                                               
316500       CONTINUE                                                           
316600     ELSE                                                                 
316700       PERFORM IMS-PURG-MAIL                                              
316800     END-IF                                                               
316900     .                                                                    
317000     EJECT                                                                
317100                                                                          
317200 S95-JUMP-6325             SECTION.                                       
317300     MOVE MSGI-KDARBTYP        TO  MOD6325-MID-KDARBTYP-IN                
317400     COMPUTE P-TO-P-KVLL       =  LENGTH OF MOD6325-MID-W6I32501          
317500                                   +  17                                  
317600     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
317700     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
317800     MOVE 'W6T325  '           TO P-TO-P-KDTRANS                          
317900     MOVE '6323'               TO P-TO-P-IDTRANS                          
318000     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
318100                                                                          
318200     MOVE MOD6325-MID-W6I32501 TO P-TO-P-DATA                             
318300     PERFORM IMS-ISRT-ALT1-MSG                                            
318400     .                                                                    
318500     SKIP3                                                                
318600                                                                          
318700 MFS-RENSA-FAELT-UT SECTION.                                              
318800*    --- ALLA UTDATA-FÄLT                                                 
318900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
319000     MOVE MFS-RENSA-FAELT TO MOD-FLKLAR                                   
319100     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
319200     .                                                                    
319300     SKIP3                                                                
319400                                                                          
319500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
319600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
319700     MOVE +1 TO RAD-IX                                                    
319800     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
319900        MOVE MFS-RENSA-FAELT TO MOD-CMD-IN(RAD-IX)                        
320000                                MOD-IDARTNR(RAD-IX)                       
320100                                MOD-KVANTAL(RAD-IX)                       
320200                                MOD-SUBEL(RAD-IX)                         
320300                                MOD-TIDATUM(RAD-IX)                       
320400                                MOD-IDDC(RAD-IX)                          
320500                                MOD-IDUSER(RAD-IX)                        
320600                                MOD-BEANST(RAD-IX)                        
320700                                MOD-FLCLASS(RAD-IX)                       
320800                                MOD-FLTEXT(RAD-IX)                        
320900        ADD +1 TO RAD-IX                                                  
321000     END-PERFORM                                                          
321100     .                                                                    
321200     EJECT                                                                
321300                                                                          
321400 MFS-RENSA-FAELT-IN SECTION.                                              
321500*    --- ALLA INDATA-FÄLT                                                 
321600     MOVE MFS-RENSA-FAELT TO MOD-FLKLAR                                   
321700     MOVE +1 TO RAD-IX                                                    
321800     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
321900        MOVE MFS-RENSA-FAELT TO MOD-CMD-IN(RAD-IX)                        
322000                                MOD-IDARTNR(RAD-IX)                       
322100                                MOD-IDDC(RAD-IX)                          
322200                                MOD-TIDATUM(RAD-IX)                       
322300        ADD +1 TO RAD-IX                                                  
322400     END-PERFORM                                                          
322500     .                                                                    
322600     SKIP2                                                                
322700                                                                          
322800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
322900*    --- ALLA UTDATA-FÄLT                                                 
323000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
323100     MOVE MFS-ROER-EJ-FAELT TO MOD-FLKLAR                                 
323200     PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                     
323300     .                                                                    
323400     EJECT                                                                
323500                                                                          
323600 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
323700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
323800     MOVE +1 TO RAD-IX                                                    
323900     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
324000        MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-IN(RAD-IX)                      
324100                                  MOD-IDARTNR(RAD-IX)                     
324200                                  MOD-KVANTAL(RAD-IX)                     
324300                                  MOD-SUBEL(RAD-IX)                       
324400                                  MOD-TIDATUM(RAD-IX)                     
324500                                  MOD-IDDC(RAD-IX)                        
324600                                  MOD-IDUSER(RAD-IX)                      
324700                                  MOD-BEANST(RAD-IX)                      
324800                                  MOD-FLCLASS(RAD-IX)                     
324900                                  MOD-FLTEXT(RAD-IX)                      
325000        ADD +1 TO RAD-IX                                                  
325100     END-PERFORM                                                          
325200     .                                                                    
325300     SKIP2                                                                
325400                                                                          
325500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
325600*    --- ALLA INDATA-FÄLT                                                 
325700     MOVE MFS-ROER-EJ-FAELT TO MOD-FLKLAR                                 
325800     MOVE +1 TO RAD-IX                                                    
325900     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
326000        MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-IN(RAD-IX)                      
326100        ADD +1 TO RAD-IX                                                  
326200     END-PERFORM                                                          
326300     .                                                                    
326400     EJECT                                                                
326500                                                                          
326600 MFS-FORM-ATTR SECTION.                                                   
326700*    --- ALLA INDATA-FÄLT                                                 
326800     MOVE MFS-FORMATETS-ATTR TO MOD-FLKLAR-ATTR                           
326900     MOVE +1 TO RAD-IX                                                    
327000     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
327100        MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR(RAD-IX)                   
327200        ADD +1 TO RAD-IX                                                  
327300     END-PERFORM                                                          
327400     .                                                                    
327500     SKIP2                                                                
327600                                                                          
327700 MFS-RENSA-SPAR-NYCKLAR SECTION.                                          
327800     MOVE MFS-RENSA-FAELT TO SPAR-IDARTNR-ENTER                           
327900                             SPAR-IDARTNR-NEXT                            
328000                             SPAR-IDDC-ENTER                              
328100                             SPAR-IDDC-NEXT                               
328200                             SPAR-IDANSK-ENTER                            
328300                             SPAR-IDANSK-NEXT                             
328400                             SPAR-DASKROT9-BEORD-ENTER                    
328500                             SPAR-DASKROT9-BEORD-NEXT                     
328600                             SPAR-KDARBTYP-NEXT                           
328700                             SPAR-KDARBTYP-ENTER                          
328800     .                                                                    
328900     EJECT                                                                
329000* --- IMS SEKTIONER ---                                                   
329100     SKIP3                                                                
329200                                                                          
329300 IMS-GET-MSG SECTION.                                                     
329400     MOVE '  QC' TO GODK-STATUSKODER                                      
329500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
329600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
329700     PERFORM IMS-STATUSKONTROLL                                           
329800     .                                                                    
329900     SKIP3                                                                
330000                                                                          
330100 IMS-INSERT-MSG SECTION.                                                  
330200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
330300     MOVE SPACE TO GODK-STATUSKODER                                       
330400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
330500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
330600     PERFORM IMS-STATUSKONTROLL                                           
330700     .                                                                    
330800     SKIP3                                                                
330900                                                                          
331000 IMS-INSERT-ALTMSG SECTION.                                               
331100     MOVE SPACE TO GODK-STATUSKODER                                       
331200     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
331300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
331400     PERFORM IMS-STATUSKONTROLL                                           
331500     .                                                                    
331600     EJECT                                                                
331700                                                                          
331800 IMS-GHU-ARTC01 SECTION.                                                  
331900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
332000          DELIMITED BY SIZE INTO SSA1                                     
332100     MOVE '  GE' TO GODK-STATUSKODER                                      
332200     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC01 SSA1                 
332300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
332400     PERFORM IMS-STATUSKONTROLL                                           
332500     .                                                                    
332600     SKIP3                                                                
332700                                                                          
332800 IMS-GHU-ARTC11 SECTION.                                                  
332900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
333000          DELIMITED BY SIZE INTO SSA1                                     
333100     MOVE 'WDK611   ' TO SSA2                                             
333200     MOVE '  GE' TO GODK-STATUSKODER                                      
333300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2            
333400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
333500     PERFORM IMS-STATUSKONTROLL                                           
333600     .                                                                    
333700     SKIP3                                                                
333800                                                                          
333900 IMS-GNP-ARTC23 SECTION.                                                  
334000     MOVE  'WDK623   ' TO  SSA1                                           
334100     MOVE '  GE' TO GODK-STATUSKODER                                      
334200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC23 SSA1                 
334300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
334400     PERFORM IMS-STATUSKONTROLL                                           
334500     .                                                                    
334600     SKIP2                                                                
334700 IMS-REPL-ARTC11 SECTION.                                                 
334800     MOVE '  ' TO GODK-STATUSKODER                                        
334900     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
335000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
335100     PERFORM IMS-STATUSKONTROLL                                           
335200     .                                                                    
335300     SKIP3                                                                
335400 IMS-GU-WDT301 SECTION.                                                   
335500     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
335600          DELIMITED BY SIZE INTO SSA1                                     
335700     MOVE '  GE' TO GODK-STATUSKODER                                      
335800     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT301 SSA1                    
335900     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
336000     PERFORM IMS-STATUSKONTROLL                                           
336100     .                                                                    
336200     SKIP3                                                                
336300                                                                          
336400 IMS-ISRT-WDT301 SECTION.                                                 
336500     MOVE 'WDT301   ' TO SSA1                                             
336600     MOVE '    ' TO GODK-STATUSKODER                                      
336700     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
336800     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
336900     PERFORM IMS-STATUSKONTROLL                                           
337000     .                                                                    
337100     EJECT                                                                
337200 IMS-ISRT-WDT311 SECTION.                                                 
337300     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
337400          DELIMITED BY SIZE INTO SSA1                                     
337500     MOVE 'WDT311   ' TO SSA2                                             
337600     MOVE '    ' TO GODK-STATUSKODER                                      
337700     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
337800     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
337900     PERFORM IMS-STATUSKONTROLL                                           
338000     .                                                                    
338100     EJECT                                                                
338200                                                                          
338300 IMS-GHU-ARTS11 SECTION.                                                  
338400     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
338500          DELIMITED BY SIZE INTO SSA1                                     
338600     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
338700          DELIMITED BY SIZE INTO SSA2                                     
338800     MOVE '  GE' TO GODK-STATUSKODER                                      
338900     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2            
339000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
339100     PERFORM IMS-STATUSKONTROLL                                           
339200     .                                                                    
339300     SKIP3                                                                
339400                                                                          
339500 IMS-REPL-ARTS11 SECTION.                                                 
339600     MOVE '  ' TO GODK-STATUSKODER                                        
339700     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-WLARTS11                     
339800     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
339900     PERFORM IMS-STATUSKONTROLL                                           
340000     .                                                                    
340100     SKIP3                                                                
340200                                                                          
340300 IMS-REPL-WDGX6324 SECTION.                                               
340400     MOVE '  ' TO GODK-STATUSKODER                                        
340500     CALL CBLTDLI USING REPL 6321-PCB DLI-IO-WDGX6324                     
340600     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
340700     PERFORM IMS-STATUSKONTROLL                                           
340800     .                                                                    
340900     EJECT                                                                
341000                                                                          
341100 IMS-GHU-WDR501-6321 SECTION.                                             
341200     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
341300          DELIMITED BY SIZE INTO SSA1                                     
341400     MOVE 'GE  ' TO GODK-STATUSKODER                                      
341500     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDR501-6321 SSA1              
341600     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
341700     PERFORM IMS-STATUSKONTROLL                                           
341800     .                                                                    
341900     SKIP3                                                                
342000                                                                          
342100 IMS-GNP-WDGX6322 SECTION.                                                
342200     STRING 'WDGX6322(DASKROT9=>' W-DASKROT9-MIN-X                        
342300                    '&DASKROT9=<' W-DASKROT9-MAX-X ')'                    
342400          DELIMITED BY SIZE INTO SSA1                                     
342500     MOVE '  GE' TO GODK-STATUSKODER                                      
342600     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6322 SSA1                 
342700     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
342800     PERFORM IMS-STATUSKONTROLL                                           
342900     .                                                                    
343000     EJECT                                                                
343100                                                                          
343200 IMS-DLET-WDGX6324 SECTION.                                               
343300     MOVE '  ' TO GODK-STATUSKODER                                        
343400     CALL CBLTDLI USING DLET 6321-PCB DLI-IO-WDGX6324                     
343500     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
343600     PERFORM IMS-STATUSKONTROLL                                           
343700     .                                                                    
343800     SKIP2                                                                
343900                                                                          
344000 IMS-GNP-WDGX6324-ART SECTION.                                            
344100     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
344200          DELIMITED BY SIZE INTO SSA1                                     
344300     STRING 'WDGX6324(IDARTNR =>' W-IDARTNR-MIN-X                         
344400                    '&IDARTNR =<' W-IDARTNR-MAX-X                         
344500                    '&IDDC     =' W-IDDC-6324-X                           
344600                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
344700          DELIMITED BY SIZE INTO SSA2                                     
344800     MOVE '  GE' TO GODK-STATUSKODER                                      
344900     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2            
345000     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
345100     PERFORM IMS-STATUSKONTROLL                                           
345200     .                                                                    
345300     SKIP2                                                                
345400                                                                          
345500 IMS-GU-WDGX6324 SECTION.                                                 
345600     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
345700          DELIMITED BY SIZE INTO SSA1                                     
345800     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
345900                    '&IDDC     =' W-IDDC-6324-X                           
346000                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
346100          DELIMITED BY SIZE INTO SSA2                                     
346200     MOVE '  GE' TO GODK-STATUSKODER                                      
346300     CALL CBLTDLI USING GHNP 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2           
346400     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
346500     PERFORM IMS-STATUSKONTROLL                                           
346600     .                                                                    
346700     SKIP2                                                                
346800                                                                          
346900 IMS-GU-WDGX6324-3 SECTION.                                               
347000     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
347100            DELIMITED BY SIZE INTO SSA1                                   
347200     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
347300          DELIMITED BY SIZE INTO SSA2                                     
347400     STRING 'WDGX6324(KY6324   =' W-KY6324-X       ')'                    
347500          DELIMITED BY SIZE INTO SSA3                                     
347600     MOVE '  GE' TO GODK-STATUSKODER                                      
347700     CALL CBLTDLI USING GU 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2             
347800                                                      SSA3                
347900     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
348000     PERFORM IMS-STATUSKONTROLL                                           
348100     .                                                                    
348200     SKIP2                                                                
348300                                                                          
348400 IMS-GU-WDGX6324-2 SECTION.                                               
348500     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
348600            DELIMITED BY SIZE INTO SSA1                                   
348700     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
348800          DELIMITED BY SIZE INTO SSA2                                     
348900     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
349000                    '&IDDC     =' W-IDDC-6324-X                           
349100                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
349200          DELIMITED BY SIZE INTO SSA3                                     
349300     MOVE '  GE' TO GODK-STATUSKODER                                      
349400     CALL CBLTDLI USING GU 6326-PCB DLI-IO-WDGX6324 SSA1 SSA2             
349500                                                      SSA3                
349600     MOVE 6326-STATUS-CODE TO STATUS-WS                                   
349700     PERFORM IMS-STATUSKONTROLL                                           
349800     .                                                                    
349900     SKIP2                                                                
350000                                                                          
350100 IMS-ISRT-WDGX6326 SECTION.                                               
350200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
350300            DELIMITED BY SIZE INTO SSA1                                   
350400     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
350500            DELIMITED BY SIZE INTO SSA2                                   
350600     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
350700                    '&IDDC     =' W-IDDC-6324-X                           
350800                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
350900          DELIMITED BY SIZE INTO SSA3                                     
351000     MOVE 'WDGX6326 '           TO SSA4                                   
351100     MOVE '    '                TO GODK-STATUSKODER                       
351200     CALL CBLTDLI USING ISRT 6326-PCB DLI-IO-WDGX6326                     
351300                                      SSA1 SSA2 SSA3 SSA4                 
351400     MOVE 6326-STATUS-CODE      TO STATUS-WS                              
351500     PERFORM IMS-STATUSKONTROLL                                           
351600     .                                                                    
351700     EJECT                                                                
351800                                                                          
351900 IMS-GNP-WDGX6326-2 SECTION.                                              
352000     STRING 'WDGX6326(TIDATETI=>' W-TIDATETIME-MIN-X                      
352100                    '&TIDATETI=<' W-TIDATETIME-MAX-X ')'                  
352200          DELIMITED BY SIZE INTO SSA1                                     
352300     MOVE '  GE'                TO GODK-STATUSKODER                       
352400     CALL CBLTDLI USING GNP 6326-PCB DLI-IO-WDGX6326                      
352500                                      SSA1                                
352600     MOVE 6326-STATUS-CODE      TO STATUS-WS                              
352700     PERFORM IMS-STATUSKONTROLL                                           
352800     .                                                                    
352900     EJECT                                                                
353000                                                                          
353100 IMS-ISRT-WLFILC SECTION.                                                 
353200     STRING 'WLFILC01    '                                                
353300          DELIMITED BY SIZE INTO SSA1                                     
353400     MOVE '   ' TO GODK-STATUSKODER                                       
353500     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
353600     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
353700     PERFORM IMS-STATUSKONTROLL                                           
353800     .                                                                    
353900     EJECT                                                                
354000                                                                          
354100 IMS-GHU-WDGX6322 SECTION.                                                
354200     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
354300          DELIMITED BY SIZE INTO SSA1                                     
354400     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
354500          DELIMITED BY SIZE INTO SSA2                                     
354600     MOVE '  GE' TO GODK-STATUSKODER                                      
354700     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2            
354800     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
354900     PERFORM IMS-STATUSKONTROLL                                           
355000     .                                                                    
355100     SKIP2                                                                
355200                                                                          
355300 IMS-GNP-WDGX6324 SECTION.                                                
355400     MOVE 'WDGX6324 ' TO SSA1                                             
355500     MOVE '  GE' TO GODK-STATUSKODER                                      
355600     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6324 SSA1                 
355700     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
355800     PERFORM IMS-STATUSKONTROLL                                           
355900     .                                                                    
356000     SKIP3                                                                
356100                                                                          
356200 IMS-GU-WDGX6324-6325 SECTION.                                            
356300     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
356400            DELIMITED BY SIZE INTO SSA1                                   
356500     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
356600            DELIMITED BY SIZE INTO SSA2                                   
356700     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
356800          DELIMITED BY SIZE INTO SSA3                                     
356900     MOVE '  GE' TO GODK-STATUSKODER                                      
357000     CALL CBLTDLI USING GU 6325-PCB DLI-IO-WDGX6324                       
357100                                      SSA1 SSA2 SSA3                      
357200     MOVE 6325-STATUS-CODE TO STATUS-WS                                   
357300     PERFORM IMS-STATUSKONTROLL                                           
357400     .                                                                    
357500     SKIP3                                                                
357600                                                                          
357700 IMS-GNP-WDGX6325 SECTION.                                                
357800     MOVE 'WDGX6325 '  TO SSA1                                            
357900     MOVE '  GE' TO GODK-STATUSKODER                                      
358000     CALL CBLTDLI USING GNP 6325-PCB DLI-IO-WDGX6325 SSA1                 
358100     MOVE 6325-STATUS-CODE TO STATUS-WS                                   
358200     PERFORM IMS-STATUSKONTROLL                                           
358300     .                                                                    
358400     SKIP3                                                                
358500                                                                          
358600 IMS-DLET-WDGX6322 SECTION.                                               
358700     MOVE '  ' TO GODK-STATUSKODER                                        
358800     CALL CBLTDLI USING DLET 6321-PCB DLI-IO-WDGX6322                     
358900     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
359000     PERFORM IMS-STATUSKONTROLL                                           
359100     .                                                                    
359200     EJECT                                                                
359300                                                                          
359400 IMS-GU-WDGX6327 SECTION.                                                 
359500     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6327 ')'                      
359600          DELIMITED BY SIZE INTO SSA1                                     
359700     MOVE '  GE' TO GODK-STATUSKODER                                      
359800     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6328 SSA1                  
359900     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
360000     PERFORM IMS-STATUSKONTROLL                                           
360100     .                                                                    
360200     SKIP2                                                                
360300                                                                          
360400 IMS-GNP-WDGX6328 SECTION.                                                
360500     MOVE 'WDGX6328' TO SSA1                                              
360600     MOVE '  GE' TO GODK-STATUSKODER                                      
360700     CALL CBLTDLI USING GNP 6327-PCB DLI-IO-WDGX6328 SSA1                 
360800     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
360900     PERFORM IMS-STATUSKONTROLL                                           
361000     .                                                                    
361100     SKIP2                                                                
361200                                                                          
361300 IMS-GU-WDGX6328 SECTION.                                                 
361400     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6327 ')'                      
361500          DELIMITED BY SIZE INTO SSA1                                     
361600     STRING 'WDGX6328(SUBEL   =>' W-SUBEL-MIN-X                           
361700                    '&SUBEL   =<' W-SUBEL-MAX-X                           
361800                    '&IDUSERGK= ' W-IDUSER-GODK-X ')'                     
361900          DELIMITED BY SIZE INTO SSA2                                     
362000     MOVE '  GE' TO GODK-STATUSKODER                                      
362100     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6328 SSA1 SSA2             
362200     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
362300     PERFORM IMS-STATUSKONTROLL                                           
362400     .                                                                    
362500     SKIP2                                                                
362600                                                                          
362700 IMS-ISRT-WDGX2402 SECTION.                                               
362800     STRING 'WDR501  (WDGXKEY  =' W-2401-KEY-X ')'                        
362900            DELIMITED BY SIZE INTO SSA1                                   
363000     MOVE 'WDGX2402'            TO SSA2                                   
363100     MOVE '  '                  TO GODK-STATUSKODER                       
363200     CALL CBLTDLI USING ISRT 2401-PCB DLI-IO-WDR501-2401 SSA1 SSA2        
363300     MOVE 2401-STATUS-CODE      TO STATUS-WS                              
363400     PERFORM IMS-STATUSKONTROLL                                           
363500     SKIP3                                                                
363600     .                                                                    
363700     EJECT                                                                
363800                                                                          
363900 IMS-GU-WDD801 SECTION.                                                   
364000     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
364100          DELIMITED BY SIZE INTO SSA1                                     
364200     MOVE '  GE' TO GODK-STATUSKODER                                      
364300     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
364400     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
364500     PERFORM IMS-STATUSKONTROLL                                           
364600     .                                                                    
364700     EJECT                                                                
364800                                                                          
364900 IMS-GHNP-WDD811 SECTION.                                                 
365000     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
365100          DELIMITED BY SIZE INTO SSA1                                     
365200     STRING 'WDD811   '                                                   
365300          DELIMITED BY SIZE INTO SSA2                                     
365400     MOVE '  GE' TO GODK-STATUSKODER                                      
365500     CALL CBLTDLI USING GHNP WDD8-PCB DLI-IO-WDD811 SSA1 SSA2             
365600     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
365700     PERFORM IMS-STATUSKONTROLL                                           
365800     .                                                                    
365900     SKIP3                                                                
366000                                                                          
366100 IMS-REPL-WDD811 SECTION.                                                 
366200     MOVE '  ' TO GODK-STATUSKODER                                        
366300     CALL CBLTDLI USING REPL WDD8-PCB DLI-IO-WDD811                       
366400     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
366500     PERFORM IMS-STATUSKONTROLL                                           
366600     .                                                                    
366700     EJECT                                                                
366800                                                                          
366900 IMS-DLET-WDD811 SECTION.                                                 
367000     MOVE '  ' TO GODK-STATUSKODER                                        
367100     CALL CBLTDLI USING DLET WDD8-PCB DLI-IO-WDD811                       
367200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
367300     PERFORM IMS-STATUSKONTROLL                                           
367400     .                                                                    
367500     EJECT                                                                
367600                                                                          
367700 IMS-GU-WDJ901 SECTION.                                                   
367800     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
367900          DELIMITED BY SIZE INTO SSA1                                     
368000     MOVE '  GE' TO GODK-STATUSKODER                                      
368100     CALL CBLTDLI USING GU WDJ9-PCB DLI-IO-WDJ901 SSA1                    
368200     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
368300     PERFORM IMS-STATUSKONTROLL                                           
368400     .                                                                    
368500     EJECT                                                                
368600                                                                          
368700 IMS-GHNP-WDJ911 SECTION.                                                 
368800     MOVE '  GE' TO GODK-STATUSKODER                                      
368900     CALL CBLTDLI USING GHNP WDJ9-PCB DLI-IO-WDJ911                       
369000     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
369100     PERFORM IMS-STATUSKONTROLL                                           
369200                                                                          
369300     EJECT                                                                
369400     .                                                                    
369500                                                                          
369600 IMS-REPL-WDJ911 SECTION.                                                 
369700     MOVE '  ' TO GODK-STATUSKODER                                        
369800     CALL CBLTDLI USING REPL WDJ9-PCB DLI-IO-WDJ911                       
369900     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
370000     PERFORM IMS-STATUSKONTROLL                                           
370100     .                                                                    
370200     EJECT                                                                
370300 IMS-ISRT-ZZAC SECTION.                                                   
370400                                                                          
370500     MOVE 'WLZZAC01 '        TO SSA1                                      
370600     MOVE '  '               TO GODK-STATUSKODER                          
370700     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA5 SSA1                   
370800     MOVE ZZAC-STATUS-CODE     TO STATUS-WS                               
370900     PERFORM IMS-STATUSKONTROLL                                           
371000     .                                                                    
371100     SKIP3                                                                
371200 IMS-ISRT-WDG901 SECTION.                                                 
371300                                                                          
371400     MOVE 'WLZZAD01 ' TO SSA1                                             
371500     MOVE '  II' TO GODK-STATUSKODER                                      
371600     CALL CBLTDLI USING ISRT ZZAD-PCB DLI-IO-WDG901 SSA1                  
371700     MOVE ZZAD-STATUS-CODE TO STATUS-WS                                   
371800     PERFORM IMS-STATUSKONTROLL                                           
371900     .                                                                    
372000     EJECT                                                                
372100                                                                          
372200                                                                          
372300 IMS-PURG-MAIL SECTION.                                                   
372400     MOVE SPACE TO GODK-STATUSKODER                                       
372500     CALL CBLTDLI USING PURG ALTMAIL-PCB MAIL-WMSGMAIL                    
372600     MOVE ALTMAIL-STATUS-CODE TO STATUS-WS                                
372700     PERFORM IMS-STATUSKONTROLL                                           
372800     .                                                                    
372900     EJECT                                                                
373000                                                                          
373100 IMS-ISRT-ALT1-MSG SECTION.                                               
373200     MOVE SPACE TO GODK-STATUSKODER                                       
373300     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW                           
373400     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
373500     PERFORM IMS-STATUSKONTROLL                                           
373600     .                                                                    
373700     SKIP2                                                                
373800 IMS-ISRT-ALT-PCB SECTION.                                                
373900     MOVE '  ' TO GODK-STATUSKODER                                        
374000     CALL CBLTDLI USING PURG ALT4-PCB W-PROG-TO-PROG-SW                   
374100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
374200     PERFORM IMS-STATUSKONTROLL                                           
374300     SKIP3                                                                
374400     .                                                                    
374500 IMS-ISRT-WDF212 SECTION.                                                 
374600                                                                          
374700     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
374800          DELIMITED BY SIZE INTO SSA1                                     
374900     MOVE 'WDF212    ' TO SSA2                                            
375000     MOVE '  IINI' TO GODK-STATUSKODER                                    
375100     CALL CBLTDLI USING ISRT WDF2-PCB DLI-IO-WDF212 SSA1 SSA2             
375200     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
375300     PERFORM IMS-STATUSKONTROLL                                           
375400     .                                                                    
375500     EJECT                                                                
375600                                                                          
375700 IMS-GN-WDB601    SECTION.                                                
375800     MOVE 'WDB601  ' TO SSA1                                              
375900     MOVE '  GB'     TO GODK-STATUSKODER                                  
376000     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
376100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
376200     PERFORM IMS-STATUSKONTROLL                                           
376300     .                                                                    
376400 IMS-STATUSKONTROLL SECTION.                                              
376500     SET STATUS-IX TO 1                                                   
376600     SEARCH GODK-STATUS                                                   
376700       AT END                                                             
376800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
376900         DELIMITED BY SIZE INTO FELTEXT                                   
377000         CALL FELLOG                                                      
377100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
377200         CONTINUE                                                         
377300     END-SEARCH                                                           
377400     .                                                                    
