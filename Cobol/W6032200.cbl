000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6032200.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   98/10/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        RELEASE SCRAP ORDERS                                             
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001100*                              WLARTS (WDK7)                              
001200*                                6321 (WDR5)                              
001300*                                6327 (WDR5)                              
001400*                                2402 (WDR5)                              
001500*                                WDF2                                     
001600*                                WDT3                                     
001700*                   STARTAR RUTIN W216S1 I SOP                            
001800*                   (SKAPAR SKROTORDER).                                  
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W6T322                                              
002200*        MID:         W6I32201                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W6O32201                                            
002600*                                                                         
002700*    E'TRACKER 4823800 20071128 NEW LDC                                   
002800*    E'TRACKER 10143271 DATE 2011-10-19 CHINA WAREHOUSE PROJECT-1         
002900*    E'TRACKER 10143273 DATE 2012-08-20 LOCAL SOURCING                    
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W6032200'.            
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100 77  LAES-SW                     PIC X     VALUE SPACE.                   
004200 77  WS-SKROTDATUM-SLUT          PIC X     VALUE SPACE.                   
004300 77  WS-DATUM-HITTAD             PIC X     VALUE SPACE.                   
004400 77  WS-ART-SLUT                 PIC X     VALUE SPACE.                   
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  PROGSW-IX                   PIC S9(3)  VALUE ZERO  COMP-3.           
004800 77  RAD-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
004900 77  RAD-IX-MAX                  PIC S9(3)  VALUE +12   COMP-3.           
005000 77  TAB-IX-MAX                  PIC S9(3)  VALUE +100  COMP-3.           
005100 77  URV-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
005200 77  URV-IX-MAX                  PIC S9(3)  VALUE +12   COMP-3.           
005300 77  IX-BEEMB                    PIC S9(4)   VALUE +0  COMP SYNC.         
005400 77  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
005500 77  MAX-IX                      PIC S9(4)   VALUE +11 COMP SYNC.         
005600 77  SPAR-IDDC                   PIC X(2)   VALUE SPACE.                  
005700 77  SPAR-IDARTNR                PIC S9(9)  VALUE ZERO COMP-3.            
005800 77  DAGENS-DATUM-Y2K            PIC 9(8)   VALUE ZERO.                   
005900 77  WS-ANTAL-X                  PIC 9(3)   VALUE ZERO COMP-3.            
006000 77  W-TID                       PIC 9(8)   VALUE ZERO.                   
006100 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
006200 77  TEST-IDINK                  PIC 9(3)    VALUE ZERO.                  
006300 77  WS-IDLOGLOP                 PIC 9(01) COMP-3 VALUE ZERO.             
006400 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
006500                                                                          
006600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006700     88  INDATA-OK                           VALUE 'J'.                   
006800     88  INDATA-FEL                          VALUE 'N'.                   
006900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007000     88  NYCKLAR-OK                          VALUE 'J'.                   
007100     88  NYCKLAR-FEL                         VALUE 'N'.                   
007200                                                                          
007300 01  NYCKLAR-TILL-DLI.                                                    
007400     03  W-IDDC-B6-X.                                                     
007500         05 W-IDDC-B6            PIC X(2).                                
007600                                                                          
007700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007800     88  EGEN-MID                            VALUE '6322'.                
007900     88  GODK-MID                            VALUE '6322' '6325'.         
008000 77  JUMP-6325-SW                PIC X       VALUE 'N'.                   
008100     88  JUMP-6325                           VALUE 'J'.                   
008200     88  NO-JUMP-6325                        VALUE 'N'.                   
008300 77  JUMP-2352-SW                PIC X       VALUE 'N'.                   
008400     88  JUMP-2352                           VALUE 'J'.                   
008500     88  NO-JUMP-2352                        VALUE 'N'.                   
008600 77  JUMP-2372-SW                PIC X       VALUE 'N'.                   
008700     88  JUMP-2372                           VALUE 'J'.                   
008800     88  NO-JUMP-2372                        VALUE 'N'.                   
008900 77  WS-CMD                      PIC X       VALUE SPACE.                 
009000 77  SW-R23                      PIC X       VALUE 'N'.                   
009100 77  WS-FL6326                   PIC X       VALUE 'N'.                   
009200                                                                          
009300 77  KDARBTYP-SOEKNING           PIC X       VALUE 'N'.                   
009400 77  KDARBTYP-PERSON-SOEKNING    PIC X       VALUE 'N'.                   
009500 77  IDARTNR-SOEKNING            PIC X       VALUE 'N'.                   
009600 77  DATUM-SOEKNING              PIC X       VALUE 'N'.                   
009700 77  DATUM-IDARTNR-SOEKNING      PIC X       VALUE 'N'.                   
009800 77  IDDC-SOEKNING               PIC X       VALUE 'N'.                   
009900 77  IDPERSON-SOEKNING           PIC X       VALUE 'N'.                   
010000 77  WS-DASKROT9                 PIC 9(8)    VALUE ZERO.                  
010100 77  WS-SPAR-DASKROT9            PIC 9(8)    VALUE ZERO.                  
010200 77  WS-SUARTSTD                 PIC 9(7)V9(2) VALUE ZERO.                
010300 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
010400 77  WS-DASKROT9-BEORD           PIC 9(8)    VALUE ZERO.                  
010500 77  WS-6322-DASKROT9            PIC 9(8)    VALUE ZERO.                  
010600 77  WS-IDPERSON-FOM             PIC X(3)    VALUE ZERO.                  
010700 77  WS-IDPERSON-TOM             PIC X(3)    VALUE ZERO.                  
010800 77  W-KVSKROT-REST              PIC S9(7)  VALUE ZERO.                   
010900 77  W-ADBUFFPL                  PIC 9(5)  VALUE ZERO.                    
011000 77  TEST-NYCKEL-IDARTNR         PIC X(9)    VALUE SPACE.                 
011100 77  TEST-NYCKEL-TIDATUM         PIC X(6)    VALUE SPACE.                 
011200 77  TEST-NYCKEL-IDANSK          PIC X(3)    VALUE SPACE.                 
011300 77  WS-FLHOGRE                  PIC X       VALUE 'N'.                   
011400 77  WS-FLURVAL                  PIC X       VALUE 'N'.                   
011500 77  WS-HIGHLEV                  PIC X       VALUE 'N'.                   
011600 77  WS-SUBEL                    PIC 9(7)    VALUE ZERO.                  
011700 77  W-ANNUL-IDUSER              PIC X(8)    VALUE SPACE.                 
011800 77  W-ANNUL-IDMAIL              PIC X(60)   VALUE SPACE.                 
011900 01  ANUL-BEANST-GODK            PIC X(25) VALUE SPACE.                   
012000 01  ANUL-IDMAIL                 PIC X(60) VALUE SPACE.                   
012100 01  WSM-IDARTNR                 PIC Z(8)9 VALUE ZERO.                    
012200 77  WS-KDERS-UTG                PIC Z(2)    VALUE ZERO.                  
012300 77  WS-SUTPO-TOT                PIC Z(6)9   VALUE ZERO.                  
012400 77  WS-KVSKROT-BEORD            PIC Z(6)9   VALUE ZERO.                  
012500 77  WS-KVSKROT-KVAR             PIC Z(6)9   VALUE ZERO.                  
012600 77  WS-KVTILLG-CDC              PIC Z(6)9   VALUE ZERO.                  
012700 77  WS-KVTILLG-SDC              PIC Z(6)9   VALUE ZERO.                  
012800 77  WS-KVAKS-CDC                PIC Z(6)9   VALUE ZERO.                  
012900 77  WS-KVAKS-SDC                PIC Z(6)9   VALUE ZERO.                  
013000 77  WS-IDLEVNR-NUM              PIC 9(5)    VALUE ZERO.                  
013100 77  WS-IDLEVNR-8                PIC X(8)    VALUE SPACE.                 
013200 77  WS-IDARTNR-8                PIC 9(08)   VALUE ZERO.                  
013300 77  WS-VAR                      PIC X       VALUE SPACE.                 
013400     EJECT                                                                
013500 01  TABELL-SKROT-VARDE.                                                  
013600     03  W-VARDE-PER-RAD         OCCURS 12.                               
013700         05 W-SUARTSTD           PIC 9(7)V9(2) VALUE ZERO.                
013800                                                                          
013900 01  TABELL-TEMAIL.                                                       
014000     03  W-TEMAIL-RAD            OCCURS 11.                               
014100         05 W-TEMAIL             PIC X(66) VALUE SPACE.                   
014200*      --- VALID IDDC CODES                                               
014300*                                                                         
014400*01    -COPY WWDC99                                                       
014500*01    -COPY WWDC99 -PRE SW-                                              
014600                                                                          
014700*01    -COPY WWPRODSL                                                     
014800                                                                          
014900 01  IDDC-TABELL.                                                         
015000     03 WS-IDDC-ARRAY  OCCURS 100 INDEXED BY TAB-IX.                      
015100       05 TAB-IDDC               PIC X(2).                                
015200                                                                          
015300 01  WS-SEKEL-KOLL               PIC 9(6).                                
015400 01  FILLER REDEFINES WS-SEKEL-KOLL.                                      
015500     03  WS-SEKEL                PIC 9(1).                                
015600     03  FILLER                  PIC 9(5).                                
015700                                                                          
015800 01  WS-TIDATETIME               PIC X(14).                               
015900 01  FILLER REDEFINES WS-TIDATETIME.                                      
016000     03  WS-DATUM                PIC 9(8).                                
016100     03  WS-TIDHHMMSS            PIC 9(6).                                
016200                                                                          
016300 01  WS-TID                      PIC 9(8) VALUE ZERO.                     
016400 01  WS-DAREGDAT                 PIC 9(8).                                
016500                                                                          
016600 01  WS-DASKROT.                                                          
016700     03  WS-DASKROT-SS                PIC 9(2).                           
016800     03  WS-DASKROT-AAMMDD            PIC 9(6).                           
016900 01  WS-AAAAMMDD REDEFINES WS-DASKROT PIC 9(8).                           
017000                                                                          
017100 01  TEST-SUARTSTD         PIC X(10).                                     
017200 01  FILLER REDEFINES TEST-SUARTSTD.                                      
017300     03  TEST-HELTAL           PIC 9(7).                                  
017400     03  FILLER                PIC X.                                     
017500     03  TEST-DECIMAL          PIC 9(2).                                  
017600                                                                          
017700 01  W-IDAVTAL-RED               PIC 9(13).                               
017800 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
017900     03  FILLER                  PIC X.                                   
018000     03  W-PREFIX                PIC X(3).                                
018100     03  W-AVTALSNR              PIC X(6).                                
018200     03  W-SUFFIX                PIC X(3).                                
018300     SKIP2                                                                
018400                                                                          
018500 01  WS-BC-PARAMETRAR.                                                    
018600     03  WS-URVAL.                                                        
018700         05  URV-FLKLAR       PIC X     VALUE SPACE.                      
018800         05  URV-KDARBTYP     PIC X(8)  VALUE SPACE.                      
018900     03  URV-TABELL.                                                      
019000         05 URV-TAB-RAD OCCURS 12.                                        
019100            07  URV-IDDC             PIC X(2).                            
019200            07  URV-IDARTNR          PIC 9(9).                            
019300            07  URV-DASKROT9-BEORD   PIC 9(8).                            
019400 01  BAS-R22-REGPOST.                                                     
019500*    03      -COPY W212R22   -PRE BAS-R22-                                
019600     03 BAS-R22-REST            PIC X(41).                                
019700     EJECT                                                                
019800 01  BAS-R23-REGPOST.                                                     
019900*    03      -COPY W212R23   -PRE BAS-R23-                                
020000     03 BAS-R23-REST            PIC X(41).                                
020100     EJECT                                                                
020200 77  WS-IX                      PIC S9(3)  VALUE ZERO  COMP-3.            
020300 77  WO-IX                      PIC S9(3)  VALUE ZERO  COMP-3.            
020400 77  O-IX                       PIC S9(3)  VALUE ZERO  COMP-3.            
020500 77  SW-VISA-6325               PIC X      VALUE 'N'.                     
020600 77  WO-IDARTNR                 PIC S9(9)  VALUE ZERO COMP-3.             
020700 77  WO-DASKROT9                PIC 9(8)   VALUE ZERO.                    
020800 77  WO-IDDC-6325               PIC X(2)   VALUE SPACE.                   
020900                                                                          
021000 01  WO-TEMFSINF-TAB.                                                     
021100     03  WO-TEMFSINF-ORS  OCCURS 6 PIC X(9).                              
021200                                                                          
021300 01  WS-TEMEMO-TAB.                                                       
021400     03  FILLER      PIC X(66)  VALUE 'SPÄRRAD KVANT       '.             
021500     03  FILLER      PIC X(66)  VALUE 'INGÅR I SATS        '.             
021600     03  FILLER      PIC X(66)  VALUE '300-/400-SERIEN     '.             
021700     03  FILLER      PIC X(66)  VALUE 'TILLBEHÖR           '.             
021800     03  FILLER      PIC X(66)  VALUE 'BESTÄLLNINGSREST    '.             
021900     03  FILLER      PIC X(66)  VALUE 'KAMPANJ             '.             
022000     03  FILLER      PIC X(66)  VALUE 'STANDARD            '.             
022100     03  FILLER      PIC X(66)  VALUE 'SÄKERHETSPRODUKT    '.             
022200     03  FILLER      PIC X(66)  VALUE 'BYTES               '.             
022300     03  FILLER      PIC X(66)  VALUE 'DEKAL               '.             
022400     03  FILLER      PIC X(66)  VALUE '01-MÄRKT            '.             
022500 01  FILLER   REDEFINES  WS-TEMEMO-TAB.                                   
022600     03  WS-TEMEMO-ORS  OCCURS 11 PIC X(66).                              
022700                                                                          
022800 01  WS-TEMEMO-KORT-TAB.                                                  
022900     03  FILLER      PIC X(09)  VALUE 'SPÄRR.KV '.                        
023000     03  FILLER      PIC X(09)  VALUE 'ING.SATS '.                        
023100     03  FILLER      PIC X(09)  VALUE '300-/400 '.                        
023200     03  FILLER      PIC X(09)  VALUE 'TILLBEH  '.                        
023300     03  FILLER      PIC X(09)  VALUE 'BESTREST '.                        
023400     03  FILLER      PIC X(09)  VALUE 'KAMPANJ  '.                        
023500     03  FILLER      PIC X(09)  VALUE 'STANDARD '.                        
023600     03  FILLER      PIC X(09)  VALUE 'SÄKERHET '.                        
023700     03  FILLER      PIC X(09)  VALUE 'BYTES    '.                        
023800     03  FILLER      PIC X(09)  VALUE 'DEKAL    '.                        
023900     03  FILLER      PIC X(09)  VALUE '01-MÄRKT '.                        
024000 01  FILLER   REDEFINES  WS-TEMEMO-KORT-TAB.                              
024100     03  WS-TEMEMO-KORT OCCURS 11 PIC X(09).                              
024200     EJECT                                                                
024300 01  WS-TEMEMO-ESC-TAB.                                                   
024400     03  FILLER      PIC X(66)                                            
024500                     VALUE 'PASSED BLOCKDATE FOR AUTO SCRAP'.             
024600     03  FILLER      PIC X(66)  VALUE 'EMBALLAGE           '.             
024700     03  FILLER      PIC X(66)  VALUE 'ORDER BLOCKED       '.             
024800     03  FILLER      PIC X(66)  VALUE 'QUALITY BLOCKED     '.             
024900     03  FILLER      PIC X(66)  VALUE 'EXCHANGE            '.             
025000     03  FILLER      PIC X(66)  VALUE 'LOCAL               '.             
025100     03  FILLER      PIC X(66)  VALUE 'REPLACED            '.             
025200     03  FILLER      PIC X(66)  VALUE '98 SCRAP            '.             
025300 01  FILLER   REDEFINES  WS-TEMEMO-ESC-TAB.                               
025400     03  WS-TEMEMO-ESC-ORS  OCCURS 11 PIC X(66).                          
025500                                                                          
025600 01  WS-TEMEMO-ESC-KORT-TAB.                                              
025700     03  FILLER      PIC X(09)  VALUE 'BLOCKDATE'.                        
025800     03  FILLER      PIC X(09)  VALUE ' EMB     '.                        
025900     03  FILLER      PIC X(09)  VALUE ' ORDBLOCK'.                        
026000     03  FILLER      PIC X(09)  VALUE ' QBLOCK  '.                        
026100     03  FILLER      PIC X(09)  VALUE ' EXCHANGE'.                        
026200     03  FILLER      PIC X(09)  VALUE ' LOCAL   '.                        
026300     03  FILLER      PIC X(09)  VALUE ' REPL    '.                        
026400     03  FILLER      PIC X(09)  VALUE ' 98 SCRAP'.                        
026500 01  FILLER   REDEFINES  WS-TEMEMO-ESC-KORT-TAB.                          
026600     03  WS-TEMEMO-ESC-KORT OCCURS 11 PIC X(09).                          
026700     EJECT                                                                
026800*    --- CLASSIC TRANS                                                    
026900*01 -COPY W21632            -PRE FILC-                                    
027000     EJECT                                                                
027100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
027200 01  GENERELLA-SUBPROGRAM.                                                
027300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
027400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
027500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
027600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
027700     03  W411ORDN                PIC X(8)    VALUE 'W411ORDN'.            
027800     EJECT                                                                
027900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
028000*01 -COPY WMEDAREA                                                        
028100     SKIP3                                                                
028200 01  ERROR-TEXT.                                                          
028300     03  FILLER                  PIC X(8)    VALUE 'ERR-TXT'.             
028400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
028500                                                                          
028600 01  MESSAGE-CODES.                                                       
028700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
028800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
028900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
029000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
029100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
029200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
029300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
029400     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
029500     03  NO-AUTHOR-TO-SCRAP      PIC X(3)    VALUE '601'.                 
029600     03  NO-AUTHOR-TO-ATT        PIC X(3)    VALUE '602'.                 
029700     03  INF-HIGHER-LEV          PIC X(3)    VALUE '603'.                 
029800     EJECT                                                                
029900 01  PROG-TO-PROG-SW.                                                     
030000*    03  -COPY WMSGSOP                                                    
030100     EJECT                                                                
030200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
030300*                                                                         
030400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
030500     SKIP3                                                                
030600*01 -COPY WMSGINIT                                                        
030700     EJECT                                                                
030800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
030900*                                                                         
031000 01  SPAR-AREA.                                                           
031100     03  SPAR-IDTRANS                PIC X(4)  VALUE '6322'.              
031200     03  SPAR-IDDC-ENTER             PIC X(2)  VALUE SPACE.               
031300     03  SPAR-IDANSK-ENTER           PIC S9(3) VALUE ZERO COMP-3.         
031400     03  SPAR-IDARTNR-ENTER          PIC S9(9) VALUE ZERO COMP-3.         
031500     03  SPAR-DASKROT9-BEORD-ENTER   PIC 9(8)  VALUE ZERO.                
031600     03  SPAR-IDDC-NEXT              PIC X(2)  VALUE SPACE.               
031700     03  SPAR-IDANSK-NEXT-MIN        PIC S9(3) VALUE ZERO COMP-3.         
031800     03  SPAR-IDANSK-NEXT-MAX        PIC S9(3) VALUE ZERO COMP-3.         
031900     03  SPAR-IDARTNR-NEXT           PIC S9(9) VALUE ZERO COMP-3.         
032000     03  SPAR-DASKROT9-BEORD-NEXT    PIC 9(8)  VALUE ZERO.                
032100     03  SPAR-KDARBTYP-ENTER         PIC X(8)  VALUE SPACE.               
032200     03  SPAR-KDARBTYP-NEXT          PIC X(8)  VALUE SPACE.               
032300     03  FILLER                      PIC X(1000) VALUE SPACE.             
032400     EJECT                                                                
032500 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
032600 01  P-TO-P-SW.                                                           
032700     03  P-TO-P-KVLL                PIC S9(4)           COMP SYNC.        
032800     03  P-TO-P-KDZ1                PIC X(1)  VALUE LOW-VALUE.            
032900     03  P-TO-P-KDZ2                PIC X(1)  VALUE LOW-VALUE.            
033000     03  P-TO-P-KDTRANS             PIC X(8).                             
033100     03  P-TO-P-IDTRANS             PIC X(4).                             
033200     03  P-TO-P-KDMFSFOR            PIC X(1).                             
033300     03  P-TO-P-DATA.                                                     
033400        05 FILLER                  PIC X(1000).                           
033500                                                                          
033600**********************************************************                
033700***   I N K Ö P S - P O S T   P V                                         
033800**********************************************************                
033900*                                                                         
034000*01  -COPY A310TB65                -PRE A310-                             
034100     EJECT                                                                
034200*01  AREA   -COPY W092W001     -PRE W092-.                                
034300     EJECT                                                                
034400******************************************************************        
034500*01  -COPY W6I32501   -PRE MOD6325-                                       
034600     EJECT                                                                
034700                                                                          
034800*01  -COPY W2I35201   -PRE 2352-                                          
034900     EJECT                                                                
035000                                                                          
035100*01  -COPY W2I37201   -PRE 2372-                                          
035200     EJECT                                                                
035300                                                                          
035400     EJECT                                                                
035500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
035600*                                                                         
035700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
035800     SKIP3                                                                
035900*01  MID -COPY W6I32201                                                   
036000     EJECT                                                                
036100*   -COPY WMSGMAIL                                                        
036200     EJECT                                                                
036300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
036400     SKIP3                                                                
036500*01  -COPY WMSGAREA                                                       
036600     EJECT                                                                
036700     03  MOD REDEFINES MSG-AREA.                                          
036800*      05  -COPY W6O32201                                                 
036900     EJECT                                                                
037000 01  FILLER              PIC X(16)  VALUE 'PROG-TO-PROG-SW'.              
037100                                                                          
037200 01  W-PROG-TO-PROG-SW.                                                   
037300     05  P-WS-LL         PIC S9(4)  VALUE +469 COMP SYNC.                 
037400     05  P-WS-Z1-Z2      PIC  X(2)  VALUE LOW-VALUE.                      
037500     05  KDTRANS-WS      PIC  X(8)  VALUE 'W1T113X '.                     
037600     05  FILLER          PIC  X(5)  VALUE '21293'.                        
037700     05  MID    -COPY W1I11301     -PRE PROGSW-                           
037800     EJECT                                                                
037900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
038000     SKIP3                                                                
038100*01  -COPY WMFSAREA                                                       
038200     EJECT                                                                
038300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
038400*                                                                         
038500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
038600     SKIP3                                                                
038700 01  NYCKLAR-TILL-DLI.                                                    
038800*                                                                         
038900     03  W-IDARTNR-MIN-X.                                                 
039000         05  W-IDARTNR-MIN   PIC S9(9)  VALUE ZERO COMP-3.                
039100     03  W-IDARTNR-MAX-X.                                                 
039200         05  W-IDARTNR-MAX   PIC S9(9)  VALUE +999999999 COMP-3.          
039300     03  W-IDPERSON-MIN-X.                                                
039400         05  W-IDPERSON-MIN    PIC S9(3)  VALUE ZERO COMP-3.              
039500     03  W-IDPERSON-MAX-X.                                                
039600         05  W-IDPERSON-MAX    PIC S9(3)  VALUE +999 COMP-3.              
039700     03  W-IDDC-MIN-X.                                                    
039800         05  W-IDDC-MIN      PIC X(2)   VALUE LOW-VALUE.                  
039900     03  W-IDDC-MAX-X.                                                    
040000         05  W-IDDC-MAX      PIC X(2)   VALUE HIGH-VALUE.                 
040100     03  W-DASKROT9-MIN-X.                                                
040200         05  W-DASKROT9-MIN  PIC 9(8)   VALUE ZERO.                       
040300     03  W-DASKROT9-MAX-X.                                                
040400         05  W-DASKROT9-MAX  PIC 9(8)   VALUE 99999999.                   
040500     03  W-IDARTNR-X.                                                     
040600         05  W-IDARTNR       PIC S9(9)  VALUE ZERO COMP-3.                
040700     03  W-IDDC-X.                                                        
040800         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
040900     03  W-KDARBTYP-X.                                                    
041000         05  W-KDARBTYP      PIC X(8)   VALUE SPACE.                      
041100     03  W-DASKROT9-X.                                                    
041200         05  W-DASKROT9      PIC 9(8)   VALUE ZERO.                       
041300     03  W-IDDC-6324-X.                                                   
041400         05  W-IDDC-6324      PIC X(2)   VALUE SPACE.                     
041500     03  W-KDSTASKR-X.                                                    
041600         05  W-KDSTASKR      PIC S9     VALUE 1 COMP-3.                   
041700     03  W-WDGXKEY-6321.                                                  
041800         05  W-6321-IDHTYP    PIC X(4)   VALUE '6321'.                    
041900         05  W-6321-KDARBTYP  PIC X(8)   VALUE SPACE.                     
042000         05  W-6321-LOWVALUE  PIC X(18)  VALUE LOW-VALUE.                 
042100     03  W-WDGXKEY-6327.                                                  
042200         05  W-6327-IDHTYP    PIC X(4)   VALUE '6327'.                    
042300         05  W-6327-KDARBTYP  PIC X(8)   VALUE SPACE.                     
042400         05  W-6327-IDDC      PIC X(2)   VALUE SPACE.                     
042500         05  W-6327-LOWVALUE  PIC X(16)  VALUE LOW-VALUE.                 
042600     03  W-IDUSER-GODK-X.                                                 
042700         05  W-IDUSER-GODK    PIC X(8)   VALUE SPACE.                     
042800     03  W-SUBEL-MIN-X.                                                   
042900         05  W-SUBEL-MIN      PIC 9(7)  VALUE ZERO.                       
043000     03  W-SUBEL-MAX-X.                                                   
043100         05  W-SUBEL-MAX      PIC 9(7)  VALUE 9999999.                    
043200     03 W-2401-KEY-X.                                                     
043300         05 FILLER               PIC X(4)    VALUE '2401'.                
043400         05 FILLER               PIC X(26)   VALUE LOW-VALUE.             
043500     03  W-KY6324-KVAL-X.                                                 
043600         05  W-IDARTNR-KVAL  PIC S9(9)  VALUE ZERO COMP-3.                
043700         05  W-IDDC-KVAL     PIC X(2)   VALUE SPACE.                      
043800         05  W-KDSTASKR-KVAL PIC S9     VALUE ZERO COMP-3.                
043900     03  W-IDRADNR-X.                                                     
044000         05  W-IDRADNR       PIC S9(5)  VALUE ZERO COMP-3.                
044100     03  W-1141-KEY-X.                                                    
044200         05 FILLER         PIC X(04)  VALUE '1141'.                       
044300         05 FILLER         PIC X(26)  VALUE LOW-VALUE.                    
044400     03  W-IDLEVNR-X.                                                     
044500         05 W-IDLEVNR      PIC X(5)   VALUE SPACE.                        
044600     03  W-WDG901KY-X.                                                    
044700         05  W-TIREGDAT          PIC S9(07)   VALUE ZERO COMP-3.          
044800         05  W-TIKLOCK           PIC S9(09)   VALUE ZERO COMP-3.          
044900                                                                          
045000     03  W-WDF201KY-X.                                                    
045100         05  W-IDLEVNR-WDF2      PIC  X(5)   VALUE SPACE.                 
045200         05  W-IDDIRGRP          PIC X(10)   VALUE SPACE.                 
045300                                                                          
045400     EJECT                                                                
045500*    --- STATUS-KOD FRÅN IMS                                              
045600 01  STATUS-WS                   PIC XX.                                  
045700     88  SEGMENT-FINNS                       VALUE '  '.                  
045800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
045900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
046000     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
046100     SKIP2                                                                
046200 01  GODK-STATUSKODER.                                                    
046300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
046400     SKIP3                                                                
046500 01  SSA1                        PIC X(128).                              
046600 01  SSA2                        PIC X(256).                              
046700 01  SSA3                        PIC X(256).                              
046800 01  SSA4                        PIC X(256).                              
046900     EJECT                                                                
047000*    --- IMS FUNKTIONSKODER                                               
047100*01  -COPY W0003                                                          
047200     EJECT                                                                
047300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
047400 01  DLI-IO-WLARTC01.                                                     
047500*    03  -COPY WDK601                                                     
047600     EJECT                                                                
047700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
047800 01  DLI-IO-WLARTC11.                                                     
047900*    03  -COPY WDK611                                                     
048000     EJECT                                                                
048100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC23'.                    
048200 01  DLI-IO-WLARTC23.                                                     
048300*    03  -COPY WDK623                                                     
048400     EJECT                                                                
048500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301  '.                    
048600 01  DLI-IO-WDT301.                                                       
048700*    03  -COPY WDT301                                                     
048800     EJECT                                                                
048900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311  '.                    
049000 01  DLI-IO-WDT311.                                                       
049100*    03  -COPY WDT311                                                     
049200     EJECT                                                                
049300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS11'.                    
049400 01  DLI-IO-WLARTS11.                                                     
049500*    03  -COPY WDK711                                                     
049600     EJECT                                                                
049700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-6321'.                 
049800 01  DLI-IO-WDR501-6321.                                                  
049900*    03  -COPY WDGX6321                                                   
050000     EJECT                                                                
050100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6322'.                    
050200 01  DLI-IO-WDGX6322.                                                     
050300*    03  -COPY WDGX6322                                                   
050400     EJECT                                                                
050500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6324'.                    
050600 01  DLI-IO-WDGX6324.                                                     
050700*    03  -COPY WDGX6324                                                   
050800     EJECT                                                                
050900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6326'.                    
051000 01  DLI-IO-WDGX6326.                                                     
051100*    03  -COPY WDGX6326                                                   
051200     EJECT                                                                
051300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6325'.                    
051400 01  DLI-IO-WDGX6325.                                                     
051500*    03  -COPY WDGX6325                                                   
051600     EJECT                                                                
051700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6327'.                    
051800 01  DLI-IO-WDGX6327.                                                     
051900*    03  -COPY WDGX6327                                                   
052000     EJECT                                                                
052100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX6328'.                    
052200 01  DLI-IO-WDGX6328.                                                     
052300*    03  -COPY WDGX6328                                                   
052400     EJECT                                                                
052500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR501-2401'.                 
052600 01  DLI-IO-WDR501-2401.                                                  
052700*    03  -COPY WDGX2402                                                   
052800     EJECT                                                                
052900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLFILC'.                      
053000 01  DLI-IO-AREA-FILC.                                                    
053100*    03  -COPY WDR301       -PRE FILC-                                    
053200     EJECT                                                                
053300                                                                          
053400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD801'.                      
053500 01  DLI-IO-WDD801.                                                       
053600*    03  -COPY WDD801                                                     
053700     EJECT                                                                
053800                                                                          
053900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD811'.                      
054000 01  DLI-IO-WDD811.                                                       
054100*    03  -COPY WDD811                                                     
054200     EJECT                                                                
054300                                                                          
054400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDJ901'.                      
054500 01  DLI-IO-WDJ901.                                                       
054600*    03  -COPY WDJ901                                                     
054700     EJECT                                                                
054800                                                                          
054900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDJ911'.                      
055000 01  DLI-IO-WDJ911.                                                       
055100*    03  -COPY WDJ911                                                     
055200     EJECT                                                                
055300 01  DLI-IO-AREA5.                                                        
055400     03  IO-AREA5              PIC X(150) VALUE SPACE.                    
055500                                                                          
055600*    03  ZZAC -COPY WDGZ01     -PRE ZZAC-  -RED IO-AREA5.                 
055700     EJECT                                                                
055800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDG901'.                      
055900 01  DLI-IO-WDG901.                                                       
056000*    03  -COPY WDG901                                                     
056100     EJECT                                                                
056200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF201'.                      
056300 01  DLI-IO-WDF201.                                                       
056400*    03  -COPY WDF201 -PRE WDF2-                                          
056500     EJECT                                                                
056600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF212'.                      
056700 01  DLI-IO-WDF212.                                                       
056800*    03  -COPY WDF212  -PRE  WDF2-                                        
056900     EJECT                                                                
057000                                                                          
057100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
057200 01  DLI-IO-WDB601.                                                       
057300*    03  -COPY WDB601                                                     
057400     EJECT                                                                
057500                                                                          
057600 LINKAGE SECTION.                                                         
057700*01  -COPY W0009   -PRE MSG-                                              
057800     EJECT                                                                
057900*01  -COPY W0009   -PRE ALT-                                              
058000     EJECT                                                                
058100*01  -COPY W0009   -PRE ALTMAIL-                                          
058200     EJECT                                                                
058300*01  -COPY W0009   -PRE ALT1-                                             
058400     EJECT                                                                
058500*01  -COPY W0009   -PRE ALT2-                                             
058600     EJECT                                                                
058700*01  -COPY W0009   -PRE ALT3-                                             
058800     EJECT                                                                
058900*01  -COPY W0009   -PRE ALT4-                                             
059000     EJECT                                                                
059100*01  -COPY W0008   -PRE USEA-                                             
059200     05  FILLER                  PIC X.                                   
059300     EJECT                                                                
059400*01  -COPY W0008   -PRE ARTC-                                             
059500     05  FILLER                  PIC X.                                   
059600     EJECT                                                                
059700*01  -COPY W0008   -PRE ARTS-                                             
059800     05  FILLER                  PIC X.                                   
059900     EJECT                                                                
060000*01  -COPY W0008   -PRE 6321-                                             
060100     05  FILLER                  PIC X.                                   
060200     EJECT                                                                
060300*01  -COPY W0008   -PRE 6327-                                             
060400     05  FILLER                  PIC X.                                   
060500     EJECT                                                                
060600*01  -COPY W0008   -PRE 2401-                                             
060700     05  FILLER                  PIC X.                                   
060800     EJECT                                                                
060900*01  -COPY W0008   -PRE FILC-                                             
061000     05  FILLER                  PIC X.                                   
061100     EJECT                                                                
061200*01  -COPY W0008  -PRE WDD8-                                              
061300     05  FILLER                  PIC X.                                   
061400     EJECT                                                                
061500*01  -COPY W0008  -PRE WDJ9-                                              
061600     05  FILLER                  PIC X.                                   
061700     EJECT                                                                
061800*01  -COPY W0008  -PRE 6325-                                              
061900     05  FILLER                  PIC X.                                   
062000     EJECT                                                                
062100*01  -COPY W0008     -PRE ZZAC-                                           
062200         05  FILLER           PIC X.                                      
062300     EJECT                                                                
062400*01  -COPY W0008  -PRE ZZAD-                                              
062500     05  FILLER                  PIC X.                                   
062600     EJECT                                                                
062700*01  -COPY W0008  -PRE WDF2-                                              
062800     05  FILLER                  PIC X.                                   
062900     EJECT                                                                
063000*01  -COPY W0008  -PRE WDB6-                                              
063100     05  FILLER                  PIC X.                                   
063200     EJECT                                                                
063300*01  -COPY W0008  -PRE WDT3-                                              
063400     05  FILLER                  PIC X.                                   
063500     EJECT                                                                
063600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB ALTMAIL-PCB                    
063700                           ALT1-PCB ALT2-PCB ALT3-PCB                     
063800                           ALT4-PCB USEA-PCB                              
063900                           ARTC-PCB ARTS-PCB 6321-PCB 6327-PCB            
064000                           2401-PCB FILC-PCB WDD8-PCB WDJ9-PCB            
064100                           6325-PCB ZZAC-PCB ZZAD-PCB                     
064200                           WDF2-PCB WDB6-PCB WDT3-PCB.                    
064300 MAIN SECTION.                                                            
064400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALTMAIL-PCB                    
064500                           ALT1-PCB ALT2-PCB ALT3-PCB                     
064600                           ALT4-PCB USEA-PCB                              
064700                           ARTC-PCB ARTS-PCB 6321-PCB 6327-PCB            
064800                           2401-PCB FILC-PCB WDD8-PCB WDJ9-PCB            
064900                           6325-PCB ZZAC-PCB ZZAD-PCB                     
065000                           WDF2-PCB WDB6-PCB WDT3-PCB.                    
065100                                                                          
065200     PERFORM IMS-GET-MSG                                                  
065300     IF SEGMENT-FINNS                                                     
065400                                                                          
065500        PERFORM A-INIT                                                    
065600        PERFORM B-KOLLA-NYCKLAR                                           
065700                                                                          
065800        IF NYCKLAR-OK                                                     
065900           IF MFS-UPDATE                                                  
066000              PERFORM G-KOLLA-INPUT                                       
066100              IF INDATA-OK                                                
066200                 PERFORM H-UPPDATERA                                      
066300              END-IF                                                      
066400           ELSE                                                           
066500              IF MFS-FIRST                                                
066600                 PERFORM C-FOERSTA-SIDA                                   
066700              ELSE                                                        
066800                 IF MFS-NEXT                                              
066900                    PERFORM D-NAESTA-SIDA                                 
067000                 ELSE                                                     
067100                    PERFORM E-SAMMA-SIDA                                  
067200                 END-IF                                                   
067300              END-IF                                                      
067400           END-IF                                                         
067500           PERFORM F-LAES-VISA-INFO                                       
067600        END-IF                                                            
067700*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
067800*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
067900       IF MFS-ENTER AND JUMP-6325                                         
068000         PERFORM S95-JUMP-6325                                            
068100       ELSE                                                               
068200         IF MFS-ENTER AND JUMP-2352                                       
068300           PERFORM S96-JUMP-2352                                          
068400         ELSE                                                             
068500           IF MFS-ENTER AND JUMP-2372                                     
068600             PERFORM S97-JUMP-2372                                        
068700           ELSE                                                           
068800             COMPUTE MSG-KVLL = LENGTH OF MOD-W6O32201 + 4                
068900             PERFORM IMS-INSERT-MSG                                       
069000           END-IF                                                         
069100         END-IF                                                           
069200       END-IF                                                             
069300     END-IF                                                               
069400                                                                          
069500     MOVE ZERO TO RETURN-CODE                                             
069600     GOBACK                                                               
069700     .                                                                    
069800     EJECT                                                                
069900 A-INIT SECTION.                                                          
070000                                                                          
070100     IF MSG-DUBBLA-TRANSKODER                                             
070200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I32201                 
070300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
070400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
070500     ELSE                                                                 
070600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I32201                  
070700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
070800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
070900     END-IF                                                               
071000                                                                          
071100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
071200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
071300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
071400                                                                          
071500     MOVE LOW-VALUE TO MSG-AREA                                           
071600     MOVE 'W6O322N1' TO MFS-IDMOD                                         
071700     MOVE '6322' TO MOD-IDTRANS                                           
071800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
071900                                                                          
072000     IF EGEN-MID                                                          
072100       CONTINUE                                                           
072200     ELSE                                                                 
072300       MOVE SPACE TO MFS-KDTRTYP                                          
072400       MOVE '7'   TO MFS-IDPFK                                            
072500     END-IF                                                               
072600                                                                          
072700     ACCEPT DAGENS-DATUM FROM DATE                                        
072800     MOVE 'W6032200'      TO FILC-FIL-IDPGM                               
072900     MOVE DAGENS-DATUM    TO FILC-FIL-TIREGDAT                            
073000     MOVE 'W21632  '      TO FILC-FIL-IDCPYTXT                            
073100     MOVE ZERO            TO FILC-FIL-TIKLOCK                             
073200     MOVE NEJ             TO JUMP-6325-SW                                 
073300     MOVE 1               TO W-KDSTASKR-KVAL                              
073400     ACCEPT POST-TIKLOCK FROM TIME                                        
073500     MOVE POST-TIKLOCK TO W-TIKLOCK                                       
073600                                                                          
073700*** READ WHOLE OF WDB6 AND FILL IN DC TABLE FOR KDDC = 'S' OR 'C'         
073800     INITIALIZE IDDC-TABELL                                               
073900     SET TAB-IX TO +1                                                     
074000     PERFORM IMS-GN-WDB601                                                
074100     PERFORM UNTIL SEGMENT-NOMORE                                         
074200        IF DCS-KDDC = 'S' OR 'C' OR 'NC' OR 'NA' OR 'NX' OR 'NS'          
074300          MOVE DCS-IDDC     TO TAB-IDDC(TAB-IX)                           
074400          SET TAB-IX UP BY +1                                             
074500          IF TAB-IX > 100                                                 
074600            MOVE 'DC-TABELLEN FULL' TO ERROR-TEXT-STR                     
074700            DISPLAY ERROR-TEXT                                            
074800            CALL FELLOG                                                   
074900          END-IF                                                          
075000        END-IF                                                            
075100        PERFORM IMS-GN-WDB601                                             
075200     END-PERFORM                                                          
075300                                                                          
075400     SET TAB-IX TO +1                                                     
075500     .                                                                    
075600     EJECT                                                                
075700 B-KOLLA-NYCKLAR SECTION.                                                 
075800                                                                          
075900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
076000     MOVE '001'             TO MSGI-KDCALL                                
076100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
076200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
076300     MOVE '6322'            TO MSGI-IDTRANS                               
076400                                                                          
076500     IF GODK-MID                                                          
076600        MOVE NEJ TO IDARTNR-SOEKNING                                      
076700                    DATUM-SOEKNING                                        
076800                    DATUM-IDARTNR-SOEKNING                                
076900                    KDARBTYP-SOEKNING                                     
077000                    KDARBTYP-PERSON-SOEKNING                              
077100                    IDDC-SOEKNING                                         
077200                    IDPERSON-SOEKNING                                     
077300                                                                          
077400        IF MID-KDARBTYP-IN  NOT = ALL '+'                                 
077500           MOVE JA TO KDARBTYP-SOEKNING                                   
077600           MOVE MID-KDARBTYP-IN TO MSGI-KDARBTYP                          
077700        ELSE                                                              
077800           IF MID-KDARBTYP-UT = SPACE OR LOW-VALUE                        
077900              MOVE SPACE TO MSGI-KDARBTYP                                 
078000           ELSE                                                           
078100              MOVE JA TO KDARBTYP-SOEKNING                                
078200              MOVE MID-KDARBTYP-UT TO MSGI-KDARBTYP                       
078300           END-IF                                                         
078400        END-IF                                                            
078500                                                                          
078600        IF MID-IDDC-IN NOT = ALL '+'                                      
078700           MOVE JA TO IDDC-SOEKNING                                       
078800           MOVE MID-IDDC-IN TO MSGI-IDDC-KEY                              
078900        ELSE                                                              
079000           MOVE MID-IDDC-UT    TO WS-IDDC                                 
079100           IF GOOD-DC OR MID-IDDC-UT = SPACE                              
079200           OR MID-IDDC-UT = ZERO                                          
079300             MOVE JA TO IDDC-SOEKNING                                     
079400             MOVE MID-IDDC-UT TO MSGI-IDDC-KEY                            
079500           ELSE                                                           
079600             MOVE SPACE TO MSGI-IDDC-KEY                                  
079700           END-IF                                                         
079800        END-IF                                                            
079900                                                                          
080000        IF EGEN-MID                                                       
080100          IF MID-IDARTNR-IN NOT = ALL '+'                                 
080200             MOVE MID-IDARTNR-IN TO TEST-NYCKEL-IDARTNR                   
080300             INSPECT TEST-NYCKEL-IDARTNR                                  
080400                    REPLACING LEADING SPACE BY ZERO                       
080500             IF TEST-NYCKEL-IDARTNR NUMERIC                               
080600                IF TEST-NYCKEL-IDARTNR > ZERO                             
080700                   MOVE JA TO IDARTNR-SOEKNING                            
080800                   MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                    
080900                ELSE                                                      
081000                   MOVE ZERO TO MSGI-IDARTNR                              
081100                END-IF                                                    
081200             ELSE                                                         
081300                MOVE ZERO TO MSGI-IDARTNR                                 
081400             END-IF                                                       
081500          ELSE                                                            
081600            INSPECT MID-IDARTNR-UT REPLACING LEADING SPACE BY ZERO        
081700             IF MID-IDARTNR-UT NUMERIC                                    
081800                IF MID-IDARTNR-UT > ZERO                                  
081900                   MOVE MID-IDARTNR-UT TO MSGI-IDARTNR                    
082000                   MOVE JA TO IDARTNR-SOEKNING                            
082100                ELSE                                                      
082200                   MOVE ZERO TO MSGI-IDARTNR                              
082300                END-IF                                                    
082400             ELSE                                                         
082500                MOVE ZERO TO MSGI-IDARTNR                                 
082600             END-IF                                                       
082700          END-IF                                                          
082800                                                                          
082900          IF MID-TIDATUM-IN NOT = ALL '+'                                 
083000             MOVE MID-TIDATUM-IN TO TEST-NYCKEL-TIDATUM                   
083100             INSPECT TEST-NYCKEL-TIDATUM                                  
083200                    REPLACING LEADING SPACE BY ZERO                       
083300             IF TEST-NYCKEL-TIDATUM NUMERIC                               
083400                IF TEST-NYCKEL-TIDATUM > ZERO                             
083500                   MOVE JA TO DATUM-SOEKNING                              
083600                   MOVE MID-TIDATUM-IN TO MSGI-TISKROT-BEORD              
083700                ELSE                                                      
083800                   MOVE ZERO TO MSGI-TISKROT-BEORD                        
083900                END-IF                                                    
084000             ELSE                                                         
084100                MOVE ZERO TO MSGI-TISKROT-BEORD                           
084200             END-IF                                                       
084300          ELSE                                                            
084400            INSPECT MID-TIDATUM-UT REPLACING LEADING SPACE BY ZERO        
084500             IF MID-TIDATUM-UT NUMERIC                                    
084600                IF MID-TIDATUM-UT > ZERO                                  
084700                   MOVE JA TO DATUM-SOEKNING                              
084800                   MOVE MID-TIDATUM-UT TO MSGI-TISKROT-BEORD              
084900                ELSE                                                      
085000                   MOVE ZERO TO MSGI-TISKROT-BEORD                        
085100                END-IF                                                    
085200             ELSE                                                         
085300                MOVE ZERO TO MSGI-TISKROT-BEORD                           
085400             END-IF                                                       
085500          END-IF                                                          
085600                                                                          
085700          MOVE ZERO TO WS-IDPERSON-FOM                                    
085800                       WS-IDPERSON-TOM                                    
085900          IF MID-IDPERSON-FOM-IN NOT = ALL '+'                            
086000             INSPECT MID-IDPERSON-FOM-IN                                  
086100                REPLACING LEADING SPACE BY ZERO                           
086200             MOVE MID-IDPERSON-FOM-IN TO WS-IDPERSON-FOM                  
086300             MOVE JA TO IDPERSON-SOEKNING                                 
086400             MOVE ZERO TO MSGI-IDANSK                                     
086500          ELSE                                                            
086600             INSPECT MID-IDPERSON-FOM-UT                                  
086700                REPLACING LEADING SPACE BY ZERO                           
086800             IF MID-IDPERSON-FOM-UT NUMERIC                               
086900                IF MID-IDPERSON-FOM-UT > ZERO                             
087000                   MOVE MID-IDPERSON-FOM-UT TO MSGI-IDANSK                
087100                                               WS-IDPERSON-FOM            
087200                   MOVE JA TO IDPERSON-SOEKNING                           
087300                ELSE                                                      
087400                   MOVE ZERO TO MSGI-IDANSK                               
087500                END-IF                                                    
087600             ELSE                                                         
087700                MOVE ZERO TO MSGI-IDANSK                                  
087800             END-IF                                                       
087900          END-IF                                                          
088000                                                                          
088100          IF MID-IDPERSON-TOM-IN NOT = ALL '+'                            
088200             INSPECT MID-IDPERSON-TOM-IN                                  
088300                REPLACING LEADING SPACE BY ZERO                           
088400             MOVE MID-IDPERSON-TOM-IN TO WS-IDPERSON-TOM                  
088500             MOVE JA TO IDPERSON-SOEKNING                                 
088600             MOVE ZERO TO MSGI-IDANSK                                     
088700          ELSE                                                            
088800             INSPECT MID-IDPERSON-TOM-UT                                  
088900                REPLACING LEADING SPACE BY ZERO                           
089000             IF MID-IDPERSON-TOM-UT NUMERIC                               
089100                IF MID-IDPERSON-TOM-UT > ZERO                             
089200                   MOVE MID-IDPERSON-TOM-UT TO MSGI-IDANSK                
089300                                               WS-IDPERSON-TOM            
089400                   MOVE JA TO IDPERSON-SOEKNING                           
089500                ELSE                                                      
089600                   MOVE ZERO TO MSGI-IDANSK                               
089700                END-IF                                                    
089800             ELSE                                                         
089900                MOVE ZERO TO MSGI-IDANSK                                  
090000             END-IF                                                       
090100          END-IF                                                          
090200        END-IF                                                            
090300     END-IF                                                               
090400                                                                          
090500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
090600     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
090700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
090800                                                                          
090900     MOVE JA TO NYCKLAR-SW                                                
091000                                                                          
091100     MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-IN                              
091200     IF MID-KDARBTYP-IN NOT = ALL '+'                                     
091300       MOVE '7'         TO MFS-IDPFK                                      
091400       MOVE SPACE       TO MFS-KDTRTYP                                    
091500       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
091600     END-IF                                                               
091700                                                                          
091800     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
091900     IF MID-IDDC-IN NOT = ALL '+'                                         
092000       MOVE '7'           TO MFS-IDPFK                                    
092100       MOVE SPACE         TO MFS-KDTRTYP                                  
092200       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
092300     END-IF                                                               
092400                                                                          
092500     MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-FOM-IN                          
092600     IF MID-IDPERSON-FOM-IN NOT = ALL '+'                                 
092700       MOVE '7'         TO MFS-IDPFK                                      
092800       MOVE SPACE       TO MFS-KDTRTYP                                    
092900       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
093000     END-IF                                                               
093100                                                                          
093200     MOVE MFS-RENSA-FAELT TO MOD-IDPERSON-TOM-IN                          
093300     IF MID-IDPERSON-TOM-IN NOT = ALL '+'                                 
093400       MOVE '7'         TO MFS-IDPFK                                      
093500       MOVE SPACE       TO MFS-KDTRTYP                                    
093600       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
093700     END-IF                                                               
093800                                                                          
093900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
094000     IF MID-IDARTNR-IN NOT = ALL '+'                                      
094100       MOVE '7'         TO MFS-IDPFK                                      
094200       MOVE SPACE       TO MFS-KDTRTYP                                    
094300       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
094400     END-IF                                                               
094500                                                                          
094600     MOVE MFS-RENSA-FAELT TO MOD-TIDATUM-IN                               
094700     IF MID-TIDATUM-IN NOT = ALL '+'                                      
094800       MOVE '7'         TO MFS-IDPFK                                      
094900       MOVE SPACE       TO MFS-KDTRTYP                                    
095000       PERFORM MFS-RENSA-SPAR-NYCKLAR                                     
095100     END-IF                                                               
095200                                                                          
095300     IF IDARTNR-SOEKNING = JA                                             
095400        INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO              
095500        IF MSGI-IDARTNR NUMERIC                                           
095600          MOVE MSGI-IDARTNR TO WS-IDARTNR                                 
095700          MOVE WS-IDARTNR TO W-IDARTNR                                    
095800        ELSE                                                              
095900          MOVE NEJ TO NYCKLAR-SW                                          
096000        END-IF                                                            
096100     END-IF                                                               
096200                                                                          
096300     IF DATUM-SOEKNING = JA                                               
096400        INSPECT MSGI-TISKROT-BEORD REPLACING LEADING SPACE BY ZERO        
096500        IF MSGI-TISKROT-BEORD NUMERIC                                     
096600           MOVE MSGI-TISKROT-BEORD TO WS-SEKEL-KOLL                       
096700                                      WS-DASKROT-AAMMDD                   
096800           IF WS-SEKEL = 9                                                
096900              MOVE 19        TO WS-DASKROT-SS                             
097000           ELSE                                                           
097100              MOVE 20        TO WS-DASKROT-SS                             
097200           END-IF                                                         
097300           COMPUTE WS-DASKROT9-BEORD = 99999999 - WS-AAAAMMDD             
097400           MOVE WS-DASKROT9-BEORD TO W-DASKROT9                           
097500        ELSE                                                              
097600           MOVE NEJ TO NYCKLAR-SW                                         
097700        END-IF                                                            
097800     END-IF                                                               
097900                                                                          
098000     IF DATUM-SOEKNING    = JA                                            
098100     AND IDARTNR-SOEKNING = JA                                            
098200        MOVE NEJ TO DATUM-SOEKNING                                        
098300                    IDARTNR-SOEKNING                                      
098400        MOVE JA TO DATUM-IDARTNR-SOEKNING                                 
098500     END-IF                                                               
098600                                                                          
098700     IF IDDC-SOEKNING = JA                                                
098800        MOVE MSGI-IDDC-KEY TO WS-IDDC                                     
098900************* 990316                                                      
099000        IF WS-IDDC = SPACE                                                
099100           MOVE ZERO TO WS-IDDC                                           
099200        END-IF                                                            
099300************* 990316                                                      
099400        IF GOOD-DC                                                        
099500        OR WS-IDDC = ZERO                                                 
099600           MOVE WS-IDDC TO W-IDDC                                         
099700        ELSE                                                              
099800           MOVE NEJ TO NYCKLAR-SW                                         
099900        END-IF                                                            
100000     ELSE                                                                 
100100        MOVE MSGI-IDDC TO W-IDDC                                          
100200                          WS-IDDC                                         
100300     END-IF                                                               
100400                                                                          
100500     IF IDPERSON-SOEKNING = JA                                            
100600        IF WS-IDPERSON-FOM NUMERIC                                        
100700           MOVE WS-IDPERSON-FOM TO W-IDPERSON-MIN                         
100800        ELSE                                                              
100900           MOVE NEJ TO NYCKLAR-SW                                         
101000        END-IF                                                            
101100        IF WS-IDPERSON-TOM NUMERIC                                        
101200           MOVE WS-IDPERSON-TOM TO W-IDPERSON-MAX                         
101300        ELSE                                                              
101400           MOVE NEJ TO NYCKLAR-SW                                         
101500        END-IF                                                            
101600                                                                          
101700        IF INDATA-OK                                                      
101800           IF WS-IDPERSON-FOM = ZERO                                      
101900              IF WS-IDPERSON-TOM = ZERO                                   
102000                 MOVE NEJ TO IDPERSON-SOEKNING                            
102100              END-IF                                                      
102200           ELSE                                                           
102300              IF WS-IDPERSON-TOM = ZERO                                   
102400                 MOVE WS-IDPERSON-FOM TO WS-IDPERSON-TOM                  
102500                                         W-IDPERSON-MAX                   
102600              END-IF                                                      
102700           END-IF                                                         
102800                                                                          
102900           IF WS-IDPERSON-FOM = WS-IDPERSON-TOM                           
103000              MOVE WS-IDPERSON-FOM TO WS-IDPERSON-TOM                     
103100                                      W-IDPERSON-MAX                      
103200           ELSE                                                           
103300              IF WS-IDPERSON-FOM < WS-IDPERSON-TOM                        
103400                 CONTINUE                                                 
103500              ELSE                                                        
103600                 MOVE NEJ TO NYCKLAR-SW                                   
103700              END-IF                                                      
103800           END-IF                                                         
103900        END-IF                                                            
104000                                                                          
104100     END-IF                                                               
104200                                                                          
104300     IF KDARBTYP-SOEKNING = JA                                            
104400        IF MSGI-KDARBTYP = 'ANSK' OR 'ESC' OR 'QUAL'                      
104500           IF MSGI-KDARBTYP = 'ANSK'                                      
104600              IF IDPERSON-SOEKNING = JA                                   
104700                 MOVE JA TO KDARBTYP-PERSON-SOEKNING                      
104800                 MOVE NEJ TO KDARBTYP-SOEKNING                            
104900                             IDPERSON-SOEKNING                            
105000              END-IF                                                      
105100              MOVE W-IDDC   TO SW-WS-IDDC                                 
105200              IF SW-CDC-SE OR SW-NDC-CN OR SW-NDC-US                      
105300                 CONTINUE                                                 
105400              ELSE                                                        
105500                 MOVE NEJ TO NYCKLAR-SW                                   
105600              END-IF                                                      
105700           ELSE                                                           
105800              IF IDPERSON-SOEKNING = JA                                   
105900                 MOVE JA TO KDARBTYP-PERSON-SOEKNING                      
106000                 MOVE NEJ TO KDARBTYP-SOEKNING                            
106100                             IDPERSON-SOEKNING                            
106200              END-IF                                                      
106300           END-IF                                                         
106400           MOVE MSGI-KDARBTYP TO W-KDARBTYP                               
106500                                 W-6327-KDARBTYP                          
106600        ELSE                                                              
106700           MOVE NEJ TO NYCKLAR-SW                                         
106800        END-IF                                                            
106900     ELSE                                                                 
107000        MOVE NEJ TO NYCKLAR-SW                                            
107100     END-IF                                                               
107200                                                                          
107300     IF GODK-MID OR NYCKLAR-OK                                            
107400        MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-UT                           
107500                                MOD-IDDC-UT                               
107600                                MOD-IDPERSON-FOM-UT                       
107700                                MOD-IDPERSON-TOM-UT                       
107800                                MOD-IDARTNR-UT                            
107900                                MOD-TIDATUM-UT                            
108000        IF KDARBTYP-PERSON-SOEKNING = JA                                  
108100           MOVE MSGI-KDARBTYP      TO MOD-KDARBTYP-UT                     
108200           MOVE WS-IDPERSON-FOM    TO MOD-IDPERSON-FOM-UT                 
108300           MOVE WS-IDPERSON-TOM    TO MOD-IDPERSON-TOM-UT                 
108400           INSPECT MOD-IDPERSON-FOM-UT                                    
108500                      REPLACING LEADING ZERO BY SPACE                     
108600           INSPECT MOD-IDPERSON-TOM-UT                                    
108700                      REPLACING LEADING ZERO BY SPACE                     
108800        END-IF                                                            
108900        IF IDARTNR-SOEKNING = JA                                          
109000           MOVE MSGI-IDARTNR       TO MOD-IDARTNR-UT                      
109100           INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE         
109200        END-IF                                                            
109300        IF DATUM-SOEKNING = JA                                            
109400           MOVE MSGI-TISKROT-BEORD TO MOD-TIDATUM-UT                      
109500           INSPECT MOD-TIDATUM-UT REPLACING LEADING ZERO BY SPACE         
109600        END-IF                                                            
109700        IF DATUM-IDARTNR-SOEKNING = JA                                    
109800           MOVE MSGI-TISKROT-BEORD TO MOD-TIDATUM-UT                      
109900           INSPECT MOD-TIDATUM-UT REPLACING LEADING ZERO BY SPACE         
110000           MOVE MSGI-IDARTNR       TO MOD-IDARTNR-UT                      
110100           INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE         
110200        END-IF                                                            
110300        IF KDARBTYP-SOEKNING = JA                                         
110400           MOVE MSGI-KDARBTYP      TO MOD-KDARBTYP-UT                     
110500        END-IF                                                            
110600        MOVE WS-IDDC            TO MOD-IDDC-UT                            
110700**************** 990413                                                   
110800*       IF GOOD-DC                                                        
110900*          MOVE WS-IDDC         TO MOD-IDDC-UT                            
111000*          INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE            
111100*       END-IF                                                            
111200**************** 990313                                                   
111300     ELSE                                                                 
111400        MOVE MFS-RENSA-FAELT TO MOD-KDARBTYP-UT                           
111500                                MOD-IDDC-UT                               
111600                                MOD-IDPERSON-FOM-UT                       
111700                                MOD-IDPERSON-TOM-UT                       
111800                                MOD-IDARTNR-UT                            
111900                                MOD-TIDATUM-UT                            
112000     END-IF                                                               
112100                                                                          
112200     IF NYCKLAR-FEL                                                       
112300        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
112400        CALL WMEDKONV USING MED-WMEDAREA                                  
112500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
112600        PERFORM MFS-RENSA-FAELT-IN                                        
112700        PERFORM MFS-RENSA-FAELT-UT                                        
112800        PERFORM MFS-RENSA-SPAR-NYCKLAR                                    
112900     END-IF                                                               
113000     .                                                                    
113100     EJECT                                                                
113200 C-FOERSTA-SIDA SECTION.                                                  
113300                                                                          
113400     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
113500     CALL WMEDKONV USING MED-WMEDAREA                                     
113600     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
113700     PERFORM MFS-RENSA-FAELT-IN                                           
113800     .                                                                    
113900     EJECT                                                                
114000 D-NAESTA-SIDA SECTION.                                                   
114100                                                                          
114200     IF SPAR-IDTRANS = '6322'                                             
114300        MOVE SPAR-IDDC-NEXT   TO SW-WS-IDDC                               
114400        IF SPAR-IDARTNR-NEXT NUMERIC                                      
114500        AND SPAR-IDANSK-NEXT-MIN NUMERIC                                  
114600        AND SPAR-IDANSK-NEXT-MAX NUMERIC                                  
114700        AND SPAR-DASKROT9-BEORD-NEXT NUMERIC                              
114800        AND SW-GOOD-DC                                                    
114900           IF SPAR-KDARBTYP-NEXT = 'ANSK' OR 'ESC' OR 'QUAL'              
115000               MOVE SPAR-IDARTNR-NEXT        TO W-IDARTNR-MIN             
115100                                                W-IDARTNR                 
115200               MOVE SPAR-DASKROT9-BEORD-NEXT TO W-DASKROT9-MIN            
115300               MOVE SPAR-IDDC-NEXT           TO W-IDDC-MIN                
115400                                                W-IDDC                    
115410                                                WS-IDDC                   
115500               MOVE SPAR-KDARBTYP-NEXT       TO W-KDARBTYP                
115600                                                W-6327-KDARBTYP           
115700               MOVE SPAR-IDANSK-NEXT-MIN     TO W-IDPERSON-MIN            
115800               MOVE SPAR-IDANSK-NEXT-MAX     TO W-IDPERSON-MAX            
115900            ELSE                                                          
116000               PERFORM MFS-RENSA-FAELT-IN                                 
116100            END-IF                                                        
116200        ELSE                                                              
116300           PERFORM MFS-RENSA-FAELT-IN                                     
116400        END-IF                                                            
116500     ELSE                                                                 
116600       PERFORM MFS-RENSA-FAELT-IN                                         
116700     END-IF                                                               
116800     .                                                                    
116900     EJECT                                                                
117000 E-SAMMA-SIDA SECTION.                                                    
117100                                                                          
117200     IF SPAR-IDTRANS = '6322'                                             
117300        MOVE SPAR-IDDC-ENTER  TO SW-WS-IDDC                               
117400        IF SPAR-IDARTNR-ENTER NUMERIC                                     
117500        AND SPAR-IDANSK-ENTER NUMERIC                                     
117600        AND SPAR-DASKROT9-BEORD-ENTER NUMERIC                             
117700        AND GOOD-DC                                                       
117800           IF SPAR-KDARBTYP-ENTER = 'ANSK' OR 'ESC' OR 'QUAL'             
117900              MOVE SPAR-IDARTNR-ENTER        TO W-IDARTNR-MIN             
118000                                                W-IDARTNR                 
118100              MOVE SPAR-IDDC-ENTER           TO W-IDDC-MIN                
118200                                                W-IDDC                    
118210                                                WS-IDDC                   
118300              MOVE SPAR-DASKROT9-BEORD-ENTER TO W-DASKROT9-MIN            
118400              MOVE SPAR-KDARBTYP-ENTER       TO W-KDARBTYP                
118500                                                W-6327-KDARBTYP           
118600            ELSE                                                          
118700               PERFORM MFS-RENSA-FAELT-IN                                 
118800            END-IF                                                        
118900        ELSE                                                              
119000           PERFORM MFS-RENSA-FAELT-IN                                     
119100        END-IF                                                            
119200     ELSE                                                                 
119300       PERFORM MFS-RENSA-FAELT-IN                                         
119400     END-IF                                                               
119500                                                                          
119600     IF SPAR-IDTRANS = '6322'                                             
119700        IF MID-CMD(1)  = ALL '+'                                          
119800        AND MID-CMD(2) = ALL '+'                                          
119900        AND MID-CMD(3) = ALL '+'                                          
120000        AND MID-CMD(4) = ALL '+'                                          
120100        AND MID-CMD(5) = ALL '+'                                          
120200        AND MID-CMD(6) = ALL '+'                                          
120300        AND MID-CMD(7) = ALL '+'                                          
120400        AND MID-CMD(8) = ALL '+'                                          
120500        AND MID-CMD(9) = ALL '+'                                          
120600        AND MID-CMD(10)= ALL '+'                                          
120700        AND MID-CMD(11)= ALL '+'                                          
120800        AND MID-CMD(12)= ALL '+'                                          
120900        AND MID-FLKLAR = ALL '+'                                          
121000           PERFORM MFS-RENSA-FAELT-IN                                     
121100        ELSE                                                              
121200          MOVE +1 TO RAD-IX                                               
121300          PERFORM UNTIL RAD-IX > 12                                       
121400            IF MID-CMD(RAD-IX) = 'T' OR 'O'                               
121500              IF MID-CMD(RAD-IX) = 'T'                                    
121600                MOVE MID-IDARTNR(RAD-IX) TO                               
121700                                    MOD6325-MID-IDARTNR-IN                
121800                MOVE MID-IDDC(RAD-IX) TO                                  
121900                                    MOD6325-MID-IDDC-IN                   
122000                IF JUMP-6325                                              
122100                  MOVE MFS-ALFA-FAELT-FEL                                 
122200                                     TO MOD-CMD-ATTR(RAD-IX)              
122300                  MOVE NEJ           TO JUMP-6325-SW                      
122400                ELSE                                                      
122500                  MOVE JA            TO JUMP-6325-SW                      
122600                END-IF                                                    
122700              ELSE                                                        
122800                MOVE RAD-IX          TO O-IX                              
122900                INSPECT MID-IDARTNR(O-IX)                                 
123000                   REPLACING LEADING SPACE BY ZERO                        
123100                IF MID-IDARTNR(O-IX) NUMERIC                              
123200                   MOVE JA                TO SW-VISA-6325                 
123300                   MOVE MID-IDDC(O-IX)    TO WO-IDDC-6325                 
123400                   MOVE MID-IDARTNR(O-IX) TO WO-IDARTNR                   
123500                   MOVE MID-TIDATUM(O-IX) TO WS-DASKROT-AAMMDD            
123600                   MOVE 20                TO WS-DASKROT-SS                
123700                   COMPUTE WO-DASKROT9 =                                  
123800                      99999999 - WS-AAAAMMDD                              
123900                END-IF                                                    
124000              END-IF                                                      
124100            ELSE                                                          
124200              IF MID-CMD(RAD-IX) = 'R' AND MSGI-KDARBTYP = 'ESC'          
124300                MOVE MSGI-IDDC-KEY     TO W-IDDC-B6                       
124400                PERFORM IMS-GU-WDB601                                     
124500                IF DCS-KDDC = 'S' OR 'NP' OR 'NC' OR 'NX' OR 'NS'         
124600                  MOVE JA            TO JUMP-2372-SW                      
124700                  MOVE MID-IDARTNR(RAD-IX) TO 2372-MID-IDARTNR-IN         
124800                  INSPECT 2372-MID-IDARTNR-IN                             
124900                          REPLACING LEADING SPACE BY ZERO                 
125000                  MOVE MID-IDDC(RAD-IX)    TO 2372-MID-IDDC-IN            
125100                  MOVE '+'                 TO 2372-MID-IDTYPE-IN          
125200                  MOVE ZERO            TO 2372-MID-IDPERSON-BUY-IN        
125300                  MOVE '+'                 TO 2372-MID-IDSTATUS-IN        
125400                  MOVE '+'                 TO 2372-MID-IDREFTYP-IN        
125500                                                                          
125600                  MOVE MFS-RENSA-FAELT TO 2372-MID-IDARTNR-UT             
125700                                          2372-MID-IDDC-UT                
125800                                          2372-MID-IDTYPE-UT              
125900                                          2372-MID-IDPERSON-BUY-UT        
126000                                          2372-MID-IDSTATUS-UT            
126100                                          2372-MID-IDREFTYP-UT            
126200                ELSE                                                      
126300                  IF DCS-KDDC = 'NA'                                      
126400                   MOVE MID-IDARTNR(RAD-IX) TO 2352-MID-IDARTNR-IN        
126500                    INSPECT 2352-MID-IDARTNR-IN                           
126600                            REPLACING LEADING SPACE BY ZERO               
126700                    MOVE MID-IDDC(RAD-IX)                                 
126800                                      TO 2352-MID-IDDC-IN                 
126900                    MOVE '+'          TO 2352-MID-IDTYPE-IN               
127000                    MOVE ZERO         TO 2352-MID-IDPERSON-BUY-IN         
127100                    MOVE '+'          TO 2352-MID-IDSTATUS-IN             
127200                    MOVE '+'          TO 2352-MID-IDREFTYP-IN             
127300                                                                          
127400                    MOVE MFS-RENSA-FAELT TO 2352-MID-IDARTNR-UT           
127500                                            2352-MID-IDDC-UT              
127600                                            2352-MID-IDTYPE-UT            
127700                                          2352-MID-IDPERSON-BUY-UT        
127800                                            2352-MID-IDSTATUS-UT          
127900                                            2352-MID-IDREFTYP-UT          
128000                                                                          
128100                   MOVE JA          TO JUMP-2352-SW                       
128200                  END-IF                                                  
128300                END-IF                                                    
128400              ELSE                                                        
128500                IF MID-CMD(RAD-IX) = ALL '+'                              
128600                  CONTINUE                                                
128700                ELSE                                                      
128800                  MOVE NEJ           TO INDATA-SW                         
128900                END-IF                                                    
129000              END-IF                                                      
129100            END-IF                                                        
129200            ADD +1 TO RAD-IX                                              
129300          END-PERFORM                                                     
129400          IF (NO-JUMP-6325 OR NO-JUMP-2352 OR NO-JUMP-2372) AND           
129500             INDATA-FEL                                                   
129600            MOVE INF-PRESS-PF11 TO MED-IDMFSINF                           
129700            CALL WMEDKONV USING MED-WMEDAREA                              
129800            MOVE MED-MFSINF TO MOD-TEMFSFEL                               
129900            PERFORM EA-MID-INDATA-TILL-MOD                                
130000          END-IF                                                          
130100        END-IF                                                            
130200     ELSE                                                                 
130300        PERFORM MFS-RENSA-FAELT-IN                                        
130400     END-IF                                                               
130500     .                                                                    
130600     EJECT                                                                
130700 EA-MID-INDATA-TILL-MOD SECTION.                                          
130800                                                                          
130900     IF MID-FLKLAR NOT = ALL '+'                                          
131000        MOVE MFS-ROER-EJ-FAELT TO MOD-FLKLAR                              
131100     ELSE                                                                 
131200        MOVE MFS-RENSA-FAELT   TO MOD-FLKLAR                              
131300     END-IF                                                               
131400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKLAR-ATTR                        
131500                                                                          
131600     MOVE +1 TO RAD-IX                                                    
131700     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
131800        IF MID-CMD(RAD-IX) NOT = ALL '+'                                  
131900           MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-IN(RAD-IX)                   
132000        ELSE                                                              
132100           MOVE MFS-RENSA-FAELT   TO MOD-CMD-IN(RAD-IX)                   
132200        END-IF                                                            
132300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR(RAD-IX)                
132400        ADD +1 TO RAD-IX                                                  
132500     END-PERFORM                                                          
132600     .                                                                    
132700     EJECT                                                                
132800 F-LAES-VISA-INFO SECTION.                                                
132900                                                                          
133000     MOVE 'A' TO WS-VAR                                                   
133100     IF WS-IDDC = ZERO                                                    
133200        PERFORM FD-KOLLA-IDDC                                             
133300     END-IF                                                               
133400                                                                          
133500     PERFORM S11-LAES-GRUNDDATA                                           
133600     MOVE +1 TO RAD-IX                                                    
133700                                                                          
133800     IF SEGMENT-SAKNAS                                                    
133900     MOVE 'B' TO WS-VAR                                                   
134000        IF WS-HIGHLEV = JA                                                
134100           MOVE INF-HIGHER-LEV TO MED-IDMFSINF                            
134200        ELSE                                                              
134300           MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                          
134400        END-IF                                                            
134500        CALL WMEDKONV USING MED-WMEDAREA                                  
134600        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
134700        PERFORM MFS-RENSA-FAELT-UT                                        
134800     ELSE                                                                 
134900     MOVE 'C' TO WS-VAR                                                   
135000        MOVE 6321-KDARBTYP TO SPAR-KDARBTYP-ENTER                         
135100                                                                          
135200        PERFORM S12-LAES-SKROTDATUM                                       
135300        IF SEGMENT-FINNS                                                  
135400     MOVE 'D' TO WS-VAR                                                   
135500           COMPUTE WS-DASKROT9 = 99999999 - 6322-DASKROT9-BEORD           
135600           END-COMPUTE                                                    
135700           MOVE 6322-DASKROT9-BEORD                                       
135800                           TO SPAR-DASKROT9-BEORD-ENTER                   
135900           PERFORM S13-LAES-RADDATA                                       
136000           MOVE 6324-IDDC TO SPAR-IDDC-ENTER                              
136100           IF SEGMENT-SAKNAS                                              
136200              PERFORM FE-SOEK-FORSTA-ART                                  
136300              IF SEGMENT-SAKNAS                                           
136400                 IF WS-HIGHLEV = JA                                       
136500                    MOVE INF-HIGHER-LEV TO MED-IDMFSINF                   
136600                 ELSE                                                     
136700                    MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                 
136800                 END-IF                                                   
136900                 CALL WMEDKONV USING MED-WMEDAREA                         
137000                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
137100                 PERFORM MFS-RENSA-FAELT-UT                               
137200              END-IF                                                      
137300           END-IF                                                         
137400        ELSE                                                              
137500     MOVE 'E' TO WS-VAR                                                   
137600           IF WS-IDDC = ZERO                                              
137700              PERFORM FG-SOEK-FORSTA-DATUM                                
137800              IF SEGMENT-SAKNAS                                           
137900                 IF WS-HIGHLEV = JA                                       
138000                    MOVE INF-HIGHER-LEV TO MED-IDMFSINF                   
138100                 ELSE                                                     
138200                    MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                 
138300                 END-IF                                                   
138400                 CALL WMEDKONV USING MED-WMEDAREA                         
138500                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
138600                 PERFORM MFS-RENSA-FAELT-UT                               
138700              END-IF                                                      
138800           ELSE                                                           
138900              IF WS-HIGHLEV = JA                                          
139000                 MOVE INF-HIGHER-LEV TO MED-IDMFSINF                      
139100              ELSE                                                        
139200                 MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                    
139300              END-IF                                                      
139400              CALL WMEDKONV USING MED-WMEDAREA                            
139500              MOVE MED-MFSINF TO MOD-TEMFSINF                             
139600              PERFORM MFS-RENSA-FAELT-UT                                  
139700           END-IF                                                         
139800        END-IF                                                            
139900     END-IF                                                               
140000                                                                          
140100     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
140200       IF SEGMENT-FINNS                                                   
140300     MOVE 'F' TO WS-VAR                                                   
140400           IF 6324-FLSKROT-GODK = 'J'                                     
140500             ADD -1 TO RAD-IX                                             
140600           ELSE                                                           
140700             IF RAD-IX = 1                                                
140800                  MOVE 6322-DASKROT9-BEORD                                
140900                           TO SPAR-DASKROT9-BEORD-ENTER                   
141000                  MOVE 6324-IDDC     TO SPAR-IDDC-ENTER                   
141100                  MOVE 6324-IDARTNR  TO MOD-IDARTNR(RAD-IX)               
141200                                        SPAR-IDARTNR-ENTER                
141300                  MOVE 6324-IDPERSON TO SPAR-IDANSK-ENTER                 
141400             END-IF                                                       
141500             MOVE WS-DASKROT9        TO MOD-TIDATUM(RAD-IX)               
141600             MOVE 6324-IDARTNR       TO MOD-IDARTNR(RAD-IX)               
141700                                        W-IDARTNR                         
141800                                        W-IDARTNR-KVAL                    
141900             MOVE 6324-IDDC          TO MOD-IDDC(RAD-IX)                  
142000                                        W-IDDC-KVAL                       
142100             MOVE 6324-IDPERSON      TO MOD-IDPERSON(RAD-IX)              
142200             MOVE 6324-IDUSER        TO MOD-IDUSER(RAD-IX)                
142300             MOVE 6324-KVSKROT-BEORD TO MOD-KVANTAL(RAD-IX)               
142400             IF 6324-IDDISTR = 81                                         
142500             AND 6324-IDKUNDNR = 111                                      
142600               MOVE 'Y'              TO MOD-FLCLASS(RAD-IX)               
142700             END-IF                                                       
142800                                                                          
142900             MOVE 1                  TO W-KDSTASKR-KVAL                   
143000             PERFORM IMS-GU-WDGX6324-6325                                 
143100             PERFORM IMS-GNP-WDGX6325                                     
143200             IF SEGMENT-FINNS                                             
143300               MOVE 'Y'              TO MOD-FLTEXT(RAD-IX)                
143400             END-IF                                                       
143500                                                                          
143600             MOVE W-IDDC TO SPAR-IDDC                                     
143700             MOVE 6324-IDDC TO  SW-WS-IDDC                                
143800             IF SW-NDC-NA OR SW-NDC-CN OR SW-LDC-CN                       
143900                PERFORM IMS-GHU-ARTS11                                    
144000                IF SEGMENT-FINNS                                          
144100                   COMPUTE WS-SUARTSTD                                    
144200                      = 6324-KVSKROT-BEORD * SLAG-PRAVCOST                
144300                   END-COMPUTE                                            
144400                   MOVE WS-SUARTSTD     TO MOD-SUARTSTD(RAD-IX)           
144500                                           W-SUARTSTD(RAD-IX)             
144600                ELSE                                                      
144700                   MOVE MFS-RENSA-FAELT TO MOD-SUARTSTD(RAD-IX)           
144800                   MOVE ZERO            TO W-SUARTSTD(RAD-IX)             
144900                END-IF                                                    
145000                MOVE SPAR-IDDC TO W-IDDC                                  
145100             ELSE                                                         
145200                PERFORM IMS-GHU-ARTC11                                    
145300                IF SEGMENT-FINNS                                          
145400                   COMPUTE WS-SUARTSTD                                    
145500                      = 6324-KVSKROT-BEORD * CLAG-PRARTSTD                
145600                   END-COMPUTE                                            
145700                   MOVE WS-SUARTSTD    TO MOD-SUARTSTD(RAD-IX)            
145800                                          W-SUARTSTD(RAD-IX)              
145900                ELSE                                                      
146000                   MOVE MFS-RENSA-FAELT TO MOD-SUARTSTD(RAD-IX)           
146100                   MOVE ZERO            TO W-SUARTSTD(RAD-IX)             
146200                END-IF                                                    
146300             END-IF                                                       
146400             IF 6324-BELAGINS-DEL = SPACE                                 
146500                MOVE MFS-RENSA-FAELT  TO MOD-REM(RAD-IX)                  
146600             ELSE                                                         
146700                MOVE 'Y'              TO MOD-REM(RAD-IX)                  
146800             END-IF                                                       
146900             IF 6324-IDKONTO   > ZERO                                     
147000             OR 6324-IDKST     > SPACE                                    
147100             OR (6324-IDANALYS NOT = SPACE)                               
147200                MOVE 'Y'              TO MOD-ACC(RAD-IX)                  
147300             ELSE                                                         
147400                IF (6324-IDUSER = 'W2617400') AND                         
147500                   (6324-KDERS-UTG = 22 OR 23 OR 25 OR 26)                
147600                   MOVE 'Y'              TO MOD-ACC(RAD-IX)               
147700                ELSE                                                      
147800                   MOVE MFS-RENSA-FAELT  TO MOD-ACC(RAD-IX)               
147900                END-IF                                                    
148000             END-IF                                                       
148100          END-IF                                                          
148200                                                                          
148300          PERFORM S13-LAES-RADDATA                                        
148400          IF SEGMENT-FINNS                                                
148500             CONTINUE                                                     
148600          ELSE                                                            
148700             PERFORM FF-SOEK-NASTA-ART                                    
148800          END-IF                                                          
148900       ELSE                                                               
149000         IF RAD-IX = 1                                                    
149100              IF WS-HIGHLEV = JA                                          
149200                 MOVE INF-HIGHER-LEV TO MED-IDMFSINF                      
149300              ELSE                                                        
149400                 MOVE INF-URVAL-SAKNAS TO MED-IDMFSINF                    
149500              END-IF                                                      
149600              CALL WMEDKONV USING MED-WMEDAREA                            
149700              MOVE MED-MFSINF TO MOD-TEMFSINF                             
149800          END-IF                                                          
149900          MOVE MFS-RENSA-FAELT TO MOD-CMD-IN(RAD-IX)                      
150000                                  MOD-IDARTNR(RAD-IX)                     
150100                                  MOD-KVANTAL(RAD-IX)                     
150200                                  MOD-SUARTSTD(RAD-IX)                    
150300                                  MOD-TIDATUM(RAD-IX)                     
150400                                  MOD-IDDC(RAD-IX)                        
150500                                  MOD-IDPERSON(RAD-IX)                    
150600                                  MOD-REM(RAD-IX)                         
150700                                  MOD-IDUSER(RAD-IX)                      
150800                                  MOD-ACC(RAD-IX)                         
150900       END-IF                                                             
151000       ADD 1 TO RAD-IX                                                    
151100     END-PERFORM                                                          
151200                                                                          
151300     IF SEGMENT-FINNS AND 6324-FLSKROT-GODK = 'N'                         
151400        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSFEL                         
151500        CALL WMEDKONV USING MED-WMEDAREA                                  
151600        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
151700        MOVE 6324-IDDC           TO SPAR-IDDC-NEXT                        
151800        MOVE 6321-KDARBTYP       TO SPAR-KDARBTYP-NEXT                    
151900        MOVE 6322-DASKROT9-BEORD TO SPAR-DASKROT9-BEORD-NEXT              
152000        MOVE 6324-IDARTNR        TO SPAR-IDARTNR-NEXT                     
152100*       MOVE 6324-IDPERSON       TO SPAR-IDANSK-NEXT                      
152200        MOVE W-IDPERSON-MIN      TO SPAR-IDANSK-NEXT-MIN                  
152300        MOVE W-IDPERSON-MAX      TO SPAR-IDANSK-NEXT-MAX                  
152400     ELSE                                                                 
152500        MOVE MFS-RENSA-FAELT TO  SPAR-IDDC-NEXT                           
152600                                 SPAR-KDARBTYP-NEXT                       
152700                                 SPAR-DASKROT9-BEORD-NEXT                 
152800                                 SPAR-IDARTNR-NEXT                        
152900                                 SPAR-IDANSK-NEXT-MIN                     
153000                                 SPAR-IDANSK-NEXT-MAX                     
153100     END-IF                                                               
153200                                                                          
153300     IF SW-VISA-6325 = JA                                                 
153400       MOVE WO-IDDC-6325   TO SW-WS-IDDC                                  
153500       IF SW-NDC-CN AND MSGI-KDARBTYP = 'ANSK'                            
153600         CONTINUE                                                         
153700       ELSE                                                               
153800         IF MSGI-KDARBTYP = 'ANSK'                                        
153900           MOVE 'ANSK'      TO W-6321-KDARBTYP                            
154000           MOVE '11'        TO W-IDDC-KVAL                                
154100         ELSE                                                             
154200           IF MSGI-KDARBTYP = 'ESC'                                       
154300             MOVE 'ESC'     TO W-6321-KDARBTYP                            
154400             MOVE MSGI-IDDC-KEY TO W-IDDC-KVAL                            
154500           END-IF                                                         
154600         END-IF                                                           
154700*                                                                         
154800         MOVE WO-DASKROT9   TO W-DASKROT9                                 
154900         MOVE WO-IDARTNR    TO W-IDARTNR-KVAL                             
155000         MOVE 1             TO W-KDSTASKR-KVAL                            
155100         PERFORM IMS-GU-WDGX6324-6325                                     
155200         IF SEGMENT-FINNS                                                 
155300            MOVE SPACE TO WO-TEMFSINF-TAB                                 
155400            MOVE +1    TO WO-IX                                           
155500            PERFORM IMS-GNP-WDGX6325                                      
155600            PERFORM UNTIL SEGMENT-SAKNAS                                  
155700               IF MSGI-KDARBTYP = 'ANSK'                                  
155800                 PERFORM FH-ORSAKSTEXTER                                  
155900               ELSE                                                       
156000                 IF MSGI-KDARBTYP = 'ESC'                                 
156100                   PERFORM FI-ORSAKSTEXTER                                
156200                 END-IF                                                   
156300               END-IF                                                     
156400               PERFORM IMS-GNP-WDGX6325                                   
156500            END-PERFORM                                                   
156600            MOVE WO-TEMFSINF-TAB TO MOD-TEMFSINF                          
156700         END-IF                                                           
156800       END-IF                                                             
156900     END-IF                                                               
157000                                                                          
157100     MOVE '002'      TO MSGI-KDCALL                                       
157200     MOVE '6322'     TO SPAR-IDTRANS                                      
157300     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
157400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
157500     .                                                                    
157600     EJECT                                                                
157700 FD-KOLLA-IDDC SECTION.                                                   
157800                                                                          
157900     MOVE W-IDDC    TO SW-WS-IDDC                                         
158000     .                                                                    
158100     EJECT                                                                
158200 FE-SOEK-FORSTA-ART SECTION.                                              
158300                                                                          
158400     IF WS-IDDC = ZERO                                                    
158500        MOVE NEJ TO WS-SKROTDATUM-SLUT                                    
158600        PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                             
158700           PERFORM S12-LAES-SKROTDATUM                                    
158800           IF SEGMENT-FINNS                                               
158900              COMPUTE WS-DASKROT9 =                                       
159000                  99999999 - 6322-DASKROT9-BEORD                          
159100              END-COMPUTE                                                 
159200              MOVE 6322-DASKROT9-BEORD                                    
159300                  TO SPAR-DASKROT9-BEORD-ENTER                            
159400              MOVE ZERO TO W-IDARTNR-MIN                                  
159500              PERFORM S13-LAES-RADDATA                                    
159600              IF SEGMENT-FINNS                                            
159700                 MOVE JA TO WS-SKROTDATUM-SLUT                            
159800              END-IF                                                      
159900           ELSE                                                           
160000              SET TAB-IX UP BY +1                                         
160100              PERFORM S11-LAES-GRUNDDATA                                  
160200              IF SEGMENT-FINNS                                            
160300                  MOVE ZERO TO W-DASKROT9                                 
160400                               W-DASKROT9-MIN                             
160500              ELSE                                                        
160600                  MOVE JA TO WS-SKROTDATUM-SLUT                           
160700              END-IF                                                      
160800           END-IF                                                         
160900        END-PERFORM                                                       
161000     ELSE                                                                 
161100        MOVE NEJ TO WS-SKROTDATUM-SLUT                                    
161200        PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                             
161300           PERFORM S12-LAES-SKROTDATUM                                    
161400           IF SEGMENT-FINNS                                               
161500              COMPUTE WS-DASKROT9 =                                       
161600                  99999999 - 6322-DASKROT9-BEORD                          
161700              END-COMPUTE                                                 
161800              MOVE 6322-DASKROT9-BEORD                                    
161900                  TO SPAR-DASKROT9-BEORD-ENTER                            
162000              MOVE ZERO TO W-IDARTNR-MIN                                  
162100              PERFORM S13-LAES-RADDATA                                    
162200              IF SEGMENT-FINNS                                            
162300                 MOVE JA TO WS-SKROTDATUM-SLUT                            
162400              END-IF                                                      
162500           ELSE                                                           
162600              MOVE JA TO WS-SKROTDATUM-SLUT                               
162700           END-IF                                                         
162800       END-PERFORM                                                        
162900     END-IF                                                               
163000     .                                                                    
163100     EJECT                                                                
163200 FF-SOEK-NASTA-ART SECTION.                                               
163300                                                                          
163400     IF WS-IDDC = ZERO                                                    
163500        MOVE NEJ TO WS-SKROTDATUM-SLUT                                    
163600                    WS-ART-SLUT                                           
163700        PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                             
163800           PERFORM S12-LAES-SKROTDATUM                                    
163900           IF SEGMENT-FINNS                                               
164000              COMPUTE WS-DASKROT9 =                                       
164100                  99999999 - 6322-DASKROT9-BEORD                          
164200              END-COMPUTE                                                 
164300              MOVE ZERO TO W-IDARTNR-MIN                                  
164400              PERFORM S13-LAES-RADDATA                                    
164500              PERFORM UNTIL SEGMENT-SAKNAS OR (WS-ART-SLUT = JA)          
164600                 IF SEGMENT-FINNS                                         
164700                    IF 6324-FLSKROT-GODK = 'N'                            
164800                       MOVE JA TO WS-SKROTDATUM-SLUT                      
164900                                  WS-ART-SLUT                             
165000                    ELSE                                                  
165100                       PERFORM S13-LAES-RADDATA                           
165200                    END-IF                                                
165300                 ELSE                                                     
165400                    MOVE JA TO WS-ART-SLUT                                
165500                 END-IF                                                   
165600              END-PERFORM                                                 
165700           ELSE                                                           
165800              SET TAB-IX UP BY +1                                         
165900              PERFORM S11-LAES-GRUNDDATA                                  
166000              IF SEGMENT-FINNS                                            
166100                  MOVE ZERO TO W-DASKROT9                                 
166200                               W-DASKROT9-MIN                             
166300              ELSE                                                        
166400                  MOVE JA TO WS-SKROTDATUM-SLUT                           
166500              END-IF                                                      
166600           END-IF                                                         
166700       END-PERFORM                                                        
166800     ELSE                                                                 
166900        MOVE NEJ TO WS-SKROTDATUM-SLUT                                    
167000                    WS-ART-SLUT                                           
167100        PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                             
167200           PERFORM S12-LAES-SKROTDATUM                                    
167300           IF SEGMENT-FINNS                                               
167400              COMPUTE WS-DASKROT9 =                                       
167500                  99999999 - 6322-DASKROT9-BEORD                          
167600              END-COMPUTE                                                 
167700              MOVE ZERO TO W-IDARTNR-MIN                                  
167800              PERFORM S13-LAES-RADDATA                                    
167900              PERFORM UNTIL SEGMENT-SAKNAS OR (WS-ART-SLUT = JA)          
168000                 IF SEGMENT-FINNS                                         
168100                    IF 6324-FLSKROT-GODK = 'N'                            
168200                       MOVE JA TO WS-SKROTDATUM-SLUT                      
168300                                  WS-ART-SLUT                             
168400                    ELSE                                                  
168500                       PERFORM S13-LAES-RADDATA                           
168600                    END-IF                                                
168700                 ELSE                                                     
168800                    MOVE JA TO WS-ART-SLUT                                
168900                 END-IF                                                   
169000              END-PERFORM                                                 
169100           ELSE                                                           
169200              MOVE JA TO WS-SKROTDATUM-SLUT                               
169300           END-IF                                                         
169400       END-PERFORM                                                        
169500     END-IF                                                               
169600     .                                                                    
169700     EJECT                                                                
169800 FG-SOEK-FORSTA-DATUM SECTION.                                            
169900                                                                          
170000     SET TAB-IX UP BY +1                                                  
170100     MOVE NEJ TO WS-DATUM-HITTAD                                          
170200     PERFORM UNTIL WS-DATUM-HITTAD = JA  OR TAB-IX > TAB-IX-MAX           
170300                   OR TAB-IDDC(TAB-IX) = SPACE                            
170400        PERFORM S11-LAES-GRUNDDATA                                        
170500        IF SEGMENT-FINNS                                                  
170600           MOVE NEJ TO WS-SKROTDATUM-SLUT                                 
170700           PERFORM UNTIL WS-SKROTDATUM-SLUT = JA                          
170800              MOVE ZERO TO W-DASKROT9-MIN                                 
170900                           W-DASKROT9                                     
171000              PERFORM S12-LAES-SKROTDATUM                                 
171100              IF SEGMENT-FINNS                                            
171200                 COMPUTE WS-DASKROT9 =                                    
171300                     99999999 - 6322-DASKROT9-BEORD                       
171400                 END-COMPUTE                                              
171500                 MOVE 6322-DASKROT9-BEORD                                 
171600                     TO SPAR-DASKROT9-BEORD-ENTER                         
171700                 MOVE ZERO       TO W-IDARTNR-MIN                         
171800                 MOVE +999999999 TO W-IDARTNR-MAX                         
171900                 PERFORM S13-LAES-RADDATA                                 
172000                 IF SEGMENT-FINNS                                         
172100                    MOVE JA TO WS-SKROTDATUM-SLUT                         
172200                               WS-DATUM-HITTAD                            
172300                 ELSE                                                     
172400                    CONTINUE                                              
172500                 END-IF                                                   
172600              ELSE                                                        
172700                 MOVE JA TO WS-SKROTDATUM-SLUT                            
172800                 SET TAB-IX UP BY +1                                      
172900              END-IF                                                      
173000           END-PERFORM                                                    
173100        ELSE                                                              
173200           SET TAB-IX UP BY +1                                            
173300        END-IF                                                            
173400     END-PERFORM                                                          
173500     .                                                                    
173600     EJECT                                                                
173700 FH-ORSAKSTEXTER      SECTION.                                            
173800                                                                          
173900     MOVE +1      TO WS-IX                                                
174000     PERFORM UNTIL WS-IX > 11                                             
174100        IF 6325-TEMEMO (3:10) = WS-TEMEMO-ORS(WS-IX) (3:10)               
174200         IF WO-IX < 5                                                     
174300           MOVE WS-TEMEMO-KORT(WS-IX) TO WO-TEMFSINF-ORS(WO-IX)           
174400           IF WS-IX = 11                                                  
174500              MOVE 6325-TEMEMO (1:9)  TO WO-TEMFSINF-ORS(WO-IX)           
174600           END-IF                                                         
174700         ELSE                                                             
174800           MOVE 'FLER ORS.'           TO WO-TEMFSINF-ORS(5)               
174900           MOVE '- SE 6325'           TO WO-TEMFSINF-ORS(6)               
175000         END-IF                                                           
175100           ADD +1 TO WO-IX                                                
175200        END-IF                                                            
175300        ADD +1    TO WS-IX                                                
175400     END-PERFORM                                                          
175500     .                                                                    
175600     EJECT                                                                
175700 FI-ORSAKSTEXTER      SECTION.                                            
175800                                                                          
175900     MOVE +1      TO WS-IX                                                
176000     PERFORM UNTIL WS-IX > 11                                             
176100        IF 6325-TEMEMO (3:10) = WS-TEMEMO-ESC-ORS(WS-IX) (3:10)           
176200         IF WO-IX < 5                                                     
176300           MOVE WS-TEMEMO-ESC-KORT(WS-IX)                                 
176400                                      TO WO-TEMFSINF-ORS(WO-IX)           
176500           IF WS-IX = 11                                                  
176600              MOVE 6325-TEMEMO (1:9)  TO WO-TEMFSINF-ORS(WO-IX)           
176700           END-IF                                                         
176800         ELSE                                                             
176900           MOVE 'FLER ORS.'           TO WO-TEMFSINF-ORS(5)               
177000           MOVE '- SE 6325'           TO WO-TEMFSINF-ORS(6)               
177100         END-IF                                                           
177200           ADD +1 TO WO-IX                                                
177300        END-IF                                                            
177400        ADD +1    TO WS-IX                                                
177500     END-PERFORM                                                          
177600     .                                                                    
177700     EJECT                                                                
177800 G-KOLLA-INPUT SECTION.                                                   
177900                                                                          
178000     MOVE JA TO INDATA-SW                                                 
178100     MOVE SPACE TO WS-CMD                                                 
178200                                                                          
178300     IF MID-CMD(1)  = ALL '+'                                             
178400     AND MID-CMD(2) = ALL '+'                                             
178500     AND MID-CMD(3) = ALL '+'                                             
178600     AND MID-CMD(4) = ALL '+'                                             
178700     AND MID-CMD(5) = ALL '+'                                             
178800     AND MID-CMD(6) = ALL '+'                                             
178900     AND MID-CMD(7) = ALL '+'                                             
179000     AND MID-CMD(8) = ALL '+'                                             
179100     AND MID-CMD(9) = ALL '+'                                             
179200     AND MID-CMD(10)= ALL '+'                                             
179300     AND MID-CMD(11)= ALL '+'                                             
179400     AND MID-CMD(12)= ALL '+'                                             
179500     AND MID-FLKLAR = ALL '+'                                             
179600        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
179700        CALL WMEDKONV USING MED-WMEDAREA                                  
179800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
179900        PERFORM MFS-ROER-EJ-FAELT-IN                                      
180000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
180100        MOVE NEJ TO INDATA-SW                                             
180200     ELSE                                                                 
180300                                                                          
180400        IF MID-FLKLAR NOT = ALL '+'                                       
180500           IF MID-FLKLAR = 'J' OR 'Y'                                     
180600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR                
180700              PERFORM GA-KOLLA-FLKLAR                                     
180800           ELSE                                                           
180900              MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR                
181000              MOVE NEJ TO INDATA-SW                                       
181100           END-IF                                                         
181200        END-IF                                                            
181300                                                                          
181400        MOVE +1 TO RAD-IX                                                 
181500        PERFORM UNTIL RAD-IX > RAD-IX-MAX                                 
181600           IF MID-CMD(RAD-IX) NOT = ALL '+'                               
181700              IF MID-FLKLAR NOT = ALL '+'                                 
181800                 MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR(RAD-IX)          
181900                 MOVE NEJ TO INDATA-SW                                    
182000              ELSE                                                        
182100                 IF MID-CMD(RAD-IX) = 'B' OR 'D'                          
182200                    IF WS-CMD = 'B' OR 'D' OR SPACE                       
182300                       MOVE MID-CMD(RAD-IX) TO WS-CMD                     
182400                       MOVE MFS-ALFA-FAELT-RAETT                          
182500                                         TO MOD-CMD-ATTR(RAD-IX)          
182600                       PERFORM GC-KOLLA-IDUSER                            
182700                       MOVE 6328-BEANST-GODK TO ANUL-BEANST-GODK          
182800                       MOVE 6328-IDMAIL      TO ANUL-IDMAIL               
182900                    ELSE                                                  
183000                       MOVE MFS-ALFA-FAELT-FEL                            
183100                                        TO MOD-CMD-ATTR(RAD-IX)           
183200                       MOVE NEJ TO INDATA-SW                              
183300                    END-IF                                                
183400                 ELSE                                                     
183500                    IF MID-CMD(RAD-IX) = 'X' OR 'A'                       
183600                       IF WS-CMD = 'X' OR 'A' OR SPACE                    
183700                          MOVE MID-CMD(RAD-IX) TO WS-CMD                  
183800                          MOVE MFS-ALFA-FAELT-RAETT                       
183900                                         TO MOD-CMD-ATTR(RAD-IX)          
184000                          PERFORM GB-KOLLA-SKROTBEORD                     
184100                          PERFORM GC-KOLLA-IDUSER                         
184200                       ELSE                                               
184300                          MOVE MFS-ALFA-FAELT-FEL                         
184400                                         TO MOD-CMD-ATTR(RAD-IX)          
184500                          MOVE NEJ TO INDATA-SW                           
184600                       END-IF                                             
184700                    ELSE                                                  
184800                       MOVE MFS-ALFA-FAELT-FEL                            
184900                                       TO MOD-CMD-ATTR(RAD-IX)            
185000                       MOVE NEJ TO INDATA-SW                              
185100                    END-IF                                                
185200                 END-IF                                                   
185300              END-IF                                                      
185400           END-IF                                                         
185500           ADD +1 TO RAD-IX                                               
185600        END-PERFORM                                                       
185700                                                                          
185800        IF INDATA-FEL                                                     
185900           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
186000           CALL WMEDKONV USING MED-WMEDAREA                               
186100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
186200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
186300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
186400        END-IF                                                            
186500     END-IF                                                               
186600                                                                          
186700     .                                                                    
186800     EJECT                                                                
186900 GA-KOLLA-FLKLAR SECTION.                                                 
187000                                                                          
187100     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
187200     MOVE W-IDDC    TO SPAR-IDDC                                          
187300     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-Y2K                  
187400                                                                          
187500     MOVE +1 TO RAD-IX                                                    
187600     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
187700        INSPECT MID-IDARTNR(RAD-IX)                                       
187800                     REPLACING LEADING SPACE BY ZERO                      
187900        IF MID-IDARTNR(RAD-IX) NUMERIC                                    
188000           IF MID-IDARTNR(RAD-IX) > ZERO                                  
188100              MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                   
188200                                          WS-DASKROT-AAMMDD               
188300              IF WS-SEKEL = 9                                             
188400                 MOVE 19 TO WS-DASKROT-SS                                 
188500              ELSE                                                        
188600                 MOVE 20 TO WS-DASKROT-SS                                 
188700              END-IF                                                      
188800              MOVE MID-IDARTNR(RAD-IX)        TO W-IDARTNR                
188900              MOVE MID-IDDC(RAD-IX)           TO W-IDDC-6324              
189000                                                 W-6327-IDDC              
189100              MOVE MID-TIDATUM(RAD-IX)        TO WS-DASKROT-AAMMDD        
189200              MOVE MSGI-KDARBTYP              TO W-6321-KDARBTYP          
189300              MOVE 20                 TO WS-DASKROT-SS                    
189400              COMPUTE W-DASKROT9 = 99999999 -                             
189500                                   WS-AAAAMMDD                            
189600              PERFORM IMS-GU-WDGX6324-2                                   
189700              IF MSGI-IDUSER = 6324-IDUSER                                
189800                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR             
189900                 MOVE NEJ TO INDATA-SW                                    
190000              END-IF                                                      
190100              IF WS-AAAAMMDD > DAGENS-DATUM-Y2K                           
190200                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKLAR-ATTR               
190300                 MOVE NEJ TO INDATA-SW                                    
190400              END-IF                                                      
190500              PERFORM S03-KOLLA-IDDC-ART                                  
190600              PERFORM GC-KOLLA-IDUSER                                     
190700           END-IF                                                         
190800        END-IF                                                            
190900        ADD +1 TO RAD-IX                                                  
191000     END-PERFORM                                                          
191100     MOVE SPAR-IDARTNR TO W-IDARTNR                                       
191200     MOVE SPAR-IDDC    TO W-IDDC                                          
191300                          W-6327-IDDC                                     
191400     .                                                                    
191500     EJECT                                                                
191600 GB-KOLLA-SKROTBEORD SECTION.                                             
191700                                                                          
191800     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
191900     MOVE W-IDDC    TO SPAR-IDDC                                          
192000     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM-Y2K                  
192100                                                                          
192200     MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                            
192300                                 WS-DASKROT-AAMMDD                        
192400     IF WS-SEKEL = 9                                                      
192500        MOVE 19 TO WS-DASKROT-SS                                          
192600     ELSE                                                                 
192700        MOVE 20 TO WS-DASKROT-SS                                          
192800     END-IF                                                               
192900     IF WS-AAAAMMDD > DAGENS-DATUM-Y2K                                    
193000        MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR(RAD-IX)                   
193100        MOVE NEJ TO INDATA-SW                                             
193200     END-IF                                                               
193300     PERFORM S03-KOLLA-IDDC-ART                                           
193400     MOVE SPAR-IDARTNR TO W-IDARTNR                                       
193500     MOVE SPAR-IDDC    TO W-IDDC                                          
193600                          W-6327-IDDC                                     
193700     .                                                                    
193800     EJECT                                                                
193900 GC-KOLLA-IDUSER SECTION.                                                 
194000                                                                          
194100     MOVE MSGI-IDUSER      TO W-IDUSER-GODK-X                             
194200     MOVE MID-IDDC(RAD-IX)                    TO W-IDDC-6324              
194300                                                 W-6327-IDDC              
194400     PERFORM IMS-GU-WDGX6328                                              
194500     IF SEGMENT-SAKNAS                                                    
194600       MOVE NO-AUTHOR-TO-ATT                     TO MED-IDMFSINF          
194700       CALL WMEDKONV USING MED-WMEDAREA                                   
194800       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
194900       MOVE NEJ TO INDATA-SW                                              
195000     END-IF                                                               
195100     MOVE MID-IDARTNR(RAD-IX)                 TO W-IDARTNR                
195200     MOVE MID-TIDATUM(RAD-IX)                 TO WS-DASKROT-AAMMDD        
195300     MOVE MSGI-KDARBTYP                       TO W-6321-KDARBTYP          
195400     MOVE 20                          TO WS-DASKROT-SS                    
195500     COMPUTE W-DASKROT9 = 99999999 -                                      
195600                          WS-AAAAMMDD                                     
195700     PERFORM IMS-GU-WDGX6324-2                                            
195800     IF MID-CMD(RAD-IX) = 'X' OR 'A'                                      
195900     OR MID-FLKLAR = 'J'                                                  
196000       IF MSGI-IDUSER = 6324-IDUSER                                       
196100         MOVE NO-AUTHOR-TO-ATT                   TO MED-IDMFSINF          
196200         CALL WMEDKONV USING MED-WMEDAREA                                 
196300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
196400         MOVE NEJ TO INDATA-SW                                            
196500       END-IF                                                             
196600     END-IF                                                               
196700     .                                                                    
196800     EJECT                                                                
196900                                                                          
197000                                                                          
197100 H-UPPDATERA SECTION.                                                     
197200     MOVE +1 TO RAD-IX                                                    
197300     MOVE ZERO TO WS-ANTAL-X                                              
197400     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
197500       IF MID-CMD(RAD-IX) = 'X' OR 'A'                                    
197600          ADD +1 TO WS-ANTAL-X                                            
197700       END-IF                                                             
197800       ADD +1 TO RAD-IX                                                   
197900     END-PERFORM                                                          
198000                                                                          
198100     MOVE +1 TO URV-IX                                                    
198200                                                                          
198300     IF MID-FLKLAR = 'J' OR 'Y'                                           
198400        PERFORM HA-SCRAP-ALL                                              
198500     ELSE                                                                 
198600        IF WS-ANTAL-X > +1                                                
198700           PERFORM HD-SKAPA-SKROTORDER-URVAL                              
198800        ELSE                                                              
198900           MOVE +1 TO RAD-IX                                              
199000           PERFORM UNTIL RAD-IX > RAD-IX-MAX                              
199100              IF MID-CMD(RAD-IX) = 'B' OR 'D'                             
199200                 PERFORM HB-UPPDAT-BORTTAG                                
199300              ELSE                                                        
199400                 IF MID-CMD(RAD-IX) = 'X' OR 'A'                          
199500                    PERFORM HC-SKAPA-EN-SKROTORDER                        
199600                    ADD +1 TO URV-IX                                      
199700                 END-IF                                                   
199800              END-IF                                                      
199900              ADD +1 TO RAD-IX                                            
200000           END-PERFORM                                                    
200100        END-IF                                                            
200200     END-IF                                                               
200300                                                                          
200400     IF WS-HIGHLEV = JA                                                   
200500       MOVE INF-HIGHER-LEV TO MED-IDMFSINF                                
200600       CALL WMEDKONV USING MED-WMEDAREA                                   
200700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
200800     END-IF                                                               
200900     MOVE INF-UPDATE-DONE TO MED-IDMFSFEL                                 
201000     CALL WMEDKONV USING MED-WMEDAREA                                     
201100     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
201200     PERFORM MFS-FORM-ATTR                                                
201300     PERFORM MFS-RENSA-FAELT-IN                                           
201400     .                                                                    
201500     EJECT                                                                
201600 HA-SCRAP-ALL SECTION.                                                    
201700                                                                          
201800     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
201900     MOVE W-IDDC    TO SPAR-IDDC                                          
202000     MOVE NEJ       TO WS-FLURVAL                                         
202100                                                                          
202200     MOVE +1 TO RAD-IX                                                    
202300     PERFORM UNTIL (RAD-IX > RAD-IX-MAX)                                  
202400             OR (MID-IDARTNR(RAD-IX) = ZERO)                              
202500        INSPECT MID-IDARTNR(RAD-IX)                                       
202600                     REPLACING LEADING SPACE BY ZERO                      
202700        MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                             
202800        MOVE W-IDARTNR TO IDARTNR-WS                                      
202900        MOVE MID-IDDC(RAD-IX)    TO W-IDDC                                
203000                                    W-IDDC-6324                           
203100                                    W-6327-IDDC                           
203200        MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                         
203300                                    WS-DASKROT-AAMMDD                     
203400        MOVE W-KDARBTYP          TO W-6321-KDARBTYP                       
203500        IF WS-SEKEL = 9                                                   
203600           MOVE 19 TO WS-DASKROT-SS                                       
203700        ELSE                                                              
203800           MOVE 20 TO WS-DASKROT-SS                                       
203900        END-IF                                                            
204000        COMPUTE WS-DASKROT9-BEORD = 99999999 - WS-AAAAMMDD                
204100        MOVE WS-DASKROT9-BEORD TO W-DASKROT9                              
204200        MOVE ZERO            TO 6324-KVSKROT-BEORD                        
204300        INSPECT MID-SUARTSTD(RAD-IX)                                      
204400                REPLACING LEADING SPACE BY ZERO                           
204500        MOVE MID-SUARTSTD(RAD-IX) TO TEST-SUARTSTD                        
204600        IF (TEST-HELTAL <= 6328-SUBEL)                                    
204700        OR (6328-SUBEL = 9999999)                                         
204800          PERFORM IMS-GHU-WDR501-6321                                     
204900          IF SEGMENT-FINNS                                                
205000             PERFORM IMS-GHNP-WDGX6324                                    
205100             IF SEGMENT-FINNS                                             
205200                MOVE 'J' TO 6324-FLSKROT-GODK                             
205300                PERFORM IMS-REPL-WDGX6324                                 
205400                IF 6324-IDKUNDNR = 111                                    
205500                   PERFORM S14-SKAPA-CLASSIC-TRANS                        
205600                END-IF                                                    
205700             END-IF                                                       
205800          END-IF                                                          
205900          MOVE 'J' TO URV-FLKLAR                                          
206000          PERFORM S01-SKAPA-URV-TRANS                                     
206100          MOVE 'J' TO WS-FLURVAL                                          
206200          ADD +1 TO URV-IX                                                
206300          MOVE W-IDDC  TO SW-WS-IDDC                                      
206400          IF SW-CDC-SE                                                    
206500             PERFORM IMS-GHU-ARTC01                                       
206600             PERFORM IMS-GHU-ARTC11                                       
206700             IF SEGMENT-FINNS                                             
206800                COMPUTE CLAG-KVSPARR-KVAL =                               
206900                        CLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD            
207000                IF CLAG-KVSPARR-KVAL < ZERO                               
207100                   MOVE ZERO TO CLAG-KVSPARR-KVAL                         
207200                END-IF                                                    
207300                MOVE MSGI-IDUSER                                          
207400                               TO CLAG-IDUSER-SPKVAL                      
207500                MOVE DAGENS-DATUM                                         
207600                               TO CLAG-TISPARR-KVAL                       
207700                                                                          
207800                MOVE 'N' TO CLAG-FLSKROT-BEORD                            
207900                MOVE DAGENS-DATUM                                         
208000                         TO CLAG-TISKROT                                  
208100                IF 6324-IDKUNDNR = 11         AND                         
208200                   6324-IDUSER   = 'W2616800' AND                         
208300                   CLAG-KDERS    = 00         AND                         
208400                  (ART-IDLEVNR NOT = 'BQ8VA')                             
208500                   MOVE 09 TO PROGSW-MID-KDERS                            
208600                   MOVE 1  TO PROGSW-MID-DIERS-ERS                        
208700                   PERFORM S10-SKAPA-TRANS-TILL-1113                      
208800                END-IF                                                    
208900                PERFORM IMS-REPL-ARTC11                                   
209000                PERFORM S04-BOKA-NER-BUFFERT                              
209100             END-IF                                                       
209200          ELSE                                                            
209300             PERFORM IMS-GHU-ARTS11                                       
209400             IF SEGMENT-FINNS                                             
209500                COMPUTE SLAG-KVSPARR-KVAL =                               
209600                        SLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD            
209700                                                                          
209800                IF SLAG-KVSPARR-KVAL < ZERO                               
209900                   MOVE ZERO TO SLAG-KVSPARR-KVAL                         
210000                END-IF                                                    
210100                                                                          
210200                MOVE MSGI-IDUSER                                          
210300                               TO SLAG-IDUSER-SPKVAL                      
210400                MOVE DAGENS-DATUM                                         
210500                               TO SLAG-TISPARR-KVAL                       
210600                                                                          
210700                MOVE 'N' TO SLAG-FLSKROT-BEORD                            
210800                PERFORM IMS-REPL-ARTS11                                   
210900             END-IF                                                       
211000          END-IF                                                          
211100        ELSE                                                              
211200          MOVE JA TO WS-HIGHLEV                                           
211300          PERFORM IMS-GHU-WDR501-6321                                     
211400          IF SEGMENT-FINNS                                                
211500             PERFORM IMS-GHNP-WDGX6324                                    
211600             IF SEGMENT-FINNS                                             
211700                MOVE 6324-IDARTNR TO W-IDARTNR-KVAL                       
211800                MOVE 6324-IDDC    TO W-IDDC-KVAL                          
211900                MOVE 1            TO W-KDSTASKR-KVAL                      
212000                PERFORM IMS-GU-WDGX6324-6325                              
212100                PERFORM IMS-GNP-WDGX6325                                  
212200                MOVE +1 TO IX                                             
212300                IF SEGMENT-FINNS                                          
212400                  PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-IX             
212500                    MOVE 6325-IDRADNR TO IX                               
212600                    MOVE 6325-TEMEMO TO W-TEMAIL(IX)                      
212700                    ADD +1 TO IX                                          
212800                    PERFORM IMS-GNP-WDGX6325                              
212900                  END-PERFORM                                             
213000                END-IF                                                    
213100                                                                          
213200                PERFORM IMS-DLET-WDGX6324                                 
213300                MOVE 2 TO 6324-KDSTASKR                                   
213400                          W-KDSTASKR-KVAL                                 
213500                PERFORM IMS-ISRT-WDGX6324                                 
213600                MOVE +1 TO IX                                             
213700                PERFORM UNTIL IX > MAX-IX                                 
213800                  IF W-TEMAIL(IX) NOT = SPACE                             
213900                    MOVE IX              TO 6325-IDRADNR                  
214000                                          W-IDRADNR                       
214100                    MOVE W-TEMAIL(IX)    TO 6325-TEMEMO                   
214200                    PERFORM IMS-ISRT-WDGX6325                             
214300                  END-IF                                                  
214400                ADD +1 TO IX                                              
214500                END-PERFORM                                               
214600                MOVE 1 TO W-KDSTASKR-KVAL                                 
214700             END-IF                                                       
214800          END-IF                                                          
214900          PERFORM HF-HAMTA-HOGRE-NIVA                                     
215000          PERFORM S90-SKICKA-MAIL                                         
215100        END-IF                                                            
215200                                                                          
215300        IF 6324-KDSTASKR = 2                                              
215400          MOVE 2         TO W-KDSTASKR                                    
215500        ELSE                                                              
215600          CONTINUE                                                        
215700        END-IF                                                            
215800        MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                             
215900        MOVE MID-IDDC(RAD-IX)    TO W-IDDC                                
216000                                    W-IDDC-6324                           
216100                                    W-6327-IDDC                           
216200        MOVE WS-DASKROT9-BEORD TO W-DASKROT9                              
216300                                  WS-6322-DASKROT9                        
216400        INSPECT MID-IDARTNR(RAD-IX)                                       
216500                     REPLACING LEADING SPACE BY ZERO                      
216600        IF MID-IDARTNR(RAD-IX) NUMERIC                                    
216700           IF MID-IDARTNR(RAD-IX) > ZERO                                  
216800              PERFORM HE-UPDATERA-6326                                    
216900           END-IF                                                         
217000        END-IF                                                            
217100        IF 6324-KDSTASKR = 1                                              
217200          PERFORM S15-SKAPA-WDGX2402                                      
217300        END-IF                                                            
217400*****ÅTERSTÄLL NYCKEL                                                     
217500        IF W-KDSTASKR = 2                                                 
217600          MOVE 1         TO W-KDSTASKR                                    
217700        END-IF                                                            
217800        ADD +1 TO RAD-IX                                                  
217900     END-PERFORM                                                          
218000                                                                          
218100     IF WS-FLURVAL = JA                                                   
218200       PERFORM S02-STARTA-URV-TRANS                                       
218300     END-IF                                                               
218400                                                                          
218500**************** ÅTERSTÄLL NYCKLAR                                        
218600     MOVE SPAR-IDARTNR        TO W-IDARTNR                                
218700     MOVE SPAR-IDDC           TO W-IDDC                                   
218800                                 W-IDDC-6324                              
218900                                 W-6327-IDDC                              
219000     .                                                                    
219100     EJECT                                                                
219200 HB-UPPDAT-BORTTAG SECTION.                                               
219300******************************************************************        
219400* FLSKROT-BEORD SÄTTS TILL NEJ PÅ ARTC ELLER ARTS                         
219500* RADEN TAS BORT FRÅN WL632121                                            
219600******************************************************************        
219700                                                                          
219800     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
219900     MOVE W-IDDC    TO SPAR-IDDC                                          
220000                                                                          
220100     INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING SPACE BY ZERO          
220200     MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                                
220300     MOVE MID-IDDC(RAD-IX)    TO W-IDDC                                   
220400                                 W-IDDC-6324                              
220500                                 W-6327-IDDC                              
220600     MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                            
220700                                 WS-DASKROT-AAMMDD                        
220800     MOVE W-KDARBTYP          TO W-6321-KDARBTYP                          
220900     IF WS-SEKEL = 9                                                      
221000        MOVE 19 TO WS-DASKROT-SS                                          
221100     ELSE                                                                 
221200        MOVE 20 TO WS-DASKROT-SS                                          
221300     END-IF                                                               
221400     COMPUTE WS-DASKROT9-BEORD = 99999999 - WS-AAAAMMDD                   
221500     MOVE WS-DASKROT9-BEORD TO W-DASKROT9                                 
221600     PERFORM IMS-GHU-WDR501-6321                                          
221700     IF SEGMENT-FINNS                                                     
221800        PERFORM IMS-GHNP-WDGX6324                                         
221900        IF SEGMENT-FINNS                                                  
222000           MOVE 6324-IDUSER TO W-ANNUL-IDUSER                             
222100                               W-IDUSER-GODK-X                            
222200           PERFORM IMS-GU-WDGX6328                                        
222300           IF SEGMENT-FINNS                                               
222400              MOVE 6328-IDMAIL TO W-ANNUL-IDMAIL                          
222500           END-IF                                                         
222600           IF W-ANNUL-IDMAIL NOT = SPACE                                  
222700           AND W-ANNUL-IDUSER NOT = MSGI-IDUSER                           
222800             PERFORM S91-MAIL-ANNUL                                       
222900           END-IF                                                         
223000           PERFORM IMS-DLET-WDGX6324                                      
223100        END-IF                                                            
223200     END-IF                                                               
223300                                                                          
223400     PERFORM IMS-GHU-WDGX6322                                             
223500     IF SEGMENT-FINNS                                                     
223600        PERFORM IMS-GNP-WDGX6324                                          
223700        IF SEGMENT-SAKNAS                                                 
223800           PERFORM IMS-GHU-WDGX6322                                       
223900           PERFORM IMS-DLET-WDGX6322                                      
224000        END-IF                                                            
224100     END-IF                                                               
224200                                                                          
224300     MOVE W-IDDC    TO SW-WS-IDDC                                         
224400     IF SW-CDC-SE                                                         
224500        PERFORM IMS-GHU-ARTC11                                            
224600        IF SEGMENT-FINNS                                                  
224700           MOVE 'N' TO CLAG-FLSKROT-BEORD                                 
224800           PERFORM IMS-REPL-ARTC11                                        
224900        END-IF                                                            
225000     ELSE                                                                 
225100        PERFORM IMS-GHU-ARTS11                                            
225200        IF SEGMENT-FINNS                                                  
225300           MOVE 'N' TO SLAG-FLSKROT-BEORD                                 
225400           PERFORM IMS-REPL-ARTS11                                        
225500        END-IF                                                            
225600     END-IF                                                               
225700                                                                          
225800**************** ÅTERSTÄLL NYCKLAR                                        
225900     MOVE SPAR-IDARTNR        TO W-IDARTNR                                
226000     MOVE SPAR-IDDC           TO W-IDDC                                   
226100                                 W-IDDC-6324                              
226200                                 W-6327-IDDC                              
226300     .                                                                    
226400     EJECT                                                                
226500 HC-SKAPA-EN-SKROTORDER SECTION.                                          
226600                                                                          
226700     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
226800     MOVE W-IDDC    TO SPAR-IDDC                                          
226900                                                                          
227000     INSPECT MID-IDARTNR(RAD-IX)                                          
227100                       REPLACING LEADING SPACE BY ZERO.                   
227200     MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                                
227300     MOVE W-IDARTNR           TO IDARTNR-WS                               
227400     MOVE MID-IDDC(RAD-IX)    TO W-IDDC                                   
227500                                 W-IDDC-6324                              
227600                                 W-6327-IDDC                              
227700                                 WS-IDDC                                  
227800     MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                            
227900                                 WS-DASKROT-AAMMDD                        
228000     MOVE W-KDARBTYP          TO W-6321-KDARBTYP                          
228100     IF WS-SEKEL = 9                                                      
228200        MOVE 19 TO WS-DASKROT-SS                                          
228300     ELSE                                                                 
228400        MOVE 20 TO WS-DASKROT-SS                                          
228500     END-IF                                                               
228600     COMPUTE WS-DASKROT9-BEORD = 99999999 - WS-AAAAMMDD                   
228700     MOVE WS-DASKROT9-BEORD TO W-DASKROT9                                 
228800                               WS-6322-DASKROT9                           
228900     MOVE ZERO               TO 6324-KVSKROT-BEORD                        
229000                                                                          
229100     INSPECT MID-SUARTSTD(RAD-IX) REPLACING LEADING SPACE                 
229200                    BY ZERO                                               
229300     MOVE MID-SUARTSTD(RAD-IX) TO TEST-SUARTSTD                           
229400     IF (TEST-HELTAL <= 6328-SUBEL)                                       
229500     OR (6328-SUBEL = 9999999)                                            
229600       PERFORM IMS-GHU-WDR501-6321                                        
229700       IF SEGMENT-FINNS                                                   
229800          PERFORM IMS-GHNP-WDGX6324                                       
229900          IF SEGMENT-FINNS                                                
230000             MOVE 'J' TO 6324-FLSKROT-GODK                                
230100             PERFORM IMS-REPL-WDGX6324                                    
230200             IF 6324-IDKUNDNR = 111                                       
230300                PERFORM S14-SKAPA-CLASSIC-TRANS                           
230400             END-IF                                                       
230500          END-IF                                                          
230600       END-IF                                                             
230700       PERFORM HE-UPDATERA-6326                                           
230800       MOVE 'N' TO URV-FLKLAR                                             
230900       PERFORM S01-SKAPA-URV-TRANS                                        
231000       PERFORM S15-SKAPA-WDGX2402                                         
231100       PERFORM S02-STARTA-URV-TRANS                                       
231200                                                                          
231300**************                                                            
231400       MOVE W-IDDC  TO SW-WS-IDDC                                         
231500       IF SW-CDC-SE                                                       
231600          PERFORM IMS-GHU-ARTC01                                          
231700          PERFORM IMS-GHU-ARTC11                                          
231800          IF SEGMENT-FINNS                                                
231900             COMPUTE CLAG-KVSPARR-KVAL =                                  
232000                     CLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD               
232100             IF CLAG-KVSPARR-KVAL < ZERO                                  
232200                MOVE ZERO    TO CLAG-KVSPARR-KVAL                         
232300             END-IF                                                       
232400             MOVE MSGI-IDUSER TO CLAG-IDUSER-SPKVAL                       
232500             MOVE DAGENS-DATUM TO CLAG-TISPARR-KVAL                       
232600                                                                          
232700             MOVE 'N' TO CLAG-FLSKROT-BEORD                               
232800             MOVE DAGENS-DATUM                                            
232900                         TO CLAG-TISKROT                                  
233000             IF 6324-IDKUNDNR = 11         AND                            
233100                6324-IDUSER   = 'W2616800' AND                            
233200                CLAG-KDERS    = 00         AND                            
233300               (ART-IDLEVNR NOT = 'BQ8VA')                                
233400                MOVE 09 TO PROGSW-MID-KDERS                               
233500                MOVE 1  TO PROGSW-MID-DIERS-ERS                           
233600                PERFORM S10-SKAPA-TRANS-TILL-1113                         
233700             END-IF                                                       
233800             PERFORM IMS-REPL-ARTC11                                      
233900             PERFORM S04-BOKA-NER-BUFFERT                                 
234000          END-IF                                                          
234100       ELSE                                                               
234200          PERFORM IMS-GHU-ARTS11                                          
234300          IF SEGMENT-FINNS                                                
234400             COMPUTE SLAG-KVSPARR-KVAL =                                  
234500                     SLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD               
234600                                                                          
234700             IF SLAG-KVSPARR-KVAL < ZERO                                  
234800                MOVE ZERO    TO SLAG-KVSPARR-KVAL                         
234900             END-IF                                                       
235000                                                                          
235100             MOVE MSGI-IDUSER TO SLAG-IDUSER-SPKVAL                       
235200             MOVE DAGENS-DATUM TO SLAG-TISPARR-KVAL                       
235300                                                                          
235400             MOVE 'N' TO SLAG-FLSKROT-BEORD                               
235500             PERFORM IMS-REPL-ARTS11                                      
235600          END-IF                                                          
235700       END-IF                                                             
235800     ELSE                                                                 
235900       MOVE JA TO WS-HIGHLEV                                              
236000       PERFORM IMS-GHU-WDR501-6321                                        
236100       IF SEGMENT-FINNS                                                   
236200          PERFORM IMS-GHNP-WDGX6324                                       
236300          IF SEGMENT-FINNS                                                
236400             MOVE 6324-IDARTNR TO W-IDARTNR-KVAL                          
236500             MOVE 6324-IDDC       TO W-IDDC-KVAL                          
236600             MOVE 1               TO W-KDSTASKR-KVAL                      
236700             PERFORM IMS-GU-WDGX6324-6325                                 
236800             PERFORM IMS-GNP-WDGX6325                                     
236900             MOVE +1 TO IX                                                
237000             IF SEGMENT-FINNS                                             
237100               PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-IX                
237200                 MOVE 6325-IDRADNR TO IX                                  
237300                 MOVE 6325-TEMEMO TO W-TEMAIL(IX)                         
237400                 ADD +1 TO IX                                             
237500                 PERFORM IMS-GNP-WDGX6325                                 
237600               END-PERFORM                                                
237700             END-IF                                                       
237800             PERFORM IMS-DLET-WDGX6324                                    
237900             MOVE 2 TO 6324-KDSTASKR                                      
238000                       W-KDSTASKR-KVAL                                    
238100             PERFORM IMS-ISRT-WDGX6324                                    
238200             MOVE +1 TO IX                                                
238300             PERFORM UNTIL IX > MAX-IX                                    
238400               IF W-TEMAIL(IX) NOT = SPACE                                
238500                 MOVE IX                 TO 6325-IDRADNR                  
238600                                          W-IDRADNR                       
238700                 MOVE W-TEMAIL(IX)       TO 6325-TEMEMO                   
238800                 PERFORM IMS-ISRT-WDGX6325                                
238900               END-IF                                                     
239000             ADD +1 TO IX                                                 
239100             END-PERFORM                                                  
239200             MOVE 1 TO W-KDSTASKR-KVAL                                    
239300          END-IF                                                          
239400       END-IF                                                             
239500       PERFORM HF-HAMTA-HOGRE-NIVA                                        
239600       PERFORM S90-SKICKA-MAIL                                            
239700       IF 6324-KDSTASKR = 2                                               
239800         MOVE 2          TO W-KDSTASKR                                    
239900       ELSE                                                               
240000         CONTINUE                                                         
240100       END-IF                                                             
240200       PERFORM HE-UPDATERA-6326                                           
240300       IF W-KDSTASKR = 2                                                  
240400         MOVE 1          TO W-KDSTASKR                                    
240500       END-IF                                                             
240600     END-IF                                                               
240700**************** ÅTERSTÄLL NYCKLAR                                        
240800     MOVE SPAR-IDARTNR        TO W-IDARTNR                                
240900     MOVE SPAR-IDDC           TO W-IDDC                                   
241000                                 W-IDDC-6324                              
241100                                 W-6327-IDDC                              
241200     .                                                                    
241300     EJECT                                                                
241400 HD-SKAPA-SKROTORDER-URVAL SECTION.                                       
241500                                                                          
241600     MOVE W-IDARTNR TO SPAR-IDARTNR                                       
241700     MOVE W-IDDC    TO SPAR-IDDC                                          
241800     MOVE NEJ       TO WS-FLURVAL                                         
241900                                                                          
242000     MOVE +1 TO RAD-IX                                                    
242100     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
242200        IF MID-CMD(RAD-IX) = 'X' OR 'A'                                   
242300           INSPECT MID-IDARTNR(RAD-IX)                                    
242400                        REPLACING LEADING SPACE BY ZERO                   
242500           MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                          
242600           MOVE W-IDARTNR           TO IDARTNR-WS                         
242700           MOVE MID-IDDC(RAD-IX)    TO W-IDDC                             
242800                                             W-IDDC-6324                  
242900                                             W-6327-IDDC                  
243000           MOVE MID-TIDATUM(RAD-IX) TO WS-SEKEL-KOLL                      
243100                                             WS-DASKROT-AAMMDD            
243200           MOVE W-KDARBTYP          TO W-6321-KDARBTYP                    
243300           IF WS-SEKEL = 9                                                
243400              MOVE 19 TO WS-DASKROT-SS                                    
243500           ELSE                                                           
243600              MOVE 20 TO WS-DASKROT-SS                                    
243700           END-IF                                                         
243800           COMPUTE WS-DASKROT9-BEORD = 99999999 - WS-AAAAMMDD             
243900           MOVE WS-DASKROT9-BEORD   TO W-DASKROT9                         
244000                                       WS-6322-DASKROT9                   
244100           MOVE ZERO                TO 6324-KVSKROT-BEORD                 
244200           INSPECT MID-SUARTSTD(RAD-IX)                                   
244300                   REPLACING LEADING SPACE BY ZERO                        
244400           MOVE MID-SUARTSTD(RAD-IX) TO TEST-SUARTSTD                     
244500           IF (TEST-HELTAL <= 6328-SUBEL)                                 
244600           OR (6328-SUBEL = 9999999)                                      
244700             PERFORM IMS-GHU-WDR501-6321                                  
244800             IF SEGMENT-FINNS                                             
244900                PERFORM IMS-GHNP-WDGX6324                                 
245000                IF SEGMENT-FINNS                                          
245100                   MOVE 'J' TO 6324-FLSKROT-GODK                          
245200                   PERFORM IMS-REPL-WDGX6324                              
245300                   IF 6324-IDKUNDNR = 111                                 
245400                     PERFORM S14-SKAPA-CLASSIC-TRANS                      
245500                   END-IF                                                 
245600                END-IF                                                    
245700             END-IF                                                       
245800                                                                          
245900             MOVE W-IDDC  TO SW-WS-IDDC                                   
246000             IF SW-CDC-SE                                                 
246100                PERFORM IMS-GHU-ARTC01                                    
246200                PERFORM IMS-GHU-ARTC11                                    
246300                IF SEGMENT-FINNS                                          
246400                   COMPUTE CLAG-KVSPARR-KVAL =                            
246500                           CLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD         
246600                   IF CLAG-KVSPARR-KVAL < ZERO                            
246700                      MOVE ZERO TO CLAG-KVSPARR-KVAL                      
246800                   END-IF                                                 
246900                   MOVE MSGI-IDUSER                                       
247000                               TO CLAG-IDUSER-SPKVAL                      
247100                   MOVE DAGENS-DATUM                                      
247200                               TO CLAG-TISPARR-KVAL                       
247300                                                                          
247400                   MOVE 'N' TO CLAG-FLSKROT-BEORD                         
247500                   MOVE DAGENS-DATUM                                      
247600                         TO CLAG-TISKROT                                  
247700                   IF 6324-IDKUNDNR = 11         AND                      
247800                      6324-IDUSER   = 'W2616800' AND                      
247900                      CLAG-KDERS    = 00         AND                      
248000                     (ART-IDLEVNR NOT = 'BQ8VA')                          
248100                      MOVE 09 TO PROGSW-MID-KDERS                         
248200                      MOVE 1  TO PROGSW-MID-DIERS-ERS                     
248300                      PERFORM S10-SKAPA-TRANS-TILL-1113                   
248400                   END-IF                                                 
248500                   PERFORM IMS-REPL-ARTC11                                
248600                   PERFORM S04-BOKA-NER-BUFFERT                           
248700                END-IF                                                    
248800             ELSE                                                         
248900                PERFORM IMS-GHU-ARTS11                                    
249000                IF SEGMENT-FINNS                                          
249100                   COMPUTE SLAG-KVSPARR-KVAL =                            
249200                           SLAG-KVSPARR-KVAL - 6324-KVSKROT-BEORD         
249300                                                                          
249400                   IF SLAG-KVSPARR-KVAL < ZERO                            
249500                      MOVE ZERO                                           
249600                               TO SLAG-KVSPARR-KVAL                       
249700                   END-IF                                                 
249800                                                                          
249900                   MOVE MSGI-IDUSER                                       
250000                               TO SLAG-IDUSER-SPKVAL                      
250100                   MOVE DAGENS-DATUM                                      
250200                               TO SLAG-TISPARR-KVAL                       
250300                                                                          
250400                   MOVE 'N' TO SLAG-FLSKROT-BEORD                         
250500                   PERFORM IMS-REPL-ARTS11                                
250600                END-IF                                                    
250700             END-IF                                                       
250800**************                                                            
250900                                                                          
251000             MOVE 'J' TO URV-FLKLAR                                       
251100             PERFORM S01-SKAPA-URV-TRANS                                  
251200             ADD +1 TO URV-IX                                             
251300             MOVE 'J' TO WS-FLURVAL                                       
251400           ELSE                                                           
251500              MOVE JA TO WS-HIGHLEV                                       
251600              PERFORM IMS-GHU-WDR501-6321                                 
251700              IF SEGMENT-FINNS                                            
251800                 PERFORM IMS-GHNP-WDGX6324                                
251900                 IF SEGMENT-FINNS                                         
252000                    MOVE 6324-IDARTNR TO W-IDARTNR-KVAL                   
252100                    MOVE 6324-IDDC TO W-IDDC-KVAL                         
252200                    MOVE 1        TO W-KDSTASKR-KVAL                      
252300                    PERFORM IMS-GU-WDGX6324-6325                          
252400                    PERFORM IMS-GNP-WDGX6325                              
252500                    MOVE +1 TO IX                                         
252600                    IF SEGMENT-FINNS                                      
252700                      PERFORM UNTIL SEGMENT-SAKNAS OR IX > MAX-IX         
252800                        MOVE 6325-IDRADNR TO IX                           
252900                        MOVE 6325-TEMEMO TO W-TEMAIL(IX)                  
253000                        ADD +1 TO IX                                      
253100                        PERFORM IMS-GNP-WDGX6325                          
253200                      END-PERFORM                                         
253300                    END-IF                                                
253400                    PERFORM IMS-DLET-WDGX6324                             
253500                    MOVE 2 TO 6324-KDSTASKR                               
253600                              W-KDSTASKR-KVAL                             
253700                    PERFORM IMS-ISRT-WDGX6324                             
253800                    MOVE +1 TO IX                                         
253900                    PERFORM UNTIL IX > MAX-IX                             
254000                      IF W-TEMAIL(IX) NOT = SPACE                         
254100                        MOVE IX          TO 6325-IDRADNR                  
254200                                          W-IDRADNR                       
254300                        MOVE W-TEMAIL(IX) TO 6325-TEMEMO                  
254400                        PERFORM IMS-ISRT-WDGX6325                         
254500                      END-IF                                              
254600                    ADD +1 TO IX                                          
254700                    END-PERFORM                                           
254800                    MOVE 1 TO W-KDSTASKR-KVAL                             
254900                 END-IF                                                   
255000              END-IF                                                      
255100              PERFORM HF-HAMTA-HOGRE-NIVA                                 
255200              PERFORM S90-SKICKA-MAIL                                     
255300           END-IF                                                         
255400          IF 6324-KDSTASKR = 2                                            
255500            MOVE 2       TO W-KDSTASKR                                    
255600          ELSE                                                            
255700            CONTINUE                                                      
255800          END-IF                                                          
255900          PERFORM HE-UPDATERA-6326                                        
256000          IF 6324-KDSTASKR = 1                                            
256100            PERFORM S15-SKAPA-WDGX2402                                    
256200          END-IF                                                          
256300*****ÅTERSTÄLL NYCKEL                                                     
256400          IF W-KDSTASKR = 2                                               
256500            MOVE 1       TO W-KDSTASKR                                    
256600          END-IF                                                          
256700        END-IF                                                            
256800        ADD +1 TO RAD-IX                                                  
256900     END-PERFORM                                                          
257000                                                                          
257100     IF WS-FLURVAL = JA                                                   
257200       PERFORM S02-STARTA-URV-TRANS                                       
257300     END-IF                                                               
257400                                                                          
257500******************* ÅTERSTÄLL NYCKLAR                                     
257600           MOVE SPAR-IDARTNR        TO W-IDARTNR                          
257700           MOVE SPAR-IDDC           TO W-IDDC                             
257800                                       W-IDDC-6324                        
257900                                       W-6327-IDDC                        
258000     .                                                                    
258100     EJECT                                                                
258200                                                                          
258300 HE-UPDATERA-6326 SECTION.                                                
258400                                                                          
258500     ACCEPT WS-TID FROM TIME                                              
258600     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATUM                          
258700     MOVE WS-TID (1:6)               TO WS-TIDHHMMSS                      
258800     MOVE WS-TIDATETIME              TO 6326-TIDATETIME                   
258900     MOVE MSGI-IDUSER                TO 6326-IDUSER-GODK                  
259000     MOVE MSGI-IDUSER                TO W-IDUSER-GODK-X                   
259100     PERFORM IMS-GU-WDGX6328                                              
259200     MOVE 6328-BEANST-GODK           TO 6326-BEANST-GODK                  
259300     PERFORM IMS-ISRT-WDGX6326                                            
259400     .                                                                    
259500     EJECT                                                                
259600 HF-HAMTA-HOGRE-NIVA SECTION.                                             
259700                                                                          
259800     MOVE NEJ TO WS-FLHOGRE                                               
259900     IF 6328-IDUSER-PRI = SPACE                                           
260000        MOVE 6328-SUBEL              TO WS-SUBEL                          
260100        PERFORM IMS-GU-WDGX6327                                           
260200        PERFORM IMS-GNP-WDGX6328                                          
260300        PERFORM UNTIL WS-FLHOGRE = JA                                     
260400                   OR SEGMENT-SAKNAS                                      
260500           IF 6328-SUBEL > WS-SUBEL                                       
260600              MOVE JA                TO WS-FLHOGRE                        
260700           ELSE                                                           
260800              PERFORM IMS-GNP-WDGX6328                                    
260900           END-IF                                                         
261000        END-PERFORM                                                       
261100     ELSE                                                                 
261200        MOVE 6328-IDUSER-PRI         TO W-IDUSER-GODK                     
261300        MOVE 6328-SUBEL              TO WS-SUBEL                          
261400        PERFORM IMS-GU-WDGX6328                                           
261500        IF SEGMENT-FINNS                                                  
261600           CONTINUE                                                       
261700        ELSE                                                              
261800           PERFORM IMS-GNP-WDGX6328                                       
261900           PERFORM UNTIL WS-FLHOGRE = JA                                  
262000                   OR SEGMENT-SAKNAS                                      
262100              IF 6328-SUBEL > WS-SUBEL                                    
262200                 MOVE JA             TO WS-FLHOGRE                        
262300              ELSE                                                        
262400                 PERFORM IMS-GNP-WDGX6328                                 
262500              END-IF                                                      
262600           END-PERFORM                                                    
262700        END-IF                                                            
262800     END-IF                                                               
262900     .                                                                    
263000     EJECT                                                                
263100 S01-SKAPA-URV-TRANS SECTION.                                             
263200                                                                          
263300     MOVE W-KDARBTYP           TO URV-KDARBTYP                            
263400     INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING SPACE BY ZERO          
263500                                                                          
263600     MOVE MID-IDARTNR(RAD-IX)  TO URV-IDARTNR(URV-IX)                     
263700     MOVE MID-IDDC(RAD-IX)     TO URV-IDDC(URV-IX)                        
263800                                                                          
263900     MOVE MID-TIDATUM(RAD-IX)  TO WS-SEKEL-KOLL                           
264000                                  WS-DASKROT-AAMMDD                       
264100     IF WS-SEKEL = 9                                                      
264200        MOVE 19 TO WS-DASKROT-SS                                          
264300     ELSE                                                                 
264400        MOVE 20 TO WS-DASKROT-SS                                          
264500     END-IF                                                               
264600     COMPUTE WS-DASKROT9-BEORD = 999999999 - WS-AAAAMMDD                  
264700     MOVE WS-DASKROT9-BEORD    TO URV-DASKROT9-BEORD(URV-IX)              
264800     .                                                                    
264900     EJECT                                                                
265000 S02-STARTA-URV-TRANS SECTION.                                            
265100                                                                          
265200     MOVE '6322'   TO MSGSOP-IDTRANS                                      
265300     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
265400     MOVE 'W216S1' TO MSGSOP-IDPROCESS                                    
265500     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
265600                                                                          
265700     STRING 'URVAL1(' WS-URVAL ') '                                       
265800            'URVAL2(' URV-TAB-RAD (1) ') '                                
265900            'URVAL3(' URV-TAB-RAD (2) ') '                                
266000            'URVAL4(' URV-TAB-RAD (3) ') '                                
266100            'URVAL5(' URV-TAB-RAD (4) ') '                                
266200            'URVAL6(' URV-TAB-RAD (5) ') '                                
266300            'URVAL7(' URV-TAB-RAD (6) ') '                                
266400            'URVAL8(' URV-TAB-RAD (7) ') '                                
266500            'URVAL9(' URV-TAB-RAD (8) ') '                                
266600            'URVAL10(' URV-TAB-RAD (9) ') '                               
266700            'URVAL11(' URV-TAB-RAD (10) ') '                              
266800            'URVAL12(' URV-TAB-RAD (11) ') '                              
266900            'URVAL13(' URV-TAB-RAD (12) ')'                               
267000              DELIMITED BY SIZE INTO MSGSOP-TESYMBV                       
267100                                                                          
267200     PERFORM IMS-INSERT-ALTMSG                                            
267300     .                                                                    
267400     EJECT                                                                
267500 S03-KOLLA-IDDC-ART SECTION.                                              
267600                                                                          
267700     IF W-KDARBTYP = 'ESC'                                                
267800        MOVE MID-IDDC(RAD-IX)   TO SW-WS-IDDC                             
267900        IF SW-NDC OR SW-LDC-CN                                            
268000           IF MID-IDDC(RAD-IX) = MSGI-IDDC                                
268100              CONTINUE                                                    
268200           ELSE                                                           
268300              MOVE MSGI-IDDC     TO SW-WS-IDDC                            
268400              IF SW-CDC-SE                                                
268500                 CONTINUE                                                 
268600              ELSE                                                        
268700                IF MID-CMD(RAD-IX) = 'X' OR 'A'                           
268800                   MOVE MFS-ALFA-FAELT-FEL                                
268900                                   TO MOD-CMD-ATTR(RAD-IX)                
269000                ELSE                                                      
269100                   MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKLAR-ATTR             
269200                END-IF                                                    
269300                MOVE NEJ TO INDATA-SW                                     
269400             END-IF                                                       
269500           END-IF                                                         
269600           IF INDATA-OK                                                   
269700**--  ENLIGT PATRIK SKALL KINA KOLLA LEV.NR ISTÄLLET FÖR PRODSL.          
269800             MOVE MSGI-IDDC   TO SW-WS-IDDC                               
269900             IF SW-CDC-SE                                                 
270000               CONTINUE                                                   
270100             ELSE                                                         
270200               IF SW-NDC-CN OR SW-LDC-CN                                  
270300                 INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING            
270400                             SPACE BY ZERO                                
270500                 MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                    
270600                 MOVE MID-IDDC(RAD-IX)    TO W-IDDC                       
270700                 PERFORM IMS-GHU-ARTS11                                   
270800                 IF SEGMENT-FINNS                                         
270900                    IF SLAG-IDLEVNR = '1441 '                             
271000                       IF MID-CMD(RAD-IX) = 'X' OR 'A'                    
271100                          MOVE MFS-ALFA-FAELT-FEL                         
271200                                    TO MOD-CMD-ATTR(RAD-IX)               
271300                       ELSE                                               
271400                        MOVE MFS-ALFA-FAELT-FEL                           
271500                                    TO MOD-FLKLAR-ATTR                    
271600                       END-IF                                             
271700                       MOVE NEJ TO INDATA-SW                              
271800                    END-IF                                                
271900                 END-IF                                                   
272000               ELSE                                                       
272100                 INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING            
272200                             SPACE BY ZERO                                
272300                 MOVE MID-IDARTNR(RAD-IX) TO W-IDARTNR                    
272400                 PERFORM IMS-GHU-ARTC01                                   
272500                 IF SEGMENT-FINNS                                         
272600                    MOVE ART-KDPRODSL TO TEST-KDPRODSL                    
272700                    IF KDPRODSL-LOCAL                                     
272800                       CONTINUE                                           
272900                    ELSE                                                  
273000                       IF MID-CMD(RAD-IX) = 'X' OR 'A'                    
273100                          MOVE MFS-ALFA-FAELT-FEL                         
273200                                    TO MOD-CMD-ATTR(RAD-IX)               
273300                       ELSE                                               
273400                        MOVE MFS-ALFA-FAELT-FEL                           
273500                                    TO MOD-FLKLAR-ATTR                    
273600                       END-IF                                             
273700                       MOVE NEJ TO INDATA-SW                              
273800                    END-IF                                                
273900                 END-IF                                                   
274000               END-IF                                                     
274100             END-IF                                                       
274200           END-IF                                                         
274300        END-IF                                                            
274400     END-IF                                                               
274500     .                                                                    
274600     EJECT                                                                
274700 S04-BOKA-NER-BUFFERT SECTION.                                            
274800*      --- BOKAR NER WDD8 VID SKROTNING                                   
274900     MOVE 6324-KVSKROT-BEORD TO W-KVSKROT-REST                            
275000     PERFORM IMS-GU-WDD801                                                
275100     IF SEGMENT-FINNS                                                     
275200       PERFORM IMS-GHNP-WDD811                                            
275300       PERFORM UNTIL SEGMENT-SAKNAS                                       
275400                  OR W-KVSKROT-REST <= +0                                 
275500         MOVE SALDO-ADBUFFPL TO W-ADBUFFPL                                
275600         IF (6324-FLJUSTBUFF = 'J' OR 'Y')                                
275700            AND SALDO-ADBUFFOMR = 59                                      
275800            AND W-ADBUFFPL (1:2) = 97                                     
275900           IF W-KVSKROT-REST > SALDO-KVBUFF-F                             
276000             COMPUTE W-KVSKROT-REST = W-KVSKROT-REST -                    
276100                                      SALDO-KVBUFF-F                      
276200                     END-COMPUTE                                          
276300             MOVE ZERO TO SALDO-KVBUFF-F                                  
276400           ELSE                                                           
276500             COMPUTE SALDO-KVBUFF-F = SALDO-KVBUFF-F -                    
276600                                      W-KVSKROT-REST                      
276700                     END-COMPUTE                                          
276800             MOVE ZERO TO W-KVSKROT-REST                                  
276900           END-IF                                                         
277000           IF SALDO-KVBUFF-F <= ZERO                                      
277100             PERFORM IMS-DLET-WDD811                                      
277200             PERFORM IMS-GU-WDJ901                                        
277300             PERFORM UNTIL SEGMENT-SAKNAS OR (HIST-KDLOC = 'B'            
277400                              AND HIST-IDDC = 11                          
277500                              AND SALDO-ADBUFFOMR  = HIST-ADLAGOMR        
277600                              AND SALDO-ADBUFFGANG = HIST-ADGANG          
277700                              AND SALDO-ADBUFFPL   = HIST-ADPLATS)        
277800               PERFORM IMS-GHNP-WDJ911                                    
277900               IF SEGMENT-FINNS AND HIST-KDLOC = 'B'                      
278000                              AND HIST-IDDC = 11                          
278100                              AND SALDO-ADBUFFOMR  = HIST-ADLAGOMR        
278200                              AND SALDO-ADBUFFGANG = HIST-ADGANG          
278300                              AND SALDO-ADBUFFPL   = HIST-ADPLATS         
278400                 MOVE FUNCTION CURRENT-DATE(1:8) TO                       
278500                                       HIST-DASTODAT                      
278600                 MOVE MSGI-IDUSER TO HIST-IDUSER-STO                      
278700                 PERFORM IMS-REPL-WDJ911                                  
278800               END-IF                                                     
278900             END-PERFORM                                                  
279000           ELSE                                                           
279100             PERFORM IMS-REPL-WDD811                                      
279200           END-IF                                                         
279300         END-IF                                                           
279400         PERFORM IMS-GHNP-WDD811                                          
279500       END-PERFORM                                                        
279600     END-IF                                                               
279700                                                                          
279800                                                                          
279900     .                                                                    
280000     EJECT                                                                
280100 S10-SKAPA-TRANS-TILL-1113 SECTION.                                       
280200                                                                          
280300     MOVE IDARTNR-WS     TO PROGSW-MID-IDARTNR-UT                         
280400*    MOVE 1              TO PROGSW-MID-DIERS-ERS                          
280500*    MOVE 09             TO PROGSW-MID-KDERS                              
280600     MOVE JA             TO PROGSW-MID-FLKLAR                             
280700                                                                          
280800     MOVE ALL '+'        TO PROGSW-MID-IDARTNR-IN                         
280900                            PROGSW-MID-IDAO                               
281000                            PROGSW-MID-TIERSDAT-PREL                      
281100                            PROGSW-MID-TEARTNOT                           
281200                                                                          
281300     MOVE  1                TO PROGSW-IX                                  
281400     PERFORM UNTIL PROGSW-IX > 09                                         
281500        MOVE ALL '+'        TO PROGSW-MID-IDKORTNR                        
281600                                            (PROGSW-IX)                   
281700                               PROGSW-MID-IDARTNR-TILLK                   
281800                                            (PROGSW-IX)                   
281900                               PROGSW-MID-DIERS-TILLK                     
282000                                            (PROGSW-IX)                   
282100                               PROGSW-MID-BEERS                           
282200                                            (PROGSW-IX)                   
282300        ADD 1               TO PROGSW-IX                                  
282400     END-PERFORM                                                          
282500                                                                          
282600     PERFORM IMS-ISRT-ALT-PCB                                             
282700     .                                                                    
282800     EJECT                                                                
282900 S11-LAES-GRUNDDATA SECTION.                                              
283000                                                                          
283100     MOVE W-KDARBTYP TO W-6321-KDARBTYP                                   
283200     MOVE NEJ TO LAES-SW                                                  
283300     IF WS-IDDC = ZERO                                                    
283400        PERFORM UNTIL TAB-IX > TAB-IX-MAX OR (LAES-SW = JA)               
283500                      OR TAB-IDDC(TAB-IX) = SPACE                         
283600           MOVE TAB-IDDC(TAB-IX) TO W-IDDC                                
283700           MOVE W-IDDC           TO W-IDDC-6324                           
283800                                    W-6327-IDDC                           
283900           PERFORM IMS-GHU-WDR501-6321                                    
284000           IF SEGMENT-FINNS                                               
284100              MOVE JA TO LAES-SW                                          
284200           ELSE                                                           
284300              SET TAB-IX UP BY +1                                         
284400           END-IF                                                         
284500        END-PERFORM                                                       
284600     ELSE                                                                 
284700        MOVE W-IDDC TO W-IDDC-6324                                        
284800                       W-6327-IDDC                                        
284900        PERFORM IMS-GHU-WDR501-6321                                       
285000     END-IF                                                               
285100     .                                                                    
285200     EJECT                                                                
285300 S12-LAES-SKROTDATUM SECTION.                                             
285400                                                                          
285500     EVALUATE TRUE                                                        
285600     WHEN KDARBTYP-PERSON-SOEKNING = JA                                   
285700        PERFORM IMS-GNP-WDGX6322                                          
285800        IF SEGMENT-FINNS                                                  
285900           MOVE 6322-DASKROT9-BEORD TO W-DASKROT9                         
286000        END-IF                                                            
286100     WHEN IDARTNR-SOEKNING = JA                                           
286200        PERFORM IMS-GNP-WDGX6322                                          
286300        IF SEGMENT-FINNS                                                  
286400           MOVE 6322-DASKROT9-BEORD TO W-DASKROT9                         
286500        END-IF                                                            
286600     WHEN DATUM-SOEKNING = JA                                             
286700        MOVE WS-DASKROT9-BEORD TO W-DASKROT9-MIN                          
286800                                  W-DASKROT9-MAX                          
286900                                  W-DASKROT9                              
287000        PERFORM IMS-GNP-WDGX6322                                          
287100     WHEN DATUM-IDARTNR-SOEKNING = JA                                     
287200        MOVE WS-DASKROT9-BEORD TO W-DASKROT9-MIN                          
287300                                  W-DASKROT9-MAX                          
287400                                  W-DASKROT9                              
287500        PERFORM IMS-GNP-WDGX6322                                          
287600     WHEN KDARBTYP-SOEKNING = JA                                          
287700        PERFORM IMS-GNP-WDGX6322                                          
287800        IF SEGMENT-FINNS                                                  
287900           MOVE 6322-DASKROT9-BEORD TO W-DASKROT9                         
288000        END-IF                                                            
288100     WHEN OTHER                                                           
288200        CONTINUE                                                          
288300     END-EVALUATE                                                         
288400     .                                                                    
288500     EJECT                                                                
288600 S13-LAES-RADDATA SECTION.                                                
288700                                                                          
288800     EVALUATE TRUE                                                        
288900     WHEN KDARBTYP-PERSON-SOEKNING = JA                                   
289000          PERFORM IMS-GNP-WDGX6324-PERSON                                 
289100     WHEN IDARTNR-SOEKNING = JA                                           
289200          MOVE W-IDARTNR TO W-IDARTNR-MIN                                 
289300                            W-IDARTNR-MAX                                 
289400          PERFORM IMS-GNP-WDGX6324-ART                                    
289500     WHEN DATUM-SOEKNING = JA                                             
289600          PERFORM IMS-GNP-WDGX6324-ART                                    
289700     WHEN DATUM-IDARTNR-SOEKNING = JA                                     
289800          MOVE W-IDARTNR TO W-IDARTNR-MIN                                 
289900                            W-IDARTNR-MAX                                 
290000          PERFORM IMS-GNP-WDGX6324-ART                                    
290100     WHEN KDARBTYP-SOEKNING = JA                                          
290200          PERFORM IMS-GNP-WDGX6324-ART                                    
290300     WHEN OTHER                                                           
290400          CONTINUE                                                        
290500     END-EVALUATE                                                         
290600     .                                                                    
290700     EJECT                                                                
290800 S14-SKAPA-CLASSIC-TRANS SECTION.                                         
290900                                                                          
291000     PERFORM IMS-GHU-ARTC01                                               
291100     IF SEGMENT-FINNS                                                     
291200        MOVE ART-IDLEVNR    TO FILC-IDLEVNR                               
291300        MOVE W-IDARTNR      TO IDARTNR-WS                                 
291400        PERFORM IMS-GHU-ARTC11                                            
291500        IF SEGMENT-FINNS                                                  
291600           MOVE 495         TO CLAG-IDANSK                                
291700           MOVE 049         TO CLAG-IDBERED                               
291800           MOVE 99          TO CLAG-BEFT                                  
291900           MOVE 9           TO CLAG-IDPLANGR-AG                           
292000           MOVE 'GCP'       TO CLAG-IDPROJ                                
292100           MOVE ZERO        TO CLAG-KDFORPPL                              
292200           MOVE ZERO        TO CLAG-KDFORPGP                              
292300           MOVE ZERO        TO CLAG-KDFORPUF                              
292400*****                                                                     
292500           IF CLAG-IDINK (1:3) NUMERIC                                    
292600              MOVE CLAG-IDINK (1:3) TO FILC-IDINK                         
292700                                       TEST-IDINK                         
292800           ELSE                                                           
292900              IF CLAG-IDINK (2:3) NUMERIC                                 
293000                 MOVE CLAG-IDINK (2:3) TO FILC-IDINK                      
293100              ELSE                                                        
293200                 MOVE ZERO TO FILC-IDINK                                  
293300              END-IF                                                      
293400           END-IF                                                         
293500           MOVE '987'       TO CLAG-IDINK                                 
293600*****                                                                     
293700           IF CLAG-KDERS = 09    AND                                      
293800             (ART-IDLEVNR NOT = 'BQ8VA')                                  
293900              MOVE 00       TO PROGSW-MID-KDERS                           
294000              MOVE ALL '+'  TO PROGSW-MID-DIERS-ERS                       
294100              PERFORM S10-SKAPA-TRANS-TILL-1113                           
294200           END-IF                                                         
294300                                                                          
294400           PERFORM IMS-REPL-ARTC11                                        
294500           PERFORM IMS-GNP-ARTC23                                         
294600           PERFORM UNTIL SEGMENT-SAKNAS                                   
294700             MOVE AVT-IDAVTAL TO W-IDAVTAL-RED                            
294800***             TAR ÄVEN MED NAP-AVTAL, PREFIX = 004                      
294900             IF (TEST-IDINK > 99 AND                                      
295000                 TEST-IDINK < 790) OR                                     
295100                (TEST-IDINK > 799 AND                                     
295200                 TEST-IDINK < 987) OR                                     
295300                (TEST-IDINK > 987 AND                                     
295400                 TEST-IDINK < 1000) OR                                    
295500                 (W-PREFIX = '004')                                       
295600                PERFORM S16-SKAPA-B65                                     
295700             ELSE                                                         
295800                PERFORM S18-SKAPA-R22POST                                 
295900             END-IF                                                       
296000             PERFORM IMS-GNP-ARTC23                                       
296100           END-PERFORM                                                    
296200           PERFORM S14A-SKAPA-FORP-HIST                                   
296300           PERFORM S19-SKAPA-R23POST                                      
296400                                                                          
296500           MOVE W-IDARTNR   TO FILC-IDARTNR                               
296600           MOVE SPACE       TO FILC-BEART                                 
296700           MOVE FILC-W21632 TO FILC-FIL-WDR301-DATA                       
296800           ACCEPT W-TID FROM TIME                                         
296900           IF W-TID = FILC-FIL-TIKLOCK                                    
297000              ADD +1        TO FILC-FIL-IDSEKVNR                          
297100           ELSE                                                           
297200              MOVE W-TID    TO FILC-FIL-TIKLOCK                           
297300              MOVE +1       TO FILC-FIL-IDSEKVNR                          
297400           END-IF                                                         
297500           PERFORM IMS-ISRT-WLFILC                                        
297600                                                                          
297700           MOVE 'BQ8VA'    TO W-IDLEVNR-WDF2                              
297800           MOVE 'CLASSIC'  TO W-IDDIRGRP                                  
297900           MOVE W-IDARTNR  TO WDF2-ART-IDARTNR                            
298000           MOVE ZERO       TO WDF2-ART-DASTADAT                           
298100***********MOVE DAGENS-DATUM-Y2K TO WDF2-ART-DASTADAT                     
298200           MOVE -99        TO WDF2-ART-KVLS-DLEV                          
298300           MOVE ZERO       TO WDF2-ART-TIINLMOT                           
298400                              WDF2-ART-TIREGDAT                           
298500*      -99 BETYDER ATT SALDOT INTE UPPDATERAS AV LEVERANTÖR               
298600*      DET BETYDER ALLTSÅ INTE ATT VI HAR ETT NEGATIVT SALDO :-)          
298700*      FÖR LEVERANTÖRER SOM SKICKAR SALDOUPPGIFTER ÄR VÄRDET >= 0         
298800*      VID NYUPPLÄGG AV ARTIKLAR PÅ LEVERANTÖRER SOM REDOVISAR            
298900*      SALDO, KOMMER ARTIKELN SÅLEDES HA VÄRDET -99 TILL FÖRSTA           
299000*      UPPDATERINGEN AV SALDOT                                            
299100                                                                          
299200           PERFORM IMS-ISRT-WDF212                                        
299300        END-IF                                                            
299400     END-IF                                                               
299500     .                                                                    
299600     EJECT                                                                
299700 S14A-SKAPA-FORP-HIST SECTION.                                            
299800                                                                          
299900     PERFORM IMS-GU-WDT301                                                
300000     IF SEGMENT-SAKNAS                                                    
300100        MOVE W-IDARTNR TO FART-IDARTNR                                    
300200        PERFORM IMS-ISRT-WDT301                                           
300300     END-IF                                                               
300400**** INSERT WDT311                                                        
300500**** MOVE 'W60322'    TO FPCK-IDUSER                                      
300600     MOVE MSGI-IDUSER TO FPCK-IDUSER                                      
300700     MOVE 99          TO FPCK-BEFT                                        
300800     MOVE ZERO        TO FPCK-KDFORPPL                                    
300900     MOVE ZERO        TO FPCK-KDFORPGP                                    
301000     MOVE ZERO        TO FPCK-KDFORPUF                                    
301100     MOVE 'ÖVERGÅTT TILL CLASSIC DIREKTLEVERANS'                          
301200                      TO FPCK-TEBEFT(1)                                   
301300     MOVE SPACE       TO FPCK-TEBEFT(2)                                   
301400     MOVE SPACE       TO FPCK-TEBEFT(3)                                   
301500     MOVE SPACE       TO FPCK-TEBEFT(4)                                   
301600     MOVE SPACE       TO FPCK-TEBEFT(5)                                   
301700     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAREGDAT                       
301800     COMPUTE FPCK-DAREGDAT-9KOMPL = 999999999 - WS-DAREGDAT               
301900     ACCEPT WS-TID FROM TIME                                              
302000     COMPUTE FPCK-TIKLOCK-9KOMPL = 999999999 - WS-TID                     
302100     MOVE 'SE'        TO FPCK-IDLANDX2                                    
302200     PERFORM IMS-ISRT-WDT311                                              
302300     .                                                                    
302400     EJECT                                                                
302500 S15-SKAPA-WDGX2402 SECTION.                                              
302600                                                                          
302700     MOVE 6324-IDARTNR      TO 2402-IDARTNR                               
302800     MOVE 6324-IDANALYS     TO 2402-IDANALYS                              
302900     MOVE 6324-IDDC         TO 2402-IDDC                                  
303000     MOVE 6324-IDDISTR      TO 2402-IDDISTR                               
303100     MOVE 6324-IDKONTO      TO 2402-IDKONTO                               
303200     MOVE 6324-IDKST        TO 2402-IDKST                                 
303300     MOVE 6324-IDPERSON     TO 2402-IDPERSON                              
303400     MOVE 6321-KDARBTYP     TO 2402-KDARBTYP                              
303500     MOVE 6324-KVSKROT-BEORD TO 2402-KVSKROT-BEORD                        
303600     MOVE 6324-KVSKROT-ONDEM TO 2402-KVSKROT-KVAR                         
303700     MOVE 6324-IDUSER       TO 2402-IDUSER                                
303800     MOVE 6324-BEANST       TO 2402-BEANST                                
303900     MOVE WS-6322-DASKROT9  TO 2402-DASKROT9-BEORD                        
304000     MOVE 6326-TIDATETIME   TO 2402-TIDATETIME(1)                         
304100     MOVE 6326-IDUSER-GODK  TO 2402-IDUSER-GODK(1)                        
304200     MOVE 6326-BEANST-GODK  TO 2402-BEANST-GODK(1)                        
304300     MOVE 6324-IDKUNDNR     TO 2402-IDKUNDNR                              
304400     MOVE 6324-KDERS-UTG    TO 2402-KDERS-UTG                             
304500     MOVE 6324-KVTILLG-CDC  TO 2402-KVTILLG-CDC                           
304600     MOVE 6324-KVTILLG-SDC  TO 2402-KVTILLG-SDC                           
304700     MOVE 6324-KVAKS-CDC    TO 2402-KVAKS-CDC                             
304800     MOVE 6324-KVAKS-SDC    TO 2402-KVAKS-SDC                             
304900     MOVE +1     TO IX-BEEMB                                              
305000     PERFORM UNTIL IX-BEEMB > 20                                          
305100       MOVE 6324-BEEMBLEM(IX-BEEMB) TO 2402-BEEMBLEM(IX-BEEMB)            
305200       ADD +1 TO IX-BEEMB                                                 
305300     END-PERFORM                                                          
305400     MOVE 6324-SUTPO-TOT    TO 2402-SUTPO-TOT                             
305500     PERFORM IMS-ISRT-WDGX2402                                            
305600     .                                                                    
305700     EJECT                                                                
305800  S16-SKAPA-B65 SECTION.                                                  
305900                                                                          
306000     ACCEPT ZZAC-TIKLOCK             FROM  TIME                           
306100     ACCEPT ZZAC-TIAAMMDD FROM             DATE                           
306200     ADD 1                          TO WS-IDLOGLOP                        
306300     MOVE WS-IDLOGLOP               TO ZZAC-IDLOGLOP                      
306400     MOVE SPACE              TO A310-LEVNUM-GODSM                         
306500                              A310-ANT-BESTANN                            
306600     MOVE 'RY2'              TO A310-KT                                   
306700     MOVE DAGENS-DATUM       TO A310-DATUM-UTSKR                          
306800     MOVE ART-IDLEVNR        TO W-IDLEVNR                                 
306900     IF W-IDLEVNR (5:1) = SPACE                                           
307000*      LEVNUM SKALL TILLS VIDARE VARA NUMERISKT I X(5)                    
307100       MOVE ZERO             TO TALLY                                     
307200       INSPECT W-IDLEVNR TALLYING TALLY                                   
307300                          FOR CHARACTERS BEFORE INITIAL SPACE             
307400       IF TALLY = ZERO                                                    
307500          MOVE ZERO          TO WS-IDLEVNR-NUM                            
307600       ELSE                                                               
307700          MOVE W-IDLEVNR (1:TALLY)                                        
307800                             TO WS-IDLEVNR-NUM                            
307900       END-IF                                                             
308000       MOVE WS-IDLEVNR-NUM TO A310-LEVNUM                                 
308100     ELSE                                                                 
308200       MOVE W-IDLEVNR        TO A310-LEVNUM                               
308300     END-IF                                                               
308400     MOVE W-IDARTNR          TO WS-IDARTNR                                
308500     MOVE WS-IDARTNR         TO WS-IDARTNR-8                              
308600     MOVE WS-IDARTNR-8       TO A310-ARTNR                                
308700                              W092-SORTBGP                                
308800                                                                          
308900     MOVE AVT-IDAVTAL        TO W-IDAVTAL-RED                             
309000     MOVE W-PREFIX           TO A310-BESTPREF                             
309100     MOVE W-AVTALSNR         TO A310-BESTLNR                              
309200     MOVE W-SUFFIX           TO A310-BESTSUFF                             
309300     MOVE A310-A310B65       TO ZZAC-LOGGPOST                             
309400     MOVE W092-AREA          TO ZZAC-SORTPOST                             
309500     PERFORM IMS-ISRT-ZZAC                                                
309600     .                                                                    
309700     EJECT                                                                
309800*                                                                         
309900 S18-SKAPA-R22POST SECTION.                                               
310000                                                                          
310100     MOVE 'R22'                TO BAS-R22-IDPTYP                          
310200     MOVE W-IDARTNR            TO BAS-R22-IDARTNR                         
310300     MOVE AVT-IDAVTAL          TO BAS-R22-IDBEST                          
310400     MOVE AVT-IDLEVNR-AVT      TO BAS-R22-IDLEVNR-BEST                    
310500     MOVE SPACE                TO BAS-R22-IDLEVNR-SHIP                    
310600*    MOVE AVT-TIAVTAL          TO BAS-R22-TIBEST                          
310700     MOVE DAGENS-DATUM         TO BAS-R22-TIBEST                          
310800     MOVE AVT-KVAVTANT         TO BAS-R22-KVBEST                          
310900     MOVE 5                    TO BAS-R22-KDBEH-BEST                      
311000     MOVE SPACE                TO BAS-R22-TENOT-BESTPRIS                  
311100     MOVE SPACE                TO BAS-R22-REST                            
311200                                                                          
311300                                                                          
311400     MOVE MSG-JULIAN-DATE      TO POST-TIREGDAT W-TIREGDAT                
311500     ADD +1                    TO POST-TIKLOCK W-TIKLOCK                  
311600     MOVE MSG-SIGNON-USERID    TO POST-IDUSER                             
311700     MOVE MSG-LTERM-NAME       TO POST-IDLTERM                            
311800     MOVE ZERO                 TO POST-TIBORT                             
311900                                                                          
312000     MOVE BAS-R22-REGPOST      TO POST-REGPOST                            
312100     PERFORM IMS-ISRT-WDG901                                              
312200     IF SEGMENT-FINNS-REDAN                                               
312300       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
312400          ADD +1 TO POST-TIKLOCK W-TIKLOCK                                
312500          PERFORM IMS-ISRT-WDG901                                         
312600       END-PERFORM                                                        
312700     END-IF                                                               
312800     .                                                                    
312900     EJECT                                                                
313000*                                                                         
313100 S19-SKAPA-R23POST SECTION.                                               
313200                                                                          
313300     MOVE 'R23'                TO BAS-R23-IDPTYP                          
313400     MOVE W-IDARTNR            TO BAS-R23-IDARTNR                         
313500     MOVE 987910987094         TO BAS-R23-IDAVTAL                         
313600     MOVE 'BQ8VA'              TO BAS-R23-IDLEVNR-AVT                     
313700                                  BAS-R23-IDLEVNR-SHIP                    
313800     MOVE DAGENS-DATUM         TO BAS-R23-TIAVTAL                         
313900     MOVE ZERO                 TO BAS-R23-KVAVTANT                        
314000     MOVE +1                   TO BAS-R23-KDBEH-AVT                       
314100     MOVE SPACE                TO BAS-R23-TENOT-AVTPRIS                   
314200     MOVE SPACE                TO BAS-R23-REST                            
314300                                                                          
314400                                                                          
314500     MOVE MSG-JULIAN-DATE      TO POST-TIREGDAT W-TIREGDAT                
314600     ADD +1                    TO POST-TIKLOCK W-TIKLOCK                  
314700     ACCEPT POST-TIKLOCK FROM TIME                                        
314800     MOVE POST-TIKLOCK TO W-TIKLOCK                                       
314900     MOVE MSG-SIGNON-USERID    TO POST-IDUSER                             
315000     MOVE MSG-LTERM-NAME       TO POST-IDLTERM                            
315100     MOVE ZERO                 TO POST-TIBORT                             
315200                                                                          
315300     MOVE BAS-R23-REGPOST      TO POST-REGPOST                            
315400     PERFORM IMS-ISRT-WDG901                                              
315500     IF SEGMENT-FINNS-REDAN                                               
315600       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
315700          ADD +1 TO POST-TIKLOCK W-TIKLOCK                                
315800          PERFORM IMS-ISRT-WDG901                                         
315900       END-PERFORM                                                        
316000     END-IF                                                               
316100     .                                                                    
316200     EJECT                                                                
316300                                                                          
316400                                                                          
316500 S90-SKICKA-MAIL SECTION.                                                 
316600     MOVE W-IDARTNR         TO WSM-IDARTNR                                
316700     MOVE '6323'            TO MAIL-IDTRANS                               
316800     MOVE '1'               TO MAIL-KDMFSFOR                              
316900     MOVE 6328-IDMAIL       TO MAIL-IDMAIL                                
317000     MOVE 'SCRAPORDER'      TO MAIL-IDMAILTTL                             
317100     MOVE +23               TO MAIL-KVMAILLN                              
317200     MOVE 'GO TO SCREEN 6323 TO APPROVE SCRAPORDER'                       
317300                            TO MAIL-TEMAIL (1)                            
317400     MOVE 'JOB ROLE:      DC:        PART NO:'                            
317500                            TO MAIL-TEMAIL (2)                            
317600     MOVE SPACE             TO MAIL-TEMAIL (3)                            
317700     MOVE MSGI-KDARBTYP     TO MAIL-TEMAIL (3)(1:8)                       
317800     MOVE W-IDDC            TO MAIL-TEMAIL (3)(16:2)                      
317900                               W-IDDC-KVAL                                
318000     MOVE WSM-IDARTNR       TO MAIL-TEMAIL (3)(27:9)                      
318100                               W-IDARTNR-KVAL                             
318200     MOVE ' '                                                             
318300                            TO MAIL-TEMAIL (4)                            
318400     MOVE 'REASON FOR SCRAPPING:'                                         
318500                            TO MAIL-TEMAIL (5)                            
318600     MOVE 2                 TO W-KDSTASKR-KVAL                            
318700                                                                          
318800     PERFORM IMS-GU-WDGX6324-6325                                         
318900     IF SEGMENT-FINNS                                                     
319000       PERFORM IMS-GNP-WDGX6325                                           
319100       MOVE +1             TO IX                                          
319200       PERFORM UNTIL IX > MAX-IX OR SEGMENT-SAKNAS                        
319300         MOVE 6325-IDRADNR TO IX                                          
319400         IF IX = 1                                                        
319500           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (6)                           
319600         END-IF                                                           
319700         IF IX = 2                                                        
319800           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (7)                           
319900         END-IF                                                           
320000         IF IX = 3                                                        
320100           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (8)                           
320200         END-IF                                                           
320300         IF IX = 4                                                        
320400           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (9)                           
320500         END-IF                                                           
320600         IF IX = 5                                                        
320700           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (10)                          
320800         END-IF                                                           
320900         IF IX = 6                                                        
321000           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (11)                          
321100         END-IF                                                           
321200         IF IX = 7                                                        
321300           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (12)                          
321400         END-IF                                                           
321500         IF IX = 8                                                        
321600           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (13)                          
321700         END-IF                                                           
321800         IF IX = 9                                                        
321900           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (14)                          
322000         END-IF                                                           
322100         IF IX = 10                                                       
322200           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (15)                          
322300         END-IF                                                           
322400         IF IX = 11                                                       
322500           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (16)                          
322600         END-IF                                                           
322700                                                                          
322800         ADD +1 TO IX                                                     
322900         PERFORM IMS-GNP-WDGX6325                                         
323000       END-PERFORM                                                        
323100                                                                          
323200       MOVE ' '                                                           
323300                              TO MAIL-TEMAIL (17)                         
323400       MOVE 'STOCK SITUATION WHEN SCRAP WAS ISSUED:'                      
323500                              TO MAIL-TEMAIL (18)                         
323600                                                                          
323700       MOVE 6324-IDDC            TO MAIL-TEMAIL (21)(4:2)                 
323800       MOVE 6324-KDERS-UTG       TO WS-KDERS-UTG                          
323900       MOVE WS-KDERS-UTG         TO MAIL-TEMAIL (21)(60:2)                
324000       MOVE 6324-SUTPO-TOT       TO WS-SUTPO-TOT                          
324100       MOVE WS-SUTPO-TOT         TO MAIL-TEMAIL (20)(51:7)                
324200       MOVE 6324-KVSKROT-BEORD   TO WS-KVSKROT-BEORD                      
324300       MOVE WS-KVSKROT-BEORD     TO MAIL-TEMAIL (21)(27:7)                
324400       MOVE 6324-KVSKROT-KVAR    TO WS-KVSKROT-KVAR                       
324500       MOVE WS-KVSKROT-KVAR      TO MAIL-TEMAIL (21)(41:7)                
324600       MOVE 6324-KVTILLG-CDC     TO WS-KVTILLG-CDC                        
324700       MOVE WS-KVTILLG-CDC       TO MAIL-TEMAIL (20)(7:7)                 
324800       MOVE 6324-KVTILLG-SDC     TO WS-KVTILLG-SDC                        
324900       MOVE WS-KVTILLG-SDC       TO MAIL-TEMAIL (21)(7:7)                 
325000       MOVE 6324-KVAKS-CDC       TO WS-KVAKS-CDC                          
325100       MOVE WS-KVAKS-CDC         TO MAIL-TEMAIL (20)(15:7)                
325200       MOVE 6324-KVAKS-SDC       TO WS-KVAKS-SDC                          
325300       MOVE WS-KVAKS-SDC         TO MAIL-TEMAIL (21)(15:7)                
325400       MOVE +1    TO IX-BEEMB                                             
325500       PERFORM UNTIL IX-BEEMB > 20                                        
325600         IF IX-BEEMB = 1                                                  
325700          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(17:5)          
325800         END-IF                                                           
325900         IF IX-BEEMB = 2                                                  
326000          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(22:5)          
326100         END-IF                                                           
326200         IF IX-BEEMB = 3                                                  
326300          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(27:5)          
326400         END-IF                                                           
326500         IF IX-BEEMB = 4                                                  
326600          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(32:5)          
326700         END-IF                                                           
326800         IF IX-BEEMB = 5                                                  
326900          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(37:5)          
327000         END-IF                                                           
327100         IF IX-BEEMB = 6                                                  
327200          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(42:5)          
327300         END-IF                                                           
327400         IF IX-BEEMB = 7                                                  
327500          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(47:5)          
327600         END-IF                                                           
327700         IF IX-BEEMB = 8                                                  
327800          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (22)(52:5)          
327900         END-IF                                                           
328000         IF IX-BEEMB = 9                                                  
328100          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(1:5)           
328200         END-IF                                                           
328300         IF IX-BEEMB = 10                                                 
328400          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(6:5)           
328500         END-IF                                                           
328600         IF IX-BEEMB = 11                                                 
328700          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(11:5)          
328800         END-IF                                                           
328900         IF IX-BEEMB = 12                                                 
329000          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(16:5)          
329100         END-IF                                                           
329200         IF IX-BEEMB = 13                                                 
329300          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(21:5)          
329400         END-IF                                                           
329500         IF IX-BEEMB = 14                                                 
329600          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(26:5)          
329700         END-IF                                                           
329800         IF IX-BEEMB = 15                                                 
329900          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(31:5)          
330000         END-IF                                                           
330100         IF IX-BEEMB = 16                                                 
330200          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(36:5)          
330300         END-IF                                                           
330400         IF IX-BEEMB = 17                                                 
330500          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(41:5)          
330600         END-IF                                                           
330700         IF IX-BEEMB = 18                                                 
330800          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(46:5)          
330900         END-IF                                                           
331000         IF IX-BEEMB = 19                                                 
331100          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(51:5)          
331200         END-IF                                                           
331300         IF IX-BEEMB = 20                                                 
331400          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (23)(56:5)          
331500         END-IF                                                           
331600         ADD +1 TO IX-BEEMB                                               
331700       END-PERFORM                                                        
331800     END-IF                                                               
331900                                                                          
332000     MOVE 'STOCK BALANCE'   TO MAIL-TEMAIL (19)(1:13)                     
332100     MOVE 'AK-BALANCE'      TO MAIL-TEMAIL (19)(15:10)                    
332200     MOVE 'QANT TO SCRAP'   TO MAIL-TEMAIL (19)(27:13)                    
332300     MOVE 'REM STOCK'       TO MAIL-TEMAIL (19)(41:9)                     
332400     MOVE 'TPO-TOTAL'       TO MAIL-TEMAIL (19)(51:9)                     
332500                                                                          
332600     MOVE 'CDC'             TO MAIL-TEMAIL (20)(1:3)                      
332700                                                                          
332800     MOVE 'DC'              TO MAIL-TEMAIL (21)(1:2)                      
332900     MOVE 'SS CODE'         TO MAIL-TEMAIL (21)(51:7)                     
333000                                                                          
333100     MOVE 'PART IN VEHICLE' TO MAIL-TEMAIL (22)(1:15)                     
333200                                                                          
333300     IF MAIL-IDMAIL = SPACE                                               
333400       CONTINUE                                                           
333500     ELSE                                                                 
333600       PERFORM IMS-PURG-MAIL                                              
333700     END-IF                                                               
333800     .                                                                    
333900     EJECT                                                                
334000                                                                          
334100 S91-MAIL-ANNUL SECTION.                                                  
334200     MOVE W-IDARTNR         TO WSM-IDARTNR                                
334300     MOVE '6322'            TO MAIL-IDTRANS                               
334400     MOVE '1'               TO MAIL-KDMFSFOR                              
334500     MOVE W-ANNUL-IDMAIL    TO MAIL-IDMAIL                                
334600     MOVE 'ANNUL. SCRORD'   TO MAIL-IDMAILTTL                             
334700     MOVE +26               TO MAIL-KVMAILLN                              
334800     MOVE 'SCRAPORDER REJECTED BY APPROVER'                               
334900                            TO MAIL-TEMAIL (1)                            
335000     MOVE 'JOB ROLE:      DC:        PART NO:'                            
335100                            TO MAIL-TEMAIL (2)                            
335200     MOVE SPACE             TO MAIL-TEMAIL (3)                            
335300     MOVE MSGI-KDARBTYP     TO MAIL-TEMAIL (3)(1:8)                       
335400     MOVE W-IDDC            TO MAIL-TEMAIL (3)(16:2)                      
335500                               W-IDDC-KVAL                                
335600     MOVE WSM-IDARTNR       TO MAIL-TEMAIL (3)(27:9)                      
335700                               W-IDARTNR-KVAL                             
335800     MOVE ' '                                                             
335900                            TO MAIL-TEMAIL (4)                            
336000     MOVE 'REJECTED BY:                  E-MAIL:'                         
336100                            TO MAIL-TEMAIL (5)                            
336200     MOVE SPACE             TO MAIL-TEMAIL (6)                            
336300     MOVE ANUL-BEANST-GODK  TO MAIL-TEMAIL (6)(1:25)                      
336400     MOVE ANUL-IDMAIL(1:34) TO MAIL-TEMAIL (6)(31:34)                     
336500     MOVE ' '                                                             
336600                            TO MAIL-TEMAIL (7)                            
336700                                                                          
336800     MOVE 'REASON FOR SCRAPPING:'                                         
336900                            TO MAIL-TEMAIL (8)                            
337000     MOVE 1                 TO W-KDSTASKR-KVAL                            
337100                                                                          
337200     MOVE SPACE        TO MAIL-TEMAIL (9)                                 
337300     MOVE SPACE        TO MAIL-TEMAIL (10)                                
337400     MOVE SPACE        TO MAIL-TEMAIL (11)                                
337500     MOVE SPACE        TO MAIL-TEMAIL (12)                                
337600     MOVE SPACE        TO MAIL-TEMAIL (13)                                
337700     MOVE SPACE        TO MAIL-TEMAIL (14)                                
337800     MOVE SPACE        TO MAIL-TEMAIL (15)                                
337900     MOVE SPACE        TO MAIL-TEMAIL (16)                                
338000     MOVE SPACE        TO MAIL-TEMAIL (17)                                
338100     MOVE SPACE        TO MAIL-TEMAIL (18)                                
338200     MOVE SPACE        TO MAIL-TEMAIL (19)                                
338300                                                                          
338400     PERFORM IMS-GU-WDGX6324-6325                                         
338500     IF SEGMENT-FINNS                                                     
338600       PERFORM IMS-GNP-WDGX6325                                           
338700       MOVE ZERO           TO IX                                          
338800       PERFORM UNTIL SEGMENT-SAKNAS                                       
338900         MOVE 6325-IDRADNR TO IX                                          
339000         IF IX = 1                                                        
339100           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (9)                           
339200         END-IF                                                           
339300         IF IX = 2                                                        
339400           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (10)                          
339500         END-IF                                                           
339600         IF IX = 3                                                        
339700           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (11)                          
339800         END-IF                                                           
339900         IF IX = 4                                                        
340000           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (12)                          
340100         END-IF                                                           
340200         IF IX = 5                                                        
340300           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (13)                          
340400         END-IF                                                           
340500         IF IX = 6                                                        
340600           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (14)                          
340700         END-IF                                                           
340800         IF IX = 7                                                        
340900           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (15)                          
341000         END-IF                                                           
341100         IF IX = 8                                                        
341200           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (16)                          
341300         END-IF                                                           
341400         IF IX = 9                                                        
341500           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (17)                          
341600         END-IF                                                           
341700         IF IX = 10                                                       
341800           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (18)                          
341900         END-IF                                                           
342000         IF IX = 11                                                       
342100           MOVE 6325-TEMEMO  TO MAIL-TEMAIL (19)                          
342200         END-IF                                                           
342300                                                                          
342400         PERFORM IMS-GNP-WDGX6325                                         
342500       END-PERFORM                                                        
342600                                                                          
342700       MOVE ' '                                                           
342800                            TO MAIL-TEMAIL (20)                           
342900       MOVE 'STOCK SITUATION WHEN SCRAP WAS ISSUED:'                      
343000                            TO MAIL-TEMAIL (21)                           
343100                                                                          
343200       MOVE 6324-IDDC            TO MAIL-TEMAIL (24)(4:2)                 
343300       MOVE 6324-KDERS-UTG       TO WS-KDERS-UTG                          
343400       MOVE WS-KDERS-UTG         TO MAIL-TEMAIL (24)(60:2)                
343500       MOVE 6324-SUTPO-TOT       TO WS-SUTPO-TOT                          
343600       MOVE WS-SUTPO-TOT         TO MAIL-TEMAIL (23)(51:7)                
343700       MOVE 6324-KVSKROT-BEORD   TO WS-KVSKROT-BEORD                      
343800       MOVE WS-KVSKROT-BEORD     TO MAIL-TEMAIL (24)(27:7)                
343900       MOVE 6324-KVSKROT-KVAR    TO WS-KVSKROT-KVAR                       
344000       MOVE WS-KVSKROT-KVAR      TO MAIL-TEMAIL (24)(41:7)                
344100       MOVE 6324-KVTILLG-CDC     TO WS-KVTILLG-CDC                        
344200       MOVE WS-KVTILLG-CDC       TO MAIL-TEMAIL (23)(7:7)                 
344300       MOVE 6324-KVTILLG-SDC     TO WS-KVTILLG-SDC                        
344400       MOVE WS-KVTILLG-SDC       TO MAIL-TEMAIL (24)(7:7)                 
344500       MOVE 6324-KVAKS-CDC       TO WS-KVAKS-CDC                          
344600       MOVE WS-KVAKS-CDC         TO MAIL-TEMAIL (23)(15:7)                
344700       MOVE 6324-KVAKS-SDC       TO WS-KVAKS-SDC                          
344800       MOVE WS-KVAKS-SDC         TO MAIL-TEMAIL (24)(15:7)                
344900                                                                          
345000       MOVE SPACE                TO MAIL-TEMAIL (25)                      
345100       MOVE SPACE                TO MAIL-TEMAIL (26)                      
345200       MOVE +1    TO IX-BEEMB                                             
345300       PERFORM UNTIL IX-BEEMB > 20                                        
345400         IF IX-BEEMB = 1                                                  
345500          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(17:5)          
345600         END-IF                                                           
345700         IF IX-BEEMB = 2                                                  
345800          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(22:5)          
345900         END-IF                                                           
346000         IF IX-BEEMB = 3                                                  
346100          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(27:5)          
346200         END-IF                                                           
346300         IF IX-BEEMB = 4                                                  
346400          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(32:5)          
346500         END-IF                                                           
346600         IF IX-BEEMB = 5                                                  
346700          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(37:5)          
346800         END-IF                                                           
346900         IF IX-BEEMB = 6                                                  
347000          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(42:5)          
347100         END-IF                                                           
347200         IF IX-BEEMB = 7                                                  
347300          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(47:5)          
347400         END-IF                                                           
347500         IF IX-BEEMB = 8                                                  
347600          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (25)(52:5)          
347700         END-IF                                                           
347800         IF IX-BEEMB = 9                                                  
347900          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(1:5)           
348000         END-IF                                                           
348100         IF IX-BEEMB = 10                                                 
348200          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(6:5)           
348300         END-IF                                                           
348400         IF IX-BEEMB = 11                                                 
348500          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(11:5)          
348600         END-IF                                                           
348700         IF IX-BEEMB = 12                                                 
348800          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(16:5)          
348900         END-IF                                                           
349000         IF IX-BEEMB = 13                                                 
349100          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(21:5)          
349200         END-IF                                                           
349300         IF IX-BEEMB = 14                                                 
349400          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(26:5)          
349500         END-IF                                                           
349600         IF IX-BEEMB = 15                                                 
349700          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(31:5)          
349800         END-IF                                                           
349900         IF IX-BEEMB = 16                                                 
350000          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(36:5)          
350100         END-IF                                                           
350200         IF IX-BEEMB = 17                                                 
350300          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(41:5)          
350400         END-IF                                                           
350500         IF IX-BEEMB = 18                                                 
350600          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(46:5)          
350700         END-IF                                                           
350800         IF IX-BEEMB = 19                                                 
350900          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(51:5)          
351000         END-IF                                                           
351100         IF IX-BEEMB = 20                                                 
351200          MOVE 6324-BEEMBLEM(IX-BEEMB) TO MAIL-TEMAIL (26)(56:5)          
351300         END-IF                                                           
351400         ADD +1 TO IX-BEEMB                                               
351500       END-PERFORM                                                        
351600     END-IF                                                               
351700                                                                          
351800     MOVE 'STOCK BALANCE'   TO MAIL-TEMAIL (22)(1:13)                     
351900     MOVE 'AK-BALANCE'      TO MAIL-TEMAIL (22)(15:10)                    
352000     MOVE 'QANT TO SCRAP'   TO MAIL-TEMAIL (22)(27:13)                    
352100     MOVE 'REM STOCK'       TO MAIL-TEMAIL (22)(41:9)                     
352200     MOVE 'TPO-TOTAL'       TO MAIL-TEMAIL (22)(51:9)                     
352300                                                                          
352400     MOVE 'CDC'             TO MAIL-TEMAIL (23)(1:3)                      
352500                                                                          
352600     MOVE 'DC'              TO MAIL-TEMAIL (24)(1:2)                      
352700     MOVE 'SS CODE'         TO MAIL-TEMAIL (24)(51:7)                     
352800                                                                          
352900     MOVE 'PART IN VEHICLE' TO MAIL-TEMAIL (25)(1:15)                     
353000                                                                          
353100     IF MAIL-IDMAIL = SPACE                                               
353200       CONTINUE                                                           
353300     ELSE                                                                 
353400       PERFORM IMS-PURG-MAIL                                              
353500     END-IF                                                               
353600     .                                                                    
353700     EJECT                                                                
353800                                                                          
353900                                                                          
354000 S95-JUMP-6325             SECTION.                                       
354100     MOVE MSGI-KDARBTYP        TO  MOD6325-MID-KDARBTYP-IN                
354200     COMPUTE P-TO-P-KVLL       =  LENGTH OF MOD6325-MID-W6I32501          
354300                                   +  17                                  
354400     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
354500     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
354600     MOVE 'W6T325  '           TO P-TO-P-KDTRANS                          
354700     MOVE '6322'               TO P-TO-P-IDTRANS                          
354800     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
354900                                                                          
355000     MOVE MOD6325-MID-W6I32501 TO P-TO-P-DATA                             
355100     PERFORM IMS-ISRT-ALT1-MSG                                            
355200     .                                                                    
355300     SKIP3                                                                
355400                                                                          
355500 S96-JUMP-2352             SECTION.                                       
355600                                                                          
355700     COMPUTE P-TO-P-KVLL       =  LENGTH OF 2352-MID-W2I35201             
355800                                   +  17                                  
355900     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
356000     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
356100     MOVE 'W2T352  '           TO P-TO-P-KDTRANS                          
356200     MOVE '6322'               TO P-TO-P-IDTRANS                          
356300     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
356400                                                                          
356500     MOVE 2352-MID-W2I35201 TO P-TO-P-DATA                                
356600     MOVE NEJ               TO JUMP-2352-SW                               
356700     PERFORM IMS-ISRT-ALT2-MSG                                            
356800     .                                                                    
356900     SKIP3                                                                
357000                                                                          
357100 S97-JUMP-2372             SECTION.                                       
357200     COMPUTE P-TO-P-KVLL       =  LENGTH OF 2372-MID-W2I37201             
357300                                   +  17                                  
357400     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
357500     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
357600     MOVE 'W2T372  '           TO P-TO-P-KDTRANS                          
357700     MOVE '6322'               TO P-TO-P-IDTRANS                          
357800     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
357900                                                                          
358000     MOVE 2372-MID-W2I37201 TO P-TO-P-DATA                                
358100     MOVE NEJ               TO JUMP-2372-SW                               
358200     PERFORM IMS-ISRT-ALT3-MSG                                            
358300     .                                                                    
358400     SKIP3                                                                
358500                                                                          
358600 MFS-RENSA-FAELT-UT SECTION.                                              
358700                                                                          
358800*    --- ALLA UTDATA-FÄLT                                                 
358900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
359000     MOVE MFS-RENSA-FAELT TO MOD-FLKLAR                                   
359100     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
359200     .                                                                    
359300     SKIP3                                                                
359400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
359500                                                                          
359600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
359700     MOVE +1 TO RAD-IX                                                    
359800     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
359900        MOVE MFS-RENSA-FAELT TO MOD-CMD-IN(RAD-IX)                        
360000                                MOD-IDARTNR(RAD-IX)                       
360100                                MOD-KVANTAL(RAD-IX)                       
360200                                MOD-SUARTSTD(RAD-IX)                      
360300                                MOD-TIDATUM(RAD-IX)                       
360400                                MOD-IDDC(RAD-IX)                          
360500                                MOD-IDPERSON(RAD-IX)                      
360600                                MOD-REM(RAD-IX)                           
360700                                MOD-IDUSER(RAD-IX)                        
360800                                MOD-ACC(RAD-IX)                           
360900                                MOD-FLCLASS(RAD-IX)                       
361000                                MOD-FLTEXT(RAD-IX)                        
361100        ADD +1 TO RAD-IX                                                  
361200     END-PERFORM                                                          
361300     .                                                                    
361400     EJECT                                                                
361500 MFS-RENSA-FAELT-IN SECTION.                                              
361600                                                                          
361700*    --- ALLA INDATA-FÄLT                                                 
361800     MOVE MFS-RENSA-FAELT TO MOD-FLKLAR                                   
361900     MOVE +1 TO RAD-IX                                                    
362000     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
362100        MOVE MFS-RENSA-FAELT TO MOD-CMD-IN(RAD-IX)                        
362200                                MOD-IDARTNR(RAD-IX)                       
362300                                MOD-IDDC(RAD-IX)                          
362400                                MOD-TIDATUM(RAD-IX)                       
362500        ADD +1 TO RAD-IX                                                  
362600     END-PERFORM                                                          
362700     .                                                                    
362800     SKIP2                                                                
362900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
363000                                                                          
363100*    --- ALLA UTDATA-FÄLT                                                 
363200*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
363300     MOVE MFS-ROER-EJ-FAELT TO MOD-FLKLAR                                 
363400     PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                     
363500     .                                                                    
363600     EJECT                                                                
363700 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
363800                                                                          
363900*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
364000     MOVE +1 TO RAD-IX                                                    
364100     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
364200        MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-IN(RAD-IX)                      
364300                                  MOD-IDARTNR(RAD-IX)                     
364400                                  MOD-KVANTAL(RAD-IX)                     
364500                                  MOD-SUARTSTD(RAD-IX)                    
364600                                  MOD-TIDATUM(RAD-IX)                     
364700                                  MOD-IDDC(RAD-IX)                        
364800                                  MOD-IDPERSON(RAD-IX)                    
364900                                  MOD-REM(RAD-IX)                         
365000                                  MOD-IDUSER(RAD-IX)                      
365100                                  MOD-ACC(RAD-IX)                         
365200                                  MOD-FLCLASS(RAD-IX)                     
365300                                  MOD-FLTEXT(RAD-IX)                      
365400        ADD +1 TO RAD-IX                                                  
365500     END-PERFORM                                                          
365600     .                                                                    
365700     SKIP2                                                                
365800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
365900                                                                          
366000*    --- ALLA INDATA-FÄLT                                                 
366100     MOVE MFS-ROER-EJ-FAELT TO MOD-FLKLAR                                 
366200     MOVE +1 TO RAD-IX                                                    
366300     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
366400        MOVE MFS-ROER-EJ-FAELT TO MOD-CMD-IN(RAD-IX)                      
366500        ADD +1 TO RAD-IX                                                  
366600     END-PERFORM                                                          
366700     .                                                                    
366800     EJECT                                                                
366900 MFS-FORM-ATTR SECTION.                                                   
367000                                                                          
367100*    --- ALLA INDATA-FÄLT                                                 
367200     MOVE MFS-FORMATETS-ATTR TO MOD-FLKLAR-ATTR                           
367300     MOVE +1 TO RAD-IX                                                    
367400     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
367500        MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR(RAD-IX)                   
367600        ADD +1 TO RAD-IX                                                  
367700     END-PERFORM                                                          
367800     .                                                                    
367900     SKIP2                                                                
368000 MFS-RENSA-SPAR-NYCKLAR SECTION.                                          
368100                                                                          
368200     MOVE MFS-RENSA-FAELT TO SPAR-IDARTNR-ENTER                           
368300                             SPAR-IDARTNR-NEXT                            
368400                             SPAR-IDDC-ENTER                              
368500                             SPAR-IDDC-NEXT                               
368600                             SPAR-IDANSK-ENTER                            
368700                             SPAR-IDANSK-NEXT-MIN                         
368800                             SPAR-IDANSK-NEXT-MAX                         
368900                             SPAR-DASKROT9-BEORD-ENTER                    
369000                             SPAR-DASKROT9-BEORD-NEXT                     
369100                             SPAR-KDARBTYP-NEXT                           
369200                             SPAR-KDARBTYP-ENTER                          
369300     .                                                                    
369400     EJECT                                                                
369500* --- IMS SEKTIONER ---                                                   
369600     SKIP3                                                                
369700 IMS-GET-MSG SECTION.                                                     
369800     MOVE '  QC' TO GODK-STATUSKODER                                      
369900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
370000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
370100     PERFORM IMS-STATUSKONTROLL                                           
370200     .                                                                    
370300     SKIP3                                                                
370400 IMS-INSERT-MSG SECTION.                                                  
370500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
370600     MOVE SPACE TO GODK-STATUSKODER                                       
370700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
370800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
370900     PERFORM IMS-STATUSKONTROLL                                           
371000     .                                                                    
371100     SKIP3                                                                
371200 IMS-INSERT-ALTMSG SECTION.                                               
371300     MOVE SPACE TO GODK-STATUSKODER                                       
371400     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
371500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
371600     PERFORM IMS-STATUSKONTROLL                                           
371700     .                                                                    
371800     EJECT                                                                
371900 IMS-GHU-ARTC01 SECTION.                                                  
372000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
372100          DELIMITED BY SIZE INTO SSA1                                     
372200     MOVE '  GE' TO GODK-STATUSKODER                                      
372300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC01 SSA1                 
372400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
372500     PERFORM IMS-STATUSKONTROLL                                           
372600     .                                                                    
372700     SKIP3                                                                
372800 IMS-GHU-ARTC11 SECTION.                                                  
372900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
373000          DELIMITED BY SIZE INTO SSA1                                     
373100     MOVE 'WDK611   ' TO SSA2                                             
373200     MOVE '  GE' TO GODK-STATUSKODER                                      
373300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-WLARTC11 SSA1 SSA2            
373400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
373500     PERFORM IMS-STATUSKONTROLL                                           
373600     .                                                                    
373700     SKIP3                                                                
373800 IMS-GNP-ARTC23 SECTION.                                                  
373900     MOVE  'WDK623   ' TO  SSA1                                           
374000     MOVE '  GE' TO GODK-STATUSKODER                                      
374100     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC23 SSA1                 
374200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
374300     PERFORM IMS-STATUSKONTROLL                                           
374400     .                                                                    
374500     SKIP2                                                                
374600 IMS-GU-WDT301 SECTION.                                                   
374700     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
374800          DELIMITED BY SIZE INTO SSA1                                     
374900     MOVE '  GE' TO GODK-STATUSKODER                                      
375000     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT301 SSA1                    
375100     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
375200     PERFORM IMS-STATUSKONTROLL                                           
375300     .                                                                    
375400     SKIP3                                                                
375500 IMS-ISRT-WDT301 SECTION.                                                 
375600     MOVE 'WDT301   ' TO SSA1                                             
375700     MOVE '    ' TO GODK-STATUSKODER                                      
375800     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT301 SSA1                  
375900     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
376000     PERFORM IMS-STATUSKONTROLL                                           
376100     .                                                                    
376200     SKIP3                                                                
376300 IMS-ISRT-WDT311 SECTION.                                                 
376400     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
376500          DELIMITED BY SIZE INTO SSA1                                     
376600     MOVE 'WDT311   ' TO SSA2                                             
376700     MOVE '    ' TO GODK-STATUSKODER                                      
376800     CALL CBLTDLI USING ISRT WDT3-PCB DLI-IO-WDT311 SSA1 SSA2             
376900     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
377000     PERFORM IMS-STATUSKONTROLL                                           
377100     .                                                                    
377200     SKIP3                                                                
377300 IMS-REPL-ARTC11 SECTION.                                                 
377400     MOVE '  ' TO GODK-STATUSKODER                                        
377500     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-WLARTC11                     
377600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
377700     PERFORM IMS-STATUSKONTROLL                                           
377800     .                                                                    
377900     EJECT                                                                
378000 IMS-GHU-ARTS11 SECTION.                                                  
378100     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
378200          DELIMITED BY SIZE INTO SSA1                                     
378300     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
378400          DELIMITED BY SIZE INTO SSA2                                     
378500     MOVE '  GE' TO GODK-STATUSKODER                                      
378600     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-WLARTS11 SSA1 SSA2            
378700     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
378800     PERFORM IMS-STATUSKONTROLL                                           
378900     .                                                                    
379000     SKIP3                                                                
379100 IMS-REPL-ARTS11 SECTION.                                                 
379200     MOVE '  ' TO GODK-STATUSKODER                                        
379300     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-WLARTS11                     
379400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
379500     PERFORM IMS-STATUSKONTROLL                                           
379600     .                                                                    
379700     SKIP3                                                                
379800 IMS-REPL-WDGX6324 SECTION.                                               
379900     MOVE '  ' TO GODK-STATUSKODER                                        
380000     CALL CBLTDLI USING REPL 6321-PCB DLI-IO-WDGX6324                     
380100     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
380200     PERFORM IMS-STATUSKONTROLL                                           
380300     .                                                                    
380400     EJECT                                                                
380500 IMS-GHU-WDR501-6321 SECTION.                                             
380600     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
380700          DELIMITED BY SIZE INTO SSA1                                     
380800     MOVE 'GE  ' TO GODK-STATUSKODER                                      
380900     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDR501-6321 SSA1              
381000     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
381100     PERFORM IMS-STATUSKONTROLL                                           
381200     .                                                                    
381300     SKIP3                                                                
381400 IMS-GNP-WDGX6322 SECTION.                                                
381500     STRING 'WDGX6322(DASKROT9=>' W-DASKROT9-MIN-X                        
381600                    '&DASKROT9=<' W-DASKROT9-MAX-X ')'                    
381700          DELIMITED BY SIZE INTO SSA1                                     
381800     MOVE '  GE' TO GODK-STATUSKODER                                      
381900     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6322 SSA1                 
382000     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
382100     PERFORM IMS-STATUSKONTROLL                                           
382200     .                                                                    
382300     EJECT                                                                
382400 IMS-DLET-WDGX6324 SECTION.                                               
382500     MOVE '  ' TO GODK-STATUSKODER                                        
382600     CALL CBLTDLI USING DLET 6321-PCB DLI-IO-WDGX6324                     
382700     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
382800     PERFORM IMS-STATUSKONTROLL                                           
382900     .                                                                    
383000     SKIP2                                                                
383100 IMS-GNP-WDGX6324-PERSON SECTION.                                         
383200     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
383300          DELIMITED BY SIZE INTO SSA1                                     
383400     STRING 'WDGX6324(IDARTNR =>' W-IDARTNR-MIN-X                         
383500                    '&IDARTNR =<' W-IDARTNR-MAX-X                         
383600                    '&KDSTASKR =' W-KDSTASKR-X                            
383700                    '&IDDC     =' W-IDDC-6324-X                           
383800                    '&IDPERSON=>' W-IDPERSON-MIN-X                        
383900                    '&IDPERSON=<' W-IDPERSON-MAX-X ')'                    
384000          DELIMITED BY SIZE INTO SSA2                                     
384100     MOVE '  GE' TO GODK-STATUSKODER                                      
384200     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2            
384300     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
384400     PERFORM IMS-STATUSKONTROLL                                           
384500     .                                                                    
384600     EJECT                                                                
384700 IMS-GNP-WDGX6324-ART SECTION.                                            
384800     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
384900          DELIMITED BY SIZE INTO SSA1                                     
385000     STRING 'WDGX6324(IDARTNR =>' W-IDARTNR-MIN-X                         
385100                    '&IDARTNR =<' W-IDARTNR-MAX-X                         
385200                    '&IDDC     =' W-IDDC-6324-X                           
385300                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
385400          DELIMITED BY SIZE INTO SSA2                                     
385500     MOVE '  GE' TO GODK-STATUSKODER                                      
385600     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2            
385700     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
385800     PERFORM IMS-STATUSKONTROLL                                           
385900     .                                                                    
386000     SKIP2                                                                
386100 IMS-GHNP-WDGX6324 SECTION.                                               
386200     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
386300          DELIMITED BY SIZE INTO SSA1                                     
386400     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
386500                    '&IDDC     =' W-IDDC-6324-X                           
386600                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
386700          DELIMITED BY SIZE INTO SSA2                                     
386800     MOVE '  GE' TO GODK-STATUSKODER                                      
386900     CALL CBLTDLI USING GHNP 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2           
387000     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
387100     PERFORM IMS-STATUSKONTROLL                                           
387200     .                                                                    
387300     SKIP2                                                                
387400 IMS-GU-WDGX6324-2 SECTION.                                               
387500     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
387600            DELIMITED BY SIZE INTO SSA1                                   
387700     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
387800          DELIMITED BY SIZE INTO SSA2                                     
387900     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
388000                    '&IDDC     =' W-IDDC-6324-X                           
388100                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
388200          DELIMITED BY SIZE INTO SSA3                                     
388300     MOVE '  GE' TO GODK-STATUSKODER                                      
388400     CALL CBLTDLI USING GU 6321-PCB DLI-IO-WDGX6324 SSA1 SSA2             
388500                                                      SSA3                
388600     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
388700     PERFORM IMS-STATUSKONTROLL                                           
388800     .                                                                    
388900     SKIP2                                                                
389000 IMS-ISRT-WDGX6326 SECTION.                                               
389100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
389200            DELIMITED BY SIZE INTO SSA1                                   
389300     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
389400            DELIMITED BY SIZE INTO SSA2                                   
389500     STRING 'WDGX6324(IDARTNR  =' W-IDARTNR-X                             
389600                    '&IDDC     =' W-IDDC-6324-X                           
389700                    '&KDSTASKR =' W-KDSTASKR-X ')'                        
389800          DELIMITED BY SIZE INTO SSA3                                     
389900     MOVE 'WDGX6326'            TO SSA4                                   
390000     MOVE '  '                  TO GODK-STATUSKODER                       
390100     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6326                     
390200                                      SSA1 SSA2 SSA3 SSA4                 
390300     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
390400     PERFORM IMS-STATUSKONTROLL                                           
390500     .                                                                    
390600     EJECT                                                                
390700 IMS-ISRT-WDGX6324 SECTION.                                               
390800     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
390900            DELIMITED BY SIZE INTO SSA1                                   
391000     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
391100            DELIMITED BY SIZE INTO SSA2                                   
391200     MOVE 'WDGX6324'            TO SSA3                                   
391300     MOVE '  '                  TO GODK-STATUSKODER                       
391400     CALL CBLTDLI USING ISRT 6321-PCB DLI-IO-WDGX6324                     
391500                                      SSA1 SSA2 SSA3                      
391600     MOVE 6321-STATUS-CODE      TO STATUS-WS                              
391700     PERFORM IMS-STATUSKONTROLL                                           
391800     .                                                                    
391900     EJECT                                                                
392000 IMS-ISRT-WLFILC SECTION.                                                 
392100     STRING 'WLFILC01    '                                                
392200          DELIMITED BY SIZE INTO SSA1                                     
392300     MOVE '   ' TO GODK-STATUSKODER                                       
392400     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-FILC SSA1               
392500     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
392600     PERFORM IMS-STATUSKONTROLL                                           
392700     .                                                                    
392800     EJECT                                                                
392900 IMS-GHU-WDGX6322 SECTION.                                                
393000     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6321 ')'                      
393100          DELIMITED BY SIZE INTO SSA1                                     
393200     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
393300          DELIMITED BY SIZE INTO SSA2                                     
393400     MOVE '  GE' TO GODK-STATUSKODER                                      
393500     CALL CBLTDLI USING GHU 6321-PCB DLI-IO-WDGX6322 SSA1 SSA2            
393600     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
393700     PERFORM IMS-STATUSKONTROLL                                           
393800     .                                                                    
393900     SKIP2                                                                
394000 IMS-GNP-WDGX6324 SECTION.                                                
394100     MOVE 'WDGX6324 ' TO SSA1                                             
394200     MOVE '  GE' TO GODK-STATUSKODER                                      
394300     CALL CBLTDLI USING GNP 6321-PCB DLI-IO-WDGX6324 SSA1                 
394400     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
394500     PERFORM IMS-STATUSKONTROLL                                           
394600     .                                                                    
394700     SKIP3                                                                
394800 IMS-DLET-WDGX6322 SECTION.                                               
394900     MOVE '  ' TO GODK-STATUSKODER                                        
395000     CALL CBLTDLI USING DLET 6321-PCB DLI-IO-WDGX6322                     
395100     MOVE 6321-STATUS-CODE TO STATUS-WS                                   
395200     PERFORM IMS-STATUSKONTROLL                                           
395300     .                                                                    
395400     EJECT                                                                
395500 IMS-GU-WDGX6324-6325 SECTION.                                            
395600                                                                          
395700     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
395800            DELIMITED BY SIZE INTO SSA1                                   
395900     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
396000            DELIMITED BY SIZE INTO SSA2                                   
396100     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
396200          DELIMITED BY SIZE INTO SSA3                                     
396300     MOVE '  GE' TO GODK-STATUSKODER                                      
396400     CALL CBLTDLI USING GU 6325-PCB DLI-IO-WDGX6324                       
396500                                      SSA1 SSA2 SSA3                      
396600     MOVE 6325-STATUS-CODE TO STATUS-WS                                   
396700     PERFORM IMS-STATUSKONTROLL                                           
396800     .                                                                    
396900     SKIP3                                                                
397000 IMS-GNP-WDGX6325 SECTION.                                                
397100                                                                          
397200     MOVE 'WDGX6325 '  TO SSA1                                            
397300     MOVE '  GE' TO GODK-STATUSKODER                                      
397400     CALL CBLTDLI USING GNP 6325-PCB DLI-IO-WDGX6325 SSA1                 
397500     MOVE 6325-STATUS-CODE TO STATUS-WS                                   
397600     PERFORM IMS-STATUSKONTROLL                                           
397700     .                                                                    
397800     SKIP3                                                                
397900 IMS-ISRT-WDGX6325 SECTION.                                               
398000                                                                          
398100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6321   ')'                    
398200            DELIMITED BY SIZE INTO SSA1                                   
398300     STRING 'WDGX6322(DASKROT9 =' W-DASKROT9-X  ')'                       
398400            DELIMITED BY SIZE INTO SSA2                                   
398500     STRING 'WDGX6324(KY6324   =' W-KY6324-KVAL-X ')'                     
398600          DELIMITED BY SIZE INTO SSA3                                     
398700     MOVE 'WDGX6325 '  TO SSA4                                            
398800     MOVE '    ' TO GODK-STATUSKODER                                      
398900     CALL CBLTDLI USING ISRT 6325-PCB DLI-IO-WDGX6325 SSA1 SSA2           
399000                                                      SSA3 SSA4           
399100     MOVE 6325-STATUS-CODE TO STATUS-WS                                   
399200     PERFORM IMS-STATUSKONTROLL                                           
399300     .                                                                    
399400     SKIP3                                                                
399500 IMS-GU-WDGX6327 SECTION.                                                 
399600     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6327 ')'                      
399700          DELIMITED BY SIZE INTO SSA1                                     
399800     MOVE '  GE' TO GODK-STATUSKODER                                      
399900     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6328 SSA1                  
400000     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
400100     PERFORM IMS-STATUSKONTROLL                                           
400200     .                                                                    
400300     SKIP2                                                                
400400 IMS-GNP-WDGX6328 SECTION.                                                
400500     MOVE 'WDGX6328' TO SSA1                                              
400600     MOVE '  GE' TO GODK-STATUSKODER                                      
400700     CALL CBLTDLI USING GNP 6327-PCB DLI-IO-WDGX6328 SSA1                 
400800     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
400900     PERFORM IMS-STATUSKONTROLL                                           
401000     .                                                                    
401100     SKIP2                                                                
401200 IMS-GU-WDGX6328 SECTION.                                                 
401300     STRING 'WDR501  (WDGXKEY = ' W-WDGXKEY-6327 ')'                      
401400          DELIMITED BY SIZE INTO SSA1                                     
401500     STRING 'WDGX6328(SUBEL   =>' W-SUBEL-MIN-X                           
401600                    '&SUBEL   =<' W-SUBEL-MAX-X                           
401700                    '&IDUSERGK= ' W-IDUSER-GODK-X ')'                     
401800          DELIMITED BY SIZE INTO SSA2                                     
401900     MOVE '  GE' TO GODK-STATUSKODER                                      
402000     CALL CBLTDLI USING GU 6327-PCB DLI-IO-WDGX6328 SSA1 SSA2             
402100     MOVE 6327-STATUS-CODE TO STATUS-WS                                   
402200     PERFORM IMS-STATUSKONTROLL                                           
402300     .                                                                    
402400     SKIP2                                                                
402500 IMS-ISRT-WDGX2402 SECTION.                                               
402600     STRING 'WDR501  (WDGXKEY  =' W-2401-KEY-X ')'                        
402700            DELIMITED BY SIZE INTO SSA1                                   
402800     MOVE 'WDGX2402'            TO SSA2                                   
402900     MOVE '  '                  TO GODK-STATUSKODER                       
403000     CALL CBLTDLI USING ISRT 2401-PCB DLI-IO-WDR501-2401 SSA1 SSA2        
403100     MOVE 2401-STATUS-CODE      TO STATUS-WS                              
403200     PERFORM IMS-STATUSKONTROLL                                           
403300     SKIP3                                                                
403400     .                                                                    
403500     EJECT                                                                
403600 IMS-GU-WDD801 SECTION.                                                   
403700                                                                          
403800     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
403900          DELIMITED BY SIZE INTO SSA1                                     
404000     MOVE '  GE' TO GODK-STATUSKODER                                      
404100     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD801 SSA1                    
404200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
404300     PERFORM IMS-STATUSKONTROLL                                           
404400     .                                                                    
404500     EJECT                                                                
404600 IMS-GHNP-WDD811 SECTION.                                                 
404700                                                                          
404800     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
404900          DELIMITED BY SIZE INTO SSA1                                     
405000     STRING 'WDD811   '                                                   
405100          DELIMITED BY SIZE INTO SSA2                                     
405200     MOVE '  GE' TO GODK-STATUSKODER                                      
405300     CALL CBLTDLI USING GHNP WDD8-PCB DLI-IO-WDD811 SSA1 SSA2             
405400     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
405500     PERFORM IMS-STATUSKONTROLL                                           
405600     .                                                                    
405700     SKIP3                                                                
405800 IMS-REPL-WDD811 SECTION.                                                 
405900                                                                          
406000     MOVE '  ' TO GODK-STATUSKODER                                        
406100     CALL CBLTDLI USING REPL WDD8-PCB DLI-IO-WDD811                       
406200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
406300     PERFORM IMS-STATUSKONTROLL                                           
406400     .                                                                    
406500     EJECT                                                                
406600 IMS-DLET-WDD811 SECTION.                                                 
406700                                                                          
406800     MOVE '  ' TO GODK-STATUSKODER                                        
406900     CALL CBLTDLI USING DLET WDD8-PCB DLI-IO-WDD811                       
407000     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
407100     PERFORM IMS-STATUSKONTROLL                                           
407200     .                                                                    
407300     EJECT                                                                
407400 IMS-GU-WDJ901 SECTION.                                                   
407500                                                                          
407600     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
407700          DELIMITED BY SIZE INTO SSA1                                     
407800     MOVE '  GE' TO GODK-STATUSKODER                                      
407900     CALL CBLTDLI USING GU WDJ9-PCB DLI-IO-WDJ901 SSA1                    
408000     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
408100     PERFORM IMS-STATUSKONTROLL                                           
408200     .                                                                    
408300     EJECT                                                                
408400 IMS-GHNP-WDJ911 SECTION.                                                 
408500                                                                          
408600     MOVE '  GE' TO GODK-STATUSKODER                                      
408700     CALL CBLTDLI USING GHNP WDJ9-PCB DLI-IO-WDJ911                       
408800     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
408900     PERFORM IMS-STATUSKONTROLL                                           
409000                                                                          
409100     EJECT                                                                
409200     .                                                                    
409300                                                                          
409400 IMS-REPL-WDJ911 SECTION.                                                 
409500                                                                          
409600     MOVE '  ' TO GODK-STATUSKODER                                        
409700     CALL CBLTDLI USING REPL WDJ9-PCB DLI-IO-WDJ911                       
409800     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
409900     PERFORM IMS-STATUSKONTROLL                                           
410000     .                                                                    
410100     EJECT                                                                
410200 IMS-ISRT-ZZAC SECTION.                                                   
410300                                                                          
410400     MOVE 'WLZZAC01 '        TO SSA1                                      
410500     MOVE '  '               TO GODK-STATUSKODER                          
410600     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA5 SSA1                   
410700     MOVE ZZAC-STATUS-CODE     TO STATUS-WS                               
410800     PERFORM IMS-STATUSKONTROLL                                           
410900     .                                                                    
411000     SKIP3                                                                
411100 IMS-ISRT-WDG901 SECTION.                                                 
411200                                                                          
411300     MOVE 'WLZZAD01 ' TO SSA1                                             
411400     MOVE '  II' TO GODK-STATUSKODER                                      
411500     CALL CBLTDLI USING ISRT ZZAD-PCB DLI-IO-WDG901 SSA1                  
411600     MOVE ZZAD-STATUS-CODE TO STATUS-WS                                   
411700     PERFORM IMS-STATUSKONTROLL                                           
411800     .                                                                    
411900     EJECT                                                                
412000                                                                          
412100 IMS-PURG-MAIL SECTION.                                                   
412200                                                                          
412300     MOVE SPACE TO GODK-STATUSKODER                                       
412400     CALL CBLTDLI USING PURG ALTMAIL-PCB MAIL-WMSGMAIL                    
412500     MOVE ALTMAIL-STATUS-CODE TO STATUS-WS                                
412600     PERFORM IMS-STATUSKONTROLL                                           
412700     .                                                                    
412800     EJECT                                                                
412900                                                                          
413000 IMS-ISRT-ALT1-MSG SECTION.                                               
413100     MOVE SPACE TO GODK-STATUSKODER                                       
413200     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-SW                           
413300     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
413400     PERFORM IMS-STATUSKONTROLL                                           
413500     .                                                                    
413600     SKIP2                                                                
413700 IMS-ISRT-ALT2-MSG SECTION.                                               
413800     MOVE SPACE TO GODK-STATUSKODER                                       
413900     CALL CBLTDLI USING ISRT ALT2-PCB P-TO-P-SW                           
414000     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
414100     PERFORM IMS-STATUSKONTROLL                                           
414200     .                                                                    
414300     SKIP2                                                                
414400 IMS-ISRT-ALT3-MSG SECTION.                                               
414500     MOVE SPACE TO GODK-STATUSKODER                                       
414600     CALL CBLTDLI USING ISRT ALT3-PCB P-TO-P-SW                           
414700     MOVE ALT3-STATUS-CODE TO STATUS-WS                                   
414800     PERFORM IMS-STATUSKONTROLL                                           
414900     .                                                                    
415000     SKIP2                                                                
415100 IMS-ISRT-ALT-PCB SECTION.                                                
415200     MOVE '  ' TO GODK-STATUSKODER                                        
415300     CALL CBLTDLI USING PURG ALT4-PCB W-PROG-TO-PROG-SW                   
415400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
415500     PERFORM IMS-STATUSKONTROLL                                           
415600     SKIP3                                                                
415700     .                                                                    
415800 IMS-ISRT-WDF212 SECTION.                                                 
415900                                                                          
416000     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
416100          DELIMITED BY SIZE INTO SSA1                                     
416200     MOVE 'WDF212    ' TO SSA2                                            
416300     MOVE '  IINI' TO GODK-STATUSKODER                                    
416400     CALL CBLTDLI USING ISRT WDF2-PCB DLI-IO-WDF212 SSA1 SSA2             
416500     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
416600     PERFORM IMS-STATUSKONTROLL                                           
416700     .                                                                    
416800     EJECT                                                                
416900                                                                          
417000 IMS-GU-WDB601    SECTION.                                                
417100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
417200          DELIMITED BY SIZE INTO SSA1                                     
417300     MOVE '  GE'     TO GODK-STATUSKODER                                  
417400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
417500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
417600     PERFORM IMS-STATUSKONTROLL                                           
417700     .                                                                    
417800 IMS-GN-WDB601    SECTION.                                                
417900     MOVE 'WDB601  ' TO SSA1                                              
418000     MOVE '  GB'     TO GODK-STATUSKODER                                  
418100     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
418200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
418300     PERFORM IMS-STATUSKONTROLL                                           
418400     .                                                                    
418500 IMS-STATUSKONTROLL SECTION.                                              
418600     SET STATUS-IX TO 1                                                   
418700     SEARCH GODK-STATUS                                                   
418800       AT END                                                             
418900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
419000         DELIMITED BY SIZE INTO FELTEXT                                   
419100         CALL FELLOG                                                      
419200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
419300         CONTINUE                                                         
419400     END-SEARCH                                                           
419500     .                                                                    
