000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4021200.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE DATAKONSULT AB                    
000400 DATE-WRITTEN.   APRIL -90.                                               
000500                                                                          
000600*    FUNKTION.                                                            
000700*        PROGRAMMET HANTERAR UPPLÄGGNING AV ORDERRADER I                  
000800*        ORDERKÖN. REGISTRERADE VÄRDEN KONTROLLERAS OCH ÖVRIGA            
000900*        HÄMTAS FRÅN ARTIKELREGISTRET.                                    
001000*                                                                         
001100*        EFTER UPPLÄGGNING AV GODKÄNDA RADER SKER UTHOPP TILL             
001200*        SVARSBILD - 4213.                                                
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T212                                              
001600*                     W4T212U                                             
001700*                     W4T212V                                             
001800*        MID:         W4I21201                                            
001900*    UTDATA.                                                              
002000*        MOD:         W4O21201                                            
002100*                                                                         
002200*    E'TRACKER: 5444132 DATED 2007-08-27                                  
002300*    E'TRACKER: 7450328 DATED 2008 HÖST   VOHF                            
002400*    E'TRACKER: 8081720  DATED 2009-04-09  ORDER STEERING                 
002500*    E'TRACKER: 10254592      2015        DECOMISSION VOHF                
002600*    E'TRACKER: 10263222      2015        FORCE TO END ORDER REG          
002700*                                                                         
002800     EJECT                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300*    -- CHECKED BY WY2000                                                 
003400     SKIP3                                                                
003500 77  IDPGM                       PIC X(08)   VALUE 'W4021200'.            
003600 77  HOPP                        PIC X(1)   VALUE 'N'.                    
003700 77  HOPP-TILL-0504              PIC X(1)   VALUE 'N'.                    
003800 77  JA                          PIC X(1)   VALUE 'J'.                    
003810 77  YES                         PIC X(1)   VALUE 'Y'.                    
003820 77  NEJ                         PIC X(1)   VALUE 'N'.                    
003830*                                                                         
003840*    ---FOR MOD0504-IDTRANS                                               
003850 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
003860                                                                          
003870 01  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
003880*01  -COPY WWDCKONS                                                       
003890                                                                          
003900*01  -COPY WWPRODSL                                                       
004000                                                                          
004100 77  FELTEXT                     PIC X(64)  VALUE SPACE.                  
004200 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
004300 77  WS-PGM-POSITION             PIC X(24)  VALUE SPACE.                  
004400 77  RKOD-ABEND                  PIC S9(4)  VALUE +33   COMP SYNC.        
004500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
004510 77  IX-DCCLEAR-MAX              PIC S9(3)   COMP SYNC VALUE +99.         
004600 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
004700 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
004800 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +14.         
004900 77  WS-INDEX-TILLK              PIC S9(9)   COMP SYNC VALUE ZERO.        
005000 77  WS-INDEX-TILLK-MAX          PIC S9(9)   COMP SYNC VALUE +20.         
005100 77  WS-INDEX-LAGOMR             PIC S9(9)   COMP SYNC VALUE ZERO.        
005200 77  WS-INDEX-LAGOMR-MAX         PIC S9(9)   COMP SYNC VALUE +99.         
005300 77  WS-INDEX-WOPS               PIC S9(9)   COMP SYNC VALUE ZERO.        
005400 77  WS-INDEX-WOPS-MAX           PIC S9(9)   COMP SYNC VALUE +100.        
005500 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005600 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005700 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
005800 77  WS-RESLATT                  PIC S9(3)   VALUE +0  COMP-3.            
005900 77  WS-IDPRQUES                 PIC S9(7)   VALUE +0.                    
006000 77  SPAR-ORAD-KVSLATT           PIC S9(7)   VALUE +0  COMP-3.            
006100 77  W-KDORDBEK                  PIC S9(2)   VALUE +0.                    
006300 77  W-KDTPOTYP                  PIC S9      VALUE +0.                    
006400 77  DAGENS-DATUM                PIC S9(7)   VALUE +0  COMP-3.            
006500 77  WS-ADLAGOMR                 PIC S9(3)   VALUE ZERO COMP-3.           
006600 77  WS-ADGANG                   PIC S9(3)   VALUE ZERO COMP-3.           
006700 77  WS-ADPLATS                  PIC S9(5)   VALUE ZERO COMP-3.           
006900 77  WS-CLDC-IX                  PIC S9(5)   VALUE +1   COMP-3.           
007010 77  WS-SAVE-INDEX               PIC S9(9)   COMP SYNC VALUE ZERO.        
007020 77  IDDC-IX                     PIC S9(3)   VALUE ZERO COMP-3.           
007100 77  W-TILLK-DC                  PIC X(2)    VALUE SPACE.                 
007110                                                                          
007400     EJECT                                                                
007500                                                                          
007600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007700     88  ALLT-OK                             VALUE 'J'.                   
007800                                                                          
007900 77  TILLK-SW                    PIC X       VALUE 'N'.                   
008000     88  TILLKOMMANDE-RAD                    VALUE 'J'.                   
008100     88  EJ-TILLKOMMANDE-RAD                 VALUE 'N'.                   
008200                                                                          
008300 77  BIPA-SW                     PIC X       VALUE 'N'.                   
008400     88  BIPA-JA                             VALUE 'J'.                   
008500                                                                          
008600 77  KOLLA-ERS-SW                PIC X       VALUE 'N'.                   
008700     88  KOLLA-ERS                           VALUE 'J'.                   
008800                                                                          
008900 77  SVARSBILD-SW                PIC X       VALUE 'N'.                   
009000     88  SVARSBILD                           VALUE 'J'.                   
009100                                                                          
009200 77  OBKR-SW                     PIC X       VALUE 'N'.                   
009300     88  SKRIV-OBKR                          VALUE 'J'.                   
009400     88  OBKR-SKRIVEN                        VALUE 'S'.                   
009500                                                                          
009600 77  SW-DDGS-TPO-OBKR71          PIC X       VALUE 'N'.                   
009700     88  DDGS-TPO-OBKR71                     VALUE 'J'.                   
009800                                                                          
009900 77  EGET-CL-RAD-SW              PIC X       VALUE 'N'.                   
010000     88  EGET-CL-RAD                         VALUE 'J'.                   
010100                                                                          
010110 77  BAL-DC-FND-SW               PIC X       VALUE 'N'.                   
010120     88  BAL-DC-FND                          VALUE 'J'.                   
010130                                                                          
010140 77  TILLK-BAL-DC-FND-SW         PIC X       VALUE 'N'.                   
010150     88  TILLK-BAL-DC-FND                    VALUE 'J'.                   
010160                                                                          
010170 77  CDC-MOVE-SW                 PIC X       VALUE 'N'.                   
010180     88  CDC-MOVE                            VALUE 'J'.                   
010190                                                                          
010191 77  KDERS-CHAIN-SW              PIC X       VALUE 'N'.                   
010192     88  KDERS-CHAIN                         VALUE 'J'.                   
010193                                                                          
010200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010300     88  EGEN-MID                            VALUE '4212'.                
010400     88  GODK-MID                            VALUE '4211' '4212'          
010500                                                   '4213' '4214'.         
010600                                                                          
010700     SKIP2                                                                
010800 01  WS-TIHHMMSS                 PIC 9(6)   VALUE ZERO.                   
010900 01  FILLER REDEFINES WS-TIHHMMSS.                                        
011000     03 WS-TIHHMM                PIC 9(4).                                
011100     03 FILLER                   PIC 9(2).                                
011200                                                                          
011300 01  WS-TITIREGD-9KOMPL          PIC 9(8).                                
011400 01  FILLER REDEFINES WS-TITIREGD-9KOMPL.                                 
011500     03  WS-SEKEL-9KOMPL         PIC 9(2).                                
011600     03  WS-AAMMDD-9KOMPL        PIC 9(6).                                
011700                                                                          
011710 01  W-WORK-VAR.                                                          
011720     03 W-FLREFILL-MAIN          PIC X       VALUE SPACE.                 
011730     03 W-KDPRODSL-MAIN          PIC S9(3)   COMP-3 VALUE 0.              
011740     03 W-KDSORT-MAIN            PIC X(2)    VALUE SPACE.                 
011750     03 W-KVQPACK-1-MAIN         PIC S9(5)   COMP-3 VALUE 0.              
011760     03 W-REDIRLEV-MAIN          PIC S9V9(2) COMP-3 VALUE 0.              
011770     03 W-FLREFILL-REPL          PIC X       VALUE SPACE.                 
011780     03 W-KDPRODSL-REPL          PIC S9(3)   COMP-3 VALUE 0.              
011790     03 W-KDSORT-REPL            PIC X(2)    VALUE SPACE.                 
011791     03 W-KVQPACK-1-REPL         PIC S9(5)   COMP-3 VALUE 0.              
011792     03 W-REDIRLEV-REPL          PIC S9V9(2) COMP-3 VALUE 0.              
011793     03 W-IDARTNR-SDCA           PIC S9(9)   COMP-3 VALUE 0.              
011794     03 W-GMT-IDDC-CLEAR-GRP.                                             
011795*                                 GRUPP AV IDDC-CLEAR                     
011796        05 W-GMT-IDDC-CLEAR   OCCURS 99 TIMES                             
011797                                 PIC X(2)    VALUE SPACE.                 
011798                                                                          
011800     EJECT                                                                
011900                                                                          
012000 01  MESSAGE-CODES.                                                       
012100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012200     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
012300     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
012400     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
012500     03  ERR-STORT-UTTAG         PIC X(3)    VALUE '725'.                 
012600     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
012700     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
012800     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
012900     EJECT                                                                
013000                                                                          
013100 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
013200*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
013300     EJECT                                                                
013400*01  FILLER   -COPY WWDIST18    -RED TEST-IDDISTR.                        
013500     EJECT                                                                
013600*01  FILLER   -COPY WWDIST79    -RED TEST-IDDISTR.                        
013700*    ----DISTR-DEALER-PRICE----                                           
013800     EJECT                                                                
013900 01 FILLER                    PIC X(16) VALUE 'TILLKOMMANDE TAB'.         
014000                                                                          
014100*    -COPY W411TILK                                                       
014200     EJECT                                                                
014300                                                                          
014400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014500 01  GENERELLA-SUBPROGRAM.                                                
014600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015200*                                                                         
015300*                                                                         
015400 01  GEMENSAMMA-SUBPROGRAM.                                               
015500     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
015600*        PRISTILLÄMPNING                                                  
015700     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
015800*        HÄMTA PRISFRÅGENR                                                
015900     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
016000*        DEALER PRISFRÅGABEHANDLING                                       
016100     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
016200*        LÄSNING ARTIKELREGISTER                                          
016300     03  W411ARTM                PIC X(8)    VALUE 'W411ARTM'.            
016400*        NYUPPLÄGG AV TOM ROT PÅ WDK9                                     
016500     03  W411CDCA                PIC X(8)    VALUE 'W411CDCA'.            
016600*        KONTROLL PRELIMINÄRAVBOKNING-CDC                                 
016700     03  W411DLEV                PIC X(8)    VALUE 'W411DLEV'.            
016800*        KONTROLL DIREKTLEVERANS                                          
016900     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
017000*        DATA TILL DEL NOTE NDC                                           
017100     03  W411KAMP                PIC X(8)    VALUE 'W411KAMP'.            
017200*        KONTROLL TPO4 - KAMPANJ                                          
017300     03  W411KERS                PIC X(8)    VALUE 'W411KERS'.            
017400*        KONTROLL ERSÄTTNINGAR                                            
017500     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
017600*        KONTROLL KVANTANPASSNING                                         
017700     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
017800*        KONTROLL ENHETSLAST                                              
017900     03  W411ORFK                PIC X(8)    VALUE 'W411ORFK'.            
018000*        FORMELLA KONTROLLER AV INDATA                                    
018100     EJECT                                                                
018200     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
018300*        BERÄKNA RANSONERING                                              
018400     03  W411NDCA                PIC X(8)    VALUE 'W411NDCA'.            
018500*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
018600     03  W411XDCA                PIC X(8)    VALUE 'W411XDCA'.            
018700*        KONTROLL PRELIMINÄRAVBOKNING-NDC                                 
019700     03  W411SDCA                PIC X(8)    VALUE 'W411SDCA'.            
019800*        KONTROLL PRELIMINÄRAVBOKNING-SDC                                 
019900     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
020000*        KONTROLL SPÄRRAR                                                 
020100     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
020200*        KONTROLL STORA UTTAG                                             
020300     03  W411TPO1                PIC X(8)    VALUE 'W411TPO1'.            
020400*        KONTROLL TPO1                                                    
020500     03  W411TPO2                PIC X(8)    VALUE 'W411TPO2'.            
020600*        KONTROLL TPO2                                                    
020700     03  W411RELS                PIC X(8)    VALUE 'W411RELS'.            
020800*        KONTROLL RELS                                                    
021100     03  W411CLDC                PIC X(8)    VALUE 'W411CLDC'.            
021200*        WDB601-SEGMENT FÖR CLARING-DC                                    
021300     03  W413AVSR                PIC X(8)    VALUE 'W413AVSR'.            
021400*        WOPS RADBEHANDLING                                               
021500     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
021600*        OMVANDLING AV LAGOMR + PLATS                                     
021700     EJECT                                                                
021800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
021900*   -COPY WMEDAREA                                                        
022000     SKIP3                                                                
022100 01  FILLER.                                                              
022200   03  FELMEDD-AREA.                                                      
022300     05  FELMEDD-ENGLISH.                                                 
022400       10  FILLER                PIC X(50)                                
022500     VALUE '622 4212 NOT AVAILABLE ONLY VALID FROM 4211'.                 
022600     EJECT                                                                
022700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
022800*   -COPY WMSGINIT                                                        
022900     EJECT                                                                
023000*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
023100 01 FILLER                       PIC X(8) VALUE 'W335PRIS'.               
023200*   -COPY W335PRIS                                                        
023300     EJECT                                                                
023400 01 FILLER                       PIC X(8) VALUE 'W335PRNO'.               
023500*   -COPY W335PRNO                                                        
023600     EJECT                                                                
023700 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
023800*   -COPY W335PRQU                                                        
023900     EJECT                                                                
024000 01 FILLER                       PIC X(8) VALUE 'W411AREG'.               
024100*   -COPY W411AREG                                                        
024200     EJECT                                                                
024300 01 FILLER                       PIC X(8) VALUE 'W411ARTM'.               
024400*   -COPY W411ARTM                                                        
024500     EJECT                                                                
024600 01 FILLER                       PIC X(8) VALUE 'W411CDCA'.               
024700*   -COPY W411CDCA                                                        
024800     EJECT                                                                
024900 01 FILLER                       PIC X(8) VALUE 'W411DLEV'.               
025000*   -COPY W411DLEV                                                        
025100     EJECT                                                                
025200 01 FILLER                       PIC X(8) VALUE 'W411DNOT'.               
025300*   -COPY W411DNOT                                                        
025400     EJECT                                                                
025500 01 FILLER                       PIC X(8) VALUE 'W411KAMP'.               
025600*   -COPY W411KAMP                                                        
025700     EJECT                                                                
025800 01 FILLER                       PIC X(8) VALUE 'W411KERS'.               
025900*   -COPY W411KERS                                                        
026000     EJECT                                                                
026100 01 FILLER                       PIC X(8) VALUE 'W411KVAN'.               
026200*   -COPY W411KVAN                                                        
026300     EJECT                                                                
026400 01 FILLER                       PIC X(8) VALUE 'W411LAST'.               
026500*   -COPY W411LAST                                                        
026600     EJECT                                                                
026700 01 FILLER                       PIC X(8) VALUE 'W411ORFK'.               
026800*   -COPY W411ORFK                                                        
026900     EJECT                                                                
027000 01 FILLER                       PIC X(8) VALUE 'W411RANS'.               
027100*   -COPY W411RANS                                                        
027200     EJECT                                                                
027300 01 FILLER                       PIC X(8) VALUE 'W411NDCA'.               
027400*   -COPY W411NDCA                                                        
027500     EJECT                                                                
027510 01 FILLER                       PIC X(8) VALUE 'W411XDCA'.               
027520*   -COPY W411XDCA                                                        
027530     EJECT                                                                
027540 01 FILLER                       PIC X(8) VALUE 'W411XDK7'.               
027550*   -COPY W411XDK7 -PRE NDCA-                                             
027560     EJECT                                                                
027570 01 FILLER                       PIC X(8) VALUE 'W411SDCA'.               
027580*   -COPY W411SDCA                                                        
027590     EJECT                                                                
027600 01 FILLER                       PIC X(8) VALUE 'W411SPAR'.               
027700*   -COPY W411SPAR                                                        
027800     EJECT                                                                
027900 01 FILLER                       PIC X(8) VALUE 'W411STOR'.               
028000*   -COPY W411STOR                                                        
028100     EJECT                                                                
028200 01 FILLER                       PIC X(8) VALUE 'W411TPO1'.               
028300*   -COPY W411TPO1                                                        
028400     EJECT                                                                
028500 01 FILLER                       PIC X(8) VALUE 'W411TPO2'.               
028600*   -COPY W411TPO2                                                        
028700     EJECT                                                                
028800 01 FILLER                       PIC X(8) VALUE 'W411RELS'.               
028900*   -COPY W411RELS                                                        
029000     EJECT                                                                
029400 01 FILLER                       PIC X(8) VALUE 'W411CLDC'.               
029500*   -COPY W411CLDC                                                        
029600     EJECT                                                                
029700 01 FILLER                       PIC X(8) VALUE 'W413AVSR'.               
029800*   -COPY W413AVSR                                                        
029900     SKIP2                                                                
030000 01 FILLER                       PIC X(8) VALUE 'W413ADRS'.               
030100*   -COPY W413ADRS                                                        
030200     EJECT                                                                
030300     EJECT                                                                
030400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
030500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
030600     SKIP3                                                                
030700*01  MID -COPY W4I21201                                                   
030800     EJECT                                                                
030900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
031000     SKIP3                                                                
031100*01  -COPY WMSGAREA                                                       
031200     EJECT                                                                
031300*    03  MOD -COPY W4O21201   -RED MSG-AREA.                              
031400*   TO RETURN TO MAIN MENU                                                
031500*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
031600     EJECT                                                                
031700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
031800     SKIP3                                                                
031900*01  -COPY WMFSAREA                                                       
032000     EJECT                                                                
032100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
032200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
032300 01  NYCKLAR-TILL-DLI.                                                    
032400                                                                          
032500     03  W-IDGMTREF-X.                                                    
032600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
032700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
032800         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
032900                                                                          
033000     03  W-IDARTNR-X.                                                     
033100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
033200                                                                          
033300     03  W-IDARTNR-CROSS-X.                                               
033400         05  W-IDARTNR-CROSS     PIC S9(9)   VALUE ZERO COMP-3.           
033500                                                                          
033510     03  W-IDLEVNR-X.                                                     
033520         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
033530                                                                          
033540     03  W-WDQ101KY-MIN-X.                                                
033550         05  W-IDORDER-Q1-MIN    PIC S9(7)   COMP-3.                      
033560         05  W-IDARTNR-Q1-MIN    PIC S9(9)   COMP-3.                      
033570         05  W-IDLOPNR-Q1-MIN    PIC S9(3)   COMP-3.                      
033580         05  W-IDSEKVNR-Q1-MIN   PIC S9(3)   COMP-3.                      
033590         05  FILLER              PIC X(04)   VALUE LOW-VALUE.             
033600                                                                          
033700     03  W-WDQ101KY-MAX-X.                                                
033800         05  W-IDORDER-Q1-MAX    PIC S9(7)   COMP-3.                      
033900         05  W-IDARTNR-Q1-MAX    PIC S9(9)   COMP-3.                      
034000         05  W-IDLOPNR-Q1-MAX    PIC S9(3)   COMP-3.                      
034100         05  W-IDSEKVNR-Q1-MAX   PIC S9(3)   COMP-3.                      
034200         05  FILLER              PIC X(04)   VALUE HIGH-VALUE.            
034300                                                                          
034400     03  W-IDGMT-X.                                                       
034500         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
034600         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
034700*                                                                         
035600     03  W-WDB101KY-X.                                                    
035700         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
035800         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
035900                                                                          
036000     03  W-IDDC-B6-X.                                                     
036100         05 W-IDDC-B6                  PIC X(2).                          
036200*                                                                         
036300     03  W-WDB301KY-X.                                                    
036400         05  W-IDDC-WDB3          PIC X(2)    VALUE SPACE.                
036500         05  W-IDDISTR-WDB3       PIC S9(5)   COMP-3 VALUE ZERO.          
036600         05  W-IDKUNDNR-WDB3      PIC S9(7)   COMP-3 VALUE ZERO.          
036700*                                                                         
036800     03  W-WDB301KY-DEF-X.                                                
036900         05  W-IDDC-WDB3-DEF      PIC X(2)    VALUE SPACE.                
037000         05  W-IDDISTR-WDB3-DEF   PIC S9(5)   COMP-3 VALUE ZERO.          
037100         05  W-IDKUNDNR-WDB3-DEF  PIC S9(7) VALUE +9999999 COMP-3.        
037200     EJECT                                                                
037300                                                                          
037400*    --- STATUS-KOD FRÅN IMS                                              
037500 01  STATUS-WS                   PIC XX.                                  
037600     88  SEGMENT-FINNS                       VALUE '  '.                  
037700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
037800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
037900     88  BASEN-SLUT                          VALUE 'GB'.                  
038000     SKIP2                                                                
038100 01  GODK-STATUSKODER.                                                    
038200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
038300                                                                          
038400 01  SSA1                        PIC X(96).                               
038500 01  SSA2                        PIC X(64).                               
038600     EJECT                                                                
038700*    --- IMS FUNKTIONSKODER                                               
038800*01  -COPY W0003                                                          
038900     EJECT                                                                
039000*    ---  DLI INPUT-OUTPUT AREA                                           
039100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
039200     SKIP3                                                                
039300 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
039400 01  DLI-IO-AREA-OBKR.                                                    
039500     03  WDQ101.                                                          
039600*        05  -COPY WDQ101                                                 
039700     EJECT                                                                
039800 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
039900 01  DLI-IO-AREA-OHUV.                                                    
040000     03  WDQ201.                                                          
040100*        05  -COPY WDQ201                                                 
040200     EJECT                                                                
040300 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
040400 01  DLI-IO-AREA-ARB.                                                     
040500     03  WDQ212.                                                          
040600*        05  -COPY WDQ212                                                 
040700     EJECT                                                                
040800 01  FILLER                      PIC X(16)   VALUE 'WDQ401-AREA'.         
040900 01  DLI-IO-AREA-ORAD.                                                    
041000     03  WDQ401.                                                          
041100*        05  -COPY WDQ401                                                 
041200     EJECT                                                                
041300 01  FILLER                      PIC X(16)   VALUE 'WDK901-AREA'.         
041400 01  DLI-IO-AREA-ART.                                                     
041500     03  WLARTM01.                                                        
041600*        05  -COPY WDK901                                                 
041700     EJECT                                                                
041800 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
041900 01  DLI-IO-AREA-WDB201.                                                  
042100*    03  -COPY WDB201                                                     
042200     EJECT                                                                
042300 01  FILLER                      PIC X(16)   VALUE 'WDB101-AREA'.         
042400 01  DLI-IO-AREA-WDB101.                                                  
042500     03  WLBETC01.                                                        
042600         05  -COPY WDB101                                                 
042700                                                                          
042800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
042900 01   DLI-IO-AREA-B601.                                                   
043000*     03  -COPY WDB601                                                    
043100                                                                          
043200 01  FILLER               PIC X(16)   VALUE 'WDB301 AREA'.                
043300 01   DLI-IO-AREA-WDB301.                                                 
043400*     03  -COPY WDB301                                                    
043500                                                                          
043600 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDF502'.          
043700 01  DLI-IO-WDF502.                                                       
043800*    03  -COPY WDF502                                                     
043900                                                                          
044000 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
044010 01   DLI-IO-AREA-R601.                                                   
044020*     03  -COPY WDR601                                                    
044030*     05  -COPY W414XDCA      -RED FIL-WDR601-DATA                        
044040     EJECT                                                                
044050 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
044060 01  4213-MSG-IO-AREA.                                                    
044070     03  4213-LL               PIC S9(4)  VALUE +69  COMP SYNC.           
044080     03  4213-Z1               PIC X.                                     
044090     03  4213-Z2               PIC X.                                     
044100     03  4213-TRANSKOD         PIC X(8)   VALUE 'W4T213  '.               
044200     03  4213-IDTRANS          PIC X(4)   VALUE '4212'.                   
044300     03  4213-SPRAK            PIC X.                                     
044400     03  4213-IDDISTR-IN       PIC X(4).                                  
044500     03  4213-IDKUNDNR-IN      PIC X(6).                                  
044600     03  4213-IDORDNR-IN       PIC X(5).                                  
044700     03  4213-IDDISTR-UT       PIC X(4).                                  
044800     03  4213-IDKUNDNR-UT      PIC X(6).                                  
044900     03  4213-IDORDNR-UT       PIC X(5).                                  
045000     03  4213-KDORDKL-UT       PIC X      VALUE SPACE.                    
045100     03  4213-FLANNULL         PIC X(1)   VALUE 'N'.                      
045200     03  FILLER                PIC X(20)  VALUE ZERO.                     
045300     EJECT                                                                
045400 01  4297-MSG-IO-AREA.                                                    
045500     03  4297-LL               PIC S9(4)  VALUE +0   COMP SYNC.           
045600     03  4297-Z1               PIC X.                                     
045700     03  4297-Z2               PIC X.                                     
045800     03  4297-TRANSKOD         PIC X(8)   VALUE 'W4T297X '.               
045900     03  4297-IDTRANS          PIC X(4)   VALUE '4212'.                   
046000     03  4297-SPRAK            PIC X.                                     
046100*    03  -COPY W4I29701  -PRE 4297-                                       
046200     EJECT                                                                
046300 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
046400     SKIP3                                                                
046500 01  -COPY WZ01SEND                                                       
046600     EJECT                                                                
046700 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
046800     SKIP3                                                                
046900 01  SEND-AREA.                                                           
047000*    03  -COPY WZ01REQU  -PRE 3039-                                       
047100*    03  -COPY W30391I1  -PRE 3039-                                       
047200     EJECT                                                                
047300 LINKAGE SECTION.                                                         
047400                                                                          
047500*01  -COPY W0009   -PRE MSG-                                              
047600     SKIP2                                                                
047700 01  2109-PCB                    PIC X.                                   
047800                                                                          
047900 01  AVSR-ALT-PCB                PIC X.                                   
048000     EJECT                                                                
048100*01  -COPY W0009   -PRE 4213-                                             
048200     SKIP2                                                                
048300*01  -COPY W0009   -PRE 4213V-                                            
048400     EJECT                                                                
048500*01  -COPY W0009   -PRE 4297-                                             
048600     EJECT                                                                
048700*01  -COPY W0009   -PRE PRQRY-                                            
048800     SKIP2                                                                
048900 01  USEA-PCB                    PIC X.                                   
049000     EJECT                                                                
049100*01  -COPY W0008   -PRE WDQ1-                                             
049200     05  FILLER                  PIC X.                                   
049300     SKIP2                                                                
049400*01  -COPY W0008   -PRE WDQ2-                                             
049500     05  FILLER                  PIC X.                                   
049600     EJECT                                                                
049700*01  -COPY W0008   -PRE WDQ4-                                             
049800     05  FILLER                  PIC X.                                   
049900     SKIP2                                                                
050000*01  -COPY W0008   -PRE ARTM-                                             
050100     05  FILLER                  PIC X.                                   
050200     EJECT                                                                
050300*01  -COPY W0008   -PRE WDB2-                                             
050400     05  FILLER                  PIC X.                                   
050500     EJECT                                                                
050600*01  -COPY W0008   -PRE WDB1-                                             
050700     05  FILLER                  PIC X.                                   
050800     EJECT                                                                
050900*01  -COPY W0008   -PRE WDB6-                                             
051000     05  FILLER                  PIC X.                                   
051100     EJECT                                                                
051200*01  -COPY W0008   -PRE WDB3-                                             
051300     05  FILLER                  PIC X.                                   
051400     EJECT                                                                
051500*01  -COPY W0008   -PRE WDF5-                                             
051600     05  FILLER                  PIC X.                                   
051700     EJECT                                                                
051800*01  -COPY W0008   -PRE WDR6-                                             
051900     05  FILLER                  PIC X.                                   
052000     EJECT                                                                
052100 01  PRIS-ARTC-PCB               PIC X.                                   
052200 01  PRIS-WDK7-PCB               PIC X.                                   
052300 01  PRIS-GMTA-PCB               PIC X.                                   
052400 01  PRIS-BETA-PCB               PIC X.                                   
052500 01  PRIS-GPRIA-PCB              PIC X.                                   
052600 01  PRIS-GPRIB-PCB              PIC X.                                   
052700 01  PRIS-COST-WDK6-PCB          PIC X.                                   
052800 01  PRIS-COST-WDK7-PCB          PIC X.                                   
052900 01  PRIS-COST-WDF1-PCB          PIC X.                                   
053000 01  PRIS-COST-9305-PCB          PIC X.                                   
053100 01  PRIS-COST-WDK72-PCB         PIC X.                                   
053200 01  PRIS-COST-WDB6-PCB          PIC X.                                   
053300 01  PRNO-3107-PCB               PIC X.                                   
053400 01  PRQU-WDG2-PCB               PIC X.                                   
053500 01  PRQU-WDC7-PCB               PIC X.                                   
053600 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
053700 01  AREG-WDK6-PCB               PIC X.                                   
053800 01  AREG-WDK7-PCB               PIC X.                                   
053900 01  ARTM-ARTM-PCB               PIC X.                                   
054000 01  DLEV-LEVF-PCB               PIC X.                                   
054100 01  DLEV-LEVG-PCB               PIC X.                                   
054200 01  DLEV-LEVA-PCB               PIC X.                                   
054300 01  DLEV-ARTS-PCB               PIC X.                                   
054400 01  SPAR-WDF8-PCB               PIC X.                                   
054500 01  SPAR-WDF8A-PCB              PIC X.                                   
054600 01  SPAR-WDK6-PCB               PIC X.                                   
054700 01  DNOT-ORQP-PCB               PIC X.                                   
054800 01  DNOT-ORQP2-PCB              PIC X.                                   
054900 01  DNOT-ORQP3-PCB              PIC X.                                   
055000 01  DNOT-4013-PCB               PIC X.                                   
055100 01  DNOT-BENA-PCB               PIC X.                                   
055200 01  KAMP-ORDP-PCB               PIC X.                                   
055300 01  KAMP-ZZAC-PCB               PIC X.                                   
055400 01  KAMP-WDM2-PCB               PIC X.                                   
055500 01  KERS-ARTC-PCB               PIC X.                                   
055600 01  KERS-ERSA-PCB               PIC X.                                   
055700 01  NDCA-USEA-PCB               PIC X.                                   
055800 01  NDCA-WDK7-PCB               PIC X.                                   
055900 01  NDCA-WDL6-PCB               PIC X.                                   
056000 01  NDCA-WDB6-PCB               PIC X.                                   
056200 01  SDCA-ARTS-PCB               PIC X.                                   
056300 01  SDCA-WDB6-PCB               PIC X.                                   
056400 01  SDCA-WDK9-PCB               PIC X.                                   
056410 01  SDCA-WDR6-PCB               PIC X.                                   
056420 01  SDCA-WDK6-PCB               PIC X.                                   
056430 01  SDCA-WDQ4B-PCB              PIC X.                                   
056440 01  SDCA-WDQ2-PCB               PIC X.                                   
056450 01  SDCA-WDQ4-PCB               PIC X.                                   
056451 01  SDCA-WDB6-2-PCB             PIC X.                                   
056452 01  SDCA-WDK6-2-PCB             PIC X.                                   
056453 01  SDCA-WDK7-2-PCB             PIC X.                                   
056454 01  SDCA-WDK7-3-PCB             PIC X.                                   
056460 01  CDCA-ARTM-PCB               PIC X.                                   
056470 01  CDCA-INLB-PCB               PIC X.                                   
056480 01  CDCA-WDB2-PCB               PIC X.                                   
056490 01  CDCA-WDC1-PCB               PIC X.                                   
056500 01  RANS-XXKM-PCB               PIC X.                                   
056600 01  RANS-ARTM-PCB               PIC X.                                   
056700 01  RANS-ARTS-PCB               PIC X.                                   
056800     EJECT                                                                
056900 01  TPO1-ORDP-PCB               PIC X.                                   
057000 01  TPO1-ARTM-PCB               PIC X.                                   
057100 01  TPO1-ZZAC-PCB               PIC X.                                   
057200 01  TPO2-ORDP-PCB               PIC X.                                   
057300 01  TPO2-XXBU-PCB               PIC X.                                   
057400 01  TPO2-XXBV-PCB               PIC X.                                   
057500 01  TPO2-ARTM-PCB               PIC X.                                   
057600 01  TPO2-FILA-PCB               PIC X.                                   
057700 01  TPO2-XXBX-PCB               PIC X.                                   
057800 01  RELS-ORDP-PCB               PIC X.                                   
057900 01  RELS-FILA-PCB               PIC X.                                   
058000 01  RELS-ARTM-PCB               PIC X.                                   
058100 01  TIME-4437-PCB               PIC X.                                   
058200 01  AVSR-ORQI-PCB               PIC X.                                   
058300 01  AVSR-GMTB-PCB               PIC X.                                   
058400 01  AVSR-GMTC-PCB               PIC X.                                   
058500 01  AVSR-WDB2-PCB               PIC X.                                   
058600 01  AVSR-WDB6-PCB               PIC X.                                   
058700 01  TRAN-XXKB-PCB               PIC X.                                   
058800 01  KVAN-WDB2-PCB               PIC X.                                   
058900 01  KVAN-WDC1-PCB               PIC X.                                   
059000 01  XDCA-USEA-PCB               PIC X.                                   
059100 01  XDCA-WDB6-PCB               PIC X.                                   
059200 01  XDCA-WDK6-PCB               PIC X.                                   
059300 01  XDCA-WDK7-PCB               PIC X.                                   
059310 01  XDCA-WDK9-PCB               PIC X.                                   
059320 01  XDCA-WDL6-PCB               PIC X.                                   
059330 01  XDCA-WDQ4B-PCB              PIC X.                                   
059340 01  XDCA-WDQ2-PCB               PIC X.                                   
059350 01  XDCA-WDQ4-PCB               PIC X.                                   
059360 01  XDCA-WDR6-PCB               PIC X.                                   
059361 01  XDCA-WDB6-2-PCB             PIC X.                                   
059362 01  XDCA-WDK6-2-PCB             PIC X.                                   
059363 01  XDCA-WDK7-2-PCB             PIC X.                                   
059364 01  XDCA-WDK7-3-PCB             PIC X.                                   
059370     EJECT                                                                
059380 PROCEDURE DIVISION  USING                                                
059390        MSG-PCB         AVSR-ALT-PCB    2109-PCB   PRQRY-PCB              
059400        4213-PCB        4213V-PCB       4297-PCB                          
059500        USEA-PCB        WDQ1-PCB        WDQ2-PCB                          
059600        WDQ4-PCB        ARTM-PCB                                          
059700        WDB2-PCB        WDB1-PCB        WDB6-PCB   WDB3-PCB               
059800        WDF5-PCB        WDR6-PCB                                          
059900        PRIS-ARTC-PCB   PRIS-WDK7-PCB                                     
060000        PRIS-GMTA-PCB   PRIS-BETA-PCB                                     
060100        PRIS-GPRIA-PCB  PRIS-GPRIB-PCB                                    
060200        PRIS-COST-WDK6-PCB                                                
060300        PRIS-COST-WDK7-PCB                                                
060400        PRIS-COST-WDF1-PCB                                                
060500        PRIS-COST-9305-PCB                                                
060600        PRIS-COST-WDK72-PCB                                               
060700        PRIS-COST-WDB6-PCB                                                
060800        PRNO-3107-PCB                                                     
060900        PRQU-WDG2-PCB                                                     
061000        PRQU-WDC7-PCB                                                     
061100        PRQU-SJKO-WDK6-PCB                                                
061200        AREG-WDK6-PCB                                                     
061300        AREG-WDK7-PCB                                                     
061400        ARTM-ARTM-PCB                                                     
061500        DLEV-LEVF-PCB                                                     
061600        DLEV-LEVG-PCB                                                     
061700        DLEV-LEVA-PCB                                                     
061800        DLEV-ARTS-PCB                                                     
061900        SPAR-WDF8-PCB                                                     
062000        SPAR-WDF8A-PCB                                                    
062100        SPAR-WDK6-PCB                                                     
062200        DNOT-ORQP-PCB                                                     
062300        DNOT-ORQP2-PCB                                                    
062400        DNOT-ORQP3-PCB                                                    
062500        DNOT-4013-PCB                                                     
062600        DNOT-BENA-PCB                                                     
062700        KAMP-ORDP-PCB   KAMP-ZZAC-PCB   KAMP-WDM2-PCB                     
062800        KERS-ARTC-PCB   KERS-ERSA-PCB                                     
062900        NDCA-USEA-PCB   NDCA-WDK7-PCB  NDCA-WDL6-PCB NDCA-WDB6-PCB        
063100        SDCA-ARTS-PCB   SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB         
063200        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
063210        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
063220        SDCA-WDK7-3-PCB                                                   
063300        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
063400        RANS-XXKM-PCB   RANS-ARTM-PCB   RANS-ARTS-PCB                     
063500        TPO1-ORDP-PCB   TPO1-ARTM-PCB   TPO1-ZZAC-PCB                     
063600        TPO2-ORDP-PCB   TPO2-XXBU-PCB   TPO2-XXBV-PCB                     
063700        TPO2-ARTM-PCB   TPO2-FILA-PCB   TPO2-XXBX-PCB                     
063800        RELS-ORDP-PCB   RELS-FILA-PCB   RELS-ARTM-PCB                     
063900        TIME-4437-PCB                                                     
064000        AVSR-ORQI-PCB   AVSR-GMTB-PCB   AVSR-GMTC-PCB                     
064100        AVSR-WDB2-PCB   AVSR-WDB6-PCB                                     
064200        TRAN-XXKB-PCB   KVAN-WDB2-PCB KVAN-WDC1-PCB                       
064210        XDCA-USEA-PCB                                                     
064300        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
064400        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
064410        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
064411        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
064412        XDCA-WDK7-3-PCB.                                                  
064420     EJECT                                                                
064430                                                                          
064440     ENTRY 'DLITCBL' USING                                                
064450        MSG-PCB         AVSR-ALT-PCB    2109-PCB   PRQRY-PCB              
064460        4213-PCB        4213V-PCB       4297-PCB                          
064470        USEA-PCB        WDQ1-PCB        WDQ2-PCB                          
064480        WDQ4-PCB        ARTM-PCB                                          
064490        WDB2-PCB        WDB1-PCB        WDB6-PCB   WDB3-PCB               
064500        WDF5-PCB        WDR6-PCB                                          
064600        PRIS-ARTC-PCB   PRIS-WDK7-PCB                                     
064700        PRIS-GMTA-PCB   PRIS-BETA-PCB                                     
064800        PRIS-GPRIA-PCB  PRIS-GPRIB-PCB                                    
064900        PRIS-COST-WDK6-PCB                                                
065000        PRIS-COST-WDK7-PCB                                                
065100        PRIS-COST-WDF1-PCB                                                
065200        PRIS-COST-9305-PCB                                                
065300        PRIS-COST-WDK72-PCB                                               
065400        PRIS-COST-WDB6-PCB                                                
065500        PRNO-3107-PCB                                                     
065600        PRQU-WDG2-PCB                                                     
065700        PRQU-WDC7-PCB                                                     
065800        PRQU-SJKO-WDK6-PCB                                                
065900        AREG-WDK6-PCB                                                     
066000        AREG-WDK7-PCB                                                     
066100        ARTM-ARTM-PCB                                                     
066200        DLEV-LEVF-PCB                                                     
066300        DLEV-LEVG-PCB                                                     
066400        DLEV-LEVA-PCB                                                     
066500        DLEV-ARTS-PCB                                                     
066600        SPAR-WDF8-PCB                                                     
066700        SPAR-WDF8A-PCB                                                    
066800        SPAR-WDK6-PCB                                                     
066900        DNOT-ORQP-PCB                                                     
067000        DNOT-ORQP2-PCB                                                    
067100        DNOT-ORQP3-PCB                                                    
067200        DNOT-4013-PCB                                                     
067300        DNOT-BENA-PCB                                                     
067400        KAMP-ORDP-PCB   KAMP-ZZAC-PCB   KAMP-WDM2-PCB                     
067500        KERS-ARTC-PCB   KERS-ERSA-PCB                                     
067600        NDCA-USEA-PCB   NDCA-WDK7-PCB  NDCA-WDL6-PCB NDCA-WDB6-PCB        
067800        SDCA-ARTS-PCB SDCA-WDB6-PCB SDCA-WDK9-PCB SDCA-WDR6-PCB           
067900        SDCA-WDK6-PCB SDCA-WDQ4B-PCB SDCA-WDQ2-PCB SDCA-WDQ4-PCB          
067910        SDCA-WDB6-2-PCB SDCA-WDK6-2-PCB SDCA-WDK7-2-PCB                   
067920        SDCA-WDK7-3-PCB                                                   
068000        CDCA-ARTM-PCB CDCA-INLB-PCB CDCA-WDB2-PCB CDCA-WDC1-PCB           
068100        RANS-XXKM-PCB   RANS-ARTM-PCB   RANS-ARTS-PCB                     
068200        TPO1-ORDP-PCB   TPO1-ARTM-PCB   TPO1-ZZAC-PCB                     
068300        TPO2-ORDP-PCB   TPO2-XXBU-PCB   TPO2-XXBV-PCB                     
068400        TPO2-ARTM-PCB   TPO2-FILA-PCB   TPO2-XXBX-PCB                     
068500        RELS-ORDP-PCB   RELS-FILA-PCB   RELS-ARTM-PCB                     
068600        TIME-4437-PCB                                                     
068700        AVSR-ORQI-PCB   AVSR-GMTB-PCB   AVSR-GMTC-PCB                     
068800        AVSR-WDB2-PCB   AVSR-WDB6-PCB                                     
068900        TRAN-XXKB-PCB   KVAN-WDB2-PCB KVAN-WDC1-PCB                       
068910        XDCA-USEA-PCB                                                     
069000        XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                         
069100        XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                        
069200        XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                         
069210        XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                   
069220        XDCA-WDK7-3-PCB.                                                  
069300     EJECT                                                                
069400                                                                          
069500     PERFORM IMS-GET-MSG                                                  
069600     IF SEGMENT-FINNS                                                     
069700        PERFORM A-INIT                                                    
069800        PERFORM B-KOLLA-NYCKLAR                                           
069900        IF ALLT-OK                                                        
070000           PERFORM C-KOLLA-ATT-ORDER-FINNS                                
070100           IF ALLT-OK                                                     
070200              PERFORM D-FORMELL-KONTROLL                                  
070300              IF ALLT-OK                                                  
070400                 PERFORM E-BEHANDLA-RADER                                 
070500              END-IF                                                      
070600           END-IF                                                         
070700        END-IF                                                            
070800        IF ALLT-OK                                                        
070900          IF SVARSBILD  OR                                                
071000                         MID-IDARTNR(WS-INDEX-MID-MAX) = ALL '+'          
071100            IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0               
071200              PERFORM I-SKICKA-PRISFRAGA                                  
071300            END-IF                                                        
071400            IF BIPA-JA AND NOT SVARSBILD                                  
071500              PERFORM H-STARTA-BIPACKNINGEN                               
071600            ELSE                                                          
071700              PERFORM F-HOPPA-TILL-SVARSBILD                              
071800            END-IF                                                        
071900          ELSE                                                            
072000            IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0               
072100              PERFORM I-SKICKA-PRISFRAGA                                  
072200            END-IF                                                        
072300            PERFORM G-VISA-TOM-SIDA                                       
072400          END-IF                                                          
072500        END-IF                                                            
072600        IF HOPP = NEJ                                                     
072700        AND HOPP-TILL-0504     = NEJ                                      
072800          PERFORM Z-FINIT-INSERT-MSG                                      
072900        END-IF                                                            
073000     END-IF                                                               
073100     MOVE +0 TO RETURN-CODE                                               
073200     GOBACK                                                               
073300     .                                                                    
073400     EJECT                                                                
073500 A-INIT SECTION.                                                          
073600                                                                          
073700     MOVE 'STA A-SEC'                     TO   WS-PGM-POSITION            
073800     MOVE SPACE                TO MED-IDMFSFEL                            
073900                                                                          
074000     IF MSG-DUBBLA-TRANSKODER                                             
074100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I21201                 
074200       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
074300       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
074400     ELSE                                                                 
074500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I21201                  
074600       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
074700       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
074800     END-IF                                                               
074900     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
075000     MOVE MSG-IDPFK            TO MFS-IDPFK                               
075100     MOVE MFS-IDTRANS          TO W-IDTRANS                               
075200     IF W-IDTRANS = '4213' AND MSG-KDTRANS-1  = 'W4T212U '                
075300        MOVE JA TO SVARSBILD-SW                                           
075400     END-IF                                                               
075500     MOVE LOW-VALUE            TO MSG-AREA                                
075600     MOVE 'W4O212N1'           TO MFS-IDMOD                               
075700     MOVE '4212'               TO MOD-IDTRANS                             
075800     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
075900                                  MOD-TEMFSINF                            
076400                                                                          
076500     IF NOT EGEN-MID  AND  NOT SVARSBILD                                  
076600       MOVE NEJ                TO ALLT-SW                                 
076700       PERFORM S20-WRONG-PICTURE-MESSAGE                                  
076800     END-IF                                                               
076900     IF ENGLISH-TEXT                                                      
077000       MOVE 'GB '              TO MED-IDSKYLT                             
077100     ELSE                                                                 
077200       MOVE 'S  '              TO MED-IDSKYLT                             
077300     END-IF                                                               
077400                                                                          
077900     PERFORM AA-NOLLA-WOPS-TABELL                                         
078000                                                                          
078100     .                                                                    
078200     EJECT                                                                
078300 AA-NOLLA-WOPS-TABELL SECTION.                                            
078400                                                                          
078500     MOVE +1                   TO WS-INDEX-WOPS                           
078600     PERFORM UNTIL WS-INDEX-WOPS > WS-INDEX-WOPS-MAX                      
078700        MOVE +0                TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
078800        MOVE SPACE             TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
078900        MOVE SPACE             TO AVSR-IDDC(WS-INDEX-WOPS)                
079000        MOVE ZERO              TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
079100        MOVE +0                TO AVSR-KVANNANT(WS-INDEX-WOPS)            
079200        MOVE +0                TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
079300        MOVE +0                TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
079400        MOVE +0                TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
079500        INITIALIZE             AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)           
079600        MOVE +0                TO AVSR-VKART(WS-INDEX-WOPS)               
079700        MOVE +0                TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
079800        MOVE SPACE             TO AVSR-KDORDSTA(WS-INDEX-WOPS)            
079900        MOVE +0                TO AVSR-KDVIA   (WS-INDEX-WOPS)            
080000        MOVE +0                TO AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
080100        MOVE +0                TO AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
080200        MOVE +0                TO AVSR-KDVSOP(WS-INDEX-WOPS)              
080300                                  AVSR-KDFARLIG(WS-INDEX-WOPS)            
080400        ADD +1                 TO WS-INDEX-WOPS                           
080500     END-PERFORM                                                          
080600                                                                          
080700     MOVE +1                   TO WS-INDEX-WOPS                           
080800     .                                                                    
080900     EJECT                                                                
081000 B-KOLLA-NYCKLAR SECTION.                                                 
081100                                                                          
081200     MOVE 'STA B-KOLLA'                   TO   WS-PGM-POSITION            
081300                                                                          
081400     MOVE MID-IDDISTR          TO WS-IDDISTR                              
081500     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
081600                                                                          
081700     MOVE MID-IDKUNDNR         TO WS-IDKUNDNR                             
081800     INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                  
081900                                                                          
082000     MOVE MID-IDORDNR5         TO WS-IDORDNR                              
082100     INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                   
082200                                                                          
082300     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
082400        MOVE WS-IDDISTR           TO W-IDDISTR                            
082500     ELSE                                                                 
082600        MOVE NEJ                  TO ALLT-SW                              
082700        MOVE ZERO                 TO WS-IDDISTR                           
082800     END-IF                                                               
082900                                                                          
083000     MOVE WS-IDDISTR              TO TEST-IDDISTR                         
083100     IF DIST79-DEALER-PRICE                                               
083200        IF ENGLISH-TEXT                                                   
083300           MOVE 'DEALERPRICE'   TO MOD-TEDDI                              
083400        ELSE                                                              
083500           MOVE '    ÅF PRIS'   TO MOD-TEDDI                              
083600        END-IF                                                            
083700     ELSE                                                                 
083800        MOVE SPACES               TO MOD-TEDDI                            
083900     END-IF                                                               
084000                                                                          
084100     IF WS-IDKUNDNR NUMERIC                                               
084200        MOVE WS-IDKUNDNR          TO W-IDKUNDNR                           
084300     ELSE                                                                 
084400        MOVE NEJ                  TO ALLT-SW                              
084500     END-IF                                                               
084600                                                                          
084700     IF WS-IDORDNR NUMERIC AND WS-IDORDNR > ZERO                          
084800        MOVE '0000000   '         TO W-IDKUNDRF                           
084900        MOVE WS-IDORDNR           TO W-IDKUNDRF(3:5)                      
085000     ELSE                                                                 
085100        MOVE NEJ                  TO ALLT-SW                              
085200     END-IF                                                               
085300                                                                          
085400     IF NOT ALLT-OK                                                       
085500        IF SVARSBILD                                                      
085600          MOVE 'FEL NYCKLAR FÅR EJ INTRÄFFA VID START FRÅN 4213'          
085700                                  TO FELTEXT                              
085800          CALL ABEND USING RKOD-ABEND                                     
085900        END-IF                                                            
086000        MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                         
086100     END-IF                                                               
086200     EJECT                                                                
086300                                                                          
086400     IF GODK-MID OR ALLT-OK                                               
086500       MOVE WS-IDDISTR              TO MOD-IDDISTR                        
086600       INSPECT MOD-IDDISTR REPLACING LEADING ZERO BY SPACE                
086700                                                                          
086800       IF WS-IDKUNDNR = ZERO                                              
086900         MOVE '     0'              TO MOD-IDKUNDNR                       
087000       ELSE                                                               
087100         MOVE WS-IDKUNDNR           TO MOD-IDKUNDNR                       
087200         INSPECT MOD-IDKUNDNR REPLACING LEADING ZERO BY SPACE             
087300       END-IF                                                             
087400                                                                          
087500       MOVE WS-IDORDNR              TO MOD-IDORDNR5                       
087600       INSPECT MOD-IDORDNR5 REPLACING LEADING ZERO BY SPACE               
087700                                                                          
087800       IF W-IDTRANS = '4211' AND NOT ALLT-OK                              
087900          MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR                        
088000                                       MOD-IDKUNDNR                       
088100                                       MOD-IDORDNR5                       
088200       END-IF                                                             
088300                                                                          
088400     ELSE                                                                 
088500       MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR                        
088600                                       MOD-IDKUNDNR                       
088700                                       MOD-IDORDNR5                       
088800     END-IF                                                               
088900     .                                                                    
089000     EJECT                                                                
089100                                                                          
089200 C-KOLLA-ATT-ORDER-FINNS SECTION.                                         
089300                                                                          
089400     MOVE 'STA C-KOLLA'                   TO   WS-PGM-POSITION            
089500     PERFORM IMS-01-GHU-WDQ2-WDQ201                                       
089600     IF SEGMENT-FINNS                                                     
089700                                                                          
089800        IF OHUV-FLKLAR = JA                                               
089900           MOVE ERR-ORDER-AVSLUTAD   TO MED-IDMFSFEL                      
090000           MOVE NEJ                  TO ALLT-SW                           
090100        ELSE                                                              
090200           IF OHUV-IDSYSTEM NOT  = '4211'                                 
090400              MOVE ERR-FEL-BILDSERIE TO MED-IDMFSFEL                      
090500              MOVE NEJ               TO ALLT-SW                           
090600           STRING OHUV-IDSYSTEM                                           
090700           DELIMITED BY SIZE INTO MOD-TEMFSINF                            
090800           ELSE                                                           
090900              IF OHUV-IDUSER NOT = MSG-SIGNON-USERID                      
091000                 MOVE ERR-OBEHORIG      TO MED-IDMFSFEL                   
091100                 MOVE NEJ               TO ALLT-SW                        
091200              ELSE                                                        
091300                 MOVE OHUV-KDORDKL      TO MOD-KDORDKL                    
091400                 PERFORM IMS-03-GNP-WDQ2-WDQ212                           
091500                 MOVE ARB-KDFRAKT       TO MOD-KDFRAKT                    
091600                                                                          
091610                 PERFORM CA-HAMTA-KUND                                    
091620                 PERFORM CB-FIXA-LOKAL-TID                                
091630                 PERFORM CC-HAMTA-WDB6-INFO                               
091700              END-IF                                                      
091800           END-IF                                                         
091900        END-IF                                                            
092200     ELSE                                                                 
092300        MOVE ERR-ORDER-SAKNAS        TO MED-IDMFSFEL                      
092400        MOVE NEJ                     TO ALLT-SW                           
092500     END-IF                                                               
092600     MOVE SPACE                      TO MOD-KDVALISO                      
092700                                                                          
093000     IF MFS-FIRST AND ALLT-OK                                             
093100        MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                
093200        PERFORM MFS-RENSA-MOD-RADER                                       
093300        MOVE NEJ                    TO ALLT-SW                            
093400     END-IF                                                               
093500     .                                                                    
093600     EJECT                                                                
093700                                                                          
093800 CA-HAMTA-KUND SECTION.                                                   
093900                                                                          
094000     MOVE WS-IDDISTR        TO W-IDDISTR-WDB2                             
094100     MOVE WS-IDKUNDNR       TO W-IDKUNDNR-WDB2                            
094200     PERFORM IMS-GU-WDB201                                                
094300                                                                          
094400     IF OHUV-KDORDKL > 1                                                  
094500                                                                          
094600        MOVE +1 TO WS-INDEX                                               
094700        PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                           
094800           MOVE GMT-IDDC-BULK(WS-INDEX) TO                                
094900                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
095000           ADD +1 TO WS-INDEX                                             
095100        END-PERFORM                                                       
095200                                                                          
095300     ELSE                                                                 
095400       IF OHUV-KDORDKL = 1                                                
095500                                                                          
095600          MOVE +1 TO WS-INDEX                                             
095700          PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                         
095800             MOVE GMT-IDDC-DAY(WS-INDEX) TO                               
095900                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
096000             ADD +1 TO WS-INDEX                                           
096100          END-PERFORM                                                     
096200                                                                          
096300       ELSE                                                               
096400         IF OHUV-KDORDKL = 0                                              
096500                                                                          
096600            MOVE +1 TO WS-INDEX                                           
096700            PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                       
096800               MOVE GMT-IDDC-VOR(WS-INDEX) TO                             
096900                                  W-GMT-IDDC-CLEAR  (WS-INDEX)            
097000               ADD +1 TO WS-INDEX                                         
097100            END-PERFORM                                                   
097200                                                                          
097300         END-IF                                                           
097400       END-IF                                                             
097500     END-IF                                                               
097510     .                                                                    
097520     EJECT                                                                
097530 CB-FIXA-LOKAL-TID SECTION.                                               
097540                                                                          
097550     MOVE ALL '+'              TO MSGI-WMSGINIT                           
097560     MOVE '013'                TO MSGI-KDCALL                             
097570     MOVE 'WIDDC   '           TO MSGI-IDUSER                             
097580     IF OHUV-IDDC-TVS = SPACE                                             
097590       MOVE OHUV-IDDC-PRIM     TO MSGI-IDUSER(6:2)                        
097591     ELSE                                                                 
097592       MOVE OHUV-IDDC-TVS      TO MSGI-IDUSER(6:2)                        
097593     END-IF                                                               
097594                                                                          
097595     MOVE '4212'               TO MSGI-IDTRANS                            
097596     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
097597                                                                          
097598     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
097599     .                                                                    
097600     EJECT                                                                
097601 CC-HAMTA-WDB6-INFO SECTION.                                              
097602                                                                          
097603     MOVE SPACE                TO CLDC-W411CLDC                           
097604     MOVE W-GMT-IDDC-CLEAR-GRP TO CLDC-IDDC-CLEAR-GRP                     
097605                                                                          
097606     CALL W411CLDC USING CLDC-W411CLDC WDB6-PCB                           
097607     .                                                                    
097610     EJECT                                                                
097700 D-FORMELL-KONTROLL SECTION.                                              
097800                                                                          
097900     MOVE 'STA D-FORMELL'                 TO   WS-PGM-POSITION            
098000     MOVE 'IMS '                     TO ORFK-IDSYSTEM                     
098100     IF OHUV-IDDC-TVS = SPACE                                             
098200       MOVE OHUV-IDDC-PRIM           TO ORFK-IDDC                         
098300     ELSE                                                                 
098400       MOVE OHUV-IDDC-TVS            TO ORFK-IDDC                         
098500     END-IF                                                               
098600     MOVE OHUV-IDDISTR               TO ORFK-IDDISTR                      
098700     MOVE OHUV-IDKUNDNR              TO ORFK-IDKUNDNR                     
098800     MOVE OHUV-IDUSER                TO ORFK-IDUSER                       
098900     MOVE OHUV-KDFAKTYP              TO ORFK-KDFAKTYP                     
099000     MOVE OHUV-KDORDKL               TO ORFK-KDORDKL                      
099100     MOVE OHUV-KDTPOTYP              TO ORFK-KDTPOTYP                     
099200     MOVE OHUV-FLFORBI               TO ORFK-FLFORBI                      
099300     MOVE OHUV-FLEMBORD              TO ORFK-FLEMBORD                     
099400     MOVE OHUV-FLORDSPE              TO ORFK-FLORDSPE                     
099500     MOVE OHUV-FLOVRLEV              TO ORFK-FLOVRLEV                     
099600     MOVE OHUV-IDFTG                 TO ORFK-IDFTG                        
099700     MOVE +1                   TO WS-INDEX-MID                            
099800     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
099900        IF MID-FLINVEST(WS-INDEX-MID) = ALL '+'                           
100000           MOVE NEJ            TO ORFK-FLINVEST(WS-INDEX-MID)             
100100        ELSE                                                              
100200           IF MID-FLINVEST(WS-INDEX-MID) = 'Y'                            
100300              MOVE JA          TO MID-FLINVEST(WS-INDEX-MID)              
100400           END-IF                                                         
100500           MOVE MID-FLINVEST(WS-INDEX-MID)                                
100600                               TO ORFK-FLINVEST(WS-INDEX-MID)             
100700        END-IF                                                            
100800        IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                            
100900           MOVE OHUV-FLRESTN   TO ORFK-FLRESTN(WS-INDEX-MID)              
101000        ELSE                                                              
101100           IF MID-FLRESTN(WS-INDEX-MID) = 'Y'                             
101200              MOVE JA          TO MID-FLRESTN(WS-INDEX-MID)               
101300           END-IF                                                         
101400           MOVE MID-FLRESTN(WS-INDEX-MID)                                 
101500                               TO ORFK-FLRESTN(WS-INDEX-MID)              
101600        END-IF                                                            
101700     EJECT                                                                
101800        IF MID-FLSLATT(WS-INDEX-MID) = ALL '+'                            
101900           MOVE JA             TO ORFK-FLSLATT(WS-INDEX-MID)              
102000        ELSE                                                              
102100           IF MID-FLSLATT(WS-INDEX-MID) = 'Y'                             
102200              MOVE JA          TO MID-FLSLATT(WS-INDEX-MID)               
102300           END-IF                                                         
102400           MOVE MID-FLSLATT(WS-INDEX-MID)                                 
102500                               TO ORFK-FLSLATT(WS-INDEX-MID)              
102600        END-IF                                                            
102700                                                                          
102800        MOVE MID-IDARTNR(WS-INDEX-MID)                                    
102900                               TO ORFK-IDARTNR-IN(WS-INDEX-MID)           
103000                                                                          
103100        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
103200                               TO ORFK-KDKVBRYT(WS-INDEX-MID)             
103300                                                                          
103400        IF MID-KDVRINFO(WS-INDEX-MID) = ALL '+'                           
103500           MOVE OHUV-KDVRINFO  TO ORFK-KDVRINFO(WS-INDEX-MID)             
103600        ELSE                                                              
103700           MOVE MID-KDVRINFO(WS-INDEX-MID)                                
103800                               TO ORFK-KDVRINFO(WS-INDEX-MID)             
103900        END-IF                                                            
104000     EJECT                                                                
104100        MOVE MID-KVBEART(WS-INDEX-MID)                                    
104200                               TO ORFK-KVBEART(WS-INDEX-MID)              
104310        IF DIST79-DEALER-PRICE                                            
104400           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
104500                               TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
104600           MOVE ALL '+'        TO ORFK-PRARTNTO(WS-INDEX-MID)             
104700           MOVE ALL '+'        TO                                         
104800                          ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)             
104900        ELSE                                                              
105000           MOVE MID-PRARTNTO(WS-INDEX-MID)                                
105100                               TO ORFK-PRARTNTO(WS-INDEX-MID)             
105200           MOVE ALL '+'        TO ORFK-PRARTNTO-LOC(WS-INDEX-MID)         
105300           MOVE ALL '+'        TO                                         
105400                          ORFK-PRARTNTO-LOCPREL(WS-INDEX-MID)             
105500        END-IF                                                            
105600        MOVE MID-TITPO(WS-INDEX-MID)                                      
105700                               TO ORFK-TITPO-RAD(WS-INDEX-MID)            
105800        MOVE ALL '+'           TO ORFK-PRARTBTO-LOC(WS-INDEX-MID)         
105900        ADD +1                 TO WS-INDEX-MID                            
106000     END-PERFORM                                                          
106100                                                                          
106200     CALL W411ORFK USING ORFK-W411ORFK                                    
106300                         AREG-WDK6-PCB                                    
106400                         AREG-WDK7-PCB                                    
106500                                                                          
106600     MOVE +1                   TO WS-INDEX-MID                            
106700     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
106800        PERFORM DA-KOLLA-FEL-FK                                           
106900        ADD +1                 TO WS-INDEX-MID                            
107000     END-PERFORM                                                          
107100     IF SVARSBILD AND NOT ALLT-OK                                         
107200       MOVE 'FORMELLA FEL FÅR EJ INTRÄFFA VID START FRÅN 4213'            
107300                               TO FELTEXT                                 
107400       CALL ABEND USING RKOD-ABEND                                        
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 DA-KOLLA-FEL-FK SECTION.                                                 
107900                                                                          
108000     IF ORFK-FLINVEST-OK(WS-INDEX-MID) = NEJ                              
108100        MOVE ERR-UPPLYSTA-FEL   TO MED-IDMFSFEL                           
108200        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLINVEST-ATTR(WS-INDEX-MID)        
108300        MOVE NEJ                 TO ALLT-SW                               
108400     END-IF                                                               
108500                                                                          
108600     IF ORFK-FLRESTN-OK(WS-INDEX-MID) = NEJ                               
108700        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
108800        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLRESTN-ATTR(WS-INDEX-MID)        
108900        MOVE NEJ                 TO ALLT-SW                               
109000     END-IF                                                               
109100                                                                          
109200     IF ORFK-FLSLATT-OK(WS-INDEX-MID) = NEJ                               
109300        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
109400        MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLSLATT-ATTR(WS-INDEX-MID)        
109500        MOVE NEJ                 TO ALLT-SW                               
109600     END-IF                                                               
109700                                                                          
109800     IF ORFK-IDARTNR-OK(WS-INDEX-MID) = NEJ                               
109900        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
110000        MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
110100        MOVE NEJ                 TO ALLT-SW                               
110200     ELSE                                                                 
110300        IF (ORFK-KDORDBEK(WS-INDEX-MID) = 58 OR 59)                       
110400                  AND NOT MFS-UPDATE                                      
110500          MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                         
110600          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)        
110700          MOVE NEJ               TO ALLT-SW                               
110825        END-IF                                                            
110826     END-IF                                                               
110827     EJECT                                                                
110828     IF ORFK-KDKVBRYT-OK(WS-INDEX-MID) = NEJ                              
110829        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
110830        MOVE MFS-NUM-FAELT-FEL TO MOD-KDKVBRYT-ATTR(WS-INDEX-MID)         
110840        MOVE NEJ                 TO ALLT-SW                               
110850     END-IF                                                               
110860                                                                          
110870     IF ORFK-KDVRINFO-OK(WS-INDEX-MID) = NEJ                              
110880        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
110890        MOVE MFS-NUM-FAELT-FEL TO MOD-KDVRINFO-ATTR(WS-INDEX-MID)         
110900        MOVE NEJ                 TO ALLT-SW                               
111000     END-IF                                                               
111100     EJECT                                                                
111200                                                                          
111300     IF ORFK-KVBEART-OK(WS-INDEX-MID) = NEJ                               
111400        IF MED-IDMFSFEL = SPACE                                           
111500           IF ORFK-KVBEART(WS-INDEX-MID) NUMERIC AND                      
111600                   ORFK-KVBEART(WS-INDEX-MID) > ZERO                      
111700              IF NOT MFS-UPDATE                                           
111800                 MOVE ERR-STORT-UTTAG TO MED-IDMFSFEL                     
111900                 MOVE MFS-NUM-FAELT-FEL   TO                              
112000                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
112100                 MOVE NEJ             TO ALLT-SW                          
112200              END-IF                                                      
112300           ELSE                                                           
112400            MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                         
112500            MOVE MFS-NUM-FAELT-FEL   TO                                   
112600                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
112700            MOVE NEJ                 TO ALLT-SW                           
112800           END-IF                                                         
112900        ELSE                                                              
113000           MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                          
113100           MOVE MFS-NUM-FAELT-FEL   TO                                    
113200                                 MOD-KVBEART-ATTR(WS-INDEX-MID)           
113300           MOVE NEJ                 TO ALLT-SW                            
113400        END-IF                                                            
113500     END-IF                                                               
113600                                                                          
113700     IF ORFK-PRARTNTO-OK(WS-INDEX-MID) = NEJ                              
113800        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
113900        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
114000        MOVE NEJ                 TO ALLT-SW                               
114100     END-IF                                                               
114200     IF ORFK-PRARTNTO-LOC-OK(WS-INDEX-MID) = NEJ                          
114300        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
114400        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
114500        MOVE NEJ                 TO ALLT-SW                               
114600     END-IF                                                               
114700     IF ORFK-PRARTNTO-LOCPREL-OK(WS-INDEX-MID) = NEJ                      
114800        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
114900        MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTNTO-ATTR(WS-INDEX-MID)         
115000        MOVE NEJ                 TO ALLT-SW                               
115100     END-IF                                                               
115200     IF ORFK-TITPO-OK(WS-INDEX-MID) = NEJ                                 
115300        MOVE ERR-UPPLYSTA-FEL    TO MED-IDMFSFEL                          
115400        MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-ATTR(WS-INDEX-MID)          
115500        MOVE NEJ                 TO ALLT-SW                               
115600     END-IF                                                               
115700     .                                                                    
115800     EJECT                                                                
115900 DAA-CHECK-CROSS-TABLE SECTION.                                           
116000                                                                          
116100*    FOR THE TIME BEEING WE ONLY CHECK FOR LYNK-PARTS                     
116200*    IF A LYNK-ORDER WE NEED TO CHECK THAT GIVEN PARTNO                   
116300*    IS A LYNK PART                                                       
116400                                                                          
116500     PERFORM IMS-GU-WDF502                                                
116600     IF SEGMENT-SAKNAS                                                    
116700        MOVE 58                   TO ORFK-KDORDBEK(WS-INDEX-MID)          
116800        MOVE ERR-IDARTNR-SAKNAS   TO MED-IDMFSFEL                         
116900        MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(WS-INDEX-MID)          
117000        MOVE NEJ                  TO ALLT-SW                              
117100     END-IF                                                               
117200     .                                                                    
117210     EJECT                                                                
117220 E-BEHANDLA-RADER SECTION.                                                
117230                                                                          
117240     MOVE 'STA E-BEHANDLA'                TO   WS-PGM-POSITION            
117250     MOVE +1 TO WS-INDEX-MID                                              
117260     MOVE NEJ                     TO TILLK-SW                             
117270                                     OBKR-SW                              
117271                                     CDC-MOVE-SW                          
117280                                                                          
117290     MOVE +0                      TO WS-IDPRQUES                          
117300                                                                          
117400     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX                        
117500        IF MID-IDARTNR(WS-INDEX-MID) NOT = ALL '+'                        
117600           PERFORM S02-RENSA-TILLK-TAB                                    
117700           MOVE ORFK-W411AREG-001(WS-INDEX-MID)                           
117800                                  TO AREG-W411AREG-001                    
117900           PERFORM EC-BEHANDLA-RAD                                        
118000           PERFORM S10-HAMTA-WDB6-INFO                                    
118100                                                                          
118200           IF DCS-NDC-NA                                                  
118300              PERFORM S03-DATA-TILL-DEL-NOTE                              
118400           END-IF                                                         
118500           MOVE JA                TO TILLK-SW                             
118510           MOVE NEJ               TO CDC-MOVE-SW                          
118600           MOVE +1                TO WS-INDEX-TILLK                       
118700           PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR           
118800              TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                         
118900              IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                      
119000                 PERFORM ED-LAES-TILLK-DATA                               
119100                 PERFORM EC-BEHANDLA-RAD                                  
119200                 IF DCS-NDC-NA                                            
119300                    PERFORM S03-DATA-TILL-DEL-NOTE                        
119400                 END-IF                                                   
119500              END-IF                                                      
119600              ADD +1              TO WS-INDEX-TILLK                       
119700           END-PERFORM                                                    
119800        END-IF                                                            
119900        MOVE NEJ                  TO TILLK-SW                             
120000                                     OBKR-SW                              
120010                                     CDC-MOVE-SW                          
120100        ADD +1 TO WS-INDEX-MID                                            
120200     END-PERFORM                                                          
120300                                                                          
120400     IF DIST79-DEALER-PRICE AND WS-IDPRQUES NOT = +0                      
120500       MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                   
120600       MOVE +3                      TO PRNO-KDCALL                        
120700                                                                          
120800       CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                    
120900     END-IF                                                               
121000                                                                          
121100     IF AVSR-IDDC(1) NOT = SPACE                                          
121200        CALL W413AVSR USING AVSR-W413AVSR AVSR-ALT-PCB                    
121300                            AVSR-ORQI-PCB AVSR-GMTB-PCB                   
121400                            AVSR-GMTC-PCB AVSR-WDB2-PCB                   
121500                            AVSR-WDB6-PCB TRAN-XXKB-PCB                   
121600     END-IF                                                               
121700                                                                          
121800     IF AVSR-KDROPACK NOT = SPACE AND ZERO                                
121900        MOVE JA             TO BIPA-SW                                    
122000     END-IF                                                               
122100                                                                          
122200     MOVE JA                      TO ALLT-SW                              
122300     .                                                                    
122400     EJECT                                                                
122500                                                                          
122600 EC-BEHANDLA-RAD SECTION.                                                 
122700                                                                          
122800     MOVE 'STA EC-BEHANDLA'                TO   WS-PGM-POSITION           
122900     PERFORM ECA-NOLLSTALL-OBKR                                           
123000     PERFORM ECB-BYGG-UPP-ORDERRAD                                        
123100     PERFORM ECC-LAS-NYA-ARTIKELREG                                       
123200     PERFORM ECF-KOMPLETTERA-KVANT-BRYTES                                 
123300     PERFORM ECI-KOMPLETTERA-DIREKTLEVERANS                               
123400                                                                          
123500     IF NOT TILLKOMMANDE-RAD                                              
123600        PERFORM ECK-KOMPLETTERA-ERSATTNING                                
123700     END-IF                                                               
123800                                                                          
123900     PERFORM ECL-KOMPLETTERA-SPARRAR                                      
124000     PERFORM ECJ-KOMPLETTERA-PRIS                                         
124100     PERFORM ECM-KOMPLETTERA-TPO1                                         
124200     PERFORM ECN-KOMPLETTERA-TPO2                                         
124300     PERFORM ECO-KOMPLETTERA-KAMPANJER                                    
124400     PERFORM ECT-KOMPLETTERA-RELEASESPARR                                 
124410     IF KOLLA-ERS AND (DCS-CDC OR DCS-SDC)                                
124420        PERFORM ECZ-CHECK-KDERS-IN-DC                                     
124430     END-IF                                                               
124500     PERFORM ECW-PREL-AVBOKNING-SDC1                                      
124600     PERFORM ECH-PREL-AVBOKNING-SDC2                                      
124700     PERFORM ECX-PREL-AVBOKNING-SDC3                                      
124800     PERFORM ECG-PREL-AVBOKNING-XDC                                       
124900     PERFORM ECP-KOMPLETTERA-RANSONERING                                  
125000     PERFORM ECQ-KOMPLETTERA-STORA-UTTAG                                  
125100     PERFORM ECR-PREL-AVBOKNING-CDC                                       
125200                                                                          
125300     EJECT                                                                
125400     IF NOT TILLKOMMANDE-RAD                                              
125500        IF SKRIV-OBKR                                                     
125600           PERFORM ECS-SKRIV-OBKR                                         
125700           IF NOT OBKR-SKRIVEN OR                                         
125800              EGET-CL-RAD                                                 
125900              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
126000              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
126100           END-IF                                                         
126200        ELSE                                                              
126300           IF TPO1-FLKLAR = JA OR TPO2-FLKLAR = JA                        
126400                               OR KAMP-FLKLAR = JA                        
126500                               OR RELS-FLKLAR = JA                        
126600              CONTINUE                                                    
126700           ELSE                                                           
126800              PERFORM ECU-KONTROLLERA-ENHETSLAST                          
126900              PERFORM ECV-BERAKNA-WOPS-SKRIV-ORAD                         
127000           END-IF                                                         
127100        END-IF                                                            
127200     ELSE                                                                 
127300        IF SKRIV-OBKR                                                     
127400           PERFORM ECS-SKRIV-OBKR                                         
127500        END-IF                                                            
127600     END-IF                                                               
127700     .                                                                    
127800     EJECT                                                                
127900 ECA-NOLLSTALL-OBKR SECTION.                                              
128000                                                                          
128100     MOVE 'STA ECA-NOLLSTALL'              TO   WS-PGM-POSITION           
128200     MOVE ZERO                 TO KVAN-KDORDBEK-UT                        
128300                                  DLEV-KDORDBEK-UT                        
128400                                  TPO1-KDORDBEK                           
128500                                  TPO2-KDORDBEK                           
128600                                  RELS-KDORDBEK                           
128700                                  KAMP-KDORDBEK                           
128800                                  STOR-KDORDBEK                           
128900                                  XDCA-KDORDBEK                           
128910*                                 NDCA-KDORDBEK                           
129000                                  SDCA-KDORDBEK                           
129100                                  SDCA-KDORDBEK-FIRST-SDC                 
129200                                  SDCA-KDORDBEK-SECOND-SDC                
129300                                  CDCA-KDORDBEK-UT                        
129400                                  SPAR-KDORDBEK                           
129500                                  KERS-KDERS                              
129600     IF NOT TILLKOMMANDE-RAD                                              
129700        MOVE ZERO              TO KERS-KDORDBEK                           
129800     ELSE                                                                 
129900        MOVE JA                TO OBKR-SW                                 
130000     END-IF                                                               
130100                                                                          
130200     MOVE JA                   TO ALLT-SW                                 
130300     MOVE NEJ                  TO EGET-CL-RAD-SW                          
130400                                  KOLLA-ERS-SW                            
130500                                                                          
130600     IF ORFK-KDORDBEK(WS-INDEX-MID) = 56                                  
130700       MOVE ORFK-KDORDBEK(WS-INDEX-MID) TO W-KDORDBEK                     
130800       MOVE +7                          TO W-KDTPOTYP                     
130900     ELSE                                                                 
131000       IF ORFK-KDORDBEK(WS-INDEX-MID) > 0                                 
131100         MOVE NEJ               TO ALLT-SW                                
131200         MOVE JA                TO OBKR-SW                                
131300       END-IF                                                             
131400     END-IF                                                               
131500     .                                                                    
131600     EJECT                                                                
131700 ECB-BYGG-UPP-ORDERRAD SECTION.                                           
131900     MOVE 'STA ECB-BYGG     '              TO   WS-PGM-POSITION           
132000     MOVE OHUV-IDORDER         TO ORAD-IDORDER                            
132010                                                                          
132100     IF OHUV-IDDC-TVS = SPACE                                             
132200       MOVE OHUV-IDDC-PRIM     TO ORAD-IDDC                               
132300     ELSE                                                                 
132400       MOVE OHUV-IDDC-TVS      TO ORAD-IDDC                               
132500     END-IF                                                               
132600     MOVE ORAD-IDDC            TO WS-IDDC                                 
132700     MOVE +0                   TO ORAD-ADLAGOMR                           
132800     MOVE +0                   TO ORAD-ADGANG                             
132900     MOVE +0                   TO ORAD-ADPLATS                            
133000     IF TILLKOMMANDE-RAD                                                  
133100        MOVE AREG-IDARTNR      TO ORAD-IDARTNR                            
133200     ELSE                                                                 
133300        MOVE ORFK-IDARTNR(WS-INDEX-MID)                                   
133400                               TO ORAD-IDARTNR                            
133500     END-IF                                                               
133600     MOVE +1                   TO ORAD-IDLOPNR                            
133800     IF MID-BERADREF(WS-INDEX-MID) = ALL '+'                              
133900        MOVE SPACE             TO ORAD-BERADREF                           
134000     ELSE                                                                 
134100        MOVE MID-BERADREF(WS-INDEX-MID)                                   
134200                               TO ORAD-BERADREF                           
134300     END-IF                                                               
134400     IF MID-BEVOLREF = SPACE                                              
134500        MOVE OHUV-BEKUNDRF     TO ORAD-BEVOLREF                           
134600     ELSE                                                                 
134700        MOVE MID-BEVOLREF      TO ORAD-BEVOLREF                           
134800     END-IF                                                               
134900     MOVE SPACE                TO ORAD-FLAKPLOC                           
135000     MOVE 'N'                  TO ORAD-FLSDCLEV                           
135100                                                                          
135200     IF MID-FLINVEST(WS-INDEX-MID) = ALL '+'                              
135300        MOVE NEJ               TO ORAD-FLINVEST                           
135400     ELSE                                                                 
135500        MOVE MID-FLINVEST(WS-INDEX-MID)                                   
135600                               TO ORAD-FLINVEST                           
135700     END-IF                                                               
135800     EJECT                                                                
135900     MOVE JA                   TO ORAD-FLOBTRAN                           
136000     IF TILLKOMMANDE-RAD                                                  
136100       IF DIST79-DEALER-PRICE                                             
136200         MOVE NEJ              TO ORAD-FLPRTILL                           
136300       ELSE                                                               
136400        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
136500           MOVE TILK-FLPRTILL(WS-INDEX-TILLK)                             
136600                               TO ORAD-FLPRTILL                           
136700        ELSE                                                              
136800           MOVE NEJ            TO ORAD-FLPRTILL                           
136900        END-IF                                                            
137000       END-IF                                                             
137100     ELSE                                                                 
137200        MOVE NEJ               TO ORAD-FLPRTILL                           
137300     END-IF                                                               
137400     IF MID-FLRESTN(WS-INDEX-MID) = ALL '+'                               
137500        MOVE OHUV-FLRESTN      TO ORAD-FLRESTN                            
137600     ELSE                                                                 
137700        MOVE MID-FLRESTN(WS-INDEX-MID)                                    
137800                               TO ORAD-FLRESTN                            
137900     END-IF                                                               
138000     IF TILLKOMMANDE-RAD                                                  
138100        MOVE JA                TO ORAD-FLTILLK                            
138200     ELSE                                                                 
138300        MOVE NEJ               TO ORAD-FLTILLK                            
138400     END-IF                                                               
138500     MOVE WC-CDC-SE            TO ORAD-IDDC-RO                            
138600     MOVE OHUV-IDDISTR         TO ORAD-IDDISTR                            
138700     MOVE OHUV-IDKUNDNR        TO ORAD-IDKUNDNR                           
138800     MOVE W-IDKUNDRF           TO ORAD-IDKUNDRF                           
138900     MOVE OHUV-IDKAMPRF        TO ORAD-IDKAMPRF                           
139000     MOVE SPACE                TO ORAD-IDLEVNR                            
139100     MOVE +0                   TO ORAD-IDLOPNR-RO                         
139200     MOVE '0000000   '         TO ORAD-IDKUNDRF-RO                        
139300     EJECT                                                                
139400     MOVE ZERO                 TO ORAD-IDSPECEMB                          
139800     MOVE 'IMS '               TO ORAD-IDSYSTEM                           
140000     MOVE AREG-KDARTURS        TO ORAD-KDARTURS                           
140100     IF MID-KDVRINFO(WS-INDEX-MID) = ALL '+'                              
140200        MOVE OHUV-KDVRINFO     TO ORAD-KDDSP                              
140300     ELSE                                                                 
140400        MOVE MID-KDVRINFO(WS-INDEX-MID)                                   
140500                               TO ORAD-KDDSP                              
140600     END-IF                                                               
140700     IF ORAD-KDDSP = +0                                                   
140800        MOVE +1                TO ORAD-KDDSP                              
140900     END-IF                                                               
141000     MOVE AREG-KDFARLIG        TO ORAD-KDFARLIG                           
141100     EJECT                                                                
141200     IF MID-KDKVBRYT(WS-INDEX-MID) = ALL '+'                              
141300        MOVE +0                TO ORAD-KDKVBRYT                           
141400     ELSE                                                                 
141500        MOVE MID-KDKVBRYT(WS-INDEX-MID)                                   
141600                               TO ORAD-KDKVBRYT                           
141700     END-IF                                                               
141800     MOVE OHUV-KDORDING        TO ORAD-KDORDING                           
141900     MOVE OHUV-KDORDKL         TO ORAD-KDORDKL                            
142000     MOVE AREG-KDPRODSL        TO ORAD-KDPRODSL                           
142100                                  TEST-KDPRODSL                           
142200     IF ORAD-KDORDING = +3                                                
142300       MOVE SPACE              TO ORAD-KDOI                               
142400     ELSE                                                                 
142500       IF KDPRODSL-VOLVO-EMB OR ORAD-KDORDING = +1                        
142600         MOVE 'CD'             TO ORAD-KDOI                               
142700       ELSE                                                               
142800         MOVE 'DT'             TO ORAD-KDOI                               
142900       END-IF                                                             
143000     END-IF                                                               
143100     MOVE SPACE                TO ORAD-CLEARGROUP                         
143200                                                                          
143300     IF TILLKOMMANDE-RAD                                                  
143400       IF DIST79-DEALER-PRICE                                             
143500          MOVE SPACE           TO ORAD-KDPRTYP                            
143600       ELSE                                                               
143700        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
143800           MOVE TILK-KDPRTYP(WS-INDEX-TILLK)                              
143900                               TO ORAD-KDPRTYP                            
144000        ELSE                                                              
144100           MOVE SPACE          TO ORAD-KDPRTYP                            
144200        END-IF                                                            
144300       END-IF                                                             
144400     ELSE                                                                 
144500        MOVE SPACE             TO ORAD-KDPRTYP                            
144600     END-IF                                                               
144700     MOVE AREG-KDSPEEMB        TO ORAD-KDSPEEMB                           
144800     MOVE OHUV-KDTPOTYP        TO ORAD-KDTPOTYP                           
144900     IF OHUV-KDTPOTYP = +0 AND                                            
145000                MID-TITPO(WS-INDEX-MID) NOT = ALL '+'                     
145100        IF OHUV-IDKAMPRF > +0                                             
145200           MOVE +4             TO ORAD-KDTPOTYP                           
145300        ELSE                                                              
145400           MOVE +2             TO ORAD-KDTPOTYP                           
145500           IF ORAD-KDORDING = +3                                          
145600              CONTINUE                                                    
145700           ELSE                                                           
145800              MOVE +2          TO ORAD-KDORDING                           
145900           END-IF                                                         
146000        END-IF                                                            
146100     END-IF                                                               
146200     MOVE JA                   TO ORAD-FLORDING                           
146300     MOVE ORFK-KDVRINFO(WS-INDEX-MID) TO ORAD-KDVRINFO                    
146400     IF TILLKOMMANDE-RAD                                                  
146500        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
146600                               TO ORAD-KVBEART                            
146700     ELSE                                                                 
146800        MOVE ORFK-KVBEART(WS-INDEX-MID)                                   
146900                               TO ORAD-KVBEART                            
147000     END-IF                                                               
147100     EJECT                                                                
147200     MOVE +0                   TO ORAD-KVBEART-Q                          
147300     MOVE +0                   TO ORAD-KVPREAVB                           
147400     MOVE +0                   TO ORAD-KVPRERO                            
147500     MOVE +0                   TO ORAD-KVOKS-PREL                         
147600     MOVE +0                   TO ORAD-KVSLATT                            
147700     MOVE +0                   TO ORAD-IDPRQUES                           
147800     MOVE +0                   TO ORAD-PRARTBTO-LOC                       
147900     MOVE +0                   TO ORAD-RERAB                              
148000     MOVE SPACE                TO ORAD-KDVALISO                           
148100     MOVE SPACE                TO ORAD-KDVAT                              
148200     MOVE SPACE                TO ORAD-KDRAB                              
148300     MOVE SPACE                TO ORAD-BEART-VIPS                         
148400                                                                          
148500     IF TILLKOMMANDE-RAD                                                  
148600       IF DIST79-DEALER-PRICE                                             
148700          MOVE +0                    TO ORAD-PRARTNTO                     
148800          MOVE TILK-PRARTNTO-LOC(WS-INDEX-TILLK)                          
148900                                TO ORAD-PRARTNTO-LOC                      
149000          MOVE TILK-PRARTNTO-LOCPREL(WS-INDEX-TILLK)                      
149100                                TO ORAD-PRARTNTO-LOCPREL                  
149200       ELSE                                                               
149300        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
149400           MOVE TILK-PRARTNTO(WS-INDEX-TILLK)                             
149500                               TO ORAD-PRARTNTO                           
149600           MOVE +0             TO ORAD-PRARTNTO-LOC                       
149700                                  ORAD-PRARTNTO-LOCPREL                   
149800        ELSE                                                              
149900           MOVE +0             TO ORAD-PRARTNTO                           
150000           MOVE +0             TO ORAD-PRARTNTO-LOC                       
150100           MOVE +0             TO ORAD-PRARTNTO-LOCPREL                   
150200        END-IF                                                            
150300      END-IF                                                              
150400     ELSE                                                                 
150500        IF MID-PRARTNTO(WS-INDEX-MID) = ALL '+'                           
150600           MOVE +0             TO ORAD-PRARTNTO                           
150700           MOVE +0             TO ORAD-PRARTNTO-LOC                       
150800           MOVE +0             TO ORAD-PRARTNTO-LOCPREL                   
150900        ELSE                                                              
151010          IF DIST79-DEALER-PRICE                                          
151100           MOVE ORFK-PRARTNTO-LOC-UT(WS-INDEX-MID)                        
151200                               TO ORAD-PRARTNTO-LOC                       
151700           MOVE +0             TO ORAD-PRARTNTO                           
151800           MOVE +0             TO ORAD-PRARTNTO-LOCPREL                   
151900          ELSE                                                            
152000           MOVE ORFK-PRARTNTO-UT(WS-INDEX-MID)                            
152100                               TO ORAD-PRARTNTO                           
152200           MOVE +0             TO ORAD-PRARTNTO-LOC                       
152300           MOVE +0             TO ORAD-PRARTNTO-LOCPREL                   
152400          END-IF                                                          
152500        END-IF                                                            
152600        MOVE +0                TO ORAD-PRBPRIS                            
152700     END-IF                                                               
152800     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
152900        MOVE MID-IDARTNR(WS-INDEX-MID) (11:1)                             
153000                               TO ORAD-REKSIFFR                           
153100     ELSE                                                                 
153200        MOVE AREG-REKSIFFR     TO ORAD-REKSIFFR                           
153300     END-IF                                                               
153400     MOVE +0                   TO ORAD-RERF-RAD                           
153500                                                                          
153600     EJECT                                                                
153700     IF TILLKOMMANDE-RAD                                                  
153800       IF DIST79-DEALER-PRICE                                             
153900        MOVE ZERO              TO ORAD-TIPRIS                             
154000       ELSE                                                               
154100        IF TILK-PRARTNTO(WS-INDEX-TILLK) > +0                             
154200           MOVE TILK-TIPRIS(WS-INDEX-TILLK)                               
154300                               TO ORAD-TIPRIS                             
154400        ELSE                                                              
154500           MOVE +0             TO ORAD-TIPRIS                             
154600        END-IF                                                            
154700       END-IF                                                             
154800     ELSE                                                                 
154900        MOVE +0                TO ORAD-TIPRIS                             
155000     END-IF                                                               
155100     MOVE MSGI-TILOKDAT        TO ORAD-TIREGDAT                           
155200     MOVE MSGI-TILOKTID        TO WS-TIHHMM                               
155300     MOVE WS-TIHHMMSS          TO ORAD-TIREGTID                           
155400     MOVE +0                   TO ORAD-TIRODAT                            
155500     IF MID-TITPO(WS-INDEX-MID) = ALL '+'                                 
155600        MOVE OHUV-TITPO        TO ORAD-TITPO                              
155700     ELSE                                                                 
155800        MOVE MID-TITPO(WS-INDEX-MID)                                      
155900                               TO ORAD-TITPO                              
156000     END-IF                                                               
156100                                                                          
156200     MOVE AREG-VKART           TO ORAD-VKART                              
156300     MOVE AREG-VKART-NTO       TO ORAD-VKART-NTO                          
156400     MOVE AREG-VLARTNTO        TO ORAD-VLARTNTO                           
156500     MOVE SPACE                TO ORAD-IDBIL                              
156600                                  ORAD-IDKLIENT                           
156700                                  ORAD-IDARBREF                           
156800                                  ORAD-IDVIN                              
156900     IF ORAD-KDORDKL = 1 AND                                              
157000        GMT-FLLDCKND = JA                                                 
157100        MOVE ORAD-BERADREF     TO ORAD-IDKUNDRF-WIP                       
157200     ELSE                                                                 
157300        MOVE SPACE             TO ORAD-IDKUNDRF-WIP                       
157400     END-IF                                                               
157500     MOVE +0                   TO ORAD-PRAVCOST                           
157600                                                                          
157700     .                                                                    
157800     EJECT                                                                
157900 ECC-LAS-NYA-ARTIKELREG SECTION.                                          
158000                                                                          
158100     MOVE 'STA ECC-LAS-ART  '              TO   WS-PGM-POSITION           
158200     IF ALLT-OK                                                           
158300                                                                          
158400     MOVE ORAD-IDARTNR         TO W-IDARTNR                               
158500     PERFORM IMS-10-GU-WLARTM-WDK901                                      
158600                                                                          
158700     IF SEGMENT-SAKNAS                                                    
158800        MOVE W-IDARTNR         TO ARTM-IDARTNR-IN                         
158900        CALL W411ARTM USING ARTM-W411ARTM ARTM-ARTM-PCB                   
159000     END-IF                                                               
159100                                                                          
159200     END-IF                                                               
159300     .                                                                    
159400     EJECT                                                                
159500 ECF-KOMPLETTERA-KVANT-BRYTES SECTION.                                    
159600                                                                          
159700     MOVE 'STA ECF-KVANT    '              TO   WS-PGM-POSITION           
159800     IF ALLT-OK                                                           
159900                                                                          
160000     MOVE ORAD-KDKVBRYT        TO KVAN-KDKVBRYT-IN                        
160100     MOVE ORAD-IDSYSTEM        TO KVAN-IDSYSTEM-IN                        
160200     MOVE ORAD-KVBEART         TO KVAN-KVBEART-IN                         
160300     MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                       
160400     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
160500     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
160600     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
160700     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
160800     MOVE OHUV-KDORDKL         TO KVAN-KDORDKL-IN                         
160900     MOVE OHUV-FLEMBORD        TO KVAN-FLEMBORD-IN                        
161000     MOVE OHUV-FLFORBI         TO KVAN-FLFORBI-IN                         
161100     MOVE OHUV-FLORDSPE        TO KVAN-FLORDSPE-IN                        
161200     MOVE OHUV-FLOVRLEV        TO KVAN-FLOVRLEV-IN                        
161300     MOVE ORAD-IDKAMPRF        TO KVAN-IDKAMPRF-IN                        
161400     MOVE ORAD-IDDC            TO KVAN-IDDC-IN                            
161500     MOVE ORAD-IDDISTR         TO KVAN-IDDISTR-IN                         
161600     MOVE ORAD-IDKUNDNR        TO KVAN-IDKUNDNR-IN                        
161700     MOVE ORAD-BERADREF        TO KVAN-BERADREF-IN                        
161800     MOVE ORAD-IDARTNR         TO KVAN-IDARTNR-IN                         
161900                                                                          
162000     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
162100                                                                          
162200     MOVE KVAN-KDKVBRYT-UT         TO ORAD-KDKVBRYT                       
162300     MOVE KVAN-KVBEART-Q-UT        TO ORAD-KVBEART-Q                      
162400                                                                          
162500     IF KVAN-KDORDBEK-UT > +0                                             
162600        MOVE JA                    TO OBKR-SW                             
162700     END-IF                                                               
162800                                                                          
162900     END-IF                                                               
163000     .                                                                    
163100     EJECT                                                                
163200 ECI-KOMPLETTERA-DIREKTLEVERANS SECTION.                                  
163300                                                                          
163400     MOVE 'STA ECI-DLEV     '              TO   WS-PGM-POSITION           
163500                                                                          
163600     MOVE NEJ  TO SW-DDGS-TPO-OBKR71                                      
163700     PERFORM S10-HAMTA-WDB6-INFO                                          
163800                                                                          
163900     IF ALLT-OK AND (DCS-CDC OR DCS-SDC)                                  
164100                                                                          
164200     MOVE ORAD-IDDISTR         TO DLEV-IDDISTR-IN                         
164300     MOVE OHUV-IDDC-TVS        TO DLEV-IDDC-IN                            
164400     MOVE ORAD-IDDC            TO DLEV-IDDC-ORD-IN                        
164500     MOVE ORAD-IDKUNDNR        TO DLEV-IDKUNDNR-IN                        
164600     MOVE OHUV-KDORDKL         TO DLEV-KDORDKL-IN                         
164700     MOVE ORAD-IDARTNR         TO DLEV-IDARTNR-IN                         
164800     MOVE AREG-IDLEVNR         TO DLEV-IDLEVNR-IN                         
164900     MOVE ORAD-KVBEART-Q       TO DLEV-KVBEART-Q-IN                       
165000     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
165100     MOVE ORAD-IDKAMPRF        TO DLEV-IDKAMPRF-IN                        
165200     MOVE ORAD-KDTPOTYP        TO DLEV-KDTPOTYP-IN                        
165300     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
165400     MOVE OHUV-FLFORBI         TO DLEV-FLFORBI-IN                         
165500     MOVE ORAD-FLRESTN         TO DLEV-FLRESTN-IN                         
165600     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
165700     MOVE ORAD-KDORDING        TO DLEV-KDORDING-IN                        
166500     MOVE OHUV-IDKUNDRF        TO DLEV-IDKUNDRF-IN                        
166510                                                                          
166520     MOVE +1 TO WS-INDEX                                                  
166530     PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                              
166540        MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                                 
166550                               TO DLEV-IDDC-CLEAR-IN(WS-INDEX)            
166560        ADD +1 TO WS-INDEX                                                
166570     END-PERFORM                                                          
166580                                                                          
166600     MOVE 1                    TO DLEV-KDCALL                             
166700     MOVE SPACE                TO DLEV-CLEARGROUP                         
166800     MOVE ORAD-KDOI            TO DLEV-KDOI-UT                            
166900                                                                          
167000     CALL W411DLEV USING DLEV-W411DLEV DLEV-LEVF-PCB                      
167100                                       DLEV-LEVG-PCB                      
167200                                       DLEV-LEVA-PCB                      
167300                                       DLEV-ARTS-PCB                      
167400                                       WDB6-PCB                           
167500                                       TPO2-FILA-PCB                      
167600                                                                          
167700*    JUSTERING FÖR DIREKTLEVERERAD TPO-RAD                                
167800     IF DLEV-KDORDBEK-UT = 95                                             
167900        IF DLEV-KDTPOTYP-IN = +2                                          
168000           IF OHUV-KDTPOTYP NOT = +2                                      
168100              MOVE 21            TO DLEV-KDORDBEK-UT                      
168200              MOVE SPACE         TO DLEV-IDLEVNR-UT                       
168300           ELSE                                                           
168400              PERFORM ECIA-KOLLA-TPODAT                                   
168500              MOVE +0            TO ORAD-KDTPOTYP                         
168600              MOVE ZERO          TO ORAD-TITPO                            
168700              MOVE 'CD'          TO DLEV-KDOI-UT                          
168800           END-IF                                                         
168900        END-IF                                                            
169000     END-IF                                                               
169100                                                                          
169200     IF DLEV-KDORDBEK-UT = 21 OR 53                                       
169300        MOVE JA                  TO OBKR-SW                               
169400        MOVE NEJ                 TO ALLT-SW                               
169500        MOVE ZERO                TO KVAN-KDORDBEK-UT                      
169600        MOVE DLEV-IDLEVNR-UT     TO ORAD-IDLEVNR                          
169700     ELSE                                                                 
169800        IF DLEV-KDORDBEK-UT = 95                                          
169900           MOVE JA               TO OBKR-SW                               
170000        END-IF                                                            
170100        IF DLEV-IDLEVNR-UT NOT = SPACE                                    
170200          IF DLEV-IDDC-UT NOT = DLEV-IDDC-IN                              
170300            MOVE DLEV-IDDC-UT    TO ORAD-IDDC                             
170400                                    WS-IDDC                               
170500          END-IF                                                          
170600                                                                          
170700          MOVE AREG-ADLAGOMR     TO ORAD-ADLAGOMR                         
170800          MOVE AREG-ADGANG       TO ORAD-ADGANG                           
170900          MOVE AREG-ADPLATS      TO ORAD-ADPLATS                          
171000        END-IF                                                            
171100        MOVE DLEV-KDORDSTA-UT     TO AVSR-KDORDSTA (WS-INDEX-WOPS)        
171200        MOVE DLEV-KDVIA-UT        TO AVSR-KDVIA    (WS-INDEX-WOPS)        
171300        MOVE DLEV-KVDAGAR-DIFF-UT TO                                      
171400                                  AVSR-KVDAGAR-DIFF(WS-INDEX-WOPS)        
171500        MOVE DLEV-TISKEPPN-DDC-UT TO                                      
171600                                  AVSR-TISKEPPN-DDC(WS-INDEX-WOPS)        
171700                                                                          
171800*        DC FRÅN WDF211                                                   
171900        IF DLEV-FLSDCLEV-UT = JA                                          
172000          MOVE DLEV-IDDC-UT       TO ORAD-IDDC                            
172100                                     WS-IDDC                              
172200        ELSE                                                              
172300          MOVE DLEV-KDOI-UT       TO ORAD-KDOI                            
172400          MOVE DLEV-CLEARGROUP    TO ORAD-CLEARGROUP                      
172500        END-IF                                                            
172600        MOVE DLEV-FLRESTN-UT      TO ORAD-FLRESTN                         
172700        MOVE DLEV-FLSDCLEV-UT     TO ORAD-FLSDCLEV                        
172800        MOVE DLEV-IDLEVNR-UT      TO ORAD-IDLEVNR                         
172900     END-IF                                                               
173000     IF DLEV-KDORDBEK-UT = 0 AND AREG-ADLAGOMR = 79                       
173100       AND ORAD-IDDC = WC-CDC-SE                                          
173200       MOVE OHUV-IDDISTR     TO TEST-IDDISTR                              
173300       IF DIST18-SKROT                                                    
173400         CONTINUE                                                         
173500       ELSE                                                               
173600         MOVE 26             TO DLEV-KDORDBEK-UT                          
173700         MOVE JA             TO OBKR-SW                                   
173800       END-IF                                                             
173900     END-IF                                                               
174000                                                                          
174100     END-IF                                                               
174200     .                                                                    
174300     EJECT                                                                
174400 ECIA-KOLLA-TPODAT  SECTION.                                              
174500                                                                          
174600     MOVE 'STA ECIA-KOLLA   '              TO   WS-PGM-POSITION           
174700                                                                          
174800     IF OHUV-TITPO = ZERO                                                 
174900        PERFORM IMS-01-GHU-WDQ2-WDQ201                                    
175000        MOVE ORAD-TITPO    TO OHUV-TITPO                                  
175100        PERFORM IMS-02-REPL-WDQ2-WDQ201                                   
175200     ELSE                                                                 
175300        IF ORAD-TITPO NOT = OHUV-TITPO                                    
175400           MOVE OHUV-TITPO TO ORAD-TITPO                                  
175500           MOVE JA         TO SW-DDGS-TPO-OBKR71                          
175600        END-IF                                                            
175700     END-IF                                                               
175800     .                                                                    
175900     EJECT                                                                
176000 ECK-KOMPLETTERA-ERSATTNING SECTION.                                      
176100                                                                          
176200     MOVE 'STA ECK-ERS      '              TO   WS-PGM-POSITION           
176300     IF ALLT-OK                                                           
176400                                                                          
176500     MOVE ORAD-IDARTNR         TO KERS-IDARTNR                            
176600     MOVE ORAD-IDDC            TO KERS-IDDC                               
176700     MOVE OHUV-FLPRERS         TO KERS-FLPRERS                            
176800     MOVE ORAD-FLPRTILL        TO KERS-FLPRTILL                           
176900     MOVE OHUV-FLFORBI         TO KERS-FLFORBI                            
177000     MOVE OHUV-FLORDSPE        TO KERS-FLORDSPE                           
177100     MOVE OHUV-FLOVRLEV        TO KERS-FLOVRLEV                           
177200     MOVE ORAD-IDKAMPRF        TO KERS-IDKAMPRF                           
177300     MOVE AREG-KDERS           TO KERS-KDERS                              
177400     MOVE AREG-KDERS-UTG       TO KERS-KDERS-UTG                          
177500     MOVE ORAD-KDPRTYP         TO KERS-KDPRTYP                            
177600     MOVE ORAD-KDTPOTYP        TO KERS-KDTPOTYP                           
177700     MOVE AREG-KDUART          TO KERS-KDUART                             
177800     MOVE ORAD-KVBEART         TO KERS-KVBEART                            
177900     MOVE ORAD-PRARTNTO        TO KERS-PRARTNTO                           
178000     MOVE ORAD-DEAL-PR-LINE    TO KERS-DEAL-PR-LINE                       
178100     MOVE ORAD-TIPRIS          TO KERS-TIPRIS                             
178300     EJECT                                                                
178500     CALL W411KERS USING KERS-W411KERS TILK-W411TILK                      
178600                         KERS-ARTC-PCB KERS-ERSA-PCB                      
178700                         SDCA-ARTS-PCB CDCA-ARTM-PCB                      
179400                                                                          
179500     IF KERS-KDORDBEK > ZERO   AND                                        
179600        ((KERS-KDERS-UTG > +20 AND KERS-KDERS = +0)  OR                   
179700          KERS-KDERS > 10 )                                               
179800        MOVE JA                  TO OBKR-SW                               
179900        MOVE NEJ                 TO ALLT-SW                               
180000     END-IF                                                               
180100                                                                          
180200     PERFORM S10-HAMTA-WDB6-INFO                                          
180300                                                                          
180400     IF KERS-KDORDBEK > +0                                                
181610        IF DCS-SDC OR DCS-CDC                                             
181620          IF AREG-KDERS = 11 OR 12 OR 17 OR                               
181630                          21 OR 22 OR 27                                  
181640             MOVE JA                TO KOLLA-ERS-SW                       
181650          ELSE                                                            
181660             IF AREG-KDERS = 14 OR 15 OR 18 OR 19 OR                      
181670                             24 OR 25 OR 28 OR 29                         
181680               MOVE JA              TO ALLT-SW                            
181690             END-IF                                                       
181691          END-IF                                                          
181692        END-IF                                                            
181700        IF DCS-NDC                                                        
181800          IF AREG-KDERS > 18                                              
181900            MOVE JA                 TO KOLLA-ERS-SW                       
182000          ELSE                                                            
182100            MOVE ZERO               TO KERS-KDORDBEK                      
182200            PERFORM S02-RENSA-TILLK-TAB                                   
182300            MOVE JA                 TO ALLT-SW                            
182400          END-IF                                                          
182500        END-IF                                                            
182510     END-IF                                                               
182600     IF KERS-KDORDBEK > +0 AND KVAN-KDORDBEK-UT > +0                      
182700        IF KERS-KDERS = 01 AND (AREG-KDSORT = 'L' OR 'M')                 
182800***        CHECK FOR 'KERS-KDERS = 01' AND KDSROT IS 'L' OR 'M' SO        
182900***        THAT QUANTITY ADAPTION DO HAPPEN EVEN WHEN THE                 
183000***        SUPERSESSION CODE IS EQUAL TO 1 AND HENCE Q1 QUANTITY          
183100***        IS NOT BROKEN FURTHER. MIN QTY ORDERED WILL BECOME Q1          
183200           CONTINUE                                                       
183300        ELSE                                                              
183400           MOVE ZERO             TO KVAN-KDORDBEK-UT                      
183500           MOVE ORAD-KVBEART     TO ORAD-KVBEART-Q                        
183600        END-IF                                                            
183700     END-IF                                                               
183800     IF KERS-KDORDBEK > +0 AND DLEV-KDORDBEK-UT > +0                      
183900        MOVE ZERO                TO DLEV-KDORDBEK-UT                      
184000     END-IF                                                               
184100                                                                          
184200     ELSE                                                                 
184300        MOVE +0                  TO KERS-KDERS                            
184400     END-IF                                                               
184500     .                                                                    
184600     EJECT                                                                
184700 ECL-KOMPLETTERA-SPARRAR SECTION.                                         
184800                                                                          
184900     MOVE 'STA ECL-SPAR     '              TO   WS-PGM-POSITION           
185000     IF ALLT-OK OR KOLLA-ERS                                              
185100                                                                          
185200     MOVE ORAD-BERADREF        TO SPAR-BERADREF                           
185300     MOVE OHUV-BEKUNDRF        TO SPAR-BEKUNDRF                           
185400     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
185500     MOVE OHUV-FLEMBORD        TO SPAR-FLEMBORD                           
185600     MOVE OHUV-FLFORBI         TO SPAR-FLFORBI                            
185700     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
185800     MOVE OHUV-FLORDSPE        TO SPAR-FLORDSPE                           
185900     MOVE OHUV-FLOVRLEV        TO SPAR-FLOVRLEV                           
186000     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
186100     MOVE ORAD-FLRESTN         TO SPAR-FLRESTN                            
186200     MOVE ORAD-IDARTNR         TO SPAR-IDARTNR                            
186300     MOVE AREG-FLIART          TO SPAR-FLIART                             
186400     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
186500     MOVE ORAD-IDDISTR         TO SPAR-IDDISTR                            
186600     MOVE ORAD-IDKUNDNR        TO SPAR-IDKUNDNR                           
186700     MOVE ORAD-IDKUNDRF-RO     TO SPAR-IDKUNDRF-RO                        
186800     PERFORM S10-HAMTA-WDB6-INFO                                          
186900                                                                          
187000     IF DCS-DDC                                                           
187100       MOVE OHUV-IDDC-PRIM     TO SPAR-IDDC                               
187200     ELSE                                                                 
187300       MOVE ORAD-IDDC          TO SPAR-IDDC                               
187400     END-IF                                                               
187500     MOVE ORAD-IDSYSTEM        TO SPAR-IDSYSTEM                           
187600     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
187700     MOVE AREG-KDERS           TO SPAR-KDERS                              
187800     MOVE OHUV-KDFAKTYP        TO SPAR-KDFAKTYP                           
187900     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
188000     MOVE +1                   TO SPAR-KDORDBEH                           
188100     MOVE OHUV-KDORDKL         TO SPAR-KDORDKL                            
188200     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
188300     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
188400     MOVE ORAD-KDPRTYP         TO SPAR-KDPRTYP                            
188500     MOVE ORAD-KDTPOTYP        TO SPAR-KDTPOTYP                           
188600     MOVE AREG-KDUART          TO SPAR-KDUART                             
188700     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
188800     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
188900     MOVE ORAD-TIRODAT         TO SPAR-TIRODAT                            
189000     MOVE ORAD-TITPO           TO SPAR-TITPO                              
189100     MOVE ORAD-FLSDCLEV        TO SPAR-FLSDCLEV                           
189200     MOVE OHUV-TIREPDAT        TO SPAR-TIREPDAT                           
189300                                                                          
189400     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
189500                                       SPAR-WDF8A-PCB                     
189600                                       SPAR-WDK6-PCB                      
189700                                                                          
189800     IF SPAR-KDORDBEK > ZERO                                              
189900        MOVE JA               TO OBKR-SW                                  
190000        MOVE NEJ              TO ALLT-SW                                  
190100                                                                          
190200        IF SPAR-KDORDBEK = 51 OR 67 OR 58                                 
190300          MOVE NEJ            TO KOLLA-ERS-SW                             
190310          MOVE ZERO           TO KERS-KDORDBEK                            
190320          PERFORM S02-RENSA-TILLK-TAB                                     
190400        END-IF                                                            
190000        MOVE ZERO             TO XDCA-DAPUBL                              
190500*FOR CODE 67 THERE IS A SPECIAL CHECK IN W411NDCA                         
190600        IF  SPAR-KDORDBEK =67 AND SPAR-FLPUBCDC = YES                     
190700          MOVE 99999999       TO XDCA-DAPUBL                              
190710*         MOVE 99999999       TO NDCA-DAPUBL                              
190800        END-IF                                                            
190900        IF KVAN-KDORDBEK-UT > +0                                          
191000           MOVE +0            TO KVAN-KDORDBEK-UT                         
191100           MOVE ORAD-KVBEART  TO ORAD-KVBEART-Q                           
191200        END-IF                                                            
191210*FOR KDERS 19/29,SPAR GIVES OCC54.BUT IF STOCKS ARE PRESENT IN A          
191220*DC,ORDERLINE WITH 19/29 CAN BE REGISTERED.                               
191230        IF SPAR-KDORDBEK = 54 AND NOT DCS-DDC                             
191240           MOVE JA             TO ALLT-SW                                 
191250        END-IF                                                            
191300        IF DLEV-KDORDBEK-UT > +0                                          
191400           MOVE +0            TO DLEV-KDORDBEK-UT                         
191500        END-IF                                                            
191600     END-IF                                                               
191700     IF AREG-KDSORT = 'SW'                                                
191800        MOVE 67                     TO SPAR-KDORDBEK                      
191900        MOVE JA                     TO OBKR-SW                            
192000        MOVE NEJ                    TO ALLT-SW                            
192010        IF KOLLA-ERS-SW = JA                                              
192020          MOVE NEJ            TO KOLLA-ERS-SW                             
192030          MOVE ZERO           TO KERS-KDORDBEK                            
192040          PERFORM S02-RENSA-TILLK-TAB                                     
192050        END-IF                                                            
192100     END-IF                                                               
192200                                                                          
192300     END-IF                                                               
192400     .                                                                    
192500     EJECT                                                                
192600 ECJ-KOMPLETTERA-PRIS SECTION.                                            
192700                                                                          
192800     MOVE 'STA ECJ-PRIS     '              TO   WS-PGM-POSITION           
192900     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
193000                                                                          
193100     IF DIST79-DEALER-PRICE                                               
193200       IF WS-IDPRQUES                = +0                                 
193300          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
193400          MOVE +1                    TO PRNO-KDCALL                       
193500                                                                          
193600          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
193700                                                                          
193800          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
193900                                        WS-IDPRQUES                       
194000          MOVE +1                    TO PRQU-KDCALL                       
194100       ELSE                                                               
194200          MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                  
194300          MOVE +2                    TO PRNO-KDCALL                       
194400                                                                          
194500          CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                 
194600                                                                          
194700          MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                     
194800                                        WS-IDPRQUES                       
194900          MOVE +2                    TO PRQU-KDCALL                       
195000       END-IF                                                             
195100                                                                          
195200       MOVE W-IDDISTR                TO PRQU-IDDISTR                      
195300       MOVE W-IDKUNDNR               TO PRQU-IDKUNDNR                     
195400       MOVE W-IDKUNDRF               TO PRQU-IDKUNDRF                     
195500       MOVE ORAD-IDORDER             TO PRQU-IDORDER                      
195600       MOVE ORAD-KDORDKL             TO PRQU-KDORDKL                      
195700       MOVE 'N'                      TO PRQU-KDPRSTA                      
195800       MOVE ORAD-IDARTNR             TO PRQU-IDARTNR                      
195900       MOVE ORAD-KVBEART-Q           TO PRQU-KVBEART-Q                    
196000                                                                          
196100       MOVE GMT-IDFTG                TO W-WDB1-IDFTG                      
196200       MOVE GMT-IDPARTNR             TO W-WDB1-IDPARTNR                   
196300       PERFORM IMS-GU-WDB101                                              
196400       MOVE BET-KDVALISO             TO ORAD-KDVALISO                     
196500                                        PRQU-KDVALISO                     
196600                                                                          
196700       MOVE ORAD-PRARTNTO-LOC        TO PRQU-PRARTNTO-LOC                 
196800       MOVE +0                       TO PRQU-PRARTNTO-LOCPREL             
196900       MOVE ORAD-IDSYSTEM            TO PRQU-IDSYSTEM                     
197000                                                                          
197100       CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                   
197200                                          PRQU-WDC7-PCB                   
197300                                          PRQU-SJKO-WDK6-PCB              
197400                                                                          
197500       MOVE PRQU-IDPRQUES            TO  ORAD-IDPRQUES                    
197600                                         WS-IDPRQUES                      
197700       MOVE PRQU-FLPRTILL            TO  ORAD-FLPRTILL                    
197800       IF ORAD-PRARTNTO-LOC = +0                                          
197900          MOVE PRQU-PRARTNTO-LOCPREL TO ORAD-PRARTNTO-LOCPREL             
198000       END-IF                                                             
198100                                                                          
198200       IF ORAD-PRARTNTO-LOC NOT = +0                                      
198300         IF ORAD-KDPRTYP = SPACE                                          
198400           MOVE 'P'            TO ORAD-KDPRTYP                            
198500           MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                             
198600         END-IF                                                           
198700       END-IF                                                             
198800                                                                          
198900     ELSE                                                                 
199000*      *NOT DIST79-DEALER-PRICE                                           
199100                                                                          
199200       PERFORM S10-HAMTA-WDB6-INFO                                        
199300                                                                          
199400       IF DCS-NDC    OR                                                   
199500          DCS-SDC    OR DCS-DDC    OR                                     
199600         (ORAD-KDTPOTYP = +0 AND AREG-KDUART = SPACE)                     
199700                                                                          
199990           IF ORAD-PRARTNTO NOT = +0                                      
200000*            *FETCH ONLY KDVALISO FROM W335PRIS                           
200100             MOVE 2                TO PRIS-KDCALL                         
200200           ELSE                                                           
200300*            *FETCH PRARTNTO/PRAVCOST AND KDVALISO FRON W335PRIS          
200400             MOVE 1                TO PRIS-KDCALL                         
200500           END-IF                                                         
200700       ELSE                                                               
200800         MOVE 2                  TO PRIS-KDCALL                           
200900       END-IF                                                             
201000                                                                          
201100       MOVE IDPGM                TO PRIS-IDPGM                            
201200       MOVE ORAD-IDARTNR         TO PRIS-IDARTNR                          
201300       MOVE ORAD-IDDISTR         TO PRIS-IDDISTR                          
201400       MOVE ORAD-IDKUNDNR        TO PRIS-IDKUNDNR                         
201500       MOVE ORAD-IDDC            TO PRIS-IDDC                             
201600       MOVE OHUV-KDORDKL         TO PRIS-KDORDKL                          
201700       MOVE ORAD-KVBEART-Q       TO PRIS-KVBEART                          
201800       MOVE ORAD-FLINVEST        TO PRIS-FLINVEST                         
201900                                                                          
202000       CALL W335PRIS USING PRIS-W335PRIS PRIS-ARTC-PCB                    
202100                           PRIS-WDK7-PCB                                  
202200                           PRIS-GMTA-PCB PRIS-BETA-PCB                    
202300                           PRIS-GPRIA-PCB PRIS-GPRIB-PCB                  
202400                           PRIS-COST-WDK6-PCB                             
202500                           PRIS-COST-WDK7-PCB                             
202600                           PRIS-COST-WDF1-PCB                             
202700                           PRIS-COST-9305-PCB                             
202800                           PRIS-COST-WDK72-PCB                            
202900                           PRIS-COST-WDB6-PCB                             
203000                                                                          
203100       IF PRIS-KDSVAR = '2'                                               
203200         MOVE 'DIST/KUND SAKNAS NÄR W335PRIS LÄSER KUNDREG'               
203300                           TO FELTEXT                                     
203400         CALL ABEND USING RKOD-ABEND                                      
203500       END-IF                                                             
203600                                                                          
203700       IF PRIS-KDCALL = 2                                                 
203800         MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                             
203900         IF ORAD-KDPRTYP = SPACE                                          
204000           MOVE 'P'           TO ORAD-KDPRTYP                             
204100           MOVE ORAD-TIREGDAT TO ORAD-TIPRIS                              
204200         END-IF                                                           
204300       ELSE                                                               
204450         MOVE PRIS-PRARTNTO  TO ORAD-PRARTNTO                             
204470         MOVE PRIS-FLPRTILL  TO ORAD-FLPRTILL                             
204480         MOVE PRIS-KDPRTYP   TO ORAD-KDPRTYP                              
204490         MOVE PRIS-PRBPRIS   TO ORAD-PRBPRIS                              
204500         MOVE ORAD-TIREGDAT  TO ORAD-TIPRIS                               
204600         MOVE PRIS-KDVALISO  TO ORAD-KDVALISO                             
204700         MOVE PRIS-PRAVCOST  TO ORAD-PRAVCOST                             
204800       END-IF                                                             
204900     END-IF                                                               
205000     END-IF                                                               
205100     .                                                                    
205200     EJECT                                                                
205300                                                                          
205400                                                                          
205500                                                                          
205600 ECM-KOMPLETTERA-TPO1 SECTION.                                            
205700                                                                          
205800     MOVE 'STA ECM-TPOI     '              TO   WS-PGM-POSITION           
205900                                                                          
206000     PERFORM S10-HAMTA-WDB6-INFO                                          
206100                                                                          
206200     IF ALLT-OK AND (DCS-CDC OR DCS-SDC)                                  
206400                                                                          
206500     MOVE ORAD-IDDISTR         TO TPO1-IDDISTR                            
206600     MOVE ORAD-IDKUNDNR        TO TPO1-IDKUNDNR                           
206700     MOVE ORAD-IDKUNDRF        TO TPO1-IDKUNDRF                           
206800     MOVE ORAD-IDARTNR         TO TPO1-IDARTNR                            
206900     MOVE ORAD-BERADREF        TO TPO1-BERADREF                           
207000     MOVE AREG-IDANSK          TO TPO1-IDANSK                             
207100     MOVE OHUV-IDKONTO         TO TPO1-IDKONTO                            
207200     MOVE OHUV-IDKST           TO TPO1-IDKST                              
207300     MOVE OHUV-IDANALYS        TO TPO1-IDANALYS                           
207400     MOVE ORAD-KDDSP           TO TPO1-KDDSP                              
207500     MOVE OHUV-KDFAKTYP        TO TPO1-KDFAKTYP                           
207600     MOVE ARB-KDFRAKT          TO TPO1-KDFRAKT                            
207700     MOVE ORAD-KDKVBRYT        TO TPO1-KDKVBRYT                           
207800     MOVE ORAD-KDORDING        TO TPO1-KDORDING                           
207900     MOVE OHUV-KDORDKL         TO TPO1-KDORDKL                            
208000     MOVE AREG-KDPRODSL        TO TPO1-KDPRODSL                           
208100     MOVE ORAD-KDVRINFO        TO TPO1-KDVRINFO                           
208200     MOVE ORAD-KVBEART-Q       TO TPO1-KVBEART-Q                          
208300     MOVE AREG-REKSIFFR        TO TPO1-REKSIFFR                           
208400     MOVE ORAD-TITPO           TO TPO1-TITPO                              
208500     IF ORAD-KDPRTYP = 'P'                                                
208600        MOVE ORAD-PRARTNTO     TO TPO1-PRARTNTO                           
208700        MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                       
208800        MOVE ORAD-KDPRTYP      TO TPO1-KDPRTYP                            
208900        MOVE ORAD-FLPRTILL     TO TPO1-FLPRTILL                           
209000     ELSE                                                                 
209100        MOVE ORAD-DEAL-PR-LINE TO TPO1-DEAL-PR-LINE                       
209200        MOVE ZERO              TO TPO1-PRARTNTO                           
209300        MOVE SPACE             TO TPO1-KDPRTYP                            
209400        MOVE NEJ               TO TPO1-FLPRTILL                           
209500     END-IF                                                               
209600     EJECT                                                                
209700     MOVE ORAD-BEVOLREF        TO TPO1-BEVOLREF                           
209800     MOVE ORAD-IDKAMPRF        TO TPO1-IDKAMPRF                           
209900     MOVE ORAD-IDSYSTEM        TO TPO1-IDSYSTEM                           
210000     MOVE ORAD-FLINVEST        TO TPO1-FLINVEST                           
210100     MOVE ORAD-IDLEVNR         TO TPO1-IDLEVNR                            
210200     MOVE AREG-FLTPO1          TO TPO1-FLTPO1                             
210300     MOVE AREG-KVFRYSTI        TO TPO1-KVFRYSTI                           
210400     MOVE +1                   TO TPO1-KDORDBEH                           
210500     MOVE OHUV-FLFORBI         TO TPO1-FLFORBI                            
210600     MOVE OHUV-FLORDSPE        TO TPO1-FLORDSPE                           
210700     MOVE OHUV-FLOVRLEV        TO TPO1-FLOVRLEV                           
210800     MOVE ORAD-KDTPOTYP        TO TPO1-KDTPOTYP                           
210900     MOVE OHUV-BEKUNDRF        TO TPO1-BEKUNDRF                           
211000                                                                          
211100     MOVE +0                   TO TPO1-KDORDBEK                           
211200     MOVE SPACE                TO TPO1-FLKLAR                             
211300     MOVE OHUV-KDORDTYP-LDC    TO TPO1-KDORDTYP-LDC                       
211400     MOVE OHUV-TIREPDAT        TO TPO1-TIREPDAT                           
211500     MOVE ORAD-IDKUNDRF-WIP    TO TPO1-IDKUNDRF-WIP                       
211600                                                                          
211700     CALL W411TPO1 USING TPO1-W411TPO1 TPO1-ORDP-PCB                      
211800                         TPO1-ARTM-PCB TPO1-ZZAC-PCB                      
211900                                                                          
212000     IF TPO1-KDORDBEK > +0                                                
212100        MOVE JA                TO OBKR-SW                                 
212200        MOVE NEJ               TO ALLT-SW                                 
212300        MOVE WC-CDC-SE         TO ORAD-IDDC                               
212400        MOVE ORAD-IDDC         TO WS-IDDC                                 
212500        IF KVAN-KDORDBEK-UT > +0                                          
212600           MOVE +0             TO KVAN-KDORDBEK-UT                        
212700           MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                          
212800        END-IF                                                            
212900        IF DLEV-KDORDBEK-UT > +0                                          
213000           MOVE +0             TO DLEV-KDORDBEK-UT                        
213100        END-IF                                                            
213200     ELSE                                                                 
213300        IF TPO1-FLKLAR = JA                                               
213400           MOVE NEJ            TO ALLT-SW                                 
213500           IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD                
213600              MOVE ZERO        TO KERS-KDORDBEK                           
213700              PERFORM S02-RENSA-TILLK-TAB                                 
213800           END-IF                                                         
213900        END-IF                                                            
214000     END-IF                                                               
214100                                                                          
214200     END-IF                                                               
214300     .                                                                    
214400     EJECT                                                                
214500 ECN-KOMPLETTERA-TPO2 SECTION.                                            
214600                                                                          
214700     MOVE 'STA ECN-TPO2     '              TO   WS-PGM-POSITION           
214800                                                                          
214900     PERFORM S10-HAMTA-WDB6-INFO                                          
215000                                                                          
215100     IF DCS-SDC             AND                                           
215200        ORAD-KDORDKL  = 1   AND                                           
215300        ORAD-IDKAMPRF = 0   AND                                           
215400       (AREG-KDUART   = 'L' OR  AREG-KDUART = 'P')                        
215500                                                                          
215600       CONTINUE                                                           
215700     ELSE                                                                 
215800       IF ALLT-OK AND (DCS-CDC OR DCS-SDC)                                
216000                                                                          
216100       MOVE ORAD-IDDISTR         TO TPO2-IDDISTR                          
216200       MOVE ORAD-IDKUNDNR        TO TPO2-IDKUNDNR                         
216300       MOVE ORAD-IDKUNDRF        TO TPO2-IDKUNDRF                         
216400       MOVE ORAD-IDARTNR         TO TPO2-IDARTNR                          
216500       MOVE ORAD-BERADREF        TO TPO2-BERADREF                         
216600       MOVE AREG-IDANSK          TO TPO2-IDANSK                           
216700       MOVE OHUV-IDKONTO         TO TPO2-IDKONTO                          
216800       MOVE OHUV-IDKST           TO TPO2-IDKST                            
216900       MOVE OHUV-IDANALYS        TO TPO2-IDANALYS                         
217000       MOVE ORAD-KDDSP           TO TPO2-KDDSP                            
217100       MOVE OHUV-KDFAKTYP        TO TPO2-KDFAKTYP                         
217200       MOVE ARB-KDFRAKT          TO TPO2-KDFRAKT                          
217300       MOVE ORAD-KDKVBRYT        TO TPO2-KDKVBRYT                         
217400       MOVE ORAD-KDORDING        TO TPO2-KDORDING                         
217500       MOVE OHUV-KDORDKL         TO TPO2-KDORDKL                          
217600       MOVE AREG-KDPRODSL        TO TPO2-KDPRODSL                         
217700       MOVE ORAD-KDVRINFO        TO TPO2-KDVRINFO                         
217800       MOVE ORAD-KVBEART-Q       TO TPO2-KVBEART-Q                        
217900       MOVE AREG-REKSIFFR        TO TPO2-REKSIFFR                         
218000       MOVE ORAD-TITPO           TO TPO2-TITPO                            
218100       MOVE ORAD-PRARTNTO        TO TPO2-PRARTNTO                         
218200       MOVE ORAD-DEAL-PR-LINE    TO TPO2-DEAL-PR-LINE                     
218300       MOVE ORAD-KDPRTYP         TO TPO2-KDPRTYP                          
218400       MOVE ORAD-FLPRTILL        TO TPO2-FLPRTILL                         
218500       EJECT                                                              
218600       MOVE ORAD-BEVOLREF        TO TPO2-BEVOLREF                         
218700       MOVE ORAD-IDKAMPRF        TO TPO2-IDKAMPRF                         
218800       MOVE ORAD-IDSYSTEM        TO TPO2-IDSYSTEM                         
218900       MOVE ORAD-FLINVEST        TO TPO2-FLINVEST                         
219000       MOVE OHUV-FLORDSPE        TO TPO2-FLORDSPE                         
219100       MOVE OHUV-FLOVRLEV        TO TPO2-FLOVRLEV                         
219200       MOVE OHUV-FLFORBI         TO TPO2-FLFORBI                          
219300       MOVE ORAD-IDLEVNR         TO TPO2-IDLEVNR                          
219400       MOVE AREG-KDUART          TO TPO2-KDUART                           
219500       MOVE AREG-KVFRYSTI        TO TPO2-KVFRYSTI                         
219600       MOVE +1                   TO TPO2-KDORDBEH                         
219700       MOVE ORAD-KDTPOTYP        TO TPO2-KDTPOTYP                         
219800       MOVE OHUV-BEKUNDRF        TO TPO2-BEKUNDRF                         
219900       MOVE ORAD-FLTILLK         TO TPO2-FLTILLK                          
220000       MOVE AREG-TIDISPIN        TO TPO2-TIDISPIN                         
220100       MOVE OHUV-BEVARREF        TO TPO2-BEVARREF                         
220200       MOVE KVAN-KVQPACK-UT      TO TPO2-KVQPACK-1                        
220300       MOVE ORAD-KVBEART         TO TPO2-KVBEART                          
220400                                                                          
220500       MOVE +0                   TO TPO2-KDORDBEK                         
220600       MOVE SPACE                TO TPO2-FLKLAR                           
220700       MOVE OHUV-KDORDTYP-LDC    TO TPO2-KDORDTYP-LDC                     
220800       MOVE OHUV-TIREPDAT        TO TPO2-TIREPDAT                         
220900       MOVE ORAD-IDKUNDRF-WIP    TO TPO2-IDKUNDRF-WIP                     
221000                                                                          
221100       CALL W411TPO2 USING TPO2-W411TPO2 TPO2-ORDP-PCB                    
221200                                       TPO2-XXBU-PCB TPO2-XXBV-PCB        
221300                         TPO2-ARTM-PCB TPO2-FILA-PCB TPO2-XXBX-PCB        
221400                         TIME-4437-PCB                                    
221500                                                                          
221600       IF TPO2-KDORDBEK > +0                                              
221700          MOVE JA                TO OBKR-SW                               
221800          MOVE NEJ               TO ALLT-SW                               
221900          MOVE WC-CDC-SE         TO ORAD-IDDC                             
222000          MOVE ORAD-IDDC         TO WS-IDDC                               
222100          MOVE TPO2-KDTPOTYP     TO ORAD-KDTPOTYP                         
222200          IF ORAD-KDTPOTYP = 6                                            
222300             IF ORAD-KDPRTYP NOT = 'P'                                    
222400                MOVE ZERO        TO ORAD-PRARTNTO                         
222500                MOVE SPACE       TO ORAD-KDPRTYP                          
222600**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
222700                IF NOT DIST79-DEALER-PRICE                                
222900                  MOVE ZERO        TO ORAD-PRARTNTO-LOC                   
223000                  MOVE NEJ         TO ORAD-FLPRTILL                       
223100                END-IF                                                    
223200*************                                                             
223300             END-IF                                                       
223400          END-IF                                                          
223500       ELSE                                                               
223600          IF TPO2-FLKLAR = JA                                             
223700             MOVE NEJ            TO ALLT-SW                               
223800             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
223900                MOVE ZERO        TO KERS-KDORDBEK                         
224000                PERFORM S02-RENSA-TILLK-TAB                               
224100             END-IF                                                       
224200          END-IF                                                          
224300       END-IF                                                             
224400                                                                          
224500       END-IF                                                             
224600     END-IF                                                               
224700     .                                                                    
224800     EJECT                                                                
224900 ECO-KOMPLETTERA-KAMPANJER SECTION.                                       
225000                                                                          
225100     MOVE 'STA ECO-KAMP     '              TO   WS-PGM-POSITION           
225200                                                                          
225300     PERFORM S10-HAMTA-WDB6-INFO                                          
225400                                                                          
225500     IF ALLT-OK AND (DCS-CDC OR DCS-SDC)                                  
225700                                                                          
225800     MOVE ORAD-IDDISTR         TO KAMP-IDDISTR                            
225900     MOVE ORAD-IDKUNDNR        TO KAMP-IDKUNDNR                           
226000     MOVE ORAD-IDKUNDRF        TO KAMP-IDKUNDRF                           
226100     MOVE ORAD-IDARTNR         TO KAMP-IDARTNR                            
226200     MOVE ORAD-BERADREF        TO KAMP-BERADREF                           
226300     MOVE AREG-IDANSK          TO KAMP-IDANSK                             
226400     MOVE OHUV-IDKONTO         TO KAMP-IDKONTO                            
226500     MOVE OHUV-IDANALYS        TO KAMP-IDANALYS                           
226600     MOVE OHUV-IDKST           TO KAMP-IDKST                              
226700     MOVE ORAD-KDDSP           TO KAMP-KDDSP                              
226800     MOVE OHUV-KDFAKTYP        TO KAMP-KDFAKTYP                           
226900     MOVE ARB-KDFRAKT          TO KAMP-KDFRAKT                            
227000     MOVE ORAD-KDKVBRYT        TO KAMP-KDKVBRYT                           
227100     MOVE ORAD-KDORDING        TO KAMP-KDORDING                           
227200     MOVE OHUV-KDORDKL         TO KAMP-KDORDKL                            
227300     MOVE AREG-KDPRODSL        TO KAMP-KDPRODSL                           
227400     MOVE ORAD-KDVRINFO        TO KAMP-KDVRINFO                           
227500     MOVE ORAD-KVBEART-Q       TO KAMP-KVBEART-Q                          
227600     MOVE AREG-REKSIFFR        TO KAMP-REKSIFFR                           
227700     MOVE ORAD-TITPO           TO KAMP-TITPO                              
227800     IF ORAD-KDPRTYP = 'P' OR ORAD-KDTPOTYP = +0                          
227900        MOVE ORAD-PRARTNTO     TO KAMP-PRARTNTO                           
228000        MOVE ORAD-DEAL-PR-LINE TO KAMP-DEAL-PR-LINE                       
228100        MOVE ORAD-KDPRTYP      TO KAMP-KDPRTYP                            
228200        MOVE ORAD-FLPRTILL     TO KAMP-FLPRTILL                           
228300     ELSE                                                                 
228400        MOVE ORAD-DEAL-PR-LINE TO KAMP-DEAL-PR-LINE                       
228500        MOVE ZERO              TO KAMP-PRARTNTO                           
228600        MOVE SPACE             TO KAMP-KDPRTYP                            
228700        MOVE NEJ               TO KAMP-FLPRTILL                           
228800     END-IF                                                               
228900     EJECT                                                                
229000     MOVE ORAD-BEVOLREF        TO KAMP-BEVOLREF                           
229100     MOVE ORAD-FLINVEST        TO KAMP-FLINVEST                           
229200     MOVE OHUV-BEKUNDRF        TO KAMP-BEKUNDRF                           
229300     MOVE ORAD-IDKAMPRF        TO KAMP-IDKAMPRF                           
229400     MOVE ORAD-IDDC            TO KAMP-IDDC                               
229500     MOVE ORAD-IDLEVNR         TO KAMP-IDLEVNR                            
229600     MOVE ORAD-IDSYSTEM        TO KAMP-IDSYSTEM                           
229700     MOVE AREG-KVFRYSTI        TO KAMP-KVFRYSTI                           
229800     MOVE ORAD-KDTPOTYP        TO KAMP-KDTPOTYP                           
229900     MOVE OHUV-FLFORBI         TO KAMP-FLFORBI                            
230000     MOVE OHUV-FLORDSPE        TO KAMP-FLORDSPE                           
230100     MOVE OHUV-FLOVRLEV        TO KAMP-FLOVRLEV                           
230200                                                                          
230300     MOVE +0                   TO KAMP-KDORDBEK                           
230400     MOVE SPACE                TO KAMP-FLKLAR                             
230500                                                                          
230600     CALL W411KAMP USING KAMP-W411KAMP KAMP-ORDP-PCB                      
230700                         KAMP-ZZAC-PCB KAMP-WDM2-PCB                      
230800                                                                          
230900     IF KAMP-KDORDBEK > +0                                                
231000        MOVE JA                TO OBKR-SW                                 
231100        MOVE NEJ               TO ALLT-SW                                 
231200        MOVE WC-CDC-SE         TO ORAD-IDDC                               
231300        MOVE ORAD-IDDC         TO WS-IDDC                                 
231400                                                                          
231500        IF KVAN-KDORDBEK-UT > +0                                          
231600           MOVE +0             TO KVAN-KDORDBEK-UT                        
231700           MOVE ORAD-KVBEART   TO ORAD-KVBEART-Q                          
231800        END-IF                                                            
231900        IF DLEV-KDORDBEK-UT > +0                                          
232000           MOVE +0             TO DLEV-KDORDBEK-UT                        
232100        END-IF                                                            
232200     ELSE                                                                 
232300        IF KAMP-FLKLAR = JA                                               
232400           MOVE NEJ            TO ALLT-SW                                 
232500           IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD                
232600              MOVE ZERO        TO KERS-KDORDBEK                           
232700              PERFORM S02-RENSA-TILLK-TAB                                 
232800           END-IF                                                         
232900        END-IF                                                            
233000     END-IF                                                               
233100                                                                          
233200     END-IF                                                               
233300     .                                                                    
233400     EJECT                                                                
233500 ECT-KOMPLETTERA-RELEASESPARR SECTION.                                    
233700     MOVE 'STA ECT-RELS     '              TO   WS-PGM-POSITION           
233800       IF ALLT-OK AND W-KDORDBEK = 56                                     
233900                                                                          
234000         MOVE ORAD-IDDISTR         TO RELS-IDDISTR                        
234100         MOVE ORAD-IDKUNDNR        TO RELS-IDKUNDNR                       
234200         MOVE ORAD-IDKUNDRF        TO RELS-IDKUNDRF                       
234300         MOVE ORAD-IDARTNR         TO RELS-IDARTNR                        
234400         MOVE ORAD-BERADREF        TO RELS-BERADREF                       
234500         MOVE AREG-IDANSK          TO RELS-IDANSK                         
234600         MOVE OHUV-IDKONTO         TO RELS-IDKONTO                        
234700         MOVE OHUV-IDKST           TO RELS-IDKST                          
234800         MOVE OHUV-IDANALYS        TO RELS-IDANALYS                       
234900         MOVE ORAD-KDDSP           TO RELS-KDDSP                          
235000         MOVE OHUV-KDFAKTYP        TO RELS-KDFAKTYP                       
235100         MOVE ARB-KDFRAKT          TO RELS-KDFRAKT                        
235200         MOVE ORAD-KDKVBRYT        TO RELS-KDKVBRYT                       
235300         MOVE ORAD-KDORDING        TO RELS-KDORDING                       
235400         MOVE OHUV-KDORDKL         TO RELS-KDORDKL                        
235500         MOVE AREG-KDPRODSL        TO RELS-KDPRODSL                       
235600         MOVE ORAD-KDVRINFO        TO RELS-KDVRINFO                       
235700         MOVE ORAD-KVBEART-Q       TO RELS-KVBEART-Q                      
235800         MOVE AREG-REKSIFFR        TO RELS-REKSIFFR                       
235900         MOVE MSGI-TILOKDAT        TO RELS-TITPO                          
236000         MOVE ORAD-PRARTNTO        TO RELS-PRARTNTO                       
236100         MOVE ORAD-PRAVCOST        TO RELS-PRAVCOST                       
236200         MOVE ORAD-DEAL-PR-LINE    TO RELS-DEAL-PR-LINE                   
236300         MOVE ORAD-KDPRTYP         TO RELS-KDPRTYP                        
236400         MOVE ORAD-FLPRTILL        TO RELS-FLPRTILL                       
236500         EJECT                                                            
236600         MOVE ORAD-BEVOLREF        TO RELS-BEVOLREF                       
236700         MOVE ORAD-IDKAMPRF        TO RELS-IDKAMPRF                       
236800         MOVE ORAD-IDSYSTEM        TO RELS-IDSYSTEM                       
236900         MOVE ORAD-FLINVEST        TO RELS-FLINVEST                       
237000         MOVE OHUV-FLORDSPE        TO RELS-FLORDSPE                       
237100         MOVE OHUV-FLOVRLEV        TO RELS-FLOVRLEV                       
237200         MOVE OHUV-FLFORBI         TO RELS-FLFORBI                        
237300         MOVE ORAD-IDLEVNR         TO RELS-IDLEVNR                        
237400         MOVE AREG-KDUART          TO RELS-KDUART                         
237500         MOVE AREG-KVFRYSTI        TO RELS-KVFRYSTI                       
237600         MOVE +1                   TO RELS-KDORDBEH                       
237700         MOVE W-KDTPOTYP           TO RELS-KDTPOTYP                       
237800         MOVE OHUV-BEKUNDRF        TO RELS-BEKUNDRF                       
237900         MOVE ORAD-FLTILLK         TO RELS-FLTILLK                        
238000         MOVE AREG-TIDISPIN        TO RELS-TIDISPIN                       
238100         MOVE OHUV-BEVARREF        TO RELS-BEVARREF                       
238200         MOVE 0                    TO RELS-KVQPACK-1                      
238300         MOVE ORAD-KVBEART         TO RELS-KVBEART                        
238400         MOVE W-KDORDBEK           TO RELS-KDORDBEK                       
238500                                                                          
238600         MOVE SPACE                TO RELS-FLKLAR                         
238700         MOVE OHUV-KDORDTYP-LDC    TO RELS-KDORDTYP-LDC                   
238800         MOVE OHUV-TIREPDAT        TO RELS-TIREPDAT                       
238900         MOVE ORAD-IDKUNDRF-WIP    TO RELS-IDKUNDRF-WIP                   
239000                                                                          
239100         CALL W411RELS USING RELS-W411RELS RELS-ORDP-PCB                  
239200                             RELS-FILA-PCB RELS-ARTM-PCB 2109-PCB         
239300                                                                          
239400         PERFORM ECTA-ANDRA-WDC711                                        
239500         IF RELS-KDORDBEK > +0                                            
239600            MOVE JA                TO OBKR-SW                             
239700            MOVE NEJ               TO ALLT-SW                             
239800            MOVE WC-CDC-SE         TO ORAD-IDDC                           
239900            MOVE ORAD-IDDC         TO WS-IDDC                             
240000         ELSE                                                             
240100            IF RELS-FLKLAR = JA                                           
240200               MOVE NEJ            TO ALLT-SW                             
240300               IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD            
240400                  MOVE ZERO        TO KERS-KDORDBEK                       
240500                  PERFORM S02-RENSA-TILLK-TAB                             
240600               END-IF                                                     
240700            END-IF                                                        
240800         END-IF                                                           
240900         MOVE +0                   TO W-KDORDBEK                          
241000         MOVE SPACE                TO RELS-FLKLAR                         
241100       END-IF                                                             
241200     .                                                                    
241300     EJECT                                                                
241400 ECTA-ANDRA-WDC711 SECTION.                                               
241500                                                                          
241600     IF W-KDTPOTYP = 7 AND RELS-FLOVRLEV NOT = JA                         
241700       IF ORAD-IDDISTR = 0778 AND OHUV-KDORDKL < 3                        
241800         INITIALIZE PRQU-W335PRQU                                         
241900         MOVE ORAD-IDDISTR       TO PRQU-IDDISTR                          
242000         MOVE ORAD-IDKUNDNR      TO PRQU-IDKUNDNR                         
242100         MOVE W-IDKUNDRF         TO PRQU-IDKUNDRF                         
242200         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
242300         MOVE 6                  TO PRQU-KDCALL                           
242400         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
242500                                            PRQU-WDC7-PCB                 
242600                                            PRQU-SJKO-WDK6-PCB            
242700         MOVE 'N'                TO ORAD-FLPRTILL                         
242800       END-IF                                                             
242900     END-IF                                                               
243000     .                                                                    
243100     EJECT                                                                
243200 ECG-PREL-AVBOKNING-XDC SECTION.                                          
243300                                                                          
243400     MOVE 'STA ECG-XDC      '              TO   WS-PGM-POSITION           
243500     IF ALLT-OK OR KOLLA-ERS OR SPAR-FLPUBCDC = YES                       
243600                                                                          
243700       PERFORM S10-HAMTA-WDB6-INFO                                        
243800                                                                          
243900       IF DCS-NDC                                                         
244000*       IF OHUV-IDUSER = 'PCCN616'                                        
244100           MOVE JA TO ALLT-SW                                             
244200           PERFORM ECGX-PREL-AVBOKNING-XDC                                
244300*       END-IF                                                            
244400                                                                          
244500*        MOVE OHUV-FLFORBI         TO NDCA-FLFORBI                        
244600*        MOVE GMT-FLLDCKND         TO NDCA-FLLDCKND                       
244700*        MOVE OHUV-FLPRELRO        TO NDCA-FLPRELRO                       
244800*        MOVE OHUV-FLRESTN         TO NDCA-FLRESTN                        
244900*        MOVE OHUV-FLORDSPE        TO NDCA-FLORDSPE                       
245000*        MOVE ORAD-IDARTNR         TO NDCA-IDARTNR                        
245100*        MOVE ORAD-IDDC            TO NDCA-IDDC                           
245200*        MOVE OHUV-IDDC-CLEAR-GRP  TO NDCA-IDDC-CLEAR-GRP                 
245300*        MOVE ORAD-CLEARGROUP      TO NDCA-CLEARGROUP                     
245400*        MOVE OHUV-IDDC-TVS        TO NDCA-IDDC-TVS                       
245500*        MOVE ORAD-IDDISTR         TO NDCA-IDDISTR                        
245600*        MOVE WS-IXDCCLEAR         TO NDCA-IXDCCLEAR                      
245700*        MOVE ORAD-KDARTURS        TO NDCA-KDARTURS                       
245800*        MOVE AREG-KDERS           TO NDCA-KDERS                          
245900*        MOVE ORAD-KDORDING        TO NDCA-KDORDING                       
246000*        MOVE ORAD-KDORDKL         TO NDCA-KDORDKL                        
246010*        MOVE AREG-KDSORT          TO NDCA-KDSORT                         
246020*        MOVE OHUV-KVDAGAR-DOW     TO NDCA-KVDAGAR-DOW                    
246030*        MOVE ORAD-KVBEART-Q       TO NDCA-KVBEART-Q                      
246040*        MOVE AREG-KVQPACK-1       TO NDCA-KVQPACK-1                      
246050*        MOVE ORAD-TIREGDAT        TO NDCA-TIREGDAT                       
246060*        MOVE ORAD-TIREGTID        TO NDCA-TIREGTID                       
246070*        MOVE ORAD-VKART           TO NDCA-VKART                          
246080*        MOVE ORAD-VKART-NTO       TO NDCA-VKART-NTO                      
246090*        MOVE ORAD-VLARTNTO        TO NDCA-VLARTNTO                       
246400*        MOVE +2                   TO NDCA-KDCALL                         
246500*        MOVE SPACE                TO NDCA-XDK7-IDDC                      
246600*        MOVE ZERO                 TO NDCA-XDK7-KDIDDC                    
246700*                                     NDCA-XDK7-KVOKS-DAG                 
246710*                                     NDCA-XDK7-KVOKS-BULK                
246720*                                                                         
246730*        CALL W411NDCA USING NDCA-W411NDCA CLDC-W411CLDC                  
246740*                                          NDCA-USEA-PCB                  
246750*                                          NDCA-WDK7-PCB                  
246760*                                          NDCA-WDL6-PCB                  
246770*                                          NDCA-WDB6-PCB                  
246780*                                          NDCA-XDK7-W411XDK7             
246790*                                                                         
246900*          PERFORM ECGX-CHECK-DIFF                                        
247100         IF XDCA-KDORDBEK > ZERO                                          
247200           IF SPAR-FLPUBCDC = YES                                         
247300*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR CDC PUBL.DATE            
247400*** IF XDCA-KDORDBEK = 15 MEANS LINE IS MOVED TO ANOTHER DC               
247500*** THEN DAPUBL IS TESTED OK IN W411XDCA                                  
247510              IF (XDCA-KDORDBEK = 15 OR 92 OR 98 OR 99)                   
247511                 AND XDCA-DAPUBL > ZERO                                   
247512                 MOVE ZERO TO SPAR-KDORDBEK                               
247513              ELSE                                                        
247515                 MOVE ZERO TO XDCA-KDORDBEK                               
247516              END-IF                                                      
247517           END-IF                                                         
247518           IF KOLLA-ERS                                                   
247521              IF XDCA-KVPREAVB > 0                                        
247522                MOVE ZERO            TO KERS-KDORDBEK                     
247523                PERFORM S02-RENSA-TILLK-TAB                               
247530                MOVE ZERO            TO SPAR-KDORDBEK                     
247540              ELSE                                                        
247550                MOVE ZERO            TO XDCA-KDORDBEK                     
247560              END-IF                                                      
247570           ELSE                                                           
247580             IF XDCA-KDORDBEK = 15                                        
247590                IF SDCA-KDORDBEK-FIRST-SDC = 15                           
247600                   MOVE ZERO         TO SDCA-KDORDBEK-FIRST-SDC           
247700                END-IF                                                    
247800                IF SDCA-KDORDBEK-SECOND-SDC = 15                          
247900                   MOVE ZERO         TO SDCA-KDORDBEK-SECOND-SDC          
248000                END-IF                                                    
248100                IF SDCA-KDORDBEK = 15                                     
248200                   MOVE ZERO         TO SDCA-KDORDBEK                     
248300                END-IF                                                    
248400             END-IF                                                       
248500             IF KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD              
248600                 MOVE ZERO           TO XDCA-KDORDBEK                     
248700             END-IF                                                       
248800           END-IF                                                         
248900           MOVE JA                   TO OBKR-SW                           
249000         ELSE                                                             
249100           IF KOLLA-ERS    OR                                             
249200             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
249300             IF XDCA-KVPREAVB > 0                                         
249410               MOVE ZERO             TO KERS-KDORDBEK                     
249500               PERFORM S02-RENSA-TILLK-TAB                                
249600               MOVE ZERO             TO SPAR-KDORDBEK                     
249700             ELSE                                                         
249800               MOVE JA               TO OBKR-SW                           
249900             END-IF                                                       
250000           ELSE                                                           
250001            IF XDCA-KVPREAVB > 0 AND SPAR-KDORDBEK ='54'                  
250002                                                                          
250003               MOVE ZERO             TO SPAR-KDORDBEK                     
250010            END-IF                                                        
250020           END-IF                                                         
250100           IF XDCA-DAPUBL > 0 AND SPAR-FLPUBCDC = YES                     
250200*** SPAR-FLPUBCDC=YES MEANS THERE IS A BLOCK FOR PUBL.DATE ON CDC         
250300*** THAT BLOCK SHALL BE REMOVED IF CHECK ON DAPUBL FOR NDC IS OK.         
250400              MOVE 0  TO SPAR-KDORDBEK                                    
250500              MOVE JA  TO ALLT-SW                                         
250600              MOVE NEJ TO OBKR-SW                                         
250700           END-IF                                                         
250800         END-IF                                                           
250900         MOVE XDCA-ADLAGOMR          TO ORAD-ADLAGOMR                     
251000         MOVE XDCA-ADGANG            TO ORAD-ADGANG                       
251100         MOVE XDCA-ADPLATS           TO ORAD-ADPLATS                      
251200         MOVE XDCA-IDDC-OUT          TO ORAD-IDDC                         
251300         MOVE XDCA-IDDC-RO           TO ORAD-IDDC-RO                      
251400         MOVE XDCA-KDARTURS          TO ORAD-KDARTURS                     
251500         MOVE XDCA-KDOI              TO ORAD-KDOI                         
251600         MOVE XDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
251700         MOVE XDCA-KVPREAVB          TO ORAD-KVPREAVB                     
251800         MOVE XDCA-KVPRERO           TO ORAD-KVPRERO                      
251900         MOVE XDCA-TIREGDAT-OUT      TO ORAD-TIREGDAT                     
252000         MOVE XDCA-TIREGTID-OUT      TO ORAD-TIREGTID                     
252100         MOVE XDCA-VKART-OUT         TO ORAD-VKART                        
252200         MOVE XDCA-VKART-NTO         TO ORAD-VKART-NTO                    
252300         MOVE XDCA-VLARTNTO          TO ORAD-VLARTNTO                     
252400         MOVE NEJ                    TO ALLT-SW                           
252500       END-IF                                                             
252600                                                                          
252700     END-IF                                                               
252800     .                                                                    
252900     EJECT                                                                
253000 ECGX-PREL-AVBOKNING-XDC SECTION.                                         
253100                                                                          
253200     MOVE 'STA ECGX-XDC      '       TO   WS-PGM-POSITION                 
253210     IF ALLT-OK OR KOLLA-ERS                                              
253220                                                                          
253230       PERFORM S10-HAMTA-WDB6-INFO                                        
253240                                                                          
253250       IF DCS-NDC                                                         
253260                                                                          
253270* XDCA-INPUT                                                              
253280         MOVE +1 TO WS-INDEX                                              
253281         PERFORM UNTIL WS-INDEX > IX-DCCLEAR-MAX                          
253282           MOVE W-GMT-IDDC-CLEAR  (WS-INDEX)                              
253283                                   TO XDCA-IDDC-CLEAR-IN(WS-INDEX)        
253284           ADD +1 TO WS-INDEX                                             
253285         END-PERFORM                                                      
253286                                                                          
253287         MOVE OHUV-IDDC-TVS        TO XDCA-IDDC-TVS                       
253290         MOVE GMT-FLLDCKND         TO XDCA-FLLDCKND                       
253291         MOVE ORAD-IDARTNR         TO XDCA-IDARTNR                        
253292         MOVE ORAD-IDDC            TO XDCA-IDDC                           
253293         MOVE ORAD-IDLEVNR         TO XDCA-IDLEVNR                        
253294         MOVE AREG-KDERS           TO XDCA-KDERS                          
253295         MOVE AREG-KDSORT          TO XDCA-KDSORT                         
253296         MOVE ORAD-KVBEART-Q       TO XDCA-KVBEART-Q                      
253297         MOVE AREG-KVQPACK-1       TO XDCA-KVQPACK-1                      
253298         MOVE ORAD-TIREGDAT        TO XDCA-TIREGDAT                       
253299         MOVE ORAD-TIREGTID        TO XDCA-TIREGTID                       
253300         MOVE ORAD-VKART           TO XDCA-VKART                          
253301         MOVE +1                   TO XDCA-KDCALL                         
253302                                                                          
253303* XDCA-OUTPUT                                                             
253304         MOVE SPACE                TO XDCA-IDDC-OUT                       
253305                                      XDCA-IDDC-RO                        
253306                                      XDCA-KDARTURS                       
253307                                      XDCA-KDOI                           
253308                                      XDCA-CLEARGROUP                     
253309         MOVE ZERO                 TO XDCA-ADLAGOMR                       
253310                                      XDCA-ADGANG                         
253320                                      XDCA-ADPLATS                        
253330                                      XDCA-KDORDBEK                       
253340                                      XDCA-KVPREAVB                       
253350                                      XDCA-KVPRERO                        
253360                                      XDCA-TIREGDAT-OUT                   
253370                                      XDCA-TIREGTID-OUT                   
253371                                      XDCA-VKART-OUT                      
253372                                      XDCA-VKART-NTO                      
253373                                      XDCA-VLARTNTO                       
253375         MOVE ZERO                 TO                                     
253376                                      XDCA-KVOKS-DAG                      
253377                                      XDCA-KVOKS-BULK                     
253378                                                                          
253379         IF XDCA-DAPUBL NOT = 99999999                                    
253380            MOVE ZERO              TO XDCA-DAPUBL                         
253381         END-IF                                                           
253382                                                                          
253383         CALL W411XDCA USING OHUV-WDQ201 XDCA-W411XDCA                    
253384         XDCA-USEA-PCB                                                    
253385         XDCA-WDB6-PCB XDCA-WDK6-PCB XDCA-WDK7-PCB                        
253390         XDCA-WDK9-PCB XDCA-WDL6-PCB XDCA-WDQ4B-PCB                       
253391         XDCA-WDQ2-PCB XDCA-WDQ4-PCB XDCA-WDR6-PCB                        
253392         XDCA-WDB6-2-PCB XDCA-WDK6-2-PCB XDCA-WDK7-2-PCB                  
253393         XDCA-WDK7-3-PCB                                                  
253394                                                                          
253395* THIS IS JUST TO BE ABLE TO COMPARE THE RESULT WITH VALUES               
253396* FROM NDCA. IF VALU NOT = SPACE ORAD- VALUES SHULD BE OVERRIDDEN         
253397         IF XDCA-KDARTURS = SPACE                                         
253398           MOVE ORAD-KDARTURS  TO XDCA-KDARTURS                           
253399         END-IF                                                           
253400         IF XDCA-VKART-NTO = ZERO                                         
253401           MOVE ORAD-VKART-NTO TO XDCA-VKART-NTO                          
253402         END-IF                                                           
253403         IF XDCA-VLARTNTO = ZERO                                          
253404           MOVE ORAD-VLARTNTO  TO XDCA-VLARTNTO                           
253405         END-IF                                                           
253406       END-IF                                                             
253407     END-IF                                                               
253408     .                                                                    
253409     EJECT                                                                
253410 ECGX-CHECK-DIFF SECTION.                                                 
253411                                                                          
253412     MOVE 'CHECK-DIFF       '              TO   WS-PGM-POSITION           
253420     IF  NDCA-KDORDBEK   = XDCA-KDORDBEK                                  
253430     AND NDCA-KVPREAVB   = XDCA-KVPREAVB                                  
253440     AND NDCA-ADLAGOMR   = XDCA-ADLAGOMR                                  
253450     AND NDCA-ADGANG     = XDCA-ADGANG                                    
253460     AND NDCA-ADPLATS    = XDCA-ADPLATS                                   
253470     AND NDCA-IDDC       = XDCA-IDDC-OUT                                  
253480     AND NDCA-IDDC-RO    = XDCA-IDDC-RO                                   
253490     AND NDCA-KDARTURS   = XDCA-KDARTURS                                  
253500     AND NDCA-KDOI       = XDCA-KDOI                                      
253510     AND NDCA-KVPRERO    = XDCA-KVPRERO                                   
253511     AND NDCA-VKART      = XDCA-VKART-OUT                                 
253512     AND NDCA-VKART-NTO  = XDCA-VKART-NTO                                 
253513     AND NDCA-VLARTNTO   = XDCA-VLARTNTO                                  
253514     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
253515     AND NDCA-CLEARGROUP = XDCA-CLEARGROUP                                
253516     AND XDCA-KVOKS-DAG     = NDCA-XDK7-KVOKS-DAG                         
253517     AND XDCA-KVOKS-BULK    = NDCA-XDK7-KVOKS-BULK                        
253518         MOVE NEJ TO DIFF-FLSVAR                                          
253519     ELSE                                                                 
253520        MOVE JA           TO DIFF-FLSVAR                                  
253521     END-IF                                                               
253522                                                                          
253523* ORDER LOG INFO                                                          
253524     IF DIFF-FLSVAR = JA                                                  
253525       MOVE IDPGM         TO FIL-IDPGM                                    
253526       ACCEPT FIL-TIREGDAT FROM DATE                                      
253527       ACCEPT FIL-TIKLOCK FROM TIME                                       
253528       MOVE 1             TO FIL-IDSEKVNR                                 
253529       MOVE 'W414'        TO FIL-CT-IDSYSTEM                              
253530       MOVE 'A'           TO FIL-CT-IDVTYP                                
253531       MOVE 'XDC'         TO FIL-CT-IDPTYP                                
253532                                                                          
253533*   ORDER LINE INFO                                                       
253534       MOVE OHUV-IDDISTR   TO DIFF-IDDISTR                                
253535       MOVE OHUV-IDKUNDNR  TO DIFF-IDKUNDNR                               
253536       MOVE OHUV-IDORDNR7  TO DIFF-IDORDNR5                               
253537       MOVE OHUV-KDORDKL   TO DIFF-KDORDKL                                
253538       MOVE ORAD-IDARTNR   TO DIFF-IDARTNR                                
253539       MOVE ORAD-KVBEART-Q TO DIFF-KVBEART-Q                              
253540       MOVE ORAD-IDDC      TO DIFF-IDDC                                   
253541       MOVE '4212'         TO DIFF-IDSYSTEM                               
253542                                                                          
253543*   NDCA INFO                                                             
253544       MOVE NDCA-XDK7-KVOKS-DAG TO DIFF-KVOKS-DAG-NDCA                    
253545       MOVE NDCA-XDK7-KVOKS-BULK TO DIFF-KVOKS-BULK-NDCA                  
253546       MOVE NDCA-KDORDBEK TO DIFF-KDORDBEK-NDCA                           
253547       MOVE NDCA-KVPREAVB TO DIFF-KVPREAVB-NDCA                           
253548       MOVE NDCA-ADLAGOMR TO DIFF-ADLAGOMR-NDCA                           
253549       MOVE NDCA-ADGANG   TO DIFF-ADGANG-NDCA                             
253550       MOVE NDCA-ADPLATS  TO DIFF-ADPLATS-NDCA                            
253551       MOVE NDCA-IDDC     TO DIFF-IDDC-NDCA                               
253552       MOVE NDCA-IDDC-RO  TO DIFF-IDDC-RO-NDCA                            
253553       MOVE NDCA-KDARTURS TO DIFF-KDARTURS-NDCA                           
253554       MOVE NDCA-KDOI     TO DIFF-KDOI-NDCA                               
253555       MOVE NDCA-KVPRERO  TO DIFF-KVPRERO-NDCA                            
253556       MOVE NDCA-VKART    TO DIFF-VKART-NDCA                              
253557       MOVE NDCA-VKART-NTO TO DIFF-VKART-NTO-NDCA                         
253558       MOVE NDCA-VLARTNTO TO DIFF-VLARTNTO-NDCA                           
253559       MOVE NDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-NDCA                     
253560                                                                          
253570*   XDCA INFO                                                             
253580       MOVE XDCA-KVOKS-DAG TO DIFF-KVOKS-DAG-XDCA                         
253590       MOVE XDCA-KVOKS-BULK TO DIFF-KVOKS-BULK-XDCA                       
253600       MOVE XDCA-KDORDBEK TO DIFF-KDORDBEK-XDCA                           
253610       MOVE XDCA-KVPREAVB TO DIFF-KVPREAVB-XDCA                           
253620       MOVE XDCA-ADLAGOMR TO DIFF-ADLAGOMR-XDCA                           
253621       MOVE XDCA-ADGANG   TO DIFF-ADGANG-XDCA                             
253622       MOVE XDCA-ADPLATS  TO DIFF-ADPLATS-XDCA                            
253623       MOVE XDCA-IDDC-OUT TO DIFF-IDDC-XDCA                               
253624       MOVE XDCA-IDDC-RO  TO DIFF-IDDC-RO-XDCA                            
253625       MOVE XDCA-KDARTURS TO DIFF-KDARTURS-XDCA                           
253626       MOVE XDCA-KDOI     TO DIFF-KDOI-XDCA                               
253627       MOVE XDCA-KVPRERO  TO DIFF-KVPRERO-XDCA                            
253628       MOVE XDCA-VKART-OUT TO DIFF-VKART-XDCA                             
253629       MOVE XDCA-VKART-NTO TO DIFF-VKART-NTO-XDCA                         
253630       MOVE XDCA-VLARTNTO TO DIFF-VLARTNTO-XDCA                           
253631       MOVE XDCA-CLEARGROUP TO DIFF-OI-CLEAR-GRP-XDCA                     
253632                                                                          
253633       PERFORM IMS-ISRT-WDR601                                            
253634       IF SEGMENT-FINNS-REDAN                                             
253635          PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                           
253636             ADD 1 TO FIL-IDSEKVNR                                        
253637             PERFORM IMS-ISRT-WDR601                                      
253638          END-PERFORM                                                     
253639       END-IF                                                             
253640     END-IF                                                               
253641     .                                                                    
253642     EJECT                                                                
253643 ECW-PREL-AVBOKNING-SDC1 SECTION.                                         
253644                                                                          
253645     MOVE 'STA ECW-SDC1     '              TO   WS-PGM-POSITION           
253646                                                                          
253648                                                                          
253649     IF ALLT-OK OR KOLLA-ERS                                              
253650                                                                          
253660       PERFORM S10-HAMTA-WDB6-INFO                                        
253670                                                                          
253680       IF DCS-SDC                                                         
253690                                                                          
253700         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
253800         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
253900         MOVE NEJ                  TO SDCA-FLORDSPE                       
254000         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
254100         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
254200         MOVE ORAD-IDDC            TO SDCA-IDDC                           
254300         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
254400         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
254500         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
254600         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
254700         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
254800         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
254900         MOVE AREG-KDSORT          TO SDCA-KDSORT                         
255000         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
255100         MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                      
255200         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
255300         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
255400         MOVE +0                   TO SDCA-TIREPDAT                       
255500         MOVE +0                   TO SDCA-KVOKS-PREL                     
255600         MOVE +1                   TO SDCA-KDCALL                         
255700         MOVE +1                   TO SDCA-IXDCCLEAR                      
255900         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
256000                             SDCA-WDB6-PCB SDCA-WDK9-PCB                  
256100                             SDCA-WDR6-PCB SDCA-WDK6-PCB                  
256200                             SDCA-WDQ4B-PCB SDCA-WDQ2-PCB                 
256300                             SDCA-WDQ4-PCB                                
256400                             SDCA-WDB6-2-PCB                              
256410                             SDCA-WDK6-2-PCB                              
256420                             SDCA-WDK7-2-PCB                              
256430                             SDCA-WDK7-3-PCB                              
256500         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-FIRST-SDC                    
256600         MOVE ZERO          TO SDCA-KDORDBEK                              
256700                                                                          
256800         IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                
256900           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-FIRST-SDC = 80         
257000*            IF OHUV-IDDC-CLEAR(2) > '19'                                 
257100*               MOVE ZERO               TO SDCA-KDORDBEK-FIRST-SDC        
257200*               MOVE OHUV-IDDC-CLEAR(2) TO ORAD-IDDC                      
257300*               MOVE ORAD-IDDC          TO WS-IDDC                        
257400*               IF SDCA-KDOI = 'XX'                                       
257500*                  MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)            
257600*               END-IF                                                    
257700*            ELSE                                                         
257800                MOVE JA        TO OBKR-SW                                 
257900                MOVE NEJ       TO ALLT-SW                                 
258000*            END-IF                                                       
258100           ELSE                                                           
258200             IF ORAD-KDORDKL > 0                                          
258300             AND ORAD-IDSYSTEM NOT = 'OREL'                               
258400             AND (AREG-KDUART = 'L'                                       
258500             OR AREG-KDUART = 'P')                                        
258600             AND OHUV-FLORDSPE NOT = JA                                   
258700             AND OHUV-FLOVRLEV NOT = JA                                   
258800             AND OHUV-FLFORBI = NEJ                                       
259000                MOVE ZERO    TO SDCA-KDORDBEK-FIRST-SDC                   
259100                MOVE 70      TO TPO2-KDORDBEK                             
259200                MOVE JA      TO OBKR-SW                                   
259300                MOVE NEJ     TO ALLT-SW                                   
259400                MOVE WC-CDC-SE     TO ORAD-IDDC                           
259500                MOVE ORAD-IDDC     TO WS-IDDC                             
259600                MOVE 6       TO ORAD-KDTPOTYP                             
259700                IF ORAD-KDPRTYP NOT = 'P'                                 
259800                   MOVE ZERO  TO ORAD-PRARTNTO                            
259900                   MOVE SPACE TO ORAD-KDPRTYP                             
260000**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
260100                   IF NOT DIST79-DEALER-PRICE                             
260300                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
260400                     MOVE NEJ         TO ORAD-FLPRTILL                    
260500                   END-IF                                                 
260600*************                                                             
260700                END-IF                                                    
260800             ELSE                                                         
260900               IF KOLLA-ERS                                               
261000*                 MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC         
261710                  IF SPAR-KDORDBEK = ZERO                                 
261720                     IF ORAD-IDDC = W-TILLK-DC                            
261730                        MOVE JA        TO OBKR-SW                         
261740                        MOVE NEJ       TO ALLT-SW                         
261750                                          KOLLA-ERS-SW                    
261760                        MOVE ZERO      TO SDCA-KDORDBEK-FIRST-SDC         
261770                     ELSE                                                 
261790                        MOVE W-GMT-IDDC-CLEAR(2)                          
261791                                       TO ORAD-IDDC                       
261792                        MOVE ORAD-IDDC TO WS-IDDC                         
261793                        IF WS-IDDC = WC-CDC-SE                            
261794                           MOVE JA     TO CDC-MOVE-SW                     
261795                        END-IF                                            
261796                     END-IF                                               
261797                  ELSE                                                    
261798                     MOVE NEJ          TO KOLLA-ERS-SW                    
261799                  END-IF                                                  
261800               ELSE                                                       
261900                 IF SDCA-KDORDBEK-FIRST-SDC = 15                          
262100                    MOVE W-GMT-IDDC-CLEAR(2) TO ORAD-IDDC                 
262200                    MOVE ORAD-IDDC          TO WS-IDDC                    
262300                    MOVE JA                 TO OBKR-SW                    
262400                 ELSE                                                     
262500                    MOVE JA            TO OBKR-SW                         
262600                    MOVE NEJ           TO ALLT-SW                         
262700                 END-IF                                                   
262800               END-IF                                                     
262900             END-IF                                                       
263000           END-IF                                                         
263100         ELSE                                                             
263200           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
263300           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
263400           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
263500           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
263600           MOVE NEJ                  TO ALLT-SW                           
264000           IF KVAN-KDORDBEK-UT = ZERO                                     
264100             MOVE JA                 TO EGET-CL-RAD-SW                    
264200           END-IF                                                         
264300           IF KOLLA-ERS    OR                                             
264400             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
264500              MOVE NEJ               TO KOLLA-ERS-SW                      
264600              MOVE ZERO              TO KERS-KDORDBEK                     
264700              PERFORM S02-RENSA-TILLK-TAB                                 
264800              MOVE ZERO              TO SPAR-KDORDBEK                     
264900           END-IF                                                         
264910           IF KERS-KDERS = +19 OR +29                                     
264920              MOVE ZERO              TO SPAR-KDORDBEK                     
264930           END-IF                                                         
265000         END-IF                                                           
265100         MOVE SDCA-KDOI              TO ORAD-KDOI                         
265200         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
265300       END-IF                                                             
265400                                                                          
265500     END-IF                                                               
265600     .                                                                    
265700     EJECT                                                                
265800 ECH-PREL-AVBOKNING-SDC2 SECTION.                                         
266000     MOVE 'STA ECH-SDC2     '              TO   WS-PGM-POSITION           
266100     IF ALLT-OK OR KOLLA-ERS                                              
266200                                                                          
266300       PERFORM S10-HAMTA-WDB6-INFO                                        
266400                                                                          
266500       IF DCS-SDC                                                         
266600                                                                          
266700         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
266800         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
266900         MOVE NEJ                  TO SDCA-FLORDSPE                       
267000         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
267100         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
267200         MOVE ORAD-IDDC            TO SDCA-IDDC                           
267300         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
267400         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
267500         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
267600         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
267700         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
267800         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
267900         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
268000         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
268100         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
268200         MOVE +1                   TO SDCA-KDCALL                         
268300         MOVE +2                   TO SDCA-IXDCCLEAR                      
268400                                                                          
268500         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
268600                             SDCA-WDB6-PCB SDCA-WDK9-PCB                  
268700                             SDCA-WDR6-PCB SDCA-WDK6-PCB                  
268800                             SDCA-WDQ4B-PCB SDCA-WDQ2-PCB                 
268900                             SDCA-WDQ4-PCB                                
268910                             SDCA-WDB6-2-PCB                              
268920                             SDCA-WDK6-2-PCB                              
268930                             SDCA-WDK7-2-PCB                              
268940                             SDCA-WDK7-3-PCB                              
269000                                                                          
269100         MOVE SDCA-KDORDBEK TO SDCA-KDORDBEK-SECOND-SDC                   
269200         MOVE ZERO          TO SDCA-KDORDBEK                              
269300                                                                          
269400         IF SDCA-KDORDBEK-SECOND-SDC > ZERO                               
269500           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK-SECOND-SDC = 80        
269600*            IF OHUV-IDDC-CLEAR(3) > '19'                                 
269700*               MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC          
269800*               MOVE OHUV-IDDC-CLEAR(3) TO ORAD-IDDC                      
269900*               MOVE ORAD-IDDC          TO WS-IDDC                        
270000*               IF SDCA-KDOI = 'XX'                                       
270100*                  MOVE JA     TO SDCA-FLCLEAR(SDCA-IXDCCLEAR)            
270200*               END-IF                                                    
270300*            ELSE                                                         
270400                MOVE JA        TO OBKR-SW                                 
270500                MOVE NEJ       TO ALLT-SW                                 
270600*            END-IF                                                       
270700           ELSE                                                           
270800             IF ORAD-KDORDKL > 0                                          
270900             AND ORAD-IDSYSTEM NOT = 'OREL'                               
271000             AND (AREG-KDUART = 'L'                                       
271100             OR AREG-KDUART = 'P')                                        
271200             AND OHUV-FLORDSPE NOT = JA                                   
271300             AND OHUV-FLOVRLEV NOT = JA                                   
271400             AND OHUV-FLFORBI = NEJ                                       
271500*            AND NOT DCS-CHINA                                            
271600                MOVE ZERO    TO SDCA-KDORDBEK-SECOND-SDC                  
271700                MOVE 70      TO TPO2-KDORDBEK                             
271800                MOVE JA      TO OBKR-SW                                   
271900                MOVE NEJ     TO ALLT-SW                                   
272000                MOVE WC-CDC-SE     TO ORAD-IDDC                           
272100                MOVE ORAD-IDDC     TO WS-IDDC                             
272200                MOVE 6       TO ORAD-KDTPOTYP                             
272300                IF ORAD-KDPRTYP NOT = 'P'                                 
272400                   MOVE ZERO  TO ORAD-PRARTNTO                            
272500                   MOVE SPACE TO ORAD-KDPRTYP                             
272600**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
272700                   IF NOT DIST79-DEALER-PRICE                             
272900                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
273000                     MOVE NEJ         TO ORAD-FLPRTILL                    
273100                   END-IF                                                 
273200*************                                                             
273300                END-IF                                                    
273400             ELSE                                                         
273500               IF KOLLA-ERS                                               
273600*                 MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC        
273700                  MOVE ZERO            TO SDCA-KDORDBEK-FIRST-SDC         
273800                  IF SPAR-KDORDBEK = ZERO                                 
273900                     IF ORAD-IDDC = W-TILLK-DC                            
274000                       MOVE JA         TO OBKR-SW                         
274100                       MOVE NEJ        TO ALLT-SW                         
274200                                          KOLLA-ERS-SW                    
274300                       MOVE ZERO       TO SDCA-KDORDBEK-SECOND-SDC        
274310                     ELSE                                                 
274330                       MOVE W-GMT-IDDC-CLEAR(3)                           
274331                                       TO ORAD-IDDC                       
274340                       MOVE ORAD-IDDC  TO WS-IDDC                         
274350                       IF WS-IDDC = WC-CDC-SE                             
274360                          MOVE JA      TO CDC-MOVE-SW                     
274370                       END-IF                                             
274380                     END-IF                                               
274390                  ELSE                                                    
274391                     MOVE NEJ          TO KOLLA-ERS-SW                    
274392                  END-IF                                                  
274400               ELSE                                                       
274500                 IF SDCA-KDORDBEK-SECOND-SDC = 15                         
274600                    IF SDCA-KDORDBEK-FIRST-SDC = 15                       
274700                       MOVE ZERO TO SDCA-KDORDBEK-FIRST-SDC               
274800                    END-IF                                                
275000                    MOVE W-GMT-IDDC-CLEAR(3) TO ORAD-IDDC                 
275100                    MOVE ORAD-IDDC          TO WS-IDDC                    
275200                    MOVE JA                 TO OBKR-SW                    
275300                 ELSE                                                     
275400                    MOVE JA            TO OBKR-SW                         
275500                    MOVE NEJ           TO ALLT-SW                         
275600                 END-IF                                                   
275700               END-IF                                                     
275800             END-IF                                                       
275900           END-IF                                                         
276000         ELSE                                                             
276100           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
276200           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
276300           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
276400           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
276500           MOVE NEJ                  TO ALLT-SW                           
276900           IF KVAN-KDORDBEK-UT = ZERO                                     
277000           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
277100             MOVE JA                 TO EGET-CL-RAD-SW                    
277200           END-IF                                                         
277300           IF KOLLA-ERS    OR                                             
277400             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
277500              MOVE NEJ               TO KOLLA-ERS-SW                      
277600              MOVE ZERO              TO KERS-KDORDBEK                     
277700              PERFORM S02-RENSA-TILLK-TAB                                 
277800              MOVE ZERO              TO SPAR-KDORDBEK                     
277900           END-IF                                                         
277910           IF KERS-KDERS = +19 OR +29                                     
277920              MOVE ZERO              TO SPAR-KDORDBEK                     
277930           END-IF                                                         
278000         END-IF                                                           
278100         MOVE SDCA-KDOI              TO ORAD-KDOI                         
278200         MOVE SDCA-CLEARGROUP        TO ORAD-CLEARGROUP                   
278300       END-IF                                                             
278400                                                                          
278500     END-IF                                                               
278600     .                                                                    
278700     EJECT                                                                
278800 ECX-PREL-AVBOKNING-SDC3 SECTION.                                         
279000     MOVE 'STA ECX-SDC3     '              TO   WS-PGM-POSITION           
279100     IF ALLT-OK OR KOLLA-ERS                                              
279200                                                                          
279300       PERFORM S10-HAMTA-WDB6-INFO                                        
279400                                                                          
279500       IF DCS-SDC                                                         
279600                                                                          
279700         MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                        
279800         MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                        
279900         MOVE NEJ                  TO SDCA-FLORDSPE                       
280000         MOVE AREG-FLREFILL        TO SDCA-FLREFILL                       
280100         MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                        
280200         MOVE ORAD-IDDC            TO SDCA-IDDC                           
280300         MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                       
280400         MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                        
280500         MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                       
280600         MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                        
280700         MOVE ORAD-KDORDING        TO SDCA-KDORDING                       
280800         MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                       
280900         MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                      
281000         MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                       
281100         MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                       
281200         MOVE +1                   TO SDCA-KDCALL                         
281300         MOVE +3                   TO SDCA-IXDCCLEAR                      
281400                                                                          
281500         CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                  
281600                             SDCA-WDB6-PCB SDCA-WDK9-PCB                  
281700                             SDCA-WDR6-PCB SDCA-WDK6-PCB                  
281800                             SDCA-WDQ4B-PCB SDCA-WDQ2-PCB                 
281900                             SDCA-WDQ4-PCB                                
281910                             SDCA-WDB6-2-PCB                              
281920                             SDCA-WDK6-2-PCB                              
281930                             SDCA-WDK7-2-PCB                              
281940                             SDCA-WDK7-3-PCB                              
282000                                                                          
282100         IF SDCA-KDORDBEK > ZERO                                          
282200                                                                          
282300           IF SDCA-FLSDCLEV = JA AND SDCA-KDORDBEK = 80                   
282400              MOVE JA        TO OBKR-SW                                   
282500              MOVE NEJ       TO ALLT-SW                                   
282600           ELSE                                                           
282700             IF ORAD-KDORDKL > 0                                          
282800             AND ORAD-IDSYSTEM NOT = 'OREL'                               
282900             AND (AREG-KDUART = 'L'                                       
283000             OR AREG-KDUART = 'P')                                        
283100             AND OHUV-FLORDSPE NOT = JA                                   
283200             AND OHUV-FLOVRLEV NOT = JA                                   
283300             AND OHUV-FLFORBI = NEJ                                       
283500                MOVE ZERO    TO SDCA-KDORDBEK                             
283600                MOVE 70      TO TPO2-KDORDBEK                             
283700                MOVE JA      TO OBKR-SW                                   
283800                MOVE NEJ     TO ALLT-SW                                   
283900                MOVE WC-CDC-SE TO ORAD-IDDC                               
284000                MOVE ORAD-IDDC TO WS-IDDC                                 
284100                MOVE 6       TO ORAD-KDTPOTYP                             
284200                IF ORAD-KDPRTYP NOT = 'P'                                 
284300                   MOVE ZERO  TO ORAD-PRARTNTO                            
284400                   MOVE SPACE TO ORAD-KDPRTYP                             
284500**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
284600                   IF NOT DIST79-DEALER-PRICE                             
284800                     MOVE ZERO        TO ORAD-PRARTNTO-LOC                
284900                     MOVE NEJ         TO ORAD-FLPRTILL                    
285000                   END-IF                                                 
285100*************                                                             
285200                END-IF                                                    
285300             ELSE                                                         
285400               IF KOLLA-ERS                                               
285500                                                                          
285600*                 MOVE NEJ             TO KOLLA-ERS-SW                    
285700*                 MOVE ZERO            TO SDCA-KDORDBEK                   
285800                  MOVE ZERO            TO SDCA-KDORDBEK-SECOND-SDC        
285900                  IF SPAR-KDORDBEK = ZERO                                 
286000                    IF ORAD-IDDC = W-TILLK-DC                             
286100                       MOVE JA         TO OBKR-SW                         
286200                       MOVE NEJ        TO ALLT-SW                         
286300                                          KOLLA-ERS-SW                    
286400                       MOVE ZERO       TO SDCA-KDORDBEK                   
286500                    ELSE                                                  
286740                       MOVE WC-CDC-SE  TO ORAD-IDDC                       
286760                       MOVE ORAD-IDDC  TO WS-IDDC                         
286780                       MOVE JA         TO CDC-MOVE-SW                     
286791                    END-IF                                                
286792                  END-IF                                                  
286800               ELSE                                                       
286900                 IF SDCA-KDORDBEK = 15                                    
287000                    IF SDCA-KDORDBEK-SECOND-SDC = 15                      
287100                       MOVE ZERO TO SDCA-KDORDBEK-SECOND-SDC              
287200                    END-IF                                                
287800                    MOVE WC-CDC-SE     TO ORAD-IDDC                       
288000                    MOVE ORAD-IDDC     TO WS-IDDC                         
288100                    MOVE JA            TO OBKR-SW                         
288200                 ELSE                                                     
288300                    MOVE JA            TO OBKR-SW                         
288400                    MOVE NEJ           TO ALLT-SW                         
288500                 END-IF                                                   
288600               END-IF                                                     
288700             END-IF                                                       
288800           END-IF                                                         
288900         ELSE                                                             
289000           MOVE SDCA-ADLAGOMR        TO ORAD-ADLAGOMR                     
289100           MOVE SDCA-ADGANG          TO ORAD-ADGANG                       
289200           MOVE SDCA-ADPLATS         TO ORAD-ADPLATS                      
289300           MOVE SDCA-KVBEART-Q       TO ORAD-KVPREAVB                     
289400           MOVE NEJ                  TO ALLT-SW                           
289800           IF  KVAN-KDORDBEK-UT  = ZERO                                   
289900           AND SDCA-KDORDBEK-FIRST-SDC = ZERO                             
290000           AND SDCA-KDORDBEK-SECOND-SDC = ZERO                            
290100             MOVE JA                 TO EGET-CL-RAD-SW                    
290200           END-IF                                                         
290300           IF KOLLA-ERS    OR                                             
290400             (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)               
290500              MOVE NEJ               TO KOLLA-ERS-SW                      
290600              MOVE ZERO              TO KERS-KDORDBEK                     
290700              PERFORM S02-RENSA-TILLK-TAB                                 
290800              MOVE ZERO              TO SPAR-KDORDBEK                     
290900           END-IF                                                         
290910           IF KERS-KDERS = +19 OR +29                                     
290920              MOVE ZERO              TO SPAR-KDORDBEK                     
290930           END-IF                                                         
291000         END-IF                                                           
291100                                                                          
291200         MOVE SDCA-KDOI            TO ORAD-KDOI                           
291300         MOVE SDCA-CLEARGROUP      TO ORAD-CLEARGROUP                     
291400       END-IF                                                             
291500                                                                          
291600     END-IF                                                               
291700     .                                                                    
291800     EJECT                                                                
291900 ECP-KOMPLETTERA-RANSONERING SECTION.                                     
292000                                                                          
292100     MOVE 'STA ECP-RANS     '              TO   WS-PGM-POSITION           
292200     IF ALLT-OK OR CDC-MOVE                                               
292300                                                                          
292400       MOVE ORAD-BERADREF        TO RANS-BERADREF                         
292500       MOVE OHUV-FLEMBORD        TO RANS-FLEMBORD                         
292600       MOVE OHUV-FLFORBI         TO RANS-FLFORBI                          
292700       MOVE OHUV-FLORDSPE        TO RANS-FLORDSPE                         
292800       MOVE OHUV-FLOVRLEV        TO RANS-FLOVRLEV                         
292900       MOVE ORAD-IDKAMPRF        TO RANS-IDKAMPRF                         
293000       MOVE ORAD-IDARTNR         TO RANS-IDARTNR                          
293100       MOVE ORAD-IDLEVNR         TO RANS-IDLEVNR                          
293200       MOVE OHUV-IDRFTAB         TO RANS-IDRFTAB                          
293300       MOVE ORAD-TIRODAT         TO RANS-TIRODAT                          
293400       MOVE OHUV-KDORDKL         TO RANS-KDORDKL                          
293500       MOVE +1                   TO RANS-KDORDBEH                         
293600       MOVE ORAD-KVBEART-Q       TO RANS-KVBEART-Q                        
293700       MOVE ORAD-KDTPOTYP        TO RANS-KDTPOTYP                         
293800       MOVE AREG-KDERS           TO RANS-KDERS                            
293900       MOVE AREG-KVLS            TO RANS-KVLS                             
294000       MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                        
294100       MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                         
294200       MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                         
294300       MOVE AREG-KVRESS          TO RANS-KVRESS                           
294400       MOVE AREG-KVSPANT         TO RANS-KVSPANT                          
294500       MOVE AREG-KVUTRS          TO RANS-KVUTRS                           
294600       MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                         
294700                                                                          
294800       MOVE AREG-KDPRODSL      TO TEST-KDPRODSL                           
294900       IF KDPRODSL-BIMA                                                   
295000         MOVE 1                TO ORAD-RERF-RAD                           
295100                                  RANS-RERF-RAD-UT                        
295200         MOVE ZERO             TO RANS-SUTPO-PB-UT                        
295300                                  RANS-SUTPO-EJPB-UT                      
295400                                  RANS-RERF-ART-UT                        
295500       ELSE                                                               
295600         CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                  
295700                             RANS-ARTM-PCB RANS-ARTS-PCB                  
295800                                                                          
295900         MOVE RANS-RERF-RAD-UT TO ORAD-RERF-RAD                           
296000       END-IF                                                             
296100                                                                          
296200     END-IF                                                               
296300     .                                                                    
296400     EJECT                                                                
296500 ECQ-KOMPLETTERA-STORA-UTTAG SECTION.                                     
296600                                                                          
296700     MOVE 'STA ECQ-STOR     '              TO   WS-PGM-POSITION           
296800     IF ALLT-OK                                                           
296900                                                                          
297000     MOVE ORAD-IDSYSTEM        TO STOR-IDSYSTEM                           
297100     MOVE ORAD-IDLEVNR         TO STOR-IDLEVNR                            
297200     MOVE ORAD-IDKUNDRF-RO     TO STOR-IDKUNDRF-RO                        
297300     MOVE SPACE                TO STOR-KDPROTYP                           
297400     MOVE ORAD-BERADREF        TO STOR-BERADREF                           
297500     MOVE OHUV-FLFORBI         TO STOR-FLFORBI                            
297600     MOVE OHUV-FLORDSPE        TO STOR-FLORDSPE                           
297700     MOVE OHUV-FLOVRLEV        TO STOR-FLOVRLEV                           
297800     MOVE OHUV-KDORDKL         TO STOR-KDORDKL                            
297900     MOVE AREG-KDERS           TO STOR-KDERS                              
298000     MOVE AREG-KDVVKL          TO STOR-KDVVKL                             
298100     MOVE ORAD-KVBEART-Q       TO STOR-KVBEART-Q                          
298200     MOVE AREG-KVPB-SEP        TO STOR-KVPB-SEP                           
298300     MOVE AREG-KVSLUTKP        TO STOR-KVSLUTKP                           
298400     MOVE RANS-RERF-ART-UT     TO STOR-RERF-ART                           
298500     MOVE OHUV-IDKAMPRF        TO STOR-IDKAMPRF                           
298600     MOVE ORAD-IDDISTR         TO STOR-IDDISTR                            
298700     MOVE AREG-KDPRODSL        TO STOR-KDPRODSL                           
298800                                                                          
298900     CALL W411STOR USING STOR-W411STOR                                    
299000                                                                          
299100     IF STOR-KDORDBEK > +0                                                
299200        MOVE +6                   TO ORAD-KDTPOTYP                        
299300        IF ORAD-KDPRTYP NOT = 'P'                                         
299400           MOVE +0                TO ORAD-PRARTNTO                        
299500           MOVE SPACE             TO ORAD-KDPRTYP                         
299600**** OM DEALERNET SKRIV INTE ÖVER FLPRTILL TL 030618                      
299700           IF NOT DIST79-DEALER-PRICE                                     
299900              MOVE ZERO        TO ORAD-PRARTNTO-LOC                       
300000              MOVE NEJ         TO ORAD-FLPRTILL                           
300100           END-IF                                                         
300200*************                                                             
300300        END-IF                                                            
300400        MOVE JA                   TO OBKR-SW                              
300500        MOVE NEJ                  TO ALLT-SW                              
300600     END-IF                                                               
300700                                                                          
300800     END-IF                                                               
300900     .                                                                    
301000     EJECT                                                                
301100 ECR-PREL-AVBOKNING-CDC SECTION.                                          
301200                                                                          
301300     MOVE 'STA ECR-CDC      '              TO   WS-PGM-POSITION           
301400     IF ALLT-OK OR CDC-MOVE                                               
301500                                                                          
301600       MOVE TILK-FLFINLV(WS-INDEX-TILLK)                                  
301700                                 TO CDCA-FLFINLV-IN                       
301800       MOVE OHUV-FLFORBI         TO CDCA-FLFORBI-IN                       
301900       MOVE OHUV-FLORDSPE        TO CDCA-FLORDSPE-IN                      
302000       MOVE OHUV-FLOVRLEV        TO CDCA-FLOVRLEV-IN                      
302100       MOVE OHUV-FLPRELRO        TO CDCA-FLPRELRO-IN                      
302200       MOVE ORAD-FLRESTN         TO CDCA-FLRESTN-IN                       
302300       MOVE ORFK-FLSLATT(WS-INDEX-MID)                                    
302400                                 TO CDCA-FLSLATT-IN                       
302500       MOVE ORAD-IDARTNR         TO CDCA-IDARTNR-IN                       
302600       MOVE OHUV-IDDISTR         TO CDCA-IDDISTR-IN                       
302700       MOVE ORAD-IDDC            TO CDCA-IDDC-IN                          
302800       MOVE ORAD-IDKAMPRF        TO CDCA-IDKAMPRF-IN                      
302900       MOVE ORAD-IDKUNDRF-RO     TO CDCA-IDKUNDRF-RO-IN                   
303000       MOVE ORAD-IDSYSTEM        TO CDCA-IDSYSTEM-IN                      
303100       MOVE ORAD-IDLEVNR         TO CDCA-IDLEVNR-IN                       
303200       MOVE AREG-KDERS           TO CDCA-KDERS-IN                         
303300       MOVE ORAD-KDKVBRYT        TO CDCA-KDKVBRYT-IN                      
303400       MOVE SPACE                TO CDCA-KDPROTYP-IN                      
303500       MOVE OHUV-KDORDKL         TO CDCA-KDORDKL-IN                       
303600       MOVE ORAD-KDPRODSL        TO CDCA-KDPRODSL-IN                      
303700       MOVE AREG-KDSORT          TO CDCA-KDSORT-IN                        
303800       MOVE ORAD-KDTPOTYP        TO CDCA-KDTPOTYP-IN                      
303900       MOVE AREG-KDUART          TO CDCA-KDUART-IN                        
304000       MOVE ORAD-KVBEART         TO CDCA-KVBEART-IN                       
304100       MOVE ORAD-KVBEART-Q       TO CDCA-KVBEART-Q-IN                     
304200       MOVE AREG-KVQPACK-0       TO CDCA-KVQPACK-0-IN                     
304300       MOVE AREG-KVQPACK-1       TO CDCA-KVQPACK-1-IN                     
304400       MOVE AREG-IDFKNGRP        TO CDCA-IDFKNGRP-IN                      
304500       MOVE RANS-RERF-RAD-UT     TO CDCA-RERF-RAD-IN                      
304600       MOVE RANS-RERF-ART-UT     TO CDCA-RERF-ART-IN                      
304700       MOVE OHUV-RESLATT         TO CDCA-RESLATT-IN                       
304800       MOVE AREG-KVAKS-CDC       TO CDCA-KVAKS-CDC-IN                     
304900       MOVE AREG-KVAKS-PAV       TO CDCA-KVAKS-PAV-IN                     
305000       MOVE AREG-KVLS            TO CDCA-KVLS-IN                          
305100       MOVE AREG-KVRESS          TO CDCA-KVRESS-IN                        
305200       MOVE AREG-KVSPANT         TO CDCA-KVSPANT-IN                       
305300       MOVE AREG-KVUTRS          TO CDCA-KVUTRS-IN                        
305400       MOVE AREG-KVSPARR-KVAL    TO CDCA-KVSPARR-KVAL-IN                  
305500       MOVE ORAD-IDKUNDNR        TO CDCA-IDKUNDNR-IN                      
305600       MOVE ORAD-BERADREF        TO CDCA-BERADREF-IN                      
305700       MOVE +1                   TO CDCA-KDCALL                           
305800       EJECT                                                              
305900                                                                          
306000       CALL W411CDCA USING CDCA-W411CDCA CDCA-ARTM-PCB                    
306100                                         CDCA-INLB-PCB                    
306200                                         CDCA-WDB2-PCB                    
306300                                         CDCA-WDC1-PCB                    
306400                                                                          
307310       IF KERS-KDERS = 0                                                  
307320          CONTINUE                                                        
307330       ELSE                                                               
307340          IF KERS-KDERS > 0 AND < 10                                      
307350             IF CDCA-KVPREAVB-UT = +0 AND CDCA-KVPRERO-UT = +0            
307360                MOVE CDCA-KVBEART-Q-UT TO CDCA-KVPRERO-UT                 
307370             END-IF                                                       
307380             PERFORM S02-RENSA-TILLK-TAB                                  
307390             MOVE ZERO           TO KERS-KDORDBEK                         
307391             MOVE NEJ            TO TILLK-SW                              
307392          ELSE                                                            
307393***FOR KDERS>10,DONT SWITCH IF A HAS STOCKS IN CDC                        
307394            IF CDCA-KVPREAVB-UT > 0                                       
307395              IF KOLLA-ERS    OR                                          
307396                (KERS-KDORDBEK > ZERO AND EJ-TILLKOMMANDE-RAD)            
307397                 MOVE NEJ        TO KOLLA-ERS-SW                          
307398                 MOVE ZERO       TO KERS-KDORDBEK                         
307399                 PERFORM S02-RENSA-TILLK-TAB                              
307400                 MOVE ZERO       TO SPAR-KDORDBEK                         
307401              ELSE                                                        
307402                 IF KERS-KDERS = +19 OR +29                               
307403                    MOVE ZERO    TO SPAR-KDORDBEK                         
307404                 END-IF                                                   
307405              END-IF                                                      
307406            ELSE                                                          
307407               IF (CDCA-KVPREAVB-UT <= 0) AND                             
307408                 (CDCA-KDORDBEK-UT = 92 OR 99)                            
307409                   MOVE ZEROES   TO CDCA-KDORDBEK-UT                      
307410               END-IF                                                     
307411               IF (KOLLA-ERS AND KERS-KDORDBEK > ZERO)                    
307412               OR (KERS-KDORDBEK = 61 AND EJ-TILLKOMMANDE-RAD)            
307413               OR SPAR-KDORDBEK = 54                                      
307414                  PERFORM S07-SPACE-SDCA-KDORDBEK                         
307415               END-IF                                                     
307416            END-IF                                                        
307417          END-IF                                                          
307418       END-IF                                                             
307430                                                                          
307500       MOVE CDCA-FLAKPLOC-UT     TO ORAD-FLAKPLOC                         
307600                                                                          
307700       IF ORAD-IDLEVNR NOT = SPACE                                        
307800          CONTINUE                                                        
307900       ELSE                                                               
308000          MOVE CDCA-KVBEART-UT   TO ORAD-KVBEART                          
308100          MOVE CDCA-KVBEART-Q-UT TO ORAD-KVBEART-Q                        
308200       END-IF                                                             
308300       MOVE CDCA-KVPREAVB-UT     TO ORAD-KVPREAVB                         
308400       MOVE CDCA-KVPRERO-UT      TO ORAD-KVPRERO                          
308500       MOVE CDCA-KVSLATT-UT      TO ORAD-KVSLATT                          
308600       MOVE CDCA-RERF-RAD-UT     TO ORAD-RERF-RAD                         
308700                                                                          
308800       IF CDCA-KDORDBEK-UT > ZERO                                         
308900          MOVE JA                TO OBKR-SW                               
309000       END-IF                                                             
309100                                                                          
309200       IF KVAN-KDORDBEK-UT > +0                                           
309300          IF CDCA-KVBEART-UT = CDCA-KVBEART-Q-UT                          
309400             MOVE +0             TO KVAN-KDORDBEK-UT                      
309500          END-IF                                                          
309600       END-IF                                                             
309700       EJECT                                                              
309800                                                                          
309900       IF (CDCA-KVPREAVB-UT > +0 OR CDCA-KVPRERO-UT > +0) AND             
310000           CDCA-KDORDBEK-UT  = +0 AND                                     
310100           KVAN-KDORDBEK-UT  = +0 AND                                     
310200           DLEV-KDORDBEK-UT  = +0 AND                                     
310300           KERS-KDORDBEK     = +0 AND                                     
310400           TPO1-KDORDBEK     = +0 AND                                     
310500           TPO2-KDORDBEK     = +0 AND                                     
310600           KAMP-KDORDBEK     = +0 AND                                     
310700           STOR-KDORDBEK     = +0 AND                                     
310800           SDCA-KDORDBEK     = +0 AND                                     
310900           SDCA-KDORDBEK-FIRST-SDC = +0 AND                               
311000           SDCA-KDORDBEK-SECOND-SDC = +0 AND                              
311100           SPAR-KDORDBEK = ZERO                                           
311200           MOVE JA                  TO EGET-CL-RAD-SW                     
311300       END-IF                                                             
311400       MOVE AREG-ADLAGOMR        TO ORAD-ADLAGOMR                         
311500       MOVE AREG-ADGANG          TO ORAD-ADGANG                           
311600       MOVE AREG-ADPLATS         TO ORAD-ADPLATS                          
311700                                                                          
311800     END-IF                                                               
311900     .                                                                    
312000     EJECT                                                                
312100 ECS-SKRIV-OBKR SECTION.                                                  
312200                                                                          
312300     MOVE 'STA ECS-OBKR     '              TO   WS-PGM-POSITION           
312400*--- EFTERSOM KVPREAVB OCH KVPRERO ENDAST SKALL SKRIVAS PÅ                
312500*    DEN SISTA ORDERBEKRÄFTELSERADEN FÖR ETT DC 'SLÄPAR'                  
312600*    ISRT AV RADEN.                                                       
312700*    OBKR-SW SÄTTS = JA NÄR EN ORDERBEKRÄFTELSE RAD SKALL                 
312800*    SKRIVAS.  DENNA TESTAR VI PÅ SIST I SECTIONEN FÖR ATT                
312900*    FÅ MED DEN SISTA ORDERBEKRÄFTELSEN MED KVPREAVB OCH KVPRERO          
313000*---                                                                      
313100     PERFORM ECSA-REDIGERA-OBKR-RAD                                       
313200                                                                          
313300     IF TILLKOMMANDE-RAD                                                  
313400        IF KERS-KDORDBEK = 41                                             
313500           MOVE KERS-KDORDBEK     TO OBKR-KDORDBEK                        
313900           MOVE '4212KER1'        TO OBKR-IDPGM                           
314000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
314100                                  TO OBKR-KVBEART-TILLK                   
314110           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
314120              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
314130             MOVE +0              TO OBKR-DIERS-KVOT                      
314140           ELSE                                                           
314150             COMPUTE OBKR-DIERS-KVOT =                                    
314160                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
314170                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
314180           END-IF                                                         
314500           MOVE 'S'               TO OBKR-SW                              
314600        END-IF                                                            
314700     END-IF                                                               
314800                                                                          
314900       IF ORFK-KDORDBEK(WS-INDEX-MID) > ZERO                              
315000          AND ORFK-KDORDBEK(WS-INDEX-MID) NOT = 56                        
315100*----(KOD 56,58, 59, 98)                                                  
315200          IF OBKR-SKRIVEN                                                 
315300             PERFORM IMS-08-ISRT-WDQ101                                   
315400             ADD +1              TO OBKR-IDSEKVNR                         
315500          END-IF                                                          
315600          MOVE ORFK-KDORDBEK(WS-INDEX-MID)                                
315700                                 TO OBKR-KDORDBEK                         
315800          MOVE '4212ORFK'        TO OBKR-IDPGM                            
315900          MOVE 'S'               TO OBKR-SW                               
316000       END-IF                                                             
316100    EJECT                                                                 
316200                                                                          
316300     IF KVAN-KDORDBEK-UT > ZERO                                           
316400*----(KOD 43, 44)                                                         
316500        IF OBKR-SKRIVEN                                                   
316600           PERFORM IMS-08-ISRT-WDQ101                                     
316700           ADD +1              TO OBKR-IDSEKVNR                           
316800        END-IF                                                            
316900        IF TILLKOMMANDE-RAD                                               
317000           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
317100                               TO OBKR-KVBEART-TILLK                      
317110           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
317120              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
317130             MOVE +0              TO OBKR-DIERS-KVOT                      
317140           ELSE                                                           
317150             COMPUTE OBKR-DIERS-KVOT =                                    
317160                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
317170                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
317180           END-IF                                                         
317500        END-IF                                                            
317600        MOVE KVAN-KDORDBEK-UT  TO OBKR-KDORDBEK                           
317700        MOVE '4212KVAN'        TO OBKR-IDPGM                              
317800        MOVE KVAN-KVBEART-IN   TO OBKR-KVBEART                            
317900        MOVE KVAN-KVBEART-Q-UT TO OBKR-KVBEART-Q                          
318000        MOVE 'S'               TO OBKR-SW                                 
318100     END-IF                                                               
318200     EJECT                                                                
318300                                                                          
318400     IF DLEV-KDORDBEK-UT > ZERO                                           
318500*----(KOD 21, 53, 95) , 26                                                
318600        IF OBKR-SKRIVEN                                                   
318700           PERFORM IMS-08-ISRT-WDQ101                                     
318800           ADD +1              TO OBKR-IDSEKVNR                           
318900        END-IF                                                            
319000        IF TILLKOMMANDE-RAD                                               
319100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
319200                               TO OBKR-KVBEART-TILLK                      
319510           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
319520              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
319530             MOVE +0              TO OBKR-DIERS-KVOT                      
319540           ELSE                                                           
319550             COMPUTE OBKR-DIERS-KVOT =                                    
319560                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
319570                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
319580           END-IF                                                         
319600        END-IF                                                            
319700        MOVE DLEV-KDORDBEK-UT  TO OBKR-KDORDBEK                           
319800        MOVE '4212DLEV'        TO OBKR-IDPGM                              
319900        MOVE 'S'               TO OBKR-SW                                 
320000     END-IF                                                               
320100     EJECT                                                                
320200     IF KERS-KDORDBEK > ZERO                                              
320400*----(KOD 41, 61)                                                         
320500                                                                          
320600        IF KERS-KDORDBEK = 61                                             
320700*----FÖR KOD 41 OCH 42 SKER ORDERBEKRÄFTELSE ENDAST PÅ TILL-              
320800*----KOMMANDE ARTIKLAR EFTER DET ATT DESSA HAR GENOMGÅTT                  
320900*----RADBEHANDLINGEN                                                      
321000           IF OBKR-SKRIVEN                                                
321100              PERFORM IMS-08-ISRT-WDQ101                                  
321200              ADD +1           TO OBKR-IDSEKVNR                           
321300           END-IF                                                         
321400           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
321500           MOVE '4212KER2'     TO OBKR-IDPGM                              
321600           MOVE 'S'            TO OBKR-SW                                 
321700           PERFORM ECSC-OBKR-FRAN-TILLK-TAB                               
321800           PERFORM S02-RENSA-TILLK-TAB                                    
321900        ELSE                                                              
322000           IF NOT TILLKOMMANDE-RAD                                        
322100              IF OBKR-SKRIVEN                                             
322200                 PERFORM IMS-08-ISRT-WDQ101                               
322300                 ADD +1        TO OBKR-IDSEKVNR                           
322400              END-IF                                                      
322500              MOVE 'S'            TO OBKR-SW                              
322600              MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                        
323000              MOVE '4212KER3'     TO OBKR-IDPGM                           
323100           END-IF                                                         
323200        END-IF                                                            
323300     END-IF                                                               
323400     EJECT                                                                
323500     IF SPAR-KDORDBEK > ZERO                                              
323600*----(KOD 51, 52, 53, 54, 55, 57, 58, 66, 67, 80, 90, 91, 92)             
323700        IF OBKR-SKRIVEN                                                   
323800           PERFORM IMS-08-ISRT-WDQ101                                     
323900           ADD +1              TO OBKR-IDSEKVNR                           
324000        END-IF                                                            
324100        IF TILLKOMMANDE-RAD                                               
324200           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
324300                               TO OBKR-KVBEART-TILLK                      
324610           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
324620              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
324630             MOVE +0              TO OBKR-DIERS-KVOT                      
324640           ELSE                                                           
324650             COMPUTE OBKR-DIERS-KVOT =                                    
324660                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
324670                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
324680           END-IF                                                         
324700        END-IF                                                            
324800        MOVE SPAR-KDORDBEK                                                
324900                               TO OBKR-KDORDBEK                           
325000        MOVE '4212SPAR'        TO OBKR-IDPGM                              
325100        MOVE 'S'               TO OBKR-SW                                 
325200     END-IF                                                               
325300     EJECT                                                                
325400                                                                          
325500     IF TPO1-KDORDBEK > ZERO                                              
325600*----(KOD 72, 73, 74, 85)                                                 
325700        IF OBKR-SKRIVEN                                                   
325800           PERFORM IMS-08-ISRT-WDQ101                                     
325900           ADD +1              TO OBKR-IDSEKVNR                           
326000        END-IF                                                            
326100        IF TILLKOMMANDE-RAD                                               
326200           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
326300                               TO OBKR-KVBEART-TILLK                      
326610           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
326620              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
326630             MOVE +0              TO OBKR-DIERS-KVOT                      
326640           ELSE                                                           
326650             COMPUTE OBKR-DIERS-KVOT =                                    
326660                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
326670                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
326680           END-IF                                                         
326700        END-IF                                                            
326800        MOVE TPO1-KDORDBEK     TO OBKR-KDORDBEK                           
326900        MOVE '4212TPO1'        TO OBKR-IDPGM                              
327000        IF TPO1-KDORDBEK = 85                                             
327100           MOVE TPO1-KVANNANT  TO OBKR-KVANNANT                           
327200        END-IF                                                            
327300        MOVE 'S'               TO OBKR-SW                                 
327400     END-IF                                                               
327500     EJECT                                                                
327600     IF TPO2-KDORDBEK > ZERO                                              
327700*----(KOD 70)                                                             
327800        IF OBKR-SKRIVEN                                                   
327900           PERFORM IMS-08-ISRT-WDQ101                                     
328000           ADD +1              TO OBKR-IDSEKVNR                           
328100        END-IF                                                            
328200        IF TILLKOMMANDE-RAD                                               
328300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
328400                               TO OBKR-KVBEART-TILLK                      
328710           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
328720              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
328730             MOVE +0              TO OBKR-DIERS-KVOT                      
328740           ELSE                                                           
328750             COMPUTE OBKR-DIERS-KVOT =                                    
328760                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
328770                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
328780           END-IF                                                         
328800        END-IF                                                            
328900        MOVE TPO2-KDORDBEK     TO OBKR-KDORDBEK                           
329000        MOVE '4212TPO2'        TO OBKR-IDPGM                              
329100        MOVE 'S'               TO OBKR-SW                                 
329200     END-IF                                                               
329300     EJECT                                                                
329400                                                                          
329500     IF KAMP-KDORDBEK > ZERO                                              
329600*----(KOD 72, 75, 76)                                                     
329700        IF OBKR-SKRIVEN                                                   
329800           PERFORM IMS-08-ISRT-WDQ101                                     
329900           ADD +1              TO OBKR-IDSEKVNR                           
330000        END-IF                                                            
330100        IF TILLKOMMANDE-RAD                                               
330200           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
330300                               TO OBKR-KVBEART-TILLK                      
330610           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
330620              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
330630             MOVE +0              TO OBKR-DIERS-KVOT                      
330640           ELSE                                                           
330650             COMPUTE OBKR-DIERS-KVOT =                                    
330660                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
330670                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
330680           END-IF                                                         
330700        END-IF                                                            
330800        MOVE KAMP-KDORDBEK     TO OBKR-KDORDBEK                           
330900        MOVE '4212KAMP'        TO OBKR-IDPGM                              
331000        MOVE 'S'               TO OBKR-SW                                 
331100     END-IF                                                               
331200     EJECT                                                                
331300                                                                          
331400     IF RELS-KDORDBEK > 0                                                 
331500*----(KOD 56)                                                             
331600          IF OBKR-SKRIVEN                                                 
331700             PERFORM IMS-08-ISRT-WDQ101                                   
331800             ADD +1              TO OBKR-IDSEKVNR                         
331900          END-IF                                                          
332000          MOVE RELS-KDORDBEK     TO OBKR-KDORDBEK                         
332100          MOVE '4212ORFK'        TO OBKR-IDPGM                            
332200          MOVE 'S'               TO OBKR-SW                               
332300       END-IF                                                             
332400    EJECT                                                                 
332500     IF XDCA-KDORDBEK > ZERO                                              
332600*----(KOD 15, 53, 55, 80, 92, 98, 99)                                     
332700        IF OBKR-SKRIVEN                                                   
332800           PERFORM IMS-08-ISRT-WDQ101                                     
332900           ADD +1              TO OBKR-IDSEKVNR                           
333000        END-IF                                                            
333100        IF TILLKOMMANDE-RAD                                               
333200           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
333300                               TO OBKR-KVBEART-TILLK                      
333610           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
333620              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
333630             MOVE +0              TO OBKR-DIERS-KVOT                      
333640           ELSE                                                           
333650             COMPUTE OBKR-DIERS-KVOT =                                    
333660                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
333670                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
333680           END-IF                                                         
333700        END-IF                                                            
333800                                                                          
333900        IF XDCA-KDORDBEK NOT = 15                                         
334000          IF OHUV-IDDC-TVS = SPACE                                        
334100            IF OHUV-IDDC-PRIM NOT = XDCA-IDDC-OUT                         
334200              MOVE 15          TO OBKR-KDORDBEK                           
334300              MOVE IDPGM       TO OBKR-IDPGM                              
334400              MOVE 'S'         TO OBKR-SW                                 
334500                                                                          
334600* FÖR ATT SLIPPA DUBBLA 15-BEKRÄFTELSER NOLLAR VI EV. SDC                 
334700              IF SDCA-KDORDBEK-FIRST-SDC = 15                             
334800                 MOVE ZERO     TO SDCA-KDORDBEK-FIRST-SDC                 
334900              END-IF                                                      
335000              IF SDCA-KDORDBEK-SECOND-SDC = 15                            
335100                 MOVE ZERO     TO SDCA-KDORDBEK-SECOND-SDC                
335200              END-IF                                                      
335300              IF SDCA-KDORDBEK = 15                                       
335400                 MOVE ZERO     TO SDCA-KDORDBEK                           
335500              END-IF                                                      
335600            END-IF                                                        
335700            IF OBKR-SKRIVEN                                               
335800              PERFORM IMS-08-ISRT-WDQ101                                  
335900              ADD +1           TO OBKR-IDSEKVNR                           
336000            END-IF                                                        
336100          END-IF                                                          
336200        END-IF                                                            
336300        IF XDCA-KDORDBEK = 80                                             
336400           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
336500        END-IF                                                            
336600        MOVE XDCA-KDORDBEK     TO OBKR-KDORDBEK                           
336700        MOVE '4212XDCA'        TO OBKR-IDPGM                              
336800        MOVE 'S'               TO OBKR-SW                                 
336900     END-IF                                                               
337000     EJECT                                                                
337100                                                                          
337200     IF SDCA-KDORDBEK-SECOND-SDC > ZERO                                   
337300*----(KOD 15, 53, 80, 92)                                                 
337400        IF OBKR-SKRIVEN                                                   
337500           PERFORM IMS-08-ISRT-WDQ101                                     
337600           ADD +1              TO OBKR-IDSEKVNR                           
337700        END-IF                                                            
337800        IF TILLKOMMANDE-RAD                                               
337900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
338000                               TO OBKR-KVBEART-TILLK                      
338310           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
338320              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
338330             MOVE +0              TO OBKR-DIERS-KVOT                      
338340           ELSE                                                           
338350             COMPUTE OBKR-DIERS-KVOT =                                    
338360                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
338370                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
338380           END-IF                                                         
338400        END-IF                                                            
338500        IF SDCA-KDORDBEK-SECOND-SDC = 80                                  
338600           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
338700        END-IF                                                            
338710        IF SDCA-KDORDBEK-SECOND-SDC = 80 OR 92                            
338720           MOVE 0              TO OBKR-IDARTNR-TILLK                      
338730        END-IF                                                            
338800        MOVE SDCA-KDORDBEK-SECOND-SDC TO OBKR-KDORDBEK                    
338900        MOVE '4212SDCA'        TO OBKR-IDPGM                              
339000        MOVE 'S'               TO OBKR-SW                                 
339100     END-IF                                                               
339200     EJECT                                                                
339300                                                                          
339400     IF SDCA-KDORDBEK-FIRST-SDC > ZERO                                    
339500*----(KOD 15, 53, 80, 92)                                                 
339600        IF OBKR-SKRIVEN                                                   
339700           PERFORM IMS-08-ISRT-WDQ101                                     
339800           ADD +1              TO OBKR-IDSEKVNR                           
339900        END-IF                                                            
340000        IF TILLKOMMANDE-RAD                                               
340100           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
340200                               TO OBKR-KVBEART-TILLK                      
340510           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
340520              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
340530             MOVE +0              TO OBKR-DIERS-KVOT                      
340540           ELSE                                                           
340550             COMPUTE OBKR-DIERS-KVOT =                                    
340560                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
340570                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
340580           END-IF                                                         
340600        END-IF                                                            
340700        IF SDCA-KDORDBEK-FIRST-SDC = 80                                   
340800           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
340900        END-IF                                                            
340901        IF SDCA-KDORDBEK-FIRST-SDC = 80 OR 92                             
340910           MOVE 0              TO OBKR-IDARTNR-TILLK                      
340920        END-IF                                                            
341000        MOVE SDCA-KDORDBEK-FIRST-SDC TO OBKR-KDORDBEK                     
341100        MOVE '4212SDCA'        TO OBKR-IDPGM                              
341200        MOVE 'S'               TO OBKR-SW                                 
341300     END-IF                                                               
341400     EJECT                                                                
341500                                                                          
341600     IF SDCA-KDORDBEK > ZERO                                              
341700*----(KOD 15, 53, 80, 92)                                                 
341800        IF OBKR-SKRIVEN                                                   
341900           PERFORM IMS-08-ISRT-WDQ101                                     
342000           ADD +1              TO OBKR-IDSEKVNR                           
342100        END-IF                                                            
342200        IF TILLKOMMANDE-RAD                                               
342300           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
342400                               TO OBKR-KVBEART-TILLK                      
342710           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
342720              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
342730             MOVE +0              TO OBKR-DIERS-KVOT                      
342740           ELSE                                                           
342750             COMPUTE OBKR-DIERS-KVOT =                                    
342760                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
342770                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
342780           END-IF                                                         
342800        END-IF                                                            
342900        IF SDCA-KDORDBEK = 80                                             
343000           MOVE ORAD-KVBEART-Q TO OBKR-KVANNANT                           
343100        END-IF                                                            
343110        IF SDCA-KDORDBEK = 80 OR 92                                       
343120           MOVE 0              TO OBKR-IDARTNR-TILLK                      
343130        END-IF                                                            
343200        MOVE SDCA-KDORDBEK     TO OBKR-KDORDBEK                           
343300        MOVE '4212SDCA'        TO OBKR-IDPGM                              
343400        MOVE 'S'               TO OBKR-SW                                 
343500     END-IF                                                               
343600     EJECT                                                                
343700     IF STOR-KDORDBEK > ZERO                                              
343800*----(KOD 70)                                                             
343900        IF OBKR-SKRIVEN                                                   
344000           PERFORM IMS-08-ISRT-WDQ101                                     
344100           ADD +1              TO OBKR-IDSEKVNR                           
344200        END-IF                                                            
344300        IF TILLKOMMANDE-RAD                                               
344400           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
344500                               TO OBKR-KVBEART-TILLK                      
344810           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
344820              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
344830             MOVE +0              TO OBKR-DIERS-KVOT                      
344840           ELSE                                                           
344850             COMPUTE OBKR-DIERS-KVOT =                                    
344860                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
344870                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
344880           END-IF                                                         
344900        END-IF                                                            
345000        MOVE STOR-KDORDBEK     TO OBKR-KDORDBEK                           
345100        MOVE '4212STOR'        TO OBKR-IDPGM                              
345200        MOVE 'S'               TO OBKR-SW                                 
345300     END-IF                                                               
345400     EJECT                                                                
345500     IF CDCA-KDORDBEK-UT > ZERO                                           
345600*----(KOD 80, 92, 99)                                                     
345700        IF OBKR-SKRIVEN                                                   
345800           PERFORM IMS-08-ISRT-WDQ101                                     
345900           ADD +1              TO OBKR-IDSEKVNR                           
346000        END-IF                                                            
346100        IF TILLKOMMANDE-RAD                                               
346200           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
346300                               TO OBKR-KVBEART-TILLK                      
346610           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
346620              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
346630             MOVE +0              TO OBKR-DIERS-KVOT                      
346640           ELSE                                                           
346650             COMPUTE OBKR-DIERS-KVOT =                                    
346660                                TILK-DIERS-TILLK(WS-INDEX-TILLK)          
346670                                / TILK-DIERS-ERS(WS-INDEX-TILLK)          
346680           END-IF                                                         
346700        END-IF                                                            
346800        IF CDCA-KDORDBEK-UT = 80                                          
346900           MOVE CDCA-KVANNANT-UT  TO OBKR-KVANNANT                        
347000        END-IF                                                            
347100        MOVE CDCA-KDORDBEK-UT  TO OBKR-KDORDBEK                           
347200        MOVE '4212CDCA'        TO OBKR-IDPGM                              
347300        MOVE 'S'               TO OBKR-SW                                 
347400     END-IF                                                               
347500*                                                                         
347600* PÅ SISTA RADEN FÖR KUNDENS NORMALA DC LÄGGS DE AVBOKADE                 
347700* ANTALEN!                                                                
347800*                                                                         
347900     IF OBKR-SKRIVEN                                                      
347910*THE IF CONDITION (OCC 61 AND KVPREAVB,KVPRERO = 0) IS CODED,             
347920*BECAUSE IF A HAS NO STOCKS IN LDC,CDC, THEN KVPRERO 1 IS                 
347930*INSERTED FOR THE LAST SUPERSEEDING PART THEREBY CREATING                 
347940*ORDERLINE FOR THAT PART ALONG WITH OCC61.                                
347950       IF OBKR-KDORDBEK = 61 AND                                          
347960        (OBKR-KVPREAVB = 0 AND OBKR-KVPRERO = 0)                          
347970          CONTINUE                                                        
347980       ELSE                                                               
347990          MOVE ORAD-KVPREAVB     TO OBKR-KVPREAVB                         
347991          MOVE ORAD-KVPRERO      TO OBKR-KVPRERO                          
347992       END-IF                                                             
348200******** TA BORT FRÅGAN FÖR ERSATT ARTIKEL                                
348300         IF KERS-KDORDBEK > 0 AND NOT TILLKOMMANDE-RAD                    
348400           PERFORM S05-DELETE-PRICE-Q-LINE                                
348500           INITIALIZE OBKR-DEAL-PR-LINE                                   
348600           MOVE 'N/A'          TO OBKR-KDVALISO                           
348700         END-IF                                                           
348800*************TL 030514                                                    
348900        PERFORM IMS-08-ISRT-WDQ101                                        
349000        ADD +1                 TO OBKR-IDSEKVNR                           
349100     END-IF                                                               
349200                                                                          
349300*** TILLÄGGSTPO SKAPAS                                                    
349400                                                                          
349500     IF DDGS-TPO-OBKR71                                                   
349600        PERFORM ECSB-SKAPA-TPO2                                           
349700     END-IF                                                               
349800     .                                                                    
349900     EJECT                                                                
350000 ECSA-REDIGERA-OBKR-RAD SECTION.                                          
350200     MOVE ORAD-IDORDER         TO OBKR-IDORDER                            
350300     MOVE ORFK-IDARTNR(WS-INDEX-MID)                                      
350400                               TO OBKR-IDARTNR                            
350500     IF NOT TILLKOMMANDE-RAD                                              
350600        MOVE OBKR-IDORDER      TO W-IDORDER-Q1-MIN                        
350700                                  W-IDORDER-Q1-MAX                        
350800        MOVE OBKR-IDARTNR      TO W-IDARTNR-Q1-MIN                        
350900                                  W-IDARTNR-Q1-MAX                        
351000        MOVE +1                TO W-IDLOPNR-Q1-MIN                        
351100                                  W-IDLOPNR-Q1-MAX                        
351200                                  W-IDSEKVNR-Q1-MIN                       
351300                                  W-IDSEKVNR-Q1-MAX                       
351400        PERFORM IMS-07-GU-WDQ1-WDQ101                                     
351500        PERFORM UNTIL SEGMENT-SAKNAS                                      
351600           ADD +1              TO W-IDLOPNR-Q1-MIN                        
351700                                  W-IDLOPNR-Q1-MAX                        
351800           PERFORM IMS-07-GU-WDQ1-WDQ101                                  
351900        END-PERFORM                                                       
352000        MOVE W-IDLOPNR-Q1-MIN  TO OBKR-IDLOPNR                            
352100        MOVE W-IDSEKVNR-Q1-MIN TO OBKR-IDSEKVNR                           
352200     END-IF                                                               
352300     MOVE ORAD-IDDC            TO OBKR-IDDC                               
352400     MOVE ZERO                 TO OBKR-KDORDBEK                           
352500     MOVE SPACE                TO OBKR-BEERS                              
352600     MOVE OHUV-BEKUNDRF        TO OBKR-BEKUNDRF                           
352700     MOVE ORAD-BERADREF        TO OBKR-BERADREF                           
352800     MOVE ORAD-BEVOLREF        TO OBKR-BEVOLREF                           
352900     MOVE ORAD-IDKAMPRF        TO OBKR-IDKAMPRF                           
353000     MOVE +0                   TO OBKR-DIERS-KVOT                         
353100     MOVE ORAD-FLAKPLOC        TO OBKR-FLAKPLOC                           
353200     MOVE ORAD-FLINVEST        TO OBKR-FLINVEST                           
353300     MOVE NEJ                  TO OBKR-FLOBOK                             
353400     MOVE ORAD-FLOBTRAN        TO OBKR-FLOBTRAN                           
353500     MOVE NEJ                  TO OBKR-FLOBPRT                            
353600     MOVE ORAD-FLPRTILL        TO OBKR-FLPRTILL                           
353700     MOVE ORAD-FLRESTN         TO OBKR-FLRESTN                            
353800     MOVE ORFK-FLSLATT(WS-INDEX-MID)                                      
353900                               TO OBKR-FLSLATT                            
354000     MOVE ORAD-FLTILLK         TO OBKR-FLTILLK                            
354100     IF TILLKOMMANDE-RAD                                                  
354200        MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                           
354300                               TO OBKR-IDARTNR-TILLK                      
354400        MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                          
354500                               TO OBKR-REKSIFFR-TILLK                     
354600     ELSE                                                                 
354700        MOVE +0                TO OBKR-IDARTNR-TILLK                      
354800        MOVE +0                TO OBKR-REKSIFFR-TILLK                     
354900     END-IF                                                               
355000     MOVE ORAD-IDDC-RO         TO OBKR-IDDC-RO                            
355100     MOVE ORAD-IDGMTREF        TO OBKR-IDGMTREF                           
355200     MOVE ORAD-IDKUNDRF-RO     TO OBKR-IDKUNDRF-RO                        
355300     MOVE ORAD-IDLEVNR         TO OBKR-IDLEVNR                            
355400     MOVE ORAD-IDLOPNR-RO      TO OBKR-IDLOPNR-RO                         
355500     MOVE ORAD-IDSYSTEM        TO OBKR-IDSYSTEM                           
355600     MOVE ORAD-KDDSP           TO OBKR-KDDSP                              
355700     IF NOT TILLKOMMANDE-RAD                                              
355800        MOVE AREG-KDERS        TO OBKR-KDERS                              
355900     END-IF                                                               
356000     MOVE ORAD-KDOI            TO OBKR-KDOI                               
356100     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
356200     MOVE ORAD-KDKVBRYT        TO OBKR-KDKVBRYT                           
356300     MOVE ORAD-KDPRTYP         TO OBKR-KDPRTYP                            
356400     MOVE ORAD-KDTPOTYP        TO OBKR-KDTPOTYP                           
356500     MOVE ORAD-KDVRINFO        TO OBKR-KDVRINFO                           
356600     MOVE +0                   TO OBKR-KVANNANT                           
356700     MOVE +0                   TO OBKR-KVAVBART                           
356800     MOVE ORAD-KVBEART         TO OBKR-KVBEART                            
356900     MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART-Q                          
357000     MOVE +0                   TO OBKR-KVBEART-TILLK                      
357100     MOVE +0                   TO OBKR-KVPREAVB                           
357200     MOVE +0                   TO OBKR-KVPRERO                            
357300     IF ALLT-OK                                                           
357400       MOVE KVAN-KVQPACK-UT    TO OBKR-KVQPACK                            
357500     ELSE                                                                 
357600       MOVE ZERO               TO OBKR-KVQPACK                            
357700     END-IF                                                               
357800     MOVE +0                   TO OBKR-KVRO                               
357900     MOVE ORAD-KVSLATT         TO OBKR-KVSLATT                            
358000     MOVE ORAD-PRARTNTO        TO OBKR-PRARTNTO                           
358100     MOVE ORAD-DEAL-PR-LINE    TO OBKR-DEAL-PR-LINE                       
358200     MOVE ORAD-PRBPRIS         TO OBKR-PRBPRIS                            
358300     MOVE ORAD-RERF-RAD        TO OBKR-RERF-RAD                           
358400     IF ORFK-KDORDBEK(WS-INDEX-MID) = 59                                  
358500        MOVE ORAD-REKSIFFR     TO OBKR-REKSIFFR                           
358600     ELSE                                                                 
358700        MOVE ORFK-REKSIFFR(WS-INDEX-MID)                                  
358800                               TO OBKR-REKSIFFR                           
358900     END-IF                                                               
359000     MOVE AREG-TIDISPIN        TO OBKR-TIDISPIN                           
359100     MOVE OHUV-TIREGDAT        TO OBKR-TIORDREG                           
359200     MOVE ORAD-TIPRIS          TO OBKR-TIPRIS                             
359300     MOVE ORAD-TIREGDAT        TO OBKR-TIREGDAT                           
359400     MOVE ORAD-TIREGTID        TO OBKR-TIREGTID                           
359500     MOVE +0                   TO OBKR-TIRODAT                            
359600     MOVE ORAD-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
359700     IF WS-AAMMDD-9KOMPL(1:2) < 50                                        
359800       MOVE 20                 TO WS-SEKEL-9KOMPL                         
359900     ELSE                                                                 
360000       MOVE 19                 TO WS-SEKEL-9KOMPL                         
360100     END-IF                                                               
360200     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
360300     MOVE ORAD-TITPO           TO OBKR-TITPO                              
360400     MOVE OHUV-TIREGDAT        TO WS-AAMMDD-9KOMPL                        
360500     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-TITIREGD-9KOMPL        
360600     MOVE ARB-KDFRAKT          TO OBKR-KDFRAKT                            
360700     MOVE OHUV-KDORDKL         TO OBKR-KDORDKL                            
360800     MOVE SPACE                TO OBKR-IDBIL                              
360900     MOVE OHUV-KDORDTYP-LDC    TO OBKR-KDORDTYP-LDC                       
361000     MOVE OHUV-TIREPDAT        TO OBKR-TIREPDAT                           
361100     MOVE ORAD-IDKUNDRF-WIP    TO OBKR-IDKUNDRF-WIP                       
361200     MOVE ZERO                 TO OBKR-TIDLEVDAT                          
361300     MOVE ORAD-PRAVCOST        TO OBKR-PRAVCOST                           
361400     MOVE ORAD-KDVALISO        TO OBKR-KDVALISO                           
361500*    *GLOBAL EXPORT PROJEKTET KRÄVER IFYLLD VALUTA                        
361600     IF OBKR-KDVALISO = SPACE                                             
361700        MOVE 'N/A'             TO OBKR-KDVALISO                           
361800     END-IF                                                               
361900     .                                                                    
362000     EJECT                                                                
362100 ECSB-SKAPA-TPO2 SECTION.                                                 
362200                                                                          
362300     MOVE 'STA ECSB-SKAPA-TPO2'           TO   WS-PGM-POSITION            
362400     MOVE WC-CDC-SE            TO W-IDDC-WDB3                             
362500                                  W-IDDC-WDB3-DEF                         
362600     MOVE OBKR-IDDISTR         TO W-IDDISTR-WDB3                          
362700                                  W-IDDISTR-WDB3-DEF                      
362800     MOVE OBKR-IDKUNDNR        TO W-IDKUNDNR-WDB3                         
362900     PERFORM IMS-GU-WDB301                                                
363000     IF SEGMENT-FINNS                                                     
363100        IF OBKR-KDORDKL = 1                                               
363200           MOVE DC-KDGENFRA-DO TO OBKR-KDFRAKT                            
363300        ELSE                                                              
363400           MOVE DC-KDGENFRA-MO TO OBKR-KDFRAKT                            
363500        END-IF                                                            
363600     END-IF                                                               
363700     MOVE +2                   TO OBKR-KDTPOTYP                           
363800     MOVE +71                  TO OBKR-KDORDBEK                           
363900     MOVE +0                   TO OBKR-KVPREAVB                           
364000     MOVE +0                   TO OBKR-KVPRERO                            
364100     MOVE IDPGM                TO OBKR-IDPGM                              
364200     IF DDGS-TPO-OBKR71                                                   
364300        MOVE OHUV-TITPO        TO OBKR-TITPO                              
364400        MOVE SPACE             TO OBKR-KDOI                               
364500     ELSE                                                                 
364600        MOVE OBKR-TIORDREG     TO OBKR-TITPO                              
364700     END-IF                                                               
364800     MOVE ORAD-KDOI            TO OBKR-KDOI                               
364900     MOVE ORAD-CLEARGROUP      TO OBKR-CLEARGROUP                         
365000     MOVE NEJ                  TO ALLT-SW                                 
365100     IF TILLKOMMANDE-RAD                                                  
365200        MOVE TILK-KVBEART(WS-INDEX-TILLK)                                 
365300                               TO OBKR-KVBEART-TILLK                      
365400        IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                        
365500           TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                             
365600          MOVE +0              TO OBKR-DIERS-KVOT                         
365610        ELSE                                                              
365620          COMPUTE OBKR-DIERS-KVOT =                                       
365630                               TILK-DIERS-TILLK(WS-INDEX-TILLK)           
365640                               / TILK-DIERS-ERS(WS-INDEX-TILLK)           
365650        END-IF                                                            
365700     END-IF                                                               
365800     MOVE 'S'                  TO OBKR-SW                                 
365900     PERFORM IMS-08-ISRT-WDQ101                                           
366000     ADD +1                    TO OBKR-IDSEKVNR                           
366100     .                                                                    
366200     EJECT                                                                
366300 ECSC-OBKR-FRAN-TILLK-TAB SECTION.                                        
366400                                                                          
366500     MOVE +1                   TO WS-INDEX-TILLK                          
366600     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
366700                TILK-IDARTNR(WS-INDEX-TILLK) = ZERO                       
366800        IF TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
366900           IF OBKR-SKRIVEN                                                
367000              PERFORM IMS-08-ISRT-WDQ101                                  
367100              ADD +1              TO OBKR-IDSEKVNR                        
367200           END-IF                                                         
367300           MOVE KERS-KDORDBEK  TO OBKR-KDORDBEK                           
367400           MOVE '4212KER4'     TO OBKR-IDPGM                              
367500           MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                        
367600                               TO OBKR-IDARTNR-TILLK                      
367700           MOVE TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)                       
367800                               TO OBKR-REKSIFFR-TILLK                     
367900           MOVE TILK-KVBEART(WS-INDEX-TILLK)                              
368000                               TO OBKR-KVBEART-TILLK                      
368310           IF TILK-DIERS-TILLK(WS-INDEX-TILLK) = 0 OR                     
368320              TILK-DIERS-ERS(WS-INDEX-TILLK) = 0                          
368330             MOVE +0              TO OBKR-DIERS-KVOT                      
368340           ELSE                                                           
368350             COMPUTE OBKR-DIERS-KVOT =                                    
368360                   TILK-DIERS-TILLK(WS-INDEX-TILLK) /                     
368370                   TILK-DIERS-ERS(WS-INDEX-TILLK)                         
368380           END-IF                                                         
368400           MOVE TILK-BEERS(WS-INDEX-TILLK)                                
368500                               TO OBKR-BEERS                              
368510           MOVE ZEROES         TO OBKR-KVPREAVB                           
368520                                  OBKR-KVPRERO                            
368600                                                                          
368700           MOVE 'S'            TO OBKR-SW                                 
368800        END-IF                                                            
368900        ADD +1                 TO WS-INDEX-TILLK                          
369000     END-PERFORM                                                          
369100     IF WS-INDEX-TILLK = +1                                               
369200        MOVE +0                TO OBKR-KDERS                              
369300     END-IF                                                               
369400     .                                                                    
369500     EJECT                                                                
369600 ECU-KONTROLLERA-ENHETSLAST SECTION.                                      
369700                                                                          
369800     MOVE 'STA ECU-ENHET    '              TO   WS-PGM-POSITION           
369900     MOVE ORAD-ADLAGOMR        TO LAST-ADLAGOMR                           
370000     MOVE OHUV-FLFORBI         TO LAST-FLFORBI                            
370100     MOVE OHUV-FLOVRLEV        TO LAST-FLOVRLEV                           
370200     MOVE OHUV-FLORDSPE        TO LAST-FLORDSPE                           
370300     MOVE ORAD-IDLEVNR         TO LAST-IDLEVNR                            
370400     MOVE ORAD-IDDC            TO LAST-IDDC                               
370500     MOVE ARB-KDFDKRAV         TO LAST-KDFDKRAV                           
370600     MOVE ORAD-KVPREAVB        TO LAST-KVPREAVB                           
370700     MOVE AREG-KVQPACK-3       TO LAST-KVQPACK-3                          
370800     MOVE AREG-KVQPACK-4       TO LAST-KVQPACK-4                          
370900                                                                          
371000     CALL W411LAST USING LAST-W411LAST                                    
371100     .                                                                    
371200     EJECT                                                                
371300 ECV-BERAKNA-WOPS-SKRIV-ORAD SECTION.                                     
371400     MOVE 'STA ECV-WOPS     '              TO   WS-PGM-POSITION           
371500     IF LAST-ADLAGOMR-UT = +0 AND                                         
371600        LAST-KVANTAL-UT  = +0 AND                                         
371700        LAST-KVBEART-UT  = +0                                             
371800*------------------------------------------------------------*            
371900*-----ALLT MOT VANLIGT LAGEROMRÅDE ( 'VANLIG' ORDERRAD)------*            
372000*------------------------------------------------------------*            
372100        PERFORM ECVA-FIXA-LAGEROMR-PLATS                                  
372200        PERFORM ECVB-REDIGERA-WOPS-AREA                                   
372300        PERFORM IMS-09-ISRT-WDQ4-WDQ401                                   
372400        PERFORM UNTIL SEGMENT-FINNS                                       
372500           ADD +1                    TO ORAD-IDLOPNR                      
372600           PERFORM IMS-09-ISRT-WDQ4-WDQ401                                
372700        END-PERFORM                                                       
372800     ELSE                                                                 
372900*------------------------------------------------------------*            
373000*----- TVÅ RADER (KVBEART&KVANTAL)---------------------------*            
373100*------------------------------------------------------------*            
373200                                                                          
373300        IF LAST-KVANTAL-UT > +0 AND LAST-KVBEART-UT > +0                  
373400*------------------------------------------------------------*            
373500*--------- RADEN MOT VANLIGT LAGEROMRÅDE (KVBEART)-----------*            
373600*------------------------------------------------------------*            
373700                                                                          
373800           MOVE LAST-KVBEART-UT      TO ORAD-KVPREAVB                     
373900           COMPUTE ORAD-KVBEART-Q = ORAD-KVPREAVB +                       
374000                                    ORAD-KVPRERO                          
374100           MOVE ORAD-KVSLATT         TO SPAR-ORAD-KVSLATT                 
374200           PERFORM ECVC-BERAEKNA-KVSLATT                                  
374300           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
374400           MOVE ORAD-ADLAGOMR        TO WS-ADLAGOMR                       
374500           MOVE ORAD-ADGANG          TO WS-ADGANG                         
374600           MOVE ORAD-ADPLATS         TO WS-ADPLATS                        
374700           PERFORM ECVB-REDIGERA-WOPS-AREA                                
374800           PERFORM IMS-09-ISRT-WDQ4-WDQ401                                
374900           PERFORM UNTIL SEGMENT-FINNS                                    
375000              ADD +1                 TO ORAD-IDLOPNR                      
375100              PERFORM IMS-09-ISRT-WDQ4-WDQ401                             
375200           END-PERFORM                                                    
375300     EJECT                                                                
375400*------------------------------------------------------------*            
375500*--------- RADEN MOT 'ENHETS' LAGEROMRÅDE (KVANTAL)----------*            
375600*------------------------------------------------------------*            
375700                                                                          
375800           MOVE WS-ADLAGOMR          TO ORAD-ADLAGOMR                     
375900           MOVE WS-ADGANG            TO ORAD-ADGANG                       
376000           MOVE WS-ADPLATS           TO ORAD-ADPLATS                      
376100                                                                          
376200           MOVE +0                   TO ORAD-KVBEART                      
376300           MOVE LAST-KVANTAL-UT      TO ORAD-KVBEART-Q                    
376400                                        ORAD-KVPREAVB                     
376500           MOVE LAST-ADLAGOMR-UT     TO ORAD-ADLAGOMR                     
376600           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64                           
376700             CONTINUE                                                     
376800           ELSE                                                           
376900             IF LAST-ADGANG-UT > ZERO                                     
377000               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
377100             END-IF                                                       
377200           END-IF                                                         
377300           MOVE +0                   TO ORAD-KVPRERO                      
377400           MOVE 1.0000               TO ORAD-RERF-RAD                     
377500           COMPUTE ORAD-KVSLATT = SPAR-ORAD-KVSLATT - ORAD-KVSLATT        
377600           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
377700           PERFORM ECVB-REDIGERA-WOPS-AREA                                
377800           PERFORM IMS-09-ISRT-WDQ4-WDQ401                                
377900           PERFORM UNTIL SEGMENT-FINNS                                    
378000              ADD +1                 TO ORAD-IDLOPNR                      
378100              PERFORM IMS-09-ISRT-WDQ4-WDQ401                             
378200           END-PERFORM                                                    
378300        ELSE                                                              
378400*------------------------------------------------------------*            
378500*-----ALLT MOT 'ENHETS' LAGEROMRÅDE -------------------------*            
378600*------------------------------------------------------------*            
378700           MOVE LAST-ADLAGOMR-UT  TO ORAD-ADLAGOMR                        
378800           IF LAST-ADLAGOMR-UT = 62 OR 63 OR 64 OR ZERO                   
378900             CONTINUE                                                     
379000           ELSE                                                           
379100             IF LAST-ADGANG-UT > ZERO                                     
379200               MOVE LAST-ADGANG-UT   TO ORAD-ADGANG                       
379300             END-IF                                                       
379400           END-IF                                                         
379500           MOVE 1.0000            TO ORAD-RERF-RAD                        
379600           PERFORM ECVA-FIXA-LAGEROMR-PLATS                               
379700           PERFORM ECVB-REDIGERA-WOPS-AREA                                
379800           PERFORM IMS-09-ISRT-WDQ4-WDQ401                                
379900           PERFORM UNTIL SEGMENT-FINNS                                    
380000              ADD +1              TO ORAD-IDLOPNR                         
380100              PERFORM IMS-09-ISRT-WDQ4-WDQ401                             
380200           END-PERFORM                                                    
380300        END-IF                                                            
380400     END-IF                                                               
380500     .                                                                    
380600     EJECT                                                                
380700 ECVA-FIXA-LAGEROMR-PLATS SECTION.                                        
380800                                                                          
380900     MOVE ORAD-ADLAGOMR        TO ADRS-ADLAGOMR-IN                        
381000     MOVE ORAD-ADPLATS         TO ADRS-ADPLATS-IN                         
381100     MOVE OHUV-BEVARREF        TO ADRS-BEVARREF-IN                        
381200     MOVE OHUV-FLFORBI         TO ADRS-FLFORBI-IN                         
381300     MOVE OHUV-IDDISTR         TO ADRS-IDDISTR-IN                         
381400     MOVE 1                    TO ADRS-KDCALL-IN                          
381500     MOVE ORAD-IDDC            TO ADRS-IDDC-IN                            
381600     MOVE OHUV-KDORDKL         TO ADRS-KDORDKL-IN                         
381700     MOVE ORAD-KVBEART-Q       TO ADRS-KVBEART-Q-IN                       
381800     MOVE ORAD-VLARTNTO        TO ADRS-VLARTNTO-IN                        
381900                                                                          
382000     CALL W413ADRS USING ADRS-W413ADRS                                    
382100                                                                          
382200*- KAMPANJORDRAR FÅR ETT FIKTIVT LAGEROMRÅDE.E'TRACKER 2218613.           
382300     IF OHUV-IDKAMPRF > 0                                                 
382400       MOVE 8                  TO ORAD-ADLAGOMR                           
382500     ELSE                                                                 
382600       MOVE ADRS-ADLAGOMR-UT   TO ORAD-ADLAGOMR                           
382700     END-IF                                                               
382800                                                                          
382900*- CHINESE COMPULSORY CERTIF. = FIKTIVT ORDEROMR. E'TR.6605105.           
383000     PERFORM S10-HAMTA-WDB6-INFO                                          
383100                                                                          
383200     MOVE OHUV-IDDISTR         TO TEST-IDDISTR                            
383300     MOVE ADRS-ADLAGOMR-UT     TO ORAD-ADLAGOMR                           
383400                                                                          
383500     MOVE ADRS-ADPLATS-UT      TO ORAD-ADPLATS                            
383600     .                                                                    
383700     EJECT                                                                
383800 ECVB-REDIGERA-WOPS-AREA SECTION.                                         
383900                                                                          
384000     MOVE +1                   TO AVSR-KDCALL                             
384100     MOVE OHUV-IDORDER         TO AVSR-IDORDER                            
384200     MOVE OHUV-KDORDKL         TO AVSR-KDORDKL                            
384300     MOVE ARB-KDFRAKT          TO AVSR-KDFRAKT                            
384400     MOVE ARB-KDROPACK         TO AVSR-KDROPACK                           
384500     MOVE MSGI-TILOKDAT        TO AVSR-TIREGDAT                           
384600     MOVE MSGI-TILOKTID        TO AVSR-TIHHMM                             
384700                                                                          
384800     MOVE ORAD-ADLAGOMR        TO AVSR-ADLAGOMR(WS-INDEX-WOPS)            
384900     MOVE ORAD-IDLEVNR         TO AVSR-IDLEVNR(WS-INDEX-WOPS)             
385000     MOVE ORAD-IDDC            TO AVSR-IDDC(WS-INDEX-WOPS)                
385100     MOVE ORAD-KDSPEEMB        TO AVSR-KDSPEEMB(WS-INDEX-WOPS)            
385200     MOVE +0                   TO AVSR-KVANNANT(WS-INDEX-WOPS)            
385300     MOVE ORAD-KVBEART-Q       TO AVSR-KVBEART-Q(WS-INDEX-WOPS)           
385400     MOVE ORAD-PRARTNTO        TO AVSR-PRARTNTO(WS-INDEX-WOPS)            
385500     MOVE ORAD-PRAVCOST        TO AVSR-PRAVCOST(WS-INDEX-WOPS)            
385600     MOVE ORAD-DEAL-PR-LINE    TO AVSR-DEAL-PR-LINE(WS-INDEX-WOPS)        
385700     MOVE ORAD-VKART           TO AVSR-VKART(WS-INDEX-WOPS)               
385800     MOVE ORAD-VLARTNTO        TO AVSR-VLARTNTO(WS-INDEX-WOPS)            
385900     MOVE AREG-KDVSOP          TO AVSR-KDVSOP(WS-INDEX-WOPS)              
386000     MOVE AREG-KDFARLIG        TO AVSR-KDFARLIG(WS-INDEX-WOPS)            
386100                                                                          
386200     ADD +1                    TO WS-INDEX-WOPS                           
386300     .                                                                    
386400     EJECT                                                                
386500 ECVC-BERAEKNA-KVSLATT SECTION.                                           
386600                                                                          
386700     IF ORAD-FLRESTN = JA AND OHUV-RESLATT > +0                           
386800                                                                          
386900        COMPUTE WS-RESLATT = OHUV-RESLATT / 100                           
387000                                                                          
387100        COMPUTE ORAD-KVSLATT ROUNDED =                                    
387200               (ORAD-KVPREAVB + ORAD-KVPRERO) * WS-RESLATT                
387300     END-IF                                                               
387400     .                                                                    
387500     EJECT                                                                
387510 ECZ-CHECK-KDERS-IN-DC SECTION.                                           
387520                                                                          
387530     MOVE WS-INDEX-TILLK    TO WS-SAVE-INDEX                              
387540     MOVE +1                TO WS-INDEX-TILLK                             
387550                               IDDC-IX                                    
387560     MOVE NEJ               TO BAL-DC-FND-SW                              
387570                               TILLK-BAL-DC-FND-SW                        
387571                               KDERS-CHAIN-SW                             
387580     MOVE AREG-W411AREG-001 TO ORFK-W411AREG-001(WS-INDEX-MID)            
387590                                                                          
387591     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX OR                 
387592                   TILK-IDARTNR(WS-INDEX-TILLK) = ZERO OR                 
387593                   BAL-DC-FND                          OR                 
387594                   TILLK-BAL-DC-FND                    OR                 
387595                   KDERS-CHAIN                                            
387596                                                                          
387597       PERFORM ECZD-CHECK-KDERS-CHAIN                                     
387598                                                                          
387603       IF KDERS-CHAIN-SW = NEJ                                            
387604        IF TILK-IDARTNR(WS-INDEX-TILLK)       > ZERO AND                  
387605           TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND                  
387606           TILK-FLTILLK-X(WS-INDEX-TILLK) = JA                            
387607            PERFORM UNTIL W-GMT-IDDC-CLEAR(IDDC-IX) = SPACES OR           
387608                          IDDC-IX > IX-DCCLEAR-MAX OR                     
387609                          BAL-DC-FND OR                                   
387610                          TILLK-BAL-DC-FND                                
387611               MOVE TILK-IDARTNR(WS-INDEX-TILLK)                          
387612                                        TO W-IDARTNR-SDCA                 
387613               PERFORM ECZB-CALL-SDCA                                     
387614               IF SDCA-KDORDBEK > 0                                       
387615                  PERFORM ECZA-GET-TILLK-DATA                             
387616                  MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                 
387617                                        TO W-IDARTNR-SDCA                 
387618                  PERFORM ECZB-CALL-SDCA                                  
387619                  IF SDCA-KDORDBEK = 0                                    
387620                    MOVE JA             TO TILLK-BAL-DC-FND-SW            
387621                    MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                        
387622                                        TO W-TILLK-DC                     
387623                  END-IF                                                  
387624               ELSE                                                       
387625                  MOVE JA               TO BAL-DC-FND-SW                  
387626               END-IF                                                     
387627               ADD +1                   TO IDDC-IX                        
387628            END-PERFORM                                                   
387629        END-IF                                                            
387630       END-IF                                                             
387631       ADD +1                          TO WS-INDEX-TILLK                  
387632     END-PERFORM                                                          
387633                                                                          
387634     MOVE WS-SAVE-INDEX                 TO WS-INDEX-TILLK                 
387635     MOVE ORFK-W411AREG-001(WS-INDEX-MID) TO AREG-W411AREG-001            
387636     MOVE ZEROES                        TO SDCA-KDORDBEK                  
387637                                                                          
387638     IF KDERS-CHAIN-SW = NEJ                                              
387639        IF BAL-DC-FND-SW = NEJ AND TILLK-BAL-DC-FND-SW = NEJ              
387640           MOVE '11'                       TO W-TILLK-DC                  
387641        END-IF                                                            
387642     END-IF                                                               
387643     .                                                                    
387644     EJECT                                                                
387645*****************************************************************         
387646*IF A(KDERS 22) SUPERSEEDED BY B(KDERS-25) AND IS SUPERSEEDED             
387647*BY C1(KDERS 00) AND C2(KDERS 00),C1,C2 WILL BE SKIPPED AND               
387648*A WILL BE CHECKED FOR STOCKS, IF NOT OCC61                               
387649*****************************************************************         
387650 ECZD-CHECK-KDERS-CHAIN SECTION.                                          
387651                                                                          
387652     IF TILK-IDARTNR(WS-INDEX-TILLK) > ZERO AND                           
387653        TILK-IDARTNR-TILLK(WS-INDEX-TILLK) > ZERO AND                     
387654        TILK-FLTILLK-X(WS-INDEX-TILLK) = NEJ AND                          
387655       (TILK-KDERS(WS-INDEX-TILLK) = 14 OR 15 OR 18 OR                    
387656                                     24 OR 25 OR 28)                      
387657          MOVE JA              TO KDERS-CHAIN-SW                          
387658     END-IF                                                               
387659     .                                                                    
387660     EJECT                                                                
387661 ECZA-GET-TILLK-DATA SECTION.                                             
387662                                                                          
387663     MOVE AREG-FLREFILL        TO W-FLREFILL-MAIN                         
387664     MOVE AREG-KDPRODSL        TO W-KDPRODSL-MAIN                         
387665     MOVE AREG-KDSORT          TO W-KDSORT-MAIN                           
387666     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-MAIN                        
387667     MOVE AREG-REDIRLEV        TO W-REDIRLEV-MAIN                         
387668                                                                          
387669     PERFORM ED-LAES-TILLK-DATA                                           
387670                                                                          
387671     MOVE AREG-FLREFILL        TO W-FLREFILL-REPL                         
387672     MOVE AREG-KDPRODSL        TO W-KDPRODSL-REPL                         
387673     MOVE AREG-KDSORT          TO W-KDSORT-REPL                           
387674     MOVE AREG-KVQPACK-1       TO W-KVQPACK-1-REPL                        
387675     MOVE AREG-REDIRLEV        TO W-REDIRLEV-REPL                         
387676                                                                          
387677     MOVE W-FLREFILL-MAIN      TO AREG-FLREFILL                           
387678     MOVE W-KDPRODSL-MAIN      TO AREG-KDPRODSL                           
387679     MOVE W-KDSORT-MAIN        TO AREG-KDSORT                             
387680     MOVE W-KVQPACK-1-MAIN     TO AREG-KVQPACK-1                          
387681     MOVE W-REDIRLEV-MAIN      TO AREG-REDIRLEV                           
387682     .                                                                    
387683     EJECT                                                                
387684 ECZB-CALL-SDCA   SECTION.                                                
387685                                                                          
387686     IF ORAD-IDARTNR = W-IDARTNR-SDCA                                     
387687        MOVE ORAD-IDARTNR         TO SDCA-IDARTNR                         
387688        MOVE AREG-FLREFILL        TO SDCA-FLREFILL                        
387689        MOVE AREG-KDPRODSL        TO SDCA-KDPRODSL                        
387690        MOVE AREG-KDSORT          TO SDCA-KDSORT                          
387691        MOVE AREG-KVQPACK-1       TO SDCA-KVQPACK-1                       
387692        MOVE AREG-REDIRLEV        TO SDCA-REDIRLEV                        
387693     ELSE                                                                 
387694        MOVE W-IDARTNR-SDCA       TO SDCA-IDARTNR                         
387695        MOVE W-FLREFILL-REPL      TO SDCA-FLREFILL                        
387696        MOVE W-KDPRODSL-REPL      TO SDCA-KDPRODSL                        
387697        MOVE W-KDSORT-REPL        TO SDCA-KDSORT                          
387698        MOVE W-KVQPACK-1-REPL     TO SDCA-KVQPACK-1                       
387699        MOVE W-REDIRLEV-REPL      TO SDCA-REDIRLEV                        
387700     END-IF                                                               
387701                                                                          
387702     MOVE OHUV-IDDISTR         TO SDCA-IDDISTR                            
387703     MOVE OHUV-FLFORBI         TO SDCA-FLFORBI                            
387704     MOVE NEJ                  TO SDCA-FLORDSPE                           
387705     MOVE W-GMT-IDDC-CLEAR(IDDC-IX)                                       
387706                               TO SDCA-IDDC                               
387707     MOVE OHUV-IDDC-TVS        TO SDCA-IDDC-TVS                           
387708     MOVE ORAD-IDLEVNR         TO SDCA-IDLEVNR                            
387709     MOVE ORAD-IDSYSTEM        TO SDCA-IDSYSTEM                           
387710     MOVE ORAD-KDORDKL         TO SDCA-KDORDKL                            
387711     MOVE ORAD-KDORDING        TO SDCA-KDORDING                           
387712     MOVE ORAD-KVBEART-Q       TO SDCA-KVBEART-Q                          
387713     MOVE ORAD-FLSDCLEV        TO SDCA-FLSDCLEV                           
387714     MOVE +0                   TO SDCA-TIREPDAT                           
387715     MOVE +0                   TO SDCA-KVOKS-PREL                         
387716     MOVE +2                   TO SDCA-KDCALL                             
387717     MOVE +1                   TO SDCA-IXDCCLEAR                          
387718                                                                          
387719     CALL W411SDCA USING SDCA-W411SDCA SDCA-ARTS-PCB                      
387720                                       SDCA-WDB6-PCB                      
387721                                       SDCA-WDK9-PCB                      
387722                                       SDCA-WDR6-PCB                      
387723                                       SDCA-WDK6-PCB                      
387724                                       SDCA-WDQ4B-PCB                     
387725                                       SDCA-WDQ2-PCB                      
387726                                       SDCA-WDQ4-PCB                      
387727                                       SDCA-WDB6-2-PCB                    
387728                                       SDCA-WDK6-2-PCB                    
387729                                       SDCA-WDK7-2-PCB                    
387730                                       SDCA-WDK7-3-PCB                    
387731     .                                                                    
387732     EJECT                                                                
387733 S07-SPACE-SDCA-KDORDBEK  SECTION.                                        
387734                                                                          
387735     IF SDCA-KDORDBEK-FIRST-SDC > 0                                       
387736         MOVE ZEROES  TO SDCA-KDORDBEK-FIRST-SDC                          
387737     ELSE                                                                 
387738        IF SDCA-KDORDBEK-SECOND-SDC > 0                                   
387739           MOVE ZEROES  TO SDCA-KDORDBEK-SECOND-SDC                       
387740        ELSE                                                              
387741           IF SDCA-KDORDBEK > 0                                           
387742              MOVE ZEROES  TO SDCA-KDORDBEK                               
387743           END-IF                                                         
387744        END-IF                                                            
387745     END-IF                                                               
387746     .                                                                    
387747                                                                          
387748     EJECT                                                                
387749 ED-LAES-TILLK-DATA SECTION.                                              
387750                                                                          
387800     MOVE 'STA ED-TILLK     '              TO   WS-PGM-POSITION           
387900     MOVE TILK-IDARTNR-TILLK(WS-INDEX-TILLK)                              
388000                               TO AREG-IDARTNR                            
388100                                                                          
388200     CALL W411AREG USING AREG-W411AREG                                    
388300                         AREG-WDK6-PCB                                    
388400                         AREG-WDK7-PCB                                    
388500     .                                                                    
388600     EJECT                                                                
388700 F-HOPPA-TILL-SVARSBILD SECTION.                                          
388800                                                                          
388900     MOVE 'STA F-HOPPA      '              TO   WS-PGM-POSITION           
389000     MOVE MFS-KDMFSFOR           TO 4213-SPRAK                            
389100     MOVE WS-IDDISTR             TO 4213-IDDISTR-IN                       
389200     MOVE WS-IDKUNDNR            TO 4213-IDKUNDNR-IN                      
389300     MOVE WS-IDORDNR             TO 4213-IDORDNR-IN                       
389400     MOVE MFS-RENSA-FAELT        TO 4213-IDDISTR-UT                       
389500                                    4213-IDKUNDNR-UT                      
389600                                    4213-IDORDNR-UT                       
389700                                                                          
389800     IF MID-KDTRTYP = 'V'                                                 
389900        MOVE 'W4T213V '          TO 4213-TRANSKOD                         
390000        PERFORM IMS-INSERT-4213V-MSG                                      
390100     ELSE                                                                 
390200        PERFORM IMS-INSERT-4213-MSG                                       
390300     END-IF                                                               
390400                                                                          
390500     MOVE JA                     TO HOPP                                  
390600     .                                                                    
390700     EJECT                                                                
390800 G-VISA-TOM-SIDA SECTION.                                                 
390900                                                                          
391000     MOVE 'STA G-VISA       '              TO   WS-PGM-POSITION           
391100     MOVE +1 TO WS-INDEX                                                  
391200     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
391300       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR(WS-INDEX)                    
391400                                 MOD-KVBEART(WS-INDEX)                    
391500                                 MOD-PRARTNTO(WS-INDEX)                   
391600                                 MOD-TITPO(WS-INDEX)                      
391700                                 MOD-FLRESTN(WS-INDEX)                    
391800                                 MOD-FLSLATT(WS-INDEX)                    
391900                                 MOD-KDKVBRYT(WS-INDEX)                   
392000                                 MOD-FLINVEST(WS-INDEX)                   
392100                                 MOD-KDVRINFO(WS-INDEX)                   
392200                                 MOD-BERADREF(WS-INDEX)                   
392300                                 MOD-FLORDING(WS-INDEX)                   
392400       ADD  +1 TO WS-INDEX                                                
392500     END-PERFORM                                                          
392600     MOVE MID-BEVOLREF           TO MOD-BEVOLREF                          
392700     MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDARTNR-ATTR(1)                   
392800     .                                                                    
392900     EJECT                                                                
393000 H-STARTA-BIPACKNINGEN SECTION.                                           
393100                                                                          
393200     MOVE 'STA H-BIPACK     '              TO   WS-PGM-POSITION           
393300     COMPUTE 4297-LL = LENGTH OF 4297-MID-W4I29701 + 17                   
393400     MOVE MFS-KDMFSFOR           TO 4297-SPRAK                            
393500                                                                          
393600     MOVE W-IDDISTR              TO 4297-MID-IDDISTR                      
393700     MOVE W-IDKUNDNR             TO 4297-MID-IDKUNDNR                     
393800     MOVE W-IDKUNDRF             TO 4297-MID-IDKUNDRF                     
393900                                                                          
394000     MOVE OHUV-KDTPOTYP          TO 4297-MID-KDTPOTYP                     
394100     MOVE OHUV-KDORDKL           TO 4297-MID-KDORDKL                      
394200     MOVE OHUV-KDFAKTYP          TO 4297-MID-KDFAKTYP                     
394300     MOVE OHUV-IDKAMPRF          TO 4297-MID-IDKAMPRF                     
394400     MOVE OHUV-IDKONTO           TO 4297-MID-IDKONTO                      
394500     MOVE OHUV-IDANALYS          TO 4297-MID-IDANALYS                     
394600     MOVE OHUV-IDKST             TO 4297-MID-IDKST                        
394700     MOVE OHUV-IDANALYS          TO 4297-MID-IDANALYS                     
394800     MOVE OHUV-FLFORBI           TO 4297-MID-FLFORBI                      
394900     MOVE OHUV-IDORDER           TO 4297-MID-IDORDER                      
395000     MOVE OHUV-BEKUNDRF          TO 4297-MID-BEKUNDRF                     
395100     MOVE OHUV-TIREGDAT          TO 4297-MID-TIREGDAT                     
395200     MOVE OHUV-IDFTG             TO 4297-MID-IDFTG                        
395300     MOVE OHUV-BEVARREF          TO 4297-MID-BEVARREF                     
395400     MOVE OHUV-IDBIPREF          TO 4297-MID-IDBIPREF                     
395500     MOVE ARB-KDROPACK           TO 4297-MID-KDROPACK                     
395600     MOVE ARB-KDFRAKT            TO 4297-MID-KDFRAKT                      
395700     IF OHUV-IDDC-TVS NOT = SPACE                                         
395800       MOVE OHUV-IDDC-TVS        TO 4297-MID-IDDC                         
395900     ELSE                                                                 
396000       MOVE SPACE                TO 4297-MID-IDDC                         
396100     END-IF                                                               
396200                                                                          
396300     PERFORM IMS-INSERT-4297-MSG                                          
396400     MOVE JA                     TO HOPP                                  
396500     .                                                                    
396600     EJECT                                                                
396700 I-SKICKA-PRISFRAGA SECTION.                                              
396800                                                                          
396900     MOVE 1                      TO 3039-REQU-IDMSGVER                    
397000     MOVE SPACE                  TO 3039-REQU-KDPGMACT                    
397100     MOVE 'W4021200'             TO 3039-REQU-IDUSER                      
397200                                                                          
397300     MOVE SPACE                  TO 3039-MID-IDBUNDLE                     
397400     MOVE ORAD-IDDISTR           TO 3039-MID-IDDISTR                      
397500     MOVE ORAD-IDKUNDNR          TO 3039-MID-IDKUNDNR                     
397600     MOVE ORAD-IDORDNR7          TO 3039-MID-IDBUNDLE                     
397700     IF MID-IDARTNR(WS-INDEX-MID-MAX) = ALL '+'                           
397800       MOVE W-IDDISTR            TO 3039-MID-IDDISTR                      
397900       MOVE W-IDKUNDNR           TO 3039-MID-IDKUNDNR                     
398000       MOVE W-IDKUNDRF           TO 3039-MID-IDBUNDLE                     
398100     END-IF                                                               
398200     MOVE ZERO                   TO 3039-MID-IDPRQUES                     
398300                                                                          
398400     PERFORM S04-SKICKA-OPEN                                              
398500     PERFORM S04-SKICKA-MEDDELANDE                                        
398600     PERFORM S04-SKICKA-CLOSE                                             
398700                                                                          
398800     .                                                                    
398900     EJECT                                                                
399000 Z-FINIT-INSERT-MSG SECTION.                                              
399100                                                                          
399200     IF MED-IDMFSFEL NOT = SPACE                                          
399300         CALL WMEDKONV USING MED-WMEDAREA                                 
399400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
399500     END-IF                                                               
399600                                                                          
399700     IF NOT ALLT-OK                                                       
399800        PERFORM MFS-ROER-EJ-BILD                                          
399900     END-IF                                                               
400000                                                                          
400100     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O21201 + 4                        
400200     PERFORM IMS-INSERT-MSG                                               
400300     .                                                                    
400400     EJECT                                                                
401900 S02-RENSA-TILLK-TAB SECTION.                                             
402000                                                                          
402100     MOVE 'STA S02-         '              TO   WS-PGM-POSITION           
402200     MOVE +1              TO WS-INDEX-TILLK                               
402300     PERFORM UNTIL WS-INDEX-TILLK > WS-INDEX-TILLK-MAX                    
402400        MOVE +0           TO TILK-IDARTNR(WS-INDEX-TILLK)                 
402500        MOVE +0           TO TILK-IDARTNR-TILLK(WS-INDEX-TILLK)           
402600        MOVE SPACE        TO TILK-BEERS(WS-INDEX-TILLK)                   
402700        MOVE +0           TO TILK-DIERS-ERS(WS-INDEX-TILLK)               
402800        MOVE +0           TO TILK-DIERS-TILLK(WS-INDEX-TILLK)             
402900        MOVE SPACE        TO TILK-FLFINLV(WS-INDEX-TILLK)                 
403000        MOVE SPACE        TO TILK-FLPRTILL(WS-INDEX-TILLK)                
403100        MOVE SPACE        TO TILK-FLTILLK-X(WS-INDEX-TILLK)               
403200        MOVE +0           TO TILK-KDERS(WS-INDEX-TILLK)                   
403300        MOVE SPACE        TO TILK-KDPRTYP(WS-INDEX-TILLK)                 
403400        MOVE +0           TO TILK-KVBEART(WS-INDEX-TILLK)                 
403500        MOVE +0           TO TILK-PRARTNTO(WS-INDEX-TILLK)                
403600        INITIALIZE        TILK-DEAL-PR-LINE(WS-INDEX-TILLK)               
403700        MOVE +0           TO TILK-TIPRIS(WS-INDEX-TILLK)                  
403800        MOVE +0           TO TILK-REKSIFFR-TILLK(WS-INDEX-TILLK)          
403900        ADD +1            TO WS-INDEX-TILLK                               
404000     END-PERFORM                                                          
404100     MOVE +1              TO WS-INDEX-TILLK                               
404200     .                                                                    
404300     EJECT                                                                
404400                                                                          
404500                                                                          
404600 S03-DATA-TILL-DEL-NOTE SECTION.                                          
404700                                                                          
404800     MOVE 'STA S03-         '              TO   WS-PGM-POSITION           
404900     MOVE OHUV-IDDISTR             TO TEST-IDDISTR                        
405000     IF DIST07-USA-RETAILER-DNOTE                                         
405100     OR DIST07-CAN-RETAILER                                               
405200        INITIALIZE DNOT-ORDER-INFO                                        
405300                                                                          
405400        MOVE IDPGM                    TO DNOT-IDPGM                       
405500        MOVE OHUV-IDORDER             TO DNOT-IDORDER                     
405600        MOVE ORAD-IDARTNR             TO DNOT-IDARTNR                     
405700        MOVE ORAD-IDDC                TO DNOT-IDDC                        
405800        MOVE OHUV-ADGMT-GATA          TO DNOT-ADGMT-GATA                  
405900        MOVE OHUV-ADGMT-PADR          TO DNOT-ADGMT-PADR                  
406000        MOVE OHUV-ADGMT-LAND          TO DNOT-ADGMT-LAND                  
406100        MOVE OHUV-BEGMT-RAD1          TO DNOT-BEGMT-RAD1                  
406200        MOVE OHUV-BEGMT-RAD2          TO DNOT-BEGMT-RAD2                  
406300        MOVE OHUV-BEKUNDRF            TO DNOT-BEKUNDRF                    
406400        MOVE ORAD-BERADREF            TO DNOT-BERADREF                    
406500        MOVE OHUV-IDGMTREF            TO DNOT-IDGMTREF                    
406600        MOVE OHUV-IDDC-PRIM           TO DNOT-IDDC-PRIM                   
406700        MOVE ORAD-IDKUNDRF-RO         TO DNOT-IDKUNDRF-RO                 
406800        MOVE ORAD-FLTILLK             TO DNOT-FLTILLK                     
406900        MOVE OHUV-KDORDKL             TO DNOT-KDORDKL                     
407000        MOVE ARB-KDFRAKT              TO DNOT-KDFRAKT                     
407100        MOVE ORAD-KVBEART             TO DNOT-KVBEART                     
407200        MOVE ORAD-KVBEART-Q           TO DNOT-KVBEART-Q                   
407300        MOVE ORAD-REKSIFFR            TO DNOT-REKSIFFR                    
407400        MOVE ORAD-TIREGDAT            TO DNOT-TIREGDAT                    
407500        MOVE ORAD-TIREGTID            TO DNOT-TIREGTID                    
407600        MOVE NEJ                      TO DNOT-FLDIRLEV                    
407700                                                                          
407800                                                                          
407900        IF WS-INDEX-MID > WS-INDEX-MID-MAX                                
408000           MOVE JA              TO DNOT-FL-ORAD-LAST                      
408100        END-IF                                                            
408200                                                                          
408300        CALL W411DNOT USING DNOT-W411DNOT                                 
408400                            DNOT-ORQP-PCB                                 
408500                            DNOT-ORQP2-PCB                                
408600                            DNOT-ORQP3-PCB                                
408700                            DNOT-4013-PCB                                 
408800                            DNOT-BENA-PCB                                 
408900     END-IF                                                               
409000     .                                                                    
409100     EJECT                                                                
409200                                                                          
409300 S04-SKICKA-OPEN SECTION.                                                 
409400                                                                          
409500     MOVE 'OPEN'                     TO SEND-KDFUNC                       
409600     MOVE 'CARPARTS.PULS.PRQRY' TO SEND-ADDISPABS                         
409700     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-OPEN-AREA                 
409800                                                                          
409900     IF SEND-KDRC > 0                                                     
410000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
410100       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
410200       DELIMITED BY SIZE INTO FELTEXT                                     
410300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
410400     END-IF                                                               
410500     .                                                                    
410600     SKIP3                                                                
410700 S04-SKICKA-MEDDELANDE SECTION.                                           
410800                                                                          
410900     MOVE 'PUT'                      TO SEND-KDFUNC                       
411000     MOVE LENGTH OF SEND-AREA        TO SEND-KVDLEN                       
411100     CALL WZ01SEND USING SEND-CONTROL-AREA SEND-KVDLEN SEND-AREA          
411200                                                                          
411300     IF SEND-KDRC > 0                                                     
411400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
411500       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
411600       DELIMITED BY SIZE INTO FELTEXT                                     
411700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
411800     END-IF                                                               
411900     .                                                                    
412000     SKIP3                                                                
412100 S04-SKICKA-CLOSE SECTION.                                                
412200                                                                          
412300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
412400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
412500                                                                          
412600     IF SEND-KDRC > 0                                                     
412700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
412800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
412900       DELIMITED BY SIZE INTO FELTEXT                                     
413000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
413100     END-IF                                                               
413200     .                                                                    
413300     EJECT                                                                
413400                                                                          
413500 S05-DELETE-PRICE-Q-LINE SECTION.                                         
413600                                                                          
413700     IF DIST79-DEALER-PRICE                                               
413800       IF OBKR-IDPRQUES > ZERO                                            
413900         INITIALIZE PRQU-W335PRQU                                         
414000         MOVE OBKR-IDDISTR       TO PRQU-IDDISTR                          
414100         MOVE OBKR-IDKUNDNR      TO PRQU-IDKUNDNR                         
414200         MOVE OBKR-IDKUNDRF      TO PRQU-IDKUNDRF                         
414300         MOVE OBKR-IDPRQUES      TO PRQU-IDPRQUES                         
414400         MOVE 4                  TO PRQU-KDCALL                           
414500         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
414600                                            PRQU-WDC7-PCB                 
414700                                            PRQU-SJKO-WDK6-PCB            
414800       END-IF                                                             
414900     END-IF                                                               
415000     .                                                                    
415100                                                                          
415200     EJECT                                                                
415300 S10-HAMTA-WDB6-INFO      SECTION.                                        
415400                                                                          
415500     IF WS-CLDC-IX > IX-DCCLEAR-MAX                                       
415600       CONTINUE                                                           
415700     ELSE                                                                 
415800     IF CLDC-IDDC (WS-CLDC-IX) NOT = WS-IDDC                              
415900                                                                          
416000        MOVE 1 TO WS-CLDC-IX                                              
416100        PERFORM UNTIL WS-CLDC-IX > IX-DCCLEAR-MAX OR                      
416200                      CLDC-IDDC (WS-CLDC-IX) = WS-IDDC OR                 
416300                      CLDC-IDDC (WS-CLDC-IX) = SPACE                      
416400           ADD 1 TO WS-CLDC-IX                                            
416500        END-PERFORM                                                       
416600                                                                          
416700     END-IF                                                               
416800     END-IF                                                               
416900     IF WS-CLDC-IX > IX-DCCLEAR-MAX OR                                    
417000        CLDC-IDDC (WS-CLDC-IX) = SPACE                                    
417100        MOVE WS-IDDC TO W-IDDC-B6                                         
417200        PERFORM IMS-GU-WDB601                                             
417300     ELSE                                                                 
417400        MOVE CLDC-WDB601 (WS-CLDC-IX)  TO DLI-IO-AREA-B601                
417500     END-IF                                                               
417510     .                                                                    
417520     EJECT                                                                
417600 S20-WRONG-PICTURE-MESSAGE SECTION.                                       
417700     SKIP2                                                                
417800* *****************************************************                   
417900*                                                     *                   
418000* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
418100*                                                     *                   
418200* *****************************************************                   
418300     SKIP2                                                                
418400     MOVE JA                  TO HOPP-TILL-0504                           
418500     MOVE 'W0O50401'          TO MFS-IDMOD                                
418600     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
418700     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
418800     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
418900     PERFORM IMS-INSERT-MSG                                               
419000     .                                                                    
419100     EJECT                                                                
419200 MFS-RENSA-MOD-RADER SECTION.                                             
419300                                                                          
419400     MOVE MFS-RENSA-FAELT    TO MOD-BEVOLREF                              
419500     MOVE +1 TO WS-INDEX                                                  
419600     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
419700       MOVE MFS-RENSA-FAELT  TO  MOD-IDARTNR(WS-INDEX)                    
419800                                 MOD-KVBEART(WS-INDEX)                    
419900                                 MOD-PRARTNTO(WS-INDEX)                   
420000                                 MOD-TITPO(WS-INDEX)                      
420100                                 MOD-FLRESTN(WS-INDEX)                    
420200                                 MOD-FLSLATT(WS-INDEX)                    
420300                                 MOD-KDKVBRYT(WS-INDEX)                   
420400                                 MOD-FLINVEST(WS-INDEX)                   
420500                                 MOD-KDVRINFO(WS-INDEX)                   
420600                                 MOD-BERADREF(WS-INDEX)                   
420700                                 MOD-FLORDING(WS-INDEX)                   
420800       ADD  +1 TO WS-INDEX                                                
420900     END-PERFORM                                                          
421000     .                                                                    
421100     EJECT                                                                
421200                                                                          
421300 MFS-ROER-EJ-BILD SECTION.                                                
421400                                                                          
421500     MOVE +1 TO WS-INDEX                                                  
421600     PERFORM UNTIL WS-INDEX > WS-INDEX-MID-MAX                            
421700       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(WS-INDEX)                    
421800                                 MOD-KVBEART(WS-INDEX)                    
421900                                 MOD-PRARTNTO(WS-INDEX)                   
422000                                 MOD-TITPO(WS-INDEX)                      
422100                                 MOD-FLRESTN(WS-INDEX)                    
422200                                 MOD-FLSLATT(WS-INDEX)                    
422300                                 MOD-KDKVBRYT(WS-INDEX)                   
422400                                 MOD-FLINVEST(WS-INDEX)                   
422500                                 MOD-KDVRINFO(WS-INDEX)                   
422600                                 MOD-BERADREF(WS-INDEX)                   
422700                                 MOD-FLORDING(WS-INDEX)                   
422800       ADD  +1 TO WS-INDEX                                                
422900     END-PERFORM                                                          
423000     .                                                                    
423100     EJECT                                                                
423200* --- IMS SEKTIONER ---                                                   
423300                                                                          
423400 IMS-GET-MSG SECTION.                                                     
423500                                                                          
423600     MOVE '  QC' TO GODK-STATUSKODER                                      
423700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
423800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
423900     PERFORM IMS-STATUSKONTROLL                                           
424000     .                                                                    
424100     SKIP2                                                                
424200 IMS-INSERT-MSG SECTION.                                                  
424300                                                                          
424400     IF NOT ENGLISH-TEXT                                                  
424500       MOVE '0' TO MFS-KDHUVOMR                                           
424600     END-IF                                                               
424700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
424800     MOVE SPACE TO GODK-STATUSKODER                                       
424900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
425000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
425100     PERFORM IMS-STATUSKONTROLL                                           
425200     .                                                                    
425300     SKIP2                                                                
425400 IMS-INSERT-4213-MSG SECTION.                                             
425500                                                                          
425600     IF NOT ENGLISH-TEXT                                                  
425700       MOVE '0' TO MFS-KDHUVOMR                                           
425800     END-IF                                                               
425900     MOVE LOW-VALUE TO 4213-Z1 4213-Z2                                    
426000     MOVE SPACE TO GODK-STATUSKODER                                       
426100     CALL CBLTDLI USING ISRT 4213-PCB 4213-MSG-IO-AREA                    
426200     MOVE 4213-STATUS-CODE TO STATUS-WS                                   
426300     PERFORM IMS-STATUSKONTROLL                                           
426400     .                                                                    
426500     EJECT                                                                
426600 IMS-INSERT-4213V-MSG SECTION.                                            
426700                                                                          
426800     IF NOT ENGLISH-TEXT                                                  
426900       MOVE '0' TO MFS-KDHUVOMR                                           
427000     END-IF                                                               
427100     MOVE LOW-VALUE TO 4213-Z1 4213-Z2                                    
427200     MOVE SPACE TO GODK-STATUSKODER                                       
427300     CALL CBLTDLI USING ISRT 4213V-PCB 4213-MSG-IO-AREA                   
427400     MOVE 4213V-STATUS-CODE TO STATUS-WS                                  
427500     PERFORM IMS-STATUSKONTROLL                                           
427600     .                                                                    
427700     SKIP3                                                                
427800 IMS-INSERT-4297-MSG SECTION.                                             
427900                                                                          
428000     MOVE LOW-VALUE TO 4297-Z1 4297-Z2                                    
428100     MOVE SPACE TO GODK-STATUSKODER                                       
428200     CALL CBLTDLI USING ISRT 4297-PCB 4297-MSG-IO-AREA                    
428300     MOVE 4297-STATUS-CODE TO STATUS-WS                                   
428400     PERFORM IMS-STATUSKONTROLL                                           
428500     .                                                                    
428600     EJECT                                                                
428700 IMS-01-GHU-WDQ2-WDQ201 SECTION.                                          
428800                                                                          
428900     STRING 'WDQ201  (WDQ2CSEQ =' W-IDGMTREF-X ')'                        
429000          DELIMITED BY SIZE INTO SSA1                                     
429100     MOVE '  GE'               TO GODK-STATUSKODER                        
429200     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-AREA-OHUV SSA1                
429300     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
429400     PERFORM IMS-STATUSKONTROLL                                           
429500     .                                                                    
429600     SKIP2                                                                
429700                                                                          
429800 IMS-02-REPL-WDQ2-WDQ201 SECTION.                                         
429900                                                                          
430000     MOVE '    '               TO GODK-STATUSKODER                        
430100     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-OHUV                    
430200     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
430300     PERFORM IMS-STATUSKONTROLL                                           
430400     .                                                                    
430500     EJECT                                                                
430600 IMS-03-GNP-WDQ2-WDQ212 SECTION.                                          
430700                                                                          
430800     MOVE   'WDQ212  '         TO SSA1                                    
430900     MOVE '  GE'               TO GODK-STATUSKODER                        
431000     CALL CBLTDLI USING GNP  WDQ2-PCB DLI-IO-AREA-ARB SSA1                
431100     MOVE WDQ2-STATUS-CODE     TO STATUS-WS                               
431200     PERFORM IMS-STATUSKONTROLL                                           
431300     .                                                                    
431400     EJECT                                                                
431500 IMS-07-GU-WDQ1-WDQ101 SECTION.                                           
431600                                                                          
431700     STRING 'WDQ101  (WDQ101KY >' W-WDQ101KY-MIN-X                        
431800                    '&WDQ101KY <' W-WDQ101KY-MAX-X ')'                    
431900          DELIMITED BY SIZE INTO SSA1                                     
432000     MOVE '  GE'               TO GODK-STATUSKODER                        
432100     CALL CBLTDLI USING GU   WDQ1-PCB DLI-IO-AREA-OBKR SSA1               
432200     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
432300     PERFORM IMS-STATUSKONTROLL                                           
432400     .                                                                    
432500     SKIP2                                                                
432600 IMS-08-ISRT-WDQ101 SECTION.                                              
432700                                                                          
432800     MOVE 'WDQ101  '           TO SSA1                                    
432900     MOVE '    '               TO GODK-STATUSKODER                        
433000     CALL CBLTDLI USING ISRT WDQ1-PCB DLI-IO-AREA-OBKR SSA1               
433100     MOVE WDQ1-STATUS-CODE     TO STATUS-WS                               
433200     PERFORM IMS-STATUSKONTROLL                                           
433300     .                                                                    
433400     SKIP2                                                                
433500 IMS-09-ISRT-WDQ4-WDQ401 SECTION.                                         
433600                                                                          
433700     MOVE 'WDQ401  '           TO SSA1                                    
433800     MOVE '  II'               TO GODK-STATUSKODER                        
433900     CALL CBLTDLI USING ISRT WDQ4-PCB DLI-IO-AREA-ORAD SSA1               
434000     MOVE WDQ4-STATUS-CODE     TO STATUS-WS                               
434100     PERFORM IMS-STATUSKONTROLL                                           
434200     .                                                                    
434300     EJECT                                                                
434400 IMS-10-GU-WLARTM-WDK901 SECTION.                                         
434500                                                                          
434600     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
434700          DELIMITED BY SIZE INTO SSA1                                     
434800     MOVE '  GE'               TO GODK-STATUSKODER                        
434900     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA-ART SSA1                  
435000     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
435100     PERFORM IMS-STATUSKONTROLL                                           
435200     .                                                                    
435300                                                                          
435400 IMS-GU-WDB201       SECTION.                                             
435500                                                                          
435600     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
435700          DELIMITED BY SIZE INTO SSA1                                     
435800     MOVE '  '                 TO GODK-STATUSKODER                        
435900     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
436000     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
436100     PERFORM IMS-STATUSKONTROLL                                           
436200     .                                                                    
436300     SKIP2                                                                
437500 IMS-GU-WDB101 SECTION.                                                   
437600                                                                          
437700     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
437800          DELIMITED BY SIZE INTO SSA1                                     
437900     MOVE '  '                 TO GODK-STATUSKODER                        
438000     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
438100     MOVE WDB1-STATUS-CODE     TO STATUS-WS                               
438200     PERFORM IMS-STATUSKONTROLL                                           
438300     .                                                                    
438400                                                                          
438500 IMS-GU-WDB601    SECTION.                                                
438600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
438700          DELIMITED BY SIZE INTO SSA1                                     
438800     MOVE '  GE' TO GODK-STATUSKODER                                      
438900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
439000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
439100     PERFORM IMS-STATUSKONTROLL                                           
439200     IF SEGMENT-SAKNAS                                                    
439300        MOVE SPACE TO DCS-KDDC                                            
439400     END-IF                                                               
439500     .                                                                    
439600     SKIP2                                                                
439700 IMS-GU-WDB301 SECTION.                                                   
439800                                                                          
439900     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
440000                    '!WDB301KY =' W-WDB301KY-DEF-X  ')'                   
440100          DELIMITED BY SIZE INTO SSA1                                     
440200     MOVE '  GE'              TO GODK-STATUSKODER                         
440300     CALL CBLTDLI USING GU WDB3-PCB DLI-IO-AREA-WDB301 SSA1               
440400     MOVE WDB3-STATUS-CODE    TO STATUS-WS                                
440500     PERFORM IMS-STATUSKONTROLL                                           
440600     .                                                                    
440700                                                                          
440800 IMS-GU-WDF502 SECTION.                                                   
440900                                                                          
441000     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-CROSS-X ')'                   
441100          DELIMITED BY SIZE INTO SSA1                                     
441200     MOVE   'WDF502  '        TO SSA2                                     
441300     MOVE '  GE'              TO GODK-STATUSKODER                         
441400     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
441500     MOVE WDF5-STATUS-CODE    TO STATUS-WS                                
441510     PERFORM IMS-STATUSKONTROLL                                           
441520     .                                                                    
441530                                                                          
441540 IMS-ISRT-WDR601 SECTION.                                                 
441550                                                                          
441560     MOVE 'WDR601' TO SSA1                                                
441570     MOVE '  II' TO GODK-STATUSKODER                                      
441580     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-AREA-R601 SSA1               
441590     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
441600     PERFORM IMS-STATUSKONTROLL                                           
441601     .                                                                    
441602                                                                          
441603 IMS-STATUSKONTROLL SECTION.                                              
441604                                                                          
441605     SET STATUS-IX TO 1                                                   
441606     SEARCH GODK-STATUS                                                   
441607       AT END CALL FELLOG                                                 
441608       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
441609     END-SEARCH                                                           
441610     .                                                                    
441620     EJECT                                                                
