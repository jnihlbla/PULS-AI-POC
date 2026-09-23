000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W911ETD.                                                 
000500 AUTHOR.         PRIYA RC.                                                
000600 DATE-WRITTEN.   DEC 2021.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        NEW ETA MODULE CALLED FROM 9113.                                 
001200*                                                                         
001300*    FUNKTION.                                                            
001400*      - PART BALANCE QUERY                                               
001500*        FOR A GIVEN DISTRICT,CUSTOMER, PART AND QUANTITY,                
001600*        CHECK WHETHER BALANCE IS AVAILABLE IN STOCK OR                   
001700*        AT DIRECT SUPPLIER.                                              
002000*                                                                         
002100*    CHANGE LOG:                                                          
002200*    DEC 2021 / STORY 2498359 / NEW ETA MODULE                            
002201*    JAN 2022 / STORY 2498360 / QTY CALCULATION FOR LDC                   
002202*    JAN 2022 / STORY 2498365 / QTY CALC FOR CDC,DC AVAILABLE DATE        
002203*    JAN 2022 / STORY 2498366 / DELIVERY SCHEDULE FOR CDC (2103)          
002204*    FEB 2022 / STORY 2498362 / CHECK & COMPARE THE USER REQUESTED        
002205*       QTY TO INVOICED QTY FOR THE PART FROM NDC TO CDC(2353)            
002206*    MAR 2022 / STORY 2648123 / QUANTITY AVAILABILITY IN NDCS             
002207*    MAR 2022 / STORY 2649215 / STOCKS ON THE WAY FROM SUPPLIER           
002300*                               TO NDC                                    
002301*    MAR 2022 / STORY 2649240 / THE DELIVERY PROMISE FROM SUPPLIER        
002302*                               TO NDCS(2403)                             
002303*    APR 2022 / STORY 2649296 / FOR REFILL PARTS,CHECK THE INVOICE        
002304*     QTY TO USER REQUESTED QTY FOR THE PART FROM CDC TO NDC(2353)        
002305*    MAY 2022 / STORY 2649298 / CALCULATE ETD FOR DDGS PARTS.             
002306*    MAY 2022 / STORY 2651386 / LEAD TIME CALCULATION(4408).              
002307*                                                                         
002320*                                                                         
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002710*    -COPY WY2000W1                                                       
002800     SKIP3                                                                
002900 77  IDPGM                       PIC X(08)   VALUE 'W911ETD '.            
003000 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003100 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  YES                         PIC X       VALUE 'Y'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 01  WS-KVFRYSTI                 PIC 9(2).                                
003510 01  WS-KVDAGAR-DIFF             PIC 9(3).                                
003511 01  WS-KDORDBEK-SUPERS          PIC 9(2)    VALUE ZERO.                  
003520 01  W-AVAIL-QTY                 PIC S9(7)   VALUE ZERO COMP-3.           
003530 01  W-IDDC-X.                                                            
003540     03  W-IDDC              PIC X(02)   VALUE SPACE.                     
003600                                                                          
010610 01 WS-DATA-DEF.                                                          
010620     03 W-KVLS                   PIC S9(7) COMP-3 VALUE ZERO.             
010621     03 W-KVRESS                 PIC S9(7) COMP-3 VALUE ZERO.             
010622     03 W-KVUTRS                 PIC S9(7) COMP-3 VALUE ZERO.             
010623     03 W-KVSPARR-KVAL           PIC S9(7) COMP-3 VALUE ZERO.             
010624     03 W-KVSPANT                PIC S9(7) COMP-3 VALUE ZERO.             
010625                                                                          
010626     03 W-OKS-VOR                PIC S9(7) COMP-3 VALUE ZERO.             
010627     03 W-OKS-DAY                PIC S9(7) COMP-3 VALUE ZERO.             
010628     03 W-OKS-BULK               PIC S9(7) COMP-3 VALUE ZERO.             
010651                                                                          
010652     03 W-BO-QTY-VOR             PIC S9(9) COMP-3 VALUE ZERO.             
010653     03 W-BO-QTY-C1              PIC S9(9) COMP-3 VALUE ZERO.             
010654     03 W-BO-QTY-BULK            PIC S9(9) COMP-3 VALUE ZERO.             
010655     03 W-BO-QTY-DAY             PIC S9(9) COMP-3 VALUE ZERO.             
010656                                                                          
010657     03 W-CDC-QTY                PIC S9(9) COMP-3 VALUE ZERO.             
010658     03 W-QTY-NEEDED             PIC S9(9) COMP-3 VALUE ZERO.             
010659                                                                          
010660     03 W-WDA5ASEQ-MIN-X.                                                 
010661       05  W-WDA5A-IDARTNR-MIN     PIC S9(9) VALUE ZERO COMP-3.           
010662       05  W-WDA5A-IDDC-MIN        PIC X(2)  VALUE SPACE.                 
010663       05  FILLER                  PIC X(2)  VALUE LOW-VALUE.             
010664                                                                          
010665     03 W-WDA5ASEQ-MAX-X.                                                 
010666       05  W-WDA5A-IDARTNR-MAX     PIC S9(9) VALUE ZERO COMP-3.           
010667       05  W-WDA5A-IDDC-MAX        PIC X(2)  VALUE SPACE.                 
010668       05  FILLER                  PIC X(2)  VALUE HIGH-VALUE.            
010669                                                                          
010670     03 W-WDA6JSEQ-MIN-X.                                                 
010671       05  SEQJ-IDARTNR-MIN        PIC S9(9) VALUE ZERO COMP-3.           
010680       05  SEQJ-TIREGDAT-MIN       PIC S9(7) VALUE ZERO COMP-3.           
010690       05  SEQJ-TIREFTID-MIN       PIC S9(9) VALUE ZERO COMP-3.           
010691     03 W-WDA6JSEQ-MAX-X.                                                 
010692       05  SEQJ-IDARTNR-MAX        PIC S9(9) VALUE ZERO COMP-3.           
010693       05  SEQJ-TIREGDAT-MAX       PIC S9(7) VALUE ZERO COMP-3.           
010694       05  SEQJ-TIREFTID-MAX       PIC S9(9) VALUE ZERO COMP-3.           
010695                                                                          
010696     03 W-WDD901KY-X.                                                     
010697       05  W-IDARTNR-WDD9-X.                                              
010698          07  W-IDARTNR-WDD9       PIC S9(8) VALUE ZERO COMP-3.           
010699       05  W-IDDC-WDD9-X.                                                 
010700          07  W-IDDC-WDD9          PIC X(2)  VALUE SPACE.                 
010710     03  W-WDF2A1KY-MIN-X.                                                
010720         05  W-IDARTNR-MIN         PIC S9(9)   VALUE ZERO COMP-3.         
010730         05  W-IDLEVNR-MIN         PIC  X(5)   VALUE SPACE.               
010740                                                                          
010750     03  W-WDF2A1KY-MAX-X.                                                
010760         05  W-IDARTNR-MAX          PIC S9(9)   VALUE ZERO COMP-3.        
010770         05  W-IDLEVNR-MAX          PIC  X(5)   VALUE SPACE.              
010780                                                                          
010790     03  W-WDF201KY-X.                                                    
010791         05  W-IDLEVNR-WDF2         PIC  X(5)   VALUE SPACE.              
010792         05  W-IDDIRGRP-WDF2        PIC X(10)   VALUE SPACE.              
010800                                                                          
010900     03 W-STOCK-ON-WAY             PIC S9(7) COMP-3 VALUE 0.              
011000     03 W-STOCK-DEL-SCHDL          PIC S9(7) COMP-3 VALUE 0.              
011100     03 W-IDDC-REF                 PIC X(2)  VALUE SPACES.                
011200                                                                          
011300     03 W-STOCK-ON-WAY-DATE        PIC S9(7) COMP-3 VALUE 0.              
011400     03 W-DC-AVAIL-DATE            PIC S9(7) COMP-3 VALUE 0.              
011500     03 WS-CURRENT-DATE            PIC 9(6)  VALUE 0.                     
011600     03 WS-CURRRENT-AAAAVV.                                               
011700        05 WS-CURRENT-AA1          PIC 9(2) VALUE 20.                     
011800        05 WS-CURRENT-AAVV         PIC 9(4) VALUE 0.                      
011900        05 WS-CURR-AAVV REDEFINES WS-CURRENT-AAVV.                        
012000           07 WS-CURR-AA2          PIC 9(2).                              
012100           07 WS-CURR-VV           PIC 9(2).                              
012200                                                                          
012300     03 INL-TABEL.                                                        
012400        05 INL-LINES OCCURS 200 INDEXED BY INL-IX.                        
012500           07 INL-QTY              PIC S9(7) COMP-3 VALUE 0.              
012600           07 INL-DC-DATE          PIC 9(6) VALUE 0.                      
012700                                                                          
012800     03 DC-TABEL.                                                         
012900        05 DC-LINES OCCURS 7 INDEXED BY DC-IX.                            
013000           07 DC-NUMBER            PIC X(2).                              
013100           07 DC-AVAIL-QTY         PIC S9(7)   COMP-3.                    
013110           07 DC-IDDC-REF          PIC X(2).                              
013120           07 DC-FLFLYG            PIC X(1).                              
013200                                                                          
013201     03 DDGS-TABLE.                                                       
013202        05 DDGS-LINE OCCURS 5 INDEXED BY DDGS-IX.                         
013203           07 DDGS-CLASS           PIC S9  COMP-3.                        
013204           07 DDGS-MIN-QTY         PIC S9(7) COMP-3.                      
013205           07 DDGS-IDDC            PIC X(2).                              
013207                                                                          
013208     03  W-DASTADAT-X.                                                    
013209         05  W-DASTADAT          PIC 9(8)    VALUE ZERO.                  
013210     03  W-IDDISTR-X.                                                     
013211         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
013212                                                                          
013213     03  W-IDKUNDNR-FOM-X.                                                
013214         05  W-IDKUNDNR-FOM      PIC S9(7)   VALUE ZERO COMP-3.           
013215     03  W-IDKUNDNR-TOM-X.                                                
013216         05  W-IDKUNDNR-TOM      PIC S9(7)   VALUE ZERO COMP-3.           
013217                                                                          
013218     03  W-DISP                      PIC S9(8)   VALUE ZERO.              
013219     03  W-KVAKS-SDC                 PIC 9(7)    VALUE ZERO.              
013220     03  W-KVOKS-DAG                 PIC 9(7)    VALUE ZERO.              
013221     03  W-KVOKS-BULK                PIC 9(7)    VALUE ZERO.              
013222     03  W-FLFLYG                PIC X(1)    VALUE SPACE.                 
013223     03  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                 
013224     03  DIR-INDX                PIC S9(9)   VALUE +0  COMP SYNC.         
013225     03  DDGS-ROWS               PIC 9(1).                                
013226     03  W-DDGS-STOCK-BAL        PIC S9(7)   VALUE ZERO COMP-3.           
013227     03  W-DDGS-RFS              PIC S9(7)   VALUE ZERO COMP-3.           
013228     03 W-WORK.                                                           
013229        05 W-IDDISTR-OK          PIC X.                                   
013230        05 W-IDKUNDNR-OK         PIC X.                                   
013231        05 W-KDFRAKT-OK          PIC X.                                   
013232        05 W-IDVAT-OK            PIC X.                                   
013233        05 W-IDPARTNR-OK         PIC X.                                   
013234        05 W-KDKREDSP-OK         PIC X.                                   
013235        05 W-KDKREDSP            PIC X.                                   
013236        05 W-IDRFTAB             PIC X(3).                                
013237     03  W-IDDC-B6-X.                                                     
013238         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
013239     03  W-IDDC-B616-X.                                                   
013240         05  W-IDDC-B616     PIC X(2)   VALUE SPACE.                      
013241     03  W-IDGMT-X.                                                       
013242         05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
013243         05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
013244                                                                          
013245     03  W-WDB101KY-X.                                                    
013246         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
013247         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
013248                                                                          
013249*01  -COPY WWDCKONS                                                       
013250                                                                          
013251 01  FILLER                      PIC  X(16) VALUE 'EG-LAND'.              
013252 01  TEST-IDLANDX2               PIC  X(2).                               
013253 01  FILLER REDEFINES TEST-IDLANDX2.                                      
013254*    03    -COPY WWLANDX2.                                                
013255     EJECT                                                                
013256 77  ALL-SW                      PIC X       VALUE 'J'.                   
013257     88  ALL-OK                              VALUE 'J'.                   
013258     88  ALL-NOT-OK                          VALUE 'N'.                   
013259                                                                          
013260 77  SALDO-SW                    PIC X       VALUE 'J'.                   
013261     88  SALDO-FINNS                         VALUE 'J'.                   
013262     88  SALDO-SAKNAS                        VALUE 'N'.                   
013263                                                                          
013264 77  SALDO-LEV-SW                PIC X       VALUE 'N'.                   
013265     88  SALDO-LEV                           VALUE 'J'.                   
013266                                                                          
013267 77  NDC-STOCKS-SW               PIC X       VALUE 'N'.                   
013268     88  NDC-STOCKS                          VALUE 'J'.                   
013269                                                                          
013270 77  DIRLEV-SW                   PIC X       VALUE 'J'.                   
013271     88  DIRLEV                              VALUE 'J'.                   
013272                                                                          
013273 77  STOCK-SW                    PIC X       VALUE 'N'.                   
013274     88  STOCK-ON-WAY                        VALUE 'J'.                   
013275                                                                          
013276 77  DDGS-IDDC-SW                PIC X       VALUE 'N'.                   
013277     88  DDGS-IDDC-FND                       VALUE 'J'.                   
013278                                                                          
013279 77  ARREARS-SW                  PIC X       VALUE 'N'.                   
013280     88  SUPP-HAS-ARREARS                    VALUE 'J'.                   
013281                                                                          
013282 77  SUPPLIER-SW                 PIC X       VALUE 'N'.                   
013283     88  SUPPLIER-FOUND                      VALUE 'J'.                   
013284                                                                          
013285 77  W-HELG-SW                   PIC X       VALUE 'N'.                   
013286     88  HELG                                VALUE 'J'.                   
013287     88  EJ-HELG                             VALUE 'N'.                   
013288 77  W-DDGS-SW                   PIC X       VALUE 'N'.                   
013289     88  DDGS-OK                             VALUE 'J'.                   
013290     88  DDGS-NOT-OK                         VALUE 'N'.                   
013291                                                                          
013292 77  W-SDCLEV-SW                 PIC X       VALUE 'J'.                   
013293     88  SDCLEV-OK                           VALUE 'J'.                   
013294     88  SDCLEV-NT-OK                        VALUE 'N'.                   
013295 77  W-DC-INSERTED               PIC X       VALUE 'N'.                   
013296     88  DC-INSERTED                         VALUE 'J'.                   
013297     88  NT-DC-INSERTED                      VALUE 'N'.                   
013298 01  CURR-DC-IX                  PIC 9(3)    VALUE ZERO.                  
013299 01  IX-DCCLEAR-MAX              PIC 9(2)    VALUE 99.                    
013300 01  IDDC-IX                     PIC S9(3)   VALUE 0 COMP-3.              
013301 01  W-GMT-IDDC-CLEAR-GRP.                                                
013302     03 W-GMT-IDDC-CLEAR             PIC X(2) OCCURS 99 TIMES.            
013303                                                                          
013304 01  CURRENT-DATE                PIC 9(6).                                
013305 01  TODAYS-DATE                 PIC 9(8).                                
013306                                                                          
013307 01  CURRENT-TIME-NUM            PIC 9(8).                                
013308 01  FILLER REDEFINES CURRENT-TIME-NUM.                                   
013309     03  CURRENT-TIME            PIC 9(6).                                
013310     03  FILLER                  PIC 9(2).                                
013313*                                                                         
013314*    -COPY WWDC99                                                         
013315*      --- VALID IDDC CODES ALT3                                          
013316*01    -COPY WWDC99 -PRE ALT3-                                            
013317 01  GENERELLA-SUBPROGRAM.                                                
013318     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013319     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013320     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013321     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
013322     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013324     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
013325     03  W411KVAN                PIC X(8)    VALUE 'W411KVAN'.            
013326     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
013327     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
013328     03  W411STOR                PIC X(8)    VALUE 'W411STOR'.            
013329     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
013330                                                                          
013331*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
013332*    --- PARAMETRAR TILL SUBPROGRAM WORKDAY                               
013333*01 -COPY WDAGAREA                                                        
013334*01 -COPY WORKAREA                                                        
013335                                                                          
013336*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
013337*01 -COPY WDATAREA                                                        
013338                                                                          
013339*    --- PARAMETRAR TILL ABEND                                            
013340                                                                          
013341 01  ERROR-TEXT                  PIC X(80).                               
013342 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
013343 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33 COMP SYNC.         
013344                                                                          
013348 01   FILLER             PIC X(5)  VALUE 'AREG '.                         
013349*   -COPY W411AREG                                                        
013350                                                                          
013351 01   FILLER             PIC X(5)  VALUE 'KVAN '.                         
013352*   -COPY W411KVAN                                                        
013353                                                                          
013354 01   FILLER             PIC X(5)  VALUE 'SPAR '.                         
013355*   -COPY W411SPAR                                                        
013356                                                                          
013357 01   FILLER             PIC X(5)  VALUE 'RANS '.                         
013358*   -COPY W411RANS                                                        
013359                                                                          
013360 01   FILLER             PIC X(5)  VALUE 'STOR '.                         
013370*   -COPY W411STOR                                                        
013500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013600*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013900 01  NYCKLAR-TILL-DLI.                                                    
014000     03  W-WDF101KY-X.                                                    
014100         05  W-IDLEVNR-WDF1      PIC  X(5)   VALUE SPACE.                 
014200     03 W-WDF118KY-X.                                                     
014300         05 W-IDDISTR-WDF1       PIC S9(5)   VALUE ZERO COMP-3.           
014310         05 W-IDKUNDNR-WDF1      PIC S9(7)   VALUE ZERO COMP-3.           
014400         05 W-KDORDKL-WDF1       PIC S9(1)   VALUE ZERO COMP-3.           
014500     03  W-IDARTNR-X.                                                     
014600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014700     03  W-FLTEXT-X.                                                      
014800         05  W-FLTEXT            PIC  X(1)   VALUE 'N'.                   
014900     03  W-IDLEVNR-X.                                                     
015000         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
015100                                                                          
015200     03  W-IDSKYLT-X.                                                     
015300         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
015400                                                                          
015500     03  W-WDGX01KEY-X.                                                   
015600         05  W-IDHTYP            PIC  X(4)   VALUE '4521'.                
015700         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
015800                                                                          
015900     03  W-WDGX11KEY-X.                                                   
016000         05  W-KDORDBEK          PIC  9(2)   VALUE ZERO.                  
016100         05  W-IDSKYLT-WDGX      PIC  X(3)   VALUE SPACE.                 
016200                                                                          
016300     03  W-WDB301KY-X.                                                    
016400         05  W-IDDC-WDB3         PIC X(2)    VALUE SPACE.                 
016500         05  W-IDDISTR-WDB3      PIC S9(5)   VALUE ZERO COMP-3.           
016600         05  W-IDKUNDNR-WDB3     PIC S9(7)   VALUE ZERO COMP-3.           
016700                                                                          
016800     03  W-WDB301KY-DEF-X.                                                
016900         05  W-IDDC-WDB3-DEF     PIC X(2)    VALUE SPACE.                 
017000         05  W-IDDISTR-WDB3-DEF  PIC S9(5)   VALUE ZERO COMP-3.           
017100         05  W-IDKUNDNR-WDB3-DEF PIC S9(7) VALUE +9999999 COMP-3.         
017200                                                                          
017300                                                                          
017400*    --- IMS FUNKTIONSKODER                                               
017500*01  -COPY W0003                                                          
017600                                                                          
017610 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
017620 01  DLI-IO-WDF101.                                                       
017630*    03  -COPY WDF101                                                     
017700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF118'.                      
017800 01  DLI-IO-WLLEVA18.                                                     
017900*    03  -COPY WDF118                                                     
018000                                                                          
018100                                                                          
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
018300 01  DLI-IO-WDK601.                                                       
018400*    03  -COPY WDK601                                                     
018500                                                                          
018600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
018700 01  DLI-IO-WDK611.                                                       
018800*    03  -COPY WDK611                                                     
018810                                                                          
018820 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
018830 01  DLI-IO-WDK629.                                                       
018840*    03  -COPY WDK629                                                     
018900                                                                          
018901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
018902 01  DLI-IO-WDK701.                                                       
018903*    03  -COPY WDK701                                                     
018904                                                                          
018905 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
018906 01  DLI-IO-WDK711.                                                       
018907*    03  -COPY WDK711                                                     
018908                                                                          
019000 01  FILLER         PIC X(16) VALUE 'DLI-IO-XXKJ11'.                      
019100 01  DLI-IO-XXKJ11.                                                       
019200*    03  -COPY WDGX4522                                                   
019300                                                                          
019400 01  FILLER                      PIC X(16)   VALUE 'WDD311-AREA'.         
019500 01  DLI-IO-WDD311.                                                       
019600*    03  -COPY WDD311                                                     
019700                                                                          
019800 01  FILLER                      PIC X(16)   VALUE 'WDD701-AREA'.         
019900 01  DLI-IO-WDD701.                                                       
020000*    03  -COPY WDD701                                                     
020100                                                                          
020200 01  FILLER                      PIC X(16)   VALUE 'WDD702-AREA'.         
020300 01  DLI-IO-WDD702.                                                       
020400*    03  -COPY WDD702                                                     
020500                                                                          
020600 01  FILLER                      PIC X(16)   VALUE 'WDB301-AREA'.         
020700 01  DLI-IO-WDB301.                                                       
020800*    03  -COPY WDB301                                                     
020900                                                                          
038510 01  FILLER                      PIC X(16)   VALUE 'WDK901 AREA'.         
038520 01   DLI-IO-AREA-K901.                                                   
038530*     03  -COPY WDK901                                                    
038540                                                                          
038550 01  FILLER                      PIC X(16)   VALUE 'WDA501 AREA'.         
038560 01   DLI-IO-AREA-A501.                                                   
038570*     03  -COPY WDA501                                                    
038580                                                                          
038591 01  FILLER                      PIC X(16)   VALUE 'WDA601 AREA'.         
038592 01   DLI-IO-AREA-A601.                                                   
038593*     03  -COPY WDA601                                                    
038594                                                                          
038595 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
038596 01  DLI-IO-WDD901.                                                       
038597*    03  -COPY WDD901                                                     
038598                                                                          
038599 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
038600 01  DLI-IO-WDD902.                                                       
038700*    03  -COPY WDD902                                                     
038800                                                                          
038900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
039000 01  DLI-IO-WDD905.                                                       
039100*    03  -COPY WDD905 -PRE AVROP-                                         
039200                                                                          
039300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD924'.                      
039400 01  DLI-IO-WDD924.                                                       
039500*    03  -COPY WDD924                                                     
039600                                                                          
039700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
039800 01  DLI-IO-WDL601.                                                       
039900*    03  -COPY WDL601                                                     
040000                                                                          
040100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
040200 01  DLI-IO-WDL611.                                                       
040300*    03  -COPY WDL611                                                     
040301                                                                          
040302 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
040303 01  DLI-IO-WDB601.                                                       
040304*    03  -COPY WDB601                                                     
040305                                                                          
040306 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
040307 01  DLI-IO-WDB616.                                                       
040308*    03  -COPY WDB616                                                     
040309                                                                          
040310 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF201'.                      
040311 01  DLI-IO-WDF201.                                                       
040312*    03  -COPY WDF201                                                     
040313                                                                          
040314 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF211'.                      
040315 01  DLI-IO-WDF211.                                                       
040316*    03  -COPY WDF211                                                     
040317                                                                          
040318 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF2A1'.                      
040319 01   DLI-IO-WDF2A1.                                                      
040320*     03  -COPY WDF2A1                                                    
040321*                                                                         
040322 01  FILLER         PIC X(16)   VALUE 'WDB201-AREA'.                      
040323 01  DLI-IO-AREA-WDB201.                                                  
040324*    03  -COPY WDB201                                                     
040325*                                                                         
040326      EJECT                                                               
040327 01  FILLER         PIC X(16)   VALUE 'WDB101-AREA'.                      
040328 01  DLI-IO-AREA-WDB101.                                                  
040329*    03  -COPY WDB101                                                     
040330*                                                                         
040331*    --- STATUS-KOD FRÅN IMS                                              
040332 01  STATUS-WS                   PIC XX.                                  
040333     88  SEGMENT-FINNS                       VALUE '  '.                  
040334     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
040335     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
040336     88  BASEN-SLUT                          VALUE 'GB'.                  
040337     SKIP2                                                                
040338 01  GODK-STATUSKODER.                                                    
040339     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
040340                                                                          
040341 01  ALL-SSA.                                                             
040342    03 SSA1                      PIC X(160).                              
040343    03 SSA2                      PIC X(96).                               
040344                                                                          
040345                                                                          
040346 LINKAGE SECTION.                                                         
040347                                                                          
040348*   -COPY W911ETD                                                         
040349                                                                          
040350*01  -COPY W0008      -PRE WDF1-                                          
040351     05  FILLER                  PIC X.                                   
040352*01  -COPY W0008      -PRE WDK6-                                          
040353     05  FILLER                  PIC X.                                   
040354*01  -COPY W0008      -PRE WDK7-                                          
040355     05  FILLER                  PIC X.                                   
040356*01  -COPY W0008      -PRE XXKJ-                                          
040357     05  FILLER                  PIC X.                                   
040358*01  -COPY W0008      -PRE WDD7-                                          
040359     05  FILLER                  PIC X.                                   
040360*01  -COPY W0008      -PRE BENA-                                          
040361     05  FILLER                  PIC X.                                   
040362*01  -COPY W0008      -PRE WDB3-                                          
040363     05  FILLER                  PIC X.                                   
040364*01  -COPY W0008      -PRE WDK9-                                          
040365     05  FILLER                  PIC X.                                   
040366*01  -COPY W0008      -PRE WDA5-                                          
040367     05  FILLER                  PIC X.                                   
040368*01  -COPY W0008      -PRE WDA6J-                                         
040369     05  FILLER                  PIC X.                                   
040370*01  -COPY W0008      -PRE WDD9-                                          
040371     05  FILLER                  PIC X.                                   
040372                                                                          
040373*01  -COPY W0008      -PRE WDL6-                                          
040374     05  FILLER                  PIC X.                                   
040375*01  -COPY W0008      -PRE WDB6-                                          
040376     05  FILLER                  PIC X.                                   
040377*01  -COPY W0008      -PRE WDF2-                                          
040378     05  FILLER                  PIC X.                                   
040379*01  -COPY W0008      -PRE WDF2A-                                         
040380     05  FILLER                  PIC X.                                   
040381*01  -COPY W0008      -PRE WDB2-                                          
040382     05  FILLER                  PIC X.                                   
040383*01  -COPY W0008      -PRE WDB1-                                          
040384     05  FILLER                  PIC X.                                   
040386                                                                          
040392 01  KVAN-WDB2-PCB               PIC X.                                   
040393 01  KVAN-WDC1-PCB               PIC X.                                   
040394                                                                          
040395 01  AREG-WDK6-PCB               PIC X.                                   
040396 01  AREG-WDK7-PCB               PIC X.                                   
040397                                                                          
040398 01  SPAR-WDF8-PCB               PIC X.                                   
040399 01  SPAR-WDF8A-PCB              PIC X.                                   
040400 01  SPAR-WDK6-PCB               PIC X.                                   
040401                                                                          
040409 01  RANS-XXKM-PCB               PIC X.                                   
040410 01  RANS-ARTM-PCB               PIC X.                                   
040411 01  RANS-ARTS-PCB               PIC X.                                   
040412                                                                          
040428 PROCEDURE DIVISION  USING ETD-W911ETD WDF1-PCB                           
040429                     WDK6-PCB WDK7-PCB XXKJ-PCB WDD7-PCB BENA-PCB         
040430                     WDB3-PCB WDK9-PCB WDA5-PCB WDA6J-PCB                 
040431                     WDD9-PCB WDL6-PCB WDB6-PCB WDF2-PCB WDF2A-PCB        
040432                     WDB2-PCB WDB1-PCB                                    
040433                                                                          
040436                     KVAN-WDB2-PCB KVAN-WDC1-PCB                          
040437                                                                          
040438                     AREG-WDK6-PCB AREG-WDK7-PCB                          
040439                                                                          
040443                     SPAR-WDF8-PCB SPAR-WDF8A-PCB SPAR-WDK6-PCB           
040444                                                                          
040452                     RANS-XXKM-PCB RANS-ARTM-PCB  RANS-ARTS-PCB.          
040461                                                                          
040462     MOVE JA  TO ALL-SW                                                   
040463     MOVE NEJ TO SALDO-SW                                                 
040464     MOVE NEJ TO DIRLEV-SW                                                
040465     MOVE NEJ TO SUPPLIER-SW                                              
040466     MOVE NEJ TO NDC-STOCKS-SW                                            
040467                                                                          
040468*INITIALIZE VARIABLE FOR BATCH PROCESS                                    
040469     INITIALIZE W-IDDC-REF                                                
040470*                                                                         
040480                                                                          
040498     PERFORM A-INITIERA-UT-AREA                                           
040499     PERFORM B-GET-CUSTOMER                                               
040500                                                                          
040501     IF ALL-OK                                                            
040502        PERFORM C-GET-PART-INFO                                           
040503        MOVE 1 TO CURR-DC-IX                                              
040504        MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC                      
040505                                                                          
040506        IF ALL-OK OR NDC                                                  
040507           PERFORM D-GET-QUANTITY                                         
040508           PERFORM E-CALL-SPAR                                            
040509           IF ALL-OK OR SPAR-FLPUBCDC = YES                               
040510              IF AREG-REDIRLEV > 0                                        
040511                 PERFORM F-GET-DDGS-INFO                                  
040512              END-IF                                                      
040513              IF NOT DIRLEV                                               
040514                 MOVE 1 TO CURR-DC-IX                                     
040515                 MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC             
040516                 IF SDC OR LDC                                            
040517                    PERFORM G-GET-SDC-LDC                                 
040518                 ELSE                                                     
040519                    IF NDC                                                
040520                       PERFORM H-GET-NDC                                  
040521                    END-IF                                                
040522                 END-IF                                                   
040523                                                                          
040524                 IF ALL-OK AND SALDO-SAKNAS AND CDC                       
040525                    PERFORM I-CALL-RANS-MODULE                            
040526                    IF ALL-OK                                             
040527                       PERFORM J-GET-MAX-LIMIT                            
040528                    END-IF                                                
040529                    IF ALL-OK                                             
040530                       PERFORM K-GET-CDC                                  
040531                    END-IF                                                
040532                 END-IF                                                   
040533              END-IF                                                      
040534           END-IF                                                         
040535        END-IF                                                            
040536     END-IF                                                               
040537                                                                          
040538     IF ALL-OK                                                            
040539        IF SALDO-SAKNAS                                                   
040540           IF DIRLEV                                                      
040541*             IF ETD-KVAVBART = ZERO                                      
040542              IF ETD-IDDC NOT = '21' AND                                  
040543                 ETD-KDORDBEK NOT = '21'                                  
040544                 PERFORM L-GET-DIRLEV-DISP                                
040545              END-IF                                                      
040550              CONTINUE                                                    
040600           ELSE                                                           
098300              MOVE ETD-IDDC        TO WS-IDDC                             
098301              IF CDC                                                      
098302                PERFORM M-GET-SALDO-DISP                                  
098303              END-IF                                                      
098304           END-IF                                                         
098305        ELSE                                                              
098306           IF W-IDDC-REF = SPACES                                         
098307             PERFORM N-CALC-CUST-ETA                                      
098308           END-IF                                                         
098309        END-IF                                                            
098310     END-IF                                                               
098311                                                                          
098312     PERFORM O-MOVE-OUTPUT-AREA                                           
098313                                                                          
098315     GOBACK                                                               
098316     .                                                                    
098317                                                                          
098318                                                                          
098319 A-INITIERA-UT-AREA  SECTION.                                             
098320     MOVE 'A-INITIERA' TO CURRENT-SECTION                                 
098321                                                                          
098322     MOVE ZERO    TO ETD-KVAVBART                                         
098323     MOVE SPACE   TO ETD-IDDC                                             
098324     MOVE ZERO    TO ETD-TIDISPIN                                         
098325                     ETD-TIKLAR                                           
098326                     ETD-KDORDBEK                                         
098327                      WS-KDORDBEK-SUPERS                                  
098328                     ETD-TIREGDAT                                         
098329     MOVE SPACE   TO ETD-FLTPO1                                           
098330     MOVE ZERO    TO ETD-KVFRYSTI                                         
098331     MOVE SPACE   TO ETD-TEORDBEK                                         
098332                     ETD-TEORDBEK-ENG                                     
098333                     ETD-BEART                                            
098334                     ETD-BEART-ENG                                        
098335                     ETD-KDSORT                                           
098336     MOVE ZERO    TO ETD-IDARTNR-TILLK                                    
098337                                                                          
098338     MOVE SPACE   TO ETD-IDMFSMED                                         
098339                                                                          
098340     MOVE FUNCTION CURRENT-DATE(1:8) TO TODAYS-DATE                       
098341                                                                          
098342     ACCEPT CURRENT-DATE     FROM DATE                                    
098343     ACCEPT CURRENT-TIME-NUM FROM TIME                                    
098344                                                                          
098345     MOVE CURRENT-DATE       TO WS-CURRENT-DATE                           
098350                                                                          
098360     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
098370     MOVE WS-CURRENT-DATE    TO DAT-I-TIDATUM                             
098380                                                                          
098390     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
098391                     DAT-O-TIDATUM DAT-KDSVAR                             
098392                                                                          
098393     IF DAT-KDSVAR-OK                                                     
098394        MOVE DAT-TIAAVV-GRP(1:2) TO WS-CURR-AA2                           
098395        MOVE DAT-TIAAVV-GRP(3:2) TO WS-CURR-VV                            
098396     ELSE                                                                 
098397         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
098398         DELIMITED BY SIZE INTO ERROR-TEXT                                
098399         CALL ABEND                                                       
098400     END-IF                                                               
098401     .                                                                    
098402                                                                          
098403 B-GET-CUSTOMER SECTION.                                                  
098404     MOVE 'B-GET-CUSTOMER' TO CURRENT-SECTION                             
098405                                                                          
098406     MOVE JA                   TO W-IDDISTR-OK                            
098407                                  W-IDKUNDNR-OK                           
098408                                  W-KDKREDSP-OK                           
098409                                  W-IDVAT-OK                              
098410                                  W-IDPARTNR-OK                           
098420                                                                          
098430*VALIDATE DISTRICT/CUSTOMER                                               
098431     MOVE ETD-IDDISTR-IN      TO W-IDDISTR-WDB2                           
098432     MOVE ETD-IDKUNDNR-IN     TO W-IDKUNDNR-WDB2                          
098433                                                                          
098434     PERFORM IMS-GU-WDB201                                                
098435                                                                          
098436     IF SEGMENT-SAKNAS                                                    
098437        MOVE NEJ               TO W-IDDISTR-OK                            
098438                                  W-IDKUNDNR-OK                           
098439                                  ALL-SW                                  
098440     ELSE                                                                 
098441        MOVE CURRENT-DATE   TO TMP1-YYMMDD                                
098442        MOVE GMT-TISTADAT   TO TMP2-YYMMDD                                
098443        MOVE GMT-TISTODAT   TO TMP3-YYMMDD                                
098444        PERFORM WY2000Q1                                                  
098445        IF(TMP1-YYMMDD < TMP3-YYMMDD OR                                   
098446            TMP3-YYMMDD = +0)                                             
098447        AND                                                               
098448          ((TMP1-YYMMDD NOT < TMP2-YYMMDD) AND                            
098449            TMP2-YYMMDD > +0)                                             
098450           PERFORM BA-MOVE-GMT-FIELDS                                     
098451        ELSE                                                              
098452          MOVE NEJ             TO W-IDKUNDNR-OK                           
098453                                  ALL-SW                                  
098454        END-IF                                                            
098455     END-IF                                                               
098456                                                                          
098457*VALIDATE IDVAT/KDKREDSP/IDPARTNR                                         
098458     MOVE SPACE                TO W-KDKREDSP                              
098459     IF ALL-SW = JA                                                       
098460        IF GMT-IDPARTNR NOT = SPACE                                       
098461          MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                      
098462          MOVE GMT-IDFTG          TO W-WDB1-IDFTG                         
098463          PERFORM IMS-GU-WDB1-WDB101                                      
098464          IF SEGMENT-FINNS                                                
098465             MOVE BET-KDKREDSP    TO W-KDKREDSP                           
098466             MOVE BET-IDLANDX2    TO TEST-IDLANDX2                        
098467             IF LANDX2-EU-EJ-SE AND BET-IDVAT = SPACE                     
098468               MOVE NEJ           TO W-IDVAT-OK                           
098469                                     ALL-SW                               
098470             END-IF                                                       
098471          ELSE                                                            
098472             MOVE NEJ             TO ALL-SW                               
098473          END-IF                                                          
098474        ELSE                                                              
098475          MOVE NEJ                TO W-IDPARTNR-OK                        
098476        END-IF                                                            
098477     END-IF                                                               
098478                                                                          
098479     IF W-IDDISTR-OK   = NEJ OR                                           
098480        W-IDKUNDNR-OK  = NEJ OR                                           
098481        W-KDKREDSP     = 1   OR                                           
098482        W-IDVAT-OK     = NEJ OR                                           
098483        W-IDPARTNR-OK  = NEJ OR                                           
098484        ALL-SW         = NEJ                                              
098485                                                                          
098486        MOVE NEJ               TO ALL-SW                                  
098487        MOVE 'B10' TO ETD-IDMFSMED                                        
098488     ELSE                                                                 
098489        MOVE GMT-IDSKYLT       TO W-IDSKYLT                               
098490                                  W-IDSKYLT-WDGX                          
098491     END-IF                                                               
098492     .                                                                    
098493                                                                          
098494 BA-MOVE-GMT-FIELDS SECTION.                                              
098495     MOVE 'BA-MOVE-GMT-FIE'    TO CURRENT-SECTION                         
098496                                                                          
098497     INITIALIZE W-GMT-IDDC-CLEAR-GRP                                      
098498                                                                          
098499     MOVE +1                   TO IDDC-IX                                 
098500     PERFORM UNTIL IDDC-IX > IX-DCCLEAR-MAX                               
098501       IF ETD-KDORDKL-IN  = 0                                             
098502         MOVE GMT-IDDC-VOR(IDDC-IX)  TO W-GMT-IDDC-CLEAR(IDDC-IX)         
098503       END-IF                                                             
098504       IF ETD-KDORDKL-IN  = 1                                             
098505         MOVE GMT-IDDC-DAY(IDDC-IX)  TO W-GMT-IDDC-CLEAR(IDDC-IX)         
098506       END-IF                                                             
098507       IF ETD-KDORDKL-IN  > 1                                             
098508         MOVE GMT-IDDC-BULK(IDDC-IX) TO W-GMT-IDDC-CLEAR(IDDC-IX)         
098509       END-IF                                                             
098510       ADD +1                  TO IDDC-IX                                 
098511     END-PERFORM                                                          
098512     MOVE GMT-IDRFTAB          TO W-IDRFTAB                               
098513     IF GMT-RESLATT = ZERO                                                
098514        MOVE '0'               TO W-IDRFTAB (3:1)                         
098515     END-IF                                                               
098516     .                                                                    
098517                                                                          
098518                                                                          
098519 C-GET-PART-INFO SECTION.                                                 
098520     MOVE 'C-GET-PART-INFO' TO CURRENT-SECTION                            
098521                                                                          
098522     MOVE ETD-IDARTNR-IN    TO AREG-IDARTNR                               
098523     MOVE W-GMT-IDDC-CLEAR(1) TO AREG-IDDC                                
098524                                                                          
098525     CALL W411AREG USING AREG-W411AREG                                    
098526                         AREG-WDK6-PCB                                    
098527                         AREG-WDK7-PCB                                    
098528                                                                          
098529     IF AREG-KDORDBEK > 0 OR AREG-KDERS-UTG > 0                           
098530        IF AREG-KDORDBEK > 0                                              
098531           MOVE AREG-KDORDBEK TO ETD-KDORDBEK                             
098532        ELSE                                                              
098533           MOVE 41            TO ETD-KDORDBEK                             
098534        END-IF                                                            
098535        MOVE AREG-FLTPO1   TO ETD-FLTPO1                                  
098536        IF ETD-KDORDBEK = 41                                              
098537           MOVE ETD-KDORDBEK  TO WS-KDORDBEK-SUPERS                       
098538        ELSE                                                              
098539           MOVE NEJ           TO ALL-SW                                   
098540        END-IF                                                            
098541     ELSE                                                                 
098542        EVALUATE TRUE                                                     
098543          WHEN AREG-KDERS = 19 OR 29                                      
098544            MOVE 54 TO ETD-KDORDBEK                                       
098545                        WS-KDORDBEK-SUPERS                                
098546            MOVE '080' TO ETD-IDMFSMED                                    
098547*CC*        MOVE NEJ TO ALL-SW                                            
098548                                                                          
098549          WHEN AREG-KDERS = 52                                            
098550            MOVE 52 TO ETD-KDORDBEK                                       
098551            MOVE '080' TO ETD-IDMFSMED                                    
098552            MOVE NEJ TO ALL-SW                                            
098553                                                                          
098554          WHEN (AREG-KDERS > 10 AND < 14) OR                              
098555               (AREG-KDERS > 20 AND < 24) OR                              
098556               (AREG-KDERS = 17 OR 27)                                    
098557            MOVE 41 TO ETD-KDORDBEK                                       
098558                        WS-KDORDBEK-SUPERS                                
098559            MOVE '080' TO ETD-IDMFSMED                                    
098560            IF AREG-KDERS = 13 OR 23                                      
098561               MOVE NEJ TO ALL-SW                                         
098562*CC*        MOVE NEJ TO ALL-SW                                            
098563            END-IF                                                        
098564                                                                          
098565          WHEN (AREG-KDERS > 13 AND < 17) OR                              
098566               (AREG-KDERS > 23 AND < 27) OR                              
098567               (AREG-KDERS = 18 OR 28)                                    
098568            MOVE 61 TO ETD-KDORDBEK                                       
098569                        WS-KDORDBEK-SUPERS                                
098570            MOVE '080' TO ETD-IDMFSMED                                    
098571            IF AREG-KDERS = 16 OR 26                                      
098572               MOVE NEJ TO ALL-SW                                         
098573*CC*        MOVE NEJ TO ALL-SW                                            
098574            END-IF                                                        
098575                                                                          
098576        END-EVALUATE                                                      
098577        PERFORM CA-COMPLETE-PART-INFO                                     
098578     END-IF                                                               
098579     .                                                                    
098580                                                                          
098581 CA-COMPLETE-PART-INFO  SECTION.                                          
098582     MOVE 'CA-COMPLETE- ' TO CURRENT-SECTION                              
098583                                                                          
098584     MOVE ETD-IDARTNR-IN    TO W-IDARTNR                                  
098585     PERFORM IMS-GU-WDK601                                                
098586     IF SEGMENT-FINNS                                                     
098587        MOVE AREG-KDSORT     TO ETD-KDSORT                                
098588*       PERFORM IMS-GNP-WDK611                                            
098589*       IF SEGMENT-FINNS                                                  
098590*          MOVE CLAG-PRINK   TO SLDO-PRINK                                
098591*       END-IF                                                            
098592        PERFORM IMS-GU-BENA11-BSEQ                                        
098593        IF SEGMENT-FINNS                                                  
098594           MOVE TEXT-BEART    TO ETD-BEART                                
098595           IF W-IDSKYLT = 'GB'                                            
098596              MOVE TEXT-BEART TO ETD-BEART-ENG                            
098597           END-IF                                                         
098598        END-IF                                                            
098599        IF W-IDSKYLT NOT = 'GB'                                           
098600           MOVE 'GB'          TO W-IDSKYLT                                
098601           PERFORM IMS-GU-BENA11-BSEQ                                     
098602           IF SEGMENT-FINNS                                               
098603              MOVE TEXT-BEART TO ETD-BEART-ENG                            
098604           END-IF                                                         
098605        END-IF                                                            
098606     END-IF                                                               
098607     .                                                                    
098608                                                                          
098609                                                                          
098610 D-GET-QUANTITY SECTION.                                                  
098611     MOVE 'D-GET-QUANTIT' TO CURRENT-SECTION                              
098612                                                                          
098613     MOVE ZERO                 TO KVAN-KDKVBRYT-IN                        
098614     MOVE 'IMS '               TO KVAN-IDSYSTEM-IN                        
098615     MOVE ETD-KVBEART-IN      TO KVAN-KVBEART-IN                          
098616     MOVE AREG-KVQPACK-0       TO KVAN-KVQPACK-0-IN                       
098617     MOVE AREG-KVQPACK-1       TO KVAN-KVQPACK-1-IN                       
098618     MOVE AREG-KDPRODSL        TO KVAN-KDPRODSL-IN                        
098619     MOVE AREG-KDSORT          TO KVAN-KDSORT-IN                          
098620     MOVE AREG-IDFKNGRP        TO KVAN-IDFKNGRP-IN                        
098621     MOVE ETD-KDORDKL-IN       TO KVAN-KDORDKL-IN                         
098622     MOVE NEJ                  TO KVAN-FLEMBORD-IN                        
098623     MOVE NEJ                  TO KVAN-FLFORBI-IN                         
098624     MOVE NEJ                  TO KVAN-FLORDSPE-IN                        
098625     MOVE NEJ                  TO KVAN-FLOVRLEV-IN                        
098626     MOVE +0                   TO KVAN-IDKAMPRF-IN                        
098627     MOVE AREG-IDDC            TO KVAN-IDDC-IN                            
098628     MOVE ETD-IDDISTR-IN      TO KVAN-IDDISTR-IN                          
098629     MOVE ETD-IDKUNDNR-IN     TO KVAN-IDKUNDNR-IN                         
098630     MOVE SPACE                TO KVAN-BERADREF-IN                        
098631     MOVE AREG-IDARTNR         TO KVAN-IDARTNR-IN                         
098632                                                                          
098633     CALL W411KVAN USING KVAN-W411KVAN KVAN-WDB2-PCB KVAN-WDC1-PCB        
098634     .                                                                    
098635                                                                          
098636                                                                          
098637 E-CALL-SPAR SECTION.                                                     
098638     MOVE 'E-CALL-SPAR    '  TO CURRENT-SECTION                           
098639                                                                          
098640     MOVE SPACE                TO SPAR-BERADREF                           
098641     MOVE SPACE                TO SPAR-BEKUNDRF                           
098642     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
098643     MOVE NEJ                  TO SPAR-FLEMBORD                           
098644     MOVE NEJ                  TO SPAR-FLFORBI                            
098645     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
098646     MOVE NEJ                  TO SPAR-FLORDSPE                           
098647     MOVE NEJ                  TO SPAR-FLOVRLEV                           
098648     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
098649     MOVE SPACE                TO SPAR-FLRESTN                            
098650     MOVE AREG-IDARTNR         TO SPAR-IDARTNR                            
098651     MOVE AREG-FLIART          TO SPAR-FLIART                             
098652     MOVE SPACE                TO SPAR-FLMARKSP                           
098653     MOVE ETD-IDDISTR-IN       TO SPAR-IDDISTR                            
098654     MOVE ETD-IDKUNDNR-IN      TO SPAR-IDKUNDNR                           
098655     MOVE '0000000   '         TO SPAR-IDKUNDRF-RO                        
098656     MOVE AREG-IDDC            TO SPAR-IDDC                               
098657     MOVE 'VDI '               TO SPAR-IDSYSTEM                           
098658     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
098659     MOVE AREG-KDERS           TO SPAR-KDERS                              
098660     MOVE SPACE                TO SPAR-KDFAKTYP                           
098661     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
098662     MOVE +0                   TO SPAR-KDORDBEH                           
098663     MOVE ETD-KDORDKL-IN       TO SPAR-KDORDKL                            
098664     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
098665     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
098666     MOVE SPACE                TO SPAR-KDPRTYP                            
098667     MOVE +0                   TO SPAR-KDTPOTYP                           
098668     MOVE AREG-KDUART          TO SPAR-KDUART                             
098669     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
098670     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
098671     MOVE +0                   TO SPAR-TIRODAT                            
098672     MOVE +0                   TO SPAR-TITPO                              
098673     MOVE NEJ                  TO SPAR-FLSDCLEV                           
098674     MOVE ZERO                 TO SPAR-TIREPDAT                           
098675                                                                          
098676     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
098677                                       SPAR-WDF8A-PCB                     
098678                                       SPAR-WDK6-PCB                      
098679     IF SPAR-KDORDBEK > ZERO                                              
098680        MOVE SPAR-KDORDBEK  TO ETD-KDORDBEK                               
098681        IF SPAR-KDORDBEK = 67                                             
098682           MOVE JA          TO ALL-SW                                     
098683        ELSE                                                              
098684           MOVE NEJ         TO ALL-SW                                     
098685        END-IF                                                            
098686     END-IF                                                               
098687     .                                                                    
098688 F-GET-DDGS-INFO SECTION.                                                 
098689     MOVE 'F-GET-DDGS-IN'        TO CURRENT-SECTION                       
098690     MOVE LOW-VALUE              TO W-WDF2A1KY-MIN-X                      
098691     MOVE HIGH-VALUE             TO W-WDF2A1KY-MAX-X                      
098692     MOVE TODAYS-DATE            TO W-DASTADAT                            
098693     MOVE ETD-IDARTNR-IN         TO W-IDARTNR-MIN                         
098694                                    W-IDARTNR-MAX                         
098695     MOVE ETD-IDDISTR-IN         TO W-IDDISTR                             
098696                                                                          
098697     MOVE +0                     TO W-IDKUNDNR-FOM                        
098698     MOVE +9999999               TO W-IDKUNDNR-TOM                        
098699     SET DIRLEV                  TO TRUE                                  
098700                                                                          
098701     PERFORM IMS-GU-WDF2A                                                 
098703     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
098704                   BASEN-SLUT     OR                                      
098705                   SUPPLIER-FOUND OR                                      
098706                   SALDO-FINNS                                            
098707       MOVE SEQA-IDLEVNR         TO W-IDLEVNR-WDF2                        
098708       MOVE SEQA-IDDIRGRP        TO W-IDDIRGRP-WDF2                       
098709       PERFORM IMS-GU-WDF201                                              
098711       IF SEGMENT-FINNS                                                   
098712         IF TODAYS-DATE >= LEV-DASTADAT                                   
098713            MOVE LEV-IDLEVNR     OF LEV-WDF201                            
098714                                 TO WS-IDLEVNR                            
098715            MOVE ETD-IDKUNDNR-IN TO W-IDKUNDNR-FOM                        
098716                                    W-IDKUNDNR-TOM                        
098717            PERFORM IMS-GU-WDF2-DIST-CUS                                  
098718            IF SEGMENT-FINNS                                              
098719                SET SUPPLIER-FOUND   TO TRUE                              
098720                PERFORM S08-FILL-DDGS-TABLE                               
098721                PERFORM FA-CHK-STOCKBAL-AND-DATE                          
098722            ELSE                                                          
098723                PERFORM FB-FIND-WDF2-DIST-CUS                             
098724                IF SUPPLIER-FOUND                                         
098725                   PERFORM FA-CHK-STOCKBAL-AND-DATE                       
098726                END-IF                                                    
098727            END-IF                                                        
098728         ELSE                                                             
098729            MOVE +0              TO W-IDKUNDNR-FOM                        
098730            MOVE +9999999        TO W-IDKUNDNR-TOM                        
098731            PERFORM IMS-GN-WDF2A                                          
098732         END-IF                                                           
098733       END-IF                                                             
098734       MOVE +0                   TO W-IDKUNDNR-FOM                        
098735       MOVE +9999999             TO W-IDKUNDNR-TOM                        
098736       PERFORM IMS-GN-WDF2A                                               
098737     END-PERFORM                                                          
098738     IF NOT SUPPLIER-FOUND                                                
098739         MOVE NEJ                TO SALDO-SW                              
098741         MOVE '21'               TO ETD-KDORDBEK                          
098743         MOVE '080'              TO ETD-IDMFSMED                          
098744     END-IF                                                               
098745     .                                                                    
098746 FA-CHK-STOCKBAL-AND-DATE SECTION.                                        
098747     MOVE 'FA-CHK-STOCKB'        TO CURRENT-SECTION                       
098748                                                                          
098749     COMPUTE DIR-INDX = ETD-KDORDKL-IN + 1                                
098750     SET DDGS-IX                 TO DIR-INDX                              
098751     MOVE DDGS-IDDC(DDGS-IX)     TO ALT3-WS-IDDC                          
098752     SET NT-DC-INSERTED          TO TRUE                                  
098753                                                                          
098754     PERFORM FAC-FIND-DDGS-IDDC                                           
098755                                                                          
098756     IF  DDGS-IDDC(DDGS-IX) > '  '                                        
098757     AND (ALT3-SDC OR ALT3-LDC)                                           
098758     AND DDGS-IDDC-FND                                                    
098766        PERFORM FAA-GET-SDCLEV-BALANCE                                    
098767        MOVE W-IDDC              TO ETD-IDDC                              
098768        IF SDCLEV-OK                                                      
098769            MOVE W-DISP          TO  ETD-KVAVBART                         
098770            MOVE JA              TO SALDO-SW                              
098771            MOVE '010'           TO ETD-IDMFSMED                          
098772        ELSE                                                              
098773* IF NO BALANCE FROM DC21, DC= 21 AND ETD = BLANK & QTY UNAVAILABL        
098774            IF DC-INSERTED                                                
098775               MOVE W-DISP       TO ETD-KVAVBART                          
098776               MOVE NEJ          TO SALDO-SW                              
098777               MOVE '080'        TO ETD-IDMFSMED                          
098778               MOVE '95'         TO ETD-KDORDBEK                          
098780            ELSE                                                          
098781               PERFORM FAB-CHK-SUPPLIER                                   
098782            END-IF                                                        
098783        END-IF                                                            
098784     ELSE                                                                 
098790        PERFORM FAB-CHK-SUPPLIER                                          
098800     END-IF                                                               
098801     .                                                                    
098802                                                                          
098803 FAA-GET-SDCLEV-BALANCE SECTION.                                          
098804     MOVE 'FAA-GET-SDCLE'        TO CURRENT-SECTION                       
098805                                                                          
098806     MOVE DDGS-IDDC(DDGS-IX)     TO W-IDDC                                
098807     PERFORM IMS-GU-WDK711                                                
098808     IF SEGMENT-FINNS                                                     
098809        SET DC-INSERTED          TO TRUE                                  
098810        IF SLAG-KVAKS-SDC < 0                                             
098811           MOVE 0                TO W-KVAKS-SDC                           
098812        ELSE                                                              
098813           MOVE SLAG-KVAKS-SDC   TO W-KVAKS-SDC                           
098814        END-IF                                                            
098815        IF SLAG-KVOKS-DAG < 0                                             
098816           MOVE 0                TO W-KVOKS-DAG                           
098817        ELSE                                                              
098818           MOVE SLAG-KVOKS-DAG   TO W-KVOKS-DAG                           
098819        END-IF                                                            
098820        IF SLAG-KVOKS-BULK < 0                                            
098821           MOVE 0                TO W-KVOKS-BULK                          
098822        ELSE                                                              
098823           MOVE SLAG-KVOKS-BULK  TO W-KVOKS-BULK                          
098824        END-IF                                                            
098825                                                                          
098826        COMPUTE W-DISP = SLAG-KVLS                                        
098827                       + W-KVAKS-SDC                                      
098828                       - W-KVOKS-DAG                                      
098829                       - W-KVOKS-BULK                                     
098830        IF W-DISP > 0                                                     
098831           COMPUTE W-DISP = W-DISP                                        
098832                          - SLAG-KVUTRS                                   
098833                          - SLAG-KVSPARR-KVAL                             
098834        END-IF                                                            
098835        IF W-DISP < 0                                                     
098836           MOVE 0                TO W-DISP                                
098837        END-IF                                                            
098838        IF KVAN-KVBEART-Q-UT > W-DISP OR                                  
098839           SLAG-KDLEVSP > 0                                               
098840           MOVE NEJ              TO W-SDCLEV-SW                           
098841        ELSE                                                              
098842          IF DDGS-MIN-QTY(DDGS-IX)  NOT = +0                              
098843            IF KVAN-KVBEART-Q-UT >= DDGS-MIN-QTY(DDGS-IX)                 
098844              MOVE JA            TO W-SDCLEV-SW                           
098845            ELSE                                                          
098846              MOVE NEJ           TO W-SDCLEV-SW                           
098847            END-IF                                                        
098848          ELSE                                                            
098849            MOVE NEJ             TO W-SDCLEV-SW                           
098850          END-IF                                                          
098851        END-IF                                                            
098852     ELSE                                                                 
098853       SET NT-DC-INSERTED        TO TRUE                                  
098854       MOVE NEJ                  TO W-SDCLEV-SW                           
098855     END-IF                                                               
098856     .                                                                    
098857                                                                          
098858 FAB-CHK-SUPPLIER SECTION.                                                
098859     MOVE 'FAB-CHK-SUPPLI'       TO CURRENT-SECTION                       
098860                                                                          
098863     IF DDGS-MIN-QTY(DDGS-IX) NOT = +0                                    
098864        IF  KVAN-KVBEART-Q-UT >= DDGS-MIN-QTY(DDGS-IX)                    
098865           MOVE JA               TO SALDO-SW                              
098867           MOVE SEQA-KVLS-DLEV   TO ETD-KVAVBART                          
098868           PERFORM FABA-DDGS-PROCESSING                                   
098869           IF KVAN-KVBEART-Q-UT <= SEQA-KVLS-DLEV                         
098870              MOVE '010'         TO ETD-IDMFSMED                          
098871           ELSE                                                           
098872              IF ETD-KVAVBART < 0                                         
098873                 MOVE ZEROES     TO ETD-KVAVBART                          
098874              END-IF                                                      
098875              MOVE ZEROES        TO ETD-TIKLAR                            
098876              MOVE NEJ           TO SALDO-SW                              
098878              MOVE '080'         TO ETD-IDMFSMED                          
098880              MOVE '95'          TO ETD-KDORDBEK                          
098882           END-IF                                                         
098883        ELSE                                                              
098884*WHEN USER QTY<MIN DIRECT DELIVERY QTY,CODE 21                            
098885           MOVE NEJ              TO SALDO-SW                              
098886           MOVE SEQA-KVLS-DLEV   TO ETD-KVAVBART                          
098887           MOVE '080'            TO ETD-IDMFSMED                          
098889           MOVE '21'             TO ETD-KDORDBEK                          
098891        END-IF                                                            
098892     ELSE                                                                 
098893*WHEN MIN DIRECT DELIVERY QTY = 0,CODE 21                                 
098894        MOVE NEJ                 TO SALDO-SW                              
098896        MOVE '21'                TO ETD-KDORDBEK                          
098898     END-IF                                                               
098899     .                                                                    
098900 FABA-DDGS-PROCESSING SECTION.                                            
098901     MOVE 'FABA-DDGS-PROC'       TO CURRENT-SECTION                       
098902                                                                          
098903     MOVE WS-IDLEVNR             TO W-IDLEVNR-WDF1                        
098904     PERFORM IMS-GU-WDF101                                                
098905     IF SEGMENT-FINNS                                                     
098906       IF SEGMENT-FINNS                                                   
098907         MOVE ETD-KDORDKL-IN     TO W-KDORDKL-WDF1                        
098908         MOVE W-IDDISTR          TO W-IDDISTR-WDF1                        
098909         MOVE ETD-IDKUNDNR-IN    TO W-IDKUNDNR-WDF1                       
098910         PERFORM IMS-GU-WDF118                                            
098911         IF SEGMENT-SAKNAS                                                
098912           MOVE 999999           TO W-IDKUNDNR-WDF1                       
098913           PERFORM IMS-GU-WDF118                                          
098914         END-IF                                                           
098915         IF SEGMENT-SAKNAS                                                
098916           MOVE 9999             TO W-IDDISTR-WDF1                        
098917           MOVE 999999           TO W-IDKUNDNR-WDF1                       
098918           PERFORM IMS-GU-WDF118                                          
098919         END-IF                                                           
098920         IF SEGMENT-FINNS                                                 
098921            MOVE DSTY-IDDC       TO ETD-IDDC                              
098922            PERFORM FABAA-DDGS-SHIPPING                                   
098923         END-IF                                                           
098924       END-IF                                                             
098925     ELSE                                                                 
098926        MOVE NEJ                 TO SALDO-SW                              
098928        MOVE '21'                TO ETD-KDORDBEK                          
098930     END-IF                                                               
098931     .                                                                    
098932 FABAA-DDGS-SHIPPING SECTION.                                             
098933     MOVE 'FABA-DDGS-SHI'        TO CURRENT-SECTION                       
098934                                                                          
098935     MOVE NEJ                    TO W-HELG-SW                             
098936     MOVE '11'                   TO WORK-IDDC                             
098937     MOVE 1                      TO WORK-KVWORKD                          
098938     MOVE CURRENT-DATE           TO WORK-TIAAMMDD-FOM                     
098939     MOVE 002                    TO WORK-KDCALL                           
098940     CALL WORKDAY  USING            WORK-KDCALL                           
098941                                    WORK-DATE-AREA                        
098942                                    WORK-KDSVAR                           
098943     END-CALL                                                             
098944     IF WORK-KDSVAR-FEL                                                   
098945       MOVE ' FEL FRÅN WORKDAY 1' TO ERROR-TEXT                           
098946       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
098947     END-IF                                                               
098948     IF WORK-TIAAMMDD-FOM NOT = WORK-TIAAMMDD-TOM                         
098949        MOVE JA                   TO W-HELG-SW                            
098950     END-IF                                                               
098951                                                                          
098952     MOVE '11'                    TO WORK-IDDC                            
098953     COMPUTE CURRENT-TIME-NUM = CURRENT-TIME-NUM / 10000                  
098954     END-COMPUTE                                                          
098955     MOVE DSTY-KVDAGAR-LEV        TO WORK-KVWORKD                         
098956     IF CURRENT-TIME-NUM > DSTY-TIMINUT-CUT                               
098957     AND NOT HELG                                                         
098958       ADD 1                      TO WORK-KVWORKD                         
098959     END-IF                                                               
098960     MOVE CURRENT-DATE            TO WORK-TIAAMMDD-FOM                    
098961     MOVE 002                     TO WORK-KDCALL                          
098962     CALL WORKDAY  USING             WORK-KDCALL                          
098963                                     WORK-DATE-AREA                       
098964                                     WORK-KDSVAR                          
098965     END-CALL                                                             
098966     IF WORK-KDSVAR-FEL                                                   
098967       MOVE ' FEL FRÅN WORKDAY 2' TO ERROR-TEXT                           
098968       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
098969     END-IF                                                               
098970     MOVE WORK-TIAAMMDD-TOM       TO W-DDGS-RFS                           
098971     .                                                                    
098972                                                                          
098973 FAC-FIND-DDGS-IDDC    SECTION.                                           
098974     MOVE 'PAC-FIND-DDG-'         TO CURRENT-SECTION                      
098975                                                                          
098976     MOVE 1                       TO IDDC-IX                              
098977     MOVE NEJ                     TO DDGS-IDDC-SW                         
098978                                                                          
098979     PERFORM UNTIL IDDC-IX > IX-DCCLEAR-MAX OR                            
098980                   W-GMT-IDDC-CLEAR(IDDC-IX) = SPACE                      
098981       IF DDGS-IDDC(DDGS-IX) = W-GMT-IDDC-CLEAR(IDDC-IX)                  
098982          MOVE JA                 TO DDGS-IDDC-SW                         
098983       END-IF                                                             
098984       ADD +1                     TO IDDC-IX                              
098985     END-PERFORM                                                          
098986      .                                                                   
098987 FB-FIND-WDF2-DIST-CUS SECTION.                                           
098988     MOVE 'FB-FIND-WDF2-'        TO CURRENT-SECTION                       
098989                                                                          
098990     MOVE NEJ                    TO SUPPLIER-SW                           
098991                                                                          
098992     PERFORM IMS-GU-WDF201                                                
098993     IF SEGMENT-FINNS                                                     
098994        PERFORM IMS-GNP-WDF211-DIST-CUS                                   
098995        PERFORM UNTIL SEGMENT-SAKNAS OR                                   
098996                      SUPPLIER-FOUND                                      
098997           IF (ETD-IDDISTR-IN  >= DIR-IDDISTR-FOM  AND                    
098998               ETD-IDDISTR-IN  <= DIR-IDDISTR-TOM) AND                    
098999              (ETD-IDKUNDNR-IN >= DIR-IDKUNDNR-FOM AND                    
099000               ETD-IDKUNDNR-IN <= DIR-IDKUNDNR-TOM)                       
099001               SET SUPPLIER-FOUND   TO TRUE                               
099002               PERFORM S08-FILL-DDGS-TABLE                                
099003           END-IF                                                         
099004           PERFORM IMS-GNP-WDF211-DIST-CUS                                
099005        END-PERFORM                                                       
099006     ELSE                                                                 
099007        MOVE NEJ                    TO SUPPLIER-SW                        
099008     END-IF                                                               
099009     .                                                                    
099010                                                                          
099011 G-GET-SDC-LDC SECTION.                                                   
099012     MOVE 'G-GET-SDC-LDC'   TO CURRENT-SECTION                            
099013                                                                          
099014     MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC                         
099015                                         ETD-IDDC                         
099016                                         W-IDDC                           
099017     PERFORM IMS-GU-WDK711                                                
099018     PERFORM UNTIL SALDO-FINNS    OR                                      
099019                   CURR-DC-IX > IX-DCCLEAR-MAX OR                         
099020                   (W-GMT-IDDC-CLEAR(CURR-DC-IX) = SPACE) OR              
099100                    NOT (SDC OR LDC)                                      
099200*                  OR SEGMENT-SAKNAS                                      
099300*WE DONT GIVE 'GE' CHECK AS WDK7 MAY NOT HAVE REC FOR SELECTED DC         
099400        IF SEGMENT-FINNS                                                  
099500           PERFORM S01-QTY-CALC-XDC                                       
099600                                                                          
099700           IF (KVAN-KVBEART-Q-UT <= W-AVAIL-QTY)                          
099800              MOVE 'J'                TO SALDO-SW                         
099900           ELSE                                                           
100000              ADD 1                   TO CURR-DC-IX                       
100100              MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC                
100200                                                  W-IDDC                  
100300           END-IF                                                         
100400           PERFORM IMS-GU-WDK711                                          
100500        ELSE                                                              
100600           ADD 1                      TO CURR-DC-IX                       
100700           MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC                   
100800                                               W-IDDC                     
100900           PERFORM IMS-GU-WDK711                                          
101000        END-IF                                                            
101100     END-PERFORM                                                          
101200     IF SALDO-FINNS AND                                                   
101300       (KVAN-KVBEART-Q-UT <= W-AVAIL-QTY)                                 
101400*RC REMOVE THE BELOW LINE.WE SHOULD NT SHOW THE AVAIL QTY.                
101500         MOVE W-AVAIL-QTY             TO ETD-KVAVBART                     
101600*        MOVE KVAN-KVBEART-Q-UT       TO ETD-KVAVBART                     
101700*RC REMOVE THE '*' IN ABOVE LINE B4 INSTALL                               
101800         MOVE JA                      TO SALDO-SW                         
101900         MOVE WS-IDDC                 TO ETD-IDDC                         
102000         MOVE '010'                   TO ETD-IDMFSMED                     
102100         IF CURR-DC-IX > 1                                                
102200             MOVE '15'                TO ETD-KDORDBEK                     
102300         END-IF                                                           
102400     ELSE                                                                 
102500*RC REMOVE THE BELOW LINE.WE WILL NT SHOW THE QTY WHEN IT IS LESS.        
102600         MOVE W-AVAIL-QTY             TO ETD-KVAVBART                     
102700*        MOVE ZEROES                  TO ETD-KVAVBART                     
102800*RC REMOVE THE '*' IN ABOVE LINE B4 INSTALL                               
102900         MOVE NEJ                     TO SALDO-SW                         
103000         MOVE SPACES                  TO ETD-IDDC                         
103100         MOVE '080'                   TO ETD-IDMFSMED                     
103200     END-IF                                                               
103201     .                                                                    
103202                                                                          
103222 H-GET-NDC SECTION.                                                       
103223     MOVE 'H-GET-NDC '  TO CURRENT-SECTION                                
103224                                                                          
103225     MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC                         
103226                                         ETD-IDDC                         
103227                                         W-IDDC                           
103228     MOVE JA                           TO NDC-STOCKS-SW                   
103229     PERFORM IMS-GU-WDK711                                                
103230     MOVE NEJ                 TO STOCK-SW                                 
103231     INITIALIZE DC-TABEL                                                  
103232     SET DC-IX                TO +1                                       
103233     PERFORM UNTIL SALDO-FINNS    OR                                      
103240                   CURR-DC-IX > IX-DCCLEAR-MAX OR                         
103250                   (W-GMT-IDDC-CLEAR(CURR-DC-IX) = SPACE) OR              
103260                   (NOT NDC)                                              
103270*                  OR SEGMENT-SAKNAS                                      
103280*WE DONT GIVE 'GE' CHECK AS WDK7 MAY NOT HAVE REC FOR SELECTED DC         
103290        IF SEGMENT-FINNS                                                  
103300           MOVE SLAG-IDDC-REF               TO W-IDDC-REF                 
103310           MOVE SLAG-FLFLYG                 TO W-FLFLYG                   
103400           PERFORM S01-QTY-CALC-XDC                                       
103500                                                                          
103600           MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO DC-NUMBER(DC-IX)          
103610           MOVE W-AVAIL-QTY                 TO DC-AVAIL-QTY(DC-IX)        
103620           MOVE W-IDDC-REF                  TO DC-IDDC-REF(DC-IX)         
103630           MOVE W-FLFLYG                    TO DC-FLFLYG(DC-IX)           
103800           IF KVAN-KVBEART-Q-UT <= W-AVAIL-QTY                            
103900              MOVE 'J'                      TO SALDO-SW                   
104000           ELSE                                                           
104100              ADD 1                         TO CURR-DC-IX                 
104200              MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC                
104300                                                  W-IDDC                  
104400           END-IF                                                         
104500           PERFORM IMS-GU-WDK711                                          
104600        ELSE                                                              
104700           ADD 1                               TO CURR-DC-IX              
104800           MOVE W-GMT-IDDC-CLEAR(CURR-DC-IX) TO WS-IDDC                   
104900                                                  W-IDDC                  
105000           PERFORM IMS-GU-WDK711                                          
105100        END-IF                                                            
105200        SET DC-IX                     UP BY +1                            
105300     END-PERFORM                                                          
105400     IF SALDO-FINNS AND                                                   
105500       (KVAN-KVBEART-Q-UT <= W-AVAIL-QTY)                                 
105600*RC REMOVE THE BELOW LINE.WE SHOULD NT SHOW THE AVAIL QTY.                
105700         MOVE W-AVAIL-QTY             TO ETD-KVAVBART                     
105800*        MOVE KVAN-KVBEART-Q-UT       TO ETD-KVAVBART                     
105900*RC REMOVE THE '*' IN ABOVE LINE B4 INSTALL                               
106000         MOVE JA                      TO SALDO-SW                         
106100         MOVE WS-IDDC                 TO ETD-IDDC                         
106200         MOVE SPACES                  TO WS-IDDC                          
106300         MOVE '010'                   TO ETD-IDMFSMED                     
106400         IF CURR-DC-IX > 1                                                
106500             MOVE '15'                TO ETD-KDORDBEK                     
106600         END-IF                                                           
106700     ELSE                                                                 
106800*RC REMOVE THE BELOW LINE.WE WILL NT SHOW THE QTY WHEN IT IS LESS.        
106900         MOVE DC-AVAIL-QTY(1)         TO ETD-KVAVBART                     
107000*        MOVE ZEROES                  TO ETD-KVAVBART                     
107100*RC REMOVE THE '*' IN ABOVE LINE B4 INSTALL                               
107200         MOVE NEJ                     TO SALDO-SW                         
107300         MOVE DC-NUMBER(1)            TO ETD-IDDC                         
107400         MOVE SPACES                  TO WS-IDDC                          
107500         MOVE  99                     TO ETD-KDORDBEK                     
107600         MOVE '080'                   TO ETD-IDMFSMED                     
107701     END-IF                                                               
107702     IF SALDO-SAKNAS                                                      
107703         PERFORM HA-STOCKS-TO-NDC                                         
107704     END-IF                                                               
107705     .                                                                    
107706                                                                          
107707******************************************************************        
107708*FOR REFILL PARTS,IF NO STOCKS ARE ON THE WAY FROM CDC/NDC TO NDC,        
107709*BUT STOCKS AVAILABLE IN CDC/NDC,CALC LEAD TIME TO NDC4408) &             
107710*CALCULATE THE ETD.                                                       
107720*EX:IF PARTS REFILL FROM DC71 TO 41, AND NO STOCKS ON THE WAY,AND         
107730*STOCKS AVAILABLE AT 71.CALCULATE THE LEAD TIME BY AIR/BOAT FROM          
107740*71 TO 41 AND DISPLAY ETD.(SAME LOGIC FOR CDC TO NDC).                    
107750******************************************************************        
107760 HA-STOCKS-TO-NDC SECTION.                                                
107770     MOVE 'HA-STOCKS-TO'  TO CURRENT-SECTION                              
107771                                                                          
107772     SET DC-IX                   TO +1                                    
107773     MOVE DC-NUMBER(DC-IX)       TO W-IDDC                                
107774     COMPUTE W-QTY-NEEDED = KVAN-KVBEART-Q-UT -                           
107775                            DC-AVAIL-QTY(DC-IX)                           
107776     MOVE DC-IDDC-REF(DC-IX)     TO W-IDDC-REF                            
107777                                                                          
107778     IF W-IDDC-REF = SPACES                                               
107779        PERFORM S02-STOCK-ON-WAY                                          
107780     ELSE                                                                 
107781        PERFORM S04-REFILL-STOCKS                                         
107782        IF W-DC-AVAIL-DATE = 0                                            
107783           MOVE W-IDDC-REF       TO WS-IDDC                               
107784                                    W-IDDC                                
107785           IF CDC                                                         
107786              PERFORM S06-CDC-QTY-CALC                                    
107787              IF W-CDC-QTY <= 0                                           
107788                 MOVE NEJ        TO SALDO-SW                              
107790              ELSE                                                        
107791                 IF KVAN-KVBEART-Q-UT <= W-CDC-QTY                        
107792                    MOVE JA      TO SALDO-SW                              
107794                 ELSE                                                     
107795                    MOVE NEJ     TO SALDO-SW                              
107797                 END-IF                                                   
107798              END-IF                                                      
107799           ELSE                                                           
107800              PERFORM S07-NDC-QTY-CALC                                    
107801           END-IF                                                         
107802           MOVE W-IDDC           TO W-IDDC-REF                            
107803           MOVE DC-FLFLYG(DC-IX) TO W-FLFLYG                              
107804           IF SALDO-FINNS                                                 
107805              PERFORM S05-CALC-LEAD-TIME                                  
107806           END-IF                                                         
107807           MOVE SPACES           TO WS-IDDC                               
107808        END-IF                                                            
107809     END-IF                                                               
107810                                                                          
107812     MOVE W-DC-AVAIL-DATE        TO ETD-TIDISPIN                          
107813                                                                          
107814     IF ETD-TIDISPIN > 0                                                  
107815        PERFORM S03-CALC-CUST-ETA                                         
107816     END-IF                                                               
107817     .                                                                    
107818                                                                          
107819 I-CALL-RANS-MODULE SECTION.                                              
107820     MOVE 'I-CALL-RANS-'  TO CURRENT-SECTION                              
107821                                                                          
107822     MOVE SPACE                TO RANS-BERADREF                           
107823     MOVE NEJ                  TO RANS-FLEMBORD                           
107824     MOVE NEJ                  TO RANS-FLFORBI                            
107825     MOVE NEJ                  TO RANS-FLORDSPE                           
107826     MOVE NEJ                  TO RANS-FLOVRLEV                           
107827     MOVE +0                   TO RANS-IDKAMPRF                           
107828     MOVE AREG-IDARTNR         TO RANS-IDARTNR                            
107829     MOVE SPACE                TO RANS-IDLEVNR                            
107830     MOVE W-IDRFTAB            TO RANS-IDRFTAB                            
107831     MOVE +0                   TO RANS-TIRODAT                            
107832     MOVE ETD-KDORDKL-IN       TO RANS-KDORDKL                            
107833     MOVE +1                   TO RANS-KDORDBEH                           
107834     MOVE KVAN-KVBEART-Q-UT    TO RANS-KVBEART-Q                          
107835     MOVE +0                   TO RANS-KDTPOTYP                           
107836     MOVE AREG-KDERS           TO RANS-KDERS                              
107837     MOVE AREG-KVLS            TO RANS-KVLS                               
107838     MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                          
107839     MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                           
107840     MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                           
107841     MOVE AREG-KVRESS          TO RANS-KVRESS                             
107842     MOVE AREG-KVSPANT         TO RANS-KVSPANT                            
107843     MOVE AREG-KVUTRS          TO RANS-KVUTRS                             
107844     MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                           
107845                                                                          
107846     IF AREG-KDPRODSL = 71 OR 72 OR 73 OR 74                              
107847        MOVE 1                 TO RANS-RERF-RAD-UT                        
107848        MOVE ZERO              TO RANS-SUTPO-PB-UT                        
107849                                  RANS-SUTPO-EJPB-UT                      
107850                                  RANS-RERF-ART-UT                        
107851     ELSE                                                                 
107852        CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB                   
107853                            RANS-ARTM-PCB RANS-ARTS-PCB                   
107854                                                                          
107855     END-IF                                                               
107856     .                                                                    
107857                                                                          
107858                                                                          
107859 J-GET-MAX-LIMIT SECTION.                                                 
107860     MOVE 'J-GET-MAX-LIM'  TO CURRENT-SECTION                             
107861                                                                          
107862     MOVE 'VDI '               TO STOR-IDSYSTEM                           
107863     MOVE SPACE                TO STOR-IDLEVNR                            
107864     MOVE '0000000   '         TO STOR-IDKUNDRF-RO                        
107865     MOVE SPACE                TO STOR-KDPROTYP                           
107866     MOVE SPACE                TO STOR-BERADREF                           
107867     MOVE NEJ                  TO STOR-FLFORBI                            
107868     MOVE NEJ                  TO STOR-FLORDSPE                           
107869     MOVE NEJ                  TO STOR-FLOVRLEV                           
107870     MOVE ETD-KDORDKL-IN       TO STOR-KDORDKL                            
107871     MOVE AREG-KDERS           TO STOR-KDERS                              
107872     MOVE AREG-KDVVKL          TO STOR-KDVVKL                             
107873     MOVE KVAN-KVBEART-Q-UT    TO STOR-KVBEART-Q                          
107874     MOVE AREG-KVPB-SEP        TO STOR-KVPB-SEP                           
107875     MOVE AREG-KVSLUTKP        TO STOR-KVSLUTKP                           
107876     MOVE RANS-RERF-ART-UT     TO STOR-RERF-ART                           
107877     MOVE +0                   TO STOR-IDKAMPRF                           
107878     MOVE ETD-IDDISTR-IN       TO STOR-IDDISTR                            
107879     MOVE AREG-KDPRODSL        TO STOR-KDPRODSL                           
107880                                                                          
107881     CALL W411STOR USING STOR-W411STOR                                    
107882                                                                          
107883     IF STOR-KDORDBEK > +0                                                
107884        MOVE '080'             TO ETD-IDMFSMED                            
107885        MOVE NEJ               TO ALL-SW                                  
107886        MOVE STOR-KDORDBEK     TO ETD-KDORDBEK                            
107887     END-IF                                                               
107888     .                                                                    
107889                                                                          
107890 K-GET-CDC SECTION.                                                       
107891     MOVE 'K-GET-CDC     ' TO CURRENT-SECTION                             
107892                                                                          
107893     PERFORM S06-CDC-QTY-CALC                                             
107894                                                                          
107895     IF W-CDC-QTY        >= KVAN-KVBEART-Q-UT                             
107900        MOVE '010'                TO ETD-IDMFSMED                         
108000        MOVE W-CDC-QTY            TO ETD-KVAVBART                         
108100        MOVE WS-IDDC              TO ETD-IDDC                             
108200        MOVE JA                   TO SALDO-SW                             
108300        IF CURR-DC-IX > 1                                                 
108400           MOVE '15'              TO ETD-KDORDBEK                         
108500        END-IF                                                            
108600     ELSE                                                                 
108700        COMPUTE W-QTY-NEEDED = KVAN-KVBEART-Q-UT -                        
108800                                   W-CDC-QTY                              
108900        MOVE W-CDC-QTY            TO ETD-KVAVBART                         
109000*RC REMOVE THE ABOVE LINE.THIS IS FOR QTY CHECKING FOR TEST!!!            
109100        MOVE NEJ                  TO SALDO-SW                             
109200        MOVE 99                   TO ETD-KDORDBEK                         
109300        MOVE '080'                TO ETD-IDMFSMED                         
109400        MOVE WS-IDDC              TO ETD-IDDC                             
109500     END-IF                                                               
109600     .                                                                    
109800                                                                          
113504 L-GET-DIRLEV-DISP SECTION.                                               
113505     MOVE 'L-GET-DIRLEV-D' TO CURRENT-SECTION                             
113506                                                                          
113507     MOVE WS-IDLEVNR      TO W-IDLEVNR-WDF1                               
113508     MOVE ETD-IDDISTR-IN  TO W-IDDISTR-WDF1                               
113509     MOVE ETD-IDKUNDNR-IN TO W-IDKUNDNR-WDF1                              
113510     MOVE ETD-KDORDKL-IN  TO W-KDORDKL-WDF1                               
113515     PERFORM IMS-GU-WDF118                                                
113516     IF SEGMENT-SAKNAS                                                    
113517        MOVE 999999 TO W-IDKUNDNR-WDF1                                    
113518        PERFORM IMS-GU-WDF118                                             
113519     END-IF                                                               
113520     IF SEGMENT-SAKNAS                                                    
113521        MOVE 9999 TO W-IDDISTR-WDF1                                       
113522        PERFORM IMS-GU-WDF118                                             
113523     END-IF                                                               
113524     IF SEGMENT-FINNS                                                     
113525        MOVE DSTY-KVDAGAR-DIFF  TO WS-KVDAGAR-DIFF                        
113526        IF ETD-TIDISPIN = ZERO                                            
113527           MOVE WC-CDC-SE       TO WORK-IDDC                              
113528           MOVE +002            TO WORK-KDCALL                            
113529           COMPUTE WORK-KVWORKD = DSTY-KVDAGAR-LEV + 1                    
113532           MOVE CURRENT-DATE    TO WORK-TIAAMMDD-FOM                      
113533           CALL WORKDAY      USING WORK-KDCALL                            
113534                                      WORK-DATE-AREA                      
113535                                      WORK-KDSVAR                         
113536           IF WORK-KDSVAR-FEL                                             
113537              MOVE 'L-KOLLA-DIRLEV-DISP, DATUM SAKNAS I WORKDAY'          
113538                                     TO ERROR-TEXT                        
113539              CALL ABEND USING RKOD-ABEND-MED-DUMP                        
113540           ELSE                                                           
113541              MOVE WORK-TIAAMMDD-TOM TO ETD-TIDISPIN                      
113543           END-IF                                                         
113544        END-IF                                                            
113545     ELSE                                                                 
113546        MOVE ZERO               TO WS-KVDAGAR-DIFF                        
113547     END-IF                                                               
113548     IF ETD-TIDISPIN > ZERO                                               
113549        MOVE 002                     TO WORK-KDCALL                       
113550        MOVE WC-CDC-SE               TO WORK-IDDC                         
113551        MOVE ETD-TIDISPIN           TO WORK-TIAAMMDD-FOM                  
113552        COMPUTE WORK-KVWORKD = WS-KVDAGAR-DIFF + 1                        
113553        CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA                     
113554                           WORK-KDSVAR                                    
113555        IF WORK-KDSVAR-OK                                                 
113556           MOVE WORK-TIAAMMDD-TOM TO ETD-TIKLAR                           
113564        END-IF                                                            
113565     END-IF                                                               
113570     .                                                                    
113587******************************************************************        
113588*FOR REFILL PARTS,IF NO STOCKS ARE ON THE WAY FROM NDC TO CDC,AND         
113589*STOCK IS AVAILABLE AT NDC,CHECK LEAD TIME FROM NDC TO CDC(4408).         
113590*CALCULATE THE ETD                                                        
113591******************************************************************        
113592 M-GET-SALDO-DISP  SECTION.                                               
113593     MOVE 'M-GET-SALDO ' TO CURRENT-SECTION                               
113594                                                                          
113595                                                                          
113596     IF W-IDDC-REF = SPACES                                               
113597        PERFORM S02-STOCK-ON-WAY                                          
113598     ELSE                                                                 
113599        PERFORM S04-REFILL-STOCKS                                         
113600        IF W-DC-AVAIL-DATE = 0                                            
113601           MOVE W-IDDC-REF     TO WS-IDDC                                 
113602                                  W-IDDC                                  
113603           PERFORM S07-NDC-QTY-CALC                                       
113604           MOVE W-IDDC                  TO W-IDDC-REF                     
113605           IF SALDO-FINNS                                                 
113606              PERFORM S05-CALC-LEAD-TIME                                  
113607           END-IF                                                         
113608        END-IF                                                            
113609     END-IF                                                               
113610     MOVE W-DC-AVAIL-DATE            TO ETD-TIDISPIN                      
113612                                                                          
113613     PERFORM S03-CALC-CUST-ETA                                            
113614     .                                                                    
113615     EJECT                                                                
113616                                                                          
113617 N-CALC-CUST-ETA    SECTION.                                              
113618     MOVE 'N-CALC-ETA   ' TO CURRENT-SECTION                              
113619                                                                          
113620                                                                          
113621     MOVE ZERO               TO WS-KVDAGAR-DIFF                           
113622     IF DIRLEV                                                            
113623        MOVE WS-IDLEVNR      TO W-IDLEVNR-WDF1                            
113624        MOVE ETD-IDDISTR-IN  TO W-IDDISTR-WDF1                            
113625        MOVE ETD-IDKUNDNR-IN TO W-IDKUNDNR-WDF1                           
113626        MOVE ETD-KDORDKL-IN  TO W-KDORDKL-WDF1                            
113627        PERFORM IMS-GU-WDF118                                             
113628        IF SEGMENT-SAKNAS                                                 
113629           MOVE 999999 TO W-IDKUNDNR-WDF1                                 
113630           PERFORM IMS-GU-WDF118                                          
113631        END-IF                                                            
113632        IF SEGMENT-SAKNAS                                                 
113633           MOVE 9999 TO W-IDDISTR-WDF1                                    
113634           PERFORM IMS-GU-WDF118                                          
113635        END-IF                                                            
113636        IF SEGMENT-FINNS                                                  
113637           COMPUTE WORK-KVWORKD = DSTY-KVDAGAR-DIFF + 1                   
113638        END-IF                                                            
113639     ELSE                                                                 
113640        MOVE ETD-IDDC             TO W-IDDC-WDB3                          
113641                                      W-IDDC-WDB3-DEF                     
113642        MOVE ETD-IDDISTR-IN       TO W-IDDISTR-WDB3                       
113643                                      W-IDDISTR-WDB3-DEF                  
113644        MOVE ETD-IDKUNDNR-IN      TO W-IDKUNDNR-WDB3                      
113645        PERFORM IMS-GU-WDB301                                             
113646        IF SEGMENT-FINNS                                                  
113647           COMPUTE WORK-KVWORKD = DC-KVDAGAR-TRP-DAY + 1                  
113648        END-IF                                                            
113649     END-IF                                                               
113650     IF DIRLEV                                                            
113651        MOVE WC-CDC-SE             TO WORK-IDDC                           
113652     ELSE                                                                 
113653        MOVE ETD-IDDC              TO WORK-IDDC                           
113655     END-IF                                                               
113656     MOVE +002                  TO WORK-KDCALL                            
113657     MOVE CURRENT-DATE          TO WORK-TIAAMMDD-FOM                      
113658     CALL WORKDAY            USING WORK-KDCALL                            
113659                                WORK-DATE-AREA                            
113660                                WORK-KDSVAR                               
113661     IF WORK-KDSVAR-FEL                                                   
113662        MOVE 'N-CALC-CUST-ETA, DATUM SAKNAS I WORKDAY'                    
113663                               TO ERROR-TEXT                              
113664        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
113665     ELSE                                                                 
113666        MOVE WORK-TIAAMMDD-TOM TO ETD-TIKLAR                              
113668     END-IF                                                               
113669     .                                                                    
113670                                                                          
113671                                                                          
113672 O-MOVE-OUTPUT-AREA SECTION.                                              
113673     MOVE 'O-MOVE-OUTPUT' TO CURRENT-SECTION                              
113674                                                                          
113675     IF WS-KDORDBEK-SUPERS > ZERO                                         
113676        IF SALDO-FINNS                                                    
113677           IF ETD-KDORDBEK = 15                                           
113678              CONTINUE                                                    
113679           ELSE                                                           
113680              MOVE ZERO             TO ETD-KDORDBEK                       
113681           END-IF                                                         
113682        ELSE                                                              
113683           MOVE SPACE               TO ETD-IDDC                           
113684           MOVE WS-KDORDBEK-SUPERS  TO ETD-KDORDBEK                       
113685        END-IF                                                            
113686     END-IF                                                               
113687     IF (ETD-KDORDBEK = ZERO OR 15) AND                                   
113688        KVAN-KDORDBEK-UT > ZERO                                           
113689*       VI KAN LEVERERA MEN ANTALET HAR KVANTANPASSATS                    
113690        MOVE KVAN-KDORDBEK-UT  TO ETD-KDORDBEK                            
113691*       MOVE KVAN-KVBEART-Q-UT TO ETD-KVAVBART                            
113692     END-IF                                                               
113693*BELOW IF SHOWS ETD < CURRENT DATE TOO                                    
113694*    IF ETD-TIDISPIN > ZERO AND ETD-TIDISPIN < CURRENT-DATE               
113695*       MOVE ZERO     TO ETD-TIDISPIN                                     
113696*                        ETD-TIKLAR                                       
113697*    END-IF                                                               
113698                                                                          
113715     IF SPAR-KDORDBEK = 67                                                
113716        IF NDC-STOCKS                                                     
113717            IF ETD-KDORDBEK = 99 OR 15 OR ZERO                            
113718                MOVE SPAR-KDORDBEK TO ETD-KDORDBEK                        
113719            END-IF                                                        
113720        ELSE                                                              
113721          MOVE SPAR-KDORDBEK TO ETD-KDORDBEK                              
113722        END-IF                                                            
113723     END-IF                                                               
113724                                                                          
113725     IF ETD-KDORDBEK > ZERO                                               
113726        MOVE ETD-KDORDBEK TO W-KDORDBEK                                   
113727        PERFORM IMS-GU-XXKJ11                                             
113728        IF SEGMENT-FINNS                                                  
113729           MOVE 4522-TEORDBEK                                             
113730                           TO ETD-TEORDBEK                                
113731        END-IF                                                            
113732        IF W-IDSKYLT-WDGX = 'GB'                                          
113733           MOVE 4522-TEORDBEK                                             
113734                           TO ETD-TEORDBEK-ENG                            
113735        ELSE                                                              
113736           MOVE 'GB'       TO W-IDSKYLT-WDGX                              
113737           PERFORM IMS-GU-XXKJ11                                          
113738           IF SEGMENT-FINNS                                               
113739              MOVE 4522-TEORDBEK                                          
113740                           TO ETD-TEORDBEK-ENG                            
113741           END-IF                                                         
113742        END-IF                                                            
113743        IF ETD-KDORDBEK = 41 OR 61                                        
113744           PERFORM IMS-GU-WDD701                                          
113745           IF SEGMENT-FINNS                                               
113746              PERFORM IMS-GNP-WDD702                                      
113747              IF SEGMENT-FINNS                                            
113748                 MOVE IDARTNR-TILLK                                       
113749                              TO ETD-IDARTNR-TILLK                        
113750                 PERFORM IMS-GNP-WDD702                                   
113751                 IF SEGMENT-FINNS                                         
113752                    MOVE ZERO TO ETD-IDARTNR-TILLK                        
113753                 END-IF                                                   
113754              END-IF                                                      
113755           END-IF                                                         
113756        END-IF                                                            
113757     END-IF                                                               
113758     .                                                                    
113759                                                                          
113760 S01-QTY-CALC-XDC SECTION.                                                
113761     MOVE 'S01-QTY-CALC-XDC' TO CURRENT-SECTION                           
113762                                                                          
113763     COMPUTE W-AVAIL-QTY = SLAG-KVLS        -                             
113764                           SLAG-KVOKS-DAG   -                             
113770                           SLAG-KVOKS-BULK  -                             
113800                           SLAG-KVROS-DAG   -                             
113900                           SLAG-KVROS-BULK  -                             
114000                           SLAG-KVRESS      -                             
114100                           SLAG-KVUTRS      -                             
114101                           SLAG-KVSPARR-KVAL                              
114102     .                                                                    
114103                                                                          
114104*****************************************************************         
114105* FOR PURCHASE PARTS:                                                     
114106* IF CDC CANT PROVIDE USER REQUESTED QTY,CHECK IF ANY STOCK IS            
114107* ON THE WAY FROM SUPPLIER TO CDC.GIVE THE DATE AS DC AVAIL DATE.         
114108*****************************************************************         
114109 S02-STOCK-ON-WAY SECTION.                                                
114110     MOVE 'S02-STOCK-ON-WAY '        TO CURRENT-SECTION                   
114120                                                                          
114130     MOVE W-IDARTNR                  TO W-IDARTNR-WDD9                    
114140     MOVE W-IDDC                     TO W-IDDC-WDD9                       
114150                                                                          
114160     PERFORM IMS-GU-WDD901                                                
114170     IF SEGMENT-FINNS                                                     
114180        PERFORM IMS-GNP-WDD924                                            
114190        IF SEGMENT-FINNS                                                  
114200           MOVE NEJ                  TO STOCK-SW                          
114300           MOVE 0                    TO W-STOCK-ON-WAY                    
114400           PERFORM UNTIL SEGMENT-SAKNAS OR                                
114500                         STOCK-ON-WAY                                     
114600             COMPUTE W-STOCK-ON-WAY = LEV-KVAVIS-BSKURS +                 
114700                                         W-STOCK-ON-WAY                   
114800             MOVE LEV-TILEVBSK-DISP  TO W-STOCK-ON-WAY-DATE               
114900             IF W-QTY-NEEDED <= W-STOCK-ON-WAY                            
115000                MOVE JA              TO STOCK-SW                          
115100                MOVE LEV-TILEVBSK-DISP TO W-DC-AVAIL-DATE                 
115200             END-IF                                                       
115300             PERFORM IMS-GNP-WDD924                                       
115400           END-PERFORM                                                    
115500           IF W-STOCK-ON-WAY > 0 AND (NOT STOCK-ON-WAY)                   
115600              COMPUTE W-QTY-NEEDED = W-QTY-NEEDED -                       
115700                                         W-STOCK-ON-WAY                   
115800              PERFORM S02A-CHK-DEL-SCHEDULE                               
115900           END-IF                                                         
116000        ELSE                                                              
116100           MOVE  ZEROES              TO W-DC-AVAIL-DATE                   
116200           PERFORM S02A-CHK-DEL-SCHEDULE                                  
116300        END-IF                                                            
116400     ELSE                                                                 
116500        MOVE  ZEROES                 TO W-DC-AVAIL-DATE                   
116501     END-IF                                                               
116502     .                                                                    
116503*****************************************************************         
116504* FOR PURCHASE PARTS:                                                     
116505* IF NO STOCKS ARE ON THE WAY FROM SUPPLIER TO CDC, CHECK THE             
116506* VALID AND APPROVED DELIVERY SCHEDULE FROM THE SUPPLIER.                 
116507* IF SUPPLIER HAS ARREARS IN THE PAST,THEN NEGLECT THE AVAILABLE          
116508* DATE.                                                                   
116509* IF THERE ARE SOME STOCKS ON THE WAY AND SOME ARE ON DELIVERY            
116510* SCHEDULE,TAKE THE DC AVAILABLE DATE AS THE DATE WHICH IS                
116520* FURTHER IN FUTURE.                                                      
116521*****************************************************************         
116522 S02A-CHK-DEL-SCHEDULE SECTION.                                           
116523     MOVE 'S02A-CHK-DEL-SCHE'         TO CURRENT-SECTION                  
116524                                                                          
116525     PERFORM IMS-GU-WDD901                                                
116526     IF SEGMENT-FINNS                                                     
116527        PERFORM IMS-GNP-WDD905                                            
116528        IF SEGMENT-FINNS                                                  
116529           MOVE NEJ                  TO ARREARS-SW                        
116530                                        STOCK-SW                          
116540           MOVE ZEROES               TO W-STOCK-DEL-SCHDL                 
116550           PERFORM UNTIL SEGMENT-SAKNAS   OR                              
116560                         SUPP-HAS-ARREARS OR                              
116570                         STOCK-ON-WAY                                     
116580              IF AVROP-KDAVROP = 2                                        
116590                 IF AVROP-DAAVROP-AVS < WS-CURRRENT-AAAAVV                
116600                   MOVE JA           TO ARREARS-SW                        
116700                   MOVE ZEROES       TO W-DC-AVAIL-DATE                   
116800                 ELSE                                                     
116900                   COMPUTE W-STOCK-DEL-SCHDL = AVROP-KVAVROP +            
117000                                         W-STOCK-DEL-SCHDL                
117100                   IF W-QTY-NEEDED <= W-STOCK-DEL-SCHDL                   
117200                      MOVE JA        TO STOCK-SW                          
117300                      IF W-STOCK-ON-WAY > 0 AND                           
117400                         W-STOCK-ON-WAY-DATE > AVROP-TIAVRDAT-INL         
117500                         MOVE W-STOCK-ON-WAY-DATE                         
117600                                     TO W-DC-AVAIL-DATE                   
117700                      ELSE                                                
117800                         MOVE AVROP-TIAVRDAT-INL                          
117900                                     TO W-DC-AVAIL-DATE                   
118000                      END-IF                                              
118100                   END-IF                                                 
118200                 END-IF                                                   
118300              END-IF                                                      
118400              PERFORM IMS-GNP-WDD905                                      
118500           END-PERFORM                                                    
118600        ELSE                                                              
118700           MOVE  ZEROES              TO W-DC-AVAIL-DATE                   
118800        END-IF                                                            
118900     ELSE                                                                 
119000        MOVE  ZEROES                 TO W-DC-AVAIL-DATE                   
119001     END-IF                                                               
119002     .                                                                    
119003                                                                          
119004*****************************************************************         
119005*ETA IS CALCULATED BASED ON DC AVAILABLE DATE(DC ETD)           *         
119006*****************************************************************         
119007 S03-CALC-CUST-ETA SECTION.                                               
119008     MOVE 'S03-CALC-CUST-ET '        TO CURRENT-SECTION                   
119009                                                                          
119010     MOVE ETD-IDDC                   TO W-IDDC-WDB3                       
119020                                        W-IDDC-WDB3-DEF                   
119030     MOVE ETD-IDDISTR-IN             TO W-IDDISTR-WDB3                    
119040                                        W-IDDISTR-WDB3-DEF                
119050     MOVE ETD-IDKUNDNR-IN            TO W-IDKUNDNR-WDB3                   
119060     PERFORM IMS-GU-WDB301                                                
119070     IF SEGMENT-FINNS                                                     
119080        MOVE 002                     TO WORK-KDCALL                       
119090        MOVE ETD-IDDC                TO WORK-IDDC                         
119100        MOVE ETD-TIDISPIN            TO WORK-TIAAMMDD-FOM                 
119200        MOVE DC-KVDAGAR-TRP-DAY      TO WORK-KVWORKD                      
119300        CALL WORKDAY          USING WORK-KDCALL WORK-DATE-AREA            
119400                                    WORK-KDSVAR                           
119500        IF WORK-KDSVAR-OK                                                 
119600           MOVE WORK-TIAAMMDD-TOM    TO ETD-TIKLAR                        
119700        END-IF                                                            
119701     END-IF                                                               
119702     .                                                                    
119703                                                                          
122904                                                                          
122905*****************************************************************         
122906* FOR REFILL PARTS:(CLAG-IDDC-REF > SPACES)                               
122907* CHECK AND COMPARE THE USER REQUESTED QTY WITH INVOICED QTY IN           
122908* WDL6 DB AND FOR ETD, TAKE THE FIELD TIBERANK (2353 FUNCTIONALITY        
122909*****************************************************************         
122910 S04-REFILL-STOCKS SECTION.                                               
122911     MOVE 'S04-REFILL-ST'           TO CURRENT-SECTION                    
122912                                                                          
122913     PERFORM IMS-GU-WDL601                                                
122914     IF SEGMENT-FINNS                                                     
122915         PERFORM IMS-GNP-WDL611                                           
122916         IF SEGMENT-FINNS                                                 
122917           SET INL-IX               TO +1                                 
122918           PERFORM UNTIL SEGMENT-SAKNAS                                   
122919*             MOVE INL-IDDC          TO WS-IDDC                           
122920*             IF CDC AND                                                  
122921              IF INL-IDDC = ETD-IDDC AND                                  
122922                (INL-IDPTYP = '310' OR 'R31' OR 'R30')                    
122923                  MOVE INL-KVAVIS   TO INL-QTY(INL-IX)                    
122924                  MOVE INL-TIBERANK TO INL-DC-DATE(INL-IX)                
122925                  SET INL-IX        UP BY +1                              
122926              END-IF                                                      
122927              PERFORM IMS-GNP-WDL611                                      
122928           END-PERFORM                                                    
122929           IF INL-IX > 1                                                  
122930              PERFORM S04A-GET-ETD-FOR-REFILL                             
122931           END-IF                                                         
122932         ELSE                                                             
122933           MOVE  ZEROES             TO W-DC-AVAIL-DATE                    
122934         END-IF                                                           
122935     ELSE                                                                 
122936         MOVE  ZEROES               TO W-DC-AVAIL-DATE                    
122937     END-IF                                                               
122939     .                                                                    
122940                                                                          
122941 S04A-GET-ETD-FOR-REFILL SECTION.                                         
122942     MOVE 'S04A-GET-ETD-'           TO CURRENT-SECTION                    
122943                                                                          
122944     SORT INL-LINES DESCENDING INL-DC-DATE                                
122945                                                                          
122946     SET INL-IX                     DOWN BY +1                            
122947     MOVE NEJ                       TO STOCK-SW                           
122948     MOVE 0                         TO W-STOCK-ON-WAY                     
122949                                       W-STOCK-ON-WAY-DATE                
122950                                                                          
122951     PERFORM UNTIL STOCK-ON-WAY OR INL-IX < 1                             
122952       COMPUTE W-STOCK-ON-WAY = W-STOCK-ON-WAY +                          
122953                                INL-QTY(INL-IX)                           
122954       IF W-QTY-NEEDED <= W-STOCK-ON-WAY                                  
122955          MOVE JA                   TO STOCK-SW                           
122956          MOVE INL-DC-DATE(INL-IX)  TO W-DC-AVAIL-DATE                    
122957       END-IF                                                             
122958       SET INL-IX                   DOWN BY +1                            
122959     END-PERFORM                                                          
122960                                                                          
122961     IF (NOT STOCK-ON-WAY)                                                
122962*        OR SALDO-FINNS                                                   
122963        MOVE  ZEROES                TO W-DC-AVAIL-DATE                    
122964     END-IF                                                               
122965     .                                                                    
122966*****************************************************************         
122967* FOR REFILL PARTS:(CLAG-IDDC-REF > SPACES)                               
122968* CALCULATE THE LEAD TIMES (4408)                                         
122969*****************************************************************         
122970 S05-CALC-LEAD-TIME SECTION.                                              
122971     MOVE 'S05-CALC-LEAD'           TO CURRENT-SECTION                    
122972                                                                          
122973     MOVE ETD-IDDC                  TO W-IDDC-B6                          
122974     MOVE W-IDDC-REF                TO W-IDDC-B616                        
122975     PERFORM IMS-GU-WDB616                                                
122976                                                                          
122977     IF W-FLFLYG = 'Y' OR 'J'                                             
122978        PERFORM S05A-ETD-AIR-TRANSPORT                                    
122979     ELSE                                                                 
122980        PERFORM S05B-ETD-BOAT-TRANSPORT                                   
122981     END-IF                                                               
122982     .                                                                    
122983                                                                          
122984 S05A-ETD-AIR-TRANSPORT SECTION.                                          
122985     MOVE 'S05A-ETD-AIR-'           TO CURRENT-SECTION                    
122986                                                                          
122987     MOVE W-IDDC-REF  TO WS-IDDC                                          
122988     IF NDC OR CDC-SE                                                     
122989        MOVE 2                      TO WORK-KDCALL                        
122990        MOVE W-IDDC-REF             TO WORK-IDDC                          
122991        MOVE WS-CURRENT-DATE        TO WORK-TIAAMMDD-FOM                  
122992        MOVE REF-KVDLTID-AIRPAC     TO WORK-KVWORKD                       
122993        ADD +1                      TO WORK-KVWORKD                       
122994                                                                          
122995        CALL WORKDAY USING WORK-KDCALL                                    
122996                  WORK-DATE-AREA WORK-KDSVAR                              
122997                                                                          
122998        IF WORK-KDSVAR-OK                                                 
122999            MOVE WORK-TIAAMMDD-TOM  TO DAG-TIAAMMDD-FOM                   
123000            MOVE 20                 TO DAG-TISEKEL-FOM                    
123001        END-IF                                                            
123002     ELSE                                                                 
123003         MOVE WS-CURRENT-DATE       TO DAG-TIAAMMDD-FOM                   
123004         MOVE 20                    TO DAG-TISEKEL-FOM                    
123005     END-IF                                                               
123006                                                                          
123007*    TRANSPORTTID FLYG (KALENDERTID)                                      
123008     MOVE 2                         TO DAG-KDCALL                         
123009     MOVE REF-KVDLTID-AIRTRP        TO DAG-KVKALDAG                       
123010     ADD +1                         TO DAG-KVKALDAG                       
123011                                                                          
123012     CALL WDAGKONV USING DAG-KDCALL                                       
123013                         DAG-DATUM-AREA DAG-KDSVAR                        
123014                                                                          
123015     IF DAG-KDSVAR = SPACE                                                
123016        MOVE DAG-TIAAMMDD-TOM       TO WORK-TIAAMMDD-FOM                  
123017                                                                          
123018*    INLÄGGNINGSTID FLYG (ARBETSTID)                                      
123019        MOVE 2                      TO WORK-KDCALL                        
123020        MOVE ETD-IDDC               TO WORK-IDDC                          
123021        MOVE REF-KVDLTID-AIRINS     TO WORK-KVWORKD                       
123022        ADD +1                      TO WORK-KVWORKD                       
123023                                                                          
123024        CALL WORKDAY USING WORK-KDCALL                                    
123025                           WORK-DATE-AREA WORK-KDSVAR                     
123026                                                                          
123027        IF WORK-KDSVAR-OK                                                 
123028           MOVE WORK-TIAAMMDD-TOM   TO W-DC-AVAIL-DATE                    
123029        END-IF                                                            
123030     END-IF                                                               
123031     MOVE SPACES                    TO WS-IDDC                            
123032     .                                                                    
123033                                                                          
123034 S05B-ETD-BOAT-TRANSPORT SECTION.                                         
123035     MOVE 'S05B-ETD-BOAT'           TO CURRENT-SECTION                    
123036*    PACKTID BÅT  (ARBETSTID)                                             
123037     MOVE W-IDDC-REF  TO WS-IDDC                                          
123038                                                                          
123039     MOVE 2                         TO WORK-KDCALL                        
123040     MOVE W-IDDC-REF                TO WORK-IDDC                          
123041     MOVE WS-CURRENT-DATE           TO WORK-TIAAMMDD-FOM                  
123042     MOVE REF-KVDLTID-BOATPAC       TO WORK-KVWORKD                       
123043     ADD +2                         TO WORK-KVWORKD                       
123044                                                                          
123045     CALL WORKDAY USING WORK-KDCALL                                       
123046                        WORK-DATE-AREA WORK-KDSVAR                        
123047     IF WORK-KDSVAR-OK                                                    
123048        MOVE WORK-TIAAMMDD-TOM      TO DAG-TIAAMMDD-FOM                   
123049        MOVE 20                     TO DAG-TISEKEL-FOM                    
123050*                                                                         
123051*    TRANSPORTTID BÅT  (KALENDERTID)                                      
123052        MOVE 2                      TO DAG-KDCALL                         
123053        MOVE REF-KVDLTID-BOATTRP    TO DAG-KVKALDAG                       
123054        ADD +1                      TO DAG-KVKALDAG                       
123055*                                                                         
123056                                                                          
123057        CALL WDAGKONV USING DAG-KDCALL                                    
123058                            DAG-DATUM-AREA DAG-KDSVAR                     
123059                                                                          
123060        IF DAG-KDSVAR = SPACE                                             
123061           MOVE DAG-TIAAMMDD-TOM    TO WORK-TIAAMMDD-FOM                  
123062                                                                          
123063*    HAMN TILL GRIND     (ARBETSTID)                                      
123064*    INLÄGGNINGSTID BÅT  (ARBETSTID)                                      
123065           MOVE 2                   TO WORK-KDCALL                        
123066           MOVE ETD-IDDC            TO WORK-IDDC                          
123067           COMPUTE WORK-KVWORKD =                                         
123068                   REF-KVDLTID-BOAT2DC + REF-KVDLTID-BOATINS              
123069           ADD +1                   TO WORK-KVWORKD                       
123070                                                                          
123071           CALL WORKDAY USING WORK-KDCALL                                 
123072                              WORK-DATE-AREA WORK-KDSVAR                  
123073                                                                          
123074           IF WORK-KDSVAR-OK                                              
123075              MOVE WORK-TIAAMMDD-TOM TO W-DC-AVAIL-DATE                   
123076           END-IF                                                         
123077        END-IF                                                            
123078     END-IF                                                               
123079     MOVE SPACES                     TO WS-IDDC                           
123080     .                                                                    
123081*****************************************************************         
123082* CALCULATE THE CUMULATIVE STOCK QTY IN CDC                               
123083*****************************************************************         
123084 S06-CDC-QTY-CALC SECTION.                                                
123085     MOVE 'S06-CDC-QTY-CAL'  TO CURRENT-SECTION                           
123086                                                                          
123087     PERFORM IMS-GU-WDK601                                                
123088     IF SEGMENT-FINNS                                                     
123089        PERFORM IMS-GNP-WDK611                                            
123090        IF SEGMENT-FINNS                                                  
123091           MOVE CLAG-KVLS         TO W-KVLS                               
123092           MOVE CLAG-KVRESS       TO W-KVRESS                             
123093           MOVE CLAG-KVUTRS       TO W-KVUTRS                             
123094           MOVE CLAG-KVSPARR-KVAL TO W-KVSPARR-KVAL                       
123095           MOVE CLAG-KVSPANT      TO W-KVSPANT                            
123096           MOVE CLAG-IDDC-REF     TO W-IDDC-REF                           
123097           IF CLAG-IDDC-REF > SPACES                                      
123098              PERFORM IMS-GNP-WDK629                                      
123099              MOVE CREF-FLFLYG    TO W-FLFLYG                             
123100           END-IF                                                         
123101        ELSE                                                              
123102           MOVE ZEROES            TO W-KVLS                               
123103                                     W-KVRESS                             
123104                                     W-KVUTRS                             
123105                                     W-KVSPARR-KVAL                       
123106                                     W-KVSPANT                            
123107        END-IF                                                            
123108        PERFORM S06A-GET-WDK9                                             
123109        PERFORM S06B-GET-BO-CDC                                           
123110        PERFORM S06C-CALC-BO-QTY                                          
123111     ELSE                                                                 
123112        MOVE ZEROES               TO W-CDC-QTY                            
123113     END-IF                                                               
123114     .                                                                    
123115*****************************************************************         
123116* GET OKS VOR,DAY AND BULK DATA FOR CDC FROM WDK9                         
123117*****************************************************************         
123118 S06A-GET-WDK9  SECTION.                                                  
123119     MOVE 'S06A-GET-WDK9   '  TO CURRENT-SECTION                          
123120                                                                          
123121     PERFORM IMS-GU-WDK901                                                
123122     IF SEGMENT-FINNS                                                     
123123       MOVE ART-KVOKS-VOR         TO W-OKS-VOR                            
123124       MOVE ART-KVOKS-DAG         TO W-OKS-DAY                            
123125       MOVE ART-KVOKS-BULK        TO W-OKS-BULK                           
123126     ELSE                                                                 
123127        MOVE ZEROS                TO W-OKS-VOR                            
123128                                     W-OKS-DAY                            
123129                                     W-OKS-BULK                           
123130     END-IF                                                               
123131     .                                                                    
123132****************************************************************          
123133*FOR BO ON CDC, QUERY WDA5 FOR CLASS 1,2,3,4 ORDERS                       
123134*               QUERY WDA6 FOR VOR ORDERS                                 
123135*FOR DAY ORDERS,SUMMARIZE VOR AND CLASS 1 QTY                             
123136*FOR BULK ORDER,SUMMARIZE CLASS 2,3,4,QTY                                 
123137****************************************************************          
123138 S06B-GET-BO-CDC SECTION.                                                 
123139     MOVE 'S06B-GET-BO-CDC '  TO CURRENT-SECTION                          
123140                                                                          
123141     PERFORM S06BA-GET-CL1234-BO-WDA5                                     
123142     PERFORM S06BB-GET-VOR-BO-WDA6                                        
123143                                                                          
123144     COMPUTE W-BO-QTY-DAY = W-BO-QTY-VOR + W-BO-QTY-C1                    
123145     .                                                                    
123146                                                                          
123147 S06BA-GET-CL1234-BO-WDA5 SECTION.                                        
123148     MOVE 'S06BA-GET-CL1234'  TO CURRENT-SECTION                          
123149                                                                          
123150     MOVE W-IDARTNR            TO W-WDA5A-IDARTNR-MIN                     
123151                                  W-WDA5A-IDARTNR-MAX                     
123152     MOVE WS-IDDC              TO W-WDA5A-IDDC-MIN                        
123153                                  W-WDA5A-IDDC-MAX                        
123154     MOVE ZEROES               TO W-BO-QTY-C1                             
123155                                  W-BO-QTY-BULK                           
123156     PERFORM IMS-GU-WDA5ASEQ                                              
123157     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
123158                   BASEN-SLUT                                             
123159        IF RAD-KDSTARAD = '2'                                             
123160           IF RAD-KDORDKL = 1                                             
123161              COMPUTE W-BO-QTY-C1 = W-BO-QTY-C1 + RAD-KVRO                
123162           ELSE                                                           
123163              IF (RAD-KDORDKL = 1 OR 2 OR 3)                              
123164                 COMPUTE W-BO-QTY-BULK = W-BO-QTY-BULK +                  
123165                                         RAD-KVRO                         
123166              END-IF                                                      
123167           END-IF                                                         
123168        END-IF                                                            
123169        PERFORM IMS-GN-WDA5ASEQ                                           
123170     END-PERFORM                                                          
123171     .                                                                    
123172                                                                          
123173 S06BB-GET-VOR-BO-WDA6 SECTION.                                           
123174     MOVE 'S06BB-GET-VOR-BO'  TO CURRENT-SECTION                          
123175                                                                          
123176     MOVE LOW-VALUE             TO W-WDA6JSEQ-MIN-X                       
123177     MOVE HIGH-VALUE            TO W-WDA6JSEQ-MAX-X                       
123178                                                                          
123179     MOVE W-IDARTNR             TO SEQJ-IDARTNR-MIN                       
123180                                   SEQJ-IDARTNR-MAX                       
123181     MOVE ZEROES                TO W-BO-QTY-VOR                           
123182                                                                          
123183     PERFORM IMS-GU-WDA6JSEQ                                              
123184     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
123185                   BASEN-SLUT                                             
123186        COMPUTE W-BO-QTY-VOR = W-BO-QTY-VOR + VOR-KVBEART-Q               
123187        PERFORM IMS-GN-WDA6JSEQ                                           
123188     END-PERFORM                                                          
123189     .                                                                    
123190 S06C-CALC-BO-QTY SECTION.                                                
123191     MOVE 'S06C-CALC-BO-QTY' TO CURRENT-SECTION                           
123192                                                                          
123193     COMPUTE W-CDC-QTY  = W-KVLS         -                                
123194                          W-OKS-VOR      -                                
123195                          W-OKS-DAY      -                                
123196                          W-OKS-BULK     -                                
123197                          W-BO-QTY-BULK  -                                
123198                          W-BO-QTY-DAY   -                                
123199                          W-KVRESS       -                                
123200                          W-KVUTRS       -                                
123201                          W-KVSPARR-KVAL -                                
123202                          W-KVSPANT                                       
123203     .                                                                    
123204                                                                          
123205 S07-NDC-QTY-CALC SECTION.                                                
123206     MOVE 'S07-NDC-QTY-CA'  TO CURRENT-SECTION                            
123207                                                                          
123208     PERFORM IMS-GU-WDK711                                                
123209     IF SEGMENT-FINNS                                                     
123210        PERFORM S01-QTY-CALC-XDC                                          
123211        IF KVAN-KVBEART-Q-UT <= W-AVAIL-QTY                               
123212           MOVE JA             TO SALDO-SW                                
123214        ELSE                                                              
123215           MOVE NEJ            TO SALDO-SW                                
123217        END-IF                                                            
123218     ELSE                                                                 
123219        MOVE NEJ               TO SALDO-SW                                
123221     END-IF                                                               
123222     .                                                                    
123223                                                                          
123224 S08-FILL-DDGS-TABLE SECTION.                                             
123225     MOVE 'FC-FILL-DD'  TO CURRENT-SECTION                                
123226                                                                          
123227     INITIALIZE DDGS-TABLE                                                
123228     SET DDGS-IX   TO +1                                                  
123229     MOVE +1 TO DIR-INDX.                                                 
123230     MOVE 0  TO DDGS-ROWS                                                 
123231                                                                          
123232     PERFORM UNTIL DDGS-IX > 5                                            
123233       MOVE DDGS-ROWS  TO DDGS-CLASS(DDGS-IX)                             
123234       MOVE DIR-KVBEART-MIN(DIR-INDX) TO DDGS-MIN-QTY(DDGS-IX)            
123235       MOVE DIR-IDDC(DIR-INDX)        TO DDGS-IDDC(DDGS-IX)               
123236       ADD +1         TO DDGS-ROWS                                        
123237       SET DDGS-IX      UP BY +1                                          
123238       ADD +1          TO  DIR-INDX                                       
123239     END-PERFORM                                                          
123240     .                                                                    
123241                                                                          
123242 IMS-GU-WDF101 SECTION.                                                   
123243     MOVE 'IMS-GU-WDF101   ' TO CURRENT-IMS-SECTION                       
123244                                                                          
123245                                                                          
123246     STRING 'WDF101  (IDLEVNR  =' W-WDF101KY-X ')'                        
123247          DELIMITED BY SIZE INTO SSA1                                     
123248     MOVE '  GE' TO GODK-STATUSKODER                                      
123249     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
123250     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
123251     PERFORM IMS-STATUSKONTROLL                                           
123252     .                                                                    
123253                                                                          
123254 IMS-GU-WDF118 SECTION.                                                   
123255     MOVE 'IMS-GU-WDF118   ' TO CURRENT-IMS-SECTION                       
123256                                                                          
123257     MOVE SPACE              TO ALL-SSA                                   
123258     STRING 'WDF101  (IDLEVNR  =' W-WDF101KY-X ')'                        
123259          DELIMITED BY SIZE INTO SSA1                                     
123260     STRING 'WDF118  (WDF118KY =' W-WDF118KY-X ')'                        
123261          DELIMITED BY SIZE INTO SSA2                                     
123262     MOVE '  GE'                 TO GODK-STATUSKODER                      
123263     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WLLEVA18 SSA1 SSA2             
123264     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
123265     PERFORM IMS-STATUSKONTROLL                                           
123266     .                                                                    
123267                                                                          
123268 IMS-GU-WDK601                 SECTION.                                   
123269     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
123270                                                                          
123271     MOVE SPACE              TO ALL-SSA                                   
123272     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
123273            DELIMITED BY SIZE INTO SSA1                                   
123274     MOVE '  GE'             TO GODK-STATUSKODER                          
123275     CALL  CBLTDLI  USING GU   WDK6-PCB DLI-IO-WDK601 SSA1                
123276     MOVE WDK6-STATUS-CODE   TO STATUS-WS                                 
123277     PERFORM IMS-STATUSKONTROLL                                           
123278     .                                                                    
123279                                                                          
123280 IMS-GNP-WDK611                SECTION.                                   
123281     MOVE 'IMS-GNP-WDK601  ' TO CURRENT-IMS-SECTION                       
123282                                                                          
123283     MOVE SPACE              TO ALL-SSA                                   
123284     MOVE 'WDK611  '                                                      
123285                             TO SSA1                                      
123286     MOVE '  GE'             TO GODK-STATUSKODER                          
123287     CALL  CBLTDLI  USING GNP  WDK6-PCB DLI-IO-WDK611 SSA1                
123288     MOVE WDK6-STATUS-CODE   TO STATUS-WS                                 
123289     PERFORM IMS-STATUSKONTROLL                                           
123290     .                                                                    
123291 IMS-GNP-WDK629     SECTION.                                              
123292                                                                          
123293     MOVE 'WDK629  '       TO SSA1                                        
123294     MOVE '  GE'   TO GODK-STATUSKODER                                    
123295     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
123296     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
123297     PERFORM IMS-STATUSKONTROLL                                           
123298     .                                                                    
123299     EJECT                                                                
123300                                                                          
123301 IMS-GU-WDK711                SECTION.                                    
123302     MOVE 'IMS-GU-WDK711  '  TO CURRENT-IMS-SECTION                       
123303                                                                          
123304     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
123305          DELIMITED BY SIZE INTO SSA1                                     
123306     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
123307          DELIMITED BY SIZE INTO SSA2                                     
123308     MOVE '  GE' TO GODK-STATUSKODER                                      
123309     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
123310     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
123311     PERFORM IMS-STATUSKONTROLL                                           
123312     .                                                                    
123313                                                                          
123314 IMS-GU-XXKJ11 SECTION.                                                   
123315     MOVE 'IMS-GU-XXKJ11   ' TO CURRENT-IMS-SECTION                       
123316                                                                          
123317     MOVE SPACE              TO ALL-SSA                                   
123318     STRING  'WLXXKJ01(WDGXKEY  =' W-WDGX01KEY-X ')'                      
123319            DELIMITED BY SIZE INTO SSA1                                   
123320     STRING  'WLXXKJ11(WDGXKEY >=' W-WDGX11KEY-X ')'                      
123321            DELIMITED BY SIZE INTO SSA2                                   
123322     MOVE    '  GE'             TO GODK-STATUSKODER                       
123323     CALL    CBLTDLI USING GU XXKJ-PCB DLI-IO-XXKJ11 SSA1 SSA2            
123324     MOVE    XXKJ-STATUS-CODE   TO STATUS-WS                              
123325     PERFORM IMS-STATUSKONTROLL                                           
123326     .                                                                    
123327                                                                          
123328 IMS-GU-WDD701 SECTION.                                                   
123329     MOVE 'IMS-GU-WDD701   ' TO CURRENT-IMS-SECTION                       
123330                                                                          
123331     MOVE SPACE              TO ALL-SSA                                   
123332                                                                          
123333     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
123334          DELIMITED BY SIZE INTO SSA1                                     
123335     MOVE 'WDD701   '         TO SSA2                                     
123336     MOVE '  GE'              TO GODK-STATUSKODER                         
123337     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
123338     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
123339     PERFORM IMS-STATUSKONTROLL                                           
123340     .                                                                    
123341                                                                          
123342 IMS-GNP-WDD702 SECTION.                                                  
123343     MOVE 'IMS-GNP-WDD702  ' TO CURRENT-IMS-SECTION                       
123344                                                                          
123345     MOVE SPACE              TO ALL-SSA                                   
123346                                                                          
123347     STRING 'WDD702  (FLTEXT   =' W-FLTEXT-X ')'                          
123348          DELIMITED BY SIZE INTO SSA1                                     
123349     MOVE '  GE'              TO GODK-STATUSKODER                         
123350     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
123351     MOVE WDD7-STATUS-CODE    TO STATUS-WS                                
123352     PERFORM IMS-STATUSKONTROLL                                           
123353     .                                                                    
123354                                                                          
123355 IMS-GU-BENA11-BSEQ   SECTION.                                            
123356     MOVE 'IMS-GNP-BENA11-B' TO CURRENT-IMS-SECTION                       
123357                                                                          
123358     MOVE SPACE              TO ALL-SSA                                   
123359                                                                          
123360     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
123361            DELIMITED BY SIZE INTO SSA1                                   
123362     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
123363            DELIMITED BY SIZE INTO SSA2                                   
123364     MOVE '  GE' TO GODK-STATUSKODER                                      
123365     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WDD311 SSA1 SSA2               
123366     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
123367     PERFORM IMS-STATUSKONTROLL                                           
123368     .                                                                    
123369                                                                          
123370 IMS-GU-WDB301 SECTION.                                                   
123371     MOVE 'IMS-GU-WDB301'   TO CURRENT-IMS-SECTION                        
123372                                                                          
123373     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
123374                    '+WDB301KY =' W-WDB301KY-DEF-X ')'                    
123375          DELIMITED BY SIZE INTO SSA1                                     
123376     MOVE '  GE' TO GODK-STATUSKODER                                      
123377     CALL CBLTDLI USING GHU WDB3-PCB DLI-IO-WDB301 SSA1                   
123378     MOVE WDB3-STATUS-CODE TO STATUS-WS                                   
123379     PERFORM IMS-STATUSKONTROLL                                           
123380     .                                                                    
123400                                                                          
139410 IMS-GU-WDK901  SECTION.                                                  
139411     MOVE 'IMS-GU-WDK901'    TO CURRENT-IMS-SECTION                       
139412                                                                          
139420     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
139430          DELIMITED BY SIZE  INTO SSA1                                    
139440     MOVE '  GE'             TO GODK-STATUSKODER                          
139450     CALL CBLTDLI            USING GU                                     
139460                                   WDK9-PCB                               
139470                                   DLI-IO-AREA-K901                       
139471                                   SSA1                                   
139472     MOVE WDK9-STATUS-CODE   TO STATUS-WS                                 
139473     PERFORM IMS-STATUSKONTROLL                                           
139492     .                                                                    
139493     EJECT                                                                
139494 IMS-GU-WDA5ASEQ SECTION.                                                 
139495     MOVE 'IMS-GU-WDA5ASEQ'  TO CURRENT-IMS-SECTION                       
139496                                                                          
139497     STRING 'WDA501  (WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
139498                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X ')'                    
139499          DELIMITED BY SIZE  INTO SSA1                                    
139500     MOVE '  GE'             TO GODK-STATUSKODER                          
139501     CALL CBLTDLI            USING GU                                     
139502                                   WDA5-PCB                               
139503                                   DLI-IO-AREA-A501                       
139504                                   SSA1                                   
139505     MOVE WDA5-STATUS-CODE   TO STATUS-WS                                 
139506     PERFORM IMS-STATUSKONTROLL                                           
139507     .                                                                    
139508                                                                          
139509 IMS-GN-WDA5ASEQ SECTION.                                                 
139510     MOVE 'IMS-GN-WDA5ASEQ'  TO CURRENT-IMS-SECTION                       
139511                                                                          
139512     STRING 'WDA501  (WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
139513                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X ')'                    
139514          DELIMITED BY SIZE  INTO SSA1                                    
139515     MOVE '  GEGB'           TO GODK-STATUSKODER                          
139516     CALL CBLTDLI            USING GN                                     
139517                                   WDA5-PCB                               
139518                                   DLI-IO-AREA-A501                       
139519                                   SSA1                                   
139520     MOVE WDA5-STATUS-CODE   TO STATUS-WS                                 
139521     PERFORM IMS-STATUSKONTROLL                                           
139522     .                                                                    
139523                                                                          
139524 IMS-GU-WDA6JSEQ SECTION.                                                 
139525     MOVE 'IMS-GU-WDA6JSEQ'  TO CURRENT-IMS-SECTION                       
139526                                                                          
139527     STRING 'WDA601  (WDA6JSEQ>=' W-WDA6JSEQ-MIN-X                        
139528                    '&WDA6JSEQ<=' W-WDA6JSEQ-MAX-X ')'                    
139529          DELIMITED BY SIZE  INTO SSA1                                    
139530     MOVE '  GE'             TO GODK-STATUSKODER                          
139540     CALL CBLTDLI            USING GU                                     
139550                                   WDA6J-PCB                              
139560                                   DLI-IO-AREA-A601                       
139561                                   SSA1                                   
139562     MOVE WDA6J-STATUS-CODE  TO STATUS-WS                                 
139563     PERFORM IMS-STATUSKONTROLL                                           
139564     .                                                                    
139565                                                                          
139566 IMS-GN-WDA6JSEQ SECTION.                                                 
139567     MOVE 'IMS-GN-WDA6JSEQ'  TO CURRENT-IMS-SECTION                       
139568                                                                          
139569     STRING 'WDA601  (WDA6JSEQ>=' W-WDA6JSEQ-MIN-X                        
139570                    '&WDA6JSEQ<=' W-WDA6JSEQ-MAX-X ')'                    
139571          DELIMITED BY SIZE  INTO SSA1                                    
139572     MOVE '  GEGB'           TO GODK-STATUSKODER                          
139573     CALL CBLTDLI            USING GN                                     
139574                                   WDA6J-PCB                              
139575                                   DLI-IO-AREA-A601                       
139576                                   SSA1                                   
139577     MOVE WDA6J-STATUS-CODE  TO STATUS-WS                                 
139578     PERFORM IMS-STATUSKONTROLL                                           
139579     .                                                                    
139580     EJECT                                                                
139590                                                                          
139591 IMS-GU-WDD901 SECTION.                                                   
139592     MOVE 'IMS-GU-WDD901'    TO CURRENT-IMS-SECTION                       
139593                                                                          
139594     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
139595          DELIMITED BY SIZE  INTO SSA1                                    
139596     MOVE '  GE'             TO GODK-STATUSKODER                          
139597     CALL CBLTDLI            USING GU                                     
139598                                   WDD9-PCB                               
139599                                   DLI-IO-WDD901                          
139600                                   SSA1                                   
139700     MOVE WDD9-STATUS-CODE   TO STATUS-WS                                 
139800     PERFORM IMS-STATUSKONTROLL                                           
139900     .                                                                    
140000     EJECT                                                                
140100                                                                          
140200 IMS-GNP-WDD924 SECTION.                                                  
140300     MOVE 'IMS-GNP-WDD924'   TO CURRENT-IMS-SECTION                       
140400     MOVE 'WDD924'               TO SSA1                                  
140500     MOVE '  GE'                 TO GODK-STATUSKODER                      
140600     CALL CBLTDLI             USING GNP                                   
140700                                    WDD9-PCB                              
140800                                    DLI-IO-WDD924                         
140900                                    SSA1                                  
141000     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
141100     PERFORM IMS-STATUSKONTROLL                                           
141200     .                                                                    
141300     EJECT                                                                
141400 IMS-GNP-WDD905 SECTION.                                                  
141500     MOVE 'IMS-GNP-WDD905'   TO CURRENT-IMS-SECTION                       
141600     MOVE 'WDD905'               TO SSA1                                  
141700     MOVE '  GE'                 TO GODK-STATUSKODER                      
141800     CALL CBLTDLI             USING GNP                                   
141900                                    WDD9-PCB                              
142000                                    DLI-IO-WDD905                         
142100                                    SSA1                                  
142200     MOVE WDD9-STATUS-CODE       TO STATUS-WS                             
142300     PERFORM IMS-STATUSKONTROLL                                           
142400     .                                                                    
142500     EJECT                                                                
142501 IMS-GU-WDL601 SECTION.                                                   
142502     MOVE 'IMS-GU-WDL601'    TO CURRENT-IMS-SECTION                       
142503                                                                          
142504     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
142505          DELIMITED BY SIZE INTO SSA1                                     
142506     MOVE '  GE'             TO GODK-STATUSKODER                          
142507     CALL CBLTDLI            USING GU                                     
142508                                   WDL6-PCB                               
142509                                   DLI-IO-WDL601                          
142510                                   SSA1                                   
142520     MOVE WDL6-STATUS-CODE   TO STATUS-WS                                 
142530     PERFORM IMS-STATUSKONTROLL                                           
142540     .                                                                    
142550     EJECT                                                                
142560                                                                          
142570 IMS-GNP-WDL611 SECTION.                                                  
142580     MOVE 'IMS-GNP-WDL611'   TO CURRENT-IMS-SECTION                       
142590                                                                          
142600     MOVE 'WDL611'               TO SSA1                                  
142700     MOVE '  GE'                 TO GODK-STATUSKODER                      
142800     CALL CBLTDLI             USING GNP                                   
142900                                    WDL6-PCB                              
143000                                    DLI-IO-WDL611                         
143100                                    SSA1                                  
143200     MOVE WDL6-STATUS-CODE       TO STATUS-WS                             
143300     PERFORM IMS-STATUSKONTROLL                                           
143400     .                                                                    
143401     EJECT                                                                
143402 IMS-GU-WDF2A SECTION.                                                    
143403     MOVE 'IMS-GU-WDF2A'     TO CURRENT-IMS-SECTION                       
143404                                                                          
143405     STRING 'WDF2A1  (WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
143406                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X                        
143407                    '&DASTADAT<=' W-DASTADAT-X ')'                        
143408          DELIMITED BY SIZE  INTO SSA1                                    
143409     MOVE '  GE'             TO GODK-STATUSKODER                          
143410     CALL CBLTDLI            USING GU                                     
143411                                   WDF2A-PCB                              
143412                                   DLI-IO-WDF2A1                          
143413                                   SSA1                                   
143414     MOVE WDF2A-STATUS-CODE  TO STATUS-WS                                 
143415     PERFORM IMS-STATUSKONTROLL                                           
143416     .                                                                    
143417 IMS-GN-WDF2A SECTION.                                                    
143418     MOVE 'IMS-GU-WDF2A'     TO CURRENT-IMS-SECTION                       
143419                                                                          
143420     STRING 'WDF2A1  (WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
143421                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X                        
143422                    '&DASTADAT<=' W-DASTADAT-X ')'                        
143423          DELIMITED BY SIZE INTO SSA1                                     
143424     MOVE '  GEGB' TO GODK-STATUSKODER                                    
143425     CALL CBLTDLI USING GN                                                
143426                        WDF2A-PCB                                         
143427                        DLI-IO-WDF2A1                                     
143428                        SSA1                                              
143429     MOVE WDF2A-STATUS-CODE TO STATUS-WS                                  
143430     PERFORM IMS-STATUSKONTROLL                                           
143431     .                                                                    
143432     EJECT                                                                
143433 IMS-GU-WDF201 SECTION.                                                   
143434     MOVE 'IMS-GU-WDF201'    TO CURRENT-IMS-SECTION                       
143435                                                                          
143436     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
143437          DELIMITED BY SIZE INTO SSA1                                     
143438     MOVE '  GE'             TO GODK-STATUSKODER                          
143439     CALL CBLTDLI USING GU                                                
143440                        WDF2-PCB                                          
143441                        DLI-IO-WDF201                                     
143442                        SSA1                                              
143443     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
143444     PERFORM IMS-STATUSKONTROLL                                           
143445     .                                                                    
143446                                                                          
143447 IMS-GU-WDF2-DIST-CUS SECTION.                                            
143448     MOVE 'IMS-GU-WDF2-D'    TO CURRENT-IMS-SECTION                       
143449                                                                          
143450     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
143451            DELIMITED BY SIZE INTO SSA1                                   
143452     STRING 'WDF211  (IDDISTRF =' W-IDDISTR-X                             
143453                    '&IDDISTRT =' W-IDDISTR-X                             
143454                    '&IDKUNDNF =' W-IDKUNDNR-FOM-X                        
143455                    '&IDKUNDNT =' W-IDKUNDNR-TOM-X ')'                    
143456            DELIMITED BY SIZE INTO SSA2                                   
143457     MOVE '  GE'             TO GODK-STATUSKODER                          
143458     CALL CBLTDLI USING GU                                                
143459                        WDF2-PCB                                          
143460                        DLI-IO-WDF211                                     
143461                        SSA1                                              
143462                        SSA2                                              
143463     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
143464     PERFORM IMS-STATUSKONTROLL                                           
143465     .                                                                    
143466                                                                          
143467 IMS-GNP-WDF211-DIST-CUS SECTION.                                         
143468     MOVE 'IMS-GNP-WDF211'   TO CURRENT-IMS-SECTION                       
143469                                                                          
143470     MOVE 'WDF211'               TO SSA1                                  
143471     MOVE '  GE'   TO GODK-STATUSKODER                                    
143472     CALL CBLTDLI USING GNP                                               
143473                        WDF2-PCB                                          
143474                        DLI-IO-WDF211                                     
143475                        SSA1                                              
143476     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
143477     PERFORM IMS-STATUSKONTROLL                                           
143478     .                                                                    
143479 IMS-GU-WDB616    SECTION.                                                
143480     MOVE 'IMS-GU-WDB616'    TO CURRENT-IMS-SECTION                       
143481                                                                          
143482     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
143483          DELIMITED BY SIZE INTO SSA1                                     
143484     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
143485          DELIMITED BY SIZE INTO SSA2                                     
143486     MOVE '  GE' TO GODK-STATUSKODER                                      
143487     CALL CBLTDLI USING GU WDB6-PCB                                       
143488                           DLI-IO-WDB616                                  
143489                           SSA1                                           
143490                           SSA2                                           
143491     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
143492     PERFORM IMS-STATUSKONTROLL                                           
143493     .                                                                    
143494     EJECT                                                                
143495 IMS-GU-WDB201 SECTION.                                                   
143496     MOVE 'IMS-GU-WDB201  '  TO CURRENT-IMS-SECTION                       
143497                                                                          
143498     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
143499          DELIMITED BY SIZE INTO SSA1                                     
143500     MOVE '  GE'               TO GODK-STATUSKODER                        
143501     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB201 SSA1               
143502     MOVE WDB2-STATUS-CODE     TO STATUS-WS                               
143503     PERFORM IMS-STATUSKONTROLL                                           
143504     .                                                                    
143505 IMS-GU-WDB1-WDB101 SECTION.                                              
143506     MOVE 'IMS-GU-WDB1-WDB'  TO CURRENT-IMS-SECTION                       
143507                                                                          
143508     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
143509          DELIMITED BY SIZE INTO SSA1                                     
143510     MOVE '  GE'              TO GODK-STATUSKODER                         
143511     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA-WDB101 SSA1               
143512     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
143513     PERFORM IMS-STATUSKONTROLL                                           
143514     .                                                                    
143515     SKIP2                                                                
143516 IMS-STATUSKONTROLL SECTION.                                              
143517                                                                          
143518     SET STATUS-IX TO 1                                                   
143519     SEARCH GODK-STATUS                                                   
143520       AT END                                                             
143521         CALL FELLOG                                                      
143522       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
143523         CONTINUE                                                         
143530     END-SEARCH                                                           
143600     .                                                                    
143700*    -COPY WY2000Q1                                                       
