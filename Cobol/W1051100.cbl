000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1051100.                                                
000400 AUTHOR.         SUSANNE ENEGARD.                                         
000500 DATE-WRITTEN.   JANUARI 1985.                                            
000510 DATE-COMPILED.                                                           
000600                                                                          
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR ETT FRÅGEPROGRAM SOM HÄMTAR INFORMATION            
001100*        OM KATALOGRADER.                                                 
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W1T511                                              
001500*        MID:         W1I51101                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W1O51101                                            
001900*                                                                         
001910*    ÄNDRINGAR:                                                           
001920*                                                                         
001921*        981021: Y2K-ÄNDRINGAR FÖR KDCATPUB-R / KDCATPUB                  
001930*                                                                         
001940*                                                                         
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002401*    -COPY WY2000W2                                                       
002410     SKIP3                                                                
002420*    -- CHECKED BY WY2000  IN 'W.ISPF.EXEC'                               
002500 77   PROGRAM-NAMN           VALUE 'W1051100'                             
002600                                 PIC X(8).                                
002700 77  JA                          PIC X       VALUE 'J'.                   
002800 77  NEJ                         PIC X       VALUE 'N'.                   
002900 77  OCH                         PIC X       VALUE '&'.                   
003000 77  ELLER                       PIC X       VALUE '!'.                   
003100 77  INDX                        PIC S9(9)   VALUE +1   COMP SYNC.        
003200 77  KOL-IX                      PIC S9(9)   VALUE +1   COMP SYNC.        
003300 77  SPRAAK-IX                   PIC S9(9)   VALUE +1   COMP SYNC.        
003400 77  RAK-IX                      PIC S9(9)   VALUE +1   COMP SYNC.        
003500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1557 COMP SYNC.        
003600 77  INDATA-FEL                  PIC X(1)    VALUE 'N'.                   
003700 77  SPRAAK-KOLL                 PIC X(1)    VALUE 'N'.                   
003800 77  FOTNOT-RAKNARE              PIC 9(1)    VALUE ZERO.                  
003900 77  EXTRA-RAD                   PIC 9(1)    VALUE ZERO.                  
004000 77  EXTRA-ANMARK                PIC X(1)    VALUE 'N'.                   
004100 77  RAD-FINNS                   PIC X(1)    VALUE 'N'.                   
004200 77  RAETT-TEXT                  PIC X(1)    VALUE 'N'.                   
004300 77  SPAR-KOD                    PIC X(1)    VALUE ' '.                   
004400                                                                          
004500 01  IDCATNR-WS                  PIC X(5)    VALUE SPACE.                 
004600 01  FILLER  REDEFINES IDCATNR-WS.                                        
004700     03  KEY-IDCATNR             PIC 9(5).                                
004800                                                                          
004900 01  IDCATGRP-WS                 PIC X(2)    VALUE SPACE.                 
005000 01  FILLER  REDEFINES IDCATGRP-WS.                                       
005100     03  KEY-IDCATGRP            PIC 9(2).                                
005200                                                                          
005300 01  IDCATAVS-WS                 PIC X(4)    VALUE SPACE.                 
005400 01  FILLER  REDEFINES IDCATAVS-WS.                                       
005500     03  KEY-IDCATAVS            PIC 9(4).                                
005600                                                                          
005700 01  IDCATRAD-WS                 PIC X(4)    VALUE SPACE.                 
005800 01  FILLER  REDEFINES IDCATRAD-WS.                                       
005900     03  KEY-IDCATRAD            PIC 9(4).                                
006000                                                                          
006100 01  IDSKYLT-WS                  PIC X(3)    VALUE SPACE.                 
006200 01  KEY-KDCATPUB-FOM            PIC X(6)    VALUE SPACE.                 
006300                                                                          
006400 01  SPAR-TAB-KDCATPUB           PIC X(6)    VALUE SPACE.                 
006500 01  SPAR-ILL-KDCATPUB           PIC X(6)    VALUE LOW-VALUE.             
006530                                                                          
006600 01  SPAR-IDCATPOS               PIC X(3)    VALUE SPACE.                 
006700 01  SPAR-KVPUNKT                PIC 9       VALUE ZERO.                  
006800 01  SPAR-BEART                  PIC X(30)   VALUE SPACE.                 
006900     EJECT                                                                
007000* - - - - - - - - - - - - - - - - - - -  DYNAMISKA SUB                    
007100 01  FILLER                      PIC X(16)  VALUE 'DYNAM SUB'.            
007200 01  DYAMISKA-SUBPROGRAM.                                                 
007300     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
007400     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
007500     03  W009VADD                PIC X(8)   VALUE 'W009VADD'.             
007600     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
007700     03  WTXTTR                  PIC X(8)   VALUE 'WTXTTR  '.             
007800* - - - - - - - - - - - - - - - - - - -  VARIABLER                        
007900 01  FILLER                      PIC X(16)  VALUE 'VARIABLER'.            
008000 01  VARIABLER.                                                           
008100     03  SPAR-IDCATRAD           PIC  9(4)   VALUE ZERO.                  
008200     03  SPAR-IDCATRAD-1         PIC  9(4)   VALUE ZERO .                 
008300     03  SPAR-IDCATRAD-7         PIC  9(4)   VALUE ZERO .                 
008400     03  NUVARANDE-IDCATRAD      PIC  9(4)   VALUE ZERO .                 
008500     03  TEST-IDARTNR            PIC S9(9)   VALUE ZERO .                 
008600     03  TEST-KDPS               PIC X(2)    VALUE SPACE.                 
008700     03  SPAR-TEKATANM           PIC X(23).                               
008800     03  SPAR-TEKOL              PIC X(25).                               
008900     03  SPAR-BERUBTEXT.                                                  
009000         05  SPAR-BERUBTEXT-1    PIC X(30).                               
009100         05  SPAR-BERUBTEXT-2    PIC X(30).                               
009200                                                                          
009300     03  FILLER      OCCURS 3.                                            
009400         05  SPAR-IDFOTNR        PIC 9(5).                                
009500                                                                          
009600     03  SPAR-KAP-FAELT          PIC X(30).                               
009700     03  FILLER     REDEFINES SPAR-KAP-FAELT.                             
009800         05  ENFOT-1             PIC 9(5).                                
009900         05  ENFOT-TECK-1        PIC X(1).                                
010000         05  ENFOT-SPACE         PIC X(24).                               
010100     03  FILLER     REDEFINES SPAR-KAP-FAELT.                             
010200         05  TVAFOT-1            PIC 9(5).                                
010300         05  TVAFOT-TECK-1       PIC X(1).                                
010400         05  TVAFOT-2            PIC 9(5).                                
010500         05  TVAFOT-TECK-2       PIC X(1).                                
010600         05  TVAFOT-SPACE        PIC X(18).                               
010700     03  FILLER     REDEFINES SPAR-KAP-FAELT.                             
010800         05  TREFOT-1            PIC 9(5).                                
010900         05  TREFOT-TECK-1       PIC X(1).                                
011000         05  TREFOT-2            PIC 9(5).                                
011100         05  TREFOT-TECK-2       PIC X(1).                                
011200         05  TREFOT-3            PIC 9(5).                                
011300         05  TREFOT-TECK-3       PIC X(1).                                
011400         05  TREFOT-SPACE        PIC X(12).                               
011500                                                                          
011600     03  UT-BETTEXT              PIC X(25)   VALUE SPACE.                 
011700     03  FILLER     OCCURS 3.                                             
011800         05  UT-BERUBTXT         PIC X(30).                               
011900                                                                          
012000     03  WS-KDERS                PIC S9(3) COMP-3 VALUE ZERO.             
012100     EJECT                                                                
012110 01  FILLER                      PIC X(16)  VALUE 'DATUMAREA'.            
012120 01  DATUMAREA.                                                           
012200     03  W009VADD-DATUM          PIC S9(5) COMP-3 VALUE ZERO.             
012300     03  W009VADD-ANTAL          PIC S9(3) COMP-3 VALUE ZERO.             
012400                                                                          
012500     03  WS-TIERSDAT-AAVVD       PIC 9(5) VALUE ZERO.                     
012600     03  FILLER REDEFINES WS-TIERSDAT-AAVVD.                              
012700         05  WS-TIERSDAT-AAVV    PIC 9(4).                                
012800         05  WS-TIERSDAT-D       PIC 9(1).                                
012900                                                                          
012910*    --- KDCATPUB-R / KDCATPUB-hantering                                  
013000     03  DAGENS-AAVVD            PIC 9(5)   VALUE ZERO.                   
013001     03  DAGENS-TIAAAA           PIC 9(4)   VALUE ZERO.                   
013010                                                                          
013011     03 WS-KDCATPUB-R-AVV        PIC X(3)    VALUE SPACE.                 
013012     03 WS-KDCATPUB-AAAAVV       PIC X(6)    VALUE SPACE.                 
013013*    --- Giltiga år =   -1   +0   +1   +2  (indexed by Y2K-IX)            
013020     03 WS-GILTIGA-AAAA.                                                  
013030        05 WS-TIAAAA  OCCURS 4   PIC 9999   VALUE ZERO.                   
013040                                                                          
013050     03 Y2K-IX                   PIC S9(9)  VALUE +1  COMP SYNC.          
013100                                                                          
013110                                                                          
013200 01  FILLER                      PIC X(16)  VALUE 'HJAELP-AREA'.          
013300 01  FEL-BEART-AREA.                                                      
013400*               ANVÄNDS SOM BEN. NÄR BEN. EJ ÄR BENREG-BEN.               
013500     03  TRUNK-BEART             PIC X(15)   VALUE SPACE.                 
013600     03  FILLER                  PIC X(10)   VALUE ALL '*'.               
013700                                                                          
013800 01  SPLIT-IDCATPOS.                                                      
013900     03  FILLER                  PIC X(1)    VALUE SPACE.                 
014000     03  SPLIT-IDCATPOS-1-2      PIC X(2)    VALUE SPACE.                 
014100     EJECT                                                                
014200* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                 
014300 01  FILLER                      PIC X(16)  VALUE 'NYCKLAR-T-DLI'.        
014400 01  NYCKLAR-TILL-DLI.                                                    
014500   03  W-WDN501-X.                                                        
014600       05  W-IDCATNR             PIC  9(5)   VALUE ZERO.                  
014700       05  W-IDCATGRP            PIC  9(2)   VALUE ZERO.                  
014800       05  W-IDCATAVS            PIC  9(4)   VALUE ZERO.                  
014900                                                                          
014910   03  W-WDN5A1-X.                                                        
014920       05  W-IDCATNR-A           PIC  9(5)   VALUE ZERO.                  
014930       05  W-IDCATGRP-A          PIC  9(2)   VALUE ZERO.                  
014940       05  W-IDCATAVS-A          PIC  9(4)   VALUE ZERO.                  
014950                                                                          
015000   03  W-WDN511KY-X.                                                      
015100       05  W-IDILLU-X.                                                    
015200         07  W-IDILLU            PIC S9(5)   VALUE ZERO  COMP-3.          
015300       05  W-KDCATPUB-11KY       PIC  X(6)   VALUE ZERO.                  
015400                                                                          
015500   03  W-WDN512KY-X.                                                      
015600       05  W-IDCATRAD-X.                                                  
015700         07  W-IDCATRAD          PIC  9(4)   VALUE ZERO.                  
015800       05  W-KDCATPUB-12KY       PIC  X(6)   VALUE SPACE.                 
015900                                                                          
015910   03  W-WDN5G1KY-X.                                                      
015911       05 W-WDN5G1KY-01-X.                                                
015912         07 W-IDCATNR-GSEQ       PIC  9(5)   VALUE ZERO.                  
015913         07 W-IDCATGRP-GSEQ      PIC  9(2)   VALUE ZERO.                  
015914         07 W-IDCATAVS-GSEQ      PIC  9(4)   VALUE ZERO.                  
015915       05 W-WDN5G1KY-12-X.                                                
015930         07 W-IDCATRAD-GSEQ      PIC  9(4)   VALUE ZERO.                  
015940         07 W-KDCATPUB-GSEQ      PIC  X(6)   VALUE SPACE.                 
015941   03  W-IDCATRKY-LO             PIC X(21) VALUE LOW-VALUE.               
015942   03  W-IDCATRKY-HI             PIC X(21) VALUE HIGH-VALUE.              
015950                                                                          
016000   03  W-KDCATPUB-GAELLANDE      PIC  X(6)   VALUE SPACE.                 
016100                                                                          
016200   03  W-IDSEGMNR-X.                                                      
016300       05  W-IDSEGMNR            PIC S9(1)   VALUE ZERO  COMP-3.          
016400   03  W-IDARTNR-X.                                                       
016500       05  W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
016600   03  W-KDCLAGER-X.                                                      
016700       05  W-KDCLAGER            PIC S9(1)   VALUE ZERO  COMP-3.          
016800   03  W-IDRUBNR-X.                                                       
016900       05  W-IDRUBNR             PIC S9(5)   VALUE ZERO  COMP-3.          
017000   03  W-IDTTEXNR-X.                                                      
017100       05  W-IDTTEXNR            PIC S9(5)   VALUE ZERO  COMP-3.          
017200   03  W-IDSKYLT-X.                                                       
017300       05  W-IDSKYLT             PIC X(3)    VALUE SPACE.                 
017400   03  W-BEART-X.                                                         
017500       05  W-BEART               PIC X(25)   VALUE SPACE.                 
017600* NYCKLAR FÖR WDN1-LÄSNING                                                
017700   03  W-IDCATNR-1-X.                                                     
017800       05  W-IDCATNR-1           PIC 9(5)    VALUE ZERO.                  
017900   03  W-TIAAAA-X.                                                        
018000       05  W-TIAAAA              PIC 9(4)    VALUE ZERO.                  
018100     EJECT                                                                
018200* - - - - - - - - - - - - - - - - - - -  MEDDELANDEN                      
018300 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDEN'.         
018400 01  MEDDELANDEN.                                                         
018500     03 FILLER-1.                                                         
018600          05 FILLER              PIC X(40)                                
018700              VALUE '    NYCKEL EJ NUMERISK                  '.           
018800          05 FILLER              PIC X(40)                                
018900              VALUE '    KEY NOT NUMERIC                     '.           
019000     03 FILLER REDEFINES FILLER-1.                                        
019100          05 FEL-1   OCCURS 2    PIC X(40).                               
019200     03 FILLER-2.                                                         
019300          05 FILLER              PIC X(40)                                
019400              VALUE '    UPPLYSTA FÄLT FEL                   '.           
019500          05 FILLER              PIC X(40)                                
019600              VALUE '    HILIGHTED FIELDS WRONG              '.           
019700     03 FILLER REDEFINES FILLER-2.                                        
019800          05 FEL-2   OCCURS 2    PIC X(40).                               
019900     03 FILLER-3.                                                         
020000          05 FILLER              PIC X(40)                                
020100              VALUE '    NYCKEL FELAKTIG                     '.           
020200          05 FILLER              PIC X(40)                                
020300              VALUE '    WRONG KEY                           '.           
020400     03 FILLER REDEFINES FILLER-3.                                        
020500          05 FEL-3   OCCURS 2    PIC X(40).                               
020600     03 FILLER-4.                                                         
020700          05 FILLER              PIC X(40)                                
020800              VALUE '    SPRÅK FINNS EJ                      '.           
020900          05 FILLER              PIC X(40)                                
021000              VALUE '    LANGUAGE NOT FOUND                  '.           
021100     03 FILLER REDEFINES FILLER-4.                                        
021200          05 FEL-4   OCCURS 2    PIC X(40).                               
021300     03 FILLER-5.                                                         
021400          05 FILLER              PIC X(40)                                
021500              VALUE '    KATALOG ELLER AVSNITT SAKNAS        '.           
021600          05 FILLER              PIC X(40)                                
021700              VALUE '    CATALOGUE OR TEXT BLOCK IS MISSING  '.           
021800     03 FILLER REDEFINES FILLER-5.                                        
021900          05 FEL-5   OCCURS 2    PIC X(40).                               
022000     03 FILLER-6.                                                         
022100          05 FILLER              PIC X(40)                                
022200              VALUE '    PUBKOD EJ GODKÄND                   '.           
022300          05 FILLER              PIC X(40)                                
022400              VALUE '    PUB CODE VALUE NOT CORRECT          '.           
022500     03 FILLER REDEFINES FILLER-6.                                        
022600          05 FEL-6   OCCURS 2    PIC X(40).                               
022610     03 FILLER-7.                                                         
022620          05 FILLER              PIC X(40)                                
022630              VALUE '    DETTA AVSNITTSHUVUD SAKNAR ILLU.    '.           
022640          05 FILLER              PIC X(40)                                
022650              VALUE '   NO ILLU REGISTERED FOR THIS T-BL HEAD'.           
022660     03 FILLER REDEFINES FILLER-7.                                        
022670          05 FEL-7   OCCURS 2    PIC X(40).                               
022700     03 FILLER-11.                                                        
022800          05 FILLER              PIC X(61)                                
022900              VALUE '    UPPDATERING GJORD                   '.           
023000          05 FILLER              PIC X(61)                                
023100              VALUE '    UPDATING DONE                       '.           
023200     03 FILLER REDEFINES FILLER-11.                                       
023300          05 MED-1   OCCURS 2    PIC X(31).                               
023400     03 FILLER-12.                                                        
023500          05 FILLER              PIC X(23)                                
023600              VALUE '    FLER RADER FINNS  '.                             
023700          05 FILLER              PIC X(23)                                
023800              VALUE '    MORE LINES EXISTS '.                             
023900     03 FILLER REDEFINES FILLER-12.                                       
024000          05 MED-2   OCCURS 2    PIC X(23).                               
024100     03 FILLER-13.                                                        
024200          05 FILLER              PIC X(61)                                
024300              VALUE '    SISTA RADEN HAR KOMPLETTERINGSTEXT  '.           
024400          05 FILLER              PIC X(61)                                
024500              VALUE '    LAST LINE HAS MORE INFORMATIONTEXT  '.           
024600     03 FILLER REDEFINES FILLER-13.                                       
024700          05 MED-3   OCCURS 2    PIC X(61).                               
024800     03 FILLER-14.                                                        
024900          05 FILLER              PIC X(25)                                
025000              VALUE '****** INGEN BENÄMN. REG.'.                          
025100          05 FILLER              PIC X(25)                                
025200              VALUE '****** NO DESCR. REGISTR.'.                          
025300     03 FILLER REDEFINES FILLER-14.                                       
025400          05 MED-4   OCCURS 2    PIC X(25).                               
025500     EJECT                                                                
025600* - - - - - - - - - - - - - - - - - - - - WDATKONV-AREA                   
025700 01  FILLER                 PIC X(16)   VALUE 'WDATKONV-AREA'.            
025800*01  -COPY WDATAREA  -PRE IDAG-.                                          
025900     EJECT                                                                
026000* - - - - - - - - - - - - - - - - - - - - WTXTTR-AREA                     
026100 01  FILLER                 PIC X(16)   VALUE 'WTXTTR-AREA'.              
026200*01  -COPY WTXTAREA                                                       
026300     EJECT                                                                
026400* - - - - - - - - - - - - - - - - - - - - LAND-AREA                       
026500 01  FILLER                      PIC X(16) VALUE 'LAND-AREA'.             
026600*01  -COPY WWLAND06                                                       
026700     EJECT                                                                
026800* - - - - - - - - - - - - - - - - - - - - MID-AREA                        
026900 01  FILLER                      PIC X(16) VALUE '1512-MID-AREA'.         
027000*01  -COPY W1I51201  -PRE 1512-                                           
027010 01  FILLER                      PIC X(16) VALUE '1519-MID-AREA'.         
027020 01  -COPY W1I51901  -PRE 1519-                                           
027100     EJECT                                                                
027200 01  FILLER                      PIC X(16) VALUE '1511-MID-AREA'.         
027300*01  -COPY W1I51101                                                       
027400     EJECT                                                                
027500* - - - - - - - - - - - - - - - - - - - - MSG-AREA                        
027600 01  FILLER                      PIC X(16) VALUE 'MSG-AREA'.              
027700*01  -COPY WMSGAREA                                                       
027800     EJECT                                                                
027900*    03  POST  -COPY W1O51101 -RED MSG-AREA.                              
028000     EJECT                                                                
028100* - - - - - - - - - - - - - - - - - - -  MFS-AREA                         
028200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
028300*01  -COPY WMFSAREA                                                       
028400     EJECT                                                                
028500* - - - - - - - - - - - - - - - - - - -  IMS-WS                           
028600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
028700 01  IMS-WS.                                                              
028800*                        **** STATUS-KOD FRÅN IMS                         
028900   03  STATUS-WS                 PIC XX.                                  
029000     88  SEGMENT-FINNS                       VALUE '  '.                  
029100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029200     88  BASEN-SLUT                          VALUE 'GB'.                  
029300     SKIP2                                                                
029400   03  GODK-STATUSKODER.                                                  
029500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029600     SKIP2                                                                
029700 01  SSA1                        PIC X(144).                              
029800 01  SSA2                        PIC X(64).                               
029900 01  SSA3                        PIC X(64).                               
030000     EJECT                                                                
030100*                            IMS FUNKTIONSKODER                           
030200*01    -COPY W0003                                                        
030300     EJECT                                                                
030400* - - - - - - - - - - - - - - - - - - -  BYTES-ARTIKEL                    
030410*                                        ARTIKELNR FÖR                    
030420*                                        RENOVERINGS-                     
030430*                                        ENHETERNA.                       
030500 01  FILLER                      PIC X(16)  VALUE 'BYTES-ARTIKEL'.        
030600 01  BYTES-IDARTNR               PIC 9(9)   COMP-3.                       
030700*01  FILLER  -COPY WWBYT02  -RED BYTES-IDARTNR.                           
030800     EJECT                                                                
030900* - - - - - - - - - - - - - - - - - - -  DLI-IO-AREA                      
031000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
031100 01  DLI-IO-AREA.                                                         
031200     03      FILLER              PIC X(16)  VALUE 'IO-AREA-G'.            
031300     03      IO-AREA-G           PIC X(48)  VALUE SPACE.                  
031400     SKIP3                                                                
031500*    03  WLKATG01 -COPY WDN5G1        -RED IO-AREA-G.                     
031501     EJECT                                                                
031510     03      FILLER              PIC X(16)  VALUE 'IO-AREA-1'.            
031520     03      IO-AREA-1           PIC X(70)  VALUE SPACE.                  
031530     SKIP3                                                                
031540*    03  WLKATH22 -COPY WDN522        -RED IO-AREA-1.                     
031600     EJECT                                                                
031700*    03  WLKATH01 -COPY WDN501        -RED IO-AREA-1.                     
031800     EJECT                                                                
031900*    03  WLKATH11 -COPY WDN511        -RED IO-AREA-1.                     
032000     EJECT                                                                
032100*    03  WLKATH12 -COPY WDN512        -RED IO-AREA-1.                     
032200     EJECT                                                                
032300*    03  WLKATH21 -COPY WDN521        -RED IO-AREA-1.                     
032400     EJECT                                                                
032500*    03  WLKATH23 -COPY WDN523        -RED IO-AREA-1.                     
032600     EJECT                                                                
032700*    03  WLKATH24 -COPY WDN524        -RED IO-AREA-1.                     
032800     EJECT                                                                
032900*    03  WLKATH25 -COPY WDN525        -RED IO-AREA-1.                     
033000     EJECT                                                                
033100*    03  WLKATH26 -COPY WDN526        -RED IO-AREA-1.                     
033200     EJECT                                                                
033300*    03  WLKATH27 -COPY WDN527        -RED IO-AREA-1.                     
033400     EJECT                                                                
033700     03      FILLER              PIC X(16)  VALUE 'IO-AREA-2'.            
033800     03      IO-AREA-2           PIC X(900) VALUE SPACE.                  
033900     SKIP3                                                                
034000*    03  WLARTC01 -COPY WDK601      -PRE ARTC01- -RED IO-AREA-2.          
034100     EJECT                                                                
034200*    03  WLARTC11 -COPY WDK611      -PRE ARTC11-  -RED IO-AREA-2.         
034300     EJECT                                                                
034400*    03  WLKATM11 -COPY WDN111      -PRE KAT-  -RED IO-AREA-2.            
034500     EJECT                                                                
034600*    03  WLKATB01 -COPY WDN201      -PRE RUB-   -RED IO-AREA-2.           
034700     EJECT                                                                
034800*    03  WLKATB11 -COPY WDN211      -PRE RUB-   -RED IO-AREA-2.           
034900     EJECT                                                                
035000*    03  WLKATD01 -COPY WDN401                  -RED IO-AREA-2.           
035100     EJECT                                                                
035200*    03  WLKATD11 -COPY WDN411                  -RED IO-AREA-2.           
035300     EJECT                                                                
035400*    03  WLBENA01 -COPY WDD301      -PRE BEN-   -RED IO-AREA-2.           
035500     EJECT                                                                
035600*    03  WLBENA11 -COPY WDD311      -PRE BEN-   -RED IO-AREA-2.           
035700     EJECT                                                                
035800*    03  WLBENA12 -COPY WDD312      -PRE BEN-   -RED IO-AREA-2.           
035900     EJECT                                                                
036000*    03  WLBENA13 -COPY WDD313      -PRE BEN-   -RED IO-AREA-2.           
036100     EJECT                                                                
036110     03      FILLER              PIC X(16)  VALUE 'WDN5A-AREA'.           
036130     SKIP3                                                                
036140*    03  WDN5A-AREA  -COPY WDN5A1.                                        
036150     EJECT                                                                
036200 LINKAGE SECTION.                                                         
036300     SKIP2                                                                
036400*01  -COPY W0009          -PRE MSG-                                       
036500     EJECT                                                                
036600*01  -COPY W0008          -PRE AVS-                                       
036700     05  FILLER                  PIC X.                                   
036800     EJECT                                                                
036900*01  -COPY W0008          -PRE ARTC-                                      
037000     05  FILLER                  PIC X.                                   
037100     EJECT                                                                
037200*01  -COPY W0008          -PRE RUB-                                       
037300     05  FILLER                  PIC X.                                   
037400     EJECT                                                                
037500*01  -COPY W0008          -PRE TEXT-                                      
037600     05  FILLER                  PIC X.                                   
037700     EJECT                                                                
037800*01  -COPY W0008          -PRE BENA-                                      
037900     05  FILLER                  PIC X.                                   
038000     EJECT                                                                
038100*01  -COPY W0008          -PRE BENB-                                      
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01  -COPY W0008          -PRE KATM-                                      
038500     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038610*01  -COPY W0008          -PRE KATS-                                      
038620     05  FILLER                  PIC X.                                   
038630     EJECT                                                                
038640*01  -COPY W0008          -PRE WDN5A-                                     
038650     05  FILLER                  PIC X.                                   
038660     EJECT                                                                
038700 PROCEDURE DIVISION USING MSG-PCB AVS-PCB ARTC-PCB                        
038800           RUB-PCB TEXT-PCB BENA-PCB BENB-PCB KATM-PCB KATS-PCB           
038810           WDN5A-PCB.                                                     
038900     ENTRY 'DLITCBL' USING MSG-PCB AVS-PCB ARTC-PCB                       
039000           RUB-PCB TEXT-PCB BENA-PCB BENB-PCB KATM-PCB KATS-PCB           
039010           WDN5A-PCB.                                                     
039100     SKIP2                                                                
039200     PERFORM IMS-GET-MSG                                                  
039300     IF SEGMENT-FINNS                                                     
039400        PERFORM A-INIT                                                    
039500        IF (IDCATNR-WS  NOT NUMERIC)                                      
039600        OR (IDCATGRP-WS NOT NUMERIC)                                      
039700        OR (IDCATAVS-WS NOT NUMERIC)                                      
039800        OR (IDCATRAD-WS NOT NUMERIC)                                      
039900           MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                         
040000           PERFORM D-RENSA-BILD                                           
040100        ELSE                                                              
040200           IF SPRAAK-KOLL = NEJ                                           
040300              MOVE FEL-4 (SPRAAK-IX) TO MOD-TEMFSFEL                      
040400              MOVE KEY-IDCATRAD   TO MOD-IDCATRAD-NEXT                    
040500           ELSE                                                           
040600              MOVE KEY-IDCATNR    TO W-IDCATNR  W-IDCATNR-1               
040700              MOVE KEY-IDCATGRP   TO W-IDCATGRP                           
040800              MOVE KEY-IDCATAVS   TO W-IDCATAVS                           
040900              MOVE KEY-KDCATPUB-FOM TO W-KDCATPUB-11KY                    
041000                                       W-KDCATPUB-12KY                    
041010              IF MFS-IDPFK = '5'                                          
041011*                -- "Find next" avsnitt med samma PUB-kod                 
041012                 MOVE W-WDN501-X TO W-WDN5A1-X                            
041013                 PERFORM IMS-GU-AVS-ASEQ                                  
041014                 IF SEGMENT-FINNS                                         
041015                    MOVE AVSA-IDCATAKY TO W-WDN501-X                      
041016                    MOVE W-IDCATNR   TO KEY-IDCATNR                       
041017                    MOVE W-IDCATGRP  TO KEY-IDCATGRP                      
041018                    MOVE W-IDCATAVS  TO KEY-IDCATAVS                      
041019                    PERFORM AB-FIXA-MOD-UTNYCKLAR                         
041020                 END-IF                                                   
041030              END-IF                                                      
041100              PERFORM B-LAS-RUBRIKER                                      
041200              IF INDATA-FEL = NEJ                                         
041300                 MOVE KEY-IDCATRAD TO W-IDCATRAD                          
041400                 MOVE KEY-KDCATPUB-FOM TO W-KDCATPUB-GAELLANDE            
041500                 IF MFS-FIRST OR MFS-IDPFK = '5'                          
041600                    MOVE +20 TO W-IDCATRAD                                
041700                 ELSE                                                     
041800                    IF MFS-NEXT                                           
041900                       PERFORM S01-FLYTTA-FROM                            
042000                    ELSE                                                  
042100                       IF  (MID-IDCATNR-IN  = ALL '+')                    
042200                       AND (MID-IDCATGRP-IN = ALL '+')                    
042300                       AND (MID-IDCATAVS-IN = ALL '+')                    
042400                       AND (MID-IDCATRAD-IN = ALL '+')                    
042500                          PERFORM S01-FLYTTA-FROM                         
042600                       END-IF                                             
042700                    END-IF                                                
042800                 END-IF                                                   
042900                 PERFORM C-LAS-RADER                                      
043000                 PERFORM F-FYLL-DOLT-FAELT                                
043100                 PERFORM E-RENSA-RADER                                    
043200              ELSE                                                        
043300                 PERFORM D-RENSA-BILD                                     
043400              END-IF                                                      
043500           END-IF                                                         
043600        END-IF                                                            
043700        MOVE LENGTH OF MOD-W1O51101 TO MSG-KVLL                           
043800        ADD   +4                    TO MSG-KVLL                           
043900        PERFORM IMS-INSERT-MSG                                            
044000     END-IF                                                               
044100                                                                          
044200     MOVE ZERO TO RETURN-CODE                                             
044300     GOBACK                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 A-INIT SECTION.                                                          
044700     SKIP2                                                                
044800     MOVE SPACE TO IDSKYLT-WS                                             
044900     IF MSG-DUBBLA-TRANSKODER                                             
045000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I51101                 
045100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
045200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
045300       MOVE MSG-IDPFK TO MFS-IDPFK                                        
045400     ELSE                                                                 
045500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I51101                  
045600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
045700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
045800     END-IF                                                               
045900                                                                          
046000     IF MFS-IDTRANS = '1511'                                              
046100        CONTINUE                                                          
046200     ELSE                                                                 
046300       MOVE SPACE TO MFS-IDPFK                                            
046400       IF MFS-IDTRANS NOT = 1512 AND 1513 AND 1514 AND 1515               
046410                                 AND 1518 AND 1519                        
046500         MOVE SPACE TO MID-IDSKYLT-IN                                     
046510                       MID-KDCATPUB-R-FOM-IN                              
046520         MOVE ZERO  TO MID-IDCATNR-IN                                     
046530                       MID-IDCATGRP-IN                                    
046540                       MID-IDCATAVS-IN                                    
046550         MOVE 0020  TO MID-IDCATRAD-IN                                    
046600       END-IF                                                             
046700     END-IF                                                               
046800                                                                          
046900     IF MID-IDCATNR-IN = ALL '+'                                          
047000        MOVE MID-IDCATNR-UT TO IDCATNR-WS                                 
047100        INSPECT IDCATNR-WS REPLACING LEADING SPACE BY ZERO                
047200     ELSE                                                                 
047300        MOVE MID-IDCATNR-IN TO IDCATNR-WS                                 
047400     END-IF                                                               
047500                                                                          
047600     IF MID-IDCATGRP-IN = ALL '+'                                         
047700        MOVE MID-IDCATGRP-UT TO IDCATGRP-WS                               
047800        INSPECT IDCATGRP-WS REPLACING LEADING SPACE BY ZERO               
047900     ELSE                                                                 
048000        MOVE MID-IDCATGRP-IN TO IDCATGRP-WS                               
048100     END-IF                                                               
048200                                                                          
048300     IF MID-IDCATAVS-IN = ALL '+'                                         
048400        MOVE MID-IDCATAVS-UT TO IDCATAVS-WS                               
048500        INSPECT IDCATAVS-WS REPLACING LEADING SPACE BY ZERO               
048600     ELSE                                                                 
048700        MOVE MID-IDCATAVS-IN TO IDCATAVS-WS                               
048800     END-IF                                                               
048900                                                                          
049000     IF MID-IDCATRAD-IN = ALL '+'                                         
049100        MOVE MID-IDCATRAD-UT TO IDCATRAD-WS                               
049200        INSPECT IDCATRAD-WS REPLACING LEADING SPACE BY ZERO               
049300     ELSE                                                                 
049400        MOVE MID-IDCATRAD-IN TO IDCATRAD-WS                               
049500     END-IF                                                               
049510                                                                          
049512     MOVE 'IDAG  ' TO IDAG-DAT-KDDATFORM                                  
049513     CALL WDATKONV USING  IDAG-DAT-KDDATFORM  IDAG-DAT-I-TIDATUM,         
049514                          IDAG-DAT-O-TIDATUM  IDAG-DAT-KDSVAR             
049515                                                                          
049520     MOVE FUNCTION CURRENT-DATE(1:4) TO DAGENS-TIAAAA                     
049530                                                                          
049540     ADD -1 TO DAGENS-TIAAAA GIVING WS-TIAAAA(1)                          
049550     ADD  0 TO DAGENS-TIAAAA GIVING WS-TIAAAA(2)                          
049551     ADD  1 TO DAGENS-TIAAAA GIVING WS-TIAAAA(3)                          
049560     ADD  2 TO DAGENS-TIAAAA GIVING WS-TIAAAA(4)                          
049600                                                                          
049700     IF MID-KDCATPUB-R-FOM-IN = ALL '+'                                   
049800       MOVE MID-KDCATPUB-R-FOM-UT TO WS-KDCATPUB-R-AVV                    
049900     ELSE                                                                 
050000       MOVE MID-KDCATPUB-R-FOM-IN TO WS-KDCATPUB-R-AVV                    
050100     END-IF                                                               
050110     PERFORM S50-Y2K-KDCATPUB-R                                           
050111                                                                          
050120     IF WS-KDCATPUB-AAAAVV = SPACE                                        
050130*      --- Lägg in dagens värden                                          
050140       MOVE DAGENS-TIAAAA        TO WS-KDCATPUB-AAAAVV                    
050150       MOVE IDAG-DAT-TIVV        TO WS-KDCATPUB-AAAAVV (5:2)              
050160     ELSE                                                                 
050170       MOVE WS-KDCATPUB-AAAAVV TO KEY-KDCATPUB-FOM                        
050180     END-IF                                                               
050200                                                                          
050300     IF  MID-IDCATNR-IN  = ALL '+'                                        
050400     AND MID-IDCATGRP-IN = ALL '+'                                        
050500     AND MID-IDCATAVS-IN = ALL '+'                                        
050600     AND MID-KDCATPUB-R-FOM-IN = ALL '+'                                  
050700        IF MID-IDCATRAD-IN = ALL '+'                                      
050800           CONTINUE                                                       
050900        ELSE                                                              
051000           IF IDCATRAD-WS NUMERIC                                         
051100              IF KEY-IDCATRAD <= 0019                                     
051200                 MOVE  0020 TO KEY-IDCATRAD                               
051300              END-IF                                                      
051400           END-IF                                                         
051500        END-IF                                                            
051600     ELSE                                                                 
051700        IF MID-IDCATRAD-IN = ALL '+'                                      
051800           MOVE  0020 TO KEY-IDCATRAD                                     
051900        ELSE                                                              
052000           IF IDCATRAD-WS NUMERIC                                         
052100              IF KEY-IDCATRAD <= 0019                                     
052200                 MOVE  0020 TO KEY-IDCATRAD                               
052300              END-IF                                                      
052400           ELSE                                                           
052500              MOVE  0020 TO KEY-IDCATRAD                                  
052600           END-IF                                                         
052700        END-IF                                                            
052800     END-IF                                                               
052900                                                                          
053000     IF MID-IDSKYLT-IN = ALL '+'                                          
053100        MOVE MID-IDSKYLT-UT TO IDSKYLT-WS                                 
053200     ELSE                                                                 
053300        MOVE MID-IDSKYLT-IN TO IDSKYLT-WS                                 
053400     END-IF                                                               
053500                                                                          
053600     IF IDSKYLT-WS = SPACE                                                
053700        MOVE 'S  '       TO IDSKYLT-WS                                    
053800     END-IF                                                               
053900                                                                          
054000     SET WWLAND06-IX TO +1                                                
054100     SEARCH WWLAND06-IDSKYLT-RAD                                          
054200                   AT END MOVE NEJ TO SPRAAK-KOLL                         
054300        WHEN WWLAND06-IDSKYLT(WWLAND06-IX) = IDSKYLT-WS                   
054400                   MOVE JA TO SPRAAK-KOLL                                 
054410*       --- Godkänner alla förekommande språk då inget uppdateras         
054500     END-SEARCH                                                           
054600     MOVE LOW-VALUE TO MSG-AREA                                           
054700     MOVE 'W1O51101' TO MFS-IDMOD                                         
054800     MOVE '1511' TO MOD-IDTRANS                                           
054900                                                                          
055000                                                                          
055100     IF ENGLISH-TEXT                                                      
055200        MOVE +2 TO SPRAAK-IX                                              
055300     ELSE                                                                 
055400        MOVE +1 TO SPRAAK-IX                                              
055500     END-IF                                                               
055600                                                                          
055700     PERFORM AB-FIXA-MOD-UTNYCKLAR                                        
057500     PERFORM AA-FIXA-HOPPNYCKLAR                                          
057600                                                                          
057700     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
057800                             MOD-IDCATGRP-IN                              
057900                             MOD-IDCATAVS-IN                              
058000                             MOD-IDCATRAD-IN                              
058100                             MOD-IDSKYLT-IN                               
058200                             MOD-KDCATPUB-R-FOM-IN                        
058300                             MOD-TEMFSFEL                                 
058400                             MOD-TEMFSINF                                 
058500     .                                                                    
058600     EJECT                                                                
058700 AA-FIXA-HOPPNYCKLAR SECTION.                                             
058800     SKIP2                                                                
058810     IF MFS-IDTRANS = '1511'                                              
058820       MOVE MID-KDCATPUB-R-MIN TO MOD-KDCATPUB-R-MIN                      
058830       MOVE MID-KDCATPUB-R-MAX TO MOD-KDCATPUB-R-MAX                      
058840     ELSE                                                                 
058850       MOVE LOW-VALUE TO MOD-KDCATPUB-R-MIN                               
058860       MOVE HIGH-VALUE TO MOD-KDCATPUB-R-MAX                              
058870     END-IF                                                               
058900     IF MFS-IDTRANS = '1512'                                              
059000       MOVE MID-W1I51101 TO 1512-MID-W1I51201                             
059100       IF 1512-MID-KDCATPUB-R-MIN-IN = ALL '+'                            
059200         MOVE 1512-MID-KDCATPUB-R-MIN-UT TO MOD-KDCATPUB-R-MIN            
059300       ELSE                                                               
059400         MOVE 1512-MID-KDCATPUB-R-MIN-IN TO MOD-KDCATPUB-R-MIN            
059500       END-IF                                                             
059600       IF 1512-MID-KDCATPUB-R-MAX-IN = ALL '+'                            
059700         MOVE 1512-MID-KDCATPUB-R-MAX-UT TO MOD-KDCATPUB-R-MAX            
059800       ELSE                                                               
059900         MOVE 1512-MID-KDCATPUB-R-MAX-IN TO MOD-KDCATPUB-R-MAX            
060000       END-IF                                                             
060100     END-IF                                                               
060110     IF MFS-IDTRANS = '1519'                                              
060120       MOVE MID-W1I51101 TO 1519-MID-W1I51901                             
060130       IF 1519-MID-KDCATPUB-R-FOM-F-IN = ALL '+'                          
060140         MOVE 1519-MID-KDCATPUB-R-FOM-F-UT TO MOD-KDCATPUB-R-MIN          
060150       ELSE                                                               
060160         MOVE 1519-MID-KDCATPUB-R-FOM-F-IN TO MOD-KDCATPUB-R-MIN          
060170       END-IF                                                             
060180       IF 1519-MID-KDCATPUB-R-FOM-T-IN = ALL '+'                          
060190         MOVE 1519-MID-KDCATPUB-R-FOM-T-UT TO MOD-KDCATPUB-R-MAX          
060191       ELSE                                                               
060192         MOVE 1519-MID-KDCATPUB-R-FOM-T-IN TO MOD-KDCATPUB-R-MAX          
060193       END-IF                                                             
060194     END-IF                                                               
061000     .                                                                    
061100     EJECT                                                                
061101 AB-FIXA-MOD-UTNYCKLAR  SECTION.                                          
061102     SKIP2                                                                
061110     MOVE IDCATNR-WS TO MOD-IDCATNR-UT                                    
061120     INSPECT MOD-IDCATNR-UT REPLACING LEADING ZERO BY SPACE               
061130                                                                          
061140     MOVE IDCATGRP-WS TO MOD-IDCATGRP-UT                                  
061150     INSPECT MOD-IDCATGRP-UT REPLACING LEADING ZERO BY SPACE              
061160     IF MOD-IDCATGRP-UT = SPACE MOVE ' 0' TO MOD-IDCATGRP-UT              
061170     END-IF                                                               
061180                                                                          
061190     MOVE IDCATAVS-WS TO MOD-IDCATAVS-UT                                  
061191     INSPECT MOD-IDCATAVS-UT REPLACING LEADING ZERO BY SPACE              
061192     IF MOD-IDCATAVS-UT = SPACE MOVE '   0' TO MOD-IDCATAVS-UT            
061193     END-IF                                                               
061194                                                                          
061195     MOVE IDSKYLT-WS TO MOD-IDSKYLT-UT                                    
061196                                                                          
061197     MOVE IDCATRAD-WS TO MOD-IDCATRAD-UT                                  
061198     INSPECT MOD-IDCATRAD-UT REPLACING LEADING ZERO BY SPACE              
061199                                                                          
061200     MOVE KEY-KDCATPUB-FOM (4:3) TO MOD-KDCATPUB-R-FOM-UT                 
061201                                                                          
061202     .                                                                    
061203     EJECT                                                                
061210 B-LAS-RUBRIKER SECTION.                                                  
061300     SKIP2                                                                
061400     MOVE SPACE TO SPAR-KOD                                               
061500     PERFORM IMS-GU-AVS                                                   
061600     IF SEGMENT-SAKNAS                                                    
061700        MOVE FEL-5 (SPRAAK-IX) TO MOD-TEMFSFEL                            
061800        MOVE JA TO INDATA-FEL                                             
061900     ELSE                                                                 
062000        MOVE AVS-IDVERS    TO MOD-IDVERS                                  
062100        MOVE AVS-FLAVSUST  TO MOD-FLAVSUST                                
062200        MOVE AVS-FLAVSTVAD TO MOD-FLAVSTVAD                               
062300        PERFORM BE-KOLLA-KEY-KDCATPUB-FOM                                 
062400        IF INDATA-FEL = NEJ                                               
062500          PERFORM BA-LAS-ILLUSTRATION                                     
062501                                                                          
062510          IF  MOD-IDILLU = ZERO OR SPACE                                  
062511          AND W-IDCATAVS > 1                                              
062512*           --- Om avsnittet SAKNAR ILLU och det är AVS > 1               
062513*           --- VISAS INTE raderna.                                       
062514            MOVE JA TO INDATA-FEL                                         
062515            MOVE FEL-7 (SPRAAK-IX) TO MOD-TEMFSFEL                        
062520          ELSE                                                            
062600            PERFORM BB-LAS-RUBRIKNR                                       
062700            PERFORM BC-LAS-RUBRIKTEXT-RAD4                                
062800            PERFORM BD-LAS-KOLUMNTEXT                                     
062900          END-IF                                                          
062910        END-IF                                                            
063000     END-IF                                                               
063100     .                                                                    
063200     EJECT                                                                
063300 BA-LAS-ILLUSTRATION SECTION.                                             
063400     SKIP2                                                                
063500     MOVE ZERO TO MOD-IDILLU                                              
063600     MOVE LOW-VALUE TO SPAR-ILL-KDCATPUB                                  
063900                                                                          
064000     PERFORM IMS-GNP-AVS-ILLU                                             
064500     PERFORM UNTIL SEGMENT-SAKNAS                                         
064600       IF  ILLU-KDCATPUB-FOM <= KEY-KDCATPUB-FOM                          
064610       AND ILLU-KDCATPUB-FOM >= SPAR-ILL-KDCATPUB                         
064700          MOVE ILLU-IDILLU       TO MOD-IDILLU                            
064800          MOVE ILLU-KDCATPUB-FOM TO SPAR-ILL-KDCATPUB                     
064900       END-IF                                                             
065000       PERFORM IMS-GNP-AVS-ILLU                                           
065100     END-PERFORM                                                          
065200     .                                                                    
065300     EJECT                                                                
065400 BB-LAS-RUBRIKNR SECTION.                                                 
065500     SKIP2                                                                
065600     PERFORM IMS-GU-AVS                                                   
065610*     <><><><><><><><><><><><>                                            
065611     MOVE 0001               TO W-IDCATRAD                                
065612     MOVE LOW-VALUE          TO W-KDCATPUB-12KY                           
065613     PERFORM IMS-GNP-FIRST-AVS-RAD                                        
065614                                                                          
065615     PERFORM UNTIL SEGMENT-SAKNAS                                         
065616     OR  RAD-IDCATRAD > 1                                                 
065617     OR ( KEY-KDCATPUB-FOM >= RAD-KDCATPUB-FOM                            
065618     AND  KEY-KDCATPUB-FOM <= RAD-KDCATPUB-TOM )                          
065619        MOVE RAD-IDCATRAD     TO W-IDCATRAD                               
065621        MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-12KY                          
065623        PERFORM IMS-GNP-NEXT-AVS-RAD                                      
065624     END-PERFORM                                                          
065625                                                                          
065626     IF SEGMENT-FINNS                                                     
065627     AND RAD-IDCATRAD = 0001                                              
065629        MOVE RAD-KDCATPUB-FOM  TO W-KDCATPUB-12KY                         
065630     END-IF                                                               
065640*     <><><><><><><><><><><><>                                            
065700     MOVE +1 TO INDX                                                      
065800     MOVE 0001              TO W-IDCATRAD                                 
065900*    Läs rubrikraderna med bildens pub (om det finns nån bild)            
065910     IF MOD-IDILLU NOT = ( ZERO AND SPACE )                               
066000       MOVE SPAR-ILL-KDCATPUB TO W-KDCATPUB-12KY                          
066010     END-IF                                                               
066200     PERFORM IMS-GU-AVS                                                   
066300     PERFORM IMS-GNP-AVS-RAD-KVAL                                         
066400     IF SEGMENT-SAKNAS                                                    
066500        ADD +1 TO INDX                                                    
066600        PERFORM UNTIL INDX > +3                                           
066700           MOVE SPACE TO MOD-BERUBTXT(INDX)                               
066800           ADD +1 TO INDX                                                 
066900        END-PERFORM                                                       
067000     ELSE                                                                 
067100        MOVE SPACE TO MOD-BERUBTXT(1)   MOD-BERUBTXT(2)                   
067200                      MOD-BERUBTXT(3)                                     
067300        PERFORM IMS-GNP-AVS-RUB                                           
067400        PERFORM UNTIL INDX > +3                                           
067500        OR (SEGMENT-SAKNAS OR BASEN-SLUT)                                 
067600           MOVE RUB-IDRUBNR TO W-IDRUBNR                                  
067700           PERFORM IMS-GU-RUB                                             
067800           IF SEGMENT-FINNS                                               
067900              MOVE IDSKYLT-WS TO W-IDSKYLT                                
068000              IF RUB-RUB-FLKOMBINERAS = NEJ                               
068100                 PERFORM IMS-GNP-RUB-TEXT                                 
068200                 PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT               
068300                    MOVE RUB-TEXT-BERUBTXT TO                             
068400                         MOD-BERUBTXT(RUB-TEXT-IDSEGMNR)                  
068500                    ADD +1 TO INDX                                        
068600                    PERFORM IMS-GNP-RUB-TEXT                              
068700                 END-PERFORM                                              
068800******           MOVE +4 TO INDX                                          
068900              ELSE                                                        
069000                 PERFORM IMS-GNP-RUB-TEXT                                 
069100                 IF SEGMENT-FINNS                                         
069200                    MOVE RUB-TEXT-BERUBTXT TO                             
069300                         MOD-BERUBTXT (RUB-IDSEGMNR)                      
069400                 END-IF                                                   
069500              END-IF                                                      
069600           END-IF                                                         
069700           PERFORM IMS-GNP-AVS-RUB                                        
069800           ADD +1 TO INDX                                                 
069900        END-PERFORM                                                       
070000     END-IF                                                               
070100     .                                                                    
070200     EJECT                                                                
070300 BC-LAS-RUBRIKTEXT-RAD4 SECTION.                                          
070400     SKIP2                                                                
070500     PERFORM IMS-GU-AVS                                                   
070600     MOVE 'A' TO SPAR-KOD                                                 
070700     MOVE 0004 TO W-IDCATRAD                                              
070800     PERFORM IMS-GNP-AVS-RAD-KVAL                                         
070900     IF SEGMENT-SAKNAS                                                    
071000        MOVE SPACE TO SPAR-BERUBTEXT                                      
071100     ELSE                                                                 
071200        PERFORM IMS-GNP-AVS-TEXT                                          
071300        IF SEGMENT-FINNS                                                  
071400           MOVE TEXT-BERUBTEXT TO SPAR-BERUBTEXT                          
071500        ELSE                                                              
071600           MOVE SPACE TO SPAR-BERUBTEXT                                   
071700        END-IF                                                            
071800        PERFORM S02-LAES-FOT                                              
071900        IF FOTNOT-RAKNARE NOT = ZERO                                      
072000           PERFORM S03-FLYTTA-FOTNOT                                      
072100           MOVE SPAR-KAP-FAELT   TO MOD-BERUBTEXT-1                       
072200           MOVE SPAR-BERUBTEXT-1 TO MOD-BERUBTEXT-2(1)                    
072300           MOVE SPAR-BERUBTEXT-2 TO MOD-BERUBTEXT-2(2)                    
072400        ELSE                                                              
072500           MOVE SPAR-BERUBTEXT   TO MOD-BERUBTEXT                         
072600        END-IF                                                            
072700     END-IF                                                               
072800     .                                                                    
072900     EJECT                                                                
073000 BD-LAS-KOLUMNTEXT SECTION.                                               
073100     SKIP2                                                                
073200     MOVE 'B' TO SPAR-KOD                                                 
073300     MOVE 0010 TO W-IDCATRAD                                              
073400     MOVE +1 TO INDX                                                      
073500     PERFORM UNTIL INDX > +5                                              
073600        PERFORM IMS-GNP-AVS-RAD-KVAL                                      
073700        IF SEGMENT-SAKNAS                                                 
073800           MOVE SPACE TO SPAR-TEKOL                                       
073900        ELSE                                                              
074000           PERFORM IMS-GNP-AVS-TEXT                                       
074100           IF SEGMENT-FINNS                                               
074200              MOVE TEXT-TEKOL TO SPAR-TEKOL                               
074300           ELSE                                                           
074400              MOVE SPACE TO SPAR-TEKOL                                    
074500           END-IF                                                         
074600        END-IF                                                            
074700        PERFORM S02-LAES-FOT                                              
074800        IF FOTNOT-RAKNARE NOT = ZERO                                      
074900           PERFORM S03-FLYTTA-FOTNOT                                      
075000           MOVE SPAR-KAP-FAELT TO MOD-TEKOL(INDX)                         
075100        ELSE                                                              
075200           MOVE SPAR-TEKOL TO MOD-TEKOL(INDX)                             
075300        END-IF                                                            
075400        ADD +1 TO INDX                                                    
075500        ADD +1 TO W-IDCATRAD                                              
075600     END-PERFORM                                                          
075700     .                                                                    
075800     EJECT                                                                
075900 BE-KOLLA-KEY-KDCATPUB-FOM SECTION.                                       
076000     SKIP2                                                                
076100     IF KEY-KDCATPUB-FOM = ALL '+'                                        
076200     OR KEY-KDCATPUB-FOM = SPACE OR '000' OR '999'                        
076300       PERFORM BEA-HAEMTA-DAGENS-PUBKOD                                   
076400     END-IF                                                               
076500                                                                          
076600     IF KEY-KDCATPUB-FOM NOT = LOW-VALUE                                  
076700       IF KEY-KDCATPUB-FOM(1:4) NUMERIC                                   
077000         MOVE KEY-KDCATPUB-FOM(1:4) TO W-TIAAAA                           
077100                                                                          
077700         PERFORM BEB-HAEMTA-NARMASTE-RAETTA-PUB                           
077800         IF INDATA-FEL = JA                                               
077900           MOVE FEL-6 (SPRAAK-IX) TO MOD-TEMFSFEL                         
078000         ELSE                                                             
078100           MOVE KEY-KDCATPUB-FOM (4:3) TO MOD-KDCATPUB-R-FOM-UT           
078200         END-IF                                                           
078300       ELSE                                                               
078400         MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                           
078500         MOVE JA TO INDATA-FEL                                            
078600       END-IF                                                             
078700     ELSE                                                                 
078800       MOVE FEL-1 (SPRAAK-IX) TO MOD-TEMFSFEL                             
078900       MOVE JA TO INDATA-FEL                                              
079000     END-IF                                                               
079100     .                                                                    
079200     EJECT                                                                
079300 BEA-HAEMTA-DAGENS-PUBKOD   SECTION.                                      
079400     SKIP2                                                                
079410     MOVE DAGENS-TIAAAA TO KEY-KDCATPUB-FOM                               
079420*    --- Nu innehåller KDCATPUB ÅÅÅÅBB                                    
079430*    --- Byt BB mot VV                                                    
079800     IF IDAG-DAT-KDSVAR-OK                                                
079900       MOVE IDAG-DAT-TIVV  TO KEY-KDCATPUB-FOM (5:2)                      
080000     ELSE                                                                 
080100       MOVE LOW-VALUE TO KEY-KDCATPUB-FOM                                 
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500 BEB-HAEMTA-NARMASTE-RAETTA-PUB SECTION.                                  
080600     SKIP2                                                                
080700     MOVE NEJ TO INDATA-FEL                                               
080800*  I W-TIAAAA LIGGER ÅRTALET FÖR TESTEN                                   
080900     PERFORM IMS-GET-KATM-TAB                                             
081000     IF SEGMENT-FINNS                                                     
081100       MOVE +1 TO INDX                                                    
081200       PERFORM UNTIL INDX > +12  OR INDATA-FEL = JA                       
081300         IF KEY-KDCATPUB-FOM = KAT-TAB-KDCATPUB-FOM(INDX)                 
081310         AND KAT-TAB-FLVADGEN(INDX) = JA                                  
081400           MOVE +99 TO INDX                                               
081500         ELSE                                                             
081600           IF KEY-KDCATPUB-FOM < KAT-TAB-KDCATPUB-FOM(INDX)               
081700             IF SPAR-TAB-KDCATPUB = SPACE                                 
081800*              FANNS INGEN IÅR                                            
081900               PERFORM BEBA-LAS-SENASTE-GILTIGA-IFJOL                     
082000             ELSE                                                         
082100               MOVE SPAR-TAB-KDCATPUB TO KEY-KDCATPUB-FOM                 
082200               MOVE +77 TO INDX                                           
082300             END-IF                                                       
082400           ELSE                                                           
082500             IF KAT-TAB-FLVADGEN(INDX) = JA                               
082600*              SPARA KDCATPUB OM DET ÄR EN GILTIG GEN-VECKA               
082700               MOVE KAT-TAB-KDCATPUB-FOM(INDX)                            
082710                                       TO SPAR-TAB-KDCATPUB               
082711             ELSE                                                         
082720               IF INDX = +12                                              
082721                 IF SPAR-TAB-KDCATPUB = SPACE                             
082722*                  FANNS INGEN IÅR                                        
082723                   PERFORM BEBA-LAS-SENASTE-GILTIGA-IFJOL                 
082724                 ELSE                                                     
082730                   MOVE SPAR-TAB-KDCATPUB TO KEY-KDCATPUB-FOM             
082740                   MOVE +77 TO INDX                                       
082810                 END-IF                                                   
082820               END-IF                                                     
082900             END-IF                                                       
082910           END-IF                                                         
083000         END-IF                                                           
083100         ADD +1 TO INDX                                                   
083200       END-PERFORM                                                        
083300       IF INDX < +77                                                      
083400         MOVE JA TO INDATA-FEL                                            
083500       END-IF                                                             
083600     ELSE                                                                 
083700       MOVE JA TO INDATA-FEL                                              
083800     END-IF                                                               
083900     .                                                                    
084000     EJECT                                                                
084100 BEBA-LAS-SENASTE-GILTIGA-IFJOL SECTION.                                  
084200     SKIP2                                                                
084300     SUBTRACT +1 FROM W-TIAAAA                                            
084400     PERFORM IMS-GET-KATM-TAB                                             
084500     IF SEGMENT-FINNS                                                     
084600       MOVE +12 TO INDX                                                   
084700                                                                          
084800       PERFORM UNTIL INDX = ZERO OR +88                                   
084900         IF KAT-TAB-FLVADGEN(INDX) = JA                                   
085000*          SPARA KDCATPUB OM DET ÄR EN GILTIG GEN-VECKA                   
085100           MOVE KAT-TAB-KDCATPUB-FOM(INDX) TO KEY-KDCATPUB-FOM            
085200                                              SPAR-TAB-KDCATPUB           
085300           MOVE +89 TO INDX                                               
085400         END-IF                                                           
085500         SUBTRACT +1 FROM INDX                                            
085600       END-PERFORM                                                        
085700                                                                          
085800       IF INDX = ZERO                                                     
085900         MOVE JA TO INDATA-FEL                                            
086000       END-IF                                                             
086100     ELSE                                                                 
086200       MOVE JA TO INDATA-FEL                                              
086300     END-IF                                                               
086400*    ÅTERSTÄLL ÅRTALET                                                    
086500     ADD +1 TO W-TIAAAA                                                   
086600     .                                                                    
086700     EJECT                                                                
086800 C-LAS-RADER SECTION.                                                     
086900     SKIP2                                                                
087000     MOVE 'C' TO SPAR-KOD                                                 
087100     MOVE +1 TO INDX                                                      
087200     MOVE ZERO TO EXTRA-RAD   SPAR-IDCATRAD-1   SPAR-IDCATRAD-7           
087300                  NUVARANDE-IDCATRAD                                      
087400     MOVE SPACE TO UT-BERUBTXT(1)    UT-BERUBTXT(2)                       
087500                   UT-BERUBTXT(3)    UT-BETTEXT                           
087600                   SPAR-KAP-FAELT                                         
087700     MOVE JA TO RAD-FINNS                                                 
087800     MOVE NEJ TO EXTRA-ANMARK                                             
087900                                                                          
088000     PERFORM IMS-GNP-AVS-RAD                                              
088100     IF SEGMENT-SAKNAS                                                    
088200        MOVE NEJ TO RAD-FINNS                                             
088300     ELSE                                                                 
088400        MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-12KY                          
088500        MOVE RAD-IDCATRAD     TO NUVARANDE-IDCATRAD                       
088600                                 MOD-IDCATRAD-ENTER                       
088700        INSPECT MOD-IDCATRAD-ENTER REPLACING LEADING SPACE BY ZERO        
088800     END-IF                                                               
088900     PERFORM UNTIL INDX > +12 OR  RAD-FINNS = NEJ                         
089000        PERFORM CK-BLANKA-TEST-FAELT                                      
089100        IF EXTRA-RAD = ZERO AND EXTRA-ANMARK = NEJ                        
089200           MOVE NUVARANDE-IDCATRAD TO W-IDCATRAD                          
089300           IF RAD-FINNS = JA                                              
089400              PERFORM CL-SPARA-NUVARANDE                                  
089500              MOVE NUVARANDE-IDCATRAD TO MOD-IDCATRAD(INDX)               
089600              MOVE RAD-KDRADST TO MOD-KDRADST(INDX)                       
089700              PERFORM CA-AVS-ART                                          
089800              PERFORM CB-AVS-TEXT                                         
089900              PERFORM CC-LAS-BEART                                        
090000              PERFORM CD-AVS-NOT                                          
090100              PERFORM CE-AVS-RUB                                          
090200              PERFORM CF-AVS-FOT                                          
090300              PERFORM CG-AVS-HAEN-REF                                     
090400           END-IF                                                         
090500           IF EXTRA-RAD NOT = ZERO                                        
090600              IF MOD-BEART(INDX) = SPACE                                  
090700                 PERFORM Q-KOLLA-EXTRA-BEART                              
090800              END-IF                                                      
090900           END-IF                                                         
091000        ELSE                                                              
091100           IF RAD-FINNS = JA                                              
091200              PERFORM CL-SPARA-NUVARANDE                                  
091300           END-IF                                                         
091400           PERFORM CI-NOLLSTALL-RAD                                       
091500           MOVE W-IDCATRAD TO MOD-IDCATRAD(INDX)                          
091600           PERFORM CJ-KOLLA-EXTRA-RAD                                     
091700        END-IF                                                            
091800        ADD +1 TO INDX                                                    
091900        IF EXTRA-RAD = ZERO AND EXTRA-ANMARK = NEJ                        
092000           PERFORM IMS-GNP-AVS-RAD                                        
092100           IF SEGMENT-SAKNAS                                              
092200              MOVE NEJ TO RAD-FINNS                                       
092300           ELSE                                                           
092400              MOVE RAD-IDCATRAD TO NUVARANDE-IDCATRAD                     
092500              MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-12KY                    
092600           END-IF                                                         
092700        ELSE                                                              
092800           IF INDX > 12                                                   
092900              PERFORM IMS-GNP-AVS-RAD                                     
093000              IF SEGMENT-FINNS                                            
093100                 MOVE RAD-IDCATRAD TO NUVARANDE-IDCATRAD                  
093200                 MOVE RAD-KDCATPUB-FOM TO W-KDCATPUB-12KY                 
093300                 MOVE JA TO RAD-FINNS                                     
093400              ELSE                                                        
093500                 MOVE NEJ TO RAD-FINNS                                    
093600              END-IF                                                      
093700           END-IF                                                         
093800        END-IF                                                            
093900     END-PERFORM                                                          
094000     .                                                                    
094100     EJECT                                                                
094200 CA-AVS-ART SECTION.                                                      
094300     SKIP2                                                                
094400     PERFORM IMS-GNP-AVS-ART                                              
094500                                                                          
094600     PERFORM CAA-FLYTTA-AVS-ART                                           
094700                                                                          
094800     IF W-IDARTNR > ZERO                                                  
094900        MOVE W-IDARTNR TO BYTES-IDARTNR                                   
094910        PERFORM CAB-KOLLA-ARTREG                                          
094911                                                                          
095000        IF BYT02-RENOV                                                    
095010*****   --- Här slår EU-MÄRKNING över NS, SP, OP, och P                   
095100           MOVE 'EU'       TO MOD-KDPS(INDX)                              
095200                              TEST-KDPS                                   
095300*          MOVE ZERO       TO MOD-KDERS(INDX)                             
095600        END-IF                                                            
095700     END-IF                                                               
095800     .                                                                    
095900     EJECT                                                                
096000 CAA-FLYTTA-AVS-ART SECTION.                                              
096100     SKIP2                                                                
096200     IF SEGMENT-FINNS                                                     
096300        MOVE ART-KDFBX     TO MOD-KDFBX(INDX)                             
096400                                                                          
096500        IF ART-IDCATPOS NUMERIC                                           
096600        OR ART-IDCATPOS = SPACE                                           
096700          MOVE ART-IDCATPOS TO MOD-IDCATPOS(INDX)                         
096800        ELSE                                                              
096900          IF ART-IDCATPOS(2:2) = SPACE                                    
097000          OR ART-IDCATPOS(2:1) ALPHABETIC                                 
097100            MOVE ART-IDCATPOS TO MOD-IDCATPOS(INDX)(2:2)                  
097200          ELSE                                                            
097300            MOVE ART-IDCATPOS TO MOD-IDCATPOS(INDX)                       
097400          END-IF                                                          
097500        END-IF                                                            
097600        INSPECT MOD-IDCATPOS(INDX)                                        
097700                REPLACING LEADING ZERO BY SPACE                           
097710*                                                                         
097720        IF ART-KDFBX = 'F'                                                
097730          MOVE SPACE TO MOD-IDCATPOS(INDX)                                
097740        ELSE                                                              
097800          IF MOD-IDCATPOS(INDX)  NOT = SPAR-IDCATPOS                      
097900            MOVE MOD-IDCATPOS(INDX) TO SPAR-IDCATPOS                      
098000          ELSE                                                            
098100*           VISA VARJE POS EN GÅNG                                        
098200            MOVE SPACE TO MOD-IDCATPOS(INDX)                              
098300          END-IF                                                          
098310        END-IF                                                            
098400*                                                                         
098500        MOVE ART-IDARTNR   TO MOD-IDARTNR(INDX)                           
098600                              TEST-IDARTNR                                
098700                              W-IDARTNR                                   
098800        PERFORM CAAA-FLYTTA-KVKOL                                         
098900        MOVE ART-KDPS      TO MOD-KDPS(INDX)                              
099000                              TEST-KDPS                                   
099100        MOVE ART-KVPUNKT   TO MOD-KVPUNKT(INDX)                           
099200                              SPAR-KVPUNKT                                
099300        IF ART-IDTTEXNR > ZERO                                            
099400           PERFORM CAAB-LAS-TEXT                                          
099500        END-IF                                                            
099600     ELSE                                                                 
099700        MOVE SPACE         TO MOD-KDFBX(INDX)                             
099800                              MOD-IDCATPOS(INDX)                          
099900                              MOD-KVKOL-GRP(INDX)                         
100000                              MOD-KDPS(INDX)                              
100100                              TEST-KDPS                                   
100200                              UT-BETTEXT                                  
100300        MOVE ZERO          TO MOD-IDARTNR(INDX)                           
100400                              TEST-IDARTNR                                
100500                              W-IDARTNR                                   
100600                              MOD-KVPUNKT(INDX)                           
100700                              SPAR-KVPUNKT                                
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100 CAAA-FLYTTA-KVKOL SECTION.                                               
101200     SKIP2                                                                
101300     MOVE +1 TO KOL-IX                                                    
101400     PERFORM UNTIL KOL-IX = +6                                            
101500       IF ART-KVKOL(KOL-IX) = SPACE                                       
101600         IF ART-KDFBX = 'F'                                               
101700           MOVE SPACE TO MOD-KVKOL(INDX, KOL-IX)                          
101800         ELSE                                                             
101900           MOVE '  -' TO MOD-KVKOL(INDX, KOL-IX)                          
102000         END-IF                                                           
102100       ELSE                                                               
102200*        HÖGER-STÄLLER INFO I KOLUMNFÄLTET,                               
102300*        FÖR ATT SKÄRM-VISNING SKALL MOTSVARA TRYCKT KATALOG              
102400         IF ART-KVKOL(KOL-IX) NUMERIC                                     
102500         OR ART-KVKOL(KOL-IX) = SPACE                                     
102600           MOVE ART-KVKOL(KOL-IX) TO MOD-KVKOL(INDX, KOL-IX)              
102700         ELSE                                                             
102800           IF ART-KVKOL(KOL-IX)(2:2) = SPACE                              
102900             MOVE ART-KVKOL(KOL-IX)(1:1) TO                               
103000                                     MOD-KVKOL(INDX, KOL-IX)(3:1)         
103100           ELSE                                                           
103200             IF ART-KVKOL(KOL-IX)(3:1) = SPACE                            
103300               MOVE ART-KVKOL(KOL-IX)(1:2) TO                             
103400                                     MOD-KVKOL(INDX, KOL-IX)(2:2)         
103410             ELSE                                                         
103411               IF ART-KVKOL(KOL-IX)(1:2) = SPACE                          
103412                 MOVE ART-KVKOL(KOL-IX) TO                                
103413                                     MOD-KVKOL(INDX, KOL-IX)              
103414               ELSE                                                       
103415                 MOVE ART-KVKOL(KOL-IX) TO MOD-KVKOL(INDX, KOL-IX)        
103500               END-IF                                                     
103510             END-IF                                                       
103600           END-IF                                                         
103610           IF MOD-KVKOL(INDX, KOL-IX) = '  .'                             
103611*            --- Blankar ut den adm. kolumn-söknings-punkten              
103612             MOVE SPACE TO MOD-KVKOL(INDX, KOL-IX)                        
103620           END-IF                                                         
103700         END-IF                                                           
103800       END-IF                                                             
103900       ADD +1 TO KOL-IX                                                   
104000     END-PERFORM                                                          
104100     .                                                                    
104200     EJECT                                                                
104300 CAAB-LAS-TEXT SECTION.                                                   
104400     SKIP2                                                                
104500     MOVE ART-IDTTEXNR TO W-IDTTEXNR                                      
104600     PERFORM IMS-GU-TEXT                                                  
104700     IF SEGMENT-FINNS                                                     
104800        MOVE IDSKYLT-WS TO W-IDSKYLT                                      
104900        PERFORM IMS-GNP-TEXT-TEXT                                         
105000        IF SEGMENT-FINNS                                                  
105100           MOVE TEXT-BETTEXT TO UT-BETTEXT                                
105200           MOVE +1 TO EXTRA-RAD                                           
105300        ELSE                                                              
105400           MOVE SPACE TO UT-BETTEXT                                       
105500        END-IF                                                            
105600     ELSE                                                                 
105700        MOVE SPACE TO UT-BETTEXT                                          
105800     END-IF                                                               
105900     .                                                                    
106000     EJECT                                                                
106100 CAB-KOLLA-ARTREG SECTION.                                                
106200     SKIP2                                                                
106300     PERFORM IMS-GU-ARTC01                                                
106400                                                                          
106500     IF SEGMENT-FINNS                                                     
106600       MOVE ARTC01-ART-TIERSDAT TO WS-TIERSDAT-AAVVD                      
106700                                                                          
106800       IF ARTC01-ART-KDERS-UTG > ZERO                                     
106900         MOVE ARTC01-ART-KDERS-UTG TO MOD-KDERS(INDX)                     
107000         EVALUATE TRUE                                                    
107100           WHEN ARTC01-ART-KDERS-UTG = 29                                 
107200              MOVE 'OP' TO MOD-KDPS(INDX) TEST-KDPS                       
107300                                                                          
107400           WHEN ARTC01-ART-KDERS-UTG > 20 AND < 27                        
107500              MOVE 'SP' TO MOD-KDPS(INDX) TEST-KDPS                       
107600                                                                          
107700           WHEN ARTC01-ART-KDERS-UTG > 29                                 
107800              MOVE 'NS' TO MOD-KDPS(INDX) TEST-KDPS                       
107900                                                                          
108000           WHEN OTHER                                                     
108100              CONTINUE                                                    
108200         END-EVALUATE                                                     
108300       ELSE                                                               
108400         PERFORM IMS-GNP-ARTC11                                           
108500                                                                          
108600         IF SEGMENT-FINNS                                                 
109100           EVALUATE TRUE                                                  
109200             WHEN ARTC11-CLAG-KDERS = 29                                  
109300               MOVE 'OP' TO MOD-KDPS(INDX) TEST-KDPS                      
109400               MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(INDX)                  
109500                                                                          
109600             WHEN ARTC11-CLAG-KDERS > 20 AND < 27                         
109700               IF ARTC11-CLAG-KDERS = 21 OR 24                            
109710*                --- Minskar ev. erskoden med 10                          
109800                 PERFORM CABA-KOLLA-TIERSDAT                              
110000               ELSE                                                       
110100                 MOVE       'SP'        TO MOD-KDPS(INDX)                 
110200                 MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(INDX)                
110300               END-IF                                                     
110310               MOVE 'SP' TO TEST-KDPS                                     
110400                                                                          
110500             WHEN ARTC11-CLAG-KDERS > 29                                  
110600               MOVE 'NS' TO MOD-KDPS(INDX) TEST-KDPS                      
110700               MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(INDX)                  
110800                                                                          
110900             WHEN OTHER                                                   
111000               MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(INDX)                  
111100           END-EVALUATE                                                   
111101                                                                          
111110           IF ARTC11-CLAG-FLLSRDEL = NEJ                                  
111120             MOVE 'NS' TO MOD-KDPS(INDX) TEST-KDPS                        
111130           END-IF                                                         
111140*****      --- Här skrivs ev. PASSIV-MÄRKNING                             
111150           IF ARTC11-CLAG-KDUART = 'P'                                    
111151             IF ARTC11-CLAG-KDERS = ZERO                                  
111160               MOVE ARTC11-CLAG-KDUART TO MOD-KDPS(INDX) TEST-KDPS        
111161             END-IF                                                       
111170           END-IF                                                         
111180*                                                                         
111200         ELSE                                                             
111300           MOVE ZERO TO MOD-KDERS(INDX)                                   
111310           MOVE 'NS' TO MOD-KDPS(INDX) TEST-KDPS                          
111400         END-IF                                                           
111500       END-IF                                                             
111600     ELSE                                                                 
111700       MOVE ZERO TO MOD-KDERS(INDX)                                       
111800     END-IF                                                               
111900     .                                                                    
112000     EJECT                                                                
112100 CABA-KOLLA-TIERSDAT SECTION.                                             
112200     SKIP2                                                                
112800     IF IDAG-DAT-KDSVAR-OK                                                
112900       MOVE IDAG-DAT-TIAAVVD TO DAGENS-AAVVD                              
113000                                                                          
113100       MOVE WS-TIERSDAT-AAVV TO W009VADD-DATUM                            
113200       MOVE +32              TO W009VADD-ANTAL                            
113300       CALL W009VADD USING W009VADD-DATUM                                 
113400                           W009VADD-ANTAL                                 
113500       MOVE W009VADD-DATUM   TO WS-TIERSDAT-AAVV                          
113600                                                                          
113601       MOVE WS-TIERSDAT-AAVVD   TO TMP1-YYWWD                             
113602       MOVE DAGENS-AAVVD        TO TMP2-YYWWD                             
113603       PERFORM WY2000P2                                                   
113700       IF TMP1-YYWWD > TMP2-YYWWD                                         
113800          SUBTRACT 10 FROM ARTC11-CLAG-KDERS GIVING WS-KDERS              
113900          MOVE WS-KDERS TO MOD-KDERS(INDX)                                
114000       ELSE                                                               
114100          MOVE 'SP' TO MOD-KDPS(INDX)                                     
114200          MOVE ARTC11-CLAG-KDERS TO MOD-KDERS(INDX)                       
114300       END-IF                                                             
114400     END-IF                                                               
114500     .                                                                    
114600     EJECT                                                                
114700 CB-AVS-TEXT SECTION.                                                     
114800     SKIP2                                                                
114900     PERFORM IMS-GNP-AVS-TEXT                                             
115000     IF SEGMENT-FINNS                                                     
115100        MOVE TEXT-TEKATANM TO MOD-TEKATANM(INDX)                          
115200                              SPAR-TEKATANM                               
115300     ELSE                                                                 
115400        MOVE SPACE         TO MOD-TEKATANM(INDX)                          
115500                              SPAR-TEKATANM                               
115600     END-IF                                                               
115700     .                                                                    
115800     EJECT                                                                
115900 CC-LAS-BEART SECTION.                                                    
116000     SKIP2                                                                
116100     MOVE SPACE TO MOD-BEART(INDX)                                        
116200                                                                          
116300     PERFORM IMS-GNP-AVS-BEN                                              
116400     IF SEGMENT-FINNS                                                     
116500        IF TEST-KDPS = 'XX'                                               
116600           MOVE BEN-BEART TO MOD-BEART(INDX)                              
116700        ELSE                                                              
116800           IF TEST-KDPS = 'LS' OR 'KL' OR 'NS' OR                         
116810                          'KN' OR 'P ' OR SPACE                           
116900              PERFORM CCA-KOLLA-BENREG                                    
117000              IF SEGMENT-SAKNAS                                           
117100                 MOVE BEN-BEART      TO TRUNK-BEART                       
117200                 MOVE FEL-BEART-AREA TO MOD-BEART(INDX)                   
117300              END-IF                                                      
117400           END-IF                                                         
117500        END-IF                                                            
117600     ELSE                                                                 
117700        MOVE IDSKYLT-WS TO W-IDSKYLT                                      
117800        IF TEST-KDPS = SPACE OR 'OP' OR 'SP' OR 'EU'                      
117900                      OR 'SW' OR 'IK' OR 'NS' OR 'P '                     
118000           IF TEST-IDARTNR = ZERO                                         
118100              CONTINUE                                                    
118200           ELSE                                                           
118300              MOVE TEST-IDARTNR TO W-IDARTNR                              
118400              PERFORM IMS-GU-BENB-SEQ                                     
118500              IF SEGMENT-FINNS                                            
118600                 IF IDSKYLT-WS = 'USA'                                    
118700                    MOVE 'GB ' TO W-IDSKYLT                               
118800                    PERFORM IMS-GNP-BEN-TEXT-BSEQ                         
118900                    IF SEGMENT-FINNS                                      
119000                       MOVE BEN-TEXT-BEART TO MOD-BEART(INDX)             
119100                    END-IF                                                
119200                    MOVE 'USA' TO W-IDSKYLT                               
119300                    PERFORM IMS-GNP-BEN-TEXT-BSEQ                         
119400                    IF SEGMENT-FINNS                                      
119500                       IF BEN-TEXT-BEART NOT = SPACE                      
119600                          MOVE BEN-TEXT-BEART TO MOD-BEART(INDX)          
119700                       END-IF                                             
119800                    END-IF                                                
119900                 ELSE                                                     
120000                    PERFORM IMS-GNP-BEN-TEXT-BSEQ                         
120100                    IF SEGMENT-FINNS                                      
120200                       MOVE BEN-TEXT-BEART TO MOD-BEART(INDX)             
120300                    END-IF                                                
120400                 END-IF                                                   
120500              ELSE                                                        
120600                 MOVE MED-4(SPRAAK-IX)  TO MOD-BEART(INDX)                
120700*                KOPPLING MOT BENREG SAKNAS FÖR ARTIKELNR                 
120800              END-IF                                                      
120900           END-IF                                                         
121000        END-IF                                                            
121100     END-IF                                                               
121200     IF MOD-BEART(INDX) NOT = SPACE                                       
121300       PERFORM S04-FIXA-PUNKTINDRAG                                       
121400     END-IF                                                               
121500     .                                                                    
121600     EJECT                                                                
121700 CCA-KOLLA-BENREG SECTION.                                                
121800     SKIP2                                                                
121900     MOVE 'S  ' TO W-IDSKYLT                                              
122000     MOVE BEN-BEART TO W-BEART                                            
122100                                                                          
122200     PERFORM IMS-GU-BENA-SEQ                                              
122300     IF SEGMENT-FINNS                                                     
122400        IF IDSKYLT-WS = 'S  '                                             
122500           MOVE BEN-BEART TO MOD-BEART(INDX)                              
122600        ELSE                                                              
122700           IF BEN-BEN-KDHOMONYM = BEN-KDHOM                               
122800              PERFORM CCAA-LAS-TEXT                                       
122900           ELSE                                                           
123000              PERFORM IMS-GNP-BENA-HOM                                    
123100              IF SEGMENT-FINNS                                            
123200                 PERFORM CCAB-HITTA-RAETT-TEXT                            
123300              ELSE                                                        
123400                 PERFORM IMS-GU-BENA-SEQ                                  
123500                 IF SEGMENT-FINNS                                         
123600                    PERFORM CCAA-LAS-TEXT                                 
123700                 END-IF                                                   
123800              END-IF                                                      
123900           END-IF                                                         
124000        END-IF                                                            
124100     END-IF                                                               
124200     .                                                                    
124300     EJECT                                                                
124400 CCAA-LAS-TEXT SECTION.                                                   
124500     SKIP2                                                                
124600     MOVE IDSKYLT-WS TO W-IDSKYLT                                         
124700                                                                          
124800     PERFORM IMS-GNP-BEN-TEXT-ASEQ                                        
124900     IF SEGMENT-FINNS                                                     
125000        MOVE BEN-TEXT-BEART TO MOD-BEART(INDX)                            
125100     END-IF                                                               
125200     .                                                                    
125300     EJECT                                                                
125400 CCAB-HITTA-RAETT-TEXT SECTION.                                           
125500     SKIP2                                                                
125600     MOVE NEJ TO RAETT-TEXT                                               
125700                                                                          
125800     PERFORM IMS-GN-BENA-SEQ                                              
125900                                                                          
126000     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
126100     OR RAETT-TEXT = JA                                                   
126200        IF BEN-BEN-KDHOMONYM = BEN-KDHOM                                  
126300           MOVE IDSKYLT-WS TO W-IDSKYLT                                   
126400                                                                          
126500           PERFORM IMS-GNP-BEN-TEXT-ASEQ                                  
126600           IF SEGMENT-FINNS                                               
126700              MOVE BEN-TEXT-BEART TO MOD-BEART(INDX)                      
126800           END-IF                                                         
126900           MOVE JA TO RAETT-TEXT                                          
127000        ELSE                                                              
127100           PERFORM IMS-GN-BENA-SEQ                                        
127200        END-IF                                                            
127300     END-PERFORM                                                          
127400     .                                                                    
127500     EJECT                                                                
127600 CD-AVS-NOT SECTION.                                                      
127700     SKIP2                                                                
127800     PERFORM IMS-GNP-AVS-NOT                                              
127900     IF SEGMENT-FINNS                                                     
128000        IF NOT-IDSEGMNR = 1                                               
128100           MOVE 'J'        TO MOD-FLNOTE(INDX)                            
128200           PERFORM IMS-GNP-AVS-NOT                                        
128300                                                                          
128400           IF SEGMENT-FINNS                                               
128500              MOVE '*'     TO MOD-FLH(INDX)                               
128600           ELSE                                                           
128700              MOVE ' '     TO MOD-FLH(INDX)                               
128800           END-IF                                                         
128900                                                                          
129000        ELSE                                                              
129100           MOVE ' '        TO MOD-FLNOTE(INDX)                            
129200           MOVE '*'        TO MOD-FLH(INDX)                               
129300        END-IF                                                            
129400                                                                          
129500     ELSE                                                                 
129600        MOVE ' ' TO MOD-FLNOTE(INDX)                                      
129700        MOVE ' ' TO MOD-FLH(INDX)                                         
129800     END-IF                                                               
129900     .                                                                    
130000     EJECT                                                                
130100 CE-AVS-RUB SECTION.                                                      
130200     SKIP2                                                                
130300     PERFORM IMS-GNP-AVS-RUB                                              
130400     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
130500        MOVE RUB-IDRUBNR TO W-IDRUBNR                                     
130600                                                                          
130700        PERFORM IMS-GU-RUB                                                
130800        IF SEGMENT-FINNS                                                  
130900           MOVE IDSKYLT-WS TO W-IDSKYLT                                   
131000           IF RUB-RUB-FLKOMBINERAS = 'N'                                  
131100              PERFORM IMS-GNP-RUB-TEXT                                    
131200              MOVE +1 TO RAK-IX                                           
131300                                                                          
131400              PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                  
131500                 MOVE RUB-TEXT-BERUBTXT TO UT-BERUBTXT(RAK-IX)            
131600                 ADD +1 TO  EXTRA-RAD  RAK-IX                             
131700                 PERFORM IMS-GNP-RUB-TEXT                                 
131800              END-PERFORM                                                 
131900           ELSE                                                           
132000              PERFORM IMS-GNP-RUB-TEXT                                    
132100              IF SEGMENT-FINNS                                            
132200                 MOVE RUB-TEXT-BERUBTXT TO                                
132300                                     UT-BERUBTXT(RUB-IDSEGMNR)            
132400                 ADD +1 TO EXTRA-RAD                                      
132500              END-IF                                                      
132600           END-IF                                                         
132700        END-IF                                                            
132800        PERFORM IMS-GNP-AVS-RUB                                           
132900     END-PERFORM                                                          
133000     .                                                                    
133100     EJECT                                                                
133200 CF-AVS-FOT SECTION.                                                      
133300     SKIP2                                                                
133400     PERFORM S02-LAES-FOT                                                 
133500                                                                          
133600     IF FOTNOT-RAKNARE NOT = ZERO                                         
133700        PERFORM S03-FLYTTA-FOTNOT                                         
133800        MOVE SPAR-KAP-FAELT TO MOD-TEKATANM(INDX)                         
133900                                                                          
134000        IF SPAR-TEKATANM NOT = SPACE                                      
134100           MOVE JA TO EXTRA-ANMARK                                        
134200        END-IF                                                            
134300     ELSE                                                                 
134400        MOVE SPACE TO SPAR-TEKATANM                                       
134500     END-IF                                                               
134600     .                                                                    
134700     EJECT                                                                
134800 CG-AVS-HAEN-REF SECTION.                                                 
134900     SKIP2                                                                
135000     PERFORM IMS-GNP-AVS-HAEN                                             
135100     IF SEGMENT-FINNS                                                     
135200        MOVE HAEN-KDHAEN    TO MOD-FLH(INDX)                              
135300     END-IF                                                               
135400                                                                          
135410     MOVE W-WDN501-X         TO W-WDN5G1KY-01-X                           
135422     MOVE NUVARANDE-IDCATRAD TO W-IDCATRAD-GSEQ                           
135423     MOVE W-KDCATPUB-12KY    TO W-KDCATPUB-GSEQ                           
135440*    --- WDN5G1KY = SEKUNDÄR-IX    av WDN527-segmenten.                   
135450*    ---            Förekomst indikerar hänvisning från annan rad         
135500     PERFORM IMS-GU-AVS-GSEQ                                              
135600     IF SEGMENT-FINNS                                                     
135700       MOVE 'R'  TO MOD-FLH-REFERENS(INDX)                                
135800     ELSE                                                                 
135900       MOVE ' '  TO MOD-FLH-REFERENS(INDX)                                
136000     END-IF                                                               
136100     .                                                                    
136200     EJECT                                                                
136300 CI-NOLLSTALL-RAD SECTION.                                                
136400     SKIP2                                                                
136500     MOVE SPACE TO MOD-KDFBX(INDX)                                        
136600                   MOD-IDCATPOS(INDX)                                     
136700                   MOD-KVKOL(INDX, 1)                                     
136800                   MOD-KVKOL(INDX, 2)                                     
136900                   MOD-KVKOL(INDX, 3)                                     
137000                   MOD-KVKOL(INDX, 4)                                     
137100                   MOD-KVKOL(INDX, 5)                                     
137200                   MOD-KDPS(INDX)                                         
137300                   MOD-BEART(INDX)                                        
137400                   MOD-TEKATANM(INDX)                                     
137500                   MOD-KDRADST(INDX)                                      
137600                   MOD-FLNOTE(INDX)                                       
137700                   MOD-FLH-REFERENS(INDX)                                 
137800                   MOD-FLH(INDX)                                          
137900     MOVE ZERO TO  MOD-IDARTNR(INDX)                                      
138000                   MOD-KVPUNKT(INDX)                                      
138100                   MOD-KDERS(INDX)                                        
138200     .                                                                    
138300     EJECT                                                                
138400 CJ-KOLLA-EXTRA-RAD SECTION.                                              
138500     SKIP2                                                                
138600     IF SPAR-TEKATANM = SPACE                                             
138700        PERFORM Q-KOLLA-EXTRA-BEART                                       
138800     ELSE                                                                 
138900        MOVE SPAR-TEKATANM TO MOD-TEKATANM(INDX)                          
139000        MOVE SPACE TO SPAR-TEKATANM                                       
139100        MOVE NEJ TO EXTRA-ANMARK                                          
139200        IF (UT-BERUBTXT(1) = SPACE) AND                                   
139300           (UT-BERUBTXT(2) = SPACE) AND                                   
139400           (UT-BERUBTXT(3) = SPACE) AND                                   
139500           (UT-BETTEXT = SPACE)                                           
139600           MOVE SPACE TO MOD-BEART(INDX)                                  
139700           MOVE ZERO TO EXTRA-RAD                                         
139800        ELSE                                                              
139900           PERFORM Q-KOLLA-EXTRA-BEART                                    
140000        END-IF                                                            
140100     END-IF                                                               
140200     .                                                                    
140300     EJECT                                                                
140400 CK-BLANKA-TEST-FAELT SECTION.                                            
140500     SKIP2                                                                
140600     MOVE ZERO  TO TEST-IDARTNR                                           
140700     MOVE SPACE TO TEST-KDPS                                              
140800     .                                                                    
140900     EJECT                                                                
141000 CL-SPARA-NUVARANDE SECTION.                                              
141100     SKIP2                                                                
141200     IF INDX = 1                                                          
141300        MOVE NUVARANDE-IDCATRAD TO SPAR-IDCATRAD-1                        
141400     END-IF                                                               
141500     IF INDX = 7                                                          
141600        MOVE NUVARANDE-IDCATRAD TO SPAR-IDCATRAD-7                        
141700     END-IF                                                               
141800     .                                                                    
141900     EJECT                                                                
142000 D-RENSA-BILD SECTION.                                                    
142100     SKIP2                                                                
142200     IF MFS-IDTRANS = '1511'                                              
142300        MOVE MID-IDCATRAD-NEXT TO MOD-IDCATRAD-NEXT                       
142400        INSPECT MOD-IDCATRAD-NEXT REPLACING LEADING SPACE BY ZERO         
142500     ELSE                                                                 
142600        MOVE   9999 TO MOD-IDCATRAD-NEXT                                  
142700     END-IF                                                               
142800                                                                          
142900     MOVE +1 TO INDX                                                      
143000     PERFORM UNTIL INDX > +5                                              
143100        MOVE MFS-RENSA-FAELT TO MOD-TEKOL(INDX)                           
143200        ADD +1 TO INDX                                                    
143300     END-PERFORM                                                          
143400                                                                          
143500     MOVE MFS-RENSA-FAELT    TO MOD-BERUBTXT(1)                           
143600                                MOD-BERUBTXT(2)                           
143700                                MOD-BERUBTXT(3)                           
143800                                MOD-BERUBTEXT                             
143900                                MOD-IDILLU                                
144000                                MOD-IDVERS                                
144100                                MOD-FLAVSTVAD                             
144200                                MOD-FLAVSUST                              
144300     MOVE +1 TO INDX                                                      
144400     PERFORM UNTIL INDX > +12                                             
144500        MOVE MFS-RENSA-FAELT TO MOD-IDCATRAD(INDX)                        
144600                                MOD-KDFBX(INDX)                           
144700                                MOD-IDCATPOS(INDX)                        
144800                                MOD-IDARTNR(INDX)                         
144900                                MOD-KVKOL-GRP(INDX)                       
145000                                MOD-KDPS(INDX)                            
145100                                MOD-KVPUNKT(INDX)                         
145200                                MOD-BEART(INDX)                           
145300                                MOD-TEKATANM(INDX)                        
145400                                MOD-KDRADST(INDX)                         
145500                                MOD-FLNOTE(INDX)                          
145600                                MOD-KDERS(INDX)                           
145700                                MOD-FLH-REFERENS(INDX)                    
145800                                MOD-FLH(INDX)                             
145900        ADD +1 TO INDX                                                    
146000     END-PERFORM                                                          
146100     .                                                                    
146200     EJECT                                                                
146300 E-RENSA-RADER SECTION.                                                   
146400     SKIP2                                                                
146500     PERFORM UNTIL INDX > +12                                             
146600        MOVE MFS-RENSA-FAELT TO MOD-IDCATRAD(INDX)                        
146700                                MOD-KDFBX(INDX)                           
146800                                MOD-IDCATPOS(INDX)                        
146900                                MOD-IDARTNR(INDX)                         
147000                                MOD-KVKOL-GRP(INDX)                       
147100                                MOD-KDPS(INDX)                            
147200                                MOD-KVPUNKT(INDX)                         
147300                                MOD-BEART(INDX)                           
147400                                MOD-TEKATANM(INDX)                        
147500                                MOD-KDRADST(INDX)                         
147600                                MOD-FLNOTE(INDX)                          
147700                                MOD-KDERS(INDX)                           
147800                                MOD-FLH-REFERENS(INDX)                    
147900                                MOD-FLH(INDX)                             
148000        ADD +1 TO INDX                                                    
148100     END-PERFORM                                                          
148200     .                                                                    
148300     EJECT                                                                
148400 F-FYLL-DOLT-FAELT SECTION.                                               
148500     SKIP2                                                                
148600     IF INDX = 13 AND SEGMENT-FINNS                                       
148700        MOVE RAD-IDCATRAD TO MOD-IDCATRAD-NEXT                            
148800        INSPECT MOD-IDCATRAD-NEXT REPLACING LEADING SPACE BY ZERO         
148900        MOVE MED-2 (SPRAAK-IX) TO MOD-TEMFSINF                            
149000     ELSE                                                                 
149100        MOVE   9999       TO MOD-IDCATRAD-NEXT                            
149200     END-IF                                                               
149300     IF EXTRA-RAD = ZERO                                                  
149400        CONTINUE                                                          
149500     ELSE                                                                 
149600        MOVE MED-3 (SPRAAK-IX) TO MOD-TEMFSINF                            
149700     END-IF                                                               
149800     .                                                                    
149900     EJECT                                                                
150000 Q-KOLLA-EXTRA-BEART SECTION.                                             
150100     SKIP2                                                                
150200     IF (UT-BERUBTXT(1) NOT = SPACE)                                      
150300     OR (UT-BERUBTXT(2) NOT = SPACE)                                      
150400     OR (UT-BERUBTXT(3) NOT = SPACE)                                      
150500                                                                          
150600        MOVE +1 TO RAK-IX                                                 
150700                                                                          
150800        PERFORM UNTIL RAK-IX > +3                                         
150900           IF UT-BERUBTXT(RAK-IX) NOT = SPACE                             
151000              MOVE UT-BERUBTXT(RAK-IX) TO MOD-BEART(INDX)                 
151100              SUBTRACT +1 FROM EXTRA-RAD                                  
151200              MOVE SPACE TO UT-BERUBTXT(RAK-IX)                           
151300              MOVE +4 TO RAK-IX                                           
151400           END-IF                                                         
151500                                                                          
151600           ADD +1 TO RAK-IX                                               
151700                                                                          
151800        END-PERFORM                                                       
151900        PERFORM S04-FIXA-PUNKTINDRAG                                      
152000     ELSE                                                                 
152100        MOVE UT-BETTEXT TO MOD-BEART(INDX)                                
152200        MOVE ZERO  TO EXTRA-RAD                                           
152300        MOVE SPACE TO UT-BETTEXT                                          
152400        PERFORM S05-FIXA-BLANKINDRAG                                      
152500     END-IF                                                               
152600     .                                                                    
152700     EJECT                                                                
152800 S01-FLYTTA-FROM SECTION.                                                 
152900     SKIP2                                                                
153000     IF MFS-IDTRANS = '1511'                                              
153100        IF MFS-NEXT                                                       
153200          IF MID-IDCATRAD-NEXT = 9999                                     
153300             MOVE 20 TO W-IDCATRAD                                        
153400          ELSE                                                            
153500             MOVE MID-IDCATRAD-NEXT TO W-IDCATRAD                         
153600          END-IF                                                          
153700        ELSE                                                              
153800          MOVE MID-IDCATRAD-ENTER   TO W-IDCATRAD                         
153900        END-IF                                                            
154000     END-IF                                                               
154100     .                                                                    
154200     EJECT                                                                
154300 S02-LAES-FOT SECTION.                                                    
154400     SKIP2                                                                
154500     MOVE ZERO TO SPAR-IDFOTNR(1)                                         
154600                  SPAR-IDFOTNR(2)                                         
154700                  SPAR-IDFOTNR(3)                                         
154800                  FOTNOT-RAKNARE                                          
154900                                                                          
155000     PERFORM IMS-GNP-AVS-FOT                                              
155100                                                                          
155200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
155300        MOVE FOT-IDFOTNR TO SPAR-IDFOTNR(FOT-IDSEGMNR)                    
155400        ADD +1 TO FOTNOT-RAKNARE                                          
155500        PERFORM IMS-GNP-AVS-FOT                                           
155600     END-PERFORM                                                          
155700     .                                                                    
155800     EJECT                                                                
155900 S03-FLYTTA-FOTNOT SECTION.                                               
156000     SKIP2                                                                
156100     MOVE +1 TO RAK-IX                                                    
156200     IF FOTNOT-RAKNARE = 1                                                
156300                                                                          
156400        PERFORM UNTIL RAK-IX > +3                                         
156500           IF SPAR-IDFOTNR(RAK-IX) NOT = ZERO                             
156600              MOVE SPAR-IDFOTNR(RAK-IX) TO ENFOT-1                        
156700              MOVE ')' TO ENFOT-TECK-1                                    
156800              MOVE +4 TO RAK-IX                                           
156900           END-IF                                                         
157000                                                                          
157100           ADD +1 TO RAK-IX                                               
157200        END-PERFORM                                                       
157300        PERFORM S031-FLYTTA-ENFOT                                         
157400     ELSE                                                                 
157500        IF FOTNOT-RAKNARE = 2                                             
157600           PERFORM UNTIL RAK-IX > +3                                      
157700              IF SPAR-IDFOTNR(RAK-IX) NOT = ZERO                          
157800                 IF FOTNOT-RAKNARE = 2                                    
157900                    MOVE SPAR-IDFOTNR(RAK-IX) TO TVAFOT-1                 
158000                    MOVE ')' TO TVAFOT-TECK-1                             
158100                 ELSE                                                     
158200                    MOVE SPAR-IDFOTNR(RAK-IX) TO TVAFOT-2                 
158300                    MOVE ')' TO TVAFOT-TECK-2                             
158400                 END-IF                                                   
158500                 SUBTRACT 1 FROM FOTNOT-RAKNARE                           
158600              END-IF                                                      
158700              ADD +1 TO RAK-IX                                            
158800           END-PERFORM                                                    
158900           PERFORM S032-FLYTTA-TVAFOT                                     
159000        ELSE                                                              
159100           PERFORM UNTIL RAK-IX > +3                                      
159200              IF SPAR-IDFOTNR(RAK-IX) NOT = ZERO                          
159300                 IF FOTNOT-RAKNARE = 3                                    
159400                    MOVE SPAR-IDFOTNR(RAK-IX) TO TREFOT-1                 
159500                    MOVE ')' TO TREFOT-TECK-1                             
159600                 ELSE                                                     
159700                    IF FOTNOT-RAKNARE = 2                                 
159800                       MOVE SPAR-IDFOTNR(RAK-IX) TO TREFOT-2              
159900                       MOVE ')' TO TREFOT-TECK-2                          
160000                    ELSE                                                  
160100                       MOVE SPAR-IDFOTNR(RAK-IX) TO TREFOT-3              
160200                       MOVE ')' TO TREFOT-TECK-3                          
160300                    END-IF                                                
160400                 END-IF                                                   
160500                 SUBTRACT 1 FROM FOTNOT-RAKNARE                           
160600              END-IF                                                      
160700              ADD +1 TO RAK-IX                                            
160800           END-PERFORM                                                    
160900           PERFORM S033-FLYTTA-TREFOT                                     
161000        END-IF                                                            
161100     END-IF                                                               
161200     .                                                                    
161300     EJECT                                                                
161400 S031-FLYTTA-ENFOT SECTION.                                               
161500     SKIP2                                                                
161600     EVALUATE SPAR-KOD                                                    
161700         WHEN 'A'                                                         
161800             MOVE SPACE TO ENFOT-SPACE                                    
161900         WHEN 'B'                                                         
162000             MOVE SPAR-TEKOL TO ENFOT-SPACE                               
162100         WHEN 'C'                                                         
162200             MOVE SPACE TO ENFOT-SPACE                                    
162300     END-EVALUATE                                                         
162400     .                                                                    
162500     EJECT                                                                
162600 S032-FLYTTA-TVAFOT SECTION.                                              
162700     SKIP2                                                                
162800     EVALUATE SPAR-KOD                                                    
162900         WHEN 'A'                                                         
163000             MOVE SPACE TO TVAFOT-SPACE                                   
163100         WHEN 'B'                                                         
163200             MOVE SPAR-TEKOL TO TVAFOT-SPACE                              
163300         WHEN 'C'                                                         
163400             MOVE SPACE TO TVAFOT-SPACE                                   
163500     END-EVALUATE                                                         
163600     .                                                                    
163700     EJECT                                                                
163800 S033-FLYTTA-TREFOT SECTION.                                              
163900     SKIP2                                                                
164000     EVALUATE SPAR-KOD                                                    
164100         WHEN 'A'                                                         
164200             MOVE SPACE TO TREFOT-SPACE                                   
164300         WHEN 'B'                                                         
164400             MOVE SPAR-TEKOL TO TREFOT-SPACE                              
164500         WHEN 'C'                                                         
164600             MOVE SPACE TO TREFOT-SPACE                                   
164700     END-EVALUATE                                                         
164800     .                                                                    
164900     EJECT                                                                
165000 S04-FIXA-PUNKTINDRAG SECTION.                                            
165100     SKIP2                                                                
165110*    --- Fixar att benämningstext visas som i katalogen.                  
165120     IF TEST-KDPS = 'XX'                                                  
165121       CONTINUE                                                           
165122*      --- Inskriven TEXT skall visas som den är.                         
165130     ELSE                                                                 
165200       MOVE FUNCTION UPPER-CASE(MOD-BEART(INDX)(1:1))                     
165300       TO MOD-BEART(INDX)(1:1)                                            
165400       MOVE MOD-BEART(INDX) TO RTXT-TETEXTTR                              
165500       MOVE 'G'           TO RTXT-KDTEXTTR                                
165600       CALL WTXTTR USING RTXT-WTXTAREA                                    
165700       MOVE RTXT-TETEXTTR TO MOD-BEART(INDX)                              
165710     END-IF                                                               
165800                                                                          
165900     IF SPAR-KVPUNKT NOT = ZERO                                           
166000        MOVE MOD-BEART(INDX) TO SPAR-BEART                                
166100        IF SPAR-KVPUNKT = 1                                               
166200           STRING '. ' SPAR-BEART DELIMITED BY SIZE                       
166300           INTO MOD-BEART(INDX)                                           
166400        END-IF                                                            
166500        IF SPAR-KVPUNKT = 2                                               
166600           STRING '.. ' SPAR-BEART DELIMITED BY SIZE                      
166700           INTO MOD-BEART(INDX)                                           
166800        END-IF                                                            
166900        IF SPAR-KVPUNKT = 3                                               
167000           STRING '... ' SPAR-BEART DELIMITED BY SIZE                     
167100           INTO MOD-BEART(INDX)                                           
167200        END-IF                                                            
167300        IF SPAR-KVPUNKT = 4                                               
167400           STRING '.... ' SPAR-BEART DELIMITED BY SIZE                    
167500           INTO MOD-BEART(INDX)                                           
167600        END-IF                                                            
167700     END-IF                                                               
167800     .                                                                    
167900     EJECT                                                                
168000 S05-FIXA-BLANKINDRAG SECTION.                                            
168100     SKIP2                                                                
168200     IF SPAR-KVPUNKT NOT = ZERO                                           
168300        MOVE MOD-BEART(INDX) TO SPAR-BEART                                
168400        IF SPAR-KVPUNKT = 1                                               
168500           STRING '  ' SPAR-BEART DELIMITED BY SIZE                       
168600           INTO MOD-BEART(INDX)                                           
168700        END-IF                                                            
168800        IF SPAR-KVPUNKT = 2                                               
168900           STRING '   ' SPAR-BEART DELIMITED BY SIZE                      
169000           INTO MOD-BEART(INDX)                                           
169100        END-IF                                                            
169200        IF SPAR-KVPUNKT = 3                                               
169300           STRING '    ' SPAR-BEART DELIMITED BY SIZE                     
169400           INTO MOD-BEART(INDX)                                           
169500        END-IF                                                            
169600        IF SPAR-KVPUNKT = 4                                               
169700           STRING '     ' SPAR-BEART DELIMITED BY SIZE                    
169800           INTO MOD-BEART(INDX)                                           
169900        END-IF                                                            
170000     END-IF                                                               
170100     .                                                                    
170200     EJECT                                                                
170201*S50-Y2K-KDCATPUB-R LIGGER I                                              
170203*COPYTEXT W.PROD.COBOL.W150Y2K1                                           
170204*                                                                         
170205*     -COPY W150Y2K1                                                      
170220     EJECT                                                                
170300* IMS SEKTIONER                                                           
170400     SKIP1                                                                
170500 IMS-GET-MSG SECTION.                                                     
170600     MOVE '  QC' TO GODK-STATUSKODER                                      
170700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
170800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
170900     PERFORM IMS-STATUSKONTROLL                                           
171000     SKIP3                                                                
171100     .                                                                    
171200 IMS-INSERT-MSG SECTION.                                                  
171300     IF ENGLISH-TEXT                                                      
171400         MOVE 'N' TO MFS-KDHUVOMR                                         
171500     END-IF                                                               
171600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
171700     MOVE SPACE TO GODK-STATUSKODER                                       
171800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
171900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
172000     PERFORM IMS-STATUSKONTROLL                                           
172100     .                                                                    
172200     EJECT                                                                
172300 IMS-GU-AVS SECTION.                                                      
172400     STRING 'WLKATH01(WDN501KY =' W-WDN501-X ')'                          
172500            DELIMITED BY SIZE INTO SSA1                                   
172600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
172700     CALL CBLTDLI USING GU AVS-PCB IO-AREA-1 SSA1                         
172800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
172900     PERFORM IMS-STATUSKONTROLL                                           
173000     SKIP2                                                                
173100     .                                                                    
173110 IMS-GU-AVS-ASEQ SECTION.                                                 
173120     STRING 'WDN5A1  (WDN5A1KY >' W-WDN5A1-X ')'                          
173130            DELIMITED BY SIZE INTO SSA1                                   
173140     MOVE '  GEGB' TO GODK-STATUSKODER                                    
173150     CALL CBLTDLI USING GU WDN5A-PCB WDN5A-AREA SSA1                      
173160     MOVE WDN5A-STATUS-CODE TO STATUS-WS                                  
173170     PERFORM IMS-STATUSKONTROLL                                           
173180     SKIP2                                                                
173190     .                                                                    
173200 IMS-GNP-AVS-ILLU SECTION.                                                
173410     MOVE   'WLKATH11  '   TO SSA1                                        
173500     MOVE '  GE' TO GODK-STATUSKODER                                      
173600     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
173700     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
173800     PERFORM IMS-STATUSKONTROLL                                           
173900     .                                                                    
174000     EJECT                                                                
174100 IMS-GNP-AVS-RAD-KVAL SECTION.                                            
174200     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
174300            DELIMITED BY SIZE INTO SSA1                                   
174400     MOVE '  GE' TO GODK-STATUSKODER                                      
174500     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
174600     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
174700     PERFORM IMS-STATUSKONTROLL                                           
174800     SKIP2                                                                
174900     .                                                                    
174910 IMS-GNP-FIRST-AVS-RAD SECTION.                                           
174920     SKIP1                                                                
174930     STRING 'WLKATH12*F(WDN512KY>=' W-WDN512KY-X ')'                      
174940            DELIMITED BY SIZE INTO SSA1                                   
174950     MOVE '  GBGE'  TO GODK-STATUSKODER                                   
174960     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
174970     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
174980     PERFORM IMS-STATUSKONTROLL                                           
174990     .                                                                    
174991     SKIP3                                                                
174992 IMS-GNP-NEXT-AVS-RAD SECTION.                                            
174993     SKIP1                                                                
174994     STRING 'WLKATH12*F(WDN512KY >' W-WDN512KY-X ')'                      
174995            DELIMITED BY SIZE INTO SSA1                                   
174996     MOVE '  GBGE'  TO GODK-STATUSKODER                                   
174997     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
174998     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
174999     PERFORM IMS-STATUSKONTROLL                                           
175000     .                                                                    
175001     SKIP3                                                                
175010 IMS-GNP-AVS-RAD SECTION.                                                 
175100     STRING 'WLKATH12(IDCATRAD=>' W-IDCATRAD-X                            
175200                 OCH 'KDCATPUF=<' W-KDCATPUB-GAELLANDE                    
175300                 OCH 'KDCATPUT=>' W-KDCATPUB-GAELLANDE ')'                
175400                                                                          
175500            DELIMITED BY SIZE INTO SSA1                                   
175600     MOVE '  GE' TO GODK-STATUSKODER                                      
175700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1                        
175800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
175900     PERFORM IMS-STATUSKONTROLL                                           
176000     .                                                                    
176100     EJECT                                                                
176200 IMS-GNP-AVS-ART SECTION.                                                 
176300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
176400            DELIMITED BY SIZE INTO SSA1                                   
176500     MOVE 'WLKATH21 ' TO SSA2                                             
176600     MOVE '  GE' TO GODK-STATUSKODER                                      
176700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
176800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
176900     PERFORM IMS-STATUSKONTROLL                                           
177000     .                                                                    
177100     EJECT                                                                
177200 IMS-GNP-AVS-TEXT SECTION.                                                
177300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
177400            DELIMITED BY SIZE INTO SSA1                                   
177500     MOVE 'WLKATH22 ' TO SSA2                                             
177600     MOVE '  GE' TO GODK-STATUSKODER                                      
177700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
177800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
177900     PERFORM IMS-STATUSKONTROLL                                           
178000     .                                                                    
178100     EJECT                                                                
178200 IMS-GNP-AVS-BEN SECTION.                                                 
178300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
178400            DELIMITED BY SIZE INTO SSA1                                   
178500     MOVE 'WLKATH23 ' TO SSA2                                             
178600     MOVE '  GE' TO GODK-STATUSKODER                                      
178700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
178800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
178900     PERFORM IMS-STATUSKONTROLL                                           
179000     .                                                                    
179100     EJECT                                                                
179200 IMS-GNP-AVS-NOT SECTION.                                                 
179300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
179400            DELIMITED BY SIZE INTO SSA1                                   
179500     MOVE 'WLKATH24 ' TO SSA2                                             
179600     MOVE '  GE' TO GODK-STATUSKODER                                      
179700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
179800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
179900     PERFORM IMS-STATUSKONTROLL                                           
180000     .                                                                    
180100     EJECT                                                                
180200 IMS-GNP-AVS-RUB SECTION.                                                 
180300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
180400            DELIMITED BY SIZE INTO SSA1                                   
180500     MOVE 'WLKATH25 ' TO SSA2                                             
180600     MOVE '  GE' TO GODK-STATUSKODER                                      
180700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
180800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
180900     PERFORM IMS-STATUSKONTROLL                                           
181000     .                                                                    
181100     EJECT                                                                
181200 IMS-GNP-AVS-FOT SECTION.                                                 
181300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
181400            DELIMITED BY SIZE INTO SSA1                                   
181500     MOVE 'WLKATH26 ' TO SSA2                                             
181600     MOVE '  GE' TO GODK-STATUSKODER                                      
181700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
181800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
181900     PERFORM IMS-STATUSKONTROLL                                           
182000     .                                                                    
182100     EJECT                                                                
182200 IMS-GNP-AVS-HAEN SECTION.                                                
182300     STRING 'WLKATH12(WDN512KY =' W-WDN512KY-X ')'                        
182400            DELIMITED BY SIZE INTO SSA1                                   
182500     MOVE 'WLKATH27 ' TO SSA2                                             
182600     MOVE '  GE' TO GODK-STATUSKODER                                      
182700     CALL CBLTDLI USING GNP AVS-PCB IO-AREA-1 SSA1 SSA2                   
182800     MOVE AVS-STATUS-CODE TO STATUS-WS                                    
182900     PERFORM IMS-STATUSKONTROLL                                           
183000     .                                                                    
183100     EJECT                                                                
183200 IMS-GU-AVS-GSEQ SECTION.                                                 
183300     STRING 'WLKATS01(WDN5G1KY>=' W-WDN5G1KY-X                            
183301                                  W-IDCATRKY-LO                           
183310                 OCH 'WDN5G1KY<=' W-WDN5G1KY-X                            
183320                                  W-IDCATRKY-HI ')'                       
183400            DELIMITED BY SIZE INTO SSA1                                   
183600     MOVE '  GE' TO GODK-STATUSKODER                                      
183700     CALL CBLTDLI USING GU KATS-PCB IO-AREA-G SSA1                        
183800     MOVE KATS-STATUS-CODE TO STATUS-WS                                   
183900     PERFORM IMS-STATUSKONTROLL                                           
184000     .                                                                    
184191     EJECT                                                                
184200 IMS-GU-ARTC01 SECTION.                                                   
184300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
184400            DELIMITED BY SIZE INTO SSA1                                   
184500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
184600     CALL CBLTDLI USING GU ARTC-PCB IO-AREA-2 SSA1                        
184700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
184800     PERFORM IMS-STATUSKONTROLL                                           
184900     SKIP2                                                                
185000     .                                                                    
185100 IMS-GNP-ARTC11 SECTION.                                                  
185200     MOVE 'WLARTC11 ' TO SSA1                                             
185300     MOVE '  GE' TO GODK-STATUSKODER                                      
185400     CALL CBLTDLI USING GNP ARTC-PCB IO-AREA-2 SSA1                       
185500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
185600     PERFORM IMS-STATUSKONTROLL                                           
185700     .                                                                    
185800     EJECT                                                                
185900 IMS-GU-RUB SECTION.                                                      
186000     STRING 'WLKATB01(IDRUBNR  =' W-IDRUBNR-X ')'                         
186100            DELIMITED BY SIZE INTO SSA1                                   
186200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
186300     CALL CBLTDLI USING GU RUB-PCB IO-AREA-2 SSA1                         
186400     MOVE RUB-STATUS-CODE TO STATUS-WS                                    
186500     PERFORM IMS-STATUSKONTROLL                                           
186600     SKIP2                                                                
186700     .                                                                    
186800 IMS-GNP-RUB-TEXT SECTION.                                                
186900     STRING 'WLKATB11(IDSKYLT  =' W-IDSKYLT-X ')'                         
187000            DELIMITED BY SIZE INTO SSA1                                   
187100     MOVE '  GE' TO GODK-STATUSKODER                                      
187200     CALL CBLTDLI USING GNP RUB-PCB IO-AREA-2 SSA1                        
187300     MOVE RUB-STATUS-CODE TO STATUS-WS                                    
187400     PERFORM IMS-STATUSKONTROLL                                           
187500     .                                                                    
187600     EJECT                                                                
187700 IMS-GU-TEXT SECTION.                                                     
187800     STRING 'WLKATD01(IDTTEXNR =' W-IDTTEXNR-X ')'                        
187900            DELIMITED BY SIZE INTO SSA1                                   
188000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
188100     CALL CBLTDLI USING GU TEXT-PCB IO-AREA-2 SSA1                        
188200     MOVE TEXT-STATUS-CODE TO STATUS-WS                                   
188300     PERFORM IMS-STATUSKONTROLL                                           
188400     SKIP2                                                                
188500     .                                                                    
188600 IMS-GNP-TEXT-TEXT SECTION.                                               
188700     STRING 'WLKATD11(IDSKYLT  =' W-IDSKYLT-X ')'                         
188800            DELIMITED BY SIZE INTO SSA1                                   
188900     MOVE '  GE' TO GODK-STATUSKODER                                      
189000     CALL CBLTDLI USING GNP TEXT-PCB IO-AREA-2 SSA1                       
189100     MOVE TEXT-STATUS-CODE TO STATUS-WS                                   
189200     PERFORM IMS-STATUSKONTROLL                                           
189300     .                                                                    
189400     EJECT                                                                
189500 IMS-GU-BENB-SEQ SECTION.                                                 
189600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
189700            DELIMITED BY SIZE INTO SSA1                                   
189800     MOVE '  GE' TO GODK-STATUSKODER                                      
189900     CALL CBLTDLI USING GU BENB-PCB IO-AREA-2 SSA1                        
190000     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
190100     PERFORM IMS-STATUSKONTROLL                                           
190200     SKIP2                                                                
190300     .                                                                    
190400 IMS-GNP-BEN-TEXT-BSEQ SECTION.                                           
190500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
190600            DELIMITED BY SIZE INTO SSA1                                   
190700     MOVE '  GE' TO GODK-STATUSKODER                                      
190800     CALL CBLTDLI USING GNP BENB-PCB IO-AREA-2 SSA1                       
190900     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
191000     PERFORM IMS-STATUSKONTROLL                                           
191100     .                                                                    
191200     EJECT                                                                
191300 IMS-GU-BENA-SEQ SECTION.                                                 
191400     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X W-BEART-X ')'               
191500            DELIMITED BY SIZE INTO SSA1                                   
191600     MOVE '  GE' TO GODK-STATUSKODER                                      
191700     CALL CBLTDLI USING GU BENA-PCB IO-AREA-2 SSA1                        
191800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
191900     PERFORM IMS-STATUSKONTROLL                                           
192000     SKIP2                                                                
192100     .                                                                    
192200 IMS-GNP-BEN-TEXT-ASEQ SECTION.                                           
192300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
192400            DELIMITED BY SIZE INTO SSA1                                   
192500     MOVE '  GE' TO GODK-STATUSKODER                                      
192600     CALL CBLTDLI USING GNP BENA-PCB IO-AREA-2 SSA1                       
192700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
192800     PERFORM IMS-STATUSKONTROLL                                           
192900     SKIP2                                                                
193000     .                                                                    
193100 IMS-GN-BENA-SEQ SECTION.                                                 
193200     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X W-BEART-X ')'               
193300            DELIMITED BY SIZE INTO SSA1                                   
193400     MOVE '  GE' TO GODK-STATUSKODER                                      
193500     CALL CBLTDLI USING GN BENA-PCB IO-AREA-2 SSA1                        
193600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
193700     PERFORM IMS-STATUSKONTROLL                                           
193800     SKIP2                                                                
193900     .                                                                    
194000 IMS-GNP-BENA-HOM SECTION.                                                
194100     MOVE 'WLBENA13 ' TO SSA1                                             
194200     MOVE '  GE' TO GODK-STATUSKODER                                      
194300     CALL CBLTDLI USING GNP BENA-PCB IO-AREA-2 SSA1                       
194400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
194500     PERFORM IMS-STATUSKONTROLL                                           
194600     .                                                                    
194700     EJECT                                                                
194800 IMS-GET-KATM-TAB SECTION.                                                
194900     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-1-X ')'                       
195000                  DELIMITED BY SIZE INTO SSA1                             
195100     STRING 'WLKATM11(TIAAAA   =' W-TIAAAA-X ')'                          
195200                  DELIMITED BY SIZE INTO SSA2                             
195300     MOVE   '  GE' TO GODK-STATUSKODER                                    
195400     CALL CBLTDLI USING GU KATM-PCB IO-AREA-2 SSA1 SSA2                   
195500     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
195600     PERFORM IMS-STATUSKONTROLL                                           
195700     .                                                                    
195800     EJECT                                                                
195900 IMS-STATUSKONTROLL SECTION.                                              
196000     SET STATUS-IX TO 1                                                   
196100     SEARCH GODK-STATUS AT END CALL FELLOG                                
196200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
196300     END-SEARCH                                                           
196400     .                                                                    
196410     EJECT                                                                
196500*    -COPY WY2000P2                                                       
