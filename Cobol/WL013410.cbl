000100 PROCESS DYNAM                                                            
000200*                                                                         
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     WL013410.                                                
000500 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000600 DATE-WRITTEN.   MAJ  2006.                                               
000700 DATE-COMPILED.                                                           
000800                                                                          
000800                                                                          
000900*    FUNKTION.                                                            
001000*        REDIGERING OCH UTSKRIFT AV PLOCKETIKETTER OCH                    
001100*        PACKUNDERLAG.                                                    
001200*        UTSKRIFT MED HJÄLP AV D&P                                        
001300*                                                                         
001400*        DETTA SUBPROGRAM UPPDATERAR HÄNDELSEBASER FÖR                    
001500*        ETIKETTER OCH UNDERLAG PER ORDERDEL                              
001600*                                                                         
001700*    LÄNKAREA :       WL013410                                            
001800*                                                                         
001900*    CHANGE LOG                                                           
002000*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
002100*    E-TRACKER: 10143273 2012-09  LOCAL SOURCING CHINA                    
002200*    E-TRACKER: 10130993 2015-04-22                                       
002300*               REDUCE NUMBER OF DELIVERY SCHEDULES                       
002400*    E-TRACKER: 10254592 2015       DECOMISSION VOHF                      
002500*                                                                         
002600*    2523176 - PRINT PICKING ROUND-WMS                                    
002700*                                                                         
002800 DATA DIVISION.                                                           
002900                                                                          
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                       PIC X(8)    VALUE 'WL013410'.            
003300 77  FILLER                      PIC X(8)    VALUE 'ERRORTEX'.            
003400 01  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003500 01  FILLER REDEFINES ERROR-TEXT.                                         
003600     03 ERROR-TEXT-1-36          PIC X(36).                               
003700     03 ERROR-TEXT-37-80         PIC X(44).                               
003800                                                                          
003900 77  FILLER                      PIC X(8)    VALUE 'CURRENT'.             
004000 77  WS-CURRENT-SECTION          PIC X(32)   VALUE SPACE.                 
004100 77  WS-CURRENT-IMS-SECTION      PIC X(24)   VALUE SPACE.                 
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  YES                         PIC X       VALUE 'Y'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  DEF-IDROLL                  PIC X(5)    VALUE 'VOR99'.               
004700                                                                          
004800*01 -COPY WWDCKONS                                                        
004900                                                                          
005000*----> TABELL FÖR ATT ÖVERSÄTTA HF-AK-PLOCK                               
005100*   -COPY W413WHFA                                                        
005200                                                                          
005300 77  WS-DIRLEV-PRC               PIC X(4)    VALUE '9998'.                
005400                                                                          
005500 77  ODEL-IX                     PIC S9(9)   VALUE +0.                    
005600 77  IX                          PIC S9(9)   VALUE +0.                    
005700 77  IX-MAX                      PIC S9(9)   VALUE +7.                    
005800 77  IX1                         PIC S9(9)   VALUE +0.                    
005900 77  ORAD-IX                     PIC S9(9)   VALUE +0.                    
006000 77  MAX-ORAD                    PIC S9(9)   VALUE +1100.                 
006100 77  OUTPUT-MSG-IX               PIC S9(9)   VALUE +0.                    
006200 77  MAX-ANT-OUTPUT-MSG          PIC S9(9)   VALUE +45.                   
006300 77  IX-CD-OMR                   PIC S9(9)   VALUE ZERO  COMP-3.          
006400 77  TECKEN-IX                   PIC S9(9)   VALUE +0.                    
006500 77  HFAK-TAB-IX                 PIC S9(9)   VALUE +0.                    
006600 77  ORDERDELS-IX                PIC  9(9)   VALUE ZERO.                  
006700 77  WS-KDMFSFOR                 PIC  9(1)   VALUE ZERO.                  
006800 77  IDMSGVER                    PIC  9(3)  VALUE 001.                    
006900 77  ADD-1-HEKTO                 PIC S9(6)V9(1) VALUE 00000.1.            
007000                                                                          
007100 77  WS-KVSEMBRA                 PIC S9(3)      COMP-3.                   
007200 77  WS-IDRADNR-SISTA            PIC S9(5)      COMP-3.                   
007300 77  WS-ADLAGOMR                 PIC S9(3)      COMP-3.                   
007400 77  WS-ADGANG                   PIC S9(3)      COMP-3.                   
007500 77  WS-ADPLATS                  PIC S9(5)      COMP-3.                   
007600 77  WS-HFAK-REF-X10             PIC X(10).                               
007700 77  WS-KDARTHNT                 PIC S9(7)      COMP-3.                   
007800 77  WS-KDARTURS                 PIC X(2).                                
007900 77  WS-IDPSN                    PIC 9(3) VALUE ZERO.                     
008000 77  WS-ORDERVARDE-PER-RAD       PIC S9(7)V9(2) COMP-3.                   
008100 77  WS-ORDERVARDE-PER-RAD-LOC   PIC S9(7)V9(2) COMP-3.                   
008200 77  WS-ORDERVARDE-PER-RAD-LOCPREL                                        
008300                                 PIC S9(7)V9(2) COMP-3.                   
008400 77  WS-PRC-KVRADER              PIC S9(5)   COMP-3 VALUE 0.              
008500 77  WS-PRC-VKORDNTO             PIC S9(6)V9(3) COMP-3.                   
008600 77  WS-PRC-VLORDNTO             PIC S9(4)V9(3) COMP-3.                   
008700 77  WS-PRC-RESPLIT              PIC S9(1)V9(2) COMP-3.                   
008800 77  WS-SPLIT-IDARTNR            PIC S9(9)      COMP-3 VALUE 0.           
008900 77  WS-KDEMBAL                  PIC X(1).                                
009000 77  WS-REGDATUM-AADDD           PIC 9(5).                                
009100 77  WS-DATUM-9KOMPL             PIC 9(8).                                
009200 77  WS-DATUM-Y2K                PIC 9(8).                                
009300 77  WS-ANTAL-DAGAR              PIC 9(5).                                
009400 77  TMP1-YYDDD                  PIC S9(5)   COMP-3 VALUE 0.              
009500 77  TMP2-YYDDD                  PIC S9(5)   COMP-3 VALUE 0.              
009600 77  WS-KDRAPRIO                 PIC S9(3)   COMP-3.                      
009700 77  WS-KDORDSTA-CX              PIC X(2)  VALUE SPACE.                   
009800 77  WS-KVROS                    PIC S9(7)   COMP-3.                      
009900 77  WS-KVLS                     PIC S9(7)   COMP-3.                      
010000 77  WS-KVEFRS                   PIC S9(7)   COMP-3.                      
010100 77  WS-KVRESS                   PIC S9(7)   COMP-3.                      
010200 77  WS-KART-KVRESS-ART          PIC S9(7)   COMP-3.                      
010300 77  WS-KVAVBART                 PIC S9(7)   COMP-3.                      
010400 77  WS-ANTOBKR                  PIC S9(3)   COMP-3.                      
010500 77  WS-IDARTNR                  PIC  9(9).                               
010600 77  WS-BEART                    PIC X(25).                               
010700 77  WS-IDPGM                    PIC X(8).                                
010800 77  WS-KDORDBEK                 PIC 9(2).                                
010900     88  VOR-KON                 VALUE 51 52 53 54 55 57 67 92.           
011000 77  WS-KDROO                    PIC S9(1)   COMP-3.                      
011100 77  WS-IDKONTO                  PIC 9(11).                               
011200 77  WS-IDDISTR-NUM4             PIC 9(4).                                
011300 77  WS-IDDISTR-NUM5             PIC 9(5).                                
011400 77  WS-IDKUNDNR-NUM6            PIC 9(6).                                
011500 77  S28-IDARTNR                 PIC 9(9)    VALUE ZERO COMP-3.           
011600 77  S28-KVVORKO                 PIC S9(7)   VALUE ZERO COMP-3.           
011700 77  S28-IDANSK                  PIC 9(3)    VALUE ZERO COMP-3.           
011800 77  S28-IDLEVNR                 PIC X(5)    VALUE SPACE.                 
011900 77  WS-VOR-TID-BRIST            PIC 9(9)    VALUE ZERO.                  
012000 77  WS-IDPRQUES                 PIC S9(7)   VALUE ZERO.                  
012100*VOLUME                                                                   
012200 77  WS-ORDERVOLYM-PER-RAD       PIC S9(4)V9(3) COMP-3.                   
012300 77  WS-KVRADER              PIC S9(5)       VALUE ZERO  COMP-3.          
012400 77  WS-VKORDNTO             PIC S9(6)V9(3)  VALUE ZERO  COMP-3.          
012500 77  WS-VLORDNTO             PIC S9(4)V9(3)  VALUE ZERO  COMP-3.          
012600 77  WS-IDPRODNR             PIC S9(7)       VALUE ZERO  COMP-3.          
012700*                                                                         
012800 77 WS-SPAR-IDORDER          PIC  9(7).                                   
012900 77 WS-OHUV-IDORDER          PIC S9(7)   VALUE ZERO  COMP-3.              
013000 77 WS-OHUV-IDUSER           PIC  X(8)   VALUE SPACE.                     
013100*WEIGHT                                                                   
013200 77 WS-ORDERVIKT-PER-RAD     PIC S9(6)V9(3) COMP-3.                       
013300 77 WS-ORDERVIKT-PER-ORDER   PIC S9(6)V9(3)  VALUE ZERO  COMP-3.          
013400*                                                                         
013500 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
013600 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
013700                                                                          
013800 77  WS-IDSKYLT-GB             PIC X(3)       VALUE 'GB '.                
013900 77  WS-IDSKYLT                PIC X(3).                                  
014000      88  UTF8-IDSKYLT                        VALUE 'CZ '                 
014100                                                    'GR '                 
014200                                                    'H  '                 
014300                                                    'IR '                 
014400                                                    'J  '                 
014500                                                    'KOR'                 
014600                                                    'PL '                 
014700                                                    'RC '                 
014800                                                    'RCN'                 
014900                                                    'RO '                 
015000                                                    'RUS'                 
015100                                                    'T  '                 
015200                                                    'TR '                 
015300                                                    'YU '.                
015400                                                                          
015500 77  FILLER                      PIC X(08)   VALUE 'DAGENS  '.            
015600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
015700 01  FILLER REDEFINES DAGENS-DATUM.                                       
015800     03  DAGENS-AA               PIC 9(2).                                
015900     03  DAGENS-MM               PIC 9(2).                                
016000     03  DAGENS-DD               PIC 9(2).                                
016100                                                                          
016200 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
016300                                                                          
016400 01      WS-KLOCKAN.                                                      
016500   03    WS-TIHHMMSS             PIC 9(6).                                
016600   03    FILLER                  PIC X(2).                                
016700                                                                          
016800 01      WS-KLOCKAN-LOK.                                                  
016900   03    WS-TIHHMMSS-LOK         PIC 9(6).                                
017000   03    FILLER                  PIC X(2).                                
017100                                                                          
017200 77      WS-DATUM-LOK            PIC 9(6).                                
017300                                                                          
017400 01  WS-IDUSER.                                                           
017500   03    FILLER                  PIC X(3)    VALUE SPACES.                
017600   03    WS-IDANSTNR             PIC X(5)    VALUE SPACES.                
017700                                                                          
017800 01  FILLER                      PIC X(16)   VALUE 'VKORDNTO-TAB'.        
017900*L138                                                                     
018000 01 ACKUMULERA-VIKT-PER-ORDER-TAB.                                        
018100    03 VKORDNTO-PER-ORDER        OCCURS 100.                              
018200       05 WS-SPAR-VKORDNTO-PER-ORDER  PIC S9(6)V9(3) VALUE ZERO.          
018300                                                                          
018400 77  FILLER                      PIC X(08)   VALUE 'VVVVVVVV'.            
018500                                                                          
018600 01  S27-IDANSK-X.                                                        
018700     03 S27-IDANSK               PIC 9(3).                                
018800                                                                          
018900 01  S27-IDARTNR-X.                                                       
019000     03 S27-IDARTNR              PIC 9(9).                                
019100                                                                          
019200 01  S27-IDDISTR-X.                                                       
019300     03 S27-IDDISTR              PIC 9(4).                                
019400                                                                          
019500 01  S27-IDKUNDNR-X.                                                      
019600     03 S27-IDKUNDNR             PIC 9(6).                                
019700                                                                          
019800*                                                                         
019900 77  WS-PARTNER                  PIC X(1)    VALUE SPACE.                 
020000*                                                                         
020100                                                                          
020200 01  WS-IDSYSTEM.                                                         
020300     03 WS-IDSYST-1-3            PIC X(3)    VALUE SPACE.                 
020400     03 WS-IDSYST-4              PIC X(1)    VALUE SPACE.                 
020500                                                                          
020600 01  W-IDDISTR-P4-X.                                                      
020700     03 W-IDDISTR-P4             PIC S9(5)   VALUE ZERO  COMP-3.          
020800                                                                          
020900 01 DB2-LASNING.                                                          
021000     03 FILLER                   PIC X(16)   VALUE                        
021100                                             'WS-DB2-SEKTION'.            
021200     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
021300                                                                          
021400     EJECT                                                                
021500 01 NYCKLAR-TP4TRAN.                                                      
021600     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
021700                                                                          
021800*LDC-GB                                                                   
021900 01  INITIERA-LDC-TABELL         PIC X       VALUE SPACE.                 
022000 01  LDC-TAB-IX                  PIC S9(3)   COMP-3.                      
022100 01  LDC-TAB-IX-MAX              PIC S9(3)   COMP-3 VALUE +100.           
022200 01  LDC-TABELL.                                                          
022300     03 LDC-TAB-WIP-ORDER        OCCURS 100.                              
022400        05 LDC-TAB-WIPID         PIC X(10)   VALUE SPACE.                 
022500*                                                                         
022600 01  WS-LOPNR-WIPID.                                                      
022700     03 WS-WIP-LOPNR             PIC  9(2).                               
022800     03 WS-WIP-STRECK            PIC  X(1)  VALUE '-'.                    
022900     03 WS-WIPID                 PIC  X(7).                               
023000                                                                          
023100 77  SW-AENDRA-LAGOMR            PIC X       VALUE 'N'.                   
023200     88  AENDRA-LAGOMR                       VALUE 'J'.                   
023300                                                                          
023400*77  PLOCKSATS-SW                PIC X.                                   
023500*    88  PLOCKSATS-OK                        VALUE 'J'.                   
023600*    88  PLOCKSATS-FEL                       VALUE 'N'.                   
023700                                                                          
023800 77  PLOCKSATS-ETIK-SW           PIC X       VALUE 'N'.                   
023900     88  PLOCKSATS-ETIK-FINNS                VALUE 'J'.                   
024000     88  PLOCKSATS-ETIK-SAKNAS               VALUE 'N'.                   
024100                                                                          
024200 77  PLOCKSATS-PU-SW             PIC X       VALUE 'N'.                   
024300     88  PLOCKSATS-PU-FINNS                  VALUE 'J'.                   
024400     88  PLOCKSATS-PU-SAKNAS                 VALUE 'N'.                   
024500                                                                          
024600 77  ALLT-SW                     PIC X.                                   
024700     88  ALLT-OK                             VALUE 'J'.                   
024800     88  ALLT-FEL                            VALUE 'N'.                   
024900                                                                          
025000 77  ODEL-SW                     PIC X.                                   
025100     88  NY-ORDERDEL                         VALUE 'J'.                   
025200                                                                          
025300 77  BIPA-SPARR-SW               PIC X.                                   
025400     88  BIPA-SPARR                          VALUE 'J'.                   
025500                                                                          
025600 77  PRC-SW                      PIC X.                                   
025700     88  PRC-EJ-OK                           VALUE 'N'.                   
025800     88  PRC-OK                              VALUE 'J'.                   
025900                                                                          
026000 77  LAGOMR-SW                   PIC X.                                   
026100     88  NYTT-LAGEROMRADE                    VALUE 'J'.                   
026200                                                                          
026300 77  ARTIKEL-SW                  PIC X.                                   
026400     88  ARTIKEL-OK                          VALUE 'J'.                   
026500     88  ARTIKEL-FEL                         VALUE 'N'.                   
026600                                                                          
026700 77  ORAD-SW                     PIC X.                                   
026800     88  WDQ4-RAD                            VALUE '2'.                   
026900                                                                          
027000 77  TRAFF-VORKO-SW              PIC X(1)    VALUE 'N'.                   
027100   88 TRAFF-VORKO                            VALUE 'J'.                   
027200                                                                          
027300 77  PLOCK-SW                    PIC X.                                   
027400     88  PLOCKSATS-KLAR                      VALUE 'J'.                   
027500                                                                          
027600 77  BEVARREF-I-HFAK-TAB-SW      PIC X       VALUE 'N'.                   
027700     88  BEVARREF-I-HFAK-TAB                 VALUE 'J'.                   
027800                                                                          
027900 77  FORSTA-GANGEN-SW            PIC X       VALUE 'J'.                   
028000                                                                          
028100 77  K722-SW                     PIC X.                                   
028200     88  K722-FINNS                          VALUE 'J'.                   
028300     88  K722-SAKNAS                         VALUE 'N'.                   
028400*                                                                         
028500*    PARAMETRAR FÖR EVENT                                                 
028600 77  EVENT-SW                     PIC X(4)   VALUE SPACE.                 
028700     88 EVENT-OK                             VALUE 'LYNB'                 
028800                                                   'LYNV'                 
028900                                                   'LYND'                 
029000                                                   'LYNK'                 
029100                                                   'TADB'                 
029200                                                   'TADV'                 
029300                                                   'TADD'                 
029400                                                   'TAD '                 
029500                                                   'POLE'                 
029600                                                   'ACC '                 
029700                                                   'APA '                 
029800                                                   'APB '                 
029900                                                   'APC '                 
030000                                                   'APD '                 
030100                                                   'APE '                 
030200                                                   'APF '                 
030300                                                   'APG '                 
030400                                                   'APH '                 
030500                                                   'API '                 
030600                                                   'APJ '                 
030700                                                   'ECOM'.                
030800                                                                          
030900 77  CREATE-EVENT-SW              PIC X(1)   VALUE 'N'.                   
031000     88 CREATE-EVENT                         VALUE 'J'.                   
031100*                                                                         
031110 77  SW-LYNK-NON-API              PIC X      VALUE 'N'.                   
031120     88  LYNK-NON-API                        VALUE 'J'.                   
031130*                                                                         
031140 77  SW-VOR                       PIC X      VALUE 'N'.                   
031150     88  VOR                                 VALUE 'J'.                   
031160*                                                                         
031200 01  WS-IDEVENTORDREF.                                                    
031300     03 WS-IDDISTR-EVENT         PIC 9(4).                                
031400     03 WS-IDKUNDNR-EVENT        PIC 9(6).                                
031500     03 WS-IDORDNR7-EVENT        PIC 9(7).                                
031600     03 WS-TIREGDAT-EVENT        PIC 9(6).                                
031700*                                                                         
031800***************************                                               
031900*  ARBETSAREA ORDERDELEN  *                                               
032000***************************                                               
032100 01  W-ORDERDEL.                                                          
032200*    03  -COPY WDQ301     -PRE W-                                         
032300                                                                          
032400******************************                                            
032500*  SPAR-AREA PLOCKSATS ETIK  *                                            
032600******************************                                            
032700 01  W-ETIK-PLOCKSATS.                                                    
032800*    03  -COPY WDGX4004   -PRE W-ETIK-                                    
032900                                                                          
033000*01  TAB-ORDDELAR.                                                        
033100*    03  TAB-ORDDEL              OCCURS 99.                               
033200*        05  TAB-IDORDER         PIC S9(7) COMP-3.                        
033300*        05  TAB-IDDC            PIC X(2).                                
033400*        05  TAB-IDPRODNR        PIC S9(7) COMP-3.                        
033500*        05  TAB-IDPLKLST        PIC S9(3) COMP-3.                        
033600                                                                          
033700*************************                                                 
033800*  ARBETSAREA RYE-TRANS *                                                 
033900*************************                                                 
034000 01  W-RYEPOST.                                                           
034100*    03  -COPY WDGZRYE    -PRE W-                                         
034200     EJECT                                                                
034300 01  W-SORTPOST.                                                          
034400*    03  -COPY WDGZRYES   -PRE W-                                         
034500                                                                          
034600*************************                                                 
034700*  ARBETSAREA RYK-TRANS *                                                 
034800*************************                                                 
034900 01  W-RYKPOST.                                                           
035000*    03  -COPY WDGZRYK    -PRE W-                                         
035100                                                                          
035200*************************                                                 
035300*  ARBETSAREA RYX-TRANS *                                                 
035400*************************                                                 
035500 01  W-RYXPOST.                                                           
035600*    03  -COPY WDGZRYX    -PRE W-                                         
035700     EJECT                                                                
035800 01  W-RYX-SORTPOST.                                                      
035900*    03  -COPY WDGZRYXS   -PRE W-                                         
036000                                                                          
036100 01  FILLER                      PIC X(16)   VALUE 'SKROTDISTR'.          
036200*   -COPY WWDIST18                                                        
036300 01  FILLER                      PIC X(16)  VALUE 'REFILLDISTR'.          
036400*   -COPY WWDIST35                                                        
036500 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
036600*   -COPY WWDIST57                                                        
036700                                                                          
036800 01  TEST-IDDISTR                PIC 9(5)  VALUE ZERO COMP-3.             
036900*    ----DIST20                                                           
037000*01  FILLER   -COPY WWDIST20     -RED TEST-IDDISTR.                       
037100                                                                          
037200*    ----DIST34                                                           
037300*01  FILLER   -COPY WWDIST34     -RED TEST-IDDISTR.                       
037400                                                                          
037500*    ----DIST79-DEALER-PRICE----                                          
037600*01  FILLER   -COPY WWDIST79     -RED TEST-IDDISTR.                       
037700                                                                          
037800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
037900 01  GENERAL-SUBPROGRAMS.                                                 
038000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
038100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
038200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
038300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
038400     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
038500     03  W335PRNO                PIC X(8)    VALUE 'W335PRNO'.            
038600     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
038700     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
038800     03  W411AREG                PIC X(8)    VALUE 'W411AREG'.            
038900     03  W411DEAV                PIC X(8)    VALUE 'W411DEAV'.            
039000     03  W411SPAR                PIC X(8)    VALUE 'W411SPAR'.            
039100     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
039200     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
039300     03  W411RANS                PIC X(8)    VALUE 'W411RANS'.            
039400     03  W413ADRS                PIC X(8)    VALUE 'W413ADRS'.            
039500     03  W411LAST                PIC X(8)    VALUE 'W411LAST'.            
039600                                                                          
039700 01  FILLER                      PIC X(16)   VALUE 'ABEND    '.           
039800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
039900 77  RKOD-ABEND-WITHOUT-DUMP     PIC S9(4) COMP SYNC VALUE   +16.         
040000 77  RKOD-FELTEXT                PIC X(32) VALUE SPACE.                   
040100                                                                          
040200 01  FILLER                      PIC X(16)   VALUE 'WL01TIDZ '.           
040300*01  -COPY WL01TIDZ                                                       
040400*                                                                         
040500 01  FILLER                      PIC X(16)   VALUE 'WDATKONV '.           
040600*   -COPY WDATAREA                                                        
040700 01  FILLER                      PIC X(16)  VALUE 'W335PRNO '.            
040800*   -COPY W335PRNO                                                        
040900 01  FILLER                      PIC X(16)  VALUE 'W335PRQU '.            
041000*   -COPY W335PRQU                                                        
041100 01  FILLER                      PIC X(16)  VALUE 'W403PLAT '.            
041200*   -COPY W403PLAT                                                        
041300 01  FILLER                      PIC X(16)  VALUE 'W411AREG '.            
041400*   -COPY W411AREG                                                        
041500 01  FILLER                      PIC X(16)  VALUE 'W411DEAV '.            
041600*   -COPY W411DEAV                                                        
041700 01  FILLER                      PIC X(16)  VALUE 'W411SPAR '.            
041800*   -COPY W411SPAR                                                        
041900 01  FILLER                      PIC X(16)  VALUE 'WTRAUTF8 '.            
042000*   -COPY WTRAUTF8                                                        
042100 01  FILLER                      PIC X(16)  VALUE 'W411RANS '.            
042200*   -COPY W411RANS                                                        
042300 01  FILLER                      PIC X(16)  VALUE 'W413ADRS '.            
042400*   -COPY W413ADRS                                                        
042500 01  FILLER                      PIC X(16)  VALUE 'W411LAST '.            
042600*   -COPY W411LAST                                                        
042700                                                                          
042800                                                                          
042900*01  MEDDELANDE.                                                          
043000*  03  PICKING-UNIT-ON-PRINTER-QUEUE       PIC X(03) VALUE '131'.         
043100*  03  NON-COMPLETED-PICKING-UNIT-ON       PIC X(03) VALUE '132'.         
043200                                                                          
043300 01  NYCKLAR-TILL-DLI.                                                    
043400                                                                          
043500     03  W-4003-ETIK-IDHTYP-X.                                            
043600         05  W-4003-ETIK-IDHTYP   PIC  X(04) VALUE '4003'.                
043700         05  W-4003-ETIK-IDPRODNR PIC  9(07).                             
043800         05  W-4003-ETIK-IDPLKLST PIC  9(03).                             
043900         05  FILLER               PIC  X(16) VALUE LOW-VALUE.             
044000                                                                          
044100     03  W-4007-PU-IDHTYP-X.                                              
044200         05  W-4007-PU-IDHTYP     PIC  X(04) VALUE '4007'.                
044300         05  W-4007-PU-IDPRODNR   PIC  9(07).                             
044400         05  W-4007-PU-IDPLKLST   PIC  9(03).                             
044500         05  FILLER               PIC  X(16) VALUE LOW-VALUE.             
044600                                                                          
044700     03  W-4017-IDHTYP-X.                                                 
044800         05  W-4017-IDHTYP       PIC  X(4)  VALUE '4017'.                 
044900         05  W-4017-IDPRODNR     PIC  9(7).                               
045000         05  W-4017-IDPLKLST     PIC  9(3).                               
045100         05  W-4017-LOW-VALUE    PIC  X(16) VALUE LOW-VALUE.              
045200                                                                          
045300     03  W-4018-KDSEGKEY-X.                                               
045400         05  W-4018-KDSEGKEY     PIC   X      VALUE '1'.                  
045500                                                                          
045600     03  W-4447-IDHTYP-X.                                                 
045700         05  W-4447-IDHTYP       PIC  X(04) VALUE '4447'.                 
045800         05  W-4447-IDDC         PIC  X(02).                              
045900         05  W-4447-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
046000                                                                          
046100     03  W-4448-IDPRC-X.                                                  
046200         05  W-4448-IDPRC        PIC  X(04).                              
046300         05  W-4448-LOW-VALUE    PIC  X(01) VALUE LOW-VALUE.              
046400                                                                          
046500     03  W-4453-IDHTYP-X.                                                 
046600         05  W-4453-IDHTYP       PIC  X(04) VALUE '4453'.                 
046700         05  W-4453-IDDC         PIC  X(02).                              
046800         05  W-4453-IDPRC        PIC  X(04).                              
046900         05  W-4453-LOW-VALUE    PIC  X(20) VALUE LOW-VALUE.              
047000                                                                          
047100*    03  W-4454-KDSEGKEY-X       PIC  X(01) VALUE '1'.                    
047200*                                                                         
047300     03  W-4511-IDHTYP-X.                                                 
047400         05  W-4511-IDHTYP       PIC  X(04) VALUE '4511'.                 
047500         05  W-LOW-VALUE         PIC  X(26) VALUE LOW-VALUE.              
047600                                                                          
047700     03  W-4512-KDTPOTYP-X.                                               
047800         05  W-4512-KDTPOTYP     PIC  S9(1) COMP-3.                       
047900     03  W-4512-KDORDKL-X.                                                
048000         05  W-4512-KDORDKL      PIC  S9(1) COMP-3.                       
048100     03  W-4512-IDDISTR-FOM-X.                                            
048200         05  W-4512-IDDISTR-FOM  PIC  S9(5) COMP-3.                       
048300     03  W-4512-IDDISTR-TOM-X.                                            
048400         05  W-4512-IDDISTR-TOM  PIC  S9(5) COMP-3.                       
048500                                                                          
048600     03  W-WDM201-X.                                                      
048700         05  W-KAMP-IDKAMPRF     PIC S9(07)   VALUE ZERO COMP-3.          
048800         05  W-KAMP-IDDC         PIC X(02)    VALUE SPACE.                
048900                                                                          
049000     03  W-WDM211-X.                                                      
049100         05  W-KART-IDARTNR      PIC S9(09)   VALUE ZERO COMP-3.          
049200                                                                          
049300     03  W-WDM221-X.                                                      
049400         05  W-KMRK-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.           
049500         05  W-KMRK-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.           
049600         05  W-KMRK-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.           
049700         05  W-KMRK-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.           
049800                                                                          
049900     03  W-4535-IDHTYP-X.                                                 
050000         05  W-4535-IDHTYP        PIC  X(4)  VALUE '4535'.                
050100         05  W-4535-KDFDKRAV      PIC S9(3)  COMP-3.                      
050200         05  W-4535-LOW-VALUE     PIC  X(24) VALUE LOW-VALUE.             
050300                                                                          
050400     03  W-4536-IDSKYLT-X.                                                
050500         05  W-4536-IDSKYLT       PIC  X(3).                              
050600         05  W-4536-LOW-VALUE     PIC  X(2)  VALUE LOW-VALUE.             
050700                                                                          
050800     03  W-WDD3BSEQ-X.                                                    
050900         05  W-D3BSEQ-IDARTNR    PIC  S9(9) COMP-3.                       
051000                                                                          
051100     03  W-IDORDER-X.                                                     
051200         05  W-IDORDER            PIC S9(7)  COMP-3.                      
051300                                                                          
051400     03  W-IDSKYLT-X.                                                     
051500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
051600                                                                          
051700     03  W-IDLAND-X.                                                      
051800         05  W-IDLAND             PIC X(2).                               
051900                                                                          
052000     03  W-IDDC-X.                                                        
052100         05  W-IDDC               PIC X(2).                               
052200                                                                          
052300     03  W-IDPRC-X.                                                       
052400         05  W-IDPRC              PIC X(4).                               
052500                                                                          
052600     03  W-IDDC-B6-X.                                                     
052700         05  W-IDDC-B6            PIC X(2).                               
052800                                                                          
052900     03  W-IDGMT-X.                                                       
053000         05  W-IDDISTR-WDB2       PIC S9(5) VALUE ZERO COMP-3.            
053100         05  W-IDKUNDNR-WDB2      PIC S9(7) VALUE ZERO COMP-3.            
053200                                                                          
053300     03  W-ART-IDARTNR-X.                                                 
053400         05  W-ART-IDARTNR        PIC  S9(9) COMP-3.                      
053500                                                                          
053600     03  W-IDARTNR-X.                                                     
053700         05  W-IDARTNR            PIC  S9(9) COMP-3.                      
053800                                                                          
053900     03  W-KDODELST               PIC X.                                  
054000                                                                          
054100     03  W-WDA601KY-MIN-X.                                                
054200         05  W-A601KY-MIN-IDDISTR      PIC S9(5) VALUE +0 COMP-3.         
054300         05  W-A601KY-MIN-IDKUNDNR     PIC S9(7) VALUE +0 COMP-3.         
054400         05  W-A601KY-MIN-IDKUNDRF     PIC X(10) VALUE SPACE.             
054500         05  W-A601KY-MIN-TIREGDAT     PIC S9(7) VALUE +0 COMP-3.         
054600         05  W-A601KY-MIN-IDARTNR      PIC S9(9) VALUE +0 COMP-3.         
054700         05  W-A601KY-MIN-TIREGTID     PIC S9(9) VALUE +0 COMP-3.         
054800         05  W-A601KY-MIN-TIREGDAT-AVV PIC S9(7) VALUE +0 COMP-3.         
054900         05  W-A601KY-MIN-TIREGTID-AVV PIC S9(9) VALUE +0 COMP-3.         
055000                                                                          
055100     03  W-WDA601KY-MAX-X.                                                
055200         05  W-A601KY-MAX-IDDISTR      PIC S9(5) VALUE +0 COMP-3.         
055300         05  W-A601KY-MAX-IDKUNDNR     PIC S9(7) VALUE +0 COMP-3.         
055400         05  W-A601KY-MAX-IDKUNDRF     PIC X(10) VALUE SPACE.             
055500         05  W-A601KY-MAX-TIREGDAT     PIC S9(7) VALUE +0 COMP-3.         
055600         05  W-A601KY-MAX-IDARTNR      PIC S9(9) VALUE +0 COMP-3.         
055700         05  W-A601KY-MAX-TIREGTID     PIC S9(9) VALUE +0 COMP-3.         
055800         05  W-A601KY-MAX-TIREGDAT-AVV PIC S9(7) VALUE +0 COMP-3.         
055900         05  W-A601KY-MAX-TIREGTID-AVV PIC S9(9) VALUE +0 COMP-3.         
056000                                                                          
056100     03  W-WDK611-KDSEGKEY-X.                                             
056200         05  W-WDK611-KDSEGKEY         PIC  X    VALUE '1'.               
056300                                                                          
056400     03  W-WDQ101KY-MIN-X.                                                
056500         05  W-Q101KY-MIN-IDORDER  PIC  S9(7) COMP-3.                     
056600         05  W-Q101KY-MIN-IDARTNR  PIC  S9(9) COMP-3.                     
056700         05  W-Q101KY-MIN-IDLOPNR  PIC  S9(3) COMP-3.                     
056800         05  W-Q101KY-MIN-IDSEKVNR PIC  S9(3) COMP-3.                     
056900         05  W-Q101KY-MIN-FILLER   PIC   X(4) VALUE LOW-VALUE.            
057000                                                                          
057100     03  W-WDQ101KY-MAX-X.                                                
057200         05  W-Q101KY-MAX-IDORDER  PIC  S9(7) COMP-3.                     
057300         05  W-Q101KY-MAX-IDARTNR  PIC  S9(9) COMP-3.                     
057400         05  W-Q101KY-MAX-IDLOPNR  PIC  S9(3) COMP-3.                     
057500         05  W-Q101KY-MAX-IDSEKVNR PIC  S9(3) COMP-3.                     
057600         05  W-Q101KY-MAX-FILLER   PIC   X(4) VALUE HIGH-VALUE.           
057700                                                                          
057800*----> SEKUNDÄR INDEX C TILL ORDERHUVUD.                                  
057900     03  W-WDQ2CSEQ-X.                                                    
058000       05  W-WDQ2CSEQ-IDGMTREF.                                           
058100         07  W-WDQ2CSEQ-IDDISTR    PIC S9(5) COMP-3 VALUE +0.             
058200         07  W-WDQ2CSEQ-IDKUNDNR   PIC S9(7) COMP-3 VALUE +0.             
058300         07  W-WDQ2CSEQ-IDKUNDRF   PIC X(10)        VALUE SPACE.          
058400*                                                                         
058500   03  W-WDQ2CSEQ.                                                        
058600     05  W-IDDISTR-CSEQ          PIC S9(5)   VALUE ZERO COMP-3.           
058700     05  W-IDKUNDNR-CSEQ         PIC S9(7)   VALUE ZERO COMP-3.           
058800     05  FILLER                  PIC 9(2)    VALUE ZERO.                  
058900     05  W-IDORDNR5-CSEQ         PIC 9(5).                                
059000     05  FILLER                  PIC X(3)    VALUE SPACE.                 
059100*                                                                         
059200                                                                          
059300     03  W-WDQ301KY-X.                                                    
059400         05  W-Q301KY-IDORDER     PIC S9(7)  COMP-3.                      
059500         05  W-Q301KY-IDDC        PIC X(2).                               
059600         05  W-Q301KY-IDPRODNR    PIC S9(7)  COMP-3.                      
059700         05  W-Q301KY-IDPLKLST    PIC S9(3)  COMP-3.                      
059800                                                                          
059900     03  W-WDQ301KY-MIN.                                                  
060000         05  W-Q301KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
060100         05  W-Q301KY-MIN-IDDC       PIC X(2).                            
060200         05  FILLER                  PIC X(6)   VALUE LOW-VALUE.          
060300                                                                          
060400     03  W-WDQ301KY-MAX.                                                  
060500         05  W-Q301KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
060600         05  W-Q301KY-MAX-IDDC       PIC X(2).                            
060700         05  FILLER                  PIC X(6)   VALUE HIGH-VALUE.         
060800                                                                          
060900     03  W-WDQ401KY-MIN-X.                                                
061000         05  W-Q401KY-MIN-IDORDER    PIC S9(7)  COMP-3.                   
061100         05  W-Q401KY-MIN-IDDC       PIC X(2).                            
061200         05  W-Q401KY-MIN-ADLAGOMR   PIC S9(3)  COMP-3.                   
061300         05  W-Q401KY-MIN-ADGANG     PIC S9(3)  COMP-3.                   
061400         05  W-Q401KY-MIN-ADPLATS    PIC S9(5)  COMP-3.                   
061500         05  W-Q401KY-MIN-IDARTNR    PIC S9(9)  COMP-3.                   
061600         05  W-Q401KY-MIN-IDLOPNR    PIC S9(3)  COMP-3.                   
061700                                                                          
061800     03  W-WDQ401KY-MAX-X.                                                
061900         05  W-Q401KY-MAX-IDORDER    PIC S9(7)  COMP-3.                   
062000         05  W-Q401KY-MAX-IDDC       PIC X(2).                            
062100         05  W-Q401KY-MAX-ADLAGOMR   PIC S9(3)  COMP-3.                   
062200         05  W-Q401KY-MAX-ADGANG     PIC S9(3)  COMP-3.                   
062300         05  W-Q401KY-MAX-ADPLATS    PIC S9(5)  COMP-3.                   
062400         05  W-Q401KY-MAX-IDARTNR    PIC S9(9)  COMP-3.                   
062500         05  W-Q401KY-MAX-IDLOPNR    PIC S9(3)  COMP-3.                   
062600*                                                                         
062700   03    W-WDA6BSEQ-MIN-X.                                                
062800     05    W-A6BSEQ-MIN-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
062900     05    W-A6BSEQ-MIN-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
063000     05    W-A6BSEQ-MIN-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
063100     05    W-A6BSEQ-MIN-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
063200     05    W-A6BSEQ-MIN-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
063300     05    FILLER                    PIC X(14) VALUE SPACE.               
063400     SKIP2                                                                
063500   03    W-WDA6BSEQ-MAX-X.                                                
063600     05    W-A6BSEQ-MAX-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.         
063700     05    W-A6BSEQ-MAX-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.         
063800     05    W-A6BSEQ-MAX-IDKUNDRF-LEV PIC X(10) VALUE SPACE.               
063900     05    W-A6BSEQ-MAX-TIREGDAT-LEV PIC S9(7) VALUE ZERO COMP-3.         
064000     05    W-A6BSEQ-MAX-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.         
064100     05    FILLER                    PIC X(14) VALUE SPACE.               
064200     SKIP2                                                                
064300                                                                          
064400*----> RESTORDER                                                          
064500*                                                                         
064600     03  W-WDA501KY-X.                                                    
064700         05  W-A501KY-IDDISTR     PIC  S9(5) COMP-3.                      
064800         05  W-A501KY-IDKUNDNR    PIC  S9(7) COMP-3.                      
064900         05  W-A501KY-IDKUNDRF    PIC  X(10).                             
065000         05  W-A501KY-IDARTNR     PIC  S9(9) COMP-3.                      
065100         05  W-A501KY-IDLOPNR     PIC  S9(3) COMP-3.                      
065200*                                                                         
065300   03  W-WDA501KY-A5-MIN-X.                                               
065400       05  W-IDDISTR-A5-MIN          PIC S9(5) VALUE ZERO COMP-3.         
065500       05  W-IDKUNDNR-A5-MIN         PIC S9(7) VALUE ZERO COMP-3.         
065600       05  FILLER                    PIC X(17) VALUE LOW-VALUE.           
065700                                                                          
065800   03  W-WDA501KY-A5-MAX-X.                                               
065900       05  W-IDDISTR-A5-MAX          PIC S9(5) VALUE ZERO COMP-3.         
066000       05  W-IDKUNDNR-A5-MAX         PIC S9(7) VALUE ZERO COMP-3.         
066100       05  FILLER                    PIC X(17) VALUE HIGH-VALUE.          
066200*                                                                         
066300                                                                          
066400 01  STATUS-WS-Q221              PIC XX.                                  
066500     88  Q221-SEG-SAKNAS         VALUE 'GE'.                              
066600                                                                          
066700 01  STATUS-WS                   PIC XX.                                  
066800     88  SEGMENT-FINNS           VALUE '  '.                              
066900     88  ISRT-OK                 VALUE '  '.                              
067000     88  SEGMENT-FINNS-REDAN     VALUE 'II'.                              
067100     88  SEGMENT-SAKNAS          VALUE 'GE'.                              
067200     88  SEGMENT-SLUT            VALUE 'GB'.                              
067300                                                                          
067400 01  GODK-STATUSKODER.                                                    
067500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
067600                                                                          
067700 01  ALL-SSA.                                                             
067800     03 SSA1                     PIC X(192).                              
067900     03 SSA2                     PIC X(64).                               
068000     03 SSA3                     PIC X(64).                               
068100                                                                          
068200*    --- IMS FUNKTIONSKODER                                               
068300*01  -COPY W0003                                                          
068400*                                                                         
068500*    --- DB2 FUNKTIONSKODER                                               
068600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
068700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
068800                                                                          
068900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
069000 01  DB2-WS.                                                              
069100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
069200         88  CURSOR-OK                       VALUE 000.                   
069300         88  RADER-FINNS                     VALUE 000.                   
069400         88  RADER-SAKNAS                    VALUE 100.                   
069500         88  ATKOMST-FEL                     VALUE 904.                   
069600     03  GODK-SQLCODEKODER.                                               
069700         05  GODK-SQLCODE OCCURS 5                                        
069800             INDEXED BY SQLCODE-IX PIC 9(3).                              
069900 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
070000     EJECT                                                                
070100                                                                          
070200 01  FILLER               PIC X(16)   VALUE 'WDGX4003'.                   
070300 01  DLI-IO-WDGX4003.                                                     
070400*    03  -COPY WDGX4003                                                   
070500                                                                          
070600 01  FILLER               PIC X(16)   VALUE 'WDGX4004'.                   
070700 01  DLI-IO-WDGX4004.                                                     
070800*    03  -COPY WDGX4004                                                   
070900                                                                          
071000 01  FILLER               PIC X(16)   VALUE 'WDGX4006'.                   
071100 01  DLI-IO-WDGX4006.                                                     
071200*    03  -COPY WDGX4006                                                   
071300                                                                          
071400 01  FILLER               PIC X(16)   VALUE 'WDGX4007'.                   
071500 01  DLI-IO-WDGX4007.                                                     
071600*    03  -COPY WDGX4007                                                   
071700                                                                          
071800 01  FILLER               PIC X(16)   VALUE 'WDGX4008'.                   
071900 01  DLI-IO-WDGX4008.                                                     
072000*    03  -COPY WDGX4008                                                   
072100                                                                          
072200 01  FILLER               PIC X(16)   VALUE 'WDGX4010'.                   
072300 01  DLI-IO-WDGX4010.                                                     
072400*    03  -COPY WDGX4010                                                   
072500                                                                          
072600 01  FILLER               PIC X(16)   VALUE 'WDGX4017'.                   
072700 01  DLI-IO-WDGX4017.                                                     
072800*    03  -COPY WDGX4017                                                   
072900                                                                          
073000 01  FILLER               PIC X(16)   VALUE 'WDGX4018'.                   
073100 01  DLI-IO-WDGX4018.                                                     
073200*    03  -COPY WDGX4018                                                   
073300                                                                          
073400 01  FILLER               PIC X(16)   VALUE 'WDGX4448'.                   
073500 01  DLI-IO-WDGX4448.                                                     
073600*    03  -COPY WDGX4448                                                   
073700                                                                          
073800*01  FILLER               PIC X(16)   VALUE 'WDGX4454'.                   
073900*01  DLI-IO-WDGX4454.                                                     
074000*    03  +COPY WDGX4454                                                   
074100*                                                                         
074200 01  FILLER               PIC X(16)   VALUE 'WDGX4512'.                   
074300 01  DLI-IO-WDGX4512.                                                     
074400*    03  -COPY WDGX4512                                                   
074500     EJECT                                                                
074600 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
074700 01  DLI-IO-WDM211.                                                       
074800*    03 -COPY WDM211                                                      
074900     EJECT                                                                
075000 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
075100 01  DLI-IO-WDM221.                                                       
075200*    03 -COPY WDM221                                                      
075300     EJECT                                                                
075400 01  FILLER               PIC X(16)   VALUE 'WDGX4536'.                   
075500 01  DLI-IO-WDGX4536.                                                     
075600*    03  -COPY WDGX4536                                                   
075700                                                                          
075800 01  FILLER               PIC X(16)   VALUE 'WDGX4541'.                   
075900 01  DLI-IO-WDGX4541.                                                     
076000*    03  -COPY WDGX4542                                                   
076100                                                                          
076200 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDG302'.              
076300 01  DLI-IO-WDG302.                                                       
076400*    03  -COPY WDGX2204                                                   
076500                                                                          
076600 01  FILLER               PIC X(16)   VALUE 'WDA501  '.                   
076700 01  DLI-IO-WDA501.                                                       
076800*    03  -COPY WDA501                                                     
076900                                                                          
077000 01  FILLER               PIC X(16)   VALUE 'WDA601  '.                   
077100 01  DLI-IO-WDA601.                                                       
077200*    03  -COPY WDA601                                                     
077300                                                                          
077400 01  FILLER               PIC X(16)   VALUE 'WDB201  '.                   
077500 01  DLI-IO-WDB201.                                                       
077600*    03  -COPY WDB201                                                     
077700                                                                          
077800 01  FILLER               PIC X(16)   VALUE 'WDB601  '.                   
077900 01  DLI-IO-WDB601.                                                       
078000*    03  -COPY WDB601                                                     
078100                                                                          
078200 01  FILLER               PIC X(16)   VALUE 'WDD311  '.                   
078300 01  DLI-IO-WDD311.                                                       
078400*    03  -COPY WDD311                                                     
078500                                                                          
078600                                                                          
078700 01  FILLER               PIC X(16)   VALUE 'WDD501  '.                   
078800 01  DLI-IO-WDD501.                                                       
078900*    03  -COPY WDD501                                                     
079000                                                                          
079100 01  FILLER               PIC X(16)   VALUE 'WDG601  '.                   
079200 01  DLI-IO-WDG601.                                                       
079300*    03  -COPY WDG601                                                     
079400                                                                          
079500 01  FILLER               PIC X(16)   VALUE 'WDK601  '.                   
079600 01  DLI-IO-WDK601.                                                       
079700*    03  -COPY WDK601                                                     
079800                                                                          
079900 01  FILLER               PIC X(16)   VALUE 'WDK611  '.                   
080000 01  DLI-IO-WDK611.                                                       
080100*    03  -COPY WDK611                                                     
080200                                                                          
080300 01  FILLER               PIC X(16)   VALUE 'WDK627  '.                   
080400 01  DLI-IO-WDK627.                                                       
080500*    03  -COPY WDK627                                                     
080600                                                                          
080700 01  FILLER               PIC X(16)   VALUE 'WDK711  '.                   
080800 01  DLI-IO-WDK711.                                                       
080900*    03  -COPY WDK711                                                     
081000                                                                          
081100 01  FILLER               PIC X(16)   VALUE 'WDK712  '.                   
081200 01  DLI-IO-WDK712.                                                       
081300*    03  -COPY WDK712                                                     
081400                                                                          
081500 01  FILLER               PIC X(16)   VALUE 'WDK722  '.                   
081600 01  DLI-IO-WDK722.                                                       
081700*    03  -COPY WDK722                                                     
081800                                                                          
081900 01  FILLER               PIC X(16)   VALUE 'WDK901  '.                   
082000 01  DLI-IO-WDK901.                                                       
082100*    03  -COPY WDK901                                                     
082200                                                                          
082300 01  FILLER               PIC X(16)   VALUE 'WDQ101  '.                   
082400 01  DLI-IO-WDQ101.                                                       
082500*    03  -COPY WDQ101                                                     
082600                                                                          
082700 01  FILLER               PIC X(16)   VALUE 'WDQ201-12'.                  
082800 01  DLI-IO-WDQ201-12.                                                    
082900*    03  -COPY WDQ201                                                     
083000*    03  -COPY WDQ212                                                     
083100                                                                          
083200 01  FILLER               PIC X(16)   VALUE 'WDQ221 '.                    
083300 01  DLI-IO-WDQ221.                                                       
083400*    03  -COPY WDQ221                                                     
083500                                                                          
083600 01  FILLER               PIC X(16)   VALUE 'WDQ201 '.                    
083700 01  DLI-IO-WDQ201.                                                       
083800*    03  -COPY WDQ201    -PRE CSQ-                                        
083900                                                                          
084000 01  FILLER               PIC X(16)   VALUE 'WDQ301  '.                   
084100 01  DLI-IO-WDQ301.                                                       
084200*    03  -COPY WDQ301                                                     
084300                                                                          
084400 01  FILLER               PIC X(16)   VALUE 'WDQ401  '.                   
084500 01  DLI-IO-WDQ401.                                                       
084600*    03  -COPY WDQ401                                                     
084700                                                                          
084800 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLLOGA01'.               
084900 01  DLI-IO-WLLOGA01.                                                     
085000*    03  WLLOGA01  -COPY WDL901                                           
085100     EJECT                                                                
085200                                                                          
085300 01  FILLER                    PIC X(16) VALUE 'ALT-IO-AREA'.             
085400 01  ALT-IO-AREA.                                                         
085500                                                                          
085600  03     ALT-LL                PIC S9(4) COMP SYNC.                       
085700  03     ALT-Z1                PIC X(1)  VALUE LOW-VALUE.                 
085800  03     ALT-Z2                PIC X(1)  VALUE LOW-VALUE.                 
085900  03     ALT-TRANSKOD          PIC X(8)  VALUE 'W2T191X '.                
086000  03     ALT-IDTRANS           PIC X(4)  VALUE '4375'.                    
086100  03     ALT-SPRAK             PIC X(1).                                  
086200* 03     MID -COPY W2I19101   -PRE ALT-                                   
086300                                                                          
086400 01  FILLER                    PIC X(16) VALUE 'P-TO-P-AREA'.             
086500 01  4397-TRANSAREA.                                                      
086600  03     4397-IDPRODNR         PIC 9(7)  VALUE ZERO.                      
086700  03     4397-IDANSTNR         PIC 9(5)  VALUE ZERO.                      
086800  03     4397-IDPLKLST         PIC 9(3)  VALUE ZERO.                      
086900  03     4397-IDPURAD          PIC 9(5)  VALUE ZERO.                      
087000  03     FILLER                PIC X(80) VALUE SPACE.                     
087100                                                                          
087200 01  FILLER                    PIC X(16) VALUE '4397-IO-AREA'.            
087300 01  4397-IO-AREA.                                                        
087400  03     4397-LL               PIC S9(4) COMP  SYNC.                      
087500  03     4397-Z1               PIC X(1).                                  
087600  03     4397-Z2               PIC X(1).                                  
087700  03     4397-TRANSKOD         PIC X(8)  VALUE SPACE.                     
087800  03     4397-IDTRANS          PIC X(4)  VALUE SPACE.                     
087900  03     4397-KDMFSFOR         PIC X(1)  VALUE SPACE.                     
088000  03     4397-AREA             PIC X(100) VALUE SPACE.                    
088100                                                                          
088200  03  W-4541-IDHTYP-X.                                                    
088300      05  W-4541-IDHTYP        PIC  X(4)  VALUE '4541'.                   
088400      05  W-4541-LOW-VALUE     PIC  X(26) VALUE LOW-VALUE.                
088500*                                                                         
088600   03  W-KDORDKL-X.                                                       
088700       05  W-KDORDKL                 PIC S9    VALUE ZERO COMP-3.         
088800                                                                          
088900   03    W-IDKUNDRF-LEV-X.                                                
089000     05  W-IDKUNDRF-LEV              PIC X(10)   VALUE SPACE.             
089100*                                                                         
089200                                                                          
089300 01  FILLER               PIC X(16)   VALUE 'WDP4A1 AREA'.                
089400 01   DLI-IO-AREA-WDP4A1.                                                 
089500*     03  -COPY WDP4A1                                                    
089600                                                                          
089700 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
089800                                                                          
089900*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
090000     EJECT                                                                
090100     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
090200     EJECT                                                                
090300                                                                          
090400 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
090500     SKIP3                                                                
090600*01  -COPY WMSGAREA                                                       
090700     EJECT                                                                
090800*    --- AREOR FÖR HANTERING AV API                                       
090900*01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-WMSG'.         
091000 01  KOM-IO-AREA.                                                         
091100     03  -COPY WMSGKOM                                                    
091200                                                                          
091300*01  FILLER                      PIC X(16)   VALUE 'Z430-REQU-A '.        
091400*01  -COPY WZ0430I1  -PRE Z430-                                           
091500*    03  -COPY WAPIORD  -RED Z430-REQU-EVENT-DATA -PRE Z430-              
091600                                                                          
091700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
091800     SKIP3                                                                
091900 LINKAGE SECTION.                                                         
092000*                                                                         
092100                                                                          
092200*01  -COPY WL013410                                                       
092300                                                                          
092400*01  -COPY W0009  -PRE MSG-                                               
092500                                                                          
092600 01  0693-PCB          PIC X.                                             
092700     EJECT                                                                
092800*01  -COPY W0009  -PRE ALT-                                               
092900                                                                          
093000*01  -COPY W0009  -PRE 4397-                                              
093100                                                                          
093200*01  -COPY W0008  -PRE 4003-                                              
093300     05  FILLER                  PIC X.                                   
093400                                                                          
093500*01  -COPY W0008  -PRE 4007-                                              
093600     05  FILLER                  PIC X.                                   
093700                                                                          
093800*01  -COPY W0008  -PRE 4017-                                              
093900     05  FILLER                  PIC X.                                   
094000                                                                          
094100*01  -COPY W0008  -PRE 4448-                                              
094200     05  FILLER                  PIC X.                                   
094300                                                                          
094400*01  -COPY W0008  -PRE 4453-                                              
094500     05  FILLER                  PIC X.                                   
094600                                                                          
094700*01  -COPY W0008  -PRE 4512-                                              
094800     05  FILLER                  PIC X.                                   
094900                                                                          
095000*01  -COPY W0008  -PRE WDM2-                                              
095100     05  FILLER                  PIC X.                                   
095200                                                                          
095300*01  -COPY W0008  -PRE 4536-                                              
095400     05  FILLER                  PIC X.                                   
095500                                                                          
095600*01  -COPY W0008  -PRE WDA5-                                              
095700     05  FILLER                  PIC X.                                   
095800                                                                          
095900*01  -COPY W0008  -PRE WDA6-                                              
096000     05  FILLER                  PIC X.                                   
096100                                                                          
096200*01  -COPY W0008  -PRE WDA6B-                                             
096300     05  FILLER                  PIC X.                                   
096400                                                                          
096500*01  -COPY W0008  -PRE WDB2-                                              
096600     05  FILLER                  PIC X.                                   
096700                                                                          
096800*01  -COPY W0008  -PRE WDB6-                                              
096900     05  FILLER                  PIC X.                                   
097000                                                                          
097100*01  -COPY W0008  -PRE WDD3-                                              
097200     05  FILLER                  PIC X.                                   
097300                                                                          
097400*01  -COPY W0008  -PRE WDD5-                                              
097500     05  FILLER                  PIC X.                                   
097600                                                                          
097700*01  -COPY W0008  -PRE WDG6-                                              
097800     05  FILLER                  PIC X.                                   
097900                                                                          
098000*01  -COPY W0008  -PRE WDK6-                                              
098100     05  FILLER                  PIC X.                                   
098200                                                                          
098300*01  -COPY W0008  -PRE WDK7-                                              
098400     05  FILLER                  PIC X.                                   
098500                                                                          
098600*01  -COPY W0008  -PRE WDK9-                                              
098700     05  FILLER                  PIC X.                                   
098800                                                                          
098900*01  -COPY W0008  -PRE WDQ1-                                              
099000     05  FILLER                  PIC X.                                   
099100                                                                          
099200*01  -COPY W0008  -PRE WDQ2-                                              
099300     05  FILLER                  PIC X.                                   
099400                                                                          
099500*01  -COPY W0008  -PRE WDQ2C-                                             
099600     05  FILLER                  PIC X.                                   
099700                                                                          
099800*01  -COPY W0008  -PRE WDQ3-                                              
099900     05  FILLER                  PIC X.                                   
100000                                                                          
100100*01  -COPY W0008  -PRE WDQ4-                                              
100200     05  FILLER                  PIC X.                                   
100300                                                                          
100400*01  -COPY W0008  -PRE 4541-                                              
100500     05  FILLER                  PIC X.                                   
100600                                                                          
100700*01  -COPY W0008  -PRE 2203-                                              
100800     05  FILLER                  PIC X.                                   
100900                                                                          
101000*01  -COPY W0008  -PRE WDP4A-                                             
101100     05  FILLER                  PIC X.                                   
101200                                                                          
101300*01  -COPY W0008  -PRE ORQICSQ-                                           
101400     05  FILLER                  PIC X.                                   
101500                                                                          
101600*01  -COPY W0008  -PRE WLLOGA-                                            
101700     05  FILLER                  PIC X.                                   
101800                                                                          
101900     EJECT                                                                
102000*----> SUBPROGRAM W411DEAV.                                               
102100 01  DEAV-ARTM-PCB               PIC X(1).                                
102200 01  DEAV-WDK7-PCB               PIC X(1).                                
102300 01  DEAV-WDB6-PCB               PIC X(1).                                
102400 01  DEAV-WDB2-PCB               PIC X(1).                                
102500 01  DEAV-WDL7-PCB               PIC X(1).                                
102600 01  DEAV-WDK72-PCB              PIC X(1).                                
102700 01  DEAV-WDR2-PCB               PIC X(1).                                
102800 01  DEAV-WDR5-PCB               PIC X(1).                                
102900 01  DEAV-WDC1-PCB               PIC X(1).                                
103000                                                                          
103100*----> SUBPROGRAM W411RANS.                                               
103200 01  RANS-XXKM-PCB               PIC X(1).                                
103300 01  RANS-ARTM-PCB               PIC X(1).                                
103400 01  RANS-ARTS-PCB               PIC X(1).                                
103500                                                                          
103600*----> SUBPROGRAM W411AREG.                                               
103700 01  AREG-WDK6-PCB               PIC X(1).                                
103800 01  AREG-WDK7-PCB               PIC X(1).                                
103900                                                                          
104000*----> SUBPROGRAM W413SPAR.                                               
104100 01  SPAR-WDF8-PCB               PIC X(1).                                
104200 01  SPAR-WDF8A-PCB              PIC X(1).                                
104300 01  SPAR-WDK6-PCB               PIC X(1).                                
104400                                                                          
104500*----> SUBPROGRAM W335PRQU.                                               
104600 01  PRQU-WDG2-PCB               PIC X.                                   
104700 01  PRQU-WDC7-PCB               PIC X.                                   
104800 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
104900                                                                          
105000*----> SUBPROGRAM W335PRNO.                                               
105100 01  PRNO-3107-PCB               PIC X.                                   
105200                                                                          
105300*----> SUBPROGRAM W403PLAT.                                               
105400 01  PLATS-DM-PCB                PIC X.                                   
105500 01  PLATS-DN-PCB                PIC X.                                   
105600 01  PLATS-DP-PCB                PIC X.                                   
105700 01  PLATS-DO-PCB                PIC X.                                   
105800 01  PLATS-WDE6C-PCB             PIC X.                                   
105900 01  PLATS-GMTC-PCB              PIC X.                                   
106000 01  PLATS-WDB6-PCB              PIC X.                                   
106100*                                                                         
106200 01  KOM-WDP8-PCB                PIC X.                                   
106300*                                                                         
106400                                                                          
106500 PROCEDURE DIVISION USING  3410-WL013410                                  
106600                           MSG-PCB  0693-PCB ALT-PCB 4397-PCB             
106700                           4003-PCB 4007-PCB 4017-PCB 4448-PCB            
106800                           4453-PCB 4512-PCB WDM2-PCB 4536-PCB            
106900                           WDA5-PCB WDA6-PCB WDA6B-PCB                    
107000                           WDB2-PCB WDB6-PCB WDD3-PCB WDD5-PCB            
107100                           WDG6-PCB WDK6-PCB WDK7-PCB WDK9-PCB            
107200                           WDQ1-PCB WDQ2-PCB WDQ2C-PCB                    
107300                           WDQ3-PCB WDQ4-PCB                              
107400                           4541-PCB 2203-PCB WDP4A-PCB                    
107500                           ORQICSQ-PCB    WLLOGA-PCB                      
107600                           DEAV-ARTM-PCB  DEAV-WDK7-PCB                   
107700                           DEAV-WDB6-PCB                                  
107800                           DEAV-WDB2-PCB  DEAV-WDL7-PCB                   
107900                           DEAV-WDK72-PCB DEAV-WDR2-PCB                   
108000                           DEAV-WDR5-PCB  DEAV-WDC1-PCB                   
108100                           RANS-XXKM-PCB                                  
108200                           RANS-ARTM-PCB  RANS-ARTS-PCB                   
108300                           AREG-WDK6-PCB  AREG-WDK7-PCB                   
108400                           SPAR-WDF8-PCB  SPAR-WDF8A-PCB                  
108500                           SPAR-WDK6-PCB                                  
108600                           PRQU-WDG2-PCB  PRQU-WDC7-PCB                   
108700                           PRQU-SJKO-WDK6-PCB                             
108800                           PRNO-3107-PCB                                  
108900                           PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB         
109000                           PLATS-DO-PCB PLATS-WDE6C-PCB                   
109100                           PLATS-GMTC-PCB PLATS-WDB6-PCB                  
109200                           KOM-WDP8-PCB.                                  
109300                                                                          
109400 MAIN SECTION.                                                            
109500                                                                          
109600     PERFORM A-INIT                                                       
109700                                                                          
109800     PERFORM B-LAES-PLOCKSATS                                             
109900     PERFORM C-BEHANDLA-ORDERDEL                                          
110000                                                                          
110100     PERFORM D-UPPDATERA-PLOCKSATS                                        
110200     PERFORM E-FIXA-ENGELSK-RADREF                                        
110300                                                                          
110400     MOVE ZERO TO RETURN-CODE                                             
110500     GOBACK.                                                              
110600                                                                          
110700                                                                          
110800 A-INIT SECTION.                                                          
110900                                                                          
111000     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
111100                                                                          
111200     MOVE JA                  TO ALLT-SW                                  
111300                                 INITIERA-LDC-TABELL                      
111400                                 FORSTA-GANGEN-SW                         
111500     MOVE NEJ                 TO PRC-SW                                   
111600                                 PLOCK-SW                                 
111700                                 SW-AENDRA-LAGOMR                         
111710                                 SW-LYNK-NON-API                          
111720                                 SW-VOR                                   
111800                                                                          
111900     MOVE ZERO                TO WS-SPAR-IDORDER                          
112000                                 WS-ORDERVIKT-PER-RAD                     
112100                                 WS-ORDERVIKT-PER-ORDER                   
112200     MOVE ZERO                TO WS-ORDERVOLYM-PER-RAD                    
112300                                                                          
112400     MOVE ZERO                TO WS-OHUV-IDORDER                          
112500                                                                          
112600     MOVE SPACE                 TO EVENT-SW                               
112700*                                                                         
112800     ACCEPT WS-KLOCKAN      FROM TIME                                     
112900     ACCEPT DAGENS-DATUM    FROM DATE                                     
113000                                                                          
113100     MOVE '011'               TO MSGI-KDCALL                              
113200     MOVE DCS-IDTIDZON        TO MSGI-IDTIDZON                            
           MOVE DCS-IDDC            TO MSGI-IDDC                                
113300     MOVE DAGENS-DATUM        TO MSGI-TILOKDAT                            
113400     MOVE WS-KLOCKAN          TO MSGI-TILOKTID                            
113500     CALL WL01TIDZ USING         MSGI-WL01TIDZ                            
113600     MOVE MSGI-TILOKDAT       TO WS-DATUM-LOK                             
113700     MOVE WS-TIHHMMSS         TO WS-TIHHMMSS-LOK                          
113800     MOVE MSGI-TILOKTID       TO WS-TIHHMMSS-LOK(1:4)                     
113900                                                                          
114000*                                                                         
114100*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
114200*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
114300     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
114400     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
114500     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
114600     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
114700     MOVE SPACE                      TO MSG-KOM-KDTRANS                   
114800     MOVE 'WL013410'                 TO MSG-KOM-IDSNDJOB                  
114900     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
115000*    -- THIS IS THE START VALUE                                           
115100     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
115200                                                                          
115300*    -- THIS IS THE START VALUE                                           
115400*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
115500     MOVE LOW-VALUE                  TO MSG-KDZ1                          
115600     MOVE LOW-VALUE                  TO MSG-KDZ2                          
115700     .                                                                    
115800     EJECT                                                                
115900                                                                          
116000                                                                          
116100 B-LAES-PLOCKSATS       SECTION.                                          
116200     MOVE 'B-LAES-PLOCKSATS'      TO WS-CURRENT-SECTION                   
116300                                                                          
116400     MOVE +1 TO ODEL-IX                                                   
116500                                                                          
116600     PERFORM BA-LAES-PLOCKSATS-ETIK                                       
116700     IF PLOCKSATS-ETIK-SAKNAS                                             
116800        PERFORM BB-INIT-PLOCKSATS-ETIK                                    
116900     END-IF                                                               
117000                                                                          
117100     PERFORM BC-LAES-PLOCKSATS-PU                                         
117200     IF PLOCKSATS-PU-SAKNAS                                               
117300        PERFORM BD-INIT-PLOCKSATS-PU                                      
117400     END-IF                                                               
117500     .                                                                    
117600                                                                          
117700 BA-LAES-PLOCKSATS-ETIK SECTION.                                          
117800     MOVE 'BA-LAES-ETIK'          TO WS-CURRENT-SECTION                   
117900                                                                          
118000                                                                          
118100     MOVE 3410-IDPRODNR-KEY  TO W-4003-ETIK-IDPRODNR                      
118200     MOVE 3410-IDPLKLST-KEY  TO W-4003-ETIK-IDPLKLST                      
118300                                                                          
118400     PERFORM IMS-01-GU-WDGX4004                                           
118500                                                                          
118600     IF SEGMENT-FINNS                                                     
118700        MOVE JA TO PLOCKSATS-ETIK-SW                                      
118800     ELSE                                                                 
118900        MOVE NEJ TO PLOCKSATS-ETIK-SW                                     
119000     END-IF                                                               
119100     .                                                                    
119200                                                                          
119300 BB-INIT-PLOCKSATS-ETIK SECTION.                                          
119400     MOVE 'BB-INIT-PLOCKSATS'     TO WS-CURRENT-SECTION                   
119500                                                                          
119600     MOVE 1          TO 4004-KDSEGKEY                                     
119700     MOVE SPACE      TO 4004-KDPRT-PLE                                    
119800     MOVE LOW-VALUE  TO 4004-NYCKEL-GRP                                   
119900     MOVE 1          TO IX1                                               
120000                                                                          
120100     PERFORM UNTIL IX1 > 101                                              
120200        MOVE 0 TO 4004-KVRADER (IX1)                                      
120300        ADD  1 TO IX1                                                     
120400     END-PERFORM                                                          
120500     .                                                                    
120600                                                                          
120700 BC-LAES-PLOCKSATS-PU SECTION.                                            
120800     MOVE 'BC-LAES-PU  '          TO WS-CURRENT-SECTION                   
120900                                                                          
121000     MOVE 3410-IDPRODNR-KEY  TO W-4007-PU-IDPRODNR                        
121100     MOVE 3410-IDPLKLST-KEY  TO W-4007-PU-IDPLKLST                        
121200                                                                          
121300     PERFORM IMS-02-GU-4007-WDGX4008                                      
121400                                                                          
121500     IF SEGMENT-FINNS                                                     
121600        MOVE JA  TO PLOCKSATS-PU-SW                                       
121700     ELSE                                                                 
121800        MOVE NEJ TO PLOCKSATS-PU-SW                                       
121900     END-IF                                                               
122000     .                                                                    
122100                                                                          
122200 BD-INIT-PLOCKSATS-PU SECTION.                                            
122300     MOVE 'BD-INIT-PLOCKSATS'     TO WS-CURRENT-SECTION                   
122400                                                                          
122500     MOVE 1                   TO 4008-KDSEGKEY                            
122600     MOVE 3410-IDLOPNR        TO 4008-IDLOPNR-PL                          
122700     MOVE SPACE               TO 4008-IDPRC                               
122800                                 4008-KDPRT-PU                            
122900     MOVE 0                   TO 4008-IDSID                               
123000     MOVE 3410-IDDC(ODEL-IX)  TO 4008-IDDC                                
123100                                                                          
123200     MOVE 0                   TO 4008-KVRADER  (1)                        
123300                                 4008-KVRADER  (2)                        
123400                                 4008-KVRADER  (3)                        
123500                                 4008-VKORDNTO (1)                        
123600                                 4008-VKORDNTO (2)                        
123700                                 4008-VKORDNTO (3)                        
123800                                 4008-VLORDNTO (1)                        
123900                                 4008-VLORDNTO (2)                        
124000                                 4008-VLORDNTO (3)                        
124100     MOVE LOW-VALUE           TO 4008-NYCKEL-GRP                          
124200     MOVE 1                   TO IX1                                      
124300                                                                          
124400     PERFORM UNTIL IX1 > 101                                              
124500        MOVE 0 TO 4008-KVRADER-GRP (IX1)                                  
124600        ADD  1 TO IX1                                                     
124700     END-PERFORM                                                          
124800     .                                                                    
124900                                                                          
125000 C-BEHANDLA-ORDERDEL   SECTION.                                           
125100                                                                          
125200     MOVE 'C-BEHANDLA-ORDERDEL'  TO WS-CURRENT-SECTION                    
125300                                                                          
125400     MOVE ZERO  TO OUTPUT-MSG-IX                                          
125500                                                                          
125600     PERFORM UNTIL ODEL-IX > 3410-IX                                      
125700        MOVE JA   TO ALLT-SW                                              
125800        MOVE 1    TO ORAD-IX                                              
125900        MOVE ZERO TO WS-KVRADER                                           
126000        MOVE ZERO               TO WS-VLORDNTO                            
126100        MOVE ZERO               TO WS-VKORDNTO                            
126200        PERFORM CA-LAES-ORDERDEL                                          
126300        IF ALLT-OK                                                        
126400           PERFORM CB-LAES-ORDERHUVUD                                     
126500           IF ALLT-OK                                                     
126600              PERFORM CC-LAES-PRC-PRINTERTABELL                           
126700              IF ALLT-OK                                                  
126800                 PERFORM CE-BEHANDLA-ORDERRADER                           
126900                 PERFORM CF-UPPDAT-ORDERDEL                               
127000              END-IF                                                      
127100           END-IF                                                         
127200        END-IF                                                            
127300        ADD 1 TO ODEL-IX                                                  
127400     END-PERFORM                                                          
127500     .                                                                    
127600                                                                          
127700 CA-LAES-ORDERDEL SECTION.                                                
127800     MOVE 'CA-LAES-ORDERDEL'      TO WS-CURRENT-SECTION                   
127900*********************************************************                 
128000*  OM ORDERDELEN HAR PRC 9998 SKALL DEN LÄGGAS TILLBAKA *                 
128100*  PÅ KÖN EFTERSOM DET ÄR DIREKTLEVERANS.               *                 
128200*  DETTA KAN INTRÄFFA OM BAKGRUNDSMPP:N SOM SKRIVER UT  *                 
128300*  DIREKTLEVERANS ÄR STOPPAD AV NÅGON ANLEDNING.        *                 
128400*********************************************************                 
128500                                                                          
128600     MOVE 'GE' TO STATUS-WS                                               
128700                                                                          
128800     PERFORM UNTIL SEGMENT-FINNS OR                                       
128900                   ODEL-IX > 3410-IX                                      
129000                                                                          
129100        MOVE 3410-IDORDER (ODEL-IX) TO W-Q301KY-IDORDER                   
129200                                       W-Q301KY-MIN-IDORDER               
129300                                       W-Q301KY-MAX-IDORDER               
129400        MOVE 3410-IDDC    (ODEL-IX) TO W-Q301KY-IDDC                      
129500                                       W-Q301KY-MIN-IDDC                  
129600                                       W-Q301KY-MAX-IDDC                  
129700        MOVE 3410-IDPRODNR(ODEL-IX) TO W-Q301KY-IDPRODNR                  
129800                                       WS-IDPRODNR                        
129900        MOVE 3410-IDPLKLST(ODEL-IX) TO W-Q301KY-IDPLKLST                  
130000                                                                          
130100        PERFORM IMS-03-GHU-WDQ301                                         
130200        IF SEGMENT-FINNS                                                  
130300           IF ODEL-IDPRC = WS-DIRLEV-PRC                                  
130400              PERFORM S13-ATERSTALL-ORDERDEL                              
130500              MOVE 'GE'             TO STATUS-WS                          
130600           END-IF                                                         
130700        ELSE                                                              
130800           ADD +1 TO ODEL-IX                                              
130900        END-IF                                                            
131000     END-PERFORM                                                          
131100                                                                          
131200     IF SEGMENT-FINNS                                                     
131300        MOVE DLI-IO-WDQ301   TO W-ORDERDEL                                
131400        MOVE JA              TO ODEL-SW                                   
131500        PERFORM CAA-LAES-EMBALLAGEKOD                                     
131600        IF 4008-IDPRC = SPACE                                             
131700           MOVE W-ODEL-IDPRC TO 4008-IDPRC                                
131800        END-IF                                                            
131900     END-IF                                                               
132000                                                                          
132100     IF ODEL-IX > 3410-IX                                                 
132200        MOVE JA    TO PLOCK-SW                                            
132300        MOVE NEJ   TO ALLT-SW                                             
132400        MOVE 1000  TO ORAD-IX                                             
132500     END-IF                                                               
132600     .                                                                    
132700                                                                          
132800 CAA-LAES-EMBALLAGEKOD SECTION.                                           
132900     MOVE 'CAA-LAES-EMBALLAGEKOD'  TO WS-CURRENT-SECTION                  
133000                                                                          
133100     MOVE SPACE           TO WS-KDEMBAL                                   
133200     MOVE W-ODEL-KDFDKRAV TO W-4535-KDFDKRAV                              
133300                                                                          
133400     IF 3410-IDDC(ODEL-IX) NOT = DCS-IDDC                                 
133500        MOVE 3410-IDDC(ODEL-IX) TO W-IDDC-B6                              
133600        PERFORM IMS-05-GU-WDB601                                          
133700     END-IF                                                               
133800                                                                          
133900     IF DCS-SWEDEN                                                        
134000       MOVE 'S  ' TO W-4536-IDSKYLT                                       
134100     ELSE                                                                 
134200       MOVE 'GB ' TO W-4536-IDSKYLT                                       
134300     END-IF                                                               
134400                                                                          
134500     PERFORM IMS-06-GU-WDGX4536                                           
134600     IF SEGMENT-FINNS                                                     
134700        MOVE 4536-KDEMBAL TO WS-KDEMBAL                                   
134800     END-IF                                                               
134900     .                                                                    
135000                                                                          
135100 CB-LAES-ORDERHUVUD SECTION.                                              
135200     MOVE 'CB-LAES-ORDERHUVUD'    TO WS-CURRENT-SECTION                   
135300                                                                          
135400     MOVE W-ODEL-IDORDER  TO W-IDORDER                                    
135500     MOVE W-ODEL-IDDC     TO W-IDDC                                       
135600                                                                          
135700     PERFORM IMS-07-GHU-WDQ201-12                                         
135800     IF OHUV-FLKLAR NOT = JA                                              
135900        MOVE NEJ  TO ALLT-SW                                              
136000     END-IF                                                               
136100                                                                          
136200     IF ALLT-OK                                                           
136300        IF OHUV-KDORDKL = 3 OR 4                                          
136400           PERFORM CBB-KOLLA-BIPA-SPARR                                   
136500        END-IF                                                            
136600        PERFORM S15-LAES-EV-KUNDREG                                       
136700*LDC-GB                                                                   
136800        MOVE OHUV-IDDISTR     TO DIST34-IDDISTR                           
136900        IF DIST34-ENGLAND-SDC AND                                         
137000           GMT-FLLDCKND = JA                                              
137100           IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                       
137200           OR  OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                      
137300                                                                          
137400              IF INITIERA-LDC-TABELL = JA                                 
137500                                                                          
137600                 MOVE HIGH-VALUE TO LDC-TABELL                            
137700                                                                          
137800                 MOVE NEJ                   TO INITIERA-LDC-TABELL        
137900                 MOVE 3410-IDPRODNR(ODEL-IX) TO W-4017-IDPRODNR           
138000                 MOVE 3410-IDPLKLST(ODEL-IX) TO W-4017-IDPLKLST           
138100                 MOVE LOW-VALUE             TO W-4017-LOW-VALUE           
138200                                                                          
138300                 PERFORM IMS-09-GHU-WDGX4018                              
138400                 IF SEGMENT-FINNS                                         
138500                    MOVE +1   TO LDC-TAB-IX                               
138600                    MOVE +100 TO LDC-TAB-IX-MAX                           
138700                    PERFORM UNTIL LDC-TAB-IX > LDC-TAB-IX-MAX             
138800                                                                          
138900                       MOVE 4018-BERADREF (LDC-TAB-IX)                    
139000                         TO LDC-TAB-WIPID (LDC-TAB-IX)                    
139100                                                                          
139200                       ADD +1 TO LDC-TAB-IX                               
139300                    END-PERFORM                                           
139400                 END-IF                                                   
139500              END-IF                                                      
139600           END-IF                                                         
139700        END-IF                                                            
139800     ELSE                                                                 
139900        MOVE W-ORDERDEL TO DLI-IO-WDQ301                                  
140000        PERFORM S13-ATERSTALL-ORDERDEL                                    
140100        ADD +1 TO ODEL-IX                                                 
140200     END-IF                                                               
140300     .                                                                    
140400     EJECT                                                                
140500 CBB-KOLLA-BIPA-SPARR SECTION.                                            
140600     MOVE 'CBB-KOLLA-BIPA-SPARR'   TO WS-CURRENT-SECTION                  
140700                                                                          
140800     IF W-ODEL-TIREGDAT = 3410-TIAAMMDD                                   
140900        MOVE NEJ TO BIPA-SPARR-SW                                         
141000     ELSE                                                                 
141100        MOVE W-ODEL-TIREGDAT TO DAT-I-TIDATUM                             
141200        PERFORM CBBA-ANROP-WDATKONV                                       
141300        MOVE DAT-TIAADDD TO WS-REGDATUM-AADDD                             
141400                                                                          
141500        MOVE 3410-TIAADDD       TO TMP1-YYDDD                             
141600        MOVE WS-REGDATUM-AADDD  TO TMP2-YYDDD                             
141700                                                                          
141800        COMPUTE WS-ANTAL-DAGAR = TMP1-YYDDD -                             
141900                                 TMP2-YYDDD                               
142000                                                                          
142100        IF WS-ANTAL-DAGAR < 2                                             
142200           MOVE NEJ TO BIPA-SPARR-SW                                      
142300        END-IF                                                            
142400     END-IF                                                               
142500     .                                                                    
142600                                                                          
142700 CBBA-ANROP-WDATKONV SECTION.                                             
142800     MOVE 'CBBA-ANROP-WDATKONV    ' TO WS-CURRENT-SECTION                 
142900                                                                          
143000     MOVE 'AAMMDD'  TO DAT-KDDATFORM                                      
143100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
143200                         DAT-O-TIDATUM DAT-KDSVAR                         
143300                                                                          
143400     IF DAT-KDSVAR-FEL                                                    
143500        MOVE 'DATUMKONVERTERINGEN HAR GÅTT SNETT' TO ERROR-TEXT           
143600        CALL FELLOG                                                       
143700     END-IF                                                               
143800     .                                                                    
143900                                                                          
144000 CC-LAES-PRC-PRINTERTABELL SECTION.                                       
144100     MOVE 'CC-LAES-PRC-PRINTERTABELL'   TO WS-CURRENT-SECTION             
144200                                                                          
144300     IF PRC-EJ-OK                                                         
144400        MOVE 4008-IDPRC      TO W-4448-IDPRC                              
144500                                W-4453-IDPRC                              
144600        MOVE W-ODEL-IDDC     TO W-4447-IDDC                               
144700                                W-4453-IDDC                               
144800        PERFORM IMS-11-GU-WDGX4448                                        
144900        IF SEGMENT-SAKNAS                                                 
145000           MOVE 9999 TO W-4448-IDPRC                                      
145100                        W-4453-IDPRC                                      
145200                                                                          
145300           PERFORM IMS-11-GU-WDGX4448                                     
145400           IF SEGMENT-SAKNAS                                              
145500              MOVE NEJ TO ALLT-SW                                         
145600           END-IF                                                         
145700        END-IF                                                            
145800                                                                          
145900        IF SEGMENT-FINNS                                                  
146000           MOVE 4448-KVRADER  TO WS-PRC-KVRADER                           
146100           MOVE 4448-VKORDNTO TO WS-PRC-VKORDNTO                          
146200           MOVE 4448-VLORDNTO TO WS-PRC-VLORDNTO                          
146300           MOVE 4448-RESPLIT  TO WS-PRC-RESPLIT                           
146400                                                                          
146500* VI GÖR ETT FÖRSÖK ATT TA BORT 4454-LÄSNING SOM KANSKE ÄR ONÖDIG         
146600*          PERFORM IMS-53-GU-WDGX4454                                     
146700*          IF SEGMENT-SAKNAS                                              
146800*             MOVE NEJ  TO ALLT-SW                                        
146900*          END-IF                                                         
147000        END-IF                                                            
147100                                                                          
147200        IF ALLT-OK                                                        
147300           MOVE JA TO PRC-SW                                              
147400        END-IF                                                            
147500     END-IF                                                               
147600                                                                          
147700     IF ALLT-FEL                                                          
147800        MOVE W-ORDERDEL TO DLI-IO-WDQ301                                  
147900        PERFORM S13-ATERSTALL-ORDERDEL                                    
148000        MOVE SPACE      TO 4008-IDPRC                                     
148100        ADD +1 TO ODEL-IX                                                 
148200     END-IF                                                               
148300     .                                                                    
148400                                                                          
148500 CE-BEHANDLA-ORDERRADER SECTION.                                          
148600     MOVE 'CE-BEHANDLA-ORDERRADER' TO WS-CURRENT-SECTION                  
148700                                                                          
148800     MOVE '2'             TO ORAD-SW                                      
148900     MOVE LOW-VALUE       TO W-WDQ401KY-MIN-X                             
149000     MOVE HIGH-VALUE      TO W-WDQ401KY-MAX-X                             
149100     MOVE ARB-KVSEMBRA        TO WS-KVSEMBRA                              
149200     MOVE ARB-IDRADNR-SISTA   TO WS-IDRADNR-SISTA                         
149300                                                                          
149400     MOVE W-ODEL-IDORDER  TO W-Q401KY-MIN-IDORDER                         
149500                             W-Q401KY-MAX-IDORDER                         
149600     MOVE W-ODEL-IDDC     TO W-Q401KY-MIN-IDDC                            
149700                             W-Q401KY-MAX-IDDC                            
149800     MOVE W-ODEL-IDPRC    TO W-IDPRC                                      
149900                                                                          
150000     PERFORM IMS-GHNP-WDQ221                                              
150100                                                                          
150200     PERFORM UNTIL ORAD-IX   > MAX-ORAD                                   
150300                OR Q221-SEG-SAKNAS                                        
150400                                                                          
150500        MOVE NEJ          TO LAGOMR-SW                                    
150600        MOVE LOR-ADLAGOMR TO W-Q401KY-MIN-ADLAGOMR                        
150700                             W-Q401KY-MAX-ADLAGOMR                        
150800                                                                          
150900        PERFORM UNTIL ORAD-IX   > MAX-ORAD                                
151000                OR NYTT-LAGEROMRADE                                       
151100                                                                          
151200           IF NY-ORDERDEL                                                 
151300              PERFORM S15-LAES-EV-KUNDREG                                 
151400              PERFORM IMS-12-GHU-WDQ401                                   
151500*L138                                                                     
151600              IF 3410-IDTRANS = 'L138' OR 'A138'                          
151700                 PERFORM CEK-KOLLA-PLATSSATTNING                          
151800               MOVE ODEL-DARFS (3:6) TO 3410-TIRFSDAT(ODEL-IX)            
151900               MOVE ODEL-DARFS (9:4) TO 3410-TIRFSTID(ODEL-IX)            
152000              END-IF                                                      
152100              MOVE NEJ TO ODEL-SW                                         
152200           ELSE                                                           
152300*IMS-LÄSN SAKNAS, MEN BORDE VARA KVAR SE MOTSV. I WL013100...             
152400*FÖR ATT KUNNA LÄSA FRAM TILL NÄSTA ARTIKEL I ORDERDELEN.                 
152500*NY KOD ÄR ELSE OCH PERFORM RADERNA.                                      
152600              PERFORM IMS-13-GHN-WDQ401                                   
152700*                                                                         
152800           END-IF                                                         
152900           IF SEGMENT-FINNS                                               
153000              MOVE JA       TO ARTIKEL-SW                                 
153100              MOVE ZERO     TO WS-KVROS                                   
153200                               WS-KVLS                                    
153300                               WS-KVEFRS                                  
153400                               WS-KVRESS                                  
153500                               WS-KART-KVRESS-ART                         
153600                               WS-KVAVBART                                
153700                               WS-ANTOBKR                                 
153800              PERFORM CEB-LAES-ART-REG-WDD3                               
153900              PERFORM CEC-KOMPLETTERA-SPARRAR                             
154000              PERFORM CED-AVROP-ARBETSTABELL                              
154100              PERFORM CEM-KOMPLETTERA-RANSONERING                         
154200              PERFORM CEE-DEFINITIV-AVBOKNING                             
154300*LDC-GB                                                                   
154400              PERFORM CEF-EV-SPARA-I-LDC-TAB                              
154500                                                                          
154600              PERFORM CEG-UPPLAGG-PU-PLE                                  
154700                                                                          
154800              PERFORM CEL-CREATE-EVENT-151                                
154900                                                                          
155000              IF NOT  DCS-NDC AND                                         
155100                 NOT (DCS-SDC AND DCS-CHINA)                              
155200                 PERFORM S04-UPPDAT-ART-REG-WDK6                          
155300              END-IF                                                      
155400              PERFORM S07-BORTTAG-ORDERRAD                                
155500                                                                          
155600              PERFORM CEH-KONTROLL-SPLITGRANS                             
155700              PERFORM CEI-JUSTERA-VARDE-PA-ORDERDEL                       
155800              ADD 1 TO ORAD-IX                                            
155900           ELSE                                                           
156000              IF LOR-KVRADER = 0                                          
156100                 PERFORM IMS-DLET-WDQ221                                  
156200              END-IF                                                      
156300              MOVE JA TO LAGOMR-SW                                        
156400           END-IF                                                         
156500        END-PERFORM                                                       
156600                                                                          
156700        PERFORM IMS-GHNP-WDQ221                                           
156800     END-PERFORM                                                          
156900     .                                                                    
157000                                                                          
157100 CEB-LAES-ART-REG-WDD3            SECTION.                                
157200     MOVE 'CEB-LAES-ART-REG-WDD3'       TO WS-CURRENT-SECTION             
157300*****************************************                                 
157400*  OM KODEN I AREG-KDORDBEK = 58 EFTER  *                                 
157500*  SUBPROGRAMMET, INNEBÄR DET ETT       *                                 
157600*  ALLVARLIGT FEL OCH PROGRAMMET SKALL  *                                 
157700*  ABENDA.                              *                                 
157800*****************************************                                 
157900                                                                          
158000     MOVE ORAD-IDARTNR               TO AREG-IDARTNR                      
158100                                        WS-IDARTNR                        
158200     MOVE ORAD-IDDC                  TO AREG-IDDC                         
158300                                                                          
158400     CALL W411AREG USING AREG-W411AREG                                    
158500                         AREG-WDK6-PCB                                    
158600                         AREG-WDK7-PCB                                    
158700     IF AREG-KDORDBEK = 58                                                
158800        MOVE 'W411AREG GAV KOD 58 = ARTIKEL FEL2' TO ERROR-TEXT           
158900        CALL FELLOG                                                       
159000     ELSE                                                                 
159100        IF WDQ4-RAD                                                       
159200           PERFORM CEBA-HAMTA-BENAMNING                                   
159300        END-IF                                                            
159400     END-IF                                                               
159500     .                                                                    
159600 CEBA-HAMTA-BENAMNING SECTION.                                            
159700     MOVE 'CEBA-HAMTA-BENAMNING'  TO WS-CURRENT-SECTION                   
159800                                                                          
159900     MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
160000     IF DCS-UNICODE-IDSKYLT                                               
160100        MOVE 'UTF8'             TO TRAUTF8-KDCP                           
160200     ELSE                                                                 
160300        MOVE '278 '             TO TRAUTF8-KDCP                           
160400     END-IF                                                               
160500                                                                          
160600     MOVE AREG-IDARTNR TO W-D3BSEQ-IDARTNR                                
160700                                                                          
160800     PERFORM IMS-14-GU-WDD311                                             
160900     IF SEGMENT-FINNS                                                     
161000        MOVE TEXT-BEART       TO TRAUTF8-TECONV-FROM                      
161100     ELSE                                                                 
161200        MOVE SPACES           TO TRAUTF8-TECONV-FROM                      
161300     END-IF                                                               
161400                                                                          
161500     IF TRAUTF8-TECONV-FROM = SPACES                                      
161600       MOVE WS-IDSKYLT-GB     TO W-IDSKYLT                                
161700       MOVE '278 '            TO TRAUTF8-KDCP                             
161800       PERFORM IMS-14-GU-WDD311                                           
161900       IF SEGMENT-FINNS                                                   
162000         MOVE TEXT-BEART      TO TRAUTF8-TECONV-FROM                      
162100       ELSE                                                               
162200         MOVE 'NAME MISSING'  TO TRAUTF8-TECONV-FROM                      
162300       END-IF                                                             
162400     END-IF                                                               
162500*     -- STRIP TRAILING EBCDIC SPACE IN CHINESE TEXT                      
162600*     -- OR WESTERN EUROPEAN DESCRIPTIONS TO UNICODE                      
162700     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
162800*lk  IF DCS-CDC                                                           
162900*lk    MOVE TEXT-BEART        TO WS-BEART                                 
163000*lk  ELSE                                                                 
163100       MOVE TRAUTF8-TECONV-TO TO WS-BEART                                 
163200*lk  END-IF                                                               
163210                                                                          
163300     .                                                                    
163400 CEC-KOMPLETTERA-SPARRAR SECTION.                                         
163500     MOVE 'CEC-KOMPLETTERA-SPARRAR' TO WS-CURRENT-SECTION                 
163600                                                                          
163700     MOVE ORAD-BERADREF          TO SPAR-BERADREF                         
163800     MOVE OHUV-BEKUNDRF          TO SPAR-BEKUNDRF                         
163900     MOVE AREG-FLAVRART          TO SPAR-FLAVRART                         
164000     MOVE OHUV-FLEMBORD          TO SPAR-FLEMBORD                         
164100     MOVE OHUV-FLOVRLEV          TO SPAR-FLOVRLEV                         
164200     MOVE OHUV-FLORDSPE          TO SPAR-FLORDSPE                         
164300     MOVE OHUV-FLFORBI           TO SPAR-FLFORBI                          
164400     MOVE AREG-FLIART            TO SPAR-FLIART                           
164500     MOVE AREG-FLMARKSP          TO SPAR-FLMARKSP                         
164600     MOVE AREG-FLLSRDEL          TO SPAR-FLLSRDEL                         
164700     MOVE AREG-FLRADREF          TO SPAR-FLRADREF                         
164800     MOVE ORAD-FLRESTN           TO SPAR-FLRESTN                          
164900     MOVE ORAD-IDARTNR           TO SPAR-IDARTNR                          
165000     MOVE ORAD-IDDISTR           TO SPAR-IDDISTR                          
165100     MOVE ORAD-IDKUNDNR          TO SPAR-IDKUNDNR                         
165200     MOVE ORAD-IDKUNDRF-RO       TO SPAR-IDKUNDRF-RO                      
165300     MOVE ORAD-IDDC              TO SPAR-IDDC                             
165400     MOVE ORAD-IDSYSTEM          TO SPAR-IDSYSTEM                         
165500     MOVE AREG-KDERS-UTG         TO SPAR-KDERS-UTG                        
165600     MOVE AREG-KDERS             TO SPAR-KDERS                            
165700     MOVE OHUV-KDFAKTYP          TO SPAR-KDFAKTYP                         
165800     MOVE AREG-KDLEVSP           TO SPAR-KDLEVSP                          
165900     MOVE 7                      TO SPAR-KDORDBEH                         
166000     MOVE OHUV-KDORDKL           TO SPAR-KDORDKL                          
166100     MOVE AREG-KDPRODSL          TO SPAR-KDPRODSL                         
166200     MOVE AREG-KDSORT            TO SPAR-KDSORT                           
166300     MOVE ORAD-KDPRTYP           TO SPAR-KDPRTYP                          
166400     MOVE ORAD-KDTPOTYP          TO SPAR-KDTPOTYP                         
166500     MOVE AREG-KDUART            TO SPAR-KDUART                           
166600     MOVE AREG-PRARTSTD          TO SPAR-PRARTSTD                         
166700     MOVE AREG-TIFINLV           TO SPAR-TIFINLV                          
166800     MOVE ORAD-TIRODAT           TO SPAR-TIRODAT                          
166900     MOVE ORAD-TITPO             TO SPAR-TITPO                            
167000     MOVE ORAD-FLSDCLEV          TO SPAR-FLSDCLEV                         
167100     MOVE OHUV-TIREPDAT          TO SPAR-TIREPDAT                         
167200                                                                          
167300     CALL W411SPAR USING SPAR-W411SPAR SPAR-WDF8-PCB                      
167400                                       SPAR-WDF8A-PCB                     
167500                                       SPAR-WDK6-PCB                      
167600                                                                          
167700*ONE PART WHERE DATE ON CDC IS TO FAR AWAY BUT ITS OK DATE ON NDC.        
167800*FLPUBCDC IS SET IN W411SPAR                                              
167900                                                                          
168000     IF SPAR-FLPUBCDC = YES                                               
168100       MOVE 0                    TO SPAR-KDORDBEK                         
168200     END-IF                                                               
168300                                                                          
168400     IF SPAR-KDORDBEK > 0                                                 
168500                                                                          
168600        MOVE SPAR-KDORDBEK                   TO WS-KDORDBEK               
168700        MOVE '3410SPAR'                      TO WS-IDPGM                  
168800        MOVE ORAD-KVBEART-Q                  TO WS-KVROS                  
168900        PERFORM S02-ORDERBEKRAFTELSE                                      
169000        PERFORM S08-SKAPA-RYETRANS                                        
169100                                                                          
169200        IF  DCS-CDC                                                       
169300        AND ORAD-IDLEVNR    = SPACE                                       
169400        AND ORAD-TIRODAT    = 0                                           
169500        AND ORAD-IDKAMPRF   = 0                                           
169600        AND OHUV-FLORDSPE = NEJ                                           
169700        AND OHUV-FLOVRLEV = NEJ                                           
169800                                                                          
169900           PERFORM CECA-UPPDAT-WDK9                                       
170000        END-IF                                                            
170100                                                                          
170200          IF (DCS-SDC OR DCS-NDC)                                         
170300          AND OHUV-FLORDSPE = NEJ                                         
170400          AND OHUV-FLOVRLEV = NEJ                                         
170500                                                                          
170600             PERFORM CECB-UPPDAT-WDK7-K9                                  
170700          END-IF                                                          
170800                                                                          
170900        IF (DCS-CDC OR DCS-CDC-TR) AND                                    
171000           (ORAD-TIRODAT    > 0    OR                                     
171100            ORAD-IDKAMPRF   > 0)                                          
171200                                                                          
171300           PERFORM CECC-MINSKA-KVRESS-WDK6                                
171400        END-IF                                                            
171500                                                                          
171600        IF (DCS-CDC OR DCS-CDC-TR)                                        
171700        OR  DCS-NDC                                                       
171800        OR (DCS-SDC AND DCS-CHINA)                                        
171900          PERFORM CECD-EV-UPPDAT-WDK7-REFILL                              
172000        END-IF                                                            
172100                                                                          
172200        IF WS-KDORDBEK = '90' OR '91'                                     
172300*           90 = RESTNOTERAD, 91 = RESTNOTERAD IGEN.                      
172400                                                                          
172500           IF ORAD-IDKUNDRF-RO NOT = '00000     '                         
172600              AND                                                         
172700              ORAD-IDKUNDRF-RO NOT = '0000000   '                         
172800                                                                          
172900              IF ORAD-TIRODAT = ZERO                                      
173000                PERFORM S12-SKAPA-RYKTRANS                                
173100              END-IF                                                      
173200              PERFORM S10-UPPDAT-BEFINTLIG-RESTORDER                      
173300           ELSE                                                           
173400              MOVE 2 TO WS-KDROO                                          
173500              PERFORM S03-NYUPPLAGG-RESTORDER                             
173600              PERFORM S12-SKAPA-RYKTRANS                                  
173700           END-IF                                                         
173800           IF NOT  DCS-NDC AND                                            
173900              NOT (DCS-SDC AND DCS-CHINA)                                 
174000              PERFORM S04-UPPDAT-ART-REG-WDK6                             
174100           END-IF                                                         
174200        END-IF                                                            
174300                                                                          
174400        IF ORAD-KDORDKL = 0                                               
174500        AND VOR-KON                                                       
174600            IF  DCS-NDC OR (DCS-SDC AND DCS-CHINA)                        
174700                                                                          
174800                PERFORM S11-UPPDAT-VOR                                    
174900            ELSE                                                          
175000                PERFORM S11D-UPDATE-VORKONY                               
175100            END-IF                                                        
175200           PERFORM S23-DELETE-PRICE-Q-LINE                                
175300        END-IF                                                            
175400                                                                          
175500        PERFORM S07-BORTTAG-ORDERRAD                                      
175600        MOVE NEJ TO ARTIKEL-SW                                            
175700     END-IF                                                               
175800     .                                                                    
175900 CECA-UPPDAT-WDK9 SECTION.                                                
176000     MOVE 'STA CEBA-UPPDAT'         TO WS-CURRENT-SECTION                 
176100                                                                          
176200     MOVE ORAD-IDARTNR TO W-IDARTNR                                       
176300                                                                          
176400     PERFORM IMS-GHU-WDK901                                               
176500                                                                          
176600     IF SEGMENT-FINNS                                                     
176700        IF ORAD-KDORDKL = 0                                               
176800                                                                          
176900           IF VOR-KON                                                     
177000              CONTINUE                                                    
177100           ELSE                                                           
177200              COMPUTE ART-KVOKS-VOR =                                     
177300                      ART-KVOKS-VOR - ORAD-KVBEART-Q                      
177400              END-COMPUTE                                                 
177500           END-IF                                                         
177600                                                                          
177700           COMPUTE ART-KVPREAVB-VOR =                                     
177800                   ART-KVPREAVB-VOR - ORAD-KVPREAVB                       
177900           END-COMPUTE                                                    
178000        ELSE                                                              
178100           IF ORAD-KDORDKL = 1                                            
178200              COMPUTE ART-KVOKS-DAG =                                     
178300                      ART-KVOKS-DAG - ORAD-KVBEART-Q                      
178400              END-COMPUTE                                                 
178500                                                                          
178600              COMPUTE ART-KVPREAVB-DAG =                                  
178700                      ART-KVPREAVB-DAG - ORAD-KVPREAVB                    
178800              END-COMPUTE                                                 
178900                                                                          
179000              COMPUTE ART-KVPRERO-DAG =                                   
179100                      ART-KVPRERO-DAG - ORAD-KVPRERO                      
179200              END-COMPUTE                                                 
179300           ELSE                                                           
179400              COMPUTE ART-KVOKS-BULK =                                    
179500                      ART-KVOKS-BULK - ORAD-KVBEART-Q                     
179600              END-COMPUTE                                                 
179700                                                                          
179800              COMPUTE ART-KVPREAVB-BULK =                                 
179900                      ART-KVPREAVB-BULK - ORAD-KVPREAVB                   
180000              END-COMPUTE                                                 
180100                                                                          
180200              COMPUTE ART-KVPRERO-BULK =                                  
180300                      ART-KVPRERO-BULK - ORAD-KVPRERO                     
180400              END-COMPUTE                                                 
180500           END-IF                                                         
180600        END-IF                                                            
180700        PERFORM IMS-REPL-WDK901                                           
180800     END-IF                                                               
180900     MOVE 'END CEBA-UPPDAT'         TO WS-CURRENT-SECTION                 
181000     .                                                                    
181100     EJECT                                                                
181200                                                                          
181300 CECB-UPPDAT-WDK7-K9 SECTION.                                             
181400     MOVE 'CECB-UPPDAT-WDK7   '     TO WS-CURRENT-SECTION                 
181500                                                                          
181600     MOVE ORAD-IDARTNR TO W-IDARTNR                                       
181700     MOVE ORAD-IDDC    TO W-IDDC                                          
181800                                                                          
181900     PERFORM IMS-GU-WDK722                                                
182000     IF SEGMENT-FINNS                                                     
182100        MOVE JA  TO K722-SW                                               
182200     ELSE                                                                 
182300        MOVE NEJ TO K722-SW                                               
182400     END-IF                                                               
182500     PERFORM IMS-17-GHU-WDK711                                            
182600     IF  ORAD-KDORDKL > +1                                                
182700         SUBTRACT ORAD-KVBEART-Q        FROM SLAG-KVOKS-BULK              
182800     ELSE                                                                 
182900         SUBTRACT ORAD-KVBEART-Q        FROM SLAG-KVOKS-DAG               
183000     END-IF                                                               
183100                                                                          
183200*--- GÄLLER NDC OCH KINA-LDC                                              
183300     IF  DCS-NDC OR (DCS-SDC AND DCS-CHINA)                               
183400       IF  ORAD-TIRODAT > 0                                               
183500           SUBTRACT ORAD-KVBEART-Q    FROM SLAG-KVRESS                    
183600       END-IF                                                             
183700                                                                          
183800       IF (WS-KDORDBEK = '90'                                             
183900       OR  WS-KDORDBEK = '91')                                            
184000       AND ORAD-IDDC = ORAD-IDDC-RO                                       
184100       AND WS-KVROS > 0                                                   
184200         IF  DCS-NDC-CN                                                   
184300         OR (DCS-NDC-NA AND DCS-USA)                                      
184400            IF SLAG-IDDC-REF = SPACE                                      
184500              PERFORM S04B-EV-LARM-2191-MID-CN-US                         
184600            END-IF                                                        
184700         END-IF                                                           
184800         IF  ORAD-KDORDKL > +1                                            
184900           ADD WS-KVROS                 TO SLAG-KVROS-BULK                
185000         ELSE                                                             
185100           ADD WS-KVROS                 TO SLAG-KVROS-DAG                 
185200         END-IF                                                           
185300       END-IF                                                             
185400                                                                          
185500       IF ORAD-KDORDKL = 0                                                
185600       AND VOR-KON                                                        
185700         ADD ORAD-KVBEART-Q             TO SLAG-KVOKS-DAG                 
185800       END-IF                                                             
185900     END-IF                                                               
186000                                                                          
186100     PERFORM IMS-18-REPL-WDK711                                           
186200                                                                          
186300*------- RESTNOTERING PÅ ANNAT NDC                                        
186400     IF  DCS-NDC OR (DCS-SDC AND DCS-CHINA)                               
186500       IF (WS-KDORDBEK = '90'                                             
186600       OR  WS-KDORDBEK = '91')                                            
186700       AND ORAD-IDDC NOT = ORAD-IDDC-RO                                   
186800       AND WS-KVROS > 0                                                   
186900           MOVE ORAD-IDDC-RO          TO W-IDDC                           
187000           PERFORM IMS-GU-WDK722                                          
187100           IF SEGMENT-FINNS                                               
187200              MOVE JA  TO K722-SW                                         
187300           ELSE                                                           
187400              MOVE NEJ TO K722-SW                                         
187500           END-IF                                                         
187600           PERFORM IMS-17-GHU-WDK711                                      
187700           IF  DCS-NDC-CN                                                 
187800           OR (DCS-NDC-NA AND DCS-USA)                                    
187900              IF SLAG-IDDC-REF = SPACE                                    
188000                PERFORM S04B-EV-LARM-2191-MID-CN-US                       
188100              END-IF                                                      
188200           END-IF                                                         
188300           IF  ORAD-KDORDKL > +1                                          
188400             ADD WS-KVROS                 TO SLAG-KVROS-BULK              
188500           ELSE                                                           
188600             ADD WS-KVROS                 TO SLAG-KVROS-DAG               
188700           END-IF                                                         
188800           PERFORM IMS-18-REPL-WDK711                                     
188900       END-IF                                                             
189000     END-IF                                                               
189100                                                                          
189200*--- GÄLLER SDC FÖRUTOM KINA-LDC                                          
189300     IF  ORAD-KDORDKL = 0                                                 
189400     AND VOR-KON                                                          
189500     AND (DCS-SDC AND NOT DCS-CHINA)                                      
189600       PERFORM IMS-GHU-WDK901                                             
189700       COMPUTE ART-KVOKS-VOR =                                            
189800               ART-KVOKS-VOR + ORAD-KVBEART-Q                             
189900                                                                          
190000       PERFORM IMS-REPL-WDK901                                            
190100     END-IF                                                               
190200     MOVE 'END CEBB-UPPDAT'         TO WS-CURRENT-SECTION                 
190300     .                                                                    
190400     EJECT                                                                
190500                                                                          
190600 CECC-MINSKA-KVRESS-WDK6 SECTION.                                         
190700     MOVE 'STA CEBC-MINSKA'         TO WS-CURRENT-SECTION                 
190800                                                                          
190900     MOVE ORAD-IDARTNR              TO W-IDARTNR                          
191000     PERFORM IMS-21-GHU-WDK611                                            
191100     IF SEGMENT-FINNS                                                     
191200        SUBTRACT ORAD-KVBEART-Q     FROM CLAG-KVRESS                      
191300        PERFORM IMS-22-REPL-WDK611                                        
191400     END-IF                                                               
191500     MOVE 'END CEBC-MINSKA'         TO WS-CURRENT-SECTION                 
191600     .                                                                    
191700     EJECT                                                                
191800                                                                          
191900 CECD-EV-UPPDAT-WDK7-REFILL         SECTION.                              
192000     MOVE 'CECD-EV-UPPDAT-WDK7'     TO WS-CURRENT-SECTION                 
192100                                                                          
192200******************************************************************        
192300*                                                                         
192400*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
192500*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
192600*                                                                         
192700******************************************************************        
192800                                                                          
192900     MOVE OHUV-IDDISTR         TO W-TP4TRAN-IDDISTR                       
193000                                                                          
193100     PERFORM DB2-SELECT-TP4TRAN                                           
193200                                                                          
193300                                                                          
193400*REFILLORDERRAD TILL  MED SPÄRRAD ARTIKEL.                                
193500     MOVE OHUV-IDDISTR         TO DIST35-IDDISTR                          
193600     MOVE OHUV-IDDISTR         TO WS-IDDISTR-NUM5                         
193700                                                                          
193800     IF  (DIST35-REFILL                                                   
193900     OR   DIST35-REFILL-INOM-NDC                                          
194000     OR   DIST35-NA-TRANSFER                                              
194100     OR   DIST35-NA-NDC-RETURNS                                           
194200     OR   DIST35-PACIFIC-TRANSFER                                         
194300     OR   DIST35-REFILL-INOM-JP                                           
194400     OR   DIST35-CN-TRANSFER                                              
194500     OR   RADER-FINNS)                                                    
194600      AND WS-KDORDBEK NOT = '90'                                          
194700      AND WS-KDORDBEK NOT = '91'                                          
194800                                                                          
194900       IF RADER-FINNS                                                     
195000         MOVE TP4TRAN-IDDC-REC TO W-IDDC                                  
195100       ELSE                                                               
195200         SEARCH ALL DIST57-REFILL-DC                                      
195300            AT END                                                        
195400               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
195500                                TO ERROR-TEXT                             
195600               CALL FELLOG                                                
195700            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = WS-IDDISTR-NUM5          
195800               MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-IDDC              
195900         END-SEARCH                                                       
196000       END-IF                                                             
196100                                                                          
196200       MOVE ORAD-IDARTNR TO W-IDARTNR                                     
196300                                                                          
196400       PERFORM IMS-GU-WDK722                                              
196500       IF SEGMENT-FINNS                                                   
196600          MOVE JA  TO K722-SW                                             
196700       ELSE                                                               
196800          MOVE NEJ TO K722-SW                                             
196900       END-IF                                                             
197000       PERFORM IMS-17-GHU-WDK711                                          
197100       SUBTRACT ORAD-KVBEART-Q      FROM SLAG-KVBEART                     
197200                                                                          
197300       PERFORM IMS-18-REPL-WDK711                                         
197400     ELSE                                                                 
197500       IF (DIST35-NONVCC-REFILL        OR                                 
197600           DIST35-NONVCC-NONVCC-TRANSFER)                                 
197700        AND WS-KDORDBEK NOT = '90'                                        
197800        AND WS-KDORDBEK NOT = '91'                                        
197900                                                                          
198000           SEARCH ALL DIST57-REFILL-DC                                    
198100              AT END                                                      
198200                 MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                     
198300                                  TO ERROR-TEXT                           
198400                 CALL FELLOG                                              
198500              WHEN DIST57-SOK-IDDISTR(DIST57-IX) = WS-IDDISTR-NUM5        
198600                 MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO W-IDDC            
198700           END-SEARCH                                                     
198800                                                                          
198900           MOVE ORAD-IDARTNR TO W-IDARTNR                                 
199000           PERFORM IMS-21-GHU-WDK611                                      
199100           SUBTRACT ORAD-KVBEART-Q   FROM CLAG-KVBEART                    
199200                                                                          
199300           PERFORM IMS-22-REPL-WDK611                                     
199400       END-IF                                                             
199500     END-IF                                                               
199600     .                                                                    
199700     SKIP2                                                                
199800 CED-AVROP-ARBETSTABELL SECTION.                                          
199900     MOVE 'CED-AVROP-ARBETSTABELL' TO WS-CURRENT-SECTION                  
200000                                                                          
200100     COMPUTE WS-ORDERVIKT-PER-RAD =                                       
200200            ((ORAD-KVBEART-Q * ORAD-VKART) / 1000)                        
200300                                                                          
200400     COMPUTE WS-ORDERVOLYM-PER-RAD =                                      
200500            ((ORAD-KVBEART-Q * ORAD-VLARTNTO) / 1000000)                  
200600                                                                          
200700     SUBTRACT 1                     FROM LOR-KVRADER                      
200800     SUBTRACT WS-ORDERVIKT-PER-RAD  FROM LOR-VKORDNTO                     
200900     SUBTRACT WS-ORDERVOLYM-PER-RAD FROM LOR-VLORDNTO                     
201000                                                                          
201100     PERFORM IMS-REPL-WDQ221                                              
201200                                                                          
201300     IF ORAD-KDSPEEMB > 0                                                 
201400        IF ORAD-IDSPECEMB = 0                                             
201500           IF  WS-KVSEMBRA > 0                                            
201600              SUBTRACT 1 FROM  WS-KVSEMBRA                                
201700           END-IF                                                         
201800        END-IF                                                            
201900     END-IF                                                               
202000     .                                                                    
202100                                                                          
202200 CEE-DEFINITIV-AVBOKNING SECTION.                                         
202300     MOVE 'CEE-DEFINITIV-AVBOKNING' TO WS-CURRENT-SECTION                 
202400                                                                          
202500     IF ARTIKEL-OK                                                        
202600        IF (((OHUV-FLORDSPE = JA AND OHUV-IDSYSTEM NOT = 'W216')          
202700         OR  OHUV-FLOVRLEV = JA)                                          
202800         AND OHUV-FLLSBOK = JA)                                           
202900        OR   OHUV-FLLSBOK = NEJ                                           
203000        OR   ORAD-IDLEVNR NOT = SPACE                                     
203100            MOVE ORAD-KVBEART    TO WS-KVLS                               
203200                                    WS-KVEFRS                             
203300                                    WS-KVAVBART                           
203400            MOVE ZERO            TO WS-KDORDBEK                           
203500                                                                          
203600           MOVE OHUV-IDDISTR     TO DIST18-IDDISTR                        
203700           IF ((((DCS-CDC OR DCS-CDC-TR)                                  
203800           AND DIST18-SKROT-KVAL-CDC)                                     
203900           OR  DCS-SDC OR  DCS-NDC)                                       
204000           AND OHUV-FLLSBOK = JA)                                         
204100                PERFORM CEEA-DEAV-AVBOK                                   
204200           ELSE                                                           
204300*SVS FROG                                                                 
204400             MOVE SPACE          TO DEAV-FLAKPLOC-UT                      
204500             MOVE ZERO           TO DEAV-RERF-RAD-UT                      
204600                                    DEAV-KDORDBEK-UT                      
204700                                    DEAV-KVEFRS-UT                        
204800                                    DEAV-KVLS-UT                          
204900                                    DEAV-KVRESS-UT                        
205000                                    DEAV-KVROS-UT                         
205100                                    DEAV-KVAVBART-UT                      
205200                                    DEAV-KDROO-UT                         
205300                                    DEAV-KVBEART-Q-IN                     
205400           END-IF                                                         
205500                                                                          
205600           IF WS-KDORDBEK > 0                                             
205700                                                                          
205800              PERFORM S02-ORDERBEKRAFTELSE                                
205900              PERFORM S08-SKAPA-RYETRANS                                  
206000           END-IF                                                         
206100                                                                          
206200*------ TILLFÄLLIGT INLAGT FÖR FIL TILL LOGISTIK                          
206300*       OBS! LÄGG INGET MELLAN FÖREGÅENDE IF-SATS                         
206400*       OCH IF-SATSEN NEDAN.                                              
206500                                                                          
206600           IF (WS-KDORDBEK = 90                                           
206700           OR  WS-KDORDBEK = 91)                                          
206800           AND DEAV-IDDC-IN = 11                                          
206900             PERFORM S98-SKAPA-RYXTRANS                                   
207000           ELSE                                                           
207100             IF  DEAV-KVBEART-Q-IN > DEAV-KVAVBART-UT                     
207200             AND DEAV-KVAVBART-UT > 0                                     
207300             AND DEAV-IDDC-IN = 11                                        
207400               PERFORM S99-SKAPA-RYXTRANS                                 
207500             END-IF                                                       
207600           END-IF                                                         
207700*------ END                                                               
207800                                                                          
207900           IF WS-KVAVBART = 0                                             
208000              IF NOT DCS-NDC AND                                          
208100                 NOT (DCS-SDC AND DCS-CHINA)                              
208200*SHOULD 'NOT (DCS-SDC AND DCS-CHINA)' BE HERE COPIED FROM W4037500        
208300                 PERFORM S04-UPPDAT-ART-REG-WDK6                          
208400              END-IF                                                      
208500              PERFORM S07-BORTTAG-ORDERRAD                                
208600              MOVE NEJ TO ARTIKEL-SW                                      
208700           END-IF                                                         
208800        ELSE                                                              
208900           PERFORM CEEA-DEAV-AVBOK                                        
209000                                                                          
209100           IF WS-KDORDBEK > 0                                             
209200                                                                          
209300              PERFORM S02-ORDERBEKRAFTELSE                                
209400              PERFORM S08-SKAPA-RYETRANS                                  
209500                                                                          
209600           END-IF                                                         
209700                                                                          
209800*------ TILLFÄLLIGT INLAGT FÖR FIL TILL LOGISTIK                          
209900*       OBS! LÄGG INGET MELLAN FÖREGÅENDE IF-SATS                         
210000*       OCH IF-SATSEN NEDAN.                                              
210100                                                                          
210200           IF (WS-KDORDBEK = 90                                           
210300           OR  WS-KDORDBEK = 91)                                          
210400           AND DEAV-IDDC-IN = 11                                          
210500             PERFORM S98-SKAPA-RYXTRANS                                   
210600           ELSE                                                           
210700             IF  DEAV-KVBEART-Q-IN > DEAV-KVAVBART-UT                     
210800             AND DEAV-KVAVBART-UT > 0                                     
210900             AND DEAV-IDDC-IN = 11                                        
211000               PERFORM S99-SKAPA-RYXTRANS                                 
211100             END-IF                                                       
211200           END-IF                                                         
211300*------ END                                                               
211400                                                                          
211500*                KDORDBEK 92 = VOR, RESTNOTERAD KVANT                     
211600           IF WS-KDORDBEK = 92                                            
211700              IF DCS-NDC OR (DCS-SDC AND DCS-CHINA)                       
211800                  MOVE ORAD-IDARTNR TO W-IDARTNR                          
211900                  MOVE ORAD-IDDC    TO W-IDDC                             
212000                  PERFORM IMS-GU-WDK722                                   
212100                  IF SEGMENT-FINNS                                        
212200                     MOVE JA  TO K722-SW                                  
212300                  ELSE                                                    
212400                     MOVE NEJ TO K722-SW                                  
212500                  END-IF                                                  
212600                  PERFORM S11-UPPDAT-VOR                                  
212700              ELSE                                                        
212800                  PERFORM S11D-UPDATE-VORKONY                             
212900              END-IF                                                      
213000              PERFORM S23-DELETE-PRICE-Q-LINE                             
213100           END-IF                                                         
213200                                                                          
213300           IF WS-KVROS > 0                                                
213400                                                                          
213500              IF ORAD-IDKUNDRF-RO NOT = '00000     '                      
213600                 AND                                                      
213700                 ORAD-IDKUNDRF-RO NOT = '0000000   '                      
213800                                                                          
213900                                                                          
214000                 IF ORAD-TIRODAT = ZERO                                   
214100                   PERFORM S12-SKAPA-RYKTRANS                             
214200                 END-IF                                                   
214300                                                                          
214400                 PERFORM S10-UPPDAT-BEFINTLIG-RESTORDER                   
214500              ELSE                                                        
214600                 PERFORM S03-NYUPPLAGG-RESTORDER                          
214700                 IF WS-KDORDBEK = 90                                      
214800                   PERFORM S12-SKAPA-RYKTRANS                             
214900                 END-IF                                                   
215000              END-IF                                                      
215100           END-IF                                                         
215200                                                                          
215300           IF WS-KVAVBART = 0                                             
215400              IF NOT DCS-NDC AND                                          
215500                 NOT (DCS-SDC AND DCS-CHINA)                              
215600*SHOULD 'NOT (DCS-SDC AND DCS-CHINA)' BE HERE COPIED FROM W4037500        
215700                 PERFORM S04-UPPDAT-ART-REG-WDK6                          
215800              END-IF                                                      
215900              PERFORM S07-BORTTAG-ORDERRAD                                
216000              MOVE NEJ TO ARTIKEL-SW                                      
216100           END-IF                                                         
216200        END-IF                                                            
216300     END-IF                                                               
216400     .                                                                    
216500                                                                          
216600 CEEA-DEAV-AVBOK SECTION.                                                 
216700     MOVE 'CEEA-DEAV-AVBOK'         TO WS-CURRENT-SECTION                 
216800                                                                          
216900     MOVE ORAD-IDKAMPRF             TO DEAV-IDKAMPRF-IN                   
217000     MOVE OHUV-FLFORBI              TO DEAV-FLFORBI-IN                    
217100     MOVE ORAD-IDARTNR              TO DEAV-IDARTNR-IN                    
217200     MOVE AREG-IDANSK               TO DEAV-IDANSK-IN                     
217300     MOVE ORAD-TIRODAT              TO DEAV-TIRODAT-IN                    
217400     MOVE ORAD-KDORDKL              TO DEAV-KDORDKL-IN                    
217500     MOVE ORAD-IDDC                 TO DEAV-IDDC-IN                       
217600     MOVE ORAD-IDDC-RO              TO DEAV-IDDC-RO-IN                    
217700     MOVE ORAD-IDDISTR              TO DEAV-IDDISTR-IN                    
217800     MOVE AREG-KDLEVSP              TO DEAV-KDLEVSP-IN                    
217900     MOVE AREG-IDFKNGRP             TO DEAV-IDFKNGRP-IN                   
218000     IF DCS-CDC                                                           
218100       MOVE AREG-KVAKS-CDC          TO DEAV-KVAKS-CDC-IN                  
218200       MOVE AREG-KVAKS-PAV          TO DEAV-KVAKS-PAV-IN                  
218300       MOVE AREG-KVLS               TO DEAV-KVLS-IN                       
218400       MOVE AREG-KVRESS             TO DEAV-KVRESS-IN                     
218500       MOVE AREG-KVUTRS             TO DEAV-KVUTRS-IN                     
218600       MOVE AREG-KVQPACK-0          TO DEAV-KVQPACK-0-IN                  
218700       MOVE AREG-KVQPACK-1          TO DEAV-KVQPACK-1-IN                  
218800       MOVE AREG-KDSORT             TO DEAV-KDSORT-IN                     
218900       MOVE AREG-KVSPARR-KVAL       TO DEAV-KVSPARR-KVAL-IN               
219000       MOVE RANS-RERF-ART-UT        TO DEAV-RERF-ART-IN                   
219100       MOVE RANS-RERF-RAD-UT        TO DEAV-RERF-RAD-NY-IN                
219200     ELSE                                                                 
219300       MOVE ZERO                    TO DEAV-KVAKS-CDC-IN                  
219400                                       DEAV-KVAKS-PAV-IN                  
219500                                       DEAV-KVLS-IN                       
219600                                       DEAV-KVRESS-IN                     
219700                                       DEAV-KVUTRS-IN                     
219800                                       DEAV-RERF-ART-IN                   
219900                                       DEAV-RERF-RAD-NY-IN                
220000                                       DEAV-KVQPACK-0-IN                  
220100                                       DEAV-KVQPACK-1-IN                  
220200                                       DEAV-KDSORT-IN                     
220300                                       DEAV-KVSPARR-KVAL-IN               
220400     END-IF                                                               
220500*    we always move qpack and sort to W411deav                            
220600*    and then in W411deav if not cdc move zero                            
220700*    when calling W411kvan                                                
220800     MOVE ORAD-KDPRODSL             TO DEAV-KDPRODSL-IN                   
220900     MOVE ORAD-FLAKPLOC             TO DEAV-FLAKPLOC-IN                   
221000     MOVE ORAD-KVBEART-Q            TO DEAV-KVBEART-Q-IN                  
221100     MOVE ORAD-KVPREAVB             TO DEAV-KVPREAVB-IN                   
221200     MOVE ORAD-KVPRERO              TO DEAV-KVPRERO-IN                    
221300     MOVE ORAD-IDKUNDRF-RO          TO DEAV-IDKUNDRF-RO-IN                
221400     MOVE ORAD-RERF-RAD             TO DEAV-RERF-RAD-IN                   
221500     MOVE ORAD-IDLEVNR              TO DEAV-IDLEVNR-IN                    
221600     MOVE ORAD-KDKVBRYT             TO DEAV-KDKVBRYT-IN                   
221700     MOVE ORAD-FLRESTN              TO DEAV-FLRESTN-IN                    
221800     MOVE ORAD-KDTPOTYP             TO DEAV-KDTPOTYP-IN                   
221900     MOVE ORAD-IDSYSTEM             TO DEAV-IDSYSTEM-IN                   
222000     MOVE ORAD-ADLAGOMR             TO DEAV-ADLAGOMR-IN                   
222100     MOVE OHUV-FLORDSPE             TO DEAV-FLORDSPE-IN                   
222200     MOVE OHUV-FLOVRLEV             TO DEAV-FLOVRLEV-IN                   
222300     MOVE ORAD-IDKUNDNR             TO DEAV-IDKUNDNR-IN                   
222400     MOVE ORAD-IDORDNR7             TO DEAV-IDORDNR5-IN                   
222500     MOVE W-Q301KY-IDPRODNR         TO DEAV-IDPRODNR-IN                   
222600     MOVE W-Q301KY-IDPLKLST         TO DEAV-IDPLKLST-IN                   
222700     MOVE ORAD-BERADREF             TO DEAV-BERADREF-IN                   
222800     MOVE ORAD-FLSDCLEV             TO DEAV-FLSDCLEV-IN                   
222900                                                                          
223000     IF ORAD-IDDC NOT = DCS-IDDC                                          
223100        MOVE ORAD-IDDC TO W-IDDC-B6                                       
223200        PERFORM IMS-05-GU-WDB601                                          
223300     END-IF                                                               
223400     MOVE AREG-KVSPANT              TO DEAV-KVSPANT-IN                    
223500     MOVE ARB-KDFDKRAV              TO DEAV-KDFDKRAV-IN                   
223600                                                                          
223700     CALL W411DEAV USING DEAV-W411DEAV DEAV-ARTM-PCB                      
223800                                       DEAV-WDK7-PCB                      
223900                                       DEAV-WDB6-PCB                      
224000                                       WLLOGA-PCB                         
224100                                       WDK6-PCB                           
224200                                       DEAV-WDB2-PCB                      
224300                                       DEAV-WDL7-PCB                      
224400                                       DEAV-WDK72-PCB                     
224500                                       DEAV-WDR2-PCB                      
224600                                       DEAV-WDR5-PCB                      
224700                                       DEAV-WDC1-PCB                      
224800                                                                          
224900     MOVE DEAV-FLAKPLOC-UT          TO ORAD-FLAKPLOC                      
225000     MOVE DEAV-RERF-RAD-UT          TO ORAD-RERF-RAD                      
225100     MOVE DEAV-KDORDBEK-UT          TO WS-KDORDBEK                        
225200     MOVE '3410DEAV'                TO WS-IDPGM                           
225300                                                                          
225400     MOVE DEAV-KVEFRS-UT            TO WS-KVEFRS                          
225500                                                                          
225600     MOVE DEAV-KVLS-UT              TO WS-KVLS                            
225700                                                                          
225800     MOVE DEAV-KVRESS-UT            TO WS-KVRESS                          
225900     MOVE DEAV-KVROS-UT             TO WS-KVROS                           
226000     MOVE DEAV-KVAVBART-UT          TO WS-KVAVBART                        
226100     MOVE DEAV-KDROO-UT             TO WS-KDROO                           
226200     .                                                                    
226300                                                                          
226400 CEF-EV-SPARA-I-LDC-TAB          SECTION.                                 
226500     MOVE 'CEF-EV-SPARA-I-LDC-TAB'  TO WS-CURRENT-SECTION                 
226600                                                                          
226700     MOVE OHUV-IDDISTR            TO DIST34-IDDISTR                       
226800     IF NOT DCS-CDC                                                       
226900     IF  DIST34-ENGLAND-SDC                                               
227000     AND GMT-FLLDCKND = JA                                                
227100       IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                           
227200       OR  OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                          
227300                                                                          
227400         MOVE +1        TO LDC-TAB-IX                                     
227500         MOVE +100      TO LDC-TAB-IX-MAX                                 
227600         PERFORM UNTIL ORAD-BERADREF = LDC-TAB-WIPID (LDC-TAB-IX)         
227700                    OR LDC-TAB-WIPID (LDC-TAB-IX) = HIGH-VALUE            
227800                    OR LDC-TAB-IX = LDC-TAB-IX-MAX                        
227900           ADD 1         TO LDC-TAB-IX                                    
228000         END-PERFORM                                                      
228100                                                                          
228200         IF ORAD-BERADREF = LDC-TAB-WIPID (LDC-TAB-IX)                    
228300            MOVE LDC-TAB-IX          TO WS-WIP-LOPNR                      
228400            MOVE '-'                 TO WS-WIP-STRECK                     
228500            MOVE ORAD-BERADREF       TO WS-WIPID                          
228600         ELSE                                                             
228700            MOVE LDC-TAB-IX     TO WS-WIP-LOPNR                           
228800            MOVE '-'            TO WS-WIP-STRECK                          
228900            MOVE ORAD-BERADREF  TO LDC-TAB-WIPID  (LDC-TAB-IX)            
229000                                   WS-WIPID                               
229100         END-IF                                                           
229200       END-IF                                                             
229300     END-IF                                                               
229400     END-IF                                                               
229500     .                                                                    
229600                                                                          
229700 CEG-UPPLAGG-PU-PLE SECTION.                                              
229800     MOVE 'CEG-UPPLAGG-PU-PLE'     TO WS-CURRENT-SECTION                  
229900                                                                          
230000     IF ARTIKEL-OK                                                        
230100        MOVE ORAD-IDARTNR          TO W-IDARTNR                           
230200        PERFORM IMS-21-GHU-WDK611                                         
230300        IF SEGMENT-FINNS                                                  
230400           MOVE CLAG-KVQPACK-3     TO 4010-KVQPACK-3                      
230500           MOVE CLAG-KDARTHNT      TO WS-KDARTHNT                         
230600*CLAG-ADLAGOMR ETC GER K6-ART.ADRESS PÅ PU/PE IST FÖR Q4-ADRESS.          
230700*MEN KAN BLI OSORTERAT VID BIPACKN.                                       
230800           IF DCS-CDC                                                     
230900              PERFORM S06Z-KOLLA-AENDRA-LAGOMR                            
231000              IF AENDRA-LAGOMR                                            
231100                MOVE CLAG-ADLAGOMR TO WS-ADLAGOMR                         
231200                MOVE CLAG-ADGANG   TO WS-ADGANG                           
231300                MOVE CLAG-ADPLATS  TO WS-ADPLATS                          
231400              ELSE                                                        
231500                MOVE ORAD-ADLAGOMR TO WS-ADLAGOMR                         
231600                MOVE ORAD-ADGANG   TO WS-ADGANG                           
231700                MOVE ORAD-ADPLATS  TO WS-ADPLATS                          
231800              END-IF                                                      
231900*SVS FROG                                                                 
232000              MOVE ORAD-IDDISTR   TO TEST-IDDISTR                         
232100              IF DIST20-EMBALLAGE-SVS                                     
232200              OR LOR-IDPRC = 2600                                         
232300                                                                          
232400                IF CLAG-ADLAGOMR-SVS > ZERO                               
232500                  MOVE CLAG-ADLAGOMR-SVS TO WS-ADLAGOMR                   
232600                  MOVE CLAG-ADGANG-SVS TO WS-ADGANG                       
232700                  MOVE CLAG-ADPLATS-SVS TO WS-ADPLATS                     
232800                END-IF                                                    
232900              END-IF                                                      
233000           ELSE                                                           
233100*FIX PLATS BYTE                                                           
233200              MOVE ORAD-IDARTNR    TO W-IDARTNR                           
233300              MOVE ORAD-IDDC       TO W-IDDC                              
233400              PERFORM IMS-17-GHU-WDK711                                   
233500              MOVE SLAG-ADLAGOMR   TO WS-ADLAGOMR                         
233600              MOVE SLAG-ADGANG     TO WS-ADGANG                           
233700              MOVE SLAG-ADPLATS    TO WS-ADPLATS                          
233800           END-IF                                                         
233900*CLAG-ADLAGOMR ETC GER K6-ART.ADRESS PÅ PU/PE IST FÖR Q4-ADRESS.          
234000        ELSE                                                              
234100           MOVE +0                 TO WS-KDARTHNT                         
234200           MOVE +0                 TO 4010-KVQPACK-3                      
234300           MOVE ORAD-ADLAGOMR      TO WS-ADLAGOMR                         
234400           MOVE ORAD-ADGANG        TO WS-ADGANG                           
234500           MOVE ORAD-ADPLATS       TO WS-ADPLATS                          
234600        END-IF                                                            
234700*FIX END PLATS BYTE                                                       
234800        IF PLOCKSATS-ETIK-SAKNAS                                          
234900           PERFORM S09-SKAPA-PLOCKSATS-ETIK                               
235000        END-IF                                                            
235100                                                                          
235200        IF PLOCKSATS-PU-SAKNAS                                            
235300           PERFORM S05-SKAPA-PLOCKSATS-PU                                 
235400        END-IF                                                            
235500                                                                          
235600        IF OHUV-BEVARREF = SPACE                                          
235700          MOVE ORAD-BERADREF TO WS-HFAK-REF-X10                           
235800        ELSE                                                              
235900          MOVE OHUV-BEVARREF TO WS-HFAK-REF-X10                           
236000          PERFORM CEGB-KOLLA-I-HFAK-TAB                                   
236100          IF BEVARREF-I-HFAK-TAB                                          
236200            MOVE OHUV-BEVARREF TO WS-HFAK-REF-X10                         
236300          ELSE                                                            
236400            MOVE ORAD-BERADREF TO WS-HFAK-REF-X10                         
236500          END-IF                                                          
236600        END-IF                                                            
236700                                                                          
236800        IF OHUV-BEVARREF   = 'RENOVA    '                                 
236900        OR ORAD-BERADREF   = 'RENOVA    '                                 
237000        OR WS-HFAK-REF-X10 = 'RENOVA    '                                 
237100           MOVE SPACE      TO WS-HFAK-REF-X10                             
237200        END-IF                                                            
237300                                                                          
237400        ADD +1       TO WS-IDRADNR-SISTA                                  
237500        PERFORM CEGC-HAMTA-KDARTURS-IDPSN                                 
237600        PERFORM CEGD-SKAPA-PLE                                            
237700        PERFORM CEGE-SKAPA-PU                                             
237800        ADD 1 TO 4004-KVRADER (100)                                       
237900     END-IF                                                               
238000     .                                                                    
238100                                                                          
238200 CEGB-KOLLA-I-HFAK-TAB   SECTION.                                         
238300     MOVE 'CEGB-KOLLA-I-HFAK-TAB  ' TO WS-CURRENT-SECTION                 
238400                                                                          
238500     IF WS-HFAK-REF-X10 NOT = SPACE                                       
238600        MOVE 1 TO HFAK-TAB-IX                                             
238700        PERFORM UNTIL HFAK-TAB-IX > MAX-HFAK-IX                           
238800           IF WS-HFAK-REF-X10 (1:3) = HFAK-BERADREF (HFAK-TAB-IX)         
238900           OR (WS-HFAK-REF-X10 (1:1) = '#'                                
239000           AND WS-HFAK-REF-X10 (2:2) NOT = SPACE                          
239100           AND HFAK-BERADREF-1 (HFAK-TAB-IX) = '#')                       
239200              MOVE JA TO BEVARREF-I-HFAK-TAB-SW                           
239300              MOVE 99 TO HFAK-TAB-IX                                      
239400           END-IF                                                         
239500           ADD 1 TO HFAK-TAB-IX                                           
239600        END-PERFORM                                                       
239700     END-IF                                                               
239800     .                                                                    
239900                                                                          
240000 CEGC-HAMTA-KDARTURS-IDPSN  SECTION.                                      
240100     MOVE 'CEGC-HAMTA-KDARTURS'    TO WS-CURRENT-SECTION                  
240200                                                                          
240300     IF DCS-CDC                                                           
240400       MOVE AREG-IDPSN             TO WS-IDPSN                            
240500       MOVE AREG-KDARTURS          TO WS-KDARTURS                         
240600     ELSE                                                                 
240700         MOVE ORAD-IDARTNR          TO W-IDARTNR                          
240800         MOVE ORAD-IDDC             TO W-IDDC                             
240900         PERFORM IMS-40-GU-WDK711                                         
241000         IF (SLAG-IDLEVNR = '1441 ' OR SLAG-IDLEVNR = 'BP2TW')            
241100            MOVE AREG-KDARTURS      TO WS-KDARTURS                        
241200         END-IF                                                           
241300                                                                          
241400         MOVE DCS-IDLANDX2          TO W-IDLAND                           
241500         PERFORM IMS-54-GU-WDK712                                         
241600         IF SEGMENT-FINNS                                                 
241700            IF LART-IDPSN-DC > ZERO                                       
241800               MOVE LART-IDPSN-DC   TO WS-IDPSN                           
241900            ELSE                                                          
242000               MOVE AREG-IDPSN      TO WS-IDPSN                           
242100            END-IF                                                        
242200            IF SLAG-IDLEVNR NOT = SPACE                                   
242300               MOVE LART-KDARTURS   TO WS-KDARTURS                        
242400            ELSE                                                          
242500               MOVE AREG-KDARTURS   TO WS-KDARTURS                        
242600            END-IF                                                        
242700         ELSE                                                             
242800            MOVE AREG-IDPSN         TO WS-IDPSN                           
242900            MOVE AREG-KDARTURS      TO WS-KDARTURS                        
243000         END-IF                                                           
243100     END-IF                                                               
243200     .                                                                    
243300                                                                          
243400 CEGD-SKAPA-PLE SECTION.                                                  
243500     MOVE 'CEGD-SKAPA-PLE      '    TO WS-CURRENT-SECTION                 
243600                                                                          
243700     MOVE WS-ADLAGOMR       TO 4006-ADLAGOMR                              
243800     IF OHUV-FLFORBI = 'S'                                                
243900       MOVE AREG-ADLAGOMR   TO 4006-ADLAGOMR                              
244000     END-IF                                                               
244100                                                                          
244200     MOVE WS-ADPLATS        TO 4006-ADPLATS                               
244300                               4006-ADPLATS-ORD                           
244400                                                                          
244500     MOVE ORAD-IDARTNR      TO 4006-IDARTNR                               
244600     MOVE ORAD-IDLOPNR      TO 4006-IDLOPNR                               
244700     MOVE ORAD-IDSYSTEM     TO 4006-IDSYSTEM                              
244800     MOVE WS-ADLAGOMR       TO 4006-ADLAGOMR-ORD                          
244900     IF OHUV-FLFORBI = 'S'                                                
245000       MOVE AREG-ADLAGOMR   TO 4006-ADLAGOMR-ORD                          
245100     END-IF                                                               
245200     MOVE WS-ADGANG         TO 4006-ADGANG                                
245300                                                                          
245400     MOVE WS-BEART          TO 4006-BEART                                 
245500     MOVE OHUV-KDORDTYP-LDC TO 4006-KDORDTYP-LDC                          
245600     MOVE OHUV-TIREPDAT     TO 4006-TIREPDAT                              
245700     MOVE ORAD-IDKUNDRF-WIP TO 4006-IDKUNDRF-WIP                          
245800     MOVE OHUV-IDDEPT       TO 4006-IDDEPT                                
245900                                                                          
246000     MOVE ORAD-BERADREF     TO 4006-BERADREF                              
246100*LDC-GB                                                                   
246200     MOVE OHUV-IDDISTR            TO DIST34-IDDISTR                       
246300       IF DIST34-ENGLAND-SDC AND                                          
246400          GMT-FLLDCKND = JA                                               
246500         IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                         
246600         OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                         
246610*LK adding exception for 1348 to remove prefix                            
246700           IF ORAD-BERADREF(1:4) = 'AUTO'                                 
246710           OR DIST34-IDDISTR  = 1348                                      
246800             MOVE ORAD-BERADREF TO 4006-BERADREF                          
246900           ELSE                                                           
247000*lk          MOVE ORAD-BERADREF TO 4006-BERADREF                          
247100             MOVE WS-LOPNR-WIPID TO 4006-BERADREF                         
247200           END-IF                                                         
247300         ELSE                                                             
247400           MOVE ORAD-BERADREF TO 4006-BERADREF                            
247500         END-IF                                                           
247600       ELSE                                                               
247700         MOVE ORAD-BERADREF TO 4006-BERADREF                              
247800       END-IF                                                             
247900                                                                          
248000*EX PÅ "REFERENCE MODIFYING".                                             
248100     MOVE 1 TO TECKEN-IX                                                  
248200     PERFORM UNTIL TECKEN-IX > 10                                         
248300       IF 4006-BERADREF (TECKEN-IX:1) < SPACE                             
248400         MOVE SPACE TO 4006-BERADREF (TECKEN-IX:1)                        
248500       END-IF                                                             
248600       ADD   1 TO TECKEN-IX                                               
248700     END-PERFORM                                                          
248800                                                                          
248900     MOVE ORAD-FLAKPLOC     TO 4006-FLAKPLOC                              
249000     MOVE 3410-IDBORD       TO 4006-IDBORD                                
249100     MOVE ORAD-IDGMTREF     TO 4006-IDGMTREF                              
249200     MOVE ODEL-IX           TO 4006-IDLOPNR-ORD                           
249300     MOVE 3410-IDLOPNR      TO 4006-IDLOPNR-PL                            
249310     MOVE W-ODEL-DARFS (3:6) TO 4006-TIRFSDAT                             
249400     MOVE W-ODEL-IDPLKLST   TO 4006-IDPLKLST                              
249500     MOVE W-ODEL-IDPRC      TO 4006-IDPRC                                 
249600     MOVE W-ODEL-IDPRODNR   TO 4006-IDPRODNR                              
249700     MOVE WS-IDRADNR-SISTA  TO 4006-IDRADNR                               
249800     MOVE ORAD-IDSPECEMB    TO 4006-IDSPECEMB                             
249900     MOVE WS-KDARTHNT       TO 4006-KDARTHNT                              
250000     MOVE WS-KDARTURS       TO 4006-KDARTURS                              
250100     MOVE WS-KDEMBAL        TO 4006-KDEMBAL                               
250200     MOVE ORAD-KDFARLIG     TO 4006-KDFARLIG                              
250300     MOVE OHUV-KDORDKL      TO 4006-KDORDKL                               
250400     MOVE AREG-KDSORT       TO 4006-KDSORT                                
250500     MOVE WS-KVAVBART       TO 4006-KVAVBART                              
250600     MOVE SPACE             TO 4006-IDZON                                 
250700     MOVE WS-IDPSN          TO 4006-IDPSN                                 
250800     MOVE ARB-KDFRAKT       TO 4006-KDFRAKT                               
250900                                                                          
251000     PERFORM IMS-41-ISRT-WDGX4006                                         
251100                                                                          
251200     MOVE ZERO TO RETURN-CODE                                             
251300                                                                          
251400     PERFORM UNTIL SEGMENT-FINNS                                          
251500        ADD 1 TO 4006-IDLOPNR                                             
251600        PERFORM IMS-41-ISRT-WDGX4006                                      
251700     END-PERFORM                                                          
251800     .                                                                    
251900                                                                          
252000 CEGE-SKAPA-PU                  SECTION.                                  
252100     MOVE 'CEGE-SKAPA-PU '      TO WS-CURRENT-SECTION                     
252200                                                                          
252300     MOVE OHUV-IDORDER          TO 4010-IDORDER                           
252400     MOVE WS-ADLAGOMR           TO 4010-ADLAGOMR                          
252500     IF OHUV-FLFORBI = 'S'                                                
252600       MOVE AREG-ADLAGOMR       TO 4010-ADLAGOMR                          
252700     END-IF                                                               
252800                                                                          
252900     MOVE WS-ADPLATS            TO 4010-ADPLATS                           
253000                                   4010-ADPLATS-ORD                       
253100                                                                          
253200     MOVE ORAD-IDARTNR          TO 4010-IDARTNR                           
253300     MOVE ORAD-IDLOPNR          TO 4010-IDLOPNR                           
253400                                                                          
253500     MOVE WS-ADLAGOMR           TO 4010-ADLAGOMR-ORD                      
253600     IF OHUV-FLFORBI = 'S'                                                
253700       MOVE AREG-ADLAGOMR       TO 4010-ADLAGOMR-ORD                      
253800     END-IF                                                               
253900     MOVE WS-ADGANG             TO 4010-ADGANG                            
254000                                                                          
254100     MOVE WS-BEART              TO 4010-BEART                             
254200     MOVE ORAD-FLAKPLOC         TO 4010-FLAKPLOC                          
254300     MOVE ORAD-FLSDCLEV         TO 4010-FLSDCLEV                          
254400     MOVE 3410-IDBORD           TO 4010-IDBORD                            
254500     MOVE ORAD-IDKUNDRF         TO 4010-IDKUNDRF                          
254600     MOVE ORAD-IDKUNDRF-RO      TO 4010-IDKUNDRF-RO                       
254700     MOVE ODEL-IX               TO 4010-IDLOPNR-ORD                       
254800     MOVE 3410-IDLOPNR          TO 4010-IDLOPNR-PL                        
254900     MOVE W-ODEL-IDPRC          TO 4010-IDPRC                             
255000     MOVE W-ODEL-IDPRODNR       TO 4010-IDPRODNR                          
255100     MOVE WS-IDRADNR-SISTA      TO 4010-IDPURAD                           
255200     MOVE ORAD-IDSPECEMB        TO 4010-IDSPECEMB                         
255300     MOVE 3410-IDUSER           TO 4010-IDUSER                            
255400     MOVE WS-KDARTURS           TO 4010-KDARTURS                          
255500     MOVE ORAD-IDDC             TO 4010-IDDC                              
255600     MOVE ORAD-IDDC-RO          TO 4010-IDDC-RO                           
255700     MOVE W-ODEL-KDFDKRAV       TO 4010-KDFDKRAV                          
255800     MOVE WS-KVAVBART           TO 4010-KVAVBART                          
255900     MOVE ORAD-KVBEART-Q        TO 4010-KVBEART-Q                         
256000     MOVE 0                     TO 4010-KVHANTTI                          
256100     MOVE ORAD-REKSIFFR         TO 4010-REKSIFFR                          
256200     MOVE W-ODEL-DALSTORD (3:10) TO 4010-TILST                            
256300     MOVE W-ODEL-TIREGDAT       TO 4010-TIREGDAT                          
256400     MOVE W-ODEL-TIREGTID       TO 4010-TIREGTID                          
256500     MOVE W-ODEL-DARFS (3:10)   TO 4010-TIRFS                             
256600     MOVE W-ODEL-DAUTSKR (3:6)  TO 4010-TIUTSKR                           
256700     MOVE W-ODEL-TIUTSTID       TO 4010-TIUTSTID                          
256800     MOVE OHUV-KDORDTYP-LDC      TO 4010-KDORDTYP-LDC                     
256900     MOVE OHUV-TIREPDAT          TO 4010-TIREPDAT                         
257000     MOVE ORAD-IDKUNDRF-WIP      TO 4010-IDKUNDRF-WIP                     
257100                                                                          
257200                                                                          
257300* ACKUMULERA VIKT/ORDER HÄR FÖR ANVÄNDNING I WL013430                     
257400*                                                                         
257500     MOVE OHUV-IDUSER    TO WS-OHUV-IDUSER                                
257600                                                                          
257700     IF WS-OHUV-IDORDER = 4010-IDORDER                                    
257800       CONTINUE                                                           
257900     ELSE                                                                 
258000                                                                          
258100       MOVE ZERO         TO WS-ORDERVIKT-PER-ORDER                        
258200*      MOVE ZERO         TO WS-ORDERVIKT-PER-RAD                          
258300*      MOVE ZERO         TO WS-ORDERVOLYM-PER-RAD                         
258400                                                                          
258500       MOVE 4010-IDORDER TO WS-OHUV-IDORDER                               
258600     END-IF                                                               
258700                                                                          
258800     IF WS-KVAVBART NOT = ORAD-KVBEART-Q                                  
258900        COMPUTE WS-ORDERVIKT-PER-RAD =                                    
259000               ((WS-KVAVBART * ORAD-VKART) / 1000)                        
259100        END-COMPUTE                                                       
259200                                                                          
259300        COMPUTE WS-ORDERVOLYM-PER-RAD =                                   
259400               ((WS-KVAVBART * ORAD-VLARTNTO) / 1000000)                  
259500        END-COMPUTE                                                       
259600     END-IF                                                               
259700*                                                                         
259800     COMPUTE WS-ORDERVIKT-PER-ORDER =                                     
259900             WS-ORDERVIKT-PER-ORDER + WS-ORDERVIKT-PER-RAD                
260000     END-COMPUTE                                                          
260100*                                                                         
260200*                                                                         
260300     IF WS-ORDERVIKT-PER-ORDER >= ZERO                                    
260400       MOVE WS-ORDERVIKT-PER-ORDER TO 3410-VKORDNTO(ODEL-IX)              
260500     ELSE                                                                 
260600       MOVE +0000000               TO 3410-VKORDNTO(ODEL-IX)              
260700     END-IF                                                               
260800     MOVE WS-ORDERVIKT-PER-RAD  TO 4010-VKORDNTO                          
260900     MOVE WS-ORDERVOLYM-PER-RAD TO 4010-VLORDNTO                          
261000*                                                                         
261100*LDC-GB                                                                   
261200     MOVE OHUV-IDDISTR            TO DIST34-IDDISTR                       
261300     IF NOT DCS-CDC                                                       
261400       IF DIST34-ENGLAND-SDC AND                                          
261500          GMT-FLLDCKND = JA                                               
261600         IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                         
261700         OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                         
261710*LK adding exception for 1348 to remove prefix                            
261800           IF ORAD-BERADREF(1:4) = 'AUTO'                                 
261810           OR DIST34-IDDISTR  = 1348                                      
261900             MOVE ORAD-BERADREF TO 4010-BERADREF                          
262000           ELSE                                                           
262100*lk          MOVE ORAD-BERADREF TO 4010-BERADREF                          
262200             MOVE WS-LOPNR-WIPID TO 4010-BERADREF                         
262300           END-IF                                                         
262400         ELSE                                                             
262500           MOVE ORAD-BERADREF   TO 4010-BERADREF                          
262600         END-IF                                                           
262700       ELSE                                                               
262800         MOVE ORAD-BERADREF     TO 4010-BERADREF                          
262900       END-IF                                                             
263000     ELSE                                                                 
263100       MOVE ORAD-BERADREF     TO 4010-BERADREF                            
263200     END-IF                                                               
263300                                                                          
263400*EX PÅ "REFERENCE MODIFYING".                                             
263500     MOVE 1 TO TECKEN-IX                                                  
263600     PERFORM UNTIL TECKEN-IX > 10                                         
263700       IF 4010-BERADREF (TECKEN-IX:1) < SPACE                             
263800         MOVE SPACE TO 4010-BERADREF (TECKEN-IX:1)                        
263900       END-IF                                                             
264000       ADD   1 TO TECKEN-IX                                               
264100     END-PERFORM                                                          
264200                                                                          
264300     MOVE ORAD-BEVOLREF         TO 4010-BEVOLREF                          
264400     MOVE ORAD-FLINVEST         TO 4010-FLINVEST                          
264500     MOVE ORAD-FLPRTILL         TO 4010-FLPRTILL                          
264600     MOVE ORAD-FLTILLK          TO 4010-FLTILLK                           
264700     MOVE ORAD-IDLEVNR          TO 4010-IDLEVNR                           
264800     MOVE W-ODEL-IDPLKLST       TO 4010-IDPLKLST                          
264900     MOVE ORAD-IDLOPNR-RO       TO 4010-IDLOPNR-RO                        
265000     MOVE ORAD-KDDSP            TO 4010-KDDSP                             
265100     MOVE ORAD-KDFARLIG         TO 4010-KDFARLIG                          
265200     MOVE ORAD-KDKVBRYT         TO 4010-KDKVBRYT                          
265300     MOVE ORAD-KDPRODSL         TO 4010-KDPRODSL                          
265400     MOVE ORAD-KDPRTYP          TO 4010-KDPRTYP                           
265500     MOVE ORAD-KDORDING         TO 4010-KDORDING                          
265600     MOVE ORAD-KDOI             TO 4010-KDOI                              
265700     MOVE ORAD-CLEARGROUP       TO 4010-CLEARGROUP                        
265800     MOVE ORAD-KDORDKL          TO 4010-KDORDKL                           
265900     MOVE ORAD-KDVRINFO         TO 4010-KDVRINFO                          
266000     MOVE ORAD-KDVALISO         TO 4010-KDVALISO                          
266100     IF 4010-KDVALISO > SPACE                                             
266200       CONTINUE                                                           
266300     ELSE                                                                 
266400        MOVE 'W4L013410  4010-KDVALISO ' TO  ERROR-TEXT                   
266500        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
266600     END-IF                                                               
266700     MOVE ORAD-KVSLATT          TO 4010-KVSLATT                           
266800     MOVE ORAD-PRARTNTO         TO 4010-PRARTNTO                          
266900     MOVE ORAD-DEAL-PR-LINE     TO 4010-DEAL-PR-LINE                      
267000     MOVE ORAD-TIPRIS           TO 4010-TIPRIS                            
267100     MOVE ORAD-TIRODAT          TO 4010-TIRODAT                           
267200     MOVE ORAD-VKART            TO 4010-VKART                             
267300     IF ORAD-VKART-NTO NUMERIC                                            
267400       MOVE ORAD-VKART-NTO      TO 4010-VKART-NTO                         
267500     ELSE                                                                 
267600       MOVE ZEROES              TO 4010-VKART-NTO                         
267700     END-IF                                                               
267800     MOVE ORAD-VLARTNTO         TO 4010-VLARTNTO                          
267900     MOVE ORAD-FLRESTN          TO 4010-FLRESTN                           
268000     MOVE ORAD-IDKAMPRF         TO 4010-IDKAMPRF                          
268100     MOVE ORAD-IDSYSTEM         TO 4010-IDSYSTEM                          
268200     MOVE ORAD-IDBIL            TO 4010-IDBIL                             
268300     MOVE ORAD-IDKLIENT         TO 4010-IDKLIENT                          
268400     MOVE ORAD-IDARBREF         TO 4010-IDARBREF                          
268500     MOVE ORAD-IDVIN            TO 4010-IDVIN                             
268600     MOVE OHUV-IDANALYS         TO 4010-IDANALYS                          
268700     MOVE WS-IDPSN              TO 4010-IDPSN                             
268800     MOVE SPACE                 TO 4010-IDZON                             
268900     MOVE SPACE                 TO 4010-FLCOD                             
269000                                                                          
269100     MOVE ORAD-IDARTNR TO W-ART-IDARTNR                                   
269200     PERFORM IMS-43-GU-WDD501                                             
269300                                                                          
269400     IF SEGMENT-FINNS                                                     
269500       MOVE ART-VKART-FG        TO 4010-VKART-FG                          
269600       MOVE ART-SUEQFG          TO 4010-SUEQFG                            
269700       MOVE ART-VLFG            TO 4010-VLFG                              
269800     ELSE                                                                 
269900       MOVE ZERO                TO 4010-VKART-FG                          
270000                                   4010-SUEQFG                            
270100                                   4010-VLFG                              
270200     END-IF                                                               
270300                                                                          
270400     MOVE OHUV-IDKONTO      TO WS-IDKONTO                                 
270500     MOVE OHUV-IDANALYS     TO 4010-IDANALYS                              
270600     MOVE OHUV-IDKST        TO 4010-IDKST                                 
270700     MOVE WS-IDKONTO        TO 4010-IDKONTO                               
270800     MOVE ORAD-PRAVCOST     TO 4010-PRAVCOST                              
270900                                                                          
271000     PERFORM IMS-42-ISRT-WDGX4010                                         
271100                                                                          
271200     PERFORM UNTIL SEGMENT-FINNS                                          
271300        ADD 1 TO 4010-IDLOPNR                                             
271400        PERFORM IMS-42-ISRT-WDGX4010                                      
271500     END-PERFORM                                                          
271600     .                                                                    
271700                                                                          
271800                                                                          
271900 CEH-KONTROLL-SPLITGRANS SECTION.                                         
272000     MOVE 'CEH-KONTROLL-SPLITGRANS'  TO WS-CURRENT-SECTION                
272100                                                                          
272200     IF ARTIKEL-OK                                                        
272300        ADD 1                     TO WS-KVRADER                           
272400        ADD WS-ORDERVIKT-PER-RAD  TO WS-VKORDNTO                          
272500        ADD WS-ORDERVOLYM-PER-RAD TO WS-VLORDNTO                          
272600     END-IF                                                               
272700     .                                                                    
272800 CEI-JUSTERA-VARDE-PA-ORDERDEL SECTION.                                   
272900     MOVE 'CEI-JUSTERA-VARDE-PA-ORDERDEL' TO WS-CURRENT-SECTION           
273000                                                                          
273100     IF ARTIKEL-FEL                                                       
273200        IF ORAD-PRAVCOST > 0                                              
273300          COMPUTE WS-ORDERVARDE-PER-RAD =                                 
273400                  ORAD-PRAVCOST * ORAD-KVBEART-Q                          
273500        ELSE                                                              
273600          COMPUTE WS-ORDERVARDE-PER-RAD =                                 
273700                  ORAD-PRARTNTO * ORAD-KVBEART-Q                          
273800        END-IF                                                            
273900        COMPUTE WS-ORDERVARDE-PER-RAD-LOC =                               
274000                ORAD-PRARTNTO-LOC * ORAD-KVBEART-Q                        
274100        COMPUTE WS-ORDERVARDE-PER-RAD-LOCPREL =                           
274200                ORAD-PRARTNTO-LOCPREL * ORAD-KVBEART-Q                    
274300        SUBTRACT WS-ORDERVARDE-PER-RAD FROM W-ODEL-SUORDV                 
274400        SUBTRACT WS-ORDERVARDE-PER-RAD-LOC                                
274500                            FROM W-ODEL-SUORDV-LOC                        
274600        SUBTRACT WS-ORDERVARDE-PER-RAD-LOCPREL                            
274700                            FROM W-ODEL-SUORDV-LOCPREL                    
274800        SUBTRACT WS-ORDERVIKT-PER-RAD  FROM W-ODEL-VKORDNTO               
274900        SUBTRACT WS-ORDERVOLYM-PER-RAD FROM W-ODEL-VLORDNTO               
275000        SUBTRACT 1                     FROM W-ODEL-KVRADER                
275100                                                                          
275200        IF W-ODEL-SUORDV   NEGATIVE                                       
275300           MOVE 0                        TO W-ODEL-SUORDV                 
275400        END-IF                                                            
275500                                                                          
275600        IF W-ODEL-SUORDV-LOC     NEGATIVE                                 
275700           MOVE 0                        TO W-ODEL-SUORDV-LOC             
275800        END-IF                                                            
275900                                                                          
276000        IF W-ODEL-SUORDV-LOCPREL NEGATIVE                                 
276100           MOVE 0                        TO W-ODEL-SUORDV-LOCPREL         
276200        END-IF                                                            
276300                                                                          
276400        IF W-ODEL-VKORDNTO NEGATIVE                                       
276500           MOVE 0                        TO W-ODEL-VKORDNTO               
276600        END-IF                                                            
276700                                                                          
276800        IF W-ODEL-VLORDNTO NEGATIVE                                       
276900           MOVE 0                        TO W-ODEL-VLORDNTO               
277000        END-IF                                                            
277100                                                                          
277200        IF W-ODEL-KVRADER  NEGATIVE                                       
277300           MOVE 0                        TO W-ODEL-KVRADER                
277400        END-IF                                                            
277500                                                                          
277600        IF W-ODEL-KVRADER  = 0                                            
277700           MOVE 0                        TO W-ODEL-SUORDV                 
277800                                            W-ODEL-SUORDV-LOC             
277900                                            W-ODEL-SUORDV-LOCPREL         
278000                                            W-ODEL-VKORDNTO               
278100                                            W-ODEL-VLORDNTO               
278200        END-IF                                                            
278300     END-IF                                                               
278400     .                                                                    
278500 CEK-KOLLA-PLATSSATTNING SECTION.                                         
278600     MOVE 'STA CEK-KOLLA-PLATSSATTNING' TO WS-CURRENT-SECTION             
278700     SKIP3                                                                
278800     MOVE OHUV-IDDISTR                  TO PLATS-IDDISTR                  
278900     MOVE OHUV-IDKUNDNR                 TO PLATS-IDKUNDNR                 
279000     MOVE ARB-KDFRAKT                   TO PLATS-KDFRAKT                  
279100     MOVE OHUV-KDORDKL                  TO PLATS-KDORDKLX                 
279200     MOVE WS-VKORDNTO                   TO PLATS-VKORDNTO-KOLLI           
279300     MOVE ZERO                          TO PLATS-ADVMODUL                 
279400                                           PLATS-ADHMODUL                 
279500                                           PLATS-ADFLOMR                  
279600                                           PLATS-ADRUTNIV                 
279700                                           PLATS-IDTRPTNR                 
279800                                           PLATS-DIHMODUL                 
279900     MOVE SPACE                         TO PLATS-ADFLGEO                  
280000                                           PLATS-FLUTLAST                 
280100                                           PLATS-IDDC-CROSS               
280200     MOVE ORAD-IDDC                     TO PLATS-IDDC                     
280300     MOVE ORAD-IDORDNR7                 TO PLATS-IDORDNR                  
280400     MOVE ZERO                          TO PLATS-DIKOLLIL                 
280500     MOVE ZERO                          TO PLATS-DIKOLLIB                 
280600     MOVE ZERO                          TO PLATS-DIKOLLIH                 
280700     MOVE ZERO                          TO PLATS-KDKOLLID                 
280800                                                                          
280900     IF ORAD-KDORDKL = +4                                                 
281000        IF ORAD-IDDISTR = +00878                                          
281100        AND ORAD-IDKUNDNR > +006000                                       
281200           MOVE 2                       TO PLATS-KDCALL                   
281300        ELSE                                                              
281400           MOVE 1                       TO PLATS-KDCALL                   
281500        END-IF                                                            
281600     ELSE                                                                 
281700        MOVE +2                         TO PLATS-KDCALL                   
281800     END-IF                                                               
281900                                                                          
282000     MOVE SPACE                          TO PLATS-ADFLGEO                 
282100     MOVE ZERO                           TO PLATS-ADFLOMR                 
282200     MOVE ZERO                           TO PLATS-ADRUTNIV                
282300                                                                          
282400     CALL W403PLAT USING PLATS-W403PLAT                                   
282500                         PLATS-DM-PCB                                     
282600                         PLATS-DN-PCB                                     
282700                         PLATS-DP-PCB                                     
282800                         PLATS-DO-PCB                                     
282900                         PLATS-WDE6C-PCB                                  
283000                         PLATS-GMTC-PCB                                   
283100                         PLATS-WDB6-PCB                                   
283200     IF PLATS-KDSVAR = SPACE                                              
283300        MOVE PLATS-ADFLGEO    TO 3410-ADFLGEO  (ODEL-IX)                  
283400        MOVE PLATS-ADFLOMR    TO 3410-ADFLOMR  (ODEL-IX)                  
283500        MOVE PLATS-ADRUTNIV   TO 3410-ADRUTNIV (ODEL-IX)                  
283600        MOVE PLATS-IDTRPTNR   TO 3410-IDTRPTNR (ODEL-IX)                  
283700        MOVE PLATS-IDDC-CROSS TO 3410-IDDC-CROSS (ODEL-IX)                
283800     ELSE                                                                 
283900        MOVE 'FELSVAR FRÅN W403PLAT'    TO  ERROR-TEXT                    
284000        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
284100     END-IF                                                               
284200     IF PLATS-ADFLGEO = '000' OR '   '                                    
284300        MOVE 'ZERO I PLATS-ADFLGEO '    TO  ERROR-TEXT                    
284400        CALL ABEND USING RKOD-ABEND-WITH-DUMP                             
284500     END-IF                                                               
284600     MOVE 'END CEK-KOLLA-PLATSSATTNING' TO WS-CURRENT-SECTION             
284700     .                                                                    
284800                                                                          
284900 CEL-CREATE-EVENT-151  SECTION.                                           
285000                                                                          
285010     IF LYNK-NON-API                                                      
285020       PERFORM S03D-CR-NON-API-EVENT                                      
285030     ELSE                                                                 
285100*NEW                                                                      
285200*EVENT HANDLING                                                           
285300*LYND = DÖSKALLE  WDQ2C                                                   
285400       IF OHUV-IDSYSTEM = 'LYND' OR 'TADD'                                
285500         MOVE OHUV-IDDISTR             TO W-IDDISTR-CSEQ                  
285600         MOVE OHUV-IDKUNDNR            TO W-IDKUNDNR-CSEQ                 
285700         MOVE ORAD-IDKUNDRF-RO(3:5)    TO W-IDORDNR5-CSEQ                 
285800         PERFORM IMS-GU-WDQ201-CSEQ-GE                                    
285900         IF SEGMENT-FINNS                                                 
286000           MOVE CSQ-OHUV-IDDISTR           TO WS-IDDISTR-EVENT            
286100           MOVE CSQ-OHUV-IDKUNDNR          TO WS-IDKUNDNR-EVENT           
286200           MOVE CSQ-OHUV-IDORDNR7          TO WS-IDORDNR7-EVENT           
286300           MOVE CSQ-OHUV-TIREGDAT          TO WS-TIREGDAT-EVENT           
286400           MOVE JA                         TO CREATE-EVENT-SW             
286500         END-IF                                                           
286600       ELSE                                                               
286700*LYNV = VOR KUNDRF-LEV -A6                                                
286800         IF OHUV-IDSYSTEM = 'LYNV' OR 'TADV'                              
286900           MOVE LOW-VALUE              TO W-WDA6BSEQ-MIN-X                
287000           MOVE HIGH-VALUE             TO W-WDA6BSEQ-MAX-X                
287100                                                                          
287200           MOVE OHUV-IDDISTR           TO W-A6BSEQ-MIN-IDDISTR            
287300                                          W-A6BSEQ-MAX-IDDISTR            
287400           MOVE OHUV-IDKUNDNR          TO W-A6BSEQ-MIN-IDKUNDNR           
287500                                          W-A6BSEQ-MAX-IDKUNDNR           
287600           MOVE OHUV-IDKUNDRF         TO W-A6BSEQ-MIN-IDKUNDRF-LEV        
287700                                         W-A6BSEQ-MAX-IDKUNDRF-LEV        
287800           PERFORM IMS-GU-SEQB-WDA601                                     
287900           IF SEGMENT-FINNS                                               
288000             MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                
288100             MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT               
288200             MOVE VOR-IDKUNDRF(1:7)    TO WS-IDORDNR7-EVENT               
288300             MOVE VOR-TIREGDAT-URSP    TO WS-TIREGDAT-EVENT               
288400             MOVE JA                   TO CREATE-EVENT-SW                 
288500           END-IF                                                         
288600         ELSE                                                             
288700*LYNB = VERKSTADS/REPARATIONS-ORDER -A5                                   
288800           IF OHUV-IDSYSTEM = 'LYNB' OR 'TADB'                            
288900                                                                          
289000             MOVE OHUV-IDDISTR         TO W-IDDISTR-A5-MIN                
289100                                          W-IDDISTR-A5-MAX                
289200             MOVE OHUV-IDKUNDNR        TO W-IDKUNDNR-A5-MIN               
289300                                          W-IDKUNDNR-A5-MAX               
289400             MOVE OHUV-KDORDKL         TO W-KDORDKL                       
289500             MOVE OHUV-IDORDNR7(3:5)   TO W-IDKUNDRF-LEV                  
289600*                                                                         
289700             PERFORM IMS-GU-WDA501                                        
289800             IF SEGMENT-FINNS                                             
289900               MOVE OHUV-IDDISTR       TO WS-IDDISTR-EVENT                
290000               MOVE OHUV-IDKUNDNR      TO WS-IDKUNDNR-EVENT               
290100               MOVE  RAD-IDORDNR5      TO WS-IDORDNR7-EVENT               
290200               MOVE  RAD-TIREGDAT      TO WS-TIREGDAT-EVENT               
290300               MOVE JA                 TO CREATE-EVENT-SW                 
290400             END-IF                                                       
290500           ELSE                                                           
290600             MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                
290700             MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT               
290800             MOVE OHUV-IDORDNR7        TO WS-IDORDNR7-EVENT               
290900             MOVE OHUV-TIREGDAT        TO WS-TIREGDAT-EVENT               
291000             MOVE JA                   TO CREATE-EVENT-SW                 
291100           END-IF                                                         
291200         END-IF                                                           
291300       END-IF                                                             
291310     END-IF                                                               
291400                                                                          
291500     MOVE OHUV-IDSYSTEM(1:3) TO WS-IDSYST-1-3                             
291600                                                                          
291700     IF CREATE-EVENT                                                      
291800                                                                          
291900     MOVE OHUV-IDSYSTEM(1:4) TO EVENT-SW                                  
292000     IF EVENT-OK OR LYNK-NON-API                                          
292100                                                                          
292200       IF (WS-IDSYST-1-3      = 'LYN') OR LYNK-NON-API                    
292300         MOVE 'L'              TO WS-PARTNER                              
292400       END-IF                                                             
292500                                                                          
292600       IF WS-IDSYST-1-3      = 'TAD'                                      
292700         MOVE 'T'              TO WS-PARTNER                              
292800       END-IF                                                             
292900                                                                          
293000       IF WS-IDSYST-1-3      = 'POL'                                      
293100         MOVE 'P'              TO WS-PARTNER                              
293200       END-IF                                                             
293300                                                                          
293400       IF WS-IDSYST-1-3      = 'ECO'                                      
293500         MOVE 'E'              TO WS-PARTNER                              
293600       END-IF                                                             
293700                                                                          
293800       IF WS-IDSYST-1-3      = 'ACC'                                      
293900         MOVE 'A'              TO WS-PARTNER                              
294000       END-IF                                                             
294100       IF WS-IDSYST-1-3      = 'APA'                                      
294200         MOVE 'K'              TO WS-PARTNER                              
294300       END-IF                                                             
294400       IF WS-IDSYST-1-3      = 'APB'                                      
294500         MOVE 'B'              TO WS-PARTNER                              
294600       END-IF                                                             
294700       IF WS-IDSYST-1-3      = 'APC'                                      
294800         MOVE 'C'              TO WS-PARTNER                              
294900       END-IF                                                             
295000       IF WS-IDSYST-1-3      = 'APD'                                      
295100         MOVE 'D'              TO WS-PARTNER                              
295200       END-IF                                                             
295300       IF WS-IDSYST-1-3      = 'APE'                                      
295400         MOVE 'M'              TO WS-PARTNER                              
295500       END-IF                                                             
295600       IF WS-IDSYST-1-3      = 'APF'                                      
295700         MOVE 'F'              TO WS-PARTNER                              
295800       END-IF                                                             
295900       IF WS-IDSYST-1-3      = 'APG'                                      
296000         MOVE 'G'              TO WS-PARTNER                              
296100       END-IF                                                             
296200       IF WS-IDSYST-1-3      = 'APH'                                      
296300         MOVE 'H'              TO WS-PARTNER                              
296400       END-IF                                                             
296500       IF WS-IDSYST-1-3      = 'API'                                      
296600         MOVE 'I'              TO WS-PARTNER                              
296700       END-IF                                                             
296800       IF WS-IDSYST-1-3      = 'APJ'                                      
296900         MOVE 'J'              TO WS-PARTNER                              
297000       END-IF                                                             
297100                                                                          
297200       MOVE SPACE               TO Z430-REQU-TIMESTAMP                    
297300       PERFORM CELA-CREATE-EVENT-151                                      
297400                                                                          
297500     END-IF                                                               
297600     END-IF                                                               
297700     .                                                                    
297800     EJECT                                                                
297900 CELA-CREATE-EVENT-151 SECTION.                                           
298000     MOVE 'STA CELA-CREATE-EVENT'   TO WS-CURRENT-SECTION                 
298100     IF LYNK-NON-API                                                      
298200        MOVE 'LYNK'             TO Z430-REQU-IDEVENTREC                   
298210     ELSE                                                                 
298220        MOVE OHUV-IDSYSTEM      TO Z430-REQU-IDEVENTREC                   
298230     END-IF                                                               
298300                                                                          
298400     MOVE IDMSGVER              TO Z430-REQU-IDMSGVER                     
298500     MOVE 'PURCHASEORDER'       TO Z430-REQU-IDEVENT                      
298600     MOVE 'UPDATE'              TO Z430-REQU-IDEVENTTYP                   
298700***  MOVE WS-TIMESTAMP          TO Z430-REQU-TIMESTAMP                    
298800     MOVE FUNCTION CURRENT-DATE TO Z430-REQU-TIMESTAMP                    
298900     MOVE 'WAPIORD'             TO Z430-REQU-IDCPYTXT                     
299000     MOVE WS-IDEVENTORDREF      TO Z430-IDAPIORDREF                       
299100     MOVE '151'                 TO Z430-IDMSG                             
299200     MOVE 'ORDER PART/LINE IS PRINTED'                                    
299300                                TO Z430-TEMFSINF                          
299400                                                                          
299500*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
299600*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
299700     MOVE 'WZ0430X '           TO MSG-KDTRANS-1                           
299800     MOVE 'Z430'               TO MSG-IDTRANS-1                           
299900     MOVE '1'                  TO MSG-KDMFSFOR-1                          
300000     MOVE 'WZ0430I1'           TO MSG-KOM-IDCPYTXT                        
300100     STRING 'EVE' WS-PARTNER WS-IDDISTR-EVENT                             
300200          DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                         
300300                                                                          
300400     ADD  1                    TO MSG-KOM-TIKLOCK                         
300500     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
300600     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
300700                                                                          
300800     CALL W006KOM USING MSG-PCB                                           
300900                        0693-PCB                                          
301000                        KOM-WDP8-PCB                                      
301100                        MSG-KOM-WMSGKOM                                   
301200                        MSG-IO-AREA                                       
301300                                                                          
301400     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
301500        MOVE                                                              
301600        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
301700                                     TO ERROR-TEXT                        
301800        CALL ABEND USING RKOD-ABEND-WITHOUT-DUMP                          
301900     END-IF                                                               
302000     .                                                                    
302100     EJECT                                                                
302200                                                                          
302300 CEM-KOMPLETTERA-RANSONERING SECTION.                                     
302400     MOVE 'STA CED-KOMPLET'   TO WS-CURRENT-SECTION                       
302500                                                                          
302600     IF ARTIKEL-OK AND DCS-CDC                                            
302700        MOVE ORAD-BERADREF        TO RANS-BERADREF                        
302800        MOVE OHUV-FLEMBORD        TO RANS-FLEMBORD                        
302900        MOVE OHUV-FLFORBI         TO RANS-FLFORBI                         
303000        MOVE OHUV-FLORDSPE        TO RANS-FLORDSPE                        
303100        MOVE OHUV-FLOVRLEV        TO RANS-FLOVRLEV                        
303200        MOVE ORAD-IDKAMPRF        TO RANS-IDKAMPRF                        
303300        MOVE ORAD-IDARTNR         TO RANS-IDARTNR                         
303400        MOVE ORAD-IDLEVNR         TO RANS-IDLEVNR                         
303500        MOVE OHUV-IDRFTAB         TO RANS-IDRFTAB                         
303600        MOVE ORAD-TIRODAT         TO RANS-TIRODAT                         
303700        MOVE 7                    TO RANS-KDORDBEH                        
303800        MOVE OHUV-KDORDKL         TO RANS-KDORDKL                         
303900        MOVE ORAD-KVBEART-Q       TO RANS-KVBEART-Q                       
304000        MOVE ORAD-KDTPOTYP        TO RANS-KDTPOTYP                        
304100        MOVE AREG-KDERS           TO RANS-KDERS                           
304200        MOVE AREG-KVLS            TO RANS-KVLS                            
304300        MOVE AREG-KVPB-SATS       TO RANS-KVPB-SATS                       
304400        MOVE AREG-KVPB-SEP        TO RANS-KVPB-SEP                        
304500        MOVE AREG-REDIRLEV        TO RANS-REDIRLEV                        
304600        MOVE AREG-KVRESS          TO RANS-KVRESS                          
304700        MOVE AREG-KVSPANT         TO RANS-KVSPANT                         
304800        MOVE AREG-KVUTRS          TO RANS-KVUTRS                          
304900        MOVE AREG-TIDISPIN        TO RANS-TIDISPIN                        
305000                                                                          
305100        MOVE ORAD-IDARTNR         TO W-IDARTNR                            
305200        PERFORM IMS-21-GHU-WDK611                                         
305300        IF  SEGMENT-FINNS                                                 
305400        AND CLAG-FLCDART = JA                                             
305500           MOVE ZERO              TO RANS-RERF-ART-UT                     
305600           MOVE 1                 TO RANS-RERF-RAD-UT                     
305700        ELSE                                                              
305800           IF AREG-KDPRODSL = 71 OR 72 OR 73 OR 74                        
305900             MOVE ZERO            TO RANS-RERF-ART-UT                     
306000             MOVE 1               TO RANS-RERF-RAD-UT                     
306100           ELSE                                                           
306200             CALL W411RANS USING RANS-W411RANS RANS-XXKM-PCB              
306300                                            RANS-ARTM-PCB                 
306400                                            RANS-ARTS-PCB                 
306500          END-IF                                                          
306600        END-IF                                                            
306700     END-IF                                                               
306800     MOVE 'END CED-KOMPLET'   TO WS-CURRENT-SECTION                       
306900     .                                                                    
307000     EJECT                                                                
307100                                                                          
307200 CF-UPPDAT-ORDERDEL SECTION.                                              
307300     MOVE 'CF-UPPDAT-ORDERDEL  ' TO WS-CURRENT-SECTION                    
307400                                                                          
307500     PERFORM IMS-07-GHU-WDQ201-12                                         
307600                                                                          
307700     IF Q221-SEG-SAKNAS                                                   
307800        PERFORM CFA-SKAPA-DUMMYPOST-PU-ETIK                               
307900        IF W-ODEL-KDODELSTA NOT = 'P'                                     
308000           MOVE WS-KVRADER      TO W-ODEL-KVRADER                         
308100           MOVE WS-VLORDNTO     TO W-ODEL-VLORDNTO                        
308200           MOVE WS-VKORDNTO     TO W-ODEL-VKORDNTO                        
308300        END-IF                                                            
308400        MOVE W-ORDERDEL TO DLI-IO-WDQ301                                  
308500        MOVE ODEL-IX    TO ODEL-IDLOPNR-ORD                               
308600        PERFORM IMS-04-REPL-WDQ301                                        
308700                                                                          
308800        MOVE JA TO ARB-FLODELUT                                           
308900        COMPUTE IX1 = 3410-IX + 1                                         
309000*                                                                         
309100*       START TRANSACTION W4T397 FOR DUMMY LINE                           
309200*       SETS CORRECT ORDER STATUS IN KOLLI-REG                            
309300*                                                                         
309400        IF WS-KVRADER = 0                                                 
309500           PERFORM CFB-START-4397                                         
309600        END-IF                                                            
309700                                                                          
309800     ELSE                                                                 
309900        MOVE W-ORDERDEL TO DLI-IO-WDQ301                                  
310000        MOVE ODEL-IX    TO ODEL-IDLOPNR-ORD                               
310100        PERFORM IMS-04-REPL-WDQ301                                        
310200     END-IF                                                               
310300                                                                          
310400     IF WS-ORDERVIKT-PER-ORDER >= ZERO                                    
310500                                                                          
310600       COMPUTE WS-ORDERVIKT-PER-ORDER =                                   
310700               WS-ORDERVIKT-PER-ORDER + ADD-1-HEKTO                       
310800       END-COMPUTE                                                        
310900     END-IF                                                               
311000                                                                          
311100     MOVE 'R'                     TO W-KDODELST                           
311200     PERFORM IMS-46-GU-WDQ301-STATUS                                      
311300     IF SEGMENT-FINNS                                                     
311400        MOVE 'R*'                 TO WS-KDORDSTA-CX                       
311500     ELSE                                                                 
311600        MOVE 'U'                  TO W-KDODELST                           
311700        PERFORM IMS-46-GU-WDQ301-STATUS                                   
311800        IF SEGMENT-FINNS                                                  
311900           MOVE 'P'               TO W-KDODELST                           
312000           PERFORM IMS-46-GU-WDQ301-STATUS                                
312100                                                                          
312200           IF SEGMENT-FINNS                                               
312300              MOVE 'U*'           TO WS-KDORDSTA-CX                       
312400           ELSE                                                           
312500              MOVE 'U '           TO WS-KDORDSTA-CX                       
312600           END-IF                                                         
312700        ELSE                                                              
312800           MOVE 'P '              TO WS-KDORDSTA-CX                       
312900        END-IF                                                            
313000     END-IF                                                               
313100                                                                          
313200     MOVE WS-KDORDSTA-CX     TO ARB-KDORDSTA                              
313300     MOVE WS-KVSEMBRA        TO ARB-KVSEMBRA                              
313400     MOVE WS-IDRADNR-SISTA   TO ARB-IDRADNR-SISTA                         
313500                                                                          
313600*    EFTER UTSKRIFT KAN ORDERN ALDRIG VARA AKTUELL                        
313700*    FÖR TVINGANDE TILLÄGG                                                
313800*    FÖLJANDE REPLACE UPPDATERAR BÅDE 01- OCH 12-SEGMENT                  
313900                                                                          
314000     MOVE NEJ TO OHUV-FLORDTIL                                            
314100     PERFORM IMS-47-REPL-WDQ212                                           
314200                                                                          
314300     IF PLOCKSATS-KLAR                                                    
314400        MOVE 1000 TO ORAD-IX                                              
314500     END-IF                                                               
314600     .                                                                    
314700                                                                          
314800 CFA-SKAPA-DUMMYPOST-PU-ETIK SECTION.                                     
314900     MOVE 'CFA-SKAPA-DUMMYPOST-PU' TO WS-CURRENT-SECTION                  
315000                                                                          
315100*************************************************                         
315200*  ENDAST I PLOCKSATSER SOM INNEHÅLLER EN       *                         
315300*  ORDERDEL SKALL MAN SKAPA DUMMYPOST.          *                         
315400*  I ÖVRIGA FALL NOLLSTÄLLES ENDAST ORDERDELS-  *                         
315500*  INFO.                                        *                         
315600*************************************************                         
315700                                                                          
315800     IF WS-KVRADER = 0                                                    
315900        PERFORM CFAA-NOLLSTALL-ODELINFO                                   
316000                                                                          
316100*       IF 3410-IX = 1                                                    
316200*     OM INGEN ORDERDEL FÅR NÅGRA ARTIKLAR MÅSTE VI ÄNDÅ                  
316300*     LÄGGA UPP DUMMYPOSTERNA, ANNARS GÅR DET ÅT PIPAN... (GK)            
316400*                                                                         
316500        IF 3410-IX = ODEL-IX AND                                          
316600           PLOCKSATS-PU-SAKNAS                                            
316700                                                                          
316800           PERFORM S05-SKAPA-PLOCKSATS-PU                                 
316900           PERFORM CFAB-FYLL-I-DUMMYPOST                                  
317000           PERFORM IMS-42-ISRT-WDGX4010                                   
317100                                                                          
317200           PERFORM UNTIL SEGMENT-FINNS                                    
317300              ADD 1 TO 4010-IDLOPNR                                       
317400              PERFORM IMS-42-ISRT-WDGX4010                                
317500           END-PERFORM                                                    
317600                                                                          
317700           PERFORM S09-SKAPA-PLOCKSATS-ETIK                               
317800           PERFORM CFAC-FYLL-I-DUMMYPOST-ETIK                             
317900           PERFORM IMS-41-ISRT-WDGX4006                                   
318000                                                                          
318100           PERFORM UNTIL SEGMENT-FINNS                                    
318200              ADD 1 TO 4006-IDLOPNR                                       
318300              PERFORM IMS-41-ISRT-WDGX4006                                
318400           END-PERFORM                                                    
318500        END-IF                                                            
318600     END-IF                                                               
318700     .                                                                    
318800                                                                          
318900 CFAA-NOLLSTALL-ODELINFO SECTION.                                         
319000     MOVE 'CFAA-NOLLSTALL-ODELINFO' TO WS-CURRENT-SECTION                 
319100                                                                          
319200     MOVE 'P'                 TO W-ODEL-KDODELSTA                         
319300     MOVE ZERO                TO W-ODEL-KVRADER                           
319400                                 W-ODEL-VKORDNTO                          
319500                                 W-ODEL-VLORDNTO                          
319600                                 W-ODEL-SUORDV                            
319700                                 W-ODEL-SUORDV-LOC                        
319800                                 W-ODEL-SUORDV-LOCPREL                    
319900     MOVE 3410-TIAAMMDD       TO W-ODEL-DAUTSKR                           
320000     MOVE 20                  TO W-ODEL-DAUTSKR (1:2)                     
320100     MOVE 3410-TIAAMMDD       TO W-ODEL-TIPACKN                           
320200     MOVE 3410-TIHHMMSS       TO W-ODEL-TIUTSTID                          
320300                                 W-ODEL-TIPACTID                          
320400     MOVE 3410-IDUSER         TO W-ODEL-IDUSER                            
320500     MOVE 3410-IDBORD         TO W-ODEL-IDBORD                            
320600     .                                                                    
320700                                                                          
320800 CFAB-FYLL-I-DUMMYPOST SECTION.                                           
320900     MOVE 'CFAB-FYLL-I-DUMMYPOST  ' TO WS-CURRENT-SECTION                 
321000                                                                          
321100     MOVE OHUV-IDORDER        TO 4010-IDORDER                             
321200     MOVE OHUV-KDORDKL        TO 4010-KDORDKL                             
321300                                                                          
321400     INITIALIZE                  4010-DEAL-PR-LINE                        
321500     MOVE SPACE               TO 4010-KDSS-PU                             
321600                                 4010-FLAKPLOC                            
321700                                 4010-BERADREF                            
321800                                 4010-BEVOLREF                            
321900                                 4010-KDARTURS                            
322000                                 4010-KDOI                                
322100                                 4010-CLEARGROUP                          
322200                                 4010-KDPRT                               
322300                                 4010-KDPRTYP                             
322400                                 4010-IDBIL                               
322500                                 4010-IDVIN                               
322600                                 4010-IDARBREF                            
322700                                 4010-IDKLIENT                            
322800                                 4010-IDDC-RO                             
322900                                 4010-IDANALYS                            
323000                                 4010-IDZON                               
323100                                 4010-IDKST                               
323200                                 4010-IDLEVNR                             
323300                                                                          
323400     MOVE 'N'                 TO 4010-FLSDCLEV                            
323500                                                                          
323600     MOVE 0                   TO 4010-ADLAGOMR                            
323700                                 4010-ADGANG                              
323800                                 4010-ADPLATS                             
323900                                 4010-IDARTNR                             
324000                                 4010-IDLOPNR                             
324100                                 4010-ADLAGOMR-ORD                        
324200                                 4010-ADPLATS-ORD                         
324300                                 4010-IDPURAD                             
324400                                 4010-IDSPECEMB                           
324500                                 4010-KVAVBART                            
324600                                 4010-KVBEART-Q                           
324700                                 4010-KVHANTTI                            
324800                                 4010-REKSIFFR                            
324900                                 4010-VKORDNTO                            
325000                                 4010-VLORDNTO                            
325100                                 4010-IDLOPNR-RO                          
325200                                 4010-KDDSP                               
325300                                 4010-KDFARLIG                            
325400                                 4010-KDKVBRYT                            
325500                                 4010-KDPRODSL                            
325600                                 4010-KDORDING                            
325700                                 4010-KDVRINFO                            
325800                                 4010-KVSLATT                             
325900                                 4010-PRARTNTO                            
326000                                 4010-TIPRIS                              
326100                                 4010-TIRODAT                             
326200                                 4010-VKART                               
326300                                 4010-VKART-NTO                           
326400                                 4010-VLARTNTO                            
326500                                 4010-IDKAMPRF                            
326600                                 4010-IDKONTO                             
326700                                                                          
326800     MOVE 3410-IDBORD         TO 4010-IDBORD                              
326900                                                                          
327000     MOVE '278'               TO TRAUTF8-KDCP                             
327100     MOVE 'NO LINES'          TO TRAUTF8-TECONV-FROM                      
327200     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
327300     MOVE TRAUTF8-TECONV-TO   TO 4010-BEART                               
327400                                                                          
327500     MOVE W-ODEL-IDKUNDRF     TO 4010-IDKUNDRF                            
327600     MOVE '0000000   '        TO 4010-IDKUNDRF-RO                         
327700     MOVE 3410-IX             TO 4010-IDLOPNR-ORD                         
327800     MOVE 3410-IDLOPNR        TO 4010-IDLOPNR-PL                          
327900     MOVE W-ODEL-IDPLKLST     TO 4010-IDPLKLST                            
328000     MOVE W-ODEL-IDPRC        TO 4010-IDPRC                               
328100     MOVE W-ODEL-IDPRODNR     TO 4010-IDPRODNR                            
328200     MOVE 3410-IDUSER         TO 4010-IDUSER                              
328300     MOVE W-ODEL-IDDC         TO 4010-IDDC                                
328400     MOVE W-ODEL-KDFDKRAV     TO 4010-KDFDKRAV                            
328500     MOVE W-ODEL-DALSTORD (3:10) TO 4010-TILST                            
328600     MOVE W-ODEL-TIREGDAT     TO 4010-TIREGDAT                            
328700     MOVE W-ODEL-TIREGTID     TO 4010-TIREGTID                            
328800     MOVE W-ODEL-DARFS (3:10) TO 4010-TIRFS                               
328900     MOVE W-ODEL-DAUTSKR (3:6) TO 4010-TIUTSKR                            
329000     MOVE W-ODEL-TIUTSTID     TO 4010-TIUTSTID                            
329100     MOVE NEJ                 TO 4010-FLCOD                               
329200     MOVE NEJ                 TO 4010-FLINVEST                            
329300     MOVE NEJ                 TO 4010-FLPRTILL                            
329400     MOVE NEJ                 TO 4010-FLTILLK                             
329500     MOVE NEJ                 TO 4010-FLRESTN                             
329600     MOVE OHUV-IDSYSTEM       TO 4010-IDSYSTEM                            
329700     MOVE ZERO                TO 4010-IDPSN                               
329800                                 4010-VKART-FG                            
329900                                 4010-SUEQFG                              
330000                                 4010-VLFG                                
330100                                 4010-PRAVCOST                            
330200     .                                                                    
330300                                                                          
330400 CFAC-FYLL-I-DUMMYPOST-ETIK SECTION.                                      
330500     MOVE 'CFAC-FYLL-I-DUMMYPOST'   TO WS-CURRENT-SECTION                 
330600                                                                          
330700     MOVE ZERO              TO 4006-IDDISTR                               
330800                               4006-IDKUNDNR                              
330900                               4006-IDKUNDRF                              
331000                               4006-ADLAGOMR                              
331100                               4006-ADPLATS                               
331200                               4006-ADPLATS-ORD                           
331300                               4006-IDARTNR                               
331400                               4006-IDLOPNR                               
331500                               4006-ADLAGOMR-ORD                          
331600                               4006-ADGANG                                
331700                                                                          
331800     MOVE '278'               TO TRAUTF8-KDCP                             
331900     MOVE 'NO LINES'          TO TRAUTF8-TECONV-FROM                      
332000     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
332100     MOVE TRAUTF8-TECONV-TO   TO 4010-BEART                               
332200                                                                          
332300     MOVE SPACE             TO 4006-KDORDTYP-LDC                          
332400     MOVE ZERO              TO 4006-TIREPDAT                              
332500     MOVE SPACE             TO 4006-IDKUNDRF-WIP                          
332600                               4006-BERADREF                              
332700                               4006-BERADREF                              
332800                               4006-FLAKPLOC                              
332900                               4006-IDBORD                                
333000     MOVE 3410-IX           TO 4006-IDLOPNR-ORD                           
333100     MOVE 3410-IDLOPNR      TO 4006-IDLOPNR-PL                            
333200     MOVE W-ODEL-IDPLKLST   TO 4006-IDPLKLST                              
333300     MOVE W-ODEL-IDPRC      TO 4006-IDPRC                                 
333400     MOVE W-ODEL-IDPRODNR   TO 4006-IDPRODNR                              
333500     MOVE ZERO              TO 4006-IDRADNR                               
333600                               4006-IDSPECEMB                             
333700                               4006-KDARTHNT                              
333800     MOVE SPACE             TO 4006-KDARTURS                              
333900                               4006-KDEMBAL                               
334000     MOVE ZERO              TO 4006-KDFARLIG                              
334100                               4006-KDORDKL                               
334200     MOVE SPACE             TO 4006-KDSORT                                
334300     MOVE ZERO              TO 4006-KVAVBART                              
334400     MOVE SPACE             TO 4006-IDZON                                 
334500     MOVE ZERO              TO 4006-IDPSN                                 
334600                               4006-KDFRAKT                               
334700                               4006-IDDEPT                                
334800                                                                          
334900     .                                                                    
335000                                                                          
335100 CFB-START-4397  SECTION.                                                 
335200                                                                          
335300     MOVE 'CFB-START-4397'        TO WS-CURRENT-SECTION                   
335400                                                                          
335500     MOVE 'W4T397X '              TO 4397-TRANSKOD                        
335600                                     4397-LTERM-NAME                      
335700     MOVE WS-KDMFSFOR             TO 4397-KDMFSFOR                        
335800     MOVE 'L134'                  TO 4397-IDTRANS                         
335900     MOVE +117                    TO 4397-LL                              
336000     MOVE 3410-IDUSER             TO WS-IDUSER                            
336100     MOVE WS-IDANSTNR             TO 4397-IDANSTNR                        
336200     MOVE WS-IDPRODNR             TO 4397-IDPRODNR                        
336300     MOVE ZERO                    TO 4397-IDPLKLST                        
336400                                     4397-IDPURAD                         
336500     MOVE 4397-TRANSAREA          TO 4397-AREA                            
336600     PERFORM IMS-INSERT-4397-TRANS                                        
336700     .                                                                    
336800                                                                          
336900 D-UPPDATERA-PLOCKSATS SECTION.                                           
337000     MOVE 'D-UPPDAT-PLOCKSATS   ' TO WS-CURRENT-SECTION                   
337100                                                                          
337200     IF PLOCKSATS-ETIK-FINNS                                              
337300        MOVE DLI-IO-WDGX4004  TO W-ETIK-PLOCKSATS                         
337400        PERFORM IMS-48-GHU-WDGX4004                                       
337500        MOVE W-ETIK-PLOCKSATS TO DLI-IO-WDGX4004                          
337600        PERFORM IMS-49-REPL-WDGX4004                                      
337700     END-IF                                                               
337800     .                                                                    
337900                                                                          
338000 E-FIXA-ENGELSK-RADREF  SECTION.                                          
338100     MOVE 'E-FIXA-ENGELSK-RADREF' TO WS-CURRENT-SECTION                   
338200                                                                          
338300     MOVE OHUV-IDDISTR            TO DIST34-IDDISTR                       
338400     IF DIST34-ENGLAND-SDC AND                                            
338500           GMT-FLLDCKND = JA                                              
338600        IF (OHUV-KDORDKL = 0 OR OHUV-KDORDKL = 1                          
338700         OR OHUV-KDORDKL = 3 OR OHUV-KDORDKL = 4)                         
338800           MOVE '4017'                 TO 4017-IDHTYP                     
338900           MOVE 3410-IDPRODNR(ODEL-IX) TO W-4017-IDPRODNR                 
339000           MOVE 3410-IDPLKLST(ODEL-IX) TO W-4017-IDPLKLST                 
339100           MOVE LOW-VALUE              TO 4017-LOWVALUE                   
339200           PERFORM IMS-08-GHU-WDGX4017                                    
339300                                                                          
339400           IF SEGMENT-SAKNAS                                              
339500              MOVE '4017'                 TO 4017-IDHTYP                  
339600              MOVE 3410-IDPRODNR(ODEL-IX) TO 4017-IDPRODNR                
339700              MOVE 3410-IDPLKLST(ODEL-IX) TO 4017-IDPLKLST                
339800              MOVE LOW-VALUE              TO 4017-LOWVALUE                
339900              PERFORM IMS-50-ISRT-WDGX4017                                
340000           END-IF                                                         
340100                                                                          
340200           MOVE '1' TO W-4018-KDSEGKEY                                    
340300           PERFORM IMS-09-GHU-WDGX4018                                    
340400           MOVE +1 TO LDC-TAB-IX                                          
340500           MOVE +100 TO LDC-TAB-IX-MAX                                    
340600           PERFORM UNTIL LDC-TAB-IX > LDC-TAB-IX-MAX                      
340700                                                                          
340800              MOVE LDC-TAB-WIPID (LDC-TAB-IX)                             
340900                        TO 4018-BERADREF (LDC-TAB-IX)                     
341000              MOVE ZERO TO 4018-KVANTART (LDC-TAB-IX)                     
341100              ADD +1    TO LDC-TAB-IX                                     
341200           END-PERFORM                                                    
341300                                                                          
341400           MOVE '1'     TO 4018-KDSEGKEY                                  
341500                           W-4018-KDSEGKEY                                
341600           IF SEGMENT-FINNS                                               
341700              PERFORM IMS-51-REPL-WDGX4018                                
341800           ELSE                                                           
341900              PERFORM IMS-52-ISRT-WDGX4018                                
342000           END-IF                                                         
342100        END-IF                                                            
342200     END-IF                                                               
342300     .                                                                    
342400                                                                          
342500 S02-ORDERBEKRAFTELSE SECTION.                                            
342600     MOVE 'S02-ORDERBEKRAFTELSE'    TO WS-CURRENT-SECTION                 
342700                                                                          
342800     IF WS-ANTOBKR = 0                                                    
342900        PERFORM S02A-KOLLA-LOPNR                                          
343000     END-IF                                                               
343100                                                                          
343200     MOVE ORAD-IDORDER            TO OBKR-IDORDER                         
343300     MOVE ORAD-IDARTNR            TO OBKR-IDARTNR                         
343400     MOVE W-Q101KY-MIN-IDLOPNR    TO OBKR-IDLOPNR                         
343500     MOVE 1                       TO OBKR-IDSEKVNR                        
343600     MOVE ORAD-IDDC               TO OBKR-IDDC                            
343700     MOVE ORAD-IDDC-RO            TO OBKR-IDDC-RO                         
343800     MOVE ORAD-KDOI               TO OBKR-KDOI                            
343900     MOVE ORAD-CLEARGROUP         TO OBKR-CLEARGROUP                      
344000     IF WS-KDORDBEK    = 91                                               
344100        AND                                                               
344200        ORAD-TIRODAT = 0                                                  
344300        MOVE 90                   TO OBKR-KDORDBEK                        
344400        MOVE IDPGM                TO OBKR-IDPGM                           
344500     ELSE                                                                 
344600        MOVE WS-KDORDBEK          TO OBKR-KDORDBEK                        
344700        MOVE WS-IDPGM             TO OBKR-IDPGM                           
344800     END-IF                                                               
344900     MOVE SPACE                   TO OBKR-BEERS                           
345000     MOVE OHUV-BEKUNDRF           TO OBKR-BEKUNDRF                        
345100     MOVE ORAD-BERADREF           TO OBKR-BERADREF                        
345200     MOVE ORAD-BEVOLREF           TO OBKR-BEVOLREF                        
345300     MOVE ORAD-IDKAMPRF           TO OBKR-IDKAMPRF                        
345400     MOVE 0                       TO OBKR-DIERS-KVOT                      
345500     MOVE ORAD-FLAKPLOC           TO OBKR-FLAKPLOC                        
345600     MOVE ORAD-FLINVEST           TO OBKR-FLINVEST                        
345700     MOVE JA                      TO OBKR-FLOBOK                          
345800     MOVE NEJ                     TO OBKR-FLOBTRAN                        
345900     MOVE NEJ                     TO OBKR-FLOBPRT                         
346000     MOVE ORAD-FLPRTILL           TO OBKR-FLPRTILL                        
346100     MOVE ORAD-FLRESTN            TO OBKR-FLRESTN                         
346200     MOVE JA                      TO OBKR-FLSLATT                         
346300     MOVE ORAD-FLTILLK            TO OBKR-FLTILLK                         
346400     MOVE 0                       TO OBKR-IDARTNR-TILLK                   
346500     MOVE ORAD-IDDISTR            TO OBKR-IDDISTR                         
346600     MOVE ORAD-IDKUNDNR           TO OBKR-IDKUNDNR                        
346700     MOVE W-ODEL-IDKUNDRF         TO OBKR-IDKUNDRF                        
346800                                                                          
346900     MOVE OHUV-IDKONTO            TO WS-IDKONTO                           
347000     MOVE ORAD-IDKUNDRF-RO        TO OBKR-IDKUNDRF-RO                     
347100     MOVE ORAD-IDLEVNR            TO OBKR-IDLEVNR                         
347200     MOVE ORAD-IDLOPNR-RO         TO OBKR-IDLOPNR-RO                      
347300     MOVE ORAD-IDSYSTEM           TO OBKR-IDSYSTEM                        
347400     MOVE ORAD-KDDSP              TO OBKR-KDDSP                           
347500     MOVE 0                       TO OBKR-KDERS                           
347600     MOVE ORAD-KDKVBRYT           TO OBKR-KDKVBRYT                        
347700     MOVE ORAD-KDPRTYP            TO OBKR-KDPRTYP                         
347800     MOVE ORAD-KDTPOTYP           TO OBKR-KDTPOTYP                        
347900     MOVE ORAD-KDVRINFO           TO OBKR-KDVRINFO                        
348000                                                                          
348100     IF WS-KDORDBEK = 90 OR 91                                            
348200        MOVE WS-KVROS             TO OBKR-KVRO                            
348300        MOVE 0                    TO OBKR-KVBEART                         
348400     ELSE                                                                 
348500        MOVE 0                    TO OBKR-KVRO                            
348600        MOVE ORAD-KVBEART-Q       TO OBKR-KVBEART                         
348700     END-IF                                                               
348800                                                                          
348900     IF WS-KDORDBEK = 92                                                  
349000        COMPUTE OBKR-KVPRERO = ORAD-KVBEART-Q - WS-KVAVBART               
349100     ELSE                                                                 
349200        MOVE 0                    TO OBKR-KVPRERO                         
349300     END-IF                                                               
349400                                                                          
349500     IF WS-KDORDBEK = 80                                                  
349600        COMPUTE OBKR-KVANNANT = ORAD-KVBEART-Q - WS-KVAVBART              
349700     ELSE                                                                 
349800        MOVE 0                    TO OBKR-KVANNANT                        
349900     END-IF                                                               
350000                                                                          
350100     MOVE WS-KVAVBART             TO OBKR-KVAVBART                        
350200     MOVE ORAD-KVBEART-Q          TO OBKR-KVBEART-Q                       
350300     MOVE ORAD-KVPREAVB           TO OBKR-KVPREAVB                        
350400     IF DCS-CDC                                                           
350500       MOVE AREG-KVQPACK-1        TO OBKR-KVQPACK                         
350600     ELSE                                                                 
350700       MOVE 0                     TO OBKR-KVQPACK                         
350800     END-IF                                                               
350900     MOVE 0                       TO OBKR-KVBEART-TILLK                   
351000     MOVE ORAD-PRARTNTO           TO OBKR-PRARTNTO                        
351100     MOVE ORAD-DEAL-PR-LINE       TO OBKR-DEAL-PR-LINE                    
351200     MOVE ORAD-PRBPRIS            TO OBKR-PRBPRIS                         
351300     MOVE ORAD-REKSIFFR           TO OBKR-REKSIFFR                        
351400     MOVE 0                       TO OBKR-REKSIFFR-TILLK                  
351500     MOVE ORAD-RERF-RAD           TO OBKR-RERF-RAD                        
351600     MOVE ORAD-KVSLATT            TO OBKR-KVSLATT                         
351700     MOVE AREG-TIDISPIN           TO OBKR-TIDISPIN                        
351800     MOVE ORAD-TIPRIS             TO OBKR-TIPRIS                          
351900     MOVE 3410-TIAAMMDD           TO OBKR-TIREGDAT                        
352000     MOVE 3410-TIHHMMSS           TO OBKR-TIREGTID                        
352100                                                                          
352200     IF WS-KDORDBEK = 90 OR 91                                            
352300        MOVE 3410-TIAAMMDD        TO OBKR-TIRODAT                         
352400     ELSE                                                                 
352500        MOVE ZERO                 TO OBKR-TIRODAT                         
352600     END-IF                                                               
352700                                                                          
352800     MOVE ARB-KDFRAKT             TO OBKR-KDFRAKT                         
352900     MOVE OHUV-KDORDKL            TO OBKR-KDORDKL                         
353000     MOVE SPACE                   TO OBKR-IDBIL                           
353100     MOVE ORAD-TITPO              TO OBKR-TITPO                           
353200     MOVE OHUV-KDORDTYP-LDC       TO OBKR-KDORDTYP-LDC                    
353300     MOVE OHUV-TIREPDAT           TO OBKR-TIREPDAT                        
353400     MOVE ORAD-IDKUNDRF-WIP       TO OBKR-IDKUNDRF-WIP                    
353500     MOVE ZERO                    TO OBKR-TIDLEVDAT                       
353600     MOVE ORAD-PRAVCOST           TO OBKR-PRAVCOST                        
353700     PERFORM S02B-DATUM-9KOMPL                                            
353800                                                                          
353900     IF OBKR-KDORDBEK = 90 OR 91 OR 92 OR 93                              
354000        MOVE ORAD-IDARTNR         TO W-IDARTNR                            
354100        MOVE DCS-IDLANDX2         TO W-IDLAND                             
354200        PERFORM IMS-54-GU-WDK712                                          
354300        IF SEGMENT-FINNS AND LART-FLREFERAL = JA                          
354400           MOVE 98                TO OBKR-KDORDBEK                        
354500        END-IF                                                            
354600     END-IF                                                               
354700                                                                          
354800     PERFORM IMS-16-ISRT-WDQ101                                           
354900                                                                          
355000     PERFORM UNTIL SEGMENT-FINNS                                          
355100        ADD 1 TO OBKR-IDSEKVNR                                            
355200        PERFORM IMS-16-ISRT-WDQ101                                        
355300     END-PERFORM                                                          
355400     ADD 1                        TO WS-ANTOBKR                           
355500     .                                                                    
355600     EJECT                                                                
355700 S02A-KOLLA-LOPNR SECTION.                                                
355800     MOVE 'S02A-KOLLA-LOPNR    '    TO WS-CURRENT-SECTION                 
355900                                                                          
356000     MOVE ORAD-IDORDER    TO W-Q101KY-MIN-IDORDER                         
356100                             W-Q101KY-MAX-IDORDER                         
356200     MOVE ORAD-IDARTNR    TO W-Q101KY-MIN-IDARTNR                         
356300                             W-Q101KY-MAX-IDARTNR                         
356400     MOVE 1               TO W-Q101KY-MIN-IDLOPNR                         
356500                             W-Q101KY-MAX-IDLOPNR                         
356600                             W-Q101KY-MIN-IDSEKVNR                        
356700                             W-Q101KY-MAX-IDSEKVNR                        
356800                                                                          
356900     PERFORM IMS-15-GU-WDQ101                                             
357000     PERFORM UNTIL SEGMENT-SAKNAS                                         
357100        ADD  1            TO W-Q101KY-MIN-IDLOPNR                         
357200                             W-Q101KY-MAX-IDLOPNR                         
357300        PERFORM IMS-15-GU-WDQ101                                          
357400     END-PERFORM                                                          
357500     .                                                                    
357600                                                                          
357700 S02B-DATUM-9KOMPL SECTION.                                               
357800     MOVE 'S02B-DATUM-9KOMPL   '    TO WS-CURRENT-SECTION                 
357900                                                                          
358000     MOVE ORAD-TIREGDAT               TO WS-DATUM-9KOMPL                  
358100     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-DATUM-9KOMPL (1:2)            
358200                                                                          
358300     COMPUTE OBKR-TITIREGD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
358400                                                                          
358500* OBKR-TIORDREG HAR FORMEN S9(7) COMP-3 (0YYMMDD) OCH MÅSTE DÄRFÖR        
358600* FLYTTAS FÖRE WS-SEKELTAL.                                               
358700     MOVE OHUV-TIREGDAT    TO OBKR-TIORDREG                               
358800     MOVE OBKR-TIORDREG    TO WS-DATUM-9KOMPL                             
358900     MOVE '20'             TO WS-DATUM-9KOMPL (1:2)                       
359000     COMPUTE OBKR-TITIORDD-9KOMPL = 999999999 - WS-DATUM-9KOMPL           
359100     .                                                                    
359200                                                                          
359300 S03-NYUPPLAGG-RESTORDER SECTION.                                         
359400     MOVE 'S03-NYUPPLAGG-RESTORDER' TO WS-CURRENT-SECTION                 
359500                                                                          
359600     PERFORM S03A-HAMTA-PRIORITETSKOD                                     
359700                                                                          
359800     MOVE ORAD-IDDISTR            TO RAD-IDDISTR                          
359900     MOVE ORAD-IDKUNDNR           TO RAD-IDKUNDNR                         
360000     MOVE SPACE                   TO RAD-IDKUNDRF                         
360100     MOVE ORAD-IDKUNDRF (3:5)     TO RAD-IDKUNDRF                         
360200     MOVE ORAD-IDARTNR            TO RAD-IDARTNR                          
360300     MOVE 1                       TO RAD-IDLOPNR                          
360400     MOVE OHUV-BEKUNDRF           TO RAD-BEKUNDRF                         
360500     MOVE ORAD-BERADREF           TO RAD-BERADREF                         
360600     MOVE NEJ                     TO RAD-FLERS                            
360700     MOVE AREG-IDANSK             TO RAD-IDANSK                           
360800                                                                          
360900     MOVE OHUV-IDANALYS           TO RAD-IDANALYS                         
361000     MOVE OHUV-IDKST              TO RAD-IDKST                            
361100     MOVE OHUV-IDKONTO            TO WS-IDKONTO                           
361200     MOVE WS-IDKONTO              TO RAD-IDKONTO                          
361300     MOVE '00000     '            TO RAD-IDKUNDRF-LEV                     
361400     MOVE ORAD-IDDC-RO            TO RAD-IDDC                             
361500                                     RAD-IDDC-RO                          
361600     MOVE ORAD-KDDSP              TO RAD-KDDSP                            
361700     MOVE OHUV-KDFAKTYP           TO RAD-KDFAKTYP                         
361800     MOVE ARB-KDFRAKT             TO RAD-KDFRAKT                          
361900     MOVE ARB-KDROPACK            TO RAD-KDROPACK                         
362000     MOVE ORAD-IDARBREF           TO RAD-IDARBREF                         
362100     MOVE ORAD-KDKVBRYT           TO RAD-KDKVBRYT                         
362200     MOVE ORAD-KDORDING           TO RAD-KDORDING                         
362300     MOVE ORAD-KDOI               TO RAD-KDOI                             
362400     MOVE ORAD-CLEARGROUP         TO RAD-CLEARGROUP                       
362500     MOVE ORAD-KDORDKL            TO RAD-KDORDKL                          
362600     MOVE ORAD-KDPRODSL           TO RAD-KDPRODSL                         
362700     MOVE WS-KDRAPRIO             TO RAD-KDRAPRIO                         
362800     MOVE WS-KDROO                TO RAD-KDROO                            
362900     MOVE WS-KVROS                TO RAD-KVRO                             
363000                                     RAD-KVART                            
363100     MOVE '2'                     TO RAD-KDSTARAD                         
363200     MOVE ORAD-KDTPOTYP           TO RAD-KDTPOTYP                         
363300     MOVE ORAD-KDVRINFO           TO RAD-KDVRINFO                         
363400     MOVE ORAD-PRARTNTO           TO RAD-PRARTNTO                         
363500     MOVE ORAD-DEAL-PR-LINE       TO RAD-DEAL-PR-LINE                     
363600     MOVE ORAD-REKSIFFR           TO RAD-REKSIFFR                         
363700     MOVE 0                       TO RAD-TIAVBOKN                         
363800     MOVE ORAD-TIREGDAT           TO RAD-TIREGDAT                         
363900     MOVE 0                       TO RAD-TIRES                            
364000     MOVE 3410-TIAAMMDD           TO RAD-DARODAT                          
364100     MOVE 20                      TO RAD-DARODAT (1:2)                    
364200     MOVE ORAD-TITPO              TO RAD-TITPO                            
364300     MOVE ORAD-KDPRTYP            TO RAD-KDPRTYP                          
364400     MOVE ORAD-BEVOLREF           TO RAD-BEVOLREF                         
364500     MOVE ORAD-FLINVEST           TO RAD-FLINVEST                         
364600     MOVE ORAD-FLPRTILL           TO RAD-FLPRTILL                         
364700     MOVE JA                      TO RAD-FLTPOBEK                         
364800     MOVE ORAD-IDKAMPRF           TO RAD-IDKAMPRF                         
364900     MOVE ORAD-IDLEVNR            TO RAD-IDLEVNR                          
365000     MOVE ORAD-IDSYSTEM           TO RAD-IDSYSTEM                         
365100     MOVE ORAD-KVBEART-Q          TO RAD-KVBEART-Q                        
365200     MOVE ORAD-TIREGTID           TO RAD-TIREGTID                         
365300     MOVE 0                       TO RAD-DASENDAT                         
365400                                     RAD-TISENBEK-KL                      
365500     MOVE OHUV-KDORDTYP-LDC       TO RAD-KDORDTYP-LDC                     
365600     MOVE OHUV-TIREPDAT           TO RAD-TIREPDAT                         
365700     MOVE ORAD-IDKUNDRF-WIP       TO RAD-IDKUNDRF-WIP                     
365800     MOVE ORAD-PRAVCOST           TO RAD-PRAVCOST                         
365900                                                                          
366000     MOVE ORAD-IDPRQUES           TO RAD-IDPRQUES                         
366100     MOVE ORAD-PRARTNTO-LOC       TO RAD-PRARTNTO-LOC                     
366200     MOVE ORAD-PRARTNTO-LOCPREL   TO RAD-PRARTNTO-LOCPREL                 
366300     MOVE ORAD-PRARTBTO-LOC       TO RAD-PRARTBTO-LOC                     
366400     MOVE ORAD-KDVALISO           TO RAD-KDVALISO                         
366500     MOVE ORAD-KDVAT              TO RAD-KDVAT                            
366600     MOVE ORAD-RERAB              TO RAD-RERAB                            
366700     MOVE ORAD-KDRAB              TO RAD-KDRAB                            
366800     MOVE ORAD-BEART-VIPS         TO RAD-BEART-VIPS                       
366900     PERFORM S36-ANDRA-WDC711                                             
367000                                                                          
367100     PERFORM IMS-19-ISRT-WDA501                                           
367200                                                                          
367300     PERFORM UNTIL SEGMENT-FINNS                                          
367400        ADD 1 TO RAD-IDLOPNR                                              
367500        PERFORM IMS-19-ISRT-WDA501                                        
367600     END-PERFORM                                                          
367700     PERFORM S03B-KOLLA-CROSS-DOCKING                                     
367800                                                                          
367810     IF LYNK-NON-API                                                      
367820       PERFORM S03D-CR-NON-API-EVENT                                      
367830     ELSE                                                                 
367900*NEW                                                                      
368000*EVENT HANDLING                                                           
368100*LYND = DÖSKALLE  WDQ2C                                                   
368200       IF OHUV-IDSYSTEM = 'LYND' OR 'TADD'                                
368300         MOVE OHUV-IDDISTR             TO W-IDDISTR-CSEQ                  
368400         MOVE OHUV-IDKUNDNR            TO W-IDKUNDNR-CSEQ                 
368500         MOVE ORAD-IDKUNDRF-RO(3:5)    TO W-IDORDNR5-CSEQ                 
368600         PERFORM IMS-GU-WDQ201-CSEQ-GE                                    
368700         IF SEGMENT-FINNS                                                 
368800           MOVE CSQ-OHUV-IDDISTR           TO WS-IDDISTR-EVENT            
368900           MOVE CSQ-OHUV-IDKUNDNR          TO WS-IDKUNDNR-EVENT           
369000           MOVE CSQ-OHUV-IDORDNR7          TO WS-IDORDNR7-EVENT           
369100           MOVE CSQ-OHUV-TIREGDAT          TO WS-TIREGDAT-EVENT           
369200           MOVE JA                         TO CREATE-EVENT-SW             
369300         END-IF                                                           
369400       ELSE                                                               
369500*LYNV = VOR KUNDRF-LEV -A6                                                
369600         IF OHUV-IDSYSTEM = 'LYNV' OR 'TADV'                              
369700           MOVE LOW-VALUE              TO W-WDA6BSEQ-MIN-X                
369800           MOVE HIGH-VALUE             TO W-WDA6BSEQ-MAX-X                
369900                                                                          
370000           MOVE OHUV-IDDISTR           TO W-A6BSEQ-MIN-IDDISTR            
370100                                          W-A6BSEQ-MAX-IDDISTR            
370200           MOVE OHUV-IDKUNDNR          TO W-A6BSEQ-MIN-IDKUNDNR           
370300                                          W-A6BSEQ-MAX-IDKUNDNR           
370400           MOVE OHUV-IDKUNDRF         TO W-A6BSEQ-MIN-IDKUNDRF-LEV        
370500                                         W-A6BSEQ-MAX-IDKUNDRF-LEV        
370600           PERFORM IMS-GU-SEQB-WDA601                                     
370700           IF SEGMENT-FINNS                                               
370800             MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                
370900             MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT               
371000             MOVE VOR-IDKUNDRF(1:7)    TO WS-IDORDNR7-EVENT               
371100             MOVE VOR-TIREGDAT-URSP    TO WS-TIREGDAT-EVENT               
371200             MOVE JA                   TO CREATE-EVENT-SW                 
371300           END-IF                                                         
371400         ELSE                                                             
371500*LYNB = VERKSTADS/REPARATIONS-ORDER -A5                                   
371600           IF OHUV-IDSYSTEM = 'LYNB' OR 'TADB'                            
371700                                                                          
371800             MOVE OHUV-IDDISTR         TO W-IDDISTR-A5-MIN                
371900                                          W-IDDISTR-A5-MAX                
372000             MOVE OHUV-IDKUNDNR        TO W-IDKUNDNR-A5-MIN               
372100                                          W-IDKUNDNR-A5-MAX               
372200             MOVE OHUV-KDORDKL         TO W-KDORDKL                       
372300             MOVE OHUV-IDORDNR7(3:5)   TO W-IDKUNDRF-LEV                  
372400*                                                                         
372500             PERFORM IMS-GU-WDA501                                        
372600             IF SEGMENT-FINNS                                             
372700               MOVE OHUV-IDDISTR       TO WS-IDDISTR-EVENT                
372800               MOVE OHUV-IDKUNDNR      TO WS-IDKUNDNR-EVENT               
372900               MOVE  RAD-IDORDNR5      TO WS-IDORDNR7-EVENT               
373000               MOVE  RAD-TIREGDAT      TO WS-TIREGDAT-EVENT               
373100               MOVE JA                 TO CREATE-EVENT-SW                 
373200             END-IF                                                       
373300           ELSE                                                           
373400             MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                
373500             MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT               
373600             MOVE OHUV-IDORDNR7        TO WS-IDORDNR7-EVENT               
373700             MOVE OHUV-TIREGDAT        TO WS-TIREGDAT-EVENT               
373800             MOVE JA                   TO CREATE-EVENT-SW                 
373900           END-IF                                                         
374000         END-IF                                                           
374100       END-IF                                                             
374110     END-IF                                                               
374200                                                                          
374300     IF CREATE-EVENT                                                      
374400                                                                          
374500       MOVE OHUV-IDSYSTEM(1:4) TO EVENT-SW                                
374600       IF EVENT-OK OR LYNK-NON-API                                        
374700                                                                          
374800         IF (OHUV-IDSYSTEM(1:3) = 'LYN') OR LYNK-NON-API                  
374900           MOVE 'L'            TO WS-PARTNER                              
375000         END-IF                                                           
375100         IF OHUV-IDSYSTEM(1:3) = 'TAD'                                    
375200           MOVE 'T'            TO WS-PARTNER                              
375300         END-IF                                                           
375400         IF OHUV-IDSYSTEM(1:3) = 'POL'                                    
375500           MOVE 'P'            TO WS-PARTNER                              
375600         END-IF                                                           
375700         IF OHUV-IDSYSTEM(1:3) = 'ECO'                                    
375800           MOVE 'E'            TO WS-PARTNER                              
375900         END-IF                                                           
376000         IF OHUV-IDSYSTEM(1:3) = 'ACC'                                    
376100           MOVE 'A'            TO WS-PARTNER                              
376200         END-IF                                                           
376300         IF OHUV-IDSYSTEM(1:3) = 'APA'                                    
376400           MOVE 'K'            TO WS-PARTNER                              
376500         END-IF                                                           
376600         IF OHUV-IDSYSTEM(1:3) = 'APB'                                    
376700           MOVE 'B'            TO WS-PARTNER                              
376800         END-IF                                                           
376900         IF OHUV-IDSYSTEM(1:3) = 'APC'                                    
377000           MOVE 'C'            TO WS-PARTNER                              
377100         END-IF                                                           
377200         IF OHUV-IDSYSTEM(1:3) = 'APD'                                    
377300           MOVE 'D'            TO WS-PARTNER                              
377400         END-IF                                                           
377500         IF OHUV-IDSYSTEM(1:3) = 'APE'                                    
377600           MOVE 'M'            TO WS-PARTNER                              
377700         END-IF                                                           
377800         IF OHUV-IDSYSTEM(1:3) = 'APF'                                    
377900           MOVE 'F'            TO WS-PARTNER                              
378000         END-IF                                                           
378100         IF OHUV-IDSYSTEM(1:3) = 'APG'                                    
378200           MOVE 'G'            TO WS-PARTNER                              
378300         END-IF                                                           
378400         IF OHUV-IDSYSTEM(1:3) = 'APH'                                    
378500           MOVE 'H'            TO WS-PARTNER                              
378600         END-IF                                                           
378700         IF OHUV-IDSYSTEM(1:3) = 'API'                                    
378800           MOVE 'I'            TO WS-PARTNER                              
378900         END-IF                                                           
379000         IF OHUV-IDSYSTEM(1:3) = 'APJ'                                    
379100           MOVE 'J'            TO WS-PARTNER                              
379200         END-IF                                                           
379300                                                                          
379400         MOVE SPACE             TO Z430-REQU-TIMESTAMP                    
379500         PERFORM S03C-CREATE-EVENT-152                                    
379600       END-IF                                                             
379700     END-IF                                                               
379800                                                                          
379900     .                                                                    
380000     EJECT                                                                
380100 S03A-HAMTA-PRIORITETSKOD SECTION.                                        
380200     MOVE 'S03A-HAMTA-PRIORITETSKOD' TO WS-CURRENT-SECTION                
380300                                                                          
380400     IF OHUV-IDKAMPRF > 0                                                 
380500        MOVE 4             TO W-4512-KDTPOTYP                             
380600     ELSE                                                                 
380700        MOVE OHUV-KDTPOTYP TO W-4512-KDTPOTYP                             
380800     END-IF                                                               
380900                                                                          
381000     MOVE OHUV-KDORDKL TO W-4512-KDORDKL                                  
381100     MOVE OHUV-IDDISTR TO W-4512-IDDISTR-FOM                              
381200                          W-4512-IDDISTR-TOM                              
381300                                                                          
381400     PERFORM IMS-20-GU-WDGX4512                                           
381500     IF SEGMENT-FINNS                                                     
381600        MOVE 4512-KDRAPRIO  TO WS-KDRAPRIO                                
381700     ELSE                                                                 
381800        MOVE 99             TO WS-KDRAPRIO                                
381900     END-IF                                                               
382000     .                                                                    
382100                                                                          
382200 S03B-KOLLA-CROSS-DOCKING   SECTION.                                      
382300     MOVE 'S03B-KOLLA-CROSS-DOCKING' TO WS-CURRENT-SECTION                
382400                                                                          
382500     MOVE ORAD-IDARTNR       TO W-IDARTNR                                 
382600     PERFORM IMS-21-GHU-WDK611                                            
382700     MOVE 1                  TO IX-CD-OMR                                 
382800     PERFORM UNTIL IX-CD-OMR > 4                                          
382900     OR ORAD-ADLAGOMR = CLAG-ADLAGOMR-CD (IX-CD-OMR)                      
383000       ADD 1                 TO IX-CD-OMR                                 
383100     END-PERFORM                                                          
383200     IF IX-CD-OMR <= 4                                                    
383300                                                                          
383400*BOKA UPP CROSS DOCKING SALDO                                             
383500                                                                          
383600        ADD ORAD-KVBEART-Q                                                
383700                         TO CLAG-KVLS-CD (IX-CD-OMR)                      
383800     END-IF                                                               
383900     PERFORM IMS-22-REPL-WDK611                                           
384000     .                                                                    
384100 S03C-CREATE-EVENT-152 SECTION.                                           
384200     MOVE 'STA S03C-CREATE-EVENT'   TO WS-CURRENT-SECTION                 
384300                                                                          
384310     IF LYNK-NON-API                                                      
384320        MOVE 'LYNK'             TO Z430-REQU-IDEVENTREC                   
384330     ELSE                                                                 
384400        MOVE OHUV-IDSYSTEM      TO Z430-REQU-IDEVENTREC                   
384410     END-IF                                                               
384500                                                                          
384600     MOVE IDMSGVER              TO Z430-REQU-IDMSGVER                     
384700     MOVE 'PURCHASEORDER'       TO Z430-REQU-IDEVENT                      
384800     MOVE 'UPDATE'              TO Z430-REQU-IDEVENTTYP                   
384900***  MOVE WS-TIMESTAMP          TO Z430-REQU-TIMESTAMP                    
385000     MOVE FUNCTION CURRENT-DATE TO Z430-REQU-TIMESTAMP                    
385100     MOVE 'WAPIORD'             TO Z430-REQU-IDCPYTXT                     
385200     MOVE WS-IDEVENTORDREF      TO Z430-IDAPIORDREF                       
385300     MOVE '152'                 TO Z430-IDMSG                             
385400     MOVE 'ONE ORDER LINE WAS BACKORDERED'                                
385500                                TO Z430-TEMFSINF                          
385600                                                                          
385700*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
385800*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
385900     MOVE 'WZ0430X '           TO MSG-KDTRANS-1                           
386000     MOVE 'Z430'               TO MSG-IDTRANS-1                           
386100     MOVE '1'                  TO MSG-KDMFSFOR-1                          
386200     MOVE 'WZ0430I1'           TO MSG-KOM-IDCPYTXT                        
386300     STRING 'EVE' WS-PARTNER WS-IDDISTR-EVENT                             
386400          DELIMITED BY SIZE INTO MSG-KOM-IDSNDNOD                         
386500                                                                          
386600     ADD  1                    TO MSG-KOM-TIKLOCK                         
386700     COMPUTE MSG-KVLL = LENGTH OF Z430-REQU-WZ0430I1 + 17                 
386800     MOVE Z430-REQU-WZ0430I1     TO MSG-INDATA-MINUS-1-TRANSKOD           
386900                                                                          
387000     CALL W006KOM USING MSG-PCB                                           
387100                        0693-PCB                                          
387200                        KOM-WDP8-PCB                                      
387300                        MSG-KOM-WMSGKOM                                   
387400                        MSG-IO-AREA                                       
387500                                                                          
387600     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
387700        MOVE                                                              
387800        'FELAKTIG UPPDATERING AV PÅ KOMMUNIKATIONS DB'                    
387900                                     TO ERROR-TEXT                        
388000        CALL ABEND USING RKOD-ABEND-WITHOUT-DUMP                          
388100     END-IF                                                               
388200     .                                                                    
388300     EJECT                                                                
388400                                                                          
388500                                                                          
388600 S03D-CR-NON-API-EVENT   SECTION.                                         
388700     MOVE 'S03D-CR-NON-API-EVENT   ' TO WS-CURRENT-SECTION                
388800                                                                          
388801     MOVE NEJ                  TO CREATE-EVENT-SW                         
388802     IF VOR                                                               
388803       MOVE LOW-VALUE              TO W-WDA6BSEQ-MIN-X                    
388804       MOVE HIGH-VALUE             TO W-WDA6BSEQ-MAX-X                    
388805                                                                          
388806       MOVE OHUV-IDDISTR           TO W-A6BSEQ-MIN-IDDISTR                
388807                                      W-A6BSEQ-MAX-IDDISTR                
388808       MOVE OHUV-IDKUNDNR          TO W-A6BSEQ-MIN-IDKUNDNR               
388809                                      W-A6BSEQ-MAX-IDKUNDNR               
388810       MOVE OHUV-IDKUNDRF          TO W-A6BSEQ-MIN-IDKUNDRF-LEV           
388811                                      W-A6BSEQ-MAX-IDKUNDRF-LEV           
388812       PERFORM IMS-GU-SEQB-WDA601                                         
388813       IF SEGMENT-FINNS                                                   
388814         MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                    
388815         MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT                   
388816         MOVE VOR-IDKUNDRF(1:7)    TO WS-IDORDNR7-EVENT                   
388817         MOVE VOR-TIREGDAT-URSP    TO WS-TIREGDAT-EVENT                   
388818         MOVE JA                   TO CREATE-EVENT-SW                     
388819       END-IF                                                             
388820     ELSE                                                                 
388821       IF (ORAD-IDKUNDRF-RO NOT = '0000000   ') AND                       
388822          (ORAD-IDKUNDRF-RO NOT = '00000     ')                           
388823         MOVE OHUV-IDDISTR             TO W-IDDISTR-CSEQ                  
388824         MOVE OHUV-IDKUNDNR            TO W-IDKUNDNR-CSEQ                 
388825         MOVE ORAD-IDKUNDRF-RO(3:5)    TO W-IDORDNR5-CSEQ                 
388826         PERFORM IMS-GU-WDQ201-CSEQ-GE                                    
388827         IF SEGMENT-FINNS                                                 
388828           MOVE CSQ-OHUV-IDDISTR           TO WS-IDDISTR-EVENT            
388829           MOVE CSQ-OHUV-IDKUNDNR          TO WS-IDKUNDNR-EVENT           
388830           MOVE CSQ-OHUV-IDORDNR7          TO WS-IDORDNR7-EVENT           
388831           MOVE CSQ-OHUV-TIREGDAT          TO WS-TIREGDAT-EVENT           
388832           MOVE JA                         TO CREATE-EVENT-SW             
388833         END-IF                                                           
388834       ELSE                                                               
388835         MOVE OHUV-IDDISTR         TO W-IDDISTR-A5-MIN                    
388836                                      W-IDDISTR-A5-MAX                    
388837         MOVE OHUV-IDKUNDNR        TO W-IDKUNDNR-A5-MIN                   
388838                                      W-IDKUNDNR-A5-MAX                   
388839         MOVE OHUV-KDORDKL         TO W-KDORDKL                           
388840         MOVE OHUV-IDORDNR7(3:5)   TO W-IDKUNDRF-LEV                      
388841                                                                          
388842         PERFORM IMS-GU-WDA501                                            
388843         IF SEGMENT-FINNS                                                 
388844           MOVE OHUV-IDDISTR       TO WS-IDDISTR-EVENT                    
388845           MOVE OHUV-IDKUNDNR      TO WS-IDKUNDNR-EVENT                   
388846           MOVE  RAD-IDORDNR5      TO WS-IDORDNR7-EVENT                   
388847           MOVE  RAD-TIREGDAT      TO WS-TIREGDAT-EVENT                   
388848           MOVE JA                 TO CREATE-EVENT-SW                     
388849         END-IF                                                           
388850       END-IF                                                             
388851     END-IF                                                               
388852     IF CREATE-EVENT-SW = NEJ                                             
388853        MOVE OHUV-IDDISTR         TO WS-IDDISTR-EVENT                     
388854        MOVE OHUV-IDKUNDNR        TO WS-IDKUNDNR-EVENT                    
388855        MOVE OHUV-IDORDNR7        TO WS-IDORDNR7-EVENT                    
388856        MOVE OHUV-TIREGDAT        TO WS-TIREGDAT-EVENT                    
388857        MOVE JA                   TO CREATE-EVENT-SW                      
388858     END-IF                                                               
388859     .                                                                    
388860     EJECT                                                                
388861                                                                          
388862                                                                          
388863 S04-UPPDAT-ART-REG-WDK6 SECTION.                                         
388864     MOVE 'S04-UPPDAT-ART-REG-WDK6 ' TO WS-CURRENT-SECTION                
388870                                                                          
388900     IF ARTIKEL-OK           AND                                          
389000        OHUV-FLLSBOK = JA    AND                                          
389100        ORAD-IDLEVNR = SPACE                                              
389200                                                                          
389300        PERFORM S04A-UPPDAT-KAMPANJ-WDM2                                  
389400        IF WS-KVROS  NOT = 0  OR                                          
389500           WS-KVLS   NOT = 0  OR                                          
389600           WS-KVEFRS NOT = 0  OR                                          
389700           WS-KVRESS NOT = 0                                              
389800                                                                          
389900           MOVE ORAD-IDARTNR    TO W-IDARTNR                              
390000           PERFORM IMS-21-GHU-WDK611                                      
390100           IF SEGMENT-FINNS                                               
390200              IF DCS-CDC                                                  
390300                ADD WS-KVEFRS         TO CLAG-KVEFRS                      
390400****** FÖLJANDE SKALL LOGGA SALDOT PÅ DATABAS WDL9. *******               
390500                IF WS-KVEFRS = 0                                          
390600                  MOVE '+'            TO LOGG-IDTECKEN-KVEFRS             
390700                  MOVE 0              TO LOGG-KVART-SALDO                 
390800                ELSE                                                      
390900                  MOVE '+'            TO LOGG-IDTECKEN-KVEFRS             
391000                  MOVE WS-KVEFRS      TO LOGG-KVART-SALDO                 
391100                END-IF                                                    
391200                                                                          
391300                SUBTRACT WS-KVLS      FROM CLAG-KVLS                      
391400****** FÖLJANDE SKALL LOGGA SALDOT PÅ DATABAS WDL9. *******               
391500                IF WS-KVLS = 0                                            
391600                  IF WS-KVEFRS > 0                                        
391700                    CONTINUE                                              
391800                   ELSE                                                   
391900                    MOVE '-'          TO LOGG-IDTECKEN-KVLS               
392000                    MOVE 0            TO LOGG-KVART-SALDO                 
392100                  END-IF                                                  
392200                ELSE                                                      
392300                  MOVE '-'            TO LOGG-IDTECKEN-KVLS               
392400                  MOVE WS-KVLS        TO LOGG-KVART-SALDO                 
392500                END-IF                                                    
392600                PERFORM S04D-CREATE-SALDOLOGG                             
392700                                                                          
392800                                                                          
392900                IF ORAD-IDKAMPRF      > 0                                 
393000                   AND                                                    
393100                   WS-KART-KVRESS-ART <= 0                                
393200                   CONTINUE                                               
393300                ELSE                                                      
393400                   SUBTRACT WS-KVRESS FROM CLAG-KVRESS                    
393500                END-IF                                                    
393600              END-IF                                                      
393700                                                                          
393800              IF WS-KVROS > 0                                             
393900                 PERFORM S04B-EV-LARM-2191-MID                            
394000                 ADD WS-KVROS       TO CLAG-KVROS                         
394100              END-IF                                                      
394200              PERFORM IMS-22-REPL-WDK611                                  
394300           END-IF                                                         
394400        END-IF                                                            
394500                                                                          
394600        PERFORM S04B-UPPDAT-SKROT-WDK6                                    
394700     END-IF                                                               
394800     .                                                                    
394900     EJECT                                                                
395000                                                                          
395100 S04A-UPPDAT-KAMPANJ-WDM2 SECTION.                                        
395200     MOVE 'S04A-UPPDAT-KAMPANJ-WDM2' TO WS-CURRENT-SECTION                
395300                                                                          
395400     IF WS-KVRESS > 0     AND                                             
395500        ORAD-IDKAMPRF > 0                                                 
395600                                                                          
395700        MOVE ORAD-IDKAMPRF TO W-KAMP-IDKAMPRF                             
395800        MOVE ORAD-IDDC     TO W-KAMP-IDDC                                 
395900        MOVE ORAD-IDARTNR  TO W-KART-IDARTNR                              
396000                                                                          
396100        PERFORM IMS-GHU-WDM211                                            
396200        IF SEGMENT-FINNS                                                  
396300           IF WS-KVAVBART = 0 AND ORAD-FLRESTN = NEJ                      
396400              SUBTRACT ORAD-KVBEART-Q FROM KART-KVBEART-KUND              
396500           ELSE                                                           
396600              MOVE KART-KVRESS-ART      TO WS-KART-KVRESS-ART             
396700              SUBTRACT WS-KVRESS        FROM KART-KVRESS-ART              
396800           END-IF                                                         
396900           PERFORM IMS-REPL-WDM211                                        
397000        END-IF                                                            
397100                                                                          
397200        MOVE ORAD-IDKAMPRF     TO W-KAMP-IDKAMPRF                         
397300        MOVE ORAD-IDDC         TO W-KAMP-IDDC                             
397400        MOVE ORAD-IDARTNR      TO W-KART-IDARTNR                          
397500        MOVE ORAD-IDDISTR      TO W-KMRK-IDDISTR-FOM                      
397600                                  W-KMRK-IDDISTR-TOM                      
397700        MOVE ORAD-IDKUNDNR     TO W-KMRK-IDKUNDNR-FOM                     
397800                                  W-KMRK-IDKUNDNR-TOM                     
397900                                                                          
398000        PERFORM S20-FINN-INTERVALL                                        
398100                                                                          
398200        PERFORM IMS-GHU-WDM221                                            
398300        IF SEGMENT-FINNS                                                  
398400           IF WS-KVAVBART = 0 AND ORAD-FLRESTN = NEJ                      
398500              SUBTRACT ORAD-KVBEART-Q FROM KMRK-KVBEART-KUND              
398600              PERFORM IMS-REPL-WDM221                                     
398700           END-IF                                                         
398800        END-IF                                                            
398900     END-IF                                                               
399000     .                                                                    
399100                                                                          
399200 S04B-UPPDAT-SKROT-WDK6     SECTION.                                      
399300                                                                          
399400     MOVE 'STA S04B-UPPDAT-SKROT'   TO WS-CURRENT-SECTION                 
399500     MOVE W-ODEL-IDDISTR TO DIST18-IDDISTR                                
399600                                                                          
399700     IF DIST18-SKROT AND DCS-CDC                                          
399800        IF WS-KVAVBART > 0                                                
399900           MOVE ORAD-IDARTNR   TO W-IDARTNR                               
400000           PERFORM IMS-21-GHU-WDK611                                      
400100           MOVE NEJ            TO CLAG-FLSKROT-BEORD                      
400200           PERFORM IMS-22-REPL-WDK611                                     
400300                                                                          
400400           PERFORM IMS-GHNP-WDK627                                        
400500           IF SEGMENT-FINNS                                               
400600              MOVE WS-KVAVBART TO SKROT-KVSKROT                           
400700              MOVE WS-DATUM-Y2K TO SKROT-DASKROT                          
400800              PERFORM IMS-REPL-WDK627                                     
400900           ELSE                                                           
401000              MOVE WS-KVAVBART TO SKROT-KVSKROT                           
401100              MOVE WS-DATUM-Y2K TO SKROT-DASKROT                          
401200              MOVE ZERO        TO SKROT-TISKROT-BEORD                     
401300              PERFORM IMS-ISRT-WDK627                                     
401400           END-IF                                                         
401500        END-IF                                                            
401600     END-IF                                                               
401700     .                                                                    
401800     EJECT                                                                
401900 S04B-EV-LARM-2191-MID SECTION.                                           
402000     MOVE 'S04B-EV-LARM-2191-MID        ' TO WS-CURRENT-SECTION           
402100                                                                          
402200** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
402300                                                                          
402400     IF OUTPUT-MSG-IX < MAX-ANT-OUTPUT-MSG AND                            
402500        CLAG-KVROS = 0 AND                                                
402600        CLAG-KVAKS-CDC = 0                                                
402700                                                                          
402800        COMPUTE ALT-LL = LENGTH OF ALT-MID-W2I19101 + 17                  
402900        MOVE +1               TO ALT-MID-KDCLAGER                         
403000        MOVE ORAD-IDARTNR     TO ALT-MID-IDARTNR                          
403100        MOVE ZERO             TO ALT-MID-TISENBEK-DAG                     
403200                                 ALT-MID-TISENBEK-KL                      
403300        MOVE SPACE            TO ALT-MID-IDKR                             
403400        MOVE AREG-IDANSK      TO ALT-MID-IDANSK                           
403500        MOVE 210              TO ALT-MID-KDLARM                           
403600        MOVE W-ODEL-IDDISTR   TO WS-IDDISTR-NUM4                          
403700        MOVE WS-IDDISTR-NUM4  TO ALT-MID-IDDISTR                          
403800        MOVE W-ODEL-IDKUNDNR  TO WS-IDKUNDNR-NUM6                         
403900        MOVE WS-IDKUNDNR-NUM6 TO ALT-MID-IDKUNDNR                         
404000        MOVE W-ODEL-IDKUNDRF  TO ALT-MID-IDKUNDRF                         
404100        MOVE 'J'              TO ALT-MID-FLNYLARM                         
404200        MOVE WC-CDC-SE        TO ALT-MID-IDDC                             
404300        MOVE SPACE            TO ALT-MID-IDLEVNR                          
404400        MOVE '1'              TO ALT-SPRAK                                
404500                                                                          
404600        PERFORM IMS-27-PURG-ALT-MSG                                       
404700        ADD 1                 TO OUTPUT-MSG-IX                            
404800     END-IF                                                               
404900     .                                                                    
405000 S04B-EV-LARM-2191-MID-CN-US SECTION.                                     
405100     MOVE 'S04B-EV-LARM-2191-MID-CN-US     ' TO WS-CURRENT-SECTION        
405200                                                                          
405300** ANSKAFFNINGEN LARMAS FÖRSTA GÅNGEN EN ARTIKEL RESTNOTERAS              
405400                                                                          
405500     IF OUTPUT-MSG-IX < MAX-ANT-OUTPUT-MSG AND                            
405600        SLAG-KVROS-DAG = 0 AND SLAG-KVROS-BULK = 0 AND                    
405700        SLAG-KVAKS-PAV = 0 AND SLAG-KVAKS-SDC = 0                         
405800                                                                          
405900        COMPUTE ALT-LL = LENGTH OF ALT-MID-W2I19101 + 17                  
406000        MOVE +1               TO ALT-MID-KDCLAGER                         
406100        MOVE ORAD-IDARTNR     TO ALT-MID-IDARTNR                          
406200        MOVE ZERO             TO ALT-MID-TISENBEK-DAG                     
406300                                 ALT-MID-TISENBEK-KL                      
406400        MOVE SPACE            TO ALT-MID-IDKR                             
406500        IF K722-FINNS AND XLAG-IDANSK > 0                                 
406600           MOVE XLAG-IDANSK   TO ALT-MID-IDANSK                           
406700        ELSE                                                              
406800           MOVE CLAG-IDANSK   TO ALT-MID-IDANSK                           
406900        END-IF                                                            
407000        MOVE 210              TO ALT-MID-KDLARM                           
407100        MOVE W-ODEL-IDDISTR   TO WS-IDDISTR-NUM4                          
407200        MOVE WS-IDDISTR-NUM4  TO ALT-MID-IDDISTR                          
407300        MOVE W-ODEL-IDKUNDNR  TO WS-IDKUNDNR-NUM6                         
407400        MOVE WS-IDKUNDNR-NUM6 TO ALT-MID-IDKUNDNR                         
407500        MOVE W-ODEL-IDKUNDRF  TO ALT-MID-IDKUNDRF                         
407600        MOVE 'J'              TO ALT-MID-FLNYLARM                         
407700        MOVE SLAG-IDDC        TO ALT-MID-IDDC                             
407800        MOVE SLAG-IDLEVNR     TO ALT-MID-IDLEVNR                          
407900        MOVE '2'              TO ALT-SPRAK                                
408000                                                                          
408100        PERFORM IMS-27-PURG-ALT-MSG                                       
408200        ADD 1                 TO OUTPUT-MSG-IX                            
408300                                                                          
408400     END-IF                                                               
408500     .                                                                    
408600                                                                          
408700 S04D-CREATE-SALDOLOGG SECTION.                                           
408800                                                                          
408900     MOVE 'STA S04D-CREATE-SALDOL'  TO WS-CURRENT-SECTION                 
409000     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
409100                                                                          
409200     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
409300     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
409400                                   - WS-AAAAMMDD                          
409500     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
409600     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
409700                                   - WS-TTMMSSTH                          
409800     MOVE 9                        TO LOGG-IDSEKVNR                       
409900     MOVE 11                       TO LOGG-IDDC                           
410000     MOVE 'OUTB'                   TO LOGG-IDHUVTYP                       
410100     MOVE 'PRT'                    TO LOGG-IDSUBTYP                       
410200     MOVE 'WL013400'               TO LOGG-IDPGM                          
410300     MOVE 'WL0134T'                TO LOGG-IDTRANS                        
410400     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
410500     MOVE SPACE                    TO LOGG-REF                            
410600     MOVE ORAD-IDDISTR             TO LOGG-IDDISTR                        
410700     MOVE ORAD-IDKUNDNR            TO LOGG-IDKUNDNR                       
410800     MOVE ORAD-IDORDNR7            TO LOGG-IDORDNR5                       
410900     MOVE 3410-IDPRODNR-KEY        TO LOGG-IDPRODNR                       
411000     MOVE 3410-IDPLKLST-KEY        TO LOGG-IDPLKLST                       
411100     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS                 
411200                                                                          
411300     COMPUTE LOGG-KVAKS = CLAG-KVAKS-CDC                                  
411400                        + CLAG-KVAKS-T                                    
411500                                                                          
411600     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
411700     MOVE CLAG-KVAKS-PAV           TO LOGG-KVAKS-PAV                      
411800     MOVE CLAG-KVEFRS              TO LOGG-KVEFRS                         
411900     MOVE CLAG-KVLS                TO LOGG-KVLS                           
412000     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
412100                                                                          
412200     PERFORM IMS-ISRT-WDL901                                              
412300     IF SEGMENT-FINNS-REDAN                                               
412400       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
412500         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
412600         PERFORM IMS-ISRT-WDL901                                          
412700       END-PERFORM                                                        
412800     END-IF                                                               
412900     .                                                                    
413000     EJECT                                                                
413100 S05-SKAPA-PLOCKSATS-PU SECTION.                                          
413200     MOVE 'S05-SKAPA-PLOCKSATS-PU'  TO WS-CURRENT-SECTION                 
413300                                                                          
413400     MOVE 3410-IDPRODNR(ODEL-IX) TO W-4007-PU-IDPRODNR                    
413500     MOVE 3410-IDPLKLST(ODEL-IX) TO W-4007-PU-IDPLKLST                    
413600                                                                          
413700     IF W-4007-PU-IDPRODNR NOT = 3410-IDPRODNR-KEY OR                     
413800        W-4007-PU-IDPLKLST NOT = 3410-IDPLKLST-KEY                        
413900*****************************************************************         
414000*                                                                         
414100*       OM VI HAMNAR HÄR KOMMER VI ATT LÄGGA UPP RÖTTER PÅ                
414200*       HÄNDELSEBASERNA SOM INTE HAR SAMMA NYCKEL SOM ANVÄNDS             
414300*       FÖR LÄSNING I SUBMODULERNA -20, -30 OCH -40.                      
414400*       FÖR ATT UNDVIKA BEKYMMER SPARAR VI NYCKELN GENOM EN               
414500*       OVERRIDE PÅ 3410-...-KEY VÄRDENA SOM SEDAN ANVÄNDS                
414600*       VID ANROP AV MODULERNA                                            
414700*                                                                         
414800*****************************************************************         
414900                                                                          
415000        MOVE W-4007-PU-IDPRODNR  TO 3410-IDPRODNR-KEY                     
415100        MOVE W-4007-PU-IDPLKLST  TO 3410-IDPLKLST-KEY                     
415200                                                                          
415300     END-IF                                                               
415400                                                                          
415500     MOVE W-4007-PU-IDHTYP-X     TO DLI-IO-WDGX4007                       
415600                                                                          
415700     PERFORM IMS-38-ISRT-WDGX4007                                         
415800     PERFORM IMS-39-ISRT-WDGX4008                                         
415900                                                                          
416000     MOVE JA TO PLOCKSATS-PU-SW                                           
416100     .                                                                    
416200 S06Z-KOLLA-AENDRA-LAGOMR SECTION.                                        
416300                                                                          
416400     MOVE 'STA S06Z-KOLLA'  TO WS-CURRENT-SECTION                         
416500     MOVE NEJ                      TO SW-AENDRA-LAGOMR                    
416600     IF  CLAG-ADLAGOMR = ORAD-ADLAGOMR                                    
416700     AND CLAG-ADGANG   = ORAD-ADGANG                                      
416800     AND CLAG-ADPLATS  = ORAD-ADPLATS                                     
416900*......INGEN FÖRÄNDRING AV LAGOMR                                         
417000       CONTINUE                                                           
417100     ELSE                                                                 
417200                                                                          
417300       PERFORM S06ZA-ANROP-W411LAST                                       
417400                                                                          
417500       IF  LAST-ADLAGOMR-UT = +000                                        
417600       AND LAST-KVANTAL-UT  = +0000000                                    
417700       AND LAST-KVBEART-UT  = +0000000                                    
417800                                                                          
417900         PERFORM S06ZB-ANROP-W413ADRS                                     
418000                                                                          
418100         IF (ADRS-ADLAGOMR-UT = +000                                      
418200         AND ADRS-ADPLATS-UT  = +00000)                                   
418300           CONTINUE                                                       
418400         ELSE                                                             
418500           IF (ADRS-ADLAGOMR-UT = ORAD-ADLAGOMR                           
418600           AND ADRS-ADPLATS-UT  = ORAD-ADPLATS)                           
418700*............orderrad gäller.                                             
418800             CONTINUE                                                     
418900           ELSE                                                           
419000*............LAGOMR I CLAG- GÄLLER!                                       
419100               MOVE JA             TO SW-AENDRA-LAGOMR                    
419200           END-IF                                                         
419300*..........LAGOMR JUSTERAD AV W413ADRS. ÄNDRA INTE.                       
419400         END-IF                                                           
419500       ELSE                                                               
419600         CONTINUE                                                         
419700*........LAGOMR JUSTERAD AV W411LAST. ÄNDRA INTE.                         
419800       END-IF                                                             
419900     END-IF                                                               
420000     .                                                                    
420100     SKIP2                                                                
420200 S06ZA-ANROP-W411LAST SECTION.                                            
420300                                                                          
420400     MOVE 'STA S06ZA-ANROP'  TO WS-CURRENT-SECTION                        
420500     MOVE ORAD-ADLAGOMR            TO LAST-ADLAGOMR                       
420600     MOVE OHUV-FLFORBI             TO LAST-FLFORBI                        
420700     MOVE OHUV-FLOVRLEV            TO LAST-FLOVRLEV                       
420800     MOVE OHUV-FLORDSPE            TO LAST-FLORDSPE                       
420900     MOVE ORAD-IDLEVNR             TO LAST-IDLEVNR                        
421000     MOVE ORAD-IDDC                TO LAST-IDDC                           
421100     MOVE ARB-KDFDKRAV             TO LAST-KDFDKRAV                       
421200     MOVE ORAD-KVPREAVB            TO LAST-KVPREAVB                       
421300     MOVE CLAG-KVQPACK-3           TO LAST-KVQPACK-3                      
421400     MOVE CLAG-KVQPACK-4           TO LAST-KVQPACK-4                      
421500                                                                          
421600     CALL W411LAST              USING LAST-W411LAST                       
421700     .                                                                    
421800     SKIP2                                                                
421900 S06ZB-ANROP-W413ADRS SECTION.                                            
422000                                                                          
422100     MOVE 'STA S06ZB-ANROP'  TO WS-CURRENT-SECTION                        
422200     MOVE ORAD-ADLAGOMR            TO ADRS-ADLAGOMR-IN                    
422300     MOVE ORAD-ADPLATS             TO ADRS-ADPLATS-IN                     
422400     MOVE OHUV-BEVARREF            TO ADRS-BEVARREF-IN                    
422500     MOVE OHUV-FLFORBI             TO ADRS-FLFORBI-IN                     
422600     MOVE OHUV-IDDISTR             TO ADRS-IDDISTR-IN                     
422700     MOVE 1                        TO ADRS-KDCALL-IN                      
422800     MOVE ORAD-IDDC                TO ADRS-IDDC-IN                        
422900     MOVE OHUV-KDORDKL             TO ADRS-KDORDKL-IN                     
423000     MOVE ORAD-KVBEART-Q           TO ADRS-KVBEART-Q-IN                   
423100     MOVE ORAD-VLARTNTO            TO ADRS-VLARTNTO-IN                    
423200                                                                          
423300     CALL W413ADRS              USING ADRS-W413ADRS                       
423400     .                                                                    
423500     EJECT                                                                
423600                                                                          
423700 S07-BORTTAG-ORDERRAD SECTION.                                            
423800     MOVE 'S07-BORTTAG-ORDERRAD'    TO WS-CURRENT-SECTION                 
423900                                                                          
424000     IF ARTIKEL-OK                                                        
424100        PERFORM IMS-28-DLET-WDQ401                                        
424200     END-IF                                                               
424300     .                                                                    
424400                                                                          
424500 S08-SKAPA-RYETRANS SECTION.                                              
424600     MOVE 'S08-SKAPA-RYETRANS  '    TO WS-CURRENT-SECTION                 
424700                                                                          
424800     PERFORM S08A-SKAPA-RYEPOST                                           
424900     PERFORM S08B-SKAPA-SORTPOST                                          
425000                                                                          
425100     MOVE W-RYEPOST      TO LOGGPOST                                      
425200     MOVE W-SORTPOST     TO SORTPOST                                      
425300     MOVE 3410-TIAAMMDD  TO TIAAMMDD                                      
425400     ACCEPT TIKLOCK FROM TIME                                             
425500     MOVE 1              TO IDLOGLOP                                      
425600                                                                          
425700     PERFORM IMS-29-ISRT-WDG601                                           
425800                                                                          
425900     PERFORM UNTIL SEGMENT-FINNS                                          
426000        IF IDLOGLOP = 9                                                   
426100           ACCEPT TIKLOCK FROM TIME                                       
426200           MOVE 0 TO IDLOGLOP                                             
426300        END-IF                                                            
426400        ADD 1 TO IDLOGLOP                                                 
426500        PERFORM IMS-29-ISRT-WDG601                                        
426600     END-PERFORM                                                          
426700     .                                                                    
426800                                                                          
426900 S08A-SKAPA-RYEPOST SECTION.                                              
427000     MOVE 'S08A-SKAPA-RYEPOST  '    TO WS-CURRENT-SECTION                 
427100                                                                          
427200     MOVE 'RYE'              TO W-RYE-IDPTYP                              
427300     MOVE ORAD-BERADREF      TO W-RYE-BERADREF                            
427400     MOVE ORAD-BEVOLREF      TO W-RYE-BEVOLREF                            
427500                                                                          
427600     IF ORAD-IDLEVNR NOT = SPACE                                          
427700        MOVE JA              TO W-RYE-FLDIRLEV                            
427800     ELSE                                                                 
427900        MOVE NEJ             TO W-RYE-FLDIRLEV                            
428000     END-IF                                                               
428100                                                                          
428200     MOVE OHUV-FLLSBOK       TO W-RYE-FLLSBOK                             
428300     MOVE OHUV-FLORDSPE      TO W-RYE-FLORDSPE                            
428400     MOVE ORAD-FLTILLK       TO W-RYE-FLTILLK                             
428500     MOVE ORAD-IDARTNR       TO W-RYE-IDARTNR                             
428600     MOVE ORAD-IDKUNDRF      TO W-RYE-IDKUNDRF                            
428700     MOVE ORAD-IDKUNDRF-RO TO W-RYE-IDKUNDRF-RO                           
428800     MOVE ORAD-IDDC          TO W-RYE-IDDC                                
428900     MOVE ORAD-KDDSP         TO W-RYE-KDDSP                               
429000     MOVE OHUV-KDFAKTYP      TO W-RYE-KDFAKTYP                            
429100     MOVE ORAD-KDKVBRYT      TO W-RYE-KDKVBRYT                            
429200     MOVE WS-KDORDBEK        TO W-RYE-KDORDBEK                            
429300     MOVE ORAD-KDORDING      TO W-RYE-KDORDING                            
429400     MOVE AREG-KDPRODSL      TO W-RYE-KDPRODSL                            
429500     MOVE ORAD-KDVRINFO      TO W-RYE-KDVRINFO                            
429600                                                                          
429700     IF ORAD-KDTPOTYP NOT = 1                                             
429800        MOVE 0               TO W-RYE-KDVRTPO                             
429900     ELSE                                                                 
430000        IF ORAD-KDTPOTYP = 1                                              
430100           IF ORAD-IDSYSTEM = 'VR'                                        
430200              MOVE 1         TO W-RYE-KDVRTPO                             
430300           ELSE                                                           
430400              MOVE 2         TO W-RYE-KDVRTPO                             
430500           END-IF                                                         
430600        END-IF                                                            
430700     END-IF                                                               
430800                                                                          
430900     MOVE WS-KVLS            TO W-RYE-KVAVBART                            
431000     MOVE ORAD-KVBEART-Q     TO W-RYE-KVBEART-Q                           
431100                                                                          
431200     IF WS-KDORDBEK = 80                                                  
431300        MOVE ORAD-KVBEART-Q  TO W-RYE-KVRO                                
431400     ELSE                                                                 
431500       IF WS-KDORDBEK = 92                                                
431600          COMPUTE W-RYE-KVRO = ORAD-KVBEART-Q - WS-KVAVBART               
431700       ELSE                                                               
431800          MOVE WS-KVROS        TO W-RYE-KVRO                              
431900       END-IF                                                             
432000     END-IF                                                               
432100                                                                          
432200     MOVE ORAD-REKSIFFR      TO W-RYE-REKSIFFR                            
432300     MOVE AREG-TIDISPIN      TO W-RYE-TIDISPIN                            
432400     MOVE ORAD-TIREGDAT      TO W-RYE-TIORDREG                            
432500     MOVE ORAD-TIRODAT       TO W-RYE-TIRODAT                             
432600     .                                                                    
432700                                                                          
432800 S08B-SKAPA-SORTPOST SECTION.                                             
432900     MOVE 'S08B-SKAPA-SORTPOST '    TO WS-CURRENT-SECTION                 
433000                                                                          
433100     MOVE ORAD-IDDISTR        TO W-RYES-IDDISTR                           
433200     MOVE ORAD-IDKUNDNR       TO W-RYES-IDKUNDNR                          
433300     IF  OHUV-FLVORKO = JA                                                
433400     OR  OHUV-FLVORKO = YES                                               
433500         MOVE JA              TO W-RYES-FLVORKO                           
433600     ELSE                                                                 
433700         MOVE OHUV-FLVORKO    TO W-RYES-FLVORKO                           
433800     END-IF                                                               
433900     MOVE OHUV-FLFORBI        TO W-RYES-FLFORBI                           
434000     MOVE OHUV-FLOVRLEV       TO W-RYES-FLOVRLEV                          
434100     MOVE AREG-KDERS          TO W-RYES-KDERS                             
434200     MOVE ARB-KDFRAKT         TO W-RYES-KDFRAKT                           
434300     MOVE ORAD-KDORDKL        TO W-RYES-KDORDKL                           
434400     MOVE ORAD-KDTPOTYP       TO W-RYES-KDTPOTYP                          
434500     MOVE ORAD-KVBEART        TO W-RYES-KVBEART                           
434600                                                                          
434700     MOVE ORAD-KVSLATT        TO W-RYES-KVSLATT                           
434800     MOVE AREG-KVQPACK-1      TO W-RYES-KVQPACK-1                         
434900     .                                                                    
435000 S09-SKAPA-PLOCKSATS-ETIK SECTION.                                        
435100     MOVE 'S09-SKAPA-PLOCKSATS-ETIK' TO WS-CURRENT-SECTION                
435200                                                                          
435300     MOVE 3410-IDPRODNR(ODEL-IX) TO W-4003-ETIK-IDPRODNR                  
435400     MOVE 3410-IDPLKLST(ODEL-IX) TO W-4003-ETIK-IDPLKLST                  
435500     MOVE W-4003-ETIK-IDHTYP-X   TO DLI-IO-WDGX4003                       
435600     PERFORM IMS-44-ISRT-WDGX4003                                         
435700     PERFORM IMS-45-ISRT-WDGX4004                                         
435800                                                                          
435900     MOVE JA TO PLOCKSATS-ETIK-SW                                         
436000     .                                                                    
436100                                                                          
436200 S10-UPPDAT-BEFINTLIG-RESTORDER SECTION.                                  
436300     MOVE 'S10-UPPDAT-BEFINTLIG-RESTORDER' TO WS-CURRENT-SECTION          
436400                                                                          
436500     MOVE ORAD-IDDISTR               TO W-A501KY-IDDISTR                  
436600     MOVE ORAD-IDKUNDNR              TO W-A501KY-IDKUNDNR                 
436700     MOVE ORAD-IDKUNDRF-RO (3:5)     TO W-A501KY-IDKUNDRF                 
436800     MOVE ORAD-IDARTNR               TO W-A501KY-IDARTNR                  
436900     MOVE ORAD-IDLOPNR-RO            TO W-A501KY-IDLOPNR                  
437000                                                                          
437100     PERFORM IMS-30-GHU-WDA501                                            
437200     IF SEGMENT-SAKNAS                                                    
437300       PERFORM S03-NYUPPLAGG-RESTORDER                                    
437400     ELSE                                                                 
437500                                                                          
437600        MOVE WS-KVROS                TO RAD-KVART                         
437700                                                                          
437800     MOVE '2'                        TO RAD-KDSTARAD                      
437900     MOVE 0                          TO RAD-TIRES                         
438000     MOVE ORAD-IDDC-RO               TO RAD-IDDC                          
438100                                        RAD-IDDC-RO                       
438200                                                                          
438300     IF RAD-DARODAT = 0                                                   
438400        MOVE 3410-TIAAMMDD        TO RAD-DARODAT                          
438500        MOVE 20                   TO RAD-DARODAT (1:2)                    
438600     END-IF                                                               
438700     PERFORM S36-ANDRA-WDC711                                             
438800                                                                          
438900     PERFORM IMS-30-REPL-WDA501                                           
439000     END-IF                                                               
439100     .                                                                    
439200 S11-UPPDAT-VOR SECTION.                                                  
439300                                                                          
439400     MOVE 'S11-UPPDAT-VOR           ' TO WS-CURRENT-SECTION               
439500     MOVE ORAD-IDDISTR         TO 4542-IDDISTR                            
439600     MOVE ORAD-IDDC            TO 4542-IDDC                               
439700     MOVE AREG-IDANSK          TO 4542-IDANSK                             
439800     MOVE ORAD-IDARTNR         TO 4542-IDARTNR                            
439900     MOVE 1                    TO 4542-IDLOPNR                            
440000     MOVE ORAD-IDORDER         TO 4542-IDORDER                            
440100     MOVE ORAD-BERADREF        TO 4542-BERADREF                           
440200     MOVE ORAD-IDKUNDNR        TO 4542-IDKUNDNR                           
440300     MOVE ORAD-IDKUNDRF        TO 4542-IDKUNDRF                           
440400     MOVE SPACE                TO 4542-IDUSER                             
440500     MOVE WS-KDORDBEK          TO 4542-KDORDBEK                           
440600     MOVE ORAD-KDPRTYP         TO 4542-KDPRTYP                            
440700     MOVE ZERO                 TO 4542-KDVORATG                           
440800     MOVE ORAD-KVBEART         TO 4542-KVBEART                            
440900     MOVE ORAD-KVBEART-Q       TO 4542-KVBEART-Q                          
441000     MOVE WS-KVAVBART          TO 4542-KVPREAVB                           
441100     MOVE ORAD-PRARTNTO        TO 4542-PRARTNTO                           
441200     MOVE ORAD-DEAL-PR-LINE    TO 4542-DEAL-PR-LINE                       
441300     MOVE SPACE                TO 4542-TEVORMRK                           
441400     MOVE WS-DATUM-LOK         TO 4542-TIREGDAT                           
441500     MOVE WS-TIHHMMSS-LOK      TO 4542-TIREGTID                           
441600     MOVE ZERO                 TO 4542-TIUPPDAT                           
441700                                  4542-TIUPPTID                           
441800     MOVE ORAD-IDLEVNR         TO 4542-IDLEVNR                            
441900                                                                          
442000     PERFORM IMS-53-ISRT-4541-WL454111                                    
442100                                                                          
442200     PERFORM UNTIL SEGMENT-FINNS                                          
442300        ADD 1 TO 4542-IDLOPNR                                             
442400        PERFORM IMS-53-ISRT-4541-WL454111                                 
442500     END-PERFORM                                                          
442600                                                                          
442700     MOVE ORAD-IDARTNR          TO W-IDARTNR                              
442800     MOVE ORAD-IDDC             TO W-IDDC                                 
442900     PERFORM IMS-40-GU-WDK711                                             
443000     IF  DCS-NDC-CN                                                       
443100     OR (DCS-NDC-NA AND DCS-USA)                                          
443200       IF SLAG-IDDC-REF = SPACE                                           
443300         MOVE ORAD-IDDISTR       TO S27-IDDISTR                           
443400         MOVE ORAD-IDKUNDNR      TO S27-IDKUNDNR                          
443500         MOVE ORAD-IDARTNR       TO S27-IDARTNR                           
443600         IF K722-FINNS AND XLAG-IDANSK > 0                                
443700            MOVE XLAG-IDANSK     TO S27-IDANSK                            
443800         ELSE                                                             
443900            MOVE CLAG-IDANSK     TO S27-IDANSK                            
444000         END-IF                                                           
444100         PERFORM S11DC-STARTA-W2T191X                                     
444200       END-IF                                                             
444300     END-IF                                                               
444400     .                                                                    
444500     EJECT                                                                
444600 S11D-UPDATE-VORKONY          SECTION.                                    
444700     MOVE 'S11D-UPDATE-VORKONY      ' TO WS-CURRENT-SECTION               
444800                                                                          
444900     MOVE ORAD-IDARTNR        TO S28-IDARTNR                              
445000     PERFORM S11DA-BESTAM-LENVR-ANSK                                      
445100                                                                          
445200*----------------------------------RADEN SKALL FINNAS PÅ VORKÖ            
445300                                                                          
445400     PERFORM S11DB-SOK-RAD-VORKO                                          
445500                                                                          
445600     IF  TRAFF-VORKO                                                      
445700         COMPUTE VOR-KVPREAVB  = VOR-KVPREAVB                             
445800                               - ORAD-KVBEART-Q                           
445900                               + WS-KVAVBART                              
446000         END-COMPUTE                                                      
446100         COMPUTE VOR-KVBEART-Q = VOR-KVBEART-Q                            
446200                               - ORAD-KVBEART-Q                           
446300                               + WS-KVAVBART                              
446400         END-COMPUTE                                                      
446500         IF  VOR-KVPREAVB = 0                                             
446600             MOVE '7'              TO VOR-KDVORATG                        
446700             MOVE WS-KDORDBEK      TO VOR-KDORDBEK                        
446800             IF VOR-TIKLAR = ZERO                                         
446900                MOVE 3410-TIAAMMDD TO VOR-TIKLAR                          
447000                COMPUTE VOR-TIKLATID  = WS-VOR-TID-BRIST                  
447100                                      / 100                               
447200             END-COMPUTE                                                  
447300             END-IF                                                       
447400         END-IF                                                           
447500         PERFORM IMS-31-REPL-SEQB-WDA601                                  
447600                                                                          
447700         MOVE 3410-TIAAMMDD     TO VOR-TIREGDAT-AVV                       
447800         ADD +1                 TO WS-VOR-TID-BRIST                       
447900         MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                       
448000         SUBTRACT 3410-TIAAMMDD   FROM 9999999                            
448100                                  GIVING VOR-TIREGDAT-AVV9                
448200         SUBTRACT WS-VOR-TID-BRIST FROM 999999999                         
448300                                   GIVING VOR-TIREGTID-AVV9               
448400         MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                       
448500         MOVE 0                 TO VOR-TIREGDAT-LEV                       
448600         MOVE 0                 TO VOR-TIREGTID-LEV                       
448700         SUBTRACT WS-KVAVBART                                             
448800                              FROM ORAD-KVBEART-Q                         
448900                              GIVING VOR-KVBEART                          
449000                                     VOR-KVBEART-Q                        
449100         MOVE 0                   TO VOR-KVPREAVB                         
449200         MOVE 3410-IDDC(ODEL-IX)  TO VOR-IDDC                             
449300         MOVE SPACE               TO VOR-IDUSER                           
449400         MOVE WS-KDORDBEK         TO VOR-KDORDBEK                         
449500         MOVE '0'                 TO VOR-KDVORATG                         
449600         MOVE 0                   TO VOR-TIKLAR                           
449700         MOVE 0                   TO VOR-TIKLATID                         
449800                                                                          
449900         PERFORM IMS-32-ISRT-WDA601                                       
450000         PERFORM UNTIL ISRT-OK                                            
450100            ADD +1                TO WS-VOR-TID-BRIST                     
450200            MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                     
450300            SUBTRACT WS-VOR-TID-BRIST FROM 999999999                      
450400                                  GIVING VOR-TIREGTID-AVV9                
450500            PERFORM IMS-32-ISRT-WDA601                                    
450600         END-PERFORM                                                      
450700     ELSE                                                                 
450800*--------------------------------------- EJ TRÄFF, FEJKA ORG. RAD         
450900*                                        BORDE NOG INTE FÖREKOMMA         
451000       MOVE ORAD-IDDISTR        TO VOR-IDDISTR                            
451100       MOVE ORAD-IDKUNDNR       TO VOR-IDKUNDNR                           
451200       MOVE ORAD-IDKUNDRF       TO VOR-IDKUNDRF                           
451300       MOVE ORAD-TIREGDAT       TO VOR-TIREGDAT-URSP                      
451400       MOVE ORAD-IDARTNR        TO VOR-IDARTNR                            
451500       ADD +1                   TO WS-VOR-TID-BRIST                       
451600       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-URSP                      
451700       MOVE 0                   TO VOR-TIREGDAT-AVV                       
451800       MOVE 0                   TO VOR-TIREGTID-AVV                       
451900       SUBTRACT 0            FROM 9999999                                 
452000                              GIVING VOR-TIREGDAT-AVV9                    
452100       SUBTRACT 0            FROM 999999999                               
452200                              GIVING VOR-TIREGTID-AVV9                    
452300       MOVE ORAD-IDKUNDRF       TO VOR-IDKUNDRF-LEV                       
452400       MOVE ORAD-TIREGDAT       TO VOR-TIREGDAT-LEV                       
452500       MOVE WS-VOR-TID-BRIST    TO VOR-TIREGTID-LEV                       
452600       MOVE S28-IDANSK          TO VOR-IDANSK                             
452700                                                                          
452800       MOVE VOR-IDDISTR         TO W-IDDISTR-P4                           
452900       PERFORM IMS-GU-WDP4A1                                              
453000       IF SEGMENT-FINNS                                                   
453100          MOVE SEQA-IDROLL      TO VOR-IDROLL                             
453200       ELSE                                                               
453300          MOVE DEF-IDROLL       TO VOR-IDROLL                             
453400       END-IF                                                             
453500                                                                          
453600       MOVE S28-IDLEVNR         TO VOR-IDLEVNR                            
453700       MOVE ORAD-BERADREF       TO VOR-BERADREF                           
453800       SUBTRACT WS-KVAVBART                                               
453900                            FROM   ORAD-KVBEART-Q                         
454000                            GIVING VOR-KVBEART-URSP                       
454100                                   VOR-KVBEART                            
454200       MOVE 0                   TO VOR-KVPREAVB                           
454300                                   VOR-KVBEART-Q                          
454400       MOVE 3410-IDDC(ODEL-IX)  TO VOR-IDDC                               
454500       MOVE SPACE               TO VOR-IDUSER                             
454600       MOVE WS-KDORDBEK         TO VOR-KDORDBEK                           
454700       MOVE ORAD-KDPRTYP        TO VOR-KDPRTYP                            
454800       MOVE '7'                 TO VOR-KDVORATG                           
454900       MOVE ORAD-PRARTNTO       TO VOR-PRARTNTO                           
455000       MOVE '  '                TO VOR-TEVORMRK                           
455100*      MOVE '  '                TO VOR-TEVORMRK-SC                        
455200       MOVE 0                   TO VOR-TIUPPDAT                           
455300       MOVE 0                   TO VOR-TIUPPTID                           
455400       IF VOR-TIKLAR = ZERO                                               
455500          MOVE 3410-TIAAMMDD    TO VOR-TIKLAR                             
455600          COMPUTE VOR-TIKLATID     = WS-VOR-TID-BRIST                     
455700                                   / 100                                  
455800       END-COMPUTE                                                        
455900       END-IF                                                             
456000       MOVE ORAD-DEAL-PR-LINE   TO VOR-DEAL-PR-LINE                       
456100       MOVE NEJ                 TO VOR-FLVORFK                            
456200       PERFORM IMS-32-ISRT-WDA601                                         
456300       PERFORM UNTIL ISRT-OK                                              
456400          ADD +1              TO VOR-TIREGTID-URSP                        
456500          ADD +1              TO VOR-TIREGTID-LEV                         
456600          PERFORM IMS-32-ISRT-WDA601                                      
456700       END-PERFORM                                                        
456800                                                                          
456900*--------------------------------------- EJ TRÄFF, AVVIK RAD              
457000       MOVE 3410-TIAAMMDD     TO VOR-TIREGDAT-AVV                         
457100       ADD +1                 TO WS-VOR-TID-BRIST                         
457200       MOVE WS-VOR-TID-BRIST  TO VOR-TIREGTID-AVV                         
457300       SUBTRACT 3410-TIAAMMDD    FROM 9999999                             
457400                                 GIVING VOR-TIREGDAT-AVV9                 
457500       SUBTRACT WS-VOR-TID-BRIST FROM 999999999                           
457600                                 GIVING VOR-TIREGTID-AVV9                 
457700       MOVE '0000000   '      TO VOR-IDKUNDRF-LEV                         
457800       MOVE 0                 TO VOR-TIREGDAT-LEV                         
457900       MOVE 0                 TO VOR-TIREGTID-LEV                         
458000       SUBTRACT WS-KVAVBART                                               
458100                            FROM ORAD-KVBEART-Q                           
458200                            GIVING VOR-KVBEART-Q                          
458300       MOVE 0                 TO VOR-KVPREAVB                             
458400       MOVE WS-KDORDBEK       TO VOR-KDORDBEK                             
458500       MOVE '0'               TO VOR-KDVORATG                             
458600       MOVE 0                 TO VOR-TIKLAR                               
458700       MOVE 0                 TO VOR-TIKLATID                             
458800                                                                          
458900       PERFORM IMS-32-ISRT-WDA601                                         
459000       PERFORM UNTIL ISRT-OK                                              
459100          ADD +1                TO WS-VOR-TID-BRIST                       
459200          MOVE WS-VOR-TID-BRIST TO VOR-TIREGTID-AVV                       
459300          SUBTRACT WS-VOR-TID-BRIST FROM 999999999                        
459400                                GIVING VOR-TIREGTID-AVV9                  
459500          PERFORM IMS-32-ISRT-WDA601                                      
459600       END-PERFORM                                                        
459700     END-IF                                                               
459800                                                                          
459900     COMPUTE CLAG-KVVORKO       = CLAG-KVVORKO                            
460000                                + VOR-KVBEART-Q                           
460100                                - VOR-KVPREAVB                            
460200     END-COMPUTE                                                          
460300                                                                          
460400     PERFORM IMS-22-REPL-WDK611                                           
460500                                                                          
460600     MOVE ORAD-IDDISTR          TO S27-IDDISTR                            
460700     MOVE ORAD-IDKUNDNR         TO S27-IDKUNDNR                           
460800     MOVE ORAD-IDARTNR          TO S27-IDARTNR                            
460900     MOVE S28-IDANSK            TO S27-IDANSK                             
461000     PERFORM S11DC-STARTA-W2T191X                                         
461100     .                                                                    
461200                                                                          
461300 S11DA-BESTAM-LENVR-ANSK SECTION.                                         
461400     MOVE 'S11DA-BESTAM-LENVR-ANSK  ' TO WS-CURRENT-SECTION               
461500                                                                          
461600     MOVE S28-IDARTNR    TO W-IDARTNR                                     
461700     PERFORM IMS-36-GU-WDK601                                             
461800     MOVE ART-IDLEVNR    TO S28-IDLEVNR                                   
461900                                                                          
462000     PERFORM IMS-37-GHNP-WDK611                                           
462100     MOVE CLAG-IDANSK    TO S28-IDANSK                                    
462200     MOVE CLAG-KVVORKO   TO S28-KVVORKO                                   
462300     .                                                                    
462400                                                                          
462500 S11DB-SOK-RAD-VORKO SECTION.                                             
462600     MOVE 'S11DB-SOK-RAD-VORKO      ' TO WS-CURRENT-SECTION               
462700                                                                          
462800     MOVE LOW-VALUE      TO W-WDA601KY-MIN-X                              
462900     MOVE HIGH-VALUE     TO W-WDA601KY-MAX-X                              
463000                                                                          
463100     MOVE ORAD-IDDISTR      TO W-A601KY-MIN-IDDISTR                       
463200                               W-A601KY-MAX-IDDISTR                       
463300     MOVE ORAD-IDKUNDNR     TO W-A601KY-MIN-IDKUNDNR                      
463400                               W-A601KY-MAX-IDKUNDNR                      
463500     MOVE ORAD-IDKUNDRF     TO W-A601KY-MIN-IDKUNDRF                      
463600                               W-A601KY-MAX-IDKUNDRF                      
463700     MOVE ORAD-TIREGDAT     TO W-A601KY-MIN-TIREGDAT                      
463800                               W-A601KY-MAX-TIREGDAT                      
463900     MOVE ORAD-IDARTNR      TO W-A601KY-MIN-IDARTNR                       
464000                               W-A601KY-MAX-IDARTNR                       
464100     MOVE NEJ               TO TRAFF-VORKO-SW                             
464200                                                                          
464300     PERFORM IMS-34-GHU-SEQB-WDA601                                       
464400     PERFORM UNTIL SEGMENT-SAKNAS                                         
464500                OR SEGMENT-SLUT                                           
464600                OR TRAFF-VORKO                                            
464700       IF  ORAD-KVBEART-Q = VOR-KVPREAVB                                  
464800           MOVE JA       TO TRAFF-VORKO-SW                                
464900       ELSE                                                               
465000           PERFORM IMS-35-GHN-SEQB-WDA601                                 
465100       END-IF                                                             
465200     END-PERFORM                                                          
465300     .                                                                    
465400                                                                          
465500 S11DC-STARTA-W2T191X  SECTION.                                           
465600     MOVE 'S11DC-STARTA-W2T191X     ' TO WS-CURRENT-SECTION               
465700                                                                          
465800     COMPUTE ALT-LL = LENGTH OF ALT-MID-W2I19101 + 17                     
465900     MOVE +1                    TO ALT-MID-KDCLAGER                       
466000     MOVE S27-IDARTNR-X         TO ALT-MID-IDARTNR                        
466100     MOVE ZERO                  TO ALT-MID-TISENBEK-DAG                   
466200                                   ALT-MID-TISENBEK-KL                    
466300     MOVE SPACE                 TO ALT-MID-IDKR                           
466400     MOVE S27-IDANSK-X          TO ALT-MID-IDANSK                         
466500     MOVE '500'                 TO ALT-MID-KDLARM                         
466600     MOVE S27-IDDISTR-X         TO ALT-MID-IDDISTR                        
466700     MOVE S27-IDKUNDNR-X        TO ALT-MID-IDKUNDNR                       
466800     MOVE ORAD-IDKUNDRF         TO ALT-MID-IDKUNDRF                       
466900     MOVE 'J'                   TO ALT-MID-FLNYLARM                       
467000     IF  DCS-NDC-CN                                                       
467100     OR (DCS-NDC-NA AND DCS-USA)                                          
467200        MOVE SLAG-IDDC          TO ALT-MID-IDDC                           
467300        MOVE '2'                TO ALT-SPRAK                              
467400     ELSE                                                                 
467500        MOVE WC-CDC-SE          TO ALT-MID-IDDC                           
467600        MOVE '1'                TO ALT-SPRAK                              
467700     END-IF                                                               
467800     MOVE SPACE                 TO ALT-MID-IDLEVNR                        
467900                                                                          
468000     PERFORM IMS-27-PURG-ALT-MSG                                          
468100                                                                          
468200     MOVE SPACE                 TO ALT-MID-W2I19101                       
468300     .                                                                    
468400                                                                          
468500 S12-SKAPA-RYKTRANS SECTION.                                              
468600     MOVE 'S12-SKAPA-RYKTRAN   '    TO WS-CURRENT-SECTION                 
468700                                                                          
468800     PERFORM S12A-SKAPA-RYKPOST                                           
468900                                                                          
469000     MOVE W-RYKPOST     TO LOGGPOST                                       
469100     MOVE SPACE         TO SORTPOST                                       
469200     MOVE 3410-TIAAMMDD TO TIAAMMDD                                       
469300     ACCEPT TIKLOCK FROM TIME                                             
469400     MOVE 1             TO IDLOGLOP                                       
469500                                                                          
469600     PERFORM IMS-29-ISRT-WDG601                                           
469700     PERFORM UNTIL SEGMENT-FINNS                                          
469800        IF IDLOGLOP = 9                                                   
469900           ACCEPT TIKLOCK FROM TIME                                       
470000           MOVE 0 TO IDLOGLOP                                             
470100        END-IF                                                            
470200        ADD 1 TO IDLOGLOP                                                 
470300        PERFORM IMS-29-ISRT-WDG601                                        
470400     END-PERFORM                                                          
470500     .                                                                    
470600                                                                          
470700 S12A-SKAPA-RYKPOST SECTION.                                              
470800     MOVE 'S12A-SKAPA-RYKPOST  '    TO WS-CURRENT-SECTION                 
470900                                                                          
471000     MOVE 'RYK'              TO W-RYK-IDPTYP                              
471100     MOVE ORAD-IDDISTR       TO W-RYK-IDDISTR                             
471200     MOVE ORAD-IDKUNDNR      TO W-RYK-IDKUNDNR                            
471300                                                                          
471400     IF ORAD-IDKUNDRF-RO = '0000000   '                                   
471500       MOVE ORAD-IDORDER     TO W-RYK-IDORDER                             
471600     ELSE                                                                 
471700       MOVE ORAD-IDDISTR     TO W-WDQ2CSEQ-IDDISTR                        
471800       MOVE ORAD-IDKUNDNR    TO W-WDQ2CSEQ-IDKUNDNR                       
471900       MOVE ORAD-IDKUNDRF-RO TO W-WDQ2CSEQ-IDKUNDRF                       
472000       PERFORM IMS-33-GU-WDQ201-CSEQ                                      
472100       MOVE CSQ-OHUV-IDORDER TO W-RYK-IDORDER                             
472200     END-IF                                                               
472300                                                                          
472400     MOVE ORAD-IDARTNR       TO W-RYK-IDARTNR                             
472500     MOVE 3410-TIAAMMDD      TO W-RYK-TIRODAT                             
472600     MOVE 0                  TO W-RYK-KVLEVART                            
472700     MOVE ORAD-KVBEART-Q     TO W-RYK-KVBEART-Q                           
472800     MOVE ORAD-KDORDKL       TO W-RYK-KDORDKL                             
472900     MOVE AREG-KDPRODSL      TO W-RYK-KDPRODSL                            
473000     MOVE WS-KDORDBEK        TO W-RYK-KDORDBEK                            
473100     .                                                                    
473200                                                                          
473300 S13-ATERSTALL-ORDERDEL SECTION.                                          
473400     MOVE 'S13-ATERSTALL-ORDERDEL' TO WS-CURRENT-SECTION                  
473500                                                                          
473600     MOVE 'R'       TO ODEL-KDODELSTA                                     
473700     MOVE SPACE     TO ODEL-IDUSER                                        
473800                       ODEL-IDBORD                                        
473900     MOVE ZERO      TO ODEL-DAUTSKR                                       
474000                       ODEL-TIUTSTID                                      
474100                                                                          
474200     PERFORM IMS-04-REPL-WDQ301                                           
474300     .                                                                    
474400                                                                          
474500 S15-LAES-EV-KUNDREG   SECTION.                                           
474600     MOVE 'S15-LAES-EV-KUNDREG   ' TO WS-CURRENT-SECTION                  
474700                                                                          
474800       IF  OHUV-IDDISTR   NOT = W-IDDISTR-WDB2                            
474900       OR  OHUV-IDKUNDNR  NOT = W-IDKUNDNR-WDB2                           
475000                                                                          
475100          MOVE OHUV-IDDISTR  TO W-IDDISTR-WDB2                            
475200          MOVE OHUV-IDKUNDNR TO W-IDKUNDNR-WDB2                           
475210          MOVE NEJ           TO SW-LYNK-NON-API                           
475220                                SW-VOR                                    
475300                                                                          
475400          PERFORM IMS-10-GU-WDB201                                        
475401          IF SEGMENT-FINNS                                                
475402            IF GMT-KDKUNDKAT = 03                                         
475403               IF  OHUV-IDSYSTEM(1:3) NOT = 'LYN'                         
475404                  MOVE JA         TO SW-LYNK-NON-API                      
475405               END-IF                                                     
475406               IF OHUV-KDORDKL = 0                                        
475407                  MOVE JA         TO SW-VOR                               
475408               END-IF                                                     
475409            END-IF                                                        
475420          END-IF                                                          
475500       END-IF                                                             
475600     .                                                                    
475700                                                                          
475800 S23-DELETE-PRICE-Q-LINE SECTION.                                         
475900     MOVE 'S23-DELETE-PRICE-Q-LINE' TO WS-CURRENT-SECTION                 
476000                                                                          
476100     MOVE OHUV-IDDISTR           TO TEST-IDDISTR                          
476200     IF DIST79-DEALER-PRICE                                               
476300       IF ORAD-IDPRQUES > ZERO                                            
476400         INITIALIZE PRQU-W335PRQU                                         
476500         MOVE OHUV-IDDISTR       TO PRQU-IDDISTR                          
476600         MOVE OHUV-IDKUNDNR      TO PRQU-IDKUNDNR                         
476700         MOVE OHUV-IDKUNDRF      TO PRQU-IDKUNDRF                         
476800         MOVE ORAD-IDPRQUES      TO PRQU-IDPRQUES                         
476900         MOVE 4                  TO PRQU-KDCALL                           
477000         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
477100                                            PRQU-WDC7-PCB                 
477200                                            PRQU-SJKO-WDK6-PCB            
477300       END-IF                                                             
477400     END-IF                                                               
477500     .                                                                    
477600 S36-ANDRA-WDC711  SECTION.                                               
477700     MOVE 'S36-ANDRA-WDC711       ' TO WS-CURRENT-SECTION                 
477800                                                                          
477900     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
478000       IF RAD-IDPRQUES > ZERO                                             
478100         INITIALIZE PRQU-W335PRQU                                         
478200         MOVE RAD-IDDISTR             TO PRQU-IDDISTR                     
478300         MOVE RAD-IDKUNDNR            TO PRQU-IDKUNDNR                    
478400         MOVE RAD-IDKUNDRF(1:5)       TO PRQU-IDKUNDRF(3:5)               
478500         MOVE '00'                    TO PRQU-IDKUNDRF(1:2)               
478600         MOVE RAD-IDPRQUES            TO PRQU-IDPRQUES                    
478700         MOVE 6                       TO PRQU-KDCALL                      
478800         CALL W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                 
478900                                            PRQU-WDC7-PCB                 
479000                                            PRQU-SJKO-WDK6-PCB            
479100         MOVE 'N'                     TO RAD-FLPRTILL                     
479200         IF RAD-PRARTNTO-LOC > +0                                         
479300           MOVE RAD-PRARTNTO-LOC      TO RAD-PRARTNTO-LOCPREL             
479400           MOVE ZERO                  TO RAD-PRARTNTO-LOC                 
479500         END-IF                                                           
479600         IF PRQU-KDCALL = -1                                              
479700           PERFORM S36A-NY-FRAGA                                          
479800         END-IF                                                           
479900       ELSE                                                               
480000*SKAPA NY PRISFRÅGA                                                       
480100         PERFORM S36A-NY-FRAGA                                            
480200       END-IF                                                             
480300     END-IF                                                               
480400     .                                                                    
480500                                                                          
480600 S36A-NY-FRAGA  SECTION.                                                  
480700     MOVE 'S36A-NY-FRAGA           ' TO WS-CURRENT-SECTION                
480800                                                                          
480900     MOVE ZERO                     TO WS-IDPRQUES                         
481000     IF WS-IDPRQUES                = +0                                   
481100        MOVE WS-IDPRQUES           TO PRNO-IDPRQUES-IN                    
481200        MOVE +1                    TO PRNO-KDCALL                         
481300                                                                          
481400        CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                   
481500                                                                          
481600        MOVE PRNO-IDPRQUES-UT      TO PRQU-IDPRQUES                       
481700                                      WS-IDPRQUES                         
481800        MOVE +1                    TO PRQU-KDCALL                         
481900     END-IF                                                               
482000     MOVE RAD-IDDISTR              TO PRQU-IDDISTR                        
482100     MOVE RAD-IDKUNDNR             TO PRQU-IDKUNDNR                       
482200     MOVE RAD-IDKUNDRF(1:5)        TO PRQU-IDKUNDRF(3:5)                  
482300     MOVE '00'                     TO PRQU-IDKUNDRF(1:2)                  
482400     MOVE ZERO                     TO PRQU-IDORDER                        
482500     MOVE RAD-KDORDKL              TO PRQU-KDORDKL                        
482600     IF RAD-IDDISTR = 0778 AND RAD-KDORDKL < 3                            
482700       AND RAD-DARODAT > 0                                                
482800       MOVE 4                      TO PRQU-KDORDKL                        
482900     END-IF                                                               
483000     MOVE 'Q'                      TO PRQU-KDPRSTA                        
483100     MOVE RAD-IDARTNR              TO PRQU-IDARTNR                        
483200     MOVE RAD-KVBEART-Q            TO PRQU-KVBEART-Q                      
483300     MOVE RAD-KDVALISO             TO PRQU-KDVALISO                       
483400     MOVE RAD-PRARTNTO-LOC         TO PRQU-PRARTNTO-LOC                   
483500     MOVE +0                       TO PRQU-PRARTNTO-LOCPREL               
483600     MOVE RAD-IDSYSTEM             TO PRQU-IDSYSTEM                       
483700*        PERFORM IMS-GU-GMTA-WDB201                                       
483800                                                                          
483900     CALL  W335PRQU USING PRQU-W335PRQU  PRQU-WDG2-PCB                    
484000                                         PRQU-WDC7-PCB                    
484100                                         PRQU-SJKO-WDK6-PCB               
484200     MOVE PRQU-IDPRQUES            TO  RAD-IDPRQUES                       
484300                                       WS-IDPRQUES                        
484400     MOVE 'N'                      TO  RAD-FLPRTILL                       
484500                                                                          
484600     IF RAD-PRARTNTO-LOC = +0                                             
484700        MOVE PRQU-PRARTNTO-LOCPREL TO                                     
484800                      RAD-PRARTNTO-LOCPREL                                
484900     ELSE                                                                 
485000        MOVE RAD-PRARTNTO-LOC  TO RAD-PRARTNTO-LOCPREL                    
485100        MOVE ZERO              TO RAD-PRARTNTO-LOC                        
485200     END-IF                                                               
485300                                                                          
485400     MOVE WS-IDPRQUES             TO PRNO-IDPRQUES-IN                     
485500     MOVE +3                      TO PRNO-KDCALL                          
485600                                                                          
485700     CALL W335PRNO USING PRNO-W335PRNO PRNO-3107-PCB                      
485800     .                                                                    
485900                                                                          
486000 S20-FINN-INTERVALL  SECTION.                                             
486100                                                                          
486200     PERFORM IMS-GU-WDM211                                                
486300     IF SEGMENT-FINNS                                                     
486400       PERFORM IMS-GNP-WDM221                                             
486500       PERFORM UNTIL SEGMENT-SAKNAS                                       
486600         IF  ORAD-IDDISTR > KMRK-IDDISTR-TOM                              
486700         OR  ORAD-IDDISTR < KMRK-IDDISTR-FOM                              
486800           CONTINUE                                                       
486900         ELSE                                                             
487000           IF  ORAD-IDDISTR  = KMRK-IDDISTR-TOM                           
487100           AND ORAD-IDKUNDNR > KMRK-IDKUNDNR-TOM                          
487200             CONTINUE                                                     
487300           ELSE                                                           
487400             IF  ORAD-IDDISTR  = KMRK-IDDISTR-FOM                         
487500             AND ORAD-IDKUNDNR < KMRK-IDKUNDNR-FOM                        
487600               CONTINUE                                                   
487700             ELSE                                                         
487800               MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM               
487900               MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM               
488000               MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM              
488100               MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM              
488200             END-IF                                                       
488300           END-IF                                                         
488400         END-IF                                                           
488500         PERFORM IMS-GNP-WDM221                                           
488600       END-PERFORM                                                        
488700     END-IF                                                               
488800     .                                                                    
488900     EJECT                                                                
489000                                                                          
489100 S98-SKAPA-RYXTRANS SECTION.                                              
489200     MOVE 'S98-SKAPA-RYXTRANS     ' TO WS-CURRENT-SECTION                 
489300                                                                          
489400     PERFORM S98A-SKAPA-RYXPOST                                           
489500     PERFORM S98B-SKAPA-SORTPOST                                          
489600                                                                          
489700     MOVE W-RYXPOST      TO LOGGPOST                                      
489800     MOVE W-RYX-SORTPOST TO SORTPOST                                      
489900                                                                          
490000     PERFORM IMS-29-ISRT-WDG601                                           
490100     .                                                                    
490200 S98A-SKAPA-RYXPOST SECTION.                                              
490300     MOVE 'S98A-SKAPA-RYXPOST     ' TO WS-CURRENT-SECTION                 
490400                                                                          
490500     MOVE 'RYX'              TO W-RYX-IDPTYP                              
490600     MOVE DEAV-IDKAMPRF-IN   TO W-RYX-IDKAMPRF-IN                         
490700     MOVE DEAV-IDARTNR-IN    TO W-RYX-IDARTNR-IN                          
490800     MOVE DEAV-TIRODAT-IN    TO W-RYX-TIRODAT-IN                          
490900     MOVE DEAV-ADLAGOMR-IN   TO W-RYX-ADLAGOMR-IN                         
491000     MOVE DEAV-KDORDKL-IN    TO W-RYX-KDORDKL-IN                          
491100     MOVE DEAV-IDDC-RO-IN    TO W-RYX-IDDC-RO-IN                          
491200     MOVE DEAV-IDDISTR-IN    TO W-RYX-IDDISTR-IN                          
491300     MOVE DEAV-KVAKS-CDC-IN  TO W-RYX-KVAKS-CDC-IN                        
491400     MOVE DEAV-KVAKS-PAV-IN  TO W-RYX-KVAKS-PAV-IN                        
491500     MOVE DEAV-KVLS-IN       TO W-RYX-KVLS-IN                             
491600     MOVE DEAV-KVRESS-IN     TO W-RYX-KVRESS-IN                           
491700     MOVE DEAV-KVSPANT-IN    TO W-RYX-KVSPANT-IN                          
491800     MOVE DEAV-KVUTRS-IN     TO W-RYX-KVUTRS-IN                           
491900     MOVE DEAV-RERF-ART-IN   TO W-RYX-RERF-ART-IN                         
492000     MOVE DEAV-RERF-RAD-NY-IN TO W-RYX-RERF-RAD-NY-IN                     
492100     MOVE DEAV-KDSORT-IN     TO W-RYX-KDSORT-IN                           
492200     MOVE DEAV-KDPRODSL-IN   TO W-RYX-KDPRODSL-IN                         
492300     MOVE DEAV-KDLEVSP-IN    TO W-RYX-KDLEVSP-IN                          
492400     MOVE DEAV-FLAKPLOC-IN   TO W-RYX-FLAKPLOC-IN                         
492500     MOVE DEAV-KVBEART-Q-IN  TO W-RYX-KVBEART-Q-IN                        
492600     MOVE DEAV-KVPREAVB-IN   TO W-RYX-KVPREAVB-IN                         
492700     MOVE DEAV-KVPRERO-IN    TO W-RYX-KVPRERO-IN                          
492800     MOVE DEAV-RERF-RAD-IN   TO W-RYX-RERF-RAD-IN                         
492900     MOVE DEAV-FLRESTN-IN    TO W-RYX-FLRESTN-IN                          
493000     MOVE DEAV-KVSPARR-KVAL-IN TO W-RYX-KVSPARR-KVAL-IN                   
493100     MOVE DEAV-IDKUNDNR-IN   TO W-RYX-IDKUNDNR-IN                         
493200     MOVE DEAV-IDORDNR5-IN   TO W-RYX-IDORDNR5-IN                         
493300     .                                                                    
493400                                                                          
493500 S98B-SKAPA-SORTPOST SECTION.                                             
493600     MOVE 'S98B-SKAPA-SORTPOST    ' TO WS-CURRENT-SECTION                 
493700                                                                          
493800     MOVE DEAV-FLAKPLOC-UT    TO W-RYXS-FLAKPLOC-UT                       
493900     MOVE DEAV-KVAVBART-UT    TO W-RYXS-KVAVBART-UT                       
494000     MOVE DEAV-KDORDBEK-UT    TO W-RYXS-KDORDBEK-UT                       
494100     MOVE DEAV-RERF-RAD-UT    TO W-RYXS-RERF-RAD-UT                       
494200     MOVE DEAV-KVEFRS-UT      TO W-RYXS-KVEFRS-UT                         
494300     MOVE DEAV-KVLS-UT        TO W-RYXS-KVLS-UT                           
494400     MOVE DEAV-KVRESS-UT      TO W-RYXS-KVRESS-UT                         
494500     MOVE DEAV-KVROS-UT       TO W-RYXS-KVROS-UT                          
494600     MOVE DEAV-KDROO-UT       TO W-RYXS-KDROO-UT                          
494700     .                                                                    
494800                                                                          
494900 S99-SKAPA-RYXTRANS SECTION.                                              
495000     MOVE 'S99-SKAPA-RYXTRANS     ' TO WS-CURRENT-SECTION                 
495100                                                                          
495200     PERFORM S98A-SKAPA-RYXPOST                                           
495300     PERFORM S98B-SKAPA-SORTPOST                                          
495400                                                                          
495500     MOVE W-RYXPOST      TO LOGGPOST                                      
495600     MOVE W-RYX-SORTPOST TO SORTPOST                                      
495700     MOVE 3410-TIAAMMDD  TO TIAAMMDD                                      
495800     ACCEPT TIKLOCK FROM TIME                                             
495900     MOVE 1              TO IDLOGLOP                                      
496000                                                                          
496100     PERFORM IMS-29-ISRT-WDG601                                           
496200                                                                          
496300     PERFORM UNTIL SEGMENT-FINNS                                          
496400        IF IDLOGLOP = 9                                                   
496500           ACCEPT TIKLOCK FROM TIME                                       
496600           MOVE 0 TO IDLOGLOP                                             
496700        END-IF                                                            
496800        ADD 1 TO IDLOGLOP                                                 
496900        PERFORM IMS-29-ISRT-WDG601                                        
497000     END-PERFORM                                                          
497100     .                                                                    
497200                                                                          
497300 IMS-01-GU-WDGX4004      SECTION.                                         
497400     MOVE 'IMS-01' TO WS-CURRENT-IMS-SECTION                              
497500                                                                          
497600                                                                          
497700     MOVE SPACE               TO ALL-SSA                                  
497800     STRING 'WL400301(WDGXKEY  =' W-4003-ETIK-IDHTYP-X ')'                
497900          DELIMITED BY SIZE INTO SSA1                                     
498000     MOVE 'WL400311 '         TO SSA2                                     
498100     MOVE '  GE' TO GODK-STATUSKODER                                      
498200     CALL CBLTDLI USING GU 4003-PCB DLI-IO-WDGX4004 SSA1 SSA2             
498300     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
498400     PERFORM IMS-STATUSKONTROLL                                           
498500     .                                                                    
498600                                                                          
498700 IMS-02-GU-4007-WDGX4008 SECTION.                                         
498800     MOVE 'IMS-02' TO WS-CURRENT-IMS-SECTION                              
498900                                                                          
499000                                                                          
499100     MOVE SPACE               TO ALL-SSA                                  
499200     STRING 'WL400701(WDGXKEY  =' W-4007-PU-IDHTYP-X ')'                  
499300          DELIMITED BY SIZE INTO SSA1                                     
499400     MOVE 'WL400711 ' TO SSA2                                             
499500     MOVE '  GE' TO GODK-STATUSKODER                                      
499600     CALL CBLTDLI USING GU 4007-PCB DLI-IO-WDGX4008 SSA1 SSA2             
499700     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
499800     PERFORM IMS-STATUSKONTROLL                                           
499900     .                                                                    
500000                                                                          
500100 IMS-03-GHU-WDQ301 SECTION.                                               
500200     MOVE 'IMS-03' TO WS-CURRENT-IMS-SECTION                              
500300                                                                          
500400     MOVE SPACE               TO ALL-SSA                                  
500500     STRING 'WDQ301  (WDQ301KY =' W-WDQ301KY-X ')'                        
500600          DELIMITED BY SIZE INTO SSA1                                     
500700     MOVE '  GE' TO GODK-STATUSKODER                                      
500800     CALL CBLTDLI USING GHU WDQ3-PCB DLI-IO-WDQ301 SSA1                   
500900     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
501000     PERFORM IMS-STATUSKONTROLL                                           
501100     .                                                                    
501200                                                                          
501300 IMS-04-REPL-WDQ301 SECTION.                                              
501400     MOVE 'IMS-04' TO WS-CURRENT-IMS-SECTION                              
501500                                                                          
501600     MOVE SPACE               TO ALL-SSA                                  
501700     MOVE '  ' TO GODK-STATUSKODER                                        
501800     CALL CBLTDLI USING REPL WDQ3-PCB DLI-IO-WDQ301                       
501900     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
502000     PERFORM IMS-STATUSKONTROLL                                           
502100     .                                                                    
502200                                                                          
502300 IMS-05-GU-WDB601    SECTION.                                             
502400     MOVE 'IMS-05' TO WS-CURRENT-IMS-SECTION                              
502500                                                                          
502600     MOVE SPACE               TO ALL-SSA                                  
502700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
502800          DELIMITED BY SIZE INTO SSA1                                     
502900     MOVE '  GE' TO GODK-STATUSKODER                                      
503000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
503100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
503200     PERFORM IMS-STATUSKONTROLL                                           
503300     .                                                                    
503400                                                                          
503500 IMS-06-GU-WDGX4536      SECTION.                                         
503600     MOVE 'IMS-06' TO WS-CURRENT-IMS-SECTION                              
503700                                                                          
503800     MOVE SPACE               TO ALL-SSA                                  
503900     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
504000          DELIMITED BY SIZE INTO SSA1                                     
504100     STRING 'WLXXKU11(WDGXKEY  =' W-4536-IDSKYLT-X ')'                    
504200          DELIMITED BY SIZE INTO SSA2                                     
504300     MOVE '  GE' TO GODK-STATUSKODER                                      
504400     CALL CBLTDLI USING GU 4536-PCB DLI-IO-WDGX4536 SSA1 SSA2             
504500     MOVE 4536-STATUS-CODE TO STATUS-WS                                   
504600     PERFORM IMS-STATUSKONTROLL                                           
504700     .                                                                    
504800                                                                          
504900 IMS-07-GHU-WDQ201-12 SECTION.                                            
505000     MOVE 'IMS-07' TO WS-CURRENT-IMS-SECTION                              
505100                                                                          
505200     MOVE SPACE               TO ALL-SSA                                  
505300     STRING 'WDQ201  *D(IDORDER  =' W-IDORDER-X ')'                       
505400          DELIMITED BY SIZE INTO SSA1                                     
505500     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
505600          DELIMITED BY SIZE INTO SSA2                                     
505700     MOVE '  GE' TO GODK-STATUSKODER                                      
505800     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-WDQ201-12 SSA1 SSA2           
505900     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
506000     PERFORM IMS-STATUSKONTROLL                                           
506100     .                                                                    
506200                                                                          
506300 IMS-GHNP-WDQ221 SECTION.                                                 
506400     MOVE 'GHNP-Q221' TO WS-CURRENT-IMS-SECTION                           
506500                                                                          
506600     STRING 'WDQ221  (IDPRC    =' W-IDPRC-X ')'                           
506700          DELIMITED BY SIZE INTO SSA1                                     
506800     MOVE '  GE' TO GODK-STATUSKODER                                      
506900     CALL CBLTDLI USING GHNP WDQ2-PCB DLI-IO-WDQ221 SSA1                  
507000     MOVE WDQ2-STATUS-CODE TO STATUS-WS  STATUS-WS-Q221                   
507100     PERFORM IMS-STATUSKONTROLL                                           
507200     .                                                                    
507300                                                                          
507400 IMS-REPL-WDQ221 SECTION.                                                 
507500     MOVE 'REPL-Q221' TO WS-CURRENT-IMS-SECTION                           
507600                                                                          
507700     MOVE SPACE               TO ALL-SSA                                  
507800     MOVE '    ' TO GODK-STATUSKODER                                      
507900     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-WDQ221                       
508000     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
508100     PERFORM IMS-STATUSKONTROLL                                           
508200     .                                                                    
508300                                                                          
508400 IMS-DLET-WDQ221 SECTION.                                                 
508500     MOVE 'DLET-Q221' TO WS-CURRENT-IMS-SECTION                           
508600                                                                          
508700     MOVE SPACE               TO ALL-SSA                                  
508800     MOVE '    ' TO GODK-STATUSKODER                                      
508900     CALL CBLTDLI USING DLET WDQ2-PCB DLI-IO-WDQ221                       
509000     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
509100     PERFORM IMS-STATUSKONTROLL                                           
509200     .                                                                    
509300                                                                          
509400 IMS-08-GHU-WDGX4017 SECTION.                                             
509500     MOVE 'IMS-08' TO WS-CURRENT-IMS-SECTION                              
509600                                                                          
509700     MOVE SPACE               TO ALL-SSA                                  
509800     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
509900          DELIMITED BY SIZE INTO SSA1                                     
510000     MOVE '  GE' TO GODK-STATUSKODER                                      
510100     CALL CBLTDLI USING GHU 4017-PCB DLI-IO-WDGX4017 SSA1                 
510200     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
510300     PERFORM IMS-STATUSKONTROLL                                           
510400     .                                                                    
510500                                                                          
510600 IMS-09-GHU-WDGX4018 SECTION.                                             
510700     MOVE 'IMS-09' TO WS-CURRENT-IMS-SECTION                              
510800                                                                          
510900     MOVE SPACE               TO ALL-SSA                                  
511000     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
511100          DELIMITED BY SIZE INTO SSA1                                     
511200     STRING 'WDGX4018(KDSEGKEY =' W-4018-KDSEGKEY-X ')'                   
511300          DELIMITED BY SIZE INTO SSA2                                     
511400     MOVE '  GE' TO GODK-STATUSKODER                                      
511500     CALL CBLTDLI USING GHU 4017-PCB DLI-IO-WDGX4017 SSA1 SSA2            
511600     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
511700     PERFORM IMS-STATUSKONTROLL                                           
511800     .                                                                    
511900                                                                          
512000 IMS-10-GU-WDB201       SECTION.                                          
512100     MOVE 'IMS-10' TO WS-CURRENT-IMS-SECTION                              
512200                                                                          
512300     MOVE SPACE               TO ALL-SSA                                  
512400     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
512500                      DELIMITED BY SIZE INTO SSA1                         
512600     MOVE '  ' TO GODK-STATUSKODER                                        
512700     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
512800     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
512900     PERFORM IMS-STATUSKONTROLL                                           
513000     .                                                                    
513100                                                                          
513200 IMS-11-GU-WDGX4448      SECTION.                                         
513300     MOVE 'IMS-11' TO WS-CURRENT-IMS-SECTION                              
513400                                                                          
513500     MOVE SPACE               TO ALL-SSA                                  
513600     STRING 'WLXXKH01(WDGXKEY  =' W-4447-IDHTYP-X ')'                     
513700          DELIMITED BY SIZE INTO SSA1                                     
513800     STRING 'WLXXKH11(WDGXKEY  =' W-4448-IDPRC-X ')'                      
513900          DELIMITED BY SIZE INTO SSA2                                     
514000     MOVE '  GE' TO GODK-STATUSKODER                                      
514100     CALL CBLTDLI USING GU 4448-PCB DLI-IO-WDGX4448 SSA1 SSA2             
514200     MOVE 4448-STATUS-CODE TO STATUS-WS                                   
514300     PERFORM IMS-STATUSKONTROLL                                           
514400     .                                                                    
514500                                                                          
514600 IMS-12-GHU-WDQ401 SECTION.                                               
514700     MOVE 'IMS-12' TO WS-CURRENT-IMS-SECTION                              
514800                                                                          
514900     MOVE SPACE               TO ALL-SSA                                  
515000     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
515100                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
515200          DELIMITED BY SIZE INTO SSA1                                     
515300     MOVE '  GE' TO GODK-STATUSKODER                                      
515400     CALL CBLTDLI USING GHU WDQ4-PCB DLI-IO-WDQ401 SSA1                   
515500     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
515600     PERFORM IMS-STATUSKONTROLL                                           
515700     .                                                                    
515800                                                                          
515900 IMS-13-GHN-WDQ401 SECTION.                                               
516000     MOVE 'IMS-13' TO WS-CURRENT-IMS-SECTION                              
516100                                                                          
516200     MOVE SPACE               TO ALL-SSA                                  
516300     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
516400                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
516500          DELIMITED BY SIZE INTO SSA1                                     
516600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
516700     CALL CBLTDLI USING GHN WDQ4-PCB DLI-IO-WDQ401 SSA1                   
516800     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
516900     PERFORM IMS-STATUSKONTROLL                                           
517000     .                                                                    
517100 IMS-14-GU-WDD311 SECTION.                                                
517200     MOVE 'IMS-14' TO WS-CURRENT-IMS-SECTION                              
517300                                                                          
517400     MOVE SPACE               TO ALL-SSA                                  
517500     STRING 'WDD301  (WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
517600          DELIMITED BY SIZE INTO SSA1                                     
517700     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
517800          DELIMITED BY SIZE INTO SSA2                                     
517900     MOVE '  GE' TO GODK-STATUSKODER                                      
518000     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
518100     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
518200     PERFORM IMS-STATUSKONTROLL                                           
518300     .                                                                    
518400                                                                          
518500 IMS-15-GU-WDQ101 SECTION.                                                
518600     MOVE 'IMS-15' TO WS-CURRENT-IMS-SECTION                              
518700                                                                          
518800     MOVE SPACE               TO ALL-SSA                                  
518900     STRING 'WDQ101  (WDQ101KY>=' W-WDQ101KY-MIN-X                        
519000                    '&WDQ101KY<=' W-WDQ101KY-MAX-X ')'                    
519100             DELIMITED BY SIZE INTO SSA1                                  
519200     MOVE '  GE' TO GODK-STATUSKODER                                      
519300     CALL CBLTDLI USING GU WDQ1-PCB DLI-IO-WDQ101 SSA1                    
519400     MOVE WDQ1-STATUS-CODE TO STATUS-WS                                   
519500     PERFORM IMS-STATUSKONTROLL                                           
519600     .                                                                    
519700                                                                          
519800 IMS-16-ISRT-WDQ101 SECTION.                                              
519900     MOVE 'IMS-16' TO WS-CURRENT-IMS-SECTION                              
520000                                                                          
520100     MOVE SPACE               TO ALL-SSA                                  
520200     MOVE 'WDQ101   ' TO SSA1                                             
520300     MOVE '  II'      TO GODK-STATUSKODER                                 
520400     CALL CBLTDLI USING ISRT WDQ1-PCB DLI-IO-WDQ101 SSA1                  
520500     MOVE WDQ1-STATUS-CODE TO STATUS-WS                                   
520600     PERFORM IMS-STATUSKONTROLL                                           
520700     .                                                                    
520800                                                                          
520900 IMS-17-GHU-WDK711 SECTION.                                               
521000     MOVE 'IMS-17' TO WS-CURRENT-IMS-SECTION                              
521100                                                                          
521200     MOVE SPACE               TO ALL-SSA                                  
521300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
521400          DELIMITED BY SIZE INTO SSA1                                     
521500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
521600          DELIMITED BY SIZE INTO SSA2                                     
521700     MOVE '  ' TO GODK-STATUSKODER                                        
521800     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
521900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
522000     PERFORM IMS-STATUSKONTROLL                                           
522100     .                                                                    
522200                                                                          
522300 IMS-GU-WDK722 SECTION.                                                   
522400     MOVE 'IMS-GU-WDK722' TO WS-CURRENT-IMS-SECTION                       
522500                                                                          
522600     MOVE SPACE               TO ALL-SSA                                  
522700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
522800          DELIMITED BY SIZE INTO SSA1                                     
522900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
523000          DELIMITED BY SIZE INTO SSA2                                     
523100     MOVE 'WDK722 '           TO SSA3                                     
523200     MOVE '  GE' TO GODK-STATUSKODER                                      
523300     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
523400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
523500     PERFORM IMS-STATUSKONTROLL                                           
523600     .                                                                    
523700                                                                          
523800 IMS-18-REPL-WDK711 SECTION.                                              
523900     MOVE 'IMS-18' TO WS-CURRENT-IMS-SECTION                              
524000                                                                          
524100     MOVE SPACE               TO ALL-SSA                                  
524200     MOVE '    ' TO GODK-STATUSKODER                                      
524300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
524400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
524500     PERFORM IMS-STATUSKONTROLL                                           
524600     .                                                                    
524700                                                                          
524800 IMS-19-ISRT-WDA501        SECTION.                                       
524900     MOVE 'IMS-19' TO WS-CURRENT-IMS-SECTION                              
525000                                                                          
525100     MOVE SPACE               TO ALL-SSA                                  
525200     MOVE 'WDA501   ' TO SSA1                                             
525300     MOVE '  II' TO GODK-STATUSKODER                                      
525400     CALL CBLTDLI USING ISRT WDA5-PCB DLI-IO-WDA501 SSA1                  
525500     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
525600     PERFORM IMS-STATUSKONTROLL                                           
525700     .                                                                    
525800                                                                          
525900 IMS-20-GU-WDGX4512 SECTION.                                              
526000     MOVE 'IMS-20' TO WS-CURRENT-IMS-SECTION                              
526100                                                                          
526200     MOVE SPACE               TO ALL-SSA                                  
526300     STRING 'WLXXJN01(WDGXKEY  =' W-4511-IDHTYP-X  ')'                    
526400          DELIMITED BY SIZE INTO SSA1                                     
526500     STRING 'WLXXJN11(KDTPOTYP =' W-4512-KDTPOTYP-X                       
526600                    '&KDORDKL  =' W-4512-KDORDKL-X                        
526700                    '&IDDISTRF<=' W-4512-IDDISTR-FOM-X                    
526800                    '&IDDISTRT>=' W-4512-IDDISTR-TOM-X ')'                
526900          DELIMITED BY SIZE INTO SSA2                                     
527000     MOVE '  GE' TO GODK-STATUSKODER                                      
527100     CALL CBLTDLI USING GU 4512-PCB DLI-IO-WDGX4512 SSA1 SSA2             
527200     MOVE 4512-STATUS-CODE TO STATUS-WS                                   
527300     PERFORM IMS-STATUSKONTROLL                                           
527400     .                                                                    
527500 IMS-21-GHU-WDK611 SECTION.                                               
527600     MOVE 'IMS-21' TO WS-CURRENT-IMS-SECTION                              
527700                                                                          
527800     MOVE SPACE               TO ALL-SSA                                  
527900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
528000          DELIMITED BY SIZE INTO SSA1                                     
528100     STRING 'WDK611  (KDSEGKEY =' W-WDK611-KDSEGKEY-X ')'                 
528200          DELIMITED BY SIZE INTO SSA2                                     
528300     MOVE '  GE' TO GODK-STATUSKODER                                      
528400     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
528500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
528600     PERFORM IMS-STATUSKONTROLL                                           
528700     .                                                                    
528800                                                                          
528900 IMS-22-REPL-WDK611 SECTION.                                              
529000     MOVE 'IMS-22' TO WS-CURRENT-IMS-SECTION                              
529100                                                                          
529200     MOVE SPACE               TO ALL-SSA                                  
529300     MOVE '    ' TO GODK-STATUSKODER                                      
529400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
529500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
529600     PERFORM IMS-STATUSKONTROLL                                           
529700     .                                                                    
529800                                                                          
529900 IMS-GHU-WDM211 SECTION.                                                  
530000     MOVE 'IMS-GU-WDM211      ' TO WS-CURRENT-IMS-SECTION                 
530100                                                                          
530200     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
530300          DELIMITED BY SIZE INTO SSA1                                     
530400     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
530500          DELIMITED BY SIZE INTO SSA2                                     
530600     MOVE '  GE'              TO GODK-STATUSKODER                         
530700     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2              
530800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
530900     PERFORM IMS-STATUSKONTROLL                                           
531000     .                                                                    
531100                                                                          
531200 IMS-REPL-WDM211 SECTION.                                                 
531300     MOVE 'IMS-REPL-WDM211     ' TO WS-CURRENT-IMS-SECTION                
531400                                                                          
531500     MOVE '  '             TO GODK-STATUSKODER                            
531600     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM211                       
531700     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
531800     PERFORM IMS-STATUSKONTROLL                                           
531900     .                                                                    
532000                                                                          
532100 IMS-GU-WDM211 SECTION.                                                   
532200                                                                          
532300     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
532400          DELIMITED BY SIZE INTO SSA1                                     
532500     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
532600          DELIMITED BY SIZE INTO SSA2                                     
532700     MOVE '  GE'              TO GODK-STATUSKODER                         
532800     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
532900     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
533000     PERFORM IMS-STATUSKONTROLL                                           
533100     .                                                                    
533200                                                                          
533300 IMS-GNP-WDM221 SECTION.                                                  
533400                                                                          
533500     MOVE 'WDM221 '           TO SSA1                                     
533600     MOVE '    GE'            TO GODK-STATUSKODER                         
533700     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
533800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
533900     PERFORM IMS-STATUSKONTROLL                                           
534000     .                                                                    
534100                                                                          
534200 IMS-GHU-WDM221 SECTION.                                                  
534300     MOVE 'IMS-GHU-WDM221      ' TO WS-CURRENT-IMS-SECTION                
534400                                                                          
534500     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
534600          DELIMITED BY SIZE INTO SSA1                                     
534700     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
534800          DELIMITED BY SIZE INTO SSA2                                     
534900     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
535000          DELIMITED BY SIZE INTO SSA3                                     
535100     MOVE '  GE' TO GODK-STATUSKODER                                      
535200     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
535300     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
535400     PERFORM IMS-STATUSKONTROLL                                           
535500     .                                                                    
535600                                                                          
535700 IMS-REPL-WDM221 SECTION.                                                 
535800     MOVE 'IMS-REPL-WDM221     ' TO WS-CURRENT-IMS-SECTION                
535900                                                                          
536000     MOVE '  '             TO GODK-STATUSKODER                            
536100     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
536200     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
536300     PERFORM IMS-STATUSKONTROLL                                           
536400     .                                                                    
536500 IMS-27-PURG-ALT-MSG SECTION.                                             
536600     MOVE 'IMS-27' TO WS-CURRENT-IMS-SECTION                              
536700                                                                          
536800     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
536900     MOVE '  '  TO GODK-STATUSKODER                                       
537000     CALL CBLTDLI USING PURG ALT-PCB ALT-IO-AREA                          
537100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
537200     PERFORM IMS-STATUSKONTROLL                                           
537300     .                                                                    
537400 IMS-28-DLET-WDQ401        SECTION.                                       
537500     MOVE 'IMS-28' TO WS-CURRENT-IMS-SECTION                              
537600                                                                          
537700     MOVE SPACE               TO ALL-SSA                                  
537800     MOVE '    ' TO GODK-STATUSKODER                                      
537900     CALL CBLTDLI USING DLET WDQ4-PCB DLI-IO-WDQ401                       
538000     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
538100     PERFORM IMS-STATUSKONTROLL                                           
538200     .                                                                    
538300 IMS-29-ISRT-WDG601        SECTION.                                       
538400     MOVE 'IMS-29' TO WS-CURRENT-IMS-SECTION                              
538500                                                                          
538600     MOVE SPACE               TO ALL-SSA                                  
538700     MOVE 'WDG601   ' TO SSA1                                             
538800     MOVE '  II' TO GODK-STATUSKODER                                      
538900     CALL CBLTDLI USING ISRT WDG6-PCB DLI-IO-WDG601 SSA1                  
539000     MOVE WDG6-STATUS-CODE TO STATUS-WS                                   
539100     PERFORM IMS-STATUSKONTROLL                                           
539200     .                                                                    
539300                                                                          
539400 IMS-30-GHU-WDA501 SECTION.                                               
539500     MOVE 'IMS-30' TO WS-CURRENT-IMS-SECTION                              
539600                                                                          
539700     MOVE SPACE               TO ALL-SSA                                  
539800     STRING 'WDA501  (WDA501KY =' W-WDA501KY-X ')'                        
539900          DELIMITED BY SIZE INTO SSA1                                     
540000     MOVE '  GE' TO GODK-STATUSKODER                                      
540100     CALL CBLTDLI USING GHU WDA5-PCB DLI-IO-WDA501 SSA1                   
540200     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
540300     PERFORM IMS-STATUSKONTROLL                                           
540400     .                                                                    
540500                                                                          
540600 IMS-30-REPL-WDA501   SECTION.                                            
540700     MOVE 'IMS-30' TO WS-CURRENT-IMS-SECTION                              
540800                                                                          
540900     MOVE SPACE               TO ALL-SSA                                  
541000     MOVE '    ' TO GODK-STATUSKODER                                      
541100     CALL CBLTDLI USING REPL WDA5-PCB DLI-IO-WDA501                       
541200     MOVE WDA5-STATUS-CODE TO STATUS-WS                                   
541300     PERFORM IMS-STATUSKONTROLL                                           
541400     .                                                                    
541500                                                                          
541600 IMS-31-REPL-SEQB-WDA601            SECTION.                              
541700     MOVE 'IMS-31' TO WS-CURRENT-IMS-SECTION                              
541800                                                                          
541900     MOVE SPACE               TO ALL-SSA                                  
542000     MOVE 'WDA601  '           TO SSA1                                    
542100     MOVE '    '               TO GODK-STATUSKODER                        
542200     CALL  CBLTDLI  USING REPL WDA6B-PCB DLI-IO-WDA601 SSA1               
542300     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
542400     PERFORM IMS-STATUSKONTROLL                                           
542500     .                                                                    
542600                                                                          
542700 IMS-32-ISRT-WDA601         SECTION.                                      
542800     MOVE 'IMS-32' TO WS-CURRENT-IMS-SECTION                              
542900                                                                          
543000     MOVE SPACE               TO ALL-SSA                                  
543100     MOVE   'WDA601  '         TO SSA1                                    
543200     MOVE '  IINI' TO GODK-STATUSKODER                                    
543300     CALL  CBLTDLI  USING ISRT WDA6-PCB DLI-IO-WDA601 SSA1                
543400     MOVE WDA6-STATUS-CODE     TO STATUS-WS                               
543500     PERFORM IMS-STATUSKONTROLL                                           
543600     .                                                                    
543700                                                                          
543800 IMS-33-GU-WDQ201-CSEQ SECTION.                                           
543900     MOVE 'IMS-33' TO WS-CURRENT-IMS-SECTION                              
544000                                                                          
544100     MOVE SPACE               TO ALL-SSA                                  
544200     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
544300             DELIMITED BY SIZE INTO SSA1                                  
544400     MOVE    '  '                TO GODK-STATUSKODER                      
544500     CALL    CBLTDLI USING       GU   WDQ2C-PCB DLI-IO-WDQ201             
544600                                      SSA1                                
544700     MOVE    WDQ2C-STATUS-CODE TO STATUS-WS                               
544800     PERFORM IMS-STATUSKONTROLL                                           
544900     .                                                                    
545000                                                                          
545100 IMS-34-GHU-SEQB-WDA601            SECTION.                               
545200     MOVE 'IMS-34' TO WS-CURRENT-IMS-SECTION                              
545300                                                                          
545400     MOVE SPACE               TO ALL-SSA                                  
545500     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
545600                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
545700            DELIMITED BY SIZE INTO SSA1                                   
545800     MOVE '  GE'                 TO GODK-STATUSKODER                      
545900     CALL  CBLTDLI  USING GHU   WDA6B-PCB DLI-IO-WDA601 SSA1              
546000     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
546100     PERFORM IMS-STATUSKONTROLL                                           
546200     .                                                                    
546300                                                                          
546400 IMS-35-GHN-SEQB-WDA601            SECTION.                               
546500     MOVE 'IMS-35' TO WS-CURRENT-IMS-SECTION                              
546600                                                                          
546700     MOVE SPACE               TO ALL-SSA                                  
546800     STRING 'WDA601  (WDA6BSEQ>=' W-WDA601KY-MIN-X                        
546900                    '&WDA6BSEQ<=' W-WDA601KY-MAX-X ')'                    
547000            DELIMITED BY SIZE INTO SSA1                                   
547100     MOVE '  GEGB'               TO GODK-STATUSKODER                      
547200     CALL  CBLTDLI  USING GHN   WDA6B-PCB DLI-IO-WDA601 SSA1              
547300     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
547400     PERFORM IMS-STATUSKONTROLL                                           
547500     .                                                                    
547600                                                                          
547700 IMS-36-GU-WDK601                 SECTION.                                
547800     MOVE 'IMS-36' TO WS-CURRENT-IMS-SECTION                              
547900                                                                          
548000     MOVE SPACE               TO ALL-SSA                                  
548100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
548200            DELIMITED BY SIZE INTO SSA1                                   
548300     MOVE '  '                   TO GODK-STATUSKODER                      
548400     CALL  CBLTDLI  USING GU   WDK6-PCB DLI-IO-WDK601 SSA1                
548500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
548600     PERFORM IMS-STATUSKONTROLL                                           
548700     .                                                                    
548800                                                                          
548900 IMS-37-GHNP-WDK611               SECTION.                                
549000     MOVE 'IMS-37' TO WS-CURRENT-IMS-SECTION                              
549100                                                                          
549200     MOVE SPACE               TO ALL-SSA                                  
549300     MOVE 'WDK611  '           TO SSA1                                    
549400     MOVE '  '                 TO GODK-STATUSKODER                        
549500     CALL  CBLTDLI  USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                
549600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
549700     PERFORM IMS-STATUSKONTROLL                                           
549800     .                                                                    
549900                                                                          
550000 IMS-38-ISRT-WDGX4007  SECTION.                                           
550100     MOVE 'IMS-38' TO WS-CURRENT-IMS-SECTION                              
550200                                                                          
550300     MOVE SPACE               TO ALL-SSA                                  
550400     MOVE 'WL400701 ' TO SSA1                                             
550500     MOVE '    ' TO GODK-STATUSKODER                                      
550600     CALL CBLTDLI USING ISRT 4007-PCB DLI-IO-WDGX4007 SSA1                
550700     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
550800     PERFORM IMS-STATUSKONTROLL                                           
550900     .                                                                    
551000                                                                          
551100 IMS-39-ISRT-WDGX4008 SECTION.                                            
551200     MOVE 'IMS-39' TO WS-CURRENT-IMS-SECTION                              
551300                                                                          
551400     MOVE SPACE               TO ALL-SSA                                  
551500     STRING 'WL400701(WDGXKEY  =' W-4007-PU-IDHTYP-X ')'                  
551600          DELIMITED BY SIZE INTO SSA1                                     
551700     MOVE 'WL400711 ' TO SSA2                                             
551800     MOVE '    ' TO GODK-STATUSKODER                                      
551900     CALL CBLTDLI USING ISRT 4007-PCB DLI-IO-WDGX4008 SSA1 SSA2           
552000     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
552100     PERFORM IMS-STATUSKONTROLL                                           
552200     .                                                                    
552300                                                                          
552400 IMS-40-GU-WDK711 SECTION.                                                
552500     MOVE 'IMS-40' TO WS-CURRENT-IMS-SECTION                              
552600                                                                          
552700     MOVE SPACE               TO ALL-SSA                                  
552800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
552900          DELIMITED BY SIZE INTO SSA1                                     
553000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
553100          DELIMITED BY SIZE INTO SSA2                                     
553200     MOVE '  ' TO GODK-STATUSKODER                                        
553300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
553400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
553500     PERFORM IMS-STATUSKONTROLL                                           
553600     .                                                                    
553700                                                                          
553800 IMS-41-ISRT-WDGX4006      SECTION.                                       
553900     MOVE 'IMS-41' TO WS-CURRENT-IMS-SECTION                              
554000                                                                          
554100     MOVE SPACE               TO ALL-SSA                                  
554200     STRING 'WL400301(WDGXKEY  =' W-4003-ETIK-IDHTYP-X ')'                
554300          DELIMITED BY SIZE INTO SSA1                                     
554400     MOVE 'WL400311 ' TO SSA2                                             
554500     MOVE 'WL400321 ' TO SSA3                                             
554600     MOVE '  II' TO GODK-STATUSKODER                                      
554700     CALL CBLTDLI USING ISRT 4003-PCB DLI-IO-WDGX4006                     
554800                             SSA1 SSA2 SSA3                               
554900     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
555000     PERFORM IMS-STATUSKONTROLL                                           
555100     .                                                                    
555200                                                                          
555300 IMS-42-ISRT-WDGX4010   SECTION.                                          
555400     MOVE 'IMS-42' TO WS-CURRENT-IMS-SECTION                              
555500                                                                          
555600     MOVE SPACE               TO ALL-SSA                                  
555700     STRING 'WL400701(WDGXKEY  =' W-4007-PU-IDHTYP-X ')'                  
555800          DELIMITED BY SIZE INTO SSA1                                     
555900     MOVE 'WL400711 ' TO SSA2                                             
556000     MOVE 'WL400721 ' TO SSA3                                             
556100     MOVE '  II' TO GODK-STATUSKODER                                      
556200     CALL CBLTDLI USING ISRT 4007-PCB DLI-IO-WDGX4010                     
556300                             SSA1 SSA2 SSA3                               
556400     MOVE 4007-STATUS-CODE TO STATUS-WS                                   
556500     PERFORM IMS-STATUSKONTROLL                                           
556600     .                                                                    
556700                                                                          
556800 IMS-43-GU-WDD501  SECTION.                                               
556900     MOVE 'IMS-43' TO WS-CURRENT-IMS-SECTION                              
557000                                                                          
557100     MOVE SPACE               TO ALL-SSA                                  
557200     STRING 'WDD501  (IDARTNR  =' W-ART-IDARTNR-X ')'                     
557300          DELIMITED BY SIZE INTO SSA1                                     
557400     MOVE '  GE' TO GODK-STATUSKODER                                      
557500     CALL CBLTDLI USING GU WDD5-PCB DLI-IO-WDD501 SSA1                    
557600     MOVE WDD5-STATUS-CODE TO STATUS-WS                                   
557700     PERFORM IMS-STATUSKONTROLL                                           
557800     .                                                                    
557900                                                                          
558000 IMS-44-ISRT-WDGX4003   SECTION.                                          
558100     MOVE 'IMS-44' TO WS-CURRENT-IMS-SECTION                              
558200                                                                          
558300     MOVE SPACE               TO ALL-SSA                                  
558400     MOVE 'WL400301 ' TO SSA1                                             
558500     MOVE '    ' TO GODK-STATUSKODER                                      
558600     CALL CBLTDLI USING ISRT 4003-PCB DLI-IO-WDGX4003 SSA1                
558700     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
558800     PERFORM IMS-STATUSKONTROLL                                           
558900     .                                                                    
559000                                                                          
559100 IMS-45-ISRT-WDGX4004   SECTION.                                          
559200     MOVE 'IMS-45' TO WS-CURRENT-IMS-SECTION                              
559300                                                                          
559400     MOVE SPACE               TO ALL-SSA                                  
559500     STRING 'WL400301(WDGXKEY  =' W-4003-ETIK-IDHTYP-X ')'                
559600          DELIMITED BY SIZE INTO SSA1                                     
559700     MOVE 'WL400311 ' TO SSA2                                             
559800     MOVE '    ' TO GODK-STATUSKODER                                      
559900     CALL CBLTDLI USING ISRT 4003-PCB DLI-IO-WDGX4004 SSA1 SSA2           
560000     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
560100     PERFORM IMS-STATUSKONTROLL                                           
560200     .                                                                    
560300                                                                          
560400 IMS-46-GU-WDQ301-STATUS    SECTION.                                      
560500     MOVE 'IMS-46' TO WS-CURRENT-IMS-SECTION                              
560600                                                                          
560700     MOVE SPACE               TO ALL-SSA                                  
560800     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN                          
560900                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
561000                    '&KDODELST =' W-KDODELST     ')'                      
561100          DELIMITED BY SIZE INTO SSA1                                     
561200     MOVE '  GE' TO GODK-STATUSKODER                                      
561300     CALL CBLTDLI USING GU  WDQ3-PCB DLI-IO-WDQ301 SSA1                   
561400     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
561500     PERFORM IMS-STATUSKONTROLL                                           
561600     .                                                                    
561700                                                                          
561800 IMS-47-REPL-WDQ212   SECTION.                                            
561900     MOVE 'IMS-47' TO WS-CURRENT-IMS-SECTION                              
562000                                                                          
562100     MOVE SPACE               TO ALL-SSA                                  
562200     MOVE '    ' TO GODK-STATUSKODER                                      
562300     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-WDQ201-12                    
562400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
562500     PERFORM IMS-STATUSKONTROLL                                           
562600     .                                                                    
562700                                                                          
562800 IMS-48-GHU-WDGX4004 SECTION.                                             
562900     MOVE 'IMS-48' TO WS-CURRENT-IMS-SECTION                              
563000                                                                          
563100     MOVE SPACE               TO ALL-SSA                                  
563200     STRING 'WL400301(WDGXKEY  =' W-4003-ETIK-IDHTYP-X ')'                
563300          DELIMITED BY SIZE INTO SSA1                                     
563400     MOVE 'WL400311 ' TO SSA2                                             
563500     MOVE '    ' TO GODK-STATUSKODER                                      
563600     CALL CBLTDLI USING GHU 4003-PCB DLI-IO-WDGX4004 SSA1 SSA2            
563700     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
563800     PERFORM IMS-STATUSKONTROLL                                           
563900     .                                                                    
564000                                                                          
564100 IMS-49-REPL-WDGX4004   SECTION.                                          
564200     MOVE 'IMS-49' TO WS-CURRENT-IMS-SECTION                              
564300                                                                          
564400     MOVE SPACE               TO ALL-SSA                                  
564500     MOVE '    ' TO GODK-STATUSKODER                                      
564600     CALL CBLTDLI USING REPL 4003-PCB DLI-IO-WDGX4004                     
564700     MOVE 4003-STATUS-CODE TO STATUS-WS                                   
564800     PERFORM IMS-STATUSKONTROLL                                           
564900     .                                                                    
565000                                                                          
565100 IMS-50-ISRT-WDGX4017 SECTION.                                            
565200     MOVE 'IMS-50' TO WS-CURRENT-IMS-SECTION                              
565300                                                                          
565400     MOVE SPACE               TO ALL-SSA                                  
565500     MOVE 'WDR401   '  TO SSA1                                            
565600     MOVE '    ' TO GODK-STATUSKODER                                      
565700     CALL CBLTDLI USING ISRT 4017-PCB 4017-WDGX4017 SSA1                  
565800     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
565900     PERFORM IMS-STATUSKONTROLL                                           
566000     .                                                                    
566100                                                                          
566200 IMS-51-REPL-WDGX4018 SECTION.                                            
566300     MOVE 'IMS-51' TO WS-CURRENT-IMS-SECTION                              
566400                                                                          
566500     MOVE SPACE               TO ALL-SSA                                  
566600     MOVE '    ' TO GODK-STATUSKODER                                      
566700     CALL CBLTDLI USING REPL 4017-PCB 4018-WDGX4018                       
566800     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
566900     PERFORM IMS-STATUSKONTROLL                                           
567000     .                                                                    
567100                                                                          
567200 IMS-52-ISRT-WDGX4018 SECTION.                                            
567300     MOVE 'IMS-52' TO WS-CURRENT-IMS-SECTION                              
567400                                                                          
567500     MOVE SPACE               TO ALL-SSA                                  
567600     STRING 'WDR401  (WDGXKEY  =' W-4017-IDHTYP-X ')'                     
567700          DELIMITED BY SIZE INTO SSA1                                     
567800     MOVE 'WDGX4018 ' TO SSA2                                             
567900     MOVE '    ' TO GODK-STATUSKODER                                      
568000     CALL CBLTDLI USING ISRT 4017-PCB 4018-WDGX4018 SSA1 SSA2             
568100     MOVE 4017-STATUS-CODE TO STATUS-WS                                   
568200     PERFORM IMS-STATUSKONTROLL                                           
568300     .                                                                    
568400                                                                          
568500 IMS-53-ISRT-4541-WL454111 SECTION.                                       
568600                                                                          
568700     MOVE SPACE               TO ALL-SSA                                  
568800     STRING 'WDR401  (WDGXKEY  =' W-4541-IDHTYP-X ')'                     
568900          DELIMITED BY SIZE INTO SSA1                                     
569000     MOVE 'WDGX4542 ' TO SSA2                                             
569100     MOVE '  II' TO GODK-STATUSKODER                                      
569200     CALL CBLTDLI USING ISRT 4541-PCB DLI-IO-WDGX4541 SSA1 SSA2           
569300     MOVE 4541-STATUS-CODE TO STATUS-WS                                   
569400     PERFORM IMS-STATUSKONTROLL                                           
569500     .                                                                    
569600 IMS-54-GU-WDK712 SECTION.                                                
569700     MOVE 'IMS-54' TO WS-CURRENT-IMS-SECTION                              
569800                                                                          
569900     MOVE SPACE               TO ALL-SSA                                  
570000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
570100          DELIMITED BY SIZE INTO SSA1                                     
570200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
570300          DELIMITED BY SIZE INTO SSA2                                     
570400     MOVE '  GE' TO GODK-STATUSKODER                                      
570500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
570600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
570700     PERFORM IMS-STATUSKONTROLL                                           
570800     .                                                                    
570900 IMS-GU-WDQ201-CSEQ-GE   SECTION.                                         
571000                                                                          
571100     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ ')'                         
571200             DELIMITED BY SIZE INTO SSA1                                  
571300     MOVE    '  GE'              TO GODK-STATUSKODER                      
571400     CALL    CBLTDLI USING       GU  WDQ2C-PCB DLI-IO-WDQ201              
571500                                      SSA1                                
571600     MOVE   WDQ2C-STATUS-CODE TO STATUS-WS                                
571700     PERFORM IMS-STATUSKONTROLL                                           
571800     .                                                                    
571900                                                                          
572000 IMS-GU-ORQI01-CSEQ SECTION.                                              
572100                                                                          
572200     STRING  'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
572300             DELIMITED BY SIZE INTO SSA1                                  
572400     MOVE    '  '                TO GODK-STATUSKODER                      
572500     CALL    CBLTDLI USING       GU   ORQICSQ-PCB DLI-IO-WDQ201           
572600                                      SSA1                                
572700     MOVE    ORQICSQ-STATUS-CODE TO STATUS-WS                             
572800     PERFORM IMS-STATUSKONTROLL                                           
572900     .                                                                    
573000                                                                          
573100 IMS-GU-SEQB-WDA601 SECTION.                                              
573200     STRING 'WDA601  (WDA6BSEQ>=' W-WDA6BSEQ-MIN-X                        
573300                    '&WDA6BSEQ<=' W-WDA6BSEQ-MAX-X ')'                    
573400            DELIMITED BY SIZE INTO SSA1                                   
573500     MOVE '  GE'                 TO GODK-STATUSKODER                      
573600     CALL  CBLTDLI  USING GU   WDA6B-PCB DLI-IO-WDA601 SSA1               
573700     MOVE WDA6B-STATUS-CODE TO STATUS-WS                                  
573800     PERFORM IMS-STATUSKONTROLL                                           
573900     .                                                                    
574000     SKIP2                                                                
574100 IMS-GU-WDA501 SECTION.                                                   
574200                                                                          
574300     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-A5-MIN-X                     
574400                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
574500                    '&KDORDKL  =' W-KDORDKL-X                             
574600                    '&IDKNDRFL =' W-IDKUNDRF-LEV-X ')'                    
574700          DELIMITED BY SIZE INTO SSA1                                     
574800     MOVE '  GE'              TO GODK-STATUSKODER                         
574900     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-WDA501 SSA1                    
575000     MOVE WDA5-STATUS-CODE    TO STATUS-WS                                
575100     PERFORM IMS-STATUSKONTROLL                                           
575200     .                                                                    
575300                                                                          
575400 IMS-GU-WDP4A1 SECTION.                                                   
575500                                                                          
575600     STRING 'WDP4A1  (IDDISTRF<=' W-IDDISTR-P4-X                          
575700                    '&IDDISTRT>=' W-IDDISTR-P4-X ')'                      
575800          DELIMITED BY SIZE INTO SSA1                                     
575900     MOVE '  GE' TO GODK-STATUSKODER                                      
576000     CALL CBLTDLI USING GU WDP4A-PCB DLI-IO-AREA-WDP4A1                   
576100                           SSA1                                           
576200     MOVE WDP4A-STATUS-CODE    TO STATUS-WS                               
576300     PERFORM IMS-STATUSKONTROLL                                           
576400     .                                                                    
576500                                                                          
576600 IMS-INSERT-4397-TRANS SECTION.                                           
576700     MOVE 'IMS-55' TO WS-CURRENT-IMS-SECTION                              
576800                                                                          
576900     MOVE LOW-VALUE TO 4397-Z1 4397-Z2                                    
577000     MOVE SPACE TO GODK-STATUSKODER                                       
577100     CALL CBLTDLI USING ISRT 4397-PCB 4397-IO-AREA                        
577200     MOVE 4397-STATUS-CODE TO STATUS-WS                                   
577300     PERFORM IMS-STATUSKONTROLL                                           
577400     .                                                                    
577500     EJECT                                                                
577600 IMS-ISRT-WDL901 SECTION.                                                 
577700                                                                          
577800     MOVE 'WLLOGA01 ' TO SSA1                                             
577900     MOVE '  II' TO GODK-STATUSKODER                                      
578000     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
578100     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
578200     PERFORM IMS-STATUSKONTROLL                                           
578300     .                                                                    
578400     EJECT                                                                
578500 IMS-GHNP-WDK627 SECTION.                                                 
578600                                                                          
578700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
578800          DELIMITED BY SIZE INTO SSA1                                     
578900     STRING 'WDK611  (KDSEGKEY =' W-WDK611-KDSEGKEY-X ')'                 
579000          DELIMITED BY SIZE INTO SSA2                                     
579100     MOVE 'WDK627   ' TO SSA3                                             
579200     MOVE '  GE' TO GODK-STATUSKODER                                      
579300     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK627 SSA1 SSA2 SSA3        
579400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
579500     PERFORM IMS-STATUSKONTROLL                                           
579600     .                                                                    
579700 IMS-REPL-WDK627 SECTION.                                                 
579800                                                                          
579900     MOVE SPACE               TO ALL-SSA                                  
580000     MOVE '    ' TO GODK-STATUSKODER                                      
580100     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK627                       
580200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
580300     PERFORM IMS-STATUSKONTROLL                                           
580400     .                                                                    
580500                                                                          
580600 IMS-ISRT-WDK627 SECTION.                                                 
580700                                                                          
580800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
580900          DELIMITED BY SIZE INTO SSA1                                     
581000     STRING 'WDK611  (KDSEGKEY =' W-WDK611-KDSEGKEY-X ')'                 
581100          DELIMITED BY SIZE INTO SSA2                                     
581200     MOVE 'WDK627   ' TO SSA3                                             
581300     MOVE '    ' TO GODK-STATUSKODER                                      
581400     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK627 SSA1 SSA2 SSA3        
581500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
581600     PERFORM IMS-STATUSKONTROLL                                           
581700     .                                                                    
581800     EJECT                                                                
581900 IMS-GHU-WDK901 SECTION.                                                  
582000                                                                          
582100     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
582200          DELIMITED BY SIZE INTO SSA1                                     
582300     MOVE '  GE' TO GODK-STATUSKODER                                      
582400     CALL CBLTDLI USING GHU WDK9-PCB DLI-IO-WDK901 SSA1                   
582500     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
582600     PERFORM IMS-STATUSKONTROLL                                           
582700     .                                                                    
582800     EJECT                                                                
582900 IMS-REPL-WDK901 SECTION.                                                 
583000                                                                          
583100     MOVE '    ' TO GODK-STATUSKODER                                      
583200     CALL CBLTDLI USING REPL WDK9-PCB DLI-IO-WDK901                       
583300     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
583400     PERFORM IMS-STATUSKONTROLL                                           
583500     .                                                                    
583600                                                                          
583700 DB2-SELECT-TP4TRAN     SECTION.                                          
583800     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
583900                                                                          
584000     MOVE 000100 TO GODK-SQLCODEKODER                                     
584100                                                                          
584200     EXEC SQL                                                             
584300           SELECT  DISTINCT                                               
584400                   IDDC_REC                                               
584500                                                                          
584600           INTO   :TP4TRAN-IDDC-REC                                       
584700                                                                          
584800           FROM    TP4TRAN                                                
584900                                                                          
585000           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
585100     END-EXEC                                                             
585200                                                                          
585300     MOVE SQLCODE TO SQLCODE-WS                                           
585400     PERFORM DB2-STATUSKONTROLL                                           
585500     .                                                                    
585600     EJECT                                                                
585700                                                                          
585800 IMS-STATUSKONTROLL SECTION.                                              
585900                                                                          
586000     SET STATUS-IX TO 1                                                   
586100     SEARCH GODK-STATUS                                                   
586200       AT END CALL FELLOG                                                 
586300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
586400     END-SEARCH                                                           
586500     .                                                                    
586600     EJECT                                                                
586700                                                                          
586800 DB2-STATUSKONTROLL  SECTION.                                             
586900                                                                          
587000     SET SQLCODE-IX TO 1                                                  
587100     SEARCH GODK-SQLCODE                                                  
587200       AT END                                                             
587300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
587400          DELIMITED BY SIZE INTO ERROR-TEXT                               
587500          CALL ABEND USING RKOD-ABEND-DB2                                 
587600       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
587700     END-SEARCH                                                           
587800     .                                                                    
587900     EJECT                                                                
